open Lwt.Infix

module Utils = struct
  let hide_progress_line s =
    let len = String.length s in
    if len > 0 then Printf.eprintf "\r%*s\r" len ""

  let display_progress ?(refresh_rate = (1, 1)) msgf =
    if Unix.isatty Unix.stderr then
      let index, rate = refresh_rate in
      if index mod rate == 0 then
        msgf
          (Format.kasprintf (fun msg ->
               hide_progress_line msg;
               Format.eprintf "%s%!" msg))

  let display_progress_end () =
    if Unix.isatty Unix.stderr then Format.eprintf "@."
end

type vertex =
  | Commit of Store.Hash.t
  | Node of Store.Hash.t
  | Contents of Store.Hash.t
[@@deriving irmin]

module Vertex = struct
  type t = vertex

  let equal = Irmin.Type.equal vertex_t

  let compare = Irmin.Type.compare vertex_t

  let hash x = Irmin.Type.short_hash vertex_t x
end

module Table = Hashtbl.Make (Vertex)

let to_commit x = Commit x

let to_node x = Node x

let pred t =
  let open Store.Private in
  function
  | Commit k -> (
      Commit.find (Repo.commit_t t) k >|= function
      | None -> []
      | Some c ->
          let next_commits = Commit.Val.parents c |> List.map to_commit in
          let next_node = Commit.Val.node c |> to_node in
          next_node :: next_commits )
  | Node k -> (
      Node.find (Repo.node_t t) k >|= function
      | None -> []
      | Some c ->
          Node.Val.list c
          |> List.map (function
               | _, `Node k' -> Node k'
               | _, `Contents (k', _) -> Contents k') )
  | _ -> Lwt.return []

let traverse ~heads ~callback t =
  (* Find the entry-points for the graph traversal. *)
  ( match heads with
  | None -> Store.Repo.heads t
  | Some commit -> Lwt.return [ commit ] )
  >|= List.map (fun x -> x |> Store.Commit.hash |> to_commit)
  >>= fun entrypoints ->
  let pred = pred t in
  let marked = Table.create 10_000_000 in
  let pending = Stack.create () in
  let mark key = Table.add marked key () in
  let has_mark key = Table.mem marked key in
  (* Add the entrypoints to the set of pending objects. *)
  List.iter (fun k -> Stack.push k pending) entrypoints;
  (* Traverse the graph and mark the encountered objects. *)
  let rec visit () =
    match Stack.top pending with
    | exception Stack.Empty -> Lwt.return_unit
    | key ->
        if has_mark key then (
          ignore (Stack.pop pending);
          (visit [@tailcall]) () )
        else (
          mark key;
          callback key >>= fun () ->
          pred key >>= fun keys ->
          List.iter
            (fun k -> if not (has_mark k) then Stack.push k pending)
            keys;
          (visit [@tailcall]) () )
  in
  visit ()

type stats = {
  mutable total_count : int;
  mutable branch_count : int;
  mutable commit_count : int;
  mutable node_count : int;
  mutable contents_count : int;
  mutable contents_size : int;
}

let dump ~heads t =
  let s =
    {
      total_count = 0;
      branch_count = 0;
      commit_count = 0;
      node_count = 0;
      contents_count = 0;
      contents_size = 0;
    }
  in
  Store.Repo.branches t >>= fun branches ->
  s.branch_count <- List.length branches;
  let callback vertex =
    s.total_count <- s.total_count + 1;
    ( match vertex with
    | Commit _ ->
        s.commit_count <- s.commit_count + 1;
        Lwt.return_unit
    | Node _ ->
        s.node_count <- s.node_count + 1;
        Lwt.return_unit
    | Contents h ->
        s.contents_count <- s.contents_count + 1;
        Store.Contents.of_hash t h
        >|= Option.iter (fun c ->
                s.contents_size <- s.contents_size + String.length c) )
    >|= fun () ->
    Utils.display_progress ~refresh_rate:(s.total_count, 1_000) (fun m ->
        m
          "Statistics: %d objects found (%d branches, %d commits, %d nodes, %d \
           contents taking up %dKB)."
          s.total_count s.branch_count s.commit_count s.node_count
          s.contents_count s.contents_size)
  in
  traverse ~heads ~callback t
