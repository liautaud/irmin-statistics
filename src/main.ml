let ( let* ) = Lwt.bind

let main () =
  Logs.set_level (Some Logs.Info);

  (* Parse command-line parameters. *)
  if Array.length Sys.argv < 2 then (
    Printf.eprintf "Usage: %s path_to_tezos_context [root_commit_hash]\n"
      Sys.argv.(0);
    Printf.eprintf
      "If [root_commit_hash] is ommited, branches are used as heads.\n";
    exit 1 );

  (* Create a read-only Irmin store for the Tezos context. *)
  let path = Sys.argv.(1) in
  let* repo =
    Store.Repo.v (Irmin_pack.config ~readonly:true ~lru_size:0 path)
  in
  let* heads =
    if Array.length Sys.argv >= 3 then
      Sys.argv.(2) |> Store_hash.of_string |> Result.get_ok
      |> Store.Commit.of_hash repo
    else Lwt.return_none
  in
  Statistics.dump ~heads repo

let () = Lwt_main.run (main ())
