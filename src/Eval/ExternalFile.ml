(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

open Value
open ExternalUtils
open Unix


let getEnv s =
  Option.map (fun x -> VStr x) (Sys.getenv_opt s)
  |> of_option

let of_result res =
  match res with
  | Ok x    -> VCtor(0, [x])
  | Error e -> VCtor(1, [e])

let ok a = of_result (Ok a)

let err_OsErr msg =
  VCtor(0, [VStr msg])

let fun_try f x =
  try f x |> ok
  with
  | Sys_error s -> of_result (Error (err_OsErr s))
  | Unix.Unix_error (e,f,s) ->
    of_result (Error (err_OsErr (Unix.error_message e)))
  | Failure s ->
    of_result (Error (err_OsErr s))

let int_fun_try f = int_fun (fun_try f)
let str_fun_try f = str_fun (fun_try f)

let getArgv () =
  let argv = Array.of_list !DblConfig.prog_args in
  of_list (Array.map (fun s -> VStr s) argv |> Array.to_list)

let openFile = str_fun (fun s ->
int_fun_try (
  fun n ->
  match n with
  | 0 -> VNum (Obj.magic @@ openfile s [O_RDONLY] 0)
  | 1 -> VNum (Obj.magic @@ openfile s [O_WRONLY; O_CREAT] 0o640)
  | 2 -> VNum (Obj.magic @@ openfile s [O_RDWR; O_CREAT] 0o640)
  | _ -> failwith "Invalid argument: dbl_openFile")
)

let readFile = int_fun (fun fd ->
  int_fun_try (
    fun n ->
    let buf = Bytes.create n in
    let c = Unix.read (Obj.magic fd) buf 0 n in
    VStr (Bytes.to_string @@ Bytes.sub buf 0 c )))

let readLine = int_fun_try (fun fd ->
  let fd = Obj.magic fd in
  let res_buffer = Buffer.create 80 in
  let char_buf = Bytes.create 1 in
  let rec read_loop () =
    if Unix.read fd char_buf 0 1 = 0 then
      if Buffer.length res_buffer = 0 then
        of_option None
      else
        of_option (Some (VStr (Buffer.contents res_buffer)))
    else
      let c = Bytes.get char_buf 0 in
      if c = '\n' then
        of_option (Some (VStr (Buffer.contents res_buffer)))
      else begin
        Buffer.add_char res_buffer c;
        read_loop ()
      end
      in
      read_loop ())


let write = int_fun (fun fd ->
  str_fun_try (fun s ->
    let buf = Bytes.of_string s in
    VNum (Unix.single_write (Obj.magic fd) buf 0 (String.length s))))

let seek = int_fun (fun fd ->
  int_fun (fun pos ->
    int_fun_try ( fun opt ->
      let opt =
        match opt with
        | 0 -> SEEK_SET
        | 1 -> SEEK_CUR
        | 2 -> SEEK_END
        | _ -> failwith "Invalid argument: dbl_seek"
      in
      VNum (Unix.lseek (Obj.magic fd) pos opt))))

let isEof = int_fun_try (fun fd ->
  let fd = Obj.magic fd in
  let curr_pos = Unix.lseek fd 0 SEEK_CUR in
  let stats = Unix.fstat fd in
  of_bool (curr_pos >= stats.st_size)
  )

let extern_file_seq =
   ["dbl_openFile",  openFile;
    "dbl_closeFile", int_fun_try (fun fd -> Unix.close @@ Obj.magic fd; v_unit);
    "dbl_readFile",  readFile;
    "dbl_readLine",  readLine;
    "dbl_isEof",     isEof;
    "dbl_write",     write;
    "dbl_flush",     int_fun_try (fun fd -> Unix.fsync @@ Obj.magic fd; v_unit);
    "dbl_seek",      seek;
    "dbl_tell",      int_fun_try (fun fd ->
      VNum (Unix.lseek (Obj.magic fd) 0 SEEK_CUR))
  ] |> List.to_seq
