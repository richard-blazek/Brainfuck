(* encode as little endian *)
let encode_int8 i = String.make 1 (Char.chr (i land 255))
let encode_int16 i = encode_int8 i ^ encode_int8 (i lsr 8)
let encode_int32 i = encode_int16 i ^ encode_int16 (i lsr 16)
let encode_int64 i = encode_int32 i ^ encode_int32 (i lsr 32)

let elf_header_size = 0x40
let program_header_size = 0x38
let program_entry_point = 1 lsl 17

let elf_header =
  "\x7FELF" ^ (* Magic bytes *)
  encode_int8 2 ^ (* 64-bit format *)
  encode_int8 1 ^ (* little endian *)
  encode_int8 1 ^ (* ELF version *)
  encode_int8 3 ^ (* Linux *)
  encode_int8 0 ^ (* ABI version, not used on Linux *)
  "\x00\x00\x00\x00\x00\x00\x00" ^ (* 7 bytes of padding *)
  encode_int16 2 ^ (* Executable file *)
  encode_int16 0x3E ^ (* CPU x86-64 *)
  encode_int32 1 ^ (* ELF version again *)
  encode_int64 program_entry_point ^
  encode_int64 0x40 ^ (* Program header table position: after header *)
  encode_int64 0 ^ (* Section header table position: none *)
  encode_int32 0 ^ (* e_flags *)
  encode_int16 elf_header_size ^
  encode_int16 program_header_size ^
  encode_int16 0x02 ^ (* Program header count: code and data *)
  encode_int16 0x40 ^ (* Section header size *)
  encode_int16 0 ^ (* Section header count: none *)
  encode_int16 0 (* e_shstrndx *)

let program_header offset_file size_file offset_mem size_mem =
  encode_int32 1 ^ (* loadable segment *)
  encode_int32 7 ^ (* read+write+execute *)
  encode_int64 offset_file ^ (* offset in the file image *)
  encode_int64 offset_mem ^ (* address in the memory *)
  encode_int64 0 ^ (* p_paddr *)
  encode_int64 size_file ^ (* size of the segment in the file image, may be 0 *)
  encode_int64 size_mem ^ (* size of the segment in the memory, may be 0 *)
  encode_int64 0 (* no alignment *)

module Instructions = struct
  let initialise =
    "\x48\x31\xf6" (* set rsi to zero *)

  let terminate =
    "\x48\x8b\x04\x25\x3c\x00\x00\x00" ^ (* put syscall number 60 to rax *)
    "\x48\x31\xff" ^                     (* put the success code 0 to rdi *)
    "\x0f\x05"                           (* syscall *)

  let inc = "\xfe\x06" (* incb (%rsi) *)

  let dec = "\xfe\x0e" (* decb (%rsi) *)

  let right =
    "\x48\xff\xc6" ^                     (* inc rsi *)
    "\x48\x23\x34\x25\xff\xff\x00\x00"   (* modulo 2^16 *)

  let left =
    "\x48\xff\xce" ^                     (* dec rsi *)
    "\x48\x23\x34\x25\xff\xff\x00\x00"   (* modulo 2^16 *)

  let put =
    "\x48\x8b\x04\x25\x01\x00\x00\x00" ^ (* put syscall number 1 to rax *)
    "\x48\x8b\x3c\x25\x01\x00\x00\x00" ^ (* put stdout fileno 1 to rdi *)
    "\x48\x8b\x14\x25\x01\x00\x00\x00" ^ (* put message size 1 to rdx *)
    "\x0f\x05"                           (* syscall *)

  let get =
    "\x48\x8b\x04\x25\x03\x00\x00\x00" ^ (* put syscall number 3 to rax *)
    "\x48\x31\xff" ^                     (* put stdin fileno 0 to rdi *)
    "\x48\x31\xd2\x48\xff\xc2" ^         (* put input size 1 to rdx *)
    "\x0f\x05"                           (* syscall *)

  let jump_by rel = "\xe9" ^ encode_int32 rel

  let while_do inner_len =
    "\x8a\x06" ^ (* load [rsi] to al *)
    "\x84\xc0" ^ (* test al *)
    "\x75\x05" ^ (* if not zero, jump over the next 5 bytes *)
    jump_by (inner_len + 5) (* skip loop body and also the jump instruction at the end *)

  let while_end inner_len =
    jump_by (-(inner_len + 5 + 3*2 + 5)) (* go to the beginning of while_do *)
end

let rec compile_cmds = function
| [] -> ""
| Parser.Add :: rest -> Instructions.inc ^ compile_cmds rest
| Parser.Sub :: rest -> Instructions.dec ^ compile_cmds rest
| Parser.Right :: rest -> Instructions.right ^ compile_cmds rest
| Parser.Left :: rest -> Instructions.left ^ compile_cmds rest
| Parser.Put :: rest -> Instructions.put ^ compile_cmds rest
| Parser.Get :: rest -> Instructions.get ^ compile_cmds rest
| Parser.Loop inner :: rest ->
  let inner_code = compile_cmds inner in
  Instructions.while_do (String.length inner_code) ^
  inner_code ^
  Instructions.while_end (String.length inner_code) ^
  compile_cmds rest

let compile cmds =
  let program = Instructions.initialise ^ compile_cmds cmds ^ Instructions.terminate in
  let program_len = String.length program in
  elf_header ^
  program_header 0 0 0 (1 lsl 16) ^ (* Data section, 2^16 bytes from 0 to 65535 *)
  program_header (elf_header_size + 2 * program_header_size) program_len program_entry_point program_len ^
  program

let generate cmds filename =
  let file = open_out_bin filename in
  output_string file (compile cmds);
  close_out file
