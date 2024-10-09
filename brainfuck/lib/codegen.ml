(* encode as little endian *)
let encode_int8 i = String.make 1 (Char.chr (i land 255))
let encode_int16 i = encode_int8 i ^ encode_int8 (i lsr 8)
let encode_int32 i = encode_int16 i ^ encode_int16 (i lsr 16)
let encode_int64 i = encode_int32 i ^ encode_int32 (i lsr 32)

(* output integer encoded as little endian *)
let output_int8 file i = output_string file (encode_int8 i)
let output_int16 file i = output_string file (encode_int16 i)
let output_int32 file i = output_string file (encode_int32 i)
let output_int64 file i = output_string file (encode_int64 i)

let elf_header_size = 0x40
let program_header_size = 0x38
let program_entry_point = 1 lsl 17

let write_elf_header file =
  output_string file "\x7FELF"; (* Magic bytes *)
  output_int8 file 2; (* 64-bit format *)
  output_int8 file 1; (* little endian *)
  output_int8 file 1; (* ELF version *)
  output_int8 file 3; (* Linux *)
  output_int8 file 0; (* ABI version, not used on Linux *)
  output_string file "\x00\x00\x00\x00\x00\x00\x00"; (* 7 bytes of padding *)
  output_int16 file 2; (* Executable file *)
  output_int16 file 0x3E; (* CPU x86-64 *)
  output_int32 file 1; (* ELF version again *)
  output_int64 file program_entry_point;
  output_int64 file 0x40; (* Program header table position: after header *)
  output_int64 file 0; (* Section header table position: none *)
  output_int32 file 0; (* e_flags *)
  output_int16 file elf_header_size;
  output_int16 file program_header_size;
  output_int16 file 0x02; (* Program header count: code and data *)
  output_int16 file 0x40; (* Section header size *)
  output_int16 file 0; (* Section header count: none *)
  output_int16 file 0 (* e_shstrndx *)

let write_program_header file offset_file size_file offset_mem size_mem =
  output_int32 file 1; (* loadable segment *)
  output_int32 file 7; (* read+write+execute *)
  output_int64 file offset_file; (* offset in the file image *)
  output_int64 file offset_mem; (* address in the memory *)
  output_int64 file 0; (* p_paddr *)
  output_int64 file size_file; (* size of the segment in the file image, may be 0 *)
  output_int64 file size_mem; (* size of the segment in the memory, may be 0 *)
  output_int64 file 0 (* no alignment *)

module Instructions = struct
  let initialise =
    "\x48\x31\xf6" (* set rsi to zero *)

  let terminate =
    "\x48\x8b\x04\x25\x3c\x00\x00\x00" ^ (* put syscall number 60 to rax *)
    "\x48\x31\xff" ^                     (* put the success code 0 to rdi *)
    "\x0f\x05"                           (* syscall *)

  let inc =
    "\x8a\x06" ^ (* load [rsi] to al *)
    "\xfe\xc0" ^ (* inc al *)
    "\x88\x06"   (* store al to [rsi] *)

  let dec =
    "\x8a\x06" ^ (* load [rsi] to al *)
    "\xfe\xc8" ^ (* inc al *)
    "\x88\x06"   (* store al to [rsi] *)

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
  Instructions.initialise ^ compile_cmds cmds ^ Instructions.terminate

let generate cmds filename =
  let file = open_out_bin filename in
  let program = compile cmds in
  write_elf_header file;
  (* Data section, 2^16 bytes from 0 to 65535 *)
  write_program_header file 0 0 0 (1 lsl 16);
  (* Program section, s 2^16 bytes from 0 to 65535 *)
  write_program_header file (elf_header_size + program_header_size) (String.length program) program_entry_point (String.length program);
  (* Write actual program *)
  output_string file program;
  close_out file
