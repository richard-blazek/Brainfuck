(* encode as little endian *)
let encode_int8 i = String.make 1 (Char.chr (i land 255))
let encode_int16 i = encode_int8 i ^ encode_int8 (i lsr 8)
let encode_int32 i = encode_int16 i ^ encode_int16 (i lsr 16)

let elf_header_size = 52
let program_header_size = 32
let memory_size = 0x10000

let elf_header =
  "\x7FELF" ^ (* Magic bytes *)
  encode_int8 1 ^ (* 64-bit format *)
  encode_int8 1 ^ (* little endian *)
  encode_int8 1 ^ (* ELF version *)
  "\x00\x00\x00\x00\x00\x00\x00\x00\x00" ^ (* 9 bytes of padding *)
  encode_int16 2 ^ (* Executable file *)
  encode_int16 3 ^ (* CPU x86 *)
  encode_int32 1 ^ (* ELF version again *)
  encode_int32 memory_size ^
  encode_int32 elf_header_size ^ (* Program header table position: after header *)
  encode_int32 0 ^ (* Section header table position: none *)
  encode_int32 0 ^ (* e_flags *)
  encode_int16 elf_header_size ^
  encode_int16 program_header_size ^
  encode_int16 2 ^ (* Program header count: code and data *)
  encode_int16 0 ^ (* Section header size *)
  encode_int16 0 ^ (* Section header count: none *)
  encode_int16 0 (* e_shstrndx *)

let program_header offset_file size_file offset_mem size_mem =
  encode_int32 1 ^ (* loadable segment *)
  encode_int32 offset_file ^ (* offset in the file image *)
  encode_int32 offset_mem ^ (* address in the memory *)
  encode_int32 0 ^ (* p_paddr *)
  encode_int32 size_file ^ (* size of the segment in the file image, may be 0 *)
  encode_int32 size_mem ^ (* size of the segment in the memory, may be 0 *)
  encode_int32 7 ^ (* read+write+execute *)
  encode_int32 0x10000 (* alignment *)

module Instructions = struct
  let initialise = "\x31\xf6" (* xor esi, esi *)

  let terminate =
    "\xc1\xc0\x40" ^ (* xor eax, eax; inc eax *)
    "\x31\xdb" ^     (* xor ebx, ebx *)
    "\xcd\x80"       (* int 0x80 *)

  let inc = "\xfe\x06" (* inc byte[esi] *)

  let dec = "\xfe\x0e" (* dec byte[esi] *)

  let right =
    "\x46" ^                     (* inc esi *)
    "\x81\xe6\xff\xff\x00\x00"   (* and esi, 0xFFFF *)

  let left =
    "\x4e" ^                     (* dec esi *)
    "\x81\xe6\xff\xff\x00\x00"   (* and esi, 0xFFFF *)

  let put =
    "\xb8\x04\x00\x00\x00" ^ (* mov eax, 4 *)
    "\x31\xdb\x43" ^         (* xor ebx, ebx; inc ebx *)
    "\x89\xf1" ^             (* mov ecx, esi *)
    "\x31\xd2\x42" ^         (* xor edx, edx; inc edx *)
    "\xcd\x80"               (* int 0x80 *)

  let get =
    "\xb8\x03\x00\x00\x00" ^ (* mov eax, 3 *)
    "\x31\xdb" ^             (* xor ebx, ebx *)
    "\x89\xf1" ^             (* mov ecx, esi *)
    "\x31\xd2\x42" ^         (* xor edx, edx; inc edx *)
    "\xcd\x80"               (* int 0x80 *)

  let jump_by rel = "\xe9" ^ encode_int32 rel

  let while_do inner_len =
    "\x8a\x06" ^ (* mov al, byte [esi] *)
    "\x84\xc0" ^ (* test al, al *)
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
  program_header 0 0 0 memory_size ^
  program_header (elf_header_size + 2 * program_header_size) program_len memory_size program_len ^
  program

let generate cmds filename =
  let file = open_out_bin filename in
  output_string file (compile cmds);
  close_out file
