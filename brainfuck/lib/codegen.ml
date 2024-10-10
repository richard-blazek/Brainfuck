(* encode as little endian *)
let encode_int8 i = String.make 1 (Char.chr (i land 255))
let encode_int16 i = encode_int8 i ^ encode_int8 (i lsr 8)
let encode_int32 i = encode_int16 i ^ encode_int16 (i lsr 16)

let elf_header_size = 0x34
let program_header_size = 0x20
let code_offset = 0x20000000
let data_offset = 0x10000000
let data_length = 0x00010000

let elf_header =
  "\x7FELF" ^ (* Magic bytes *)
  encode_int8 1 ^ (* 64-bit format *)
  encode_int8 1 ^ (* little endian *)
  encode_int8 1 ^ (* ELF version *)
  "\x00\x00\x00\x00\x00\x00\x00\x00\x00" ^ (* 9 bytes of padding *)
  encode_int16 2 ^ (* Executable file *)
  encode_int16 3 ^ (* CPU x86 *)
  encode_int32 1 ^ (* ELF version again *)
  encode_int32 (code_offset + elf_header_size + program_header_size * 2) ^ (* Entry point *)
  encode_int32 elf_header_size ^ (* Program header table position: after header *)
  encode_int32 0 ^ (* Section header table position: none *)
  encode_int32 0 ^ (* e_flags *)
  encode_int16 elf_header_size ^
  encode_int16 program_header_size ^
  encode_int16 2 ^ (* Program header count: code and data *)
  encode_int16 0 ^ (* Section header size *)
  encode_int16 0 ^ (* Section header count: none *)
  encode_int16 0 (* e_shstrndx *)

let program_header_code size =
  encode_int32 1 ^ (* loadable segment *)
  encode_int32 0 ^ (* offset in the file image *)
  encode_int32 code_offset ^ (* address in the memory *)
  encode_int32 code_offset ^ (* p_paddr *)
  encode_int32 size ^ (* size of the segment in the file image, may be 0 *)
  encode_int32 size ^ (* size of the segment in the memory, may be 0 *)
  encode_int32 5 ^ (* read+execute *)
  encode_int32 0x1000 (* alignment *)

let program_header_data =
  encode_int32 1 ^ (* loadable segment *)
  encode_int32 0 ^ (* offset in the file image *)
  encode_int32 data_offset ^ (* address in the memory *)
  encode_int32 data_offset ^ (* p_paddr *)
  encode_int32 0 ^ (* size of the segment in the file image, may be 0 *)
  encode_int32 data_length ^ (* size of the segment in the memory, may be 0 *)
  encode_int32 6 ^ (* read+write *)
  encode_int32 0x1000 (* alignment *)

module Instructions = struct
  let initialise = "\xbe" ^ encode_int32 data_offset (* mov esi, data_offset *)

  let terminate =
    "\x31\xc0\x40" ^ (* xor eax, eax; inc eax *)
    "\x31\xdb" ^     (* xor ebx, ebx *)
    "\xcd\x80"       (* int 0x80 *)

  let inc = "\xfe\x06"       (* inc byte[esi] *)
  let dec = "\xfe\x0e"       (* dec byte[esi] *)
  let right = "\x66\x46"     (* inc si *)
  let left = "\x66\x4e"      (* dec si *)

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
  program_header_code (program_len + elf_header_size + program_header_size * 2) ^
  program_header_data ^
  program

let generate cmds filename =
  let file = open_out_bin filename in
  output_string file (compile cmds);
  close_out file
