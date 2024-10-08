type command = Add | Sub | Left | Right | Put | Get | Loop of command list
val parse : string -> command list
val stringify : command list -> string
