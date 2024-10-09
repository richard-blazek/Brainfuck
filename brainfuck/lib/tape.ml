type tape = int list * int * int list

let modulo n d = let rem = n mod d in (rem + d) mod d

let empty = [], 0, []
let get (_, this, _) = this
let put value (l, _, r) = l, modulo value 256, r
let add (l, this, r) = l, modulo (this + 1) 256, r
let sub (l, this, r) = l, modulo (this - 1) 256, r

let right (l, this, r) = match r with
| [] -> this :: l, 0, []
| rh :: rt -> this :: l, rh, rt

let left (l, this, r) = match l with
| [] -> [], 0, this :: r
| lh :: lt -> lt, lh, this :: r
