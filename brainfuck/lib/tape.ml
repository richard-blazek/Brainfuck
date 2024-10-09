type tape = int list * int * int list

let empty = [], 0, []
let current (_, this, _) = this
let add (l, this, r) = l, this + 1, r
let sub (l, this, r) = l, this - 1, r

let right (l, this, r) = match r with
| [] -> this :: l, 0, []
| rh :: rt -> this :: l, rh, rt

let left (l, this, r) = match l with
| [] -> [], 0, this :: r
| lh :: lt -> lt, lh, this :: r
