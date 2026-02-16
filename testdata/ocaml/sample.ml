open Printf
open List

let greet name =
  sprintf "Hello, %s" name

let rec factorial n =
  if n <= 1 then 1
  else n * factorial (n - 1)

let add x y = x + y

type config = {
  name : string;
  port : int;
}

module Utils = struct
  let helper x = x * 2
end
