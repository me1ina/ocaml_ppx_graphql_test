type x = int
let x_gql = "int"
type t = {
  a: int ;
  b: x }[@@deriving gql]
;;print_endline ("\n" ^ typename)