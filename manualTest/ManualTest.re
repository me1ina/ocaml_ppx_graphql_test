[@deriving gql]
type x = int;

[@deriving gql]
type y = option(string);

[@deriving gql]
type z = y;

[@deriving gql]
type t = {
  a: list(array(option(int))),
  b: x,
  c: y,
  d: z,
};

print_endline("\n" ++ t_gql);