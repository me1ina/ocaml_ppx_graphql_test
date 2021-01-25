let record_types = [];

[@deriving gql]
type x = int;

[@deriving gql]
type y = option(string);

[@deriving gql]
type z = y;


/* [@deriving gql]
type a = {
  a: int,
  b: string,
}; */

[@deriving gql] 
type t = {
  a: option(list(array(option(int)))),
  b: x,
  c: y,
};

print_endline("\n" ++ t_gql);


/* 
type A {
 a: Int!
 b: String!
}

type T {
  a: [[Int]!]
  b: Int!
  c: String
  d: A!
 } */