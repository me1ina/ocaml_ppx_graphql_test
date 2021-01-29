let record_types = [];

[@deriving gql]
type x = int;

[@deriving gql]
type y = option(string);

[@deriving gql]
type z = y;


[@deriving gql]
type a = {
  a: int,
  b: string,
}; 

[@deriving gql] 
type t = {
  a: option(list(array(option(a)))),
  b: x,
  c: y,
  d: a
};

print_endline("\n" ++ t_gql);
print_endline("\n" ++ a_gql);
List.iter((item => print_endline(item)), record_types);


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