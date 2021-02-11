

let record_types = [];

[@deriving gql]
type x = int;

[@deriving gql]
type y = option(string);

[@deriving gql]
type z = y;

/* [@deriving gql(~name="aaaa")]
type a = {
  a: int,
  b: string,
};  */


/* ModuleTest.a */

[@deriving gql(~name="Type1")] 
type t = {
  a: option(list(array(option(ModuleTest.a)))),
  b: ModuleTest.a,
  c: y,
  d: x
};

print_endline("\n" ++ t_gql);
print_endline("\n" ++ moduleTest_a_gql);
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