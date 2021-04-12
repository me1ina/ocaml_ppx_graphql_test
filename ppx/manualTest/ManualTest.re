let record_types = [];
module StringSet = Set.Make(String);
let rec_types = StringSet.empty;
/* let set2 = StringSet.add("a", rec_types);
   print_endline(StringSet.exists(String.equal("a"), set2) ? "true" : "false"); */

[@deriving gql]
type x = int;

[@deriving gql]
type y = option(string);

[@deriving gql]
type z = y;

[@deriving gql(~name="aaaa")]
type a = {
  a: y,
  b: z,
};

/* ModuleTest.a */

[@deriving gql(~name="Type1")]
type t = {
  a: option(list(array(option(ModuleTest.a)))),
  b: a,
  c: x,
  d: option(array(ModuleTest.b)),
  e: ModuleTest.c,
};

print_endline("");

StringSet.iter(print_endline, rec_types);

/* let r = Str.regexp("hello \\([A-Za-z]+\\)");
let s = Str.replace_first(r, "\\1", "hello world");

print_endline(s);
 */
print_endline("\n" ++ t_gql);
print_endline("\n" ++ moduleTest_a_gql);
print_endline("\n" ++ moduleTest_b_gql);
List.iter(item => print_endline(item), record_types);

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