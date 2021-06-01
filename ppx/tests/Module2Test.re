[@deriving gql]
type a = {
  a: ModuleTest.a,
  b: option(ModuleTest.a),
};

[@deriving gql]
type b = option(ModuleTest.b);

[@deriving gql]
type c = b;

[@deriving gql]
type d = option(ModuleTest.c);

[@deriving gql]
type e = option(ModuleTest.a);

[@deriving gql]
type f = list(ModuleTest.e);
 
[@deriving gql]
type g = ModuleTest.d;