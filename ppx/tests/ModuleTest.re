[@deriving gql]
type a = {
  a: int,
  b: string,
};

[@deriving gql]
type b = array(a);

[@deriving gql]
type c = b;

[@deriving gql]
type d = array(bool);

[@deriving gql]
type e = option(int);