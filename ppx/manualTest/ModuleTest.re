let record_types = [];
module StringSet = Set.Make(String);
let rec_types = StringSet.of_list([]);

[@deriving gql(~name="aaaa")]
type a = {
  a: int,
  b: string,
};

[@deriving gql]
type b = a;

[@deriving gql]
type c = b;
