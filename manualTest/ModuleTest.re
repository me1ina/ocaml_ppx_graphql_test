let record_types = [];

[@deriving gql(~name="aaaa")]
type a = {
  a: int,
  b: string,
}; 