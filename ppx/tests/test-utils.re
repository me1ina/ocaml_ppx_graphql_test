module type GqlTest = {
    let name: string
    let test_gql: string
    let expected: string
  };

let test: (string, array((module GqlTest))) => unit = 