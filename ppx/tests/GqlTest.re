module type GqlTest = {
    let name: string
    let test_gql: string
    let expected: string
  };