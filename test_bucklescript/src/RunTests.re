module type GqlTest = {
    let name: string
    let test_gql: string
    let expected: string
  };
  
module TestString = {
    module StringSet = Set.Make(String);
    let rec_types = StringSet.empty;
    [@deriving gql]
    type test = {x: string};
    let name = "__MODULE__";
    let expected = 
        "type Test = {
             x: String!
            }";
  };
  
let gqlTest: (module(GqlTest)) => unit = (testCase) => {
    let (module GqlTest) = testCase;
    Tests.run(GqlTest.name, GqlTest.test_gql, Js.String.equals, GqlTest.expected);
  }; 

let run = () => {
    print_endline("run");
    [
      (module TestString)/* 
      module(TestRecord),
      module(TestCombined) */
    ] -> Belt.Array.iter(testCase => gqlTest(testCase))
  }