type loc = ((string, int, int, int), string);
module type GqlTest = {
  let name: loc;
  let test_gql: string;
  let expected: string;
};

module TestString = {
  module StringSet = Set.Make(String);
  let rec_types = StringSet.empty;
  [@deriving gql]
  type test = {x: string};
  let name = __POS_OF__(__MODULE__);
  let expected =
"type Test {
 x: String!
}";
};

let stringEquals = (a, b) => String.compare(a, b) == 0;

let gqlTest: (module GqlTest) => unit =
  testCase => {
    let (module GqlTest) = testCase;
    Tests.run(GqlTest.name, GqlTest.test_gql, stringEquals, GqlTest.expected);
  };

let run = () => {
  print_endline("run tests");
  (
    [|
      (module TestString) /*
      module(TestRecord),
      module(TestCombined) */
    |]:
      array(module GqlTest)
  )
  ->Belt.Array.forEach(testCase => gqlTest(testCase));
};
run();
