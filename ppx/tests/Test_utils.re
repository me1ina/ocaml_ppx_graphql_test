open GqlTest;

let stringCheck = (test_case) => {
  let (module GqlTest): (module GqlTest)= test_case;
  () => Alcotest.(check(string))("test_gql exists and presents the correct schema", GqlTest.expected, GqlTest.test_gql);
};


let test: (string, array((module GqlTest))) => unit = (_test_name, test_cases) => {
  let test_cases_set = Array.sub(test_cases, 1, Array.length(test_cases)-1) |> Array.fold_left(
    (acc, test_case) => {
      let (module GqlTest): (module GqlTest)= test_case;
      List.append(acc, [(GqlTest.name, [(GqlTest.name, `Quick, stringCheck(test_case))])]);
    }, {
      let (module GqlTest): (module GqlTest)= test_cases[0];
      [(GqlTest.name, [(GqlTest.name, `Quick, stringCheck(test_cases[0]))])];
  }
  );
  Alcotest.run(
    "PPX-GQL tests",
    test_cases_set,
  ); 
}
