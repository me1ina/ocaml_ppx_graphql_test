// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var $$Set = require("bs-platform/lib/js/set.js");
var Curry = require("bs-platform/lib/js/curry.js");
var $$String = require("bs-platform/lib/js/string.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Tests$Test_bucklescript = require("./Tests.bs.js");

var StringSet = $$Set.Make({
      compare: $$String.compare
    });

var test_gql = "type Test {\n x: String!\n}";

var rec_types = Curry._2(StringSet.add, "Test", StringSet.empty);

var name_000 = /* tuple */[
  "RunTests.re",
  13,
  24,
  34
];

var name = /* tuple */[
  name_000,
  "RunTests-Test_bucklescript"
];

var expected = "type Test {\n x: String!\n}";

var TestString = {
  StringSet: StringSet,
  test_gql: test_gql,
  rec_types: rec_types,
  name: name,
  expected: expected
};

function stringEquals(a, b) {
  return $$String.compare(a, b) === 0;
}

function gqlTest(testCase) {
  return Tests$Test_bucklescript.run(testCase.name, testCase.test_gql, stringEquals, testCase.expected);
}

function run(param) {
  console.log("run tests");
  return Belt_Array.forEach([{
                name: name,
                test_gql: test_gql,
                expected: expected
              }], gqlTest);
}

run(undefined);

exports.TestString = TestString;
exports.stringEquals = stringEquals;
exports.gqlTest = gqlTest;
exports.run = run;
/* StringSet Not a pure module */
