module RecordBasicTest = {
    [@deriving gql]
    type test = {
        a: string,
        b: option(list(array(option(int)))),
        c: array(option(bool)),
    };
    let name = "RecordBasicTest";
    let expected =
"type Test {
 a: String!
 b: [[Integer]!]
 c: [Bool]!
}";
};

module RecordModuleTest = {
    [@deriving gql]
    type test = {
        a: option(list(array(option(ModuleTest.a)))),
        b: ModuleTest.b,
        c: option(ModuleTest.c),        
        d: option(array(ModuleTest.d)),
        e: array(ModuleTest.e),
      };
    let name = "RecordModuleTest";
    let expected =
"type Test {
 a: [[ModuleTest_A]!]
 b: [ModuleTest_A!]!
 c: [ModuleTest_A!]
 d: [[Bool!]!]
 e: [Integer]!
}";
}; 

module RecordModule2Test = {
    [@deriving gql]
    type test = {
        a: option(list(array(option(Module2Test.a)))),
        b: Module2Test.b,
        c: option(Module2Test.c),        
        d: option(array(Module2Test.d)),
        e: Module2Test.e,
        f: array(Module2Test.f),
        g: list(Module2Test.g),
      };
    let name = "RecordModule2Test";
    let expected =
"type Test {
 a: [[Module2Test_A]!]
 b: [ModuleTest_A!]
 c: [ModuleTest_A!]
 d: [[ModuleTest_A!]]
 e: ModuleTest_A
 f: [[Integer]!]!
 g: [[Bool!]!]!
}";
}; 

module RecordAliasTest = {
    [@deriving gql]
    type a = {
    a: int,
    b: string,
    };

    [@deriving gql]
    type b = a;

    [@deriving gql]
    type c = b;

    [@deriving gql]
    type d = array(bool);

    [@deriving gql]
    type e = option(int);

    [@deriving gql]
    type test = {
        a: list(a),
        b: b,
        c: option(array(c)),        
        d: array(d),
        e: e,
      };
    let name = "RecordAliasTest";
    let expected =
"type Test {
 a: [A!]!
 b: A!
 c: [A!]
 d: [[Bool!]!]!
 e: Integer
}";
};

module RecordAliasPartTest = {
    [@deriving gql]
    type a1 = {
    a: int,
    b: string,
    };

    [@deriving gql]
    type b1 = a1;

    [@deriving gql]
    type c1 = option(b1);

    [@deriving gql]
    type test = {
        a2: list(b1),
        b2: list(c1),
      };
    let name = "RecordAliasPartTest";
    let expected =
"type Test {
 a2: [A1!]!
 b2: [A1]!
}";
};