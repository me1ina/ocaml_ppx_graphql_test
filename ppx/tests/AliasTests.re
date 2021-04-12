module AliasStringTest = {
    [@deriving gql]
    type test = string;
    let name = "AliasStringTest";
    let expected = "String!";
};

module AliasIntegerTest = {
    [@deriving gql]
    type test = array(option(option(int)));
    let name = "AliasIntegerTest";
    let expected = "[Integer]!";
};

module AliasBoolTest = {
    [@deriving gql]
    type test = list(bool);
    let name = "AliasBoolTest";
    let expected = "[Bool!]!";
};

module AliasRecordTest = {
    [@deriving gql]
    type a = {
    a: int,
    b: option(string),
    };
    [@deriving gql]
    type test = a;
    let name = "AliasRecordTest";
    let expected = "A!";
};

module AliasModuleTest = {
    [@deriving gql]
    type test = array(ModuleTest.d);
    let name = "AliasModuleTest";
    let expected = "[[Bool!]!]!";
};

module AliasModuleRecordTest = {
    [@deriving gql]
    type test = option(ModuleTest.a);
    let name = "AliasModuleRecordTest";
    let expected = "ModuleTest_A";
};