open AliasTests;
open RecordTests;
open RuntimeTests;


 let () = Test_utils.test("Alias Tests", 
          [|(module AliasStringTest), (module AliasIntegerTest), (module AliasBoolTest), (module AliasRecordTest), (module AliasModuleTest), (module AliasModuleRecordTest),
            (module AddRecTypeTest), (module CheckIfRecTypeTest),
            (module RecordBasicTest), (module RecordAliasPartTest), (module RecordAliasTest), (module RecordModuleTest)
          |]);