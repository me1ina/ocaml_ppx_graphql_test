type recordItem = {
    propertyName: string,    
    type_: detectableTypes    
}    
and detectableTypes =
    | Alias(string)
    | Module(string, string)
    | Array(detectableTypes)
    | Option(detectableTypes)
    | Record(list(recordItem))
    | String
    | Integer
    | Bool;