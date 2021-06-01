open Ppxlib;
open RecTypes;

/*
* Creates a structure item list which includes the variable which holds the GraphQL schema string and the handling of the record type set
*
* @param  loc:location                      location variable which is needed for any AST operation
* @param  generated_data:detectableTypes    the input type represented in our internal data structure
* @param  type_name:label                   the name of the input type
* @param  name:label                        the name of the input type can also be manually defined by an overload, can also be an empty string
* @return                                   the structure item list which includes the variable which holds the GraphQL schema string and the handling 
                                            of the record type set
*
*/
let create_structure_item_list:
  (location, detectableTypes, label, label) => list(structure_item);

/*
* Creates a structure item list which includes the variable which holds the type value of the abstract type
*
* @param  loc:location                      location variable which is needed for any AST operation
* @param  generated_data:detectableTypes    the input type represented in our internal data structure
* @param  type_name:label                   the name of the input type
* @return                                   the structure item list which includes the variable which holds the type value of the abstract type
*/
let create_abstract_structure_item:
  (location, detectableTypes, label) => list(structure_item);