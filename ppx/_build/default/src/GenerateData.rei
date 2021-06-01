open Ppxlib;
open RecTypes;

/*
* Generates internal variant datastructure which represents the nested input type
*
* @param  loc:location          location variable which is needed for any AST operation
* @param  type_declarations     raw AST structure which represents the type of the input AST, is either of type Ptype_abstract or Ptype_reord
* @error                        error is raised if type_declarations is not of type Ptype_abstract or Ptype_reord          
* @return                       the unpacked type_decaration represented in our internal datastructure of the type detectableTypes
*/

let generate_data: (location, type_declaration) => detectableTypes;