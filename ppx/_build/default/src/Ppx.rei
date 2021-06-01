/*
* Generates internal variant datastructure which represents the nested input type
*
* @error                        error is raised if type_declarations is not of type Ptype_abstract or Ptype_reord          
* @return                       the unpacked type_decaration represented in our internal datastructure of the type detectableTypes
*/

let my_deriver: Ppxlib.Deriving.t;