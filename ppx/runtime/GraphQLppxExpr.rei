/*
* Check if the last letter of the type name is corresponding to the coorect option status. Important bug fix which solves errors from referencing option types
*
* @param  type_name:string   the type name which needs to be checked
* @param  opt:bool           if true, the first parameter should be an option (-> last letter should not be a !), if false the last letter should be !
* @return                    a string with the correct last letter (-> ! if option, no ! if not option)
*/

let check_if_option:(string, bool) => string;