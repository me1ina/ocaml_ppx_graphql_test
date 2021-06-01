/*
* This function adds a deriver which is defined by a string argument (by which it can be referred by a user) and a generator 
* which takes an input AST, in our case the structure item of a type declaration, and returns an output AST
*        
*/

let my_deriver: Ppxlib.Deriving.t;