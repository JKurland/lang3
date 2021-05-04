add struct parser
add member access
    * "." token
    * "." operator
    * member specifier ast node - allowed on left of assignment or in expression
    * member access cfg instruction (still needs to be in terms of member name)
    * extend type inference to deal with member access instruction
    * when creating vm program need to lookup offset for member name in struct
allow member setting
    * When left of assignment is a member specifier
