# Raccoonlang grammar

## Reference ANTLR grammar

```g4
grammar raccoonlang;

file
    : top_level_decl * 
    ;
    
top_level_decl
    : use
    | mod
    | fn
    ;
    
use
    : 'use' path (use_alias | multi_use)? ';'
    ;
    
use_alias
    :  'as' Identifier
    ;
    
multi_use
    : '::' {' multi_use_item (',' multi_use_item)* ','? '}'
    ;
    
multi_use_item
    : Identifier use_alias?
    ;
    
path
    : '::'? Identifier ('::' Identifier)*
    ;
    
mod
    : 'mod' path ';'
    ;

// TODO: make return_type optional
fn
    : 'pub'? 'fn' Identifier fn_parameter_list return_type block_expression
    ;
    
return_type
    : '->' type
    ;
    
fn_parameter_list
    : '(' (fn_parameter (',' fn_parameter)* ','?)? ')'
    ;
    
fn_parameter
    : Identifier ':' type
    ;
    
type
    : named_type
//     | tuple_type
//     | fn_type
    ;
    
named_type
    : path // TODO: generic_part
    ;
    
// tuple_type
//     :
//     ;
    
// fn_type
//     :
//     ;


block_expression
    : '{' expression? '}'
//    : '{' statement* expression? '}'
    ;
    
// statement
//     : 
//     ;
    
expression
    : literal_expression
//    | name_expression
    ;

// name_expression
//     : Identifier
//     ;

literal_expression
    : integer_literal
//    | float_literal
    ;
    
integer_literal
    : '-'? DecInteger // Identifier?
    : HexInteger // Identifier?
    : OctInteger // Identifier?
    : BinInteger // Identifier?
    ;
    
```

```
pub foobar () {
    let foo: u16 = 10u16;
}

```