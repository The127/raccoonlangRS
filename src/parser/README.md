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
    : 'pub'? 'fn' Identifier fn_parameter_list return_type fn_body
    ;
    
return_type
    : '->' type
    ;

fn_body
    : '{' '}' // TODO: impl
    
fn_parameter_list
    : '(' fn_parameter (',' fn_parameter)* ','? ')'
    ;
    
fn_parameter
    : Identifier ':' type
    ;
    
type
    : namedType
//     | tupleType
//     | fnType
    ;
    
namedType
    : path // TODO: genericPart
    ;
    
// tupleType
//     :
//     ;
    
// fnType
//     :
//     ;
    
```

```
pub foobar () {} fn

```