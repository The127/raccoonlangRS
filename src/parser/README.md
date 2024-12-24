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


// fn_parameter (',' fn_parameter)* ','? 

fn_parameter_list
    : '(' ')'
    ;

fn_body
    : '{' '}'

fn
    : 'pub'? 'fn' Identifier fn_parameter_list fn_body
    ;
```

```
use foo::bar::{};
use foo::bar;

```