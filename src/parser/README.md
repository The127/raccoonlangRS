# Raccoonlang grammar

## Reference ANTLR grammar

```g4
grammar raccoonlang;

file:
    uses mods 
    ;
    
uses:
    use*
    ;
    
use:
    'use' path (use_alias | multi_use)? ';'
    ;
    
use_alias
    :  'as' identifier
    ;
    
multi_use
    : '::' {' multi_use_item (',' multi_use_item)* ','? '}'
    ;
    
multi_use_item
    : identifier use_alias?
    ;
    
path
    : '::'? identifier ('::' identifier)*
    ;
    
mods:
    mod*
    ;
    
mod: 
    'mod' path ';'
    ;
```

```
use foo::bar::{};
use foo::bar;

```