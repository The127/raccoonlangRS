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
    'use' path (single_use | multi_use) ';'
    ;
    
single_use
    :  ('as' identifier)?
    ;
    
multi_use
    : '::' {' multi_use_item (',' multi_use_item)* ','? '}'
    ;
    
multi_use_item
    : identifier ('as' identifier)?
    ;
    
path
    : '::'? identifier ('::' identifier)*
    ;
    
mods:
    mod*
    ;
    
mod: 
    'mod' ';'
    ;
```

```
foo::bar ::(1+2)::asdf

```