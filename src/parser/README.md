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
    'use' ';'
    ;
    
mods:
    mod*
    ;
    
mod: 
    'mod' ';'
    ;
```