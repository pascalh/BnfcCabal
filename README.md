# 1) Introduction

## 1.1) BNF Converter

The BNF Converter is a compiler construction tool generating a 
compiler front-end from a labeled BNF grammar. It is currently
 able to generate representations for several languages. Given
 a labeled BNF grammar BNFC generates:
 
   * an abstract syntax implementation
   * an Alex, JLex or Flex lexer generator file
   * an Happy, CUP, or Bison parser generator file
   * a pretty printer
   * a LaTeX file containing a readable specification of the language
 
Further informations and sources can be found at:

http://www.cse.chalmers.se/research/group/Language-technology/BNFC/

Bnfc has to be installed in order to use bnfcCabal.

## 1.2) BnfcCabal

BnfcCabal is a cabal file creating tool for labeled BNF grammars of 
file extension *.cf. BnfcCabal runs bnfc on a given labeled grammar
and automatically builds a cabal file containing all modules bnfc 
produces. 

# 2) Installing BnfcCabal

BnfcCabal can be installed using 

```Bash
cabal install bnfcCabal
```

# 3) Using BnfcCabal

## 3.1) How to build a cabal file for your own grammar

1. Create a labeled grammar like `mygrammar.cf` 

2. Launch `bnfcCabal mygrammar.cf`
  
3. Type `cabal install` in order to install the bnfc modules

## 3.2) Example: How to build the c grammar?
    
1. Grammar `Examples/c.cf` is already existing
2. Chane to directory `Examples` 
3. Type `bnfcCabal c.cf` to build a cabal file for c grammar
4. To install grammar type `cabal install` 
    
See Section (3.3) how to use installed modules

## 3.3) Using installed modules

We use a grammar named `mygrammar` as a 
running example in this section. After running `bnfcCabal mygrammar.cf` the modules of a grammar `mygrammar` can be found as submodules of
`Language.Mygrammar`. In order to use the parser of 
`mygrammar` some modules have to be imported:
```Haskell
import Language.Mygrammar.Parmygrammar
import Language.Mygrammar.Absmygrammar
import Language.Mygrammar.ErrM
```

Module `ErrM` contains a error monad to handle parse errors, whereas `Absmygrammar` contains the abstract syntax.
The module `Parmygrammar` contains lexer and parse functions. The lexer 
function is always `myLexer`, but the parsing function is named
after the root of your grammar. Assume data type `Myroot` is the root of
 `mygrammar`. The function
```Haskell 
(pMyroot . myLexer) :: String -> Err Root
```
is the correct parsing function for data type `Myroot`. Have a look at module
`Language.Mygrammar.ErrM` to get an impression of the error monad.

# 4) To Do
    
* [BUG] Module `Main` (in file `Testmygrammar`) is not being installled 

* check whether link to bnfc binary is correct (preConf-hook)
      
