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
 
Further information and sources can be found at:

http://www.cse.chalmers.se/research/group/Language-technology/BNFC/

Bnfc has to be installed in order to use bnfcCabal.

## 1.2) BnfcCabal

BnfcCabal is a cabal file creating tool for labeled BNF grammars. BnfcCabal runs bnfc on a given labeled grammar
and automatically builds a cabal file containing all modules created by bnfc. 

# 2) Installing BnfcCabal

BnfcCabal can be installed using 

```Bash
cabal install bnfcCabal
```

# 3) Using BnfcCabal

Before using BnfcCabal ensure the executable `bnfc` can be found in system PATH.

## 3.1) How to build a cabal file for your own grammar

We use a grammar named `mygrammar` as a running example in this and the subsequent section.

1. Create a labeled grammar like `mygrammar.cf` 

2. Launch `bnfcCabal mygrammar.cf`
  
3. Type `cabal install` in order to install the bnfc modules

## 3.2) Using installed modules

 After running `bnfcCabal mygrammar.cf` (as shown in 3.1) the modules of a grammar `mygrammar` can be found as submodules of
`Language.Mygrammar`. In order to use the parser of 
`mygrammar` the following modules have to be imported:
```Haskell
import Language.Mygrammar.Parmygrammar
import Language.Mygrammar.Absmygrammar
import Language.Mygrammar.ErrM
```

Module `ErrM` contains a error monad to handle parse errors, whereas `Absmygrammar` contains the abstract syntax.
The module `Parmygrammar` contains a lexer and parse functions. The lexer 
function is always called `myLexer`, but the parsing function is named
after the root of your grammar. Since you can define multiple entry points for your grammar, there may also exist multiple parsers. Assume data type `Myroot` is one possible entry point of
 `mygrammar`. The function
```Haskell 
(pMyroot . myLexer) :: String -> Err Root
```
is the correct parsing function for data type `Myroot`. Have a look at module
`Language.Mygrammar.ErrM` to get an impression of the error monad.

## 3.3) Example: How to build and use the c grammar?
    
1. Grammar `Examples/c.cf` is already existing
2. Change to directory `Examples` 
3. Type `bnfcCabal c.cf` to run bfnc on the grammar and build a cabal file containing all generated modules
4. To install grammar type `cabal install` 
5. The following code snippet shows how to define a parsing function accepting c programs:
```Haskell
import Language.C.Parc
import Language.C.Absc
import Language.C.ErrM

parseProgram :: String -> Err Program
parseProgram = pProgram . myLexer
```
Alternatively one can also use the generated parsers `pStm` and `pExp` to parse statements or expressions respectively. This is possible because the grammar defined in `c.cf` defines entry points for c programs, statements and expressions.

# 4) To Do
    
* [BUG] Module `Main` (in file `Testmygrammar`) is not being installled 

* check whether link to bnfc binary is correct (preConf-hook)
      
