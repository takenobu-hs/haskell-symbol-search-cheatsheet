<p align="left"><img src="http://takenobu-hs.github.io/downloads/images/haskell-logo-s.png"/></p>

Haskell/GHC symbol search cheatsheet
====================================

Several features of Haskell/GHC have low googleability.
Because some of them are composed of symbols :)  
This page is a reference collection to support search of them.

If you want to search for function symbols like `.`, `$`, `>>=`, `<*>`, ..., you can use the following search engines:
  * [Hoogle search on Stackage](https://www.stackage.org)
  * [Hoogle on Haskell.org](https://hoogle.haskell.org/)
  * [Aelve Codesearch](https://codesearch.aelve.com/haskell) (used "in the wild", and regex support)

Happy Haskelling!


---
`!` : "strictness flag"
-------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-680004.2) \]

```Haskell
data Vec = Vec !Int
```


---
`!` : "bang pattern"
-------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#bang-patterns-informal) \]

```Haskell
f1 !x = 
```


---
`#` : "MagicHash"
-------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#the-magic-hash) \]

```Haskell
data Int = I# Int#
```


---
`#` : "OverloadedLabels"
-------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#overloaded-labels) \]

```Haskell
example = #x (Point 1 2)
```


---
`#` : C pre-processor's directive
-------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/phases.html#options-affecting-the-c-pre-processor) \]

```Haskell
#include "MachDeps.h"
```


---
`#` : hsc2hs command's operator
-------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/utils.html#input-syntax) \]

```Haskell
flag = #const VER_MAJORVERSION
```


---
`$( )` : Template Haskell’s splice syntax
------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#template-haskell) \]

```Haskell
two = $(add1 1)
```


---
`$$( )` : Typed Template Haskell’s splice syntax
------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#template-haskell) \]

```Haskell
two = $$(add1 1)
```


---
`'` : an identifier consists of a letter followed by zero or more letters, digits, underscores, and single quotes
-------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-180002.4) \]

```Haskell
xs' = f ys
```


---
`'` : promoted constructors are prefixed by a tick '
-------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#datatype-promotion) \]

```Haskell
type * = TYPE 'LiftedRep
```


---
`'` `''` : Template Haskell’s quotation syntax
-------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/%7Eghc/latest/docs/html/users_guide/glasgow_exts.html#th-syntax) \]

```Haskell
makeLenses ''FooBar
```


---
`()` : "unit type"
-------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-650004.1.2) \]
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1220006.1.5) \]

```Haskell
main :: IO ()
```


---
`()` : "unit expression"
-------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-380003.9) \]
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1220006.1.5) \]

```Haskell
return ()
```


---
`( )` : "section" - a convenient syntax for partial application
-------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-300003.5) \]

```Haskell
add1 = (1+)
```


---
`(,)` : the constructor for a tuple
-------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-360003.8) \]

```Haskell
f x y = liftM2 (,) x y 
```


---
`(, xxx)` : "TupleSections"
-------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-TupleSections) \]

```Haskell
f xs = fmap (, True) xs
```


---
`(#  #)` : "unboxed tuple"
-------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#unboxed-tuples) \]

```Haskell
f x y = (# x+1, y-1 #)
```

---
`(# | | #)` : "unboxed sum"
-------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#unboxed-sums) \]

```Haskell
f :: (# Int | Bool | Char #) -> Int
f (# x | | #)    = 1
f (# | True | #) = 2
f _              = 3
```


---
`(..)` : export all of its names
-------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-1000005.2) \]

```Haskell
module GHC.Arr (
        Ix(..),
```


---
`(..)` : import all of its names 
-------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-1010005.3) \]

```Haskell
import GHC.Types (Bool(..))
```


---
`*` : the kind of ordinary types (synonym for `Type` and ``TYPE `LiftedRep``)
-------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-640004.1.1) \]
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#the-kind) \]
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#levity-polymorphism) \]

```Haskell
ghci> :kind Int
Int :: *
```


---
`->` : case expression
-------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-460003.13) \]

```Haskell
f x = case x of
        Nothing -> False
        Just _  -> True
```


---
`->` : "view pattern"
-------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#view-patterns) \]

```Haskell
size (view -> Unit)        = 1
size (view -> Arrow t1 t2) = size t1 + size t2
```


---
`->` : "function type"
-------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-650004.1.2) \]

```Haskell
id :: a -> a
```


`.` : module names are a dot-separated sequence
------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-180002.4) \]

```Haskell
import Data.Maybe
import qualified Text.Read.Lex as L

lexP = lift L.lex
```


`.` : universal quantification
------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#explicit-universal-quantification-forall) \]

```Haskell
f :: forall a. a -> [a]
```


---
`:` : "list constructor" (cons)
-------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-340003.7) \]
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1200006.1.3) \]
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-180002.4) \]

```Haskell
f x xs = x:xs
```


---
`:` : an operator symbol starting with a colon is a constructor
-------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-180002.4) \]

```Haskell
data NonEmpty a = a :| [a]
```


---
`::` : "type signature"
-------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-810004.4.1) \]

```Haskell
id :: a -> a
id x =  x
```


---
`::` : "expression type-signature" (type annotation)
-------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-560003.16) \]

```Haskell
x = fromIntegral (maxBound::Int)
```


---
`;` : semicolon in layout rule
------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-210002.7) \]


```Haskell
f x = let a = 1; b = 2  
          g y = exp2  
      in exp1 
```


---
`<-` : lambda-bound in do expression
-------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-470003.14) \]

```Haskell
f = do
  x <- getLine
  putStrLn x
```


---
`<-` : "pattern guard"
------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-830004.4.3) \]


```Haskell
f x
  | Just y <- g x = 
```


---
`=>` : context (type class constraint)
-------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-660004.1.3) \]

```Haskell
subtract :: (Num a) => a -> a -> a
subtract x y = y - x
```


---
`?` : "ImplicitParams"
-------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-ImplicitParams) \]

```Haskell
sort :: (?cmp :: a -> a -> Bool) => [a] -> [a]
sort = sortBy ?cmp
```


---
`@` : "as pattern"
-------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-580003.17) \]

```Haskell
f s@(x:xs) = 
```


---
`@` : "type application"
------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-TypeApplications) \]

```Haskell
f = read @Int
```


---
`[]` : "empty list" (nil)
-------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-340003.7) \]
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1200006.1.3) \]

```Haskell
null [] = True
null _  = False
```


---
`[ .. ]` : "arithmetic sequence"
-------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-400003.10) \]

```Haskell
xs = [1..10]
```


---
`[ | <- ]` : "list comprehension"
-------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-420003.11) \]

```Haskell
xs = [x^2 | x <- [1..10]] 
```


---
`[|  |]` : Template Haskell’s quotation syntax
------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#template-haskell) \]

```Haskell
add1 x = [| x + 1 |]
```


---
`[||  ||]` : Typed Template Haskell’s quotation syntax
------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#template-haskell) \]

```Haskell
add1 x = [|| x + 1 ||]
```


---
`_` : "wildcard pattern"
-------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-580003.17) \]

```Haskell
f Red  =
f Blue =
f _    =
```


---
`_` : unused identifiers beginning with underscore
-------------------
\[ [GHC User’s Guide](http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-warnings.html#ghc-flag--Wunused-binds) \]
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-180002.4) \]

```Haskell
_w = True                -- No warning: _w starts with an underscore
```


---
`_` : "typed hole"
-------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#typed-holes) \]

```Haskell
sum xs = foldr _ 0 xs
```


---
`_` : "NumericUnderscores"
-------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#numeric-underscores) \]

```Haskell
million = 1_000_000
```


---
`\ ->` : "lambda abstraction"
-------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-260003.3) \]

```Haskell
add1 = \x -> x + 1
```


---
`\case ->` : "LambdaCase"
-------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#lambda-case) \]

```Haskell
f = \case
      Red  -> 2
      Blue -> 1
      _    -> 0
```


---
`` ` ` `` : "infix notation" - an identifier enclosed in grave accents
-------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-240003.2) \]

```Haskell
div10 x = x `div` 10
```


---
`{ }` : brace in layout rule
-------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-210002.7) \]

```Haskell
f x = case x of {Nothing -> False; Just _ -> True}
```


---
`{ }` : "record syntax" (datatypes with field labels)
-------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-490003.15) \]

```Haskell
data MyPoint = Point { x :: Int, y :: Int }
```


---
`{..}` : "record wildcard"
------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#record-wildcards) \]

```Haskell
f Vec{..} = 
```


---
`{-#  #-}` : "compiler pragma"
------------------

\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch12.html#x19-18800012) \]
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#pragmas) \]
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#language-options) \]

```Haskell
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs            #-}

{-# INLINE fmap #-}
 ```


---
`|` : "boolean guard" (guard)
------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-830004.4.3) \]

```Haskell
clip255 x
  | x > 255   = 255
  | otherwise = x 
```


---
`|` : "MultiWayIf"
-------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#multi-way-if-expressions) \]

```Haskell
if | x == ":q" -> quit
   | isError x -> errorExit x
   | otherwise -> execCommand x
```


`|` : algebraic datatype declaration
------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-690004.2.1) \]

```Haskell
data Maybe a = Nothing | Just a
```


---
`|` : "functional dependency"
------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#functional-dependencies) \]

```Haskell
class Foo a b c | a b -> c where 
```


---
`~` : "irrefutable pattern"
-------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-580003.17) \]

```Haskell
f1 ~(as,bs) =
```


---
`~` : "equality constraint"
-------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/%7Eghc/latest/docs/html/users_guide/glasgow_exts.html#equality-constraints) \]

```Haskell
class (F a ~ b) => C a b where
```
