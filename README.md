<p align="left"><img src="http://takenobu-hs.github.io/downloads/images/haskell-logo-s.png"/></p>

Haskell/GHC symbol search cheatsheet
====================================

Several features of Haskell/GHC have low googleability.
Because some of them are composed of symbols :)  
This page is a reference collection to support search of them.

If you want to search for function symbols like `<*>`, `$`, `>>=`, ..., you can use the following search engines:
  * [Hoogle search on Stackage](https://www.stackage.org)
  * [Hoogle on Haskell.org](https://www.haskell.org/hoogle)
  | [Hoogle 5 on Haskell.org](https://hoogle.haskell.org/)

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
`$( )` : Template Haskell’s splice syntax
------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#template-haskell) \]

```Haskell
two = $(add1 1)
```


---
`'` : promoted constructors are prefixed by a tick '
-------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#datatype-promotion) \]

```Haskell
type * = TYPE 'LiftedRep
```


---
`(#  #)` : "unboxed tuple"
-------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#unboxed-tuples) \]

```Haskell
f x y = (# x+1, y-1 #)
```


---
`->` : "view pattern"
-------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#view-patterns) \]

```Haskell
size (view -> Unit) = 1
size (view -> Arrow t1 t2) = size t1 + size t2
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
`;` : semicolon in Layout rule
------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-210002.7) \]


```Haskell
f x = let a = 1; b = 2  
          g y = exp2  
      in exp1 
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
`[|  |]` : Template Haskell’s quotation syntax
------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#template-haskell) \]

```Haskell
add1 x = [| x + 1 |]
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
\[ [GHC User’s Guide](http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-warnings.html#ghc-flag--Wunused-binds) \] \[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-180002.4) \]

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
`{..}` : "record wildcard"
------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#record-wildcards) \]


```Haskell
f Vec{..} = 
```


---
`|` : "functional dependencies"
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
