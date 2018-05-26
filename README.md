<p align="left"><img src="http://takenobu-hs.github.io/downloads/images/haskell-logo-s.png"/></p>

Haskell/GHC symbol search cheatsheet
====================================

Several features of Haskell/GHC have low googleability.
Because some of them are composed of symbols :)  
This page is a reference collection to support search of them.

If you want to search for function symbols like `<*>`, `$`, `>>=`, ..., you can use the following search engines:
  * [Hoogle search on Stackage](https://www.stackage.org)
  * [Hoogle on Haskell.org](https://www.haskell.org/hoogle)

Happy Haskelling!


---
`!` : "strictness flag"
-------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html) \]

```Haskell
data Vec = Vec !Int
```


---
`!` : "bang pattern"
-------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=bang%20pattern#bang-patterns-informal) \]

```Haskell
f1 !x = 
```


---
`#` : "MagicHash"
-------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=magichash#the-magic-hash) \]

```Haskell
data Int = I# Int#
```


---
`$( )` : Template Haskell’s splice syntax
------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=template%20haskell#template-haskell) \]

```Haskell
two = $(add1 1)
```


---
`'` : promoted constructors are prefixed by a tick '
-------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=data%20promotion#datatype-promotion) \]

```Haskell
type * = TYPE 'LiftedRep
```


---
`(#  #)` : "unboxed tuple"
-------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=unbox%20tuple#unboxed-tuples) \]

```Haskell
f x y = (# x+1, y-1 #)
```


---
`->` : "view pattern"
-------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=unbox%20tuple#unboxed-tuples) \]

```Haskell
size (view -> Unit) = 1
size (view -> Arrow t1 t2) = size t1 + size t2
```


---
`@` : "as pattern"
-------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html) \]

```Haskell
f s@(x:xs) = 
```


---
`@` : "type application"
------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=type%20application#extension-TypeApplications) \]


```Haskell
f = read @Int
```


---
`[|  |]` : Template Haskell’s quotation syntax
------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=template%20haskell#template-haskell) \]

```Haskell
add1 x = [| x + 1 |]
```


---
`_` : "wildcard pattern"
-------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html) \]

```Haskell
f Red  =
f Blue =
f _    =
```


---
`{..}` : "record wildcard"
------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=record%20wild%20card#record-wildcards) \]


```Haskell
f Vec{..} = 
```


---
`|` : "functional dependencies"
------------------
\[ [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=functionaldependencies#functional-dependencies) \]


```Haskell
class Foo a b c | a b -> c where 
```


---
`~` : "irrefutable pattern"
-------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch10.html) \]

```Haskell
f1 ~(as,bs) =
```


