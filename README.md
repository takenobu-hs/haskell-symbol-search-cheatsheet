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

`@` : "as-pattern"
-------------------
\[ [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html) \]

```Haskell
f xxs@(x:xs) = ...
```

---

`@` : "TypeInType"
------------------
\[ [GHC User’s Guide](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html) \]
\[ [Haskell 2010 Language Report](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide) \]


```Haskell
f xxs@(x:xs) = ...
```
