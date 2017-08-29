# language-javascript-wl-pprint

Note: this is experimental.

Rework of the original language-javascript library using wl-pprint (https://hackage.haskell.org/package/wl-pprint).

## Usage

Example:

```
stack ghci
```

```
js <- parseFile "..."

prettyPrint js

putStrLn $ prettyPrint js

```
