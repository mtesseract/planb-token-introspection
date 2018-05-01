# Token Introspection for PlanB [![Hackage version](https://img.shields.io/hackage/v/planb-token-introspection.svg?label=Hackage)](https://hackage.haskell.org/package/planb-token-introspection) [![Stackage version](https://www.stackage.org/package/planb-token-introspection/badge/lts?label=Stackage)](https://www.stackage.org/package/planb-token-introspection) [![Build Status](https://travis-ci.org/mtesseract/planb-token-introspection.svg?branch=master)](https://travis-ci.org/mtesseract/planb-token-introspection)

This package provides token introspection functionality for
[PlanB](http://planb.readthedocs.io/en/latest/).

## Example

```haskell
printTokenInfo :: ByteString -> IO ()
printTokenInfo token = do
  introspector <- PlanB.new "https://planb-endpoint"
  tokenInfo <- PlanB.introspectToken introspector token
  print tokenInfo
```

If the PlanB introspection endpoint to use can be retrieved from the
environment variable `PLANB_INTROSPECTION_ENDPOINT`, then one can
alternatively use

```haskell
  introspector <- PlanB.newFromEnv
```

for creating the PlanB introspector.
