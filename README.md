# Token Introspection for PlanB

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
