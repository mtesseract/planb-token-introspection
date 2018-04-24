# Token Introspection for PlanB

This package provides token introspection functionality for
[PlanB](http://planb.readthedocs.io/en/latest/).

## Example

```haskell
printTokenInfo :: ByteString -> IO ()
printTokenInfo token = do
  conf <- newConfIO "https://planb-endpoint"
  tokenInfo <- introspectToken conf token
  print tokenInfo
```
