# GUS REGON (BIR 1.1) API Client

## Usage

An API key is required to access the production version of the database. See: https://api.stat.gov.pl/Home/RegonApi

This library provides two interfaces:

* Web.BIR.BIR11.Methods - IO
* Web.BIR.BIR11.Methods.Lifted - lifted

Lifted interface requires `HasBirState`:

```
class HasBirState m where
  getBirApiUrl :: m Text
  getBirApiKey :: m ApiKey
  getBirSessionKey :: m SessionKey
  putBirSessionKey :: SessionKey -> m ()

type MonadBir m = (MonadUnliftIO m, HasBirState m)
```

## Tests

Test suite contains basic tests against the non-production version of the API.

To run the test suite:
```
nix develop
cabal test
```

## License

MIT. See LICENSE file.
