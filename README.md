# GUS REGON (BIR 1.1) API Client

## Usage

An API key is required to access the production version of the database. See: https://api.stat.gov.pl/Home/RegonApi

This library provides two interfaces:

* Web.BIR.BIR11.Methods - IO
* Web.BIR.BIR11.Methods.Lifted - lifted

For the lifted interface you will need to instantiate `HasBirState` and provide both `MonadIO` and `MonadError Bir11Error`:

```
class HasBirState m where
  getBirApiUrl :: m String
  getBirApiKey :: m ApiKey
  getBirSessionKey :: m SessionKey
  putBirSessionKey :: SessionKey -> m ()

type MonadBir m = (MonadIO m, MonadError Bir11Error m, HasBirState m)
```

## Tests

Test suite contains basic tests against the non-production version of the API.

To run the test suite:
```
nix-shell
cabal v2-test
```

## License

MIT. See LICENSE file.
