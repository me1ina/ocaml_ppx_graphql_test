# Basic Reason Template

Hello! This project allows you to quickly get started with ReScript using Reason syntax.

# Build

```bash
# for yarn
yarn build

# for npm
npm run build
```

# Build + Watch

```bash
# for yarn
yarn start

# for npm
npm run start
```
# Notes about PPX

Rescript 8 renamed it's ppx from `@bs.deriving` to `@deriving` which overrules any other derivers added by `ppx-lags` in `bsconfig.json`. (see [PR](https://github.com/rescript-lang/rescript-compiler/pull/4701))  
Another [PR](https://github.com/rescript-lang/rescript-compiler/pull/4911) relaxed the prviously fatal error (when trying to use another `@deriving` than rescript's) to be a warning. But it's still not possible
to use any other deriver.

Our ppx works until `bs-platform@7.3.2`. To make it usable in bs-platform 8 and above we need to find a way to call the ppx by just `@gql`.

`bs-deriving` isn't using ppxlib (directly?) but faced the same issue and solved it in [this PR](https://github.com/ELLIOTTCABLE/bs-deriving/pull/9).
