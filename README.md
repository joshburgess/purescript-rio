# rio

A ZIO/Effect-style library for PureScript. Tracks required services and possible errors in row types alongside `Aff`.

```purescript
newtype RIO r e a = RIO (Record r -> Aff (Either (Variant e) a))
```

Status: pre-alpha. See [`PROJECT_BUILD_PLAN.md`](./PROJECT_BUILD_PLAN.md) for the roadmap.

## Build

```sh
npm install
npx spago build
npx spago test
```
