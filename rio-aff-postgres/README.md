# rio-aff-postgres

Postgres adapter for [`rio-aff`](../rio-aff/README.md), built on top of
[`purescript-postgresql`](https://pursuit.purescript.org/packages/purescript-postgresql)
(the `node-postgres` / `pg` driver).

```purescript
import RIO.Aff.Core (RIO, runRIO)
import RIO.Aff.Postgres (Postgres, PgError, query)
import RIO.Aff.Postgres.Layer (postgresLayer)
import RIO.Aff.Layer (provideLayer)
import Type.Proxy (Proxy(..))

countTodos
  :: forall e
   . RIO (postgres :: Postgres)
      (db :: PgError | e)
      Int
countTodos = query (Proxy :: Proxy "db") "select count(*) from todo"

main = launchAff_ do
  let layer = postgresLayer { connectionString: "postgres://..." }
  result <- runRIO (provideLayer layer countTodos)
  ...
```

`postgresLayer` allocates a fresh pool on each
`provideLayer` and registers the pool's shutdown as a
finalizer on the surrounding scope, so the pool drains on
every exit path (success, typed failure, defect, kill).

`Postgres` is a single service token under the conventional
label `postgres`. Combinators like `query`, `exec`, and
`withTransaction` ask for it implicitly and surface driver
errors on a caller-chosen typed tag carrying `PgError`. A
typical application wraps these once with its preferred tag
the same way the
[todo-api example](../examples/todo-api/) does with
`rio-aff-http`'s `requireAuth`.

## What's wrapped

- Pool lifecycle (`Pool.make` / `Pool.end`) is managed by
  `postgresLayer`.
- Per-call client lifecycle (`Pool.connect` / `Pool.release`)
  is managed by `withClient`, which brackets the inner
  callback so the client is returned to the pool on every
  termination path.
- `query`, `exec`, and the in-transaction `queryUsing` /
  `execUsing` variants forward to
  `Effect.Aff.Postgres.Client.query` / `.exec`, lifting their
  `Except Aff` failures onto a typed-failure tag.
- `withTransaction` issues `BEGIN`, runs the body, and either
  `COMMIT`s on success or `ROLLBACK`s on any typed failure (it
  catches the whole error row, not just the chosen tag) then
  re-raises. Defects (Aff exceptions) skip the rollback path.

## Testing

Integration tests live under `rio-aff-postgres/test/` and run
against a real Postgres reached via `PG_CONNECTION_STRING`.
For local runs, bring up the workspace's `docker-compose.yml`
service:

```sh
docker compose up -d postgres
export PG_CONNECTION_STRING=postgres://rio:rio@localhost:55432/rio_test
npx spago test -p rio-aff-postgres
```

CI runs the same suite in the `postgres-integration` job
against a service container, alongside the `rio-aff-postgres-json`,
`rio-aff-postgres-migrate`, and `rio-example-notify` integration
runs.

If `PG_CONNECTION_STRING` is unset the suite is skipped (printed
as a pending case) so contributors who don't have Postgres
handy can still run the rest of the workspace.
