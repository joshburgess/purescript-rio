# Contributing to rio

Thanks for the interest. This file describes the conventions this project
follows so that contributions move quickly through review.

## Prerequisites

- Node.js 20 or 22.
- `npm install` once at the repository root brings in the pinned
  PureScript toolchain (`purs`, `spago`, `purs-tidy`) as local devDependencies.
  We do not rely on globally-installed versions.

## Local checks (everything CI runs)

```sh
npm install                                       # one time
npx purs-tidy check src test spikes               # format check
npx spago build -p rio                            # main package
npx spago test  -p rio                            # main tests
npx spago build -p spike-row-inference            # row-inference spike
npx spago build -p spike-aff-interruption         # interruption spike
npx spago run   -p spike-aff-interruption         # exercise interruption harness
```

`npm run format` reformats sources in place.

## Project structure

```
src/                         main rio package source
test/                        main rio package tests
spikes/<name>/               one workspace package per de-risking spike,
                             each with its own spago.yaml and FINDINGS.md
docs/                        user-facing guide content
examples/                    end-to-end example programs
benchmarks/                  benchmark suite (workspace package)
compile-fail/                negative tests for error-message quality
rio-*/                       adapter / integration packages
FUTURE_WORK.md               remaining open items relative to ZIO / Effect
```

## Submitting a change

1. Open a draft PR with a short, descriptive title (imperative mood:
   "Add ask primitive", not "Added").
2. Make sure your branch is rebased onto current `main`.
3. Run the local checks above; CI will run the same set.
4. Fill in the PR template.
5. Mark the PR ready for review once the checks pass.

A PR is ready for review when:

- Code compiles without warnings under the pinned `purs` version.
- Tests cover the happy path, at least one failure path, and at least one
  edge case.
- All new public functions have docstrings with at least one example.
- Format check is green: `npx purs-tidy check src test spikes`.
- `CHANGELOG.md` has an entry under `Unreleased`.
- User-facing items have an updated entry in `docs/`.

## Branches

- `main` is the integration branch and is protected.
- Feature branches: short kebab-case slugs (e.g. `add-channel-primitive`).
- Spike branches: `spike-<short-slug>` (e.g. `spike-rows-inference`).
- Fix branches: `fix-<short-slug>`.

## Commit messages

- Use the imperative mood: "Add ask primitive", not "Added" or "Adding".
- First line is a short summary (60 characters where possible).
- Body explains the "why" if it is not obvious from the diff.
- Group related changes into a single commit; avoid noisy fix-ups in the
  same PR (use interactive rebase before pushing if needed, except do not
  use `git rebase -i` in automated agent workflows since interactive rebase
  requires manual input).

## Code style

- `purs-tidy` is the source of truth for formatting; configuration is in
  `.tidyrc.json`. CI enforces it.
- Avoid adding type signatures inside the spike packages: the spikes are
  about what the compiler can infer. Production code under `src/` should
  carry type signatures on every public binding.
- Prefer composing existing primitives over adding new ones. Each new
  primitive should be justified by the change that introduces it.

## Adding dependencies

- Edit the relevant `spago.yaml` (main or per-spike) and add the dependency
  under `package.dependencies`.
- Run `npx spago build -p <package>` to refresh `spago.lock`.
- Commit both files.
- Prefer packages that are in the pinned `registry` set (`77.0.0`). If you
  need an `extraPackage`, justify the choice in the PR.

## Documentation

- Doc files live in `docs/`.
- API docstrings are required for every public binding (see "Definition of
  Done" below). One example per docstring.
- The migration guides (`docs/migrating-from-zio.md`,
  `docs/migrating-from-effect.md`) are intentionally code-snippet-heavy.
  Add new snippets there when an idiom doesn't already have a 1:1 mapping.
- The constraints doc (`docs/aff-constraints.md`) is the canonical
  statement of the `Aff` runtime ceiling. Update it if a change either
  raises or lowers what `rio` can do relative to `Aff`.

## Definition of Done

A change is **done** when:

1. Code compiles with no warnings under the pinned `purs` version.
2. All new public functions have docstrings with at least one example.
3. Tests cover happy path, at least one failure path, and at least one
   edge case.
4. CI is green on the PR branch.
5. `CHANGELOG.md` entry added under `Unreleased`.
6. If user-facing: relevant doc file in `docs/` is updated.

A spike is **done** when its findings document is written, reviewed, and
the recommended decision is recorded (kept or rejected).

## Versioning Policy

While in the `0.x` series, breaking changes may land in any minor release
(`0.1 -> 0.2`) without a deprecation cycle. Patch releases (`0.1.1`,
`0.1.2`) are always backwards-compatible bug fixes and additive changes.
The `1.0.0` release will commit to semver proper, with a documented
deprecation policy and a stable public API surface.

`spago.yaml` carries a placeholder version string while nothing is
published to the PureScript registry or Pursuit. Treat any pre-`1.0.0`
tag you see as a snapshot rather than a stability promise.

## Reporting issues

Open a GitHub issue using one of the templates (when they exist). For now,
free-form is fine. If your issue is about a remaining gap relative to
ZIO / Effect, see `FUTURE_WORK.md` for the live list.
