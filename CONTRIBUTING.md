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
npx spago build -p spike-row-inference            # spike 0.4
npx spago build -p spike-aff-interruption         # spike 0.5
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
bench/                       benchmark suite (Phase 8.4)
PROJECT_BUILD_PLAN.md        roadmap (phases, work items, acceptance criteria)
```

## Work items

Work proceeds against `PROJECT_BUILD_PLAN.md`. Each item in the plan is sized
to be a single PR. Before starting:

1. Pick an item from the plan whose acceptance criteria you understand.
2. Confirm the item is unblocked (its phase's prerequisites are merged).
3. Open a draft PR titled after the item ID, e.g. `2.1 ask and asks primitives`.

A PR is ready for review when:

- Code compiles without warnings under the pinned `purs` version.
- Tests cover the happy path, at least one failure path, and at least one
  edge case.
- All new public functions have docstrings with at least one example.
- Format check is green: `npx purs-tidy check src test spikes`.
- `CHANGELOG.md` has an entry under `Unreleased`.
- User-facing items have an updated entry in `docs/`.
- The PR description references the work-item ID from
  `PROJECT_BUILD_PLAN.md`.

## Branches

- `main` is the integration branch and is protected.
- Feature branches: `phase-<n>.<m>-<short-slug>`, e.g. `phase-2.1-ask`.
- Spike branches: `spike-<n>.<m>-<short-slug>`, e.g. `spike-0.4-rows`.
- Fix branches: `fix-<short-slug>`.

## Commit messages

- Use the imperative mood: "Add ask primitive", not "Added" or "Adding".
- First line is a short summary (60 characters where possible).
- Body explains the "why" if it is not obvious from the diff.
- Reference the work-item ID at the top of the body, e.g. `Phase 2.1`.
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
  primitive should be justified by the work item that introduces it.

## Adding dependencies

- Edit the relevant `spago.yaml` (main or per-spike) and add the dependency
  under `package.dependencies`.
- Run `npx spago build -p <package>` to refresh `spago.lock`.
- Commit both files.
- Prefer packages that are in the pinned `registry` set (`77.0.0`). If you
  need an `extraPackage`, justify the choice in the PR.

## Documentation

- Doc files live in `docs/` and are numbered by phase.
- API docstrings are required for every public binding (Definition of Done
  in the plan). One example per docstring.
- The migration guides (`docs/migrating-from-zio.md`,
  `docs/migrating-from-effect-ts.md`) are intentionally code-snippet-heavy.
  Add new snippets there when an idiom doesn't already have a 1:1 mapping.

## Reporting issues

Open a GitHub issue using one of the templates (when they exist). For now,
free-form is fine. Include the work-item ID if the issue is about
something the plan has already named.
