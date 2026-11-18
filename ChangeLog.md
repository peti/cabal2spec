# Revision history for cabal2spec

## 2.9.0 (unreleased)

- Generated spec files now wrap every reference to profiling packages in
  `%if %{with ghc_prof}`: the `-prof` build dependencies, including those
  of the test suite; the `-prof` sub-package; and its `%files` section.
  Packages now build even when profiling is disabled.
- Generated spec files require `ghc-rpm-macros >= 2.9.2`.

## 2.8.2 (2026-11-15)

- Revert the `%{with ghc_prof}` guards that were accidentally added in 2.8.1.
  This change requires a major version bump and should not have landed in 2.8.x.

## 2.8.1 (2026-11-15): DEPRECATED on hackage.haskell.org

- Support Cabal 3.16.x and 3.18.x.
- Wrap all references to profiling packages in `%if %{with ghc_prof}`.

## 2.8.0 (2025-07-23)

- Support Cabal 3.12 and later, which uses the new `SymbolicPath` type.

## 2.7.1 (2024-03-26)

- Support GHC 9.8.x.

## 2.7.0 (2023-03-30)

- Adapt the generated spec files to the new `ghc-rpm-macros` for GHC 9.4.x:
  libraries get separate `-doc` and `-prof` sub-packages, and the
  build dependencies list both `-devel` and `-prof` packages.
- Name the `-doc` and `-prof` sub-packages explicitly
  (`ghc-%{pkg_name}-doc`, `ghc-%{pkg_name}-prof`). This fixes packages
  whose primary package is an executable that also provides a library.
- Don't generate `-doc` and `-prof` sub-packages for packages that have
  no library.
- List the package's actual license files in the `-doc` sub-package
  instead of a hard-coded `LICENSE`.
- Exclude `ghc-bignum` (not `bignum`) from the dependencies, because it
  ships with the compiler.
- Build Haddock documentation for packages with internal sub-libraries,
  and stop emitting the unused `has_internal_sub_libraries` define.

## 2.6.4 (2023-02-02)

- Build with Cabal 3.6 and 3.8.

## 2.6.3 (2022-02-04)

- Add `ExcludeArch: %{ix86}`, because Haskell packages are not built
  for 32-bit x86 any more.
- Use `https` for the openSUSE Bugzilla URL in the spec file header.
- Build with Cabal 3.4 and 3.6.

## 2.6.2 (2020-08-18)

- Use `%autosetup` in the `%prep` phase.

## 2.6.1 (2020-06-09)

- Change the copyright holder in the spec file header to "SUSE LLC".
- Map the `pq` system library to `postgresql-server-devel`.

## 2.6 (2020-04-01)

- Support Cabal 3.2.x.

## 2.5 (2020-01-28)

- Require `base >= 4.8`. (This release fixes the version number of 2.4.2,
  which should have been a major bump.)

## 2.4.2 (2020-01-26)

- New `--copyright-year` flag to set the year in the spec file header.
  `createSpecFile` and `cabal2spec` take an extra argument for it.

## 2.4.1 (2020-01-10)

- Support GHC 8.10.x and Cabal 3.1.x.

## 2.4 (2019-10-24)

- Stop emitting `Group:` tags, because openSUSE no longer supports them.

## 2.3 (2019-09-06)

- Require Cabal 3.0.x.

## 2.2.2.1 (2019-01-16)

- No user-visible changes.

## 2.2.2 (2018-09-23)

- Ignore test suites that aren't buildable when collecting test
  dependencies.

## 2.2.1 (2018-07-19)

- Fix the command-line tool, which ignored the `--compiler` and
  `--platform` flags.

## 2.2.0 (2018-07-17)

- New `--enable-tests` flag to run the test suite by default
  (`%bcond_without tests`).

## 2.1.1 (2018-05-23)

- Replace `[` and `]` in `%files` entries with `?` so that rpm doesn't
  read them as glob character classes (e.g. pandoc's
  `[Content_Types].xml`).
- Add system library mappings for `odbc` and `snappy`.
- Match the output of `spec-cleaner` more closely.

## 2.1 (2018-04-18)

- Support internal sub-libraries.
- Use SPDX license identifiers.
- Mark license files with `%license`.
- Don't run Haddock for libraries that expose no modules.
- Match the output of `spec-cleaner` more closely.

## 2.0.2 (2018-03-12)

- Require Cabal 2.2.x.
- Fix the command-line handling of Cabal flags (`--flag`).
- Only consider buildable components when collecting dependencies.
- Add a system library mapping for `gsl`.

## 2.0.1 (2018-03-05)

- Sort all `%files` lists alphabetically.
- Add a system library mapping for `re2`.

## 2.0.0 (2018-01-25)

- First release as a standalone package, with a command-line tool and a
  library.
