# Revision history for dzen-dhall

## 1.0.2 -- 2020-18-02

### Changes
- Updated Dhall version to 1.28.0

### Fixed
- stdout/stderr handles for spawned processes were not being closed, resulting in `too many open files` errors when using plugins that emit events a lot (#9)

## 1.0.1 -- 2019-08-29

### Changes
- Add a small delay between dzen2 startups to preserve the ordering of bars (#6)

### Fixed
- Validation for `#XXXXXX`-formatted colors (#5)
- Padding (#4)

## 1.0.0 -- 2019-08-26

* First version. Released on an unsuspecting world.
