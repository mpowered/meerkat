# Changelog for meerkat

## 0.5.1

### Changed

- Support bushpig jobs
- ghc 8.10.4

## 0.5.0.1

### Changed

- Added a little bit of strictness

## 0.5.0.0

### Changed

- Switched back to nixpkgs, use ghc 8.10.2

## 0.4.0.0

### Changed

- Switched to using haskell.nix

## 0.3.4.0

### Changed

- Sidekiq now takes a list of redis databases

## 0.3.3.1

### Changed

- No longer using Sidekiq namespaces changed the queue names

## 0.3.3.0

### Added

- HTTP request headers

## 0.3.2.0

### Added

- Mysql processlist support

## 0.3.1.3

### Changed

- Coalesce jobs pre-insert

  postgresql returns "ON CONFLICT DO UPDATE command cannot affect row a second time" if non-unique job_ids attempt to
  update a row

## 0.3.1.2

### Changed

- Group import entries by time for batched insert

## 0.3.1.1

### Changed

- Poll sidekiq_queues once every 60s instead of 20s

## 0.3.1.0

### Changed

- Include class and jobs ids in SidekiqQueues

## 0.3.0.2

### Changed

- Clean \0000 from ActionController/SidekiqJob params

## 0.3.0.1

### Changed

- SidekiqJob uses coalesce to update started\_at and completed\_at timestamps

## 0.3.0.0

### Added

- JSON file importer that supports ActionController and SidekiqJob formats

### Removed

- SidekiqJobs no longer fetched from Redis

## 0.2.4.1

### Fixed

- Use spaces not + for Honeybadger query

## 0.2.4.0

### Added

- Poll Honeybadger for total faults for a project environment

## 0.2.3.0

### Changed

- Only open one Redis connection (Hedis uses a connpool)

## 0.2.2.0

### Added

- Compute average age of jobs in Sidekiq queues per class

## 0.2.1.0

### Added

- Ability to fetch stats from a running Puma server using the control API

## Unreleased changes
