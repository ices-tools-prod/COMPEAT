# S3 publication failure recovery

The earlier marker-only fix was incomplete. Local files must be validated, and
the production Shiny failure path must explicitly display them.

- `assessment_cache_complete()` still requires successful persistence in S3 mode.
- `assessment_pending_results_available()` validates the pending manifest and
  every file's size and checksum.
- After a failed worker, waiting sessions display validated pending results with
  a persistent warning. The status distinguishes local availability from S3 publication.
- Calculation failures and corrupt pending files do not expose results.
- To retry after loading, clear the selection or choose another period, then
  select the assessment again. Pending publication retries without recalculation.
- Pending results remain recoverable after an app restart if local disk survives.
  They do not survive loss of that disk until S3 publication succeeds.

Regression tests: `tests/test-assessment-storage.R` exercises the production
Shiny server and real S3 HTTP client against a loopback fixture, including
failed uploads, shared jobs, restart, successful retry, and corrupt results.
