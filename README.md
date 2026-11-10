# OSPAR Common Procedure Eutrophication Assessment Tool

OSPAR Common Procedure for the Identification of the Eutrophication Status of the OSPAR Maritime Area

- The Screening Procedure
- The Comprehensive Procedure
  
<https://www.ospar.org/work-areas/hasec/eutrophication/common-procedure>

Assessment units are defined by OSPAR contracting parties. An assessment unit can have multiple indicators. Each indicator can have different temporal (months) and spatial (depths) coverage and reference values within the different assessment units.

Indicator results can be calculated either from raw data reported to ICES or indicator results can be reported directly. 

### Docker

Build and run the multistage image from the repository root:

```sh
docker build -f Dockerfile-multistage -t compeat .
docker run --rm -p 3838:3838 compeat
```

The original single-stage `Dockerfile` is preserved; use
`docker build -t compeat .` to build it instead.

Open <http://localhost:3838>. The multistage build retains the lockfile's R
version, restores packages in the `dependencies` stage, and copies a standalone
R library into the `runtime` stage. Development packages and the renv build cache
are not copied into the final image. The runtime stage checks that installed R
package namespaces load before copying application code.

Code-only builds reuse dependency layers; changes to `renv.lock` trigger a
restore. Retain Docker's build cache to benefit from warm builds. BuildKit's renv
cache also reuses package downloads across restores. Runtime system libraries
target Ubuntu 24.04, matching the pinned Rocker base; review them when changing
the base image or adding dependencies. Image-size and build-time improvements
depend on the packages and available cache.

### Assessment Units

### Indicators

- Dissolved Inorganic Nitrogen (DIN)
- Dissolved Inorganic Phosphorus (DIP)
- Chlorophyll a (CPHL)
- Oxygen Deficiency (DOXY)
- Total Nitrogen (TN)
- Total Phosphorus (TP)
- Secchi Depth (SECC)
- Dissolved Inorganic Nitrogen:Dissolved Inorganic Phosphorus (DIN:DIP)
- Total Nitrogen:Total Phosphorus (TN:TP)

### Abbreviations

- ES = Eutrophication Status
- SD = Standard Deviation
- N = Number of observations
- NM = Number of unique Months
- NMP = Number of potential Months
- SE = Standards Error
- CI = Confidense Interval
- ET = Eutrophication Target / Threshold
- ACDEV = Acceptable Deviation
- BEST = Reference Condition
- EQR = Ecological Quality Ratio
- EQR_HG = Ecological Quality Ratio High/Good boundary
- EQR_GM = Ecological Quality Ratio Good/Moderate boundary
- EQR_MP = Ecological Quality Ratio Moderate/Poor boundary
- EQR_PB = Ecological Quality Ratio Poor/Bad boundary
- EQRS = Ecological Quality Ratio Scaled
- GTC = General Temporal Confidence
- GTC_HM = General Temporal Confidence High/Moderate boundary
- GTC_ML = General Temporal Confidence Moderate/Low boundary
- STC = Specific Temporal Confidence
- STC_HM = Specific Temporal Confidence High/Moderate boundary
- STC_ML = Specific Temporal Confidence Moderate/Low boundary
- TC = Temporal Confidence
- GSC = General Spatial Confidence
- GSC_HM = General Spatial Confidence High/Moderate boundary
- GSC_ML = General Spatial Confidence Moderate/Low boundary
- SSC = Specific Spatial Confidence
- SSC_HM = Specific Spatial Confidence High/Moderate boundary
- SSC_ML = Specific Spatial Confidence Moderate/Low boundary
- SC = Spatial Confidence
- C = Confidence
- NPA = None Problem Area
- PA = Problem Area
- AC = Accuracy Confidence
- ACC = Accuracy Confidence Class
- IW = Indicator Weight

### Shared background assessments

Selecting an assessment period loads cached results from its `data/<period>/output`
folder only when an `assessment.complete` marker and all required indicator,
assessment, station and shapefile outputs exist (S3 mode also requires a matching
persistence receipt). Otherwise, the app prepares the assessment in a `callr`
background process. Sessions requesting the same period share that process and
check for completion every five seconds. Jobs continue when a user disconnects;
results are loaded after the process finishes. S3 publication failures can expose
validated pending local results with a warning, without marking publication as
successful. Other failures are reported to waiting users without displaying
results, and selecting the period again retries the assessment.

Use **Recalculate assessment...** on the Assessment tab to explicitly rerun the
selected period in either storage mode. Confirmation bypasses local result reuse,
S3 restore and pending-publication reuse, and downloads all configured input files
again, replacing existing downloads and re-extracting archives. Normal selection
still reuses cached inputs. Calculation runs in a separate output directory and validates
its results before replacing the current output. Calculation failures preserve
previous completed results. S3 publication creates a new run and advances
`current.json` only after successful uploads, preserving previous archives.
Other sessions viewing the period reload new results on their next poll.
Forced requests are rejected while another job for that period is running.
See [RERUN_ASSESSMENT_GUIDE.md](./RERUN_ASSESSMENT_GUIDE.md) for publication retry
and recovery details. No local backup is kept once the validated calculation
replaces the old output, including when S3 publication fails. There are no
built-in operator roles: deployment access
controls must restrict recalculation to trusted users.

Normal worker output is written directly to `assessment-stdout.log` and
`assessment-stderr.log` in the period's output folder, rather than captured in
unread pipes that can fill and stall downloads or calculations. Logs are replaced
when a new assessment job starts; cached-result loads leave them unchanged.
Forced-job logs live one level above `output`, in the period folder, so the output
directory can be replaced without moving open log files.

Inputs download quietly to temporary files in the input folder and are renamed
to their final names only after a successful transfer. Failed downloads are
reported and their temporary files are removed. A container shutdown may leave
temporary files, but subsequent runs do not treat them as completed downloads.
Cached ZIP files are extracted again so interrupted extraction can recover.
Files cached by older versions are not validated: remove any known incomplete
input file before retrying the assessment.

Job tracking is shared within one Shiny R process, not across multiple workers or
app instances. Jobs do not survive an app process shutdown, but completed output
files remain available for subsequent sessions. The completion marker is
published by an atomic rename after the assessment script succeeds and its
required outputs are checked. For normal preparation it is removed before regeneration; failed or
interrupted runs cannot produce a reusable cache. Existing output folders without
a marker are recalculated once unless restored from S3. Normal local cache loads
check completion and file presence, not file contents or changes to inputs.
Forced recalculation instead preserves the old output and marker until its
staged calculation has succeeded.

Assessment selectors across tabs track the requested period. Selecting a period
clears previous results immediately. Maps and tables remain blank, station and
indicator selectors clear, and downloads are disabled until that assessment
completes successfully, its completed cache is loaded, or its pending local
results are validated after an S3 publication failure. Other failed requests leave
results blank rather than restoring an older period. Failed forced calculations
can redisplay preserved completed results for the same period with an error
notification. The app-wide status identifies
the requested and displayed period. Selecting the empty option clears both.

Assessment selectors are locked across all tabs during generation and while
results (including cached results) are loading. They unlock after the browser
reports that its reactive updates are idle. Late selection events are rejected
and the selector is restored to the active request. Generation failures unlock
the selectors so another period can be selected.

Run the shared-job regression tests from the repository root with
`Rscript tests/test-shared-assessments.R`. They use temporary fixture data rather
than running a full assessment, including workers that produce more than 2 MiB
on each output stream. Run `Rscript tests/test-assessment-downloads.R` to check
download failure recovery and caching without network access.

### Local and EDITO S3 storage

The app always computes and reads results on a local filesystem. In S3 mode,
completed results are also published to S3 so a replacement container can restore
them without recalculation. Local storage means disk inside the app container or
an attached volume, not the browser user's computer. The app displays its active
storage destination above the assessment status.

Configure storage through environment variables before starting the app:

| Variable | Default | Purpose |
| --- | --- | --- |
| `COMPEAT_STORAGE` | `auto` | `auto`, `local` or `s3`. Automatic mode selects S3 when `AWS_S3_ENDPOINT` is nonempty; otherwise local storage. This detects storage configuration, not EDITO identity. |
| `COMPEAT_DATA_DIR` | `<app directory>/data` | Local input/output working cache. Relative paths are resolved against the app directory. Use a writable directory with enough capacity for inputs and outputs. |
| `COMPEAT_S3_BUCKET` | None | Required in S3 mode: an existing, accessible bucket. Not inferred from the username. |
| `COMPEAT_S3_PREFIX` | `compeat` | Permitted object prefix within the bucket; may be empty for the bucket root. |
| `COMPEAT_S3_URL_STYLE` | `path` | `path` or `virtual`, according to the S3 service configuration. |
| `AWS_S3_ENDPOINT` | None | Required in S3 mode: endpoint hostname, optionally with `https://` or `http://` and a port. No bucket or path component. HTTPS is used when the scheme is omitted. |
| `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY` | None | Required injected credentials in S3 mode. |
| `AWS_SESSION_TOKEN` | None | Inherited automatically when temporary credentials are injected. |
| `AWS_DEFAULT_REGION` | `us-east-1` for the standard AWS endpoint | Used for `s3.amazonaws.com`. For custom endpoints, `aws.s3` requires `region = ""` to avoid prepending the region to the hostname. |

For example, set these non-secret application settings in the EDITO service
configuration, using the bucket and prefix actually granted to that service:

```text
COMPEAT_STORAGE=auto
COMPEAT_S3_BUCKET=<your-accessible-bucket>
COMPEAT_S3_PREFIX=<your-permitted-prefix>/compeat
COMPEAT_DATA_DIR=/tmp/compeat-data
```

Keep the platform-injected AWS credentials and endpoint; do not put credentials in
source code or the container image. `COMPEAT_STORAGE=local` explicitly disables
S3 even when AWS variables exist. Missing or invalid S3 configuration stops app
startup; transfer failures are reported to waiting users, never silently treated
as successful local persistence. Restore dependencies from `renv.lock` when
deploying; it includes `aws.s3` and `aws.signature`.

#### Publication and recovery

Sessions selecting the same assessment share one background preparation job,
including S3 downloads, calculation and uploads. Different assessments can run
concurrently, as before. S3 access happens in the worker, not a session observer,
so publication continues after a browser disconnects.

Each assessment is stored under:

```text
<prefix>/<assessment>/runs/<run-id>/output/<result files>
<prefix>/<assessment>/runs/<run-id>/manifest.json
<prefix>/<assessment>/current.json
```

The worker uploads all output artifacts (including station sample outputs and
shapefile companions), excluding logs and internal assessment metadata. A manifest
records filenames, byte sizes and MD5 checksums for transfer-integrity checks.
`current.json` is written last and references that manifest and its checksum.
Uploads use bounded-size multipart transfers when needed and verify the stored
object size before proceeding.
An interrupted replacement leaves the previously published run available.
Unreferenced runs from failed uploads are not loaded; cleanup/retention is an
operator responsibility. Inputs remain a local download cache. Station-sample
downloads in the UI continue fetching the original configured source URLs.

With no completed local S3 cache, the worker first restores the published run
into a temporary staging directory and validates its files before exposing it
to the UI. Only an explicit S3 `NoSuchKey` response for `current.json` means no
published assessment exists. Permission errors, missing buckets, incomplete runs
and checksum failures fail visibly instead of triggering recalculation.
Failed downloads report the HTTP status and a recognized S3 error code (such as
`AccessDenied`, `ExpiredToken` or `SignatureDoesNotMatch`), with a troubleshooting
hint. Raw response bodies, server messages and signed requests are not included
in these notifications; unrecognized or malformed responses retain the HTTP
status without exposing their contents.
An unrecognized HTTP 403 also identifies credentials, bucket/prefix permissions
and the S3 API endpoint (including proxies/gateways) as checks. A missing
`current.json` can return 403 without `ListBucket` permission; the app cannot
safely treat that response as an absent assessment. The service needs read/write
access to its configured object prefix and sufficient bucket-list permission to
distinguish missing objects from denied access.
GET and upload-verification HEAD requests pass unencoded object keys to
`aws.s3::s3HTTP`, which encodes individual path segments before signing. Do not
pre-encode spaces: this can leave parentheses unencoded and produce a signature
mismatch for names such as `COMP 5 (2021-2026)`.
Existing local completed results can be published if S3 has no published run.

After calculation, `assessment.generated.json` records validated local results
before upload. If publication fails, waiting sessions display these local results
only after validating every manifest file's size and checksum, with a persistent
warning and a status indicating that S3 publication is pending. They are not
treated as successfully persisted: no completion marker or new receipt is written.
Calculation failures and invalid local manifests still leave results blank.
After results finish loading, select the empty option or another period, then
select the assessment again to retry
publication without recalculation, including after an app restart while that
local directory survives. `assessment.persisted.json` identifies the destination,
and `assessment.complete` is created only after successful restoration or
publication. A cache persisted to a different destination is not silently reused
as already published there.

Do not manually edit a generated cache. If a pending publication manifest reports
changed files, remove the affected assessment's local output cache while the app
is stopped, then retry. Completed caches are reused until deliberately
invalidated; input/configuration changes do not automatically create a new cache
identity, and running containers do not automatically refresh locally cached
results when another deployment publishes to S3.

#### Deployment limits

This implementation supports **one Shiny R process / one replica** with multiple
browser sessions and identical inputs/settings for each assessment period.
Do not share its writable cache across independent app processes. S3 publication
uses isolated run prefixes, but is not a distributed calculation lock; multiple
replicas require a shared job coordinator before deployment.

Browser disconnection does not stop jobs, but app process or pod shutdown can.
Only successfully published results survive loss of ephemeral disk. Temporary
credentials must remain valid for the duration of a job; if they expire, refresh
the service credentials using the platform's supported mechanism and retry.
There is no automatic credential renewal in the app.

Run `Rscript tests/test-assessment-storage.R` for configuration, publication,
restoration, integrity, retry and failure tests. It also exercises the actual S3
HTTP client against a loopback fixture server, including session-token
inheritance, shared background restores and publication after session disconnect.
No live S3 credentials or external network access are required.
