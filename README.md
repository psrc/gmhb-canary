# gmhb-canary

Retrieve Growth Management Hearings Board orders from the Washington ELUHO CMS
using R and direct HTTP. Write a UTF-8 CSV and optionally send a readable SMTP
notification. The default window is the previous complete calendar month.

## Run

Requires R >= 4.1, `httr2`, `jsonlite`, `curl`, and `xml2`.
From the repository directory, install into the ignored local library:

```R
packages <- c("httr2", "jsonlite", "curl", "xml2")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  install.packages(missing)
}
```

Default retrieves the previous complete calendar month (as determined by today's
date in `America/Los_Angeles` timezone):

```powershell
Rscript scripts/query_orders.R
```

To specify the query range explicitly:

```powershell
Rscript scripts/query_orders.R --since 2026-09-01 --before 2026-10-01
```

CSV files go into ignored `results/`, with dates in the filename. Repeating the
same command replaces that output. `--output path/to/orders.csv` overrides the
destination. The script and local package library are located relative to the
script, so execution does not depend on the working directory. An explicit
relative output path is relative to the working directory.

The CSV preserves returned Salesforce column names, identifiers, UTC timestamps,
and `Case_Link__c` HTML. Case links are preserved as supplied; no HTML is executed.

Zero results produce a header-only CSV and exit 0. Network, HTTP, API-contract,
or output failures produce an error on stderr and exit 1. Do not consume an old
CSV after a failed run; always check the process exit status.

Since it would be rare for the Board to make more decisions in one month than fit 
on a single CMS results page, the script exports up to 50 records and will indicate
**REVIEW REQUIRED** if 50 or more were returned. Check the CMS manually for 
additional orders.

## SMTP email

Set these variables in a private `.Renviron` file. This keeps passwords out of
scripts, command arguments, and version control.

| Variable | Meaning |
| --- | --- |
| `SMTP` | Relay hostname; optional `smtp://` or `smtps://` scheme |
| `SMTPPORT` | Port number, stored separately from the hostname |
| `MAILER_EMAIL` | Sender address and SMTP authentication username |
| `MAILER_PW` | SMTP password |
| `MYEMAIL` | Recipient address; multiple addresses may be comma/semicolon separated |
| `GMHB_LOG_FILE` | Optional log path; defaults to `logs/gmhb-canary.log` in the project |

The implementation uses [curl's SMTP transport](https://search.r-project.org/CRAN/refmans/curl/html/send_mail.html)
with verbose logging disabled and 15-second connection/60-second total timeouts.
There is no dependency on Outlook or an interactive email login.

To preview the email without loading SMTP settings or sending anything:

```powershell
Rscript --vanilla scripts/query_orders.R --preview-email
```

This queries the CMS and writes `.html` and `.txt` previews alongside the CSV.

The normal email contains both plain text and HTML, with order titles, dates,
types, docket entries, clickable case names, and the Order Search link. CMS markup
is parsed as data and escaped, not copied into the email. There are no attachments
or unverified document URLs.

- Zero selected orders: no email
- Successful delivery: `EMAIL_ACCEPTED` means the relay accepted the message,
  not that inbox delivery has been confirmed.
- CMS or export/formatting failure: attempt one failure email if SMTP configuration
  was successfully loaded; the run still exits 1.
- SMTP failure: log the failure and exit 1. Do not automatically retry because
  the message might already have been accepted. The same relay cannot reliably
  alert about its own failure; inspect the log and Task Scheduler result.
- Configuration/startup failure: exit 1; a failure email may not be possible.


## Windows Task Scheduler

1. Create a task with a monthly trigger at a time the user is normally logged in.
   The command's default window covers the previous month regardless of the run day.
2. Action: **Start a program**. Program is the full path to `Rscript.exe`, for
   example `C:\Program Files\R\R-4.6.1\bin\x64\Rscript.exe`.
3. Arguments: Full path to `query_orders.R`, for example 
   `C:\projects\gmhb-canary\scripts\query_orders.R`.
4. Set **Start in** to the project directory. The script uses absolute project
   paths internally, so that working directory is not required for file lookup.
5. In Settings, enable running a missed task as soon as possible and choose
   **Do not start a new instance** when the task is already running. Do not enable
   automatic restart on failure; inspect the log before resending.
6. Allow enough time for the two CMS requests and SMTP delivery (for example a
   five-minute execution limit). The computer must be awake, logged in, and able
   to reach both the CMS and relay.

No scheduled task is created by this repository. Test it manually under the
intended account and confirm the email arrives before enabling the trigger.
Check `logs/gmhb-canary.log` and Task Scheduler's Last Run Result. Log events use
UTC timestamps and record counts/stages without credentials or raw server replies.

A delayed task uses the previous month relative to its **actual execution date**.
If an entire month is missed, rerun the missing month with explicit `--since` and
`--before` arguments. Late-posted or backdated orders would require a manual rerun.
