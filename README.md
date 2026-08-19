# Field Trial Planner

A Shiny app for planning agricultural field experiments. It works through the
decisions that determine what a trial can show, and explains the reasoning as it
goes, so the plan and the understanding of it are built at the same time.

## What it does

Covers the design one section at a time:

- the trial aim
- treatment factors and their levels
- replication, with residual degrees of freedom calculated as you go
- response variables, their data types, sampling and timing
- research questions, built from the factors and responses already entered
- implementation across sites and years

The sections are a way to make a complex task tractable, not a sequence.
Treatments, replication, measurements and questions all shape one another, so a
decision made in one section regularly sends you back to revise an earlier one —
adding a factor changes the replication you need, and finding the replication
unaffordable changes which questions the trial can carry. Experimental design is
iterative rather than step-by-step, and the app is built to be returned to
rather than completed once.

As you go it checks the plan against itself and reports what the design
structurally involves — treatments, plots, analyses, interactions, restricted
randomisation. Typical findings: a question asked about a factor the trial never
varied, a factor declared with six levels and two described, five treatments at
four replicates leaving 12 residual degrees of freedom where 15 was the target.

Answers are held in the browser session only. Nothing is written to a server,
and everything leaves as two downloads: a CSV that reloads into the app, and a
self-contained HTML summary.

## Running it

```r
# R 4.4 or later
install.packages(c("shiny", "bslib", "yaml", "readr", "tibble", "rmarkdown"))
shiny::runApp()
```

Rendering the HTML summary needs pandoc. Shiny Server and shinyapps.io provide
it; locally, an RStudio or Quarto install supplies one and the app finds it.

```sh
Rscript tests/testthat.R    # 187 tests
```

## Repository layout

| Path | Purpose |
|---|---|
| `R/fct_schema.R` | The data model. Export, import, validation and the UI all derive from it, so a field is added in one place. |
| `R/fct_config.R` | Loads the YAML configuration, merging any local overrides. |
| `R/fct_transfer.R` | Long-format CSV export and import. |
| `R/fct_metrics.R` | Treatments, plots, residual degrees of freedom, design profile. |
| `R/fct_questions.R` | Builds research question sentences; flags questions the design cannot answer. |
| `R/fct_validate.R` | Cross-section consistency checks. |
| `R/fct_report.R` | Renders the HTML summary. |
| `R/fct_example.R` | The worked example, which is also the test fixture. |
| `R/fct_steps.R` | The workflow steps, and how much of each has been filled in. |
| `R/mod_*.R` | One module per step: its inputs, its guidance, its navigation. |
| `R/utils_ui.R` | Shared interface pieces used across the steps. |
| `inst/config/` | Wording, vocabularies, guidance copy, profile weights. |
| `inst/report/` | Summary template and stylesheet. |
| `www/` | Stylesheet for the app itself. |

## Export format

One row per recorded fact:

```csv
section,entity_id,parent_id,field,value
trials,E1,,trial_name,Nitrogen rate trial
factors,E1_F1,E1,factor_name,Nitrogen rate
factors,E1_F1,E1,n_levels,5
levels,E1_F1_L1,E1_F1,level_desc,0 kg N/ha (nil applied)
```

A trial has many factors and a factor has many levels, so a wide table would be
mostly empty columns. This shape avoids that, stays readable in Excel, and
pivots back into wide tables for analysis. The same file reloads into the app,
which matters because plans get revised: the download and the save file are one
thing rather than two.

## Configuration

Wording, vocabularies and thresholds are YAML rather than R:

| File | Controls |
|---|---|
| `app.yml` | Names, wording, the residual df target, theme colours. |
| `lists.yml` | Dropdown vocabularies and question sentence templates. |
| `guidance.yml` | The explanatory copy shown alongside each section. |
| `complexity.yml` | How the design profile is weighted and banded. |

Files of the same name in `inst/config/local/` are merged over the top, key by
key, so a one-line file changes one thing and everything else falls through to
the defaults. `options(FieldTrialPlanner.config_dir = )` moves the whole
configuration outside the app directory.

## Status

Under development.

The foundations are complete and tested: data model, configuration, validation,
design metrics, CSV round trip and HTML summary.

The interface is being built step by step. The Start and Project steps are done;
the remaining steps are reachable and describe what they will ask for, but do
not yet collect it. A plan loaded from a file keeps everything, including the
parts no step has been built for yet.

## Licence

Copyright (C) 2026 rmegirian. GNU General Public License v3.0 — see
[LICENSE](LICENSE).

Free to use, copy, modify and share. If you distribute a modified version, it
has to be under the same licence, with source available, so that it stays as
open as you found it.
