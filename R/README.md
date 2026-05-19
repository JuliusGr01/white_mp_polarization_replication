# R Replication of the White-Style Pipeline

This folder is a self-contained R port of the current Python pipeline, written
in the same script-driven style as the `final/` folder of the thesis project.
`main.R` does setup and sourcing, `1_Data.R` rebuilds the estimation panel from
the raw Excel/BLS inputs, `2_LP.R` contains the empirical workflow, and
`functions.R` only contains reusable helper functions.

Run from this directory:

```r
source("main.R")
```

or from a shell with R on `PATH`:

```powershell
Rscript main.R
```

`run_all.R` is kept as a compatibility wrapper and simply sources `main.R`.
The scripts use only base R, `stats`, and base graphics.

## What It Does

1. Rebuilds the 1969-1982 historical occupation panel from:
   - `data/1969_1982_CPS.xlsx`
   - `data/cps_ee_1969_1982_alm_crosswalk.csv`
2. Builds the 1983+ broad occupation panel and aggregate employment from:
   - `data/bls_raw/ln.data.1.AllData`
3. The raw input files are also copied under `data/` for inspection and future
   extensions:
   - `data/cps_ee_1969_1982_employment_monthly.csv`
   - `data/bls_occ_employed_monthly.csv`
4. Loads the Romer-Romer meeting-level shocks from
   `data/RR_MPshocks_Updated(GBforecasts).csv`, sums them to monthly shocks,
   and fills no-meeting months with zero.
5. Runs Jorda local projections with:
   - 48 monthly horizons
   - 12 lags of monthly outcome changes
   - 12 lags of monetary shocks
   - Newey-West HAC standard errors with 12 lags
6. Applies the same Figure 3 display smoother as the Python pipeline and also
   writes unsmoothed diagnostic outputs.
7. Writes Figures 1-3, all individual IRF plots, FEV shares, the LP panel, and
   a Python-reference validation file.

The data-build diagnostics are written to:

```text
output/data_build/
```

## Important Difference From the Latest Python Plot

The current Python `output/figure3_linear_occupations.png` includes a display
smoother. The R port writes both the smoothed Python-style output and the raw
unsmoothed diagnostic output:

- `output/figure3_linear_occupations.png`
- `output/figure3_linear_irfs.csv`
- `output/figure3_linear_occupations_unsmoothed.png`
- `output/figure3_linear_irfs_unsmoothed.csv`

## Reference Files

The `reference/` folder contains copied Python outputs from the previous step.
The R pipeline compares its raw and plotted Figure 3 IRFs against that reference
and writes:

```text
output/validation_against_python.csv
```

No original Python input files were removed; they were copied here so this R
folder can be run independently without breaking the existing Python workflow.
