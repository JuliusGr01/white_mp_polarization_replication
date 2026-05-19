# R Replication of the White-Style Pipeline

This folder is a self-contained R port of the current Python pipeline, focused
on the **unsmoothed** Figure 3 results.

Run from this directory:

```r
source("run_all.R")
```

or from a shell with R on `PATH`:

```powershell
Rscript run_all.R
```

The script uses only base R, `stats`, and base graphics.

## What It Does

1. Rebuilds the 1969+ routine/nonroutine employment panel from:
   - `data/cps_ee_1969_1982_employment_monthly.csv`
   - `data/bls_occ_employed_monthly.csv`
2. Applies multiplicative STL seasonal adjustment to the occupation series.
3. Loads the Romer-Romer meeting-level shocks from
   `data/RR_MPshocks_Updated(GBforecasts).csv`, sums them to monthly shocks,
   and fills no-meeting months with zero.
4. Applies the seasonally adjusted routine share to BLS aggregate
   nonagricultural wage-and-salary employment, matching the Python pipeline.
5. Runs unsmoothed Jorda local projections with:
   - 48 monthly horizons
   - 12 lags of monthly outcome changes
   - 12 lags of monetary shocks
   - Newey-West HAC standard errors with 12 lags
6. Writes Figures 1-3, all individual IRF plots, FEV shares, the rebuilt LP
   panel, and a Python-reference validation file.

## Important Difference From the Latest Python Plot

The latest Python `output/figure3_linear_occupations.png` includes a display
smoother. This R port intentionally reproduces the **unsmoothed** raw local
projection results:

- `output/figure3_linear_occupations_unsmoothed.png`
- `output/figure3_linear_irfs_unsmoothed.csv`

For convenience, `output/figure3_linear_occupations.png` and
`output/figure3_linear_irfs.csv` are also written, but they are the same
unsmoothed R results.

## Reference Files

The `reference/` folder contains copied Python outputs from the previous step,
including `python_figure3_linear_irfs_unsmoothed.csv`. The R pipeline compares
its unsmoothed IRFs against that reference and writes:

```text
output/validation_against_python_unsmoothed.csv
```

No original Python input files were removed; they were copied here so this R
folder can be run independently without breaking the existing Python workflow.
