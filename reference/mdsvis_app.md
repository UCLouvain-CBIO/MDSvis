# Launch shiny app for MDS projection visualization

Launch shiny app for MDS projection visualization

## Usage

``` r
mdsvis_app(preLoadDemoDataset = FALSE)
```

## Arguments

- preLoadDemoDataset:

  if TRUE, pre-load the *Krieg_Anti_PD_1* dataset

## Value

no return value

## Examples

``` r
if (interactive()) {
  mdsvis_app()
}
```
