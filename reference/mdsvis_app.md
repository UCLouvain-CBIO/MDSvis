# Launch shiny app for MDS projection visualization

Launch shiny app for MDS projection visualization

## Usage

``` r
mdsvis_app(preLoadDemoDataset = FALSE, maxUploadSize = 50 * 1024^2)
```

## Arguments

- preLoadDemoDataset:

  if TRUE, pre-load the *Krieg_Anti_PD_1* dataset

- maxUploadSize:

  maximum size for file upload (50MB by default)

## Value

no return value

## Examples

``` r
if (interactive()) {
  mdsvis_app()
}
```
