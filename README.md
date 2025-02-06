# MDSgui

The MDSgui package implements visualization of Multi Dimensional Scaling (MDS)
objects representing a low dimensional projection of cytometry samples.
Such objects can be obtained using the functionalities implemented in the
`CytoMDS` package. The visualization is realised via a Shiny app that allows 
the user to interactively customise the plots depending on a series of input
parameters.

Below a screenshot of the app 'View' tab. The controls on the side allow to
choose the projection axes, colour, label, or shape the points according to
phenodata variables, add biplot, show a `plotly` plot for interactive plot
exploration or flip axes.

![](vignettes/images/ViewPlotly.png) 

### License

The `MDSgui` code is provided under [GPL license version 3.0 or 
higher](https://opensource.org/licenses/GPL-3.0). The documentation, 
including the manual pages and the vignettes, are distributed under a [CC BY-SA 
4.0 license](https://creativecommons.org/licenses/by-sa/4.0/).

### Citation

If you use `MDSgui` in your research, please use the following citation:

>Hauchamps, Philippe, Simon Delandre, Stephane T. Temmerman, 
> Dan Lin, and Laurent Gatto. 2024. 
> “Visual Quality Control with CytoMDS, a Bioconductor Package 
> for Low Dimensional Representation of Cytometry Sample Distances.” 
> bioRxiv. https://doi.org/10.1101/2024.07.01.601465.
