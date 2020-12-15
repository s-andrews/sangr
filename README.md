# sangr
An R package for simulating Sanger sequencing chromatograms

![Sanger Chromatogram](https://raw.githubusercontent.com/s-andrews/sangr/main/Images/chromatogram.png)

This is the development repository for the ```sangr``` R package.  It is designed to simulate sanger sequencing chromatorgrams, both creating per base signal density datasets, and then rendering these as chromatogram images.

## Installation
To install the package you will need to first install the ```devtools``` R package:

```install_packages("devtools")```

You will then need to load it:

```library(devtools)```

That should then allow you to run:

```install_github("s-andrews/sangr", build_vignettes=TRUE)```

..which should install the package and allow you to load it with:

```library(sangr)```

## Documentation
You can see how to use this package by reading the [vignette](https://htmlpreview.github.io/?https://github.com/s-andrews/sangr/blob/main/doc/sangr_usage.html)
