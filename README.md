# NFP: Network fingerprint analysis in R

[![Travis-CI Build Status](https://travis-ci.org/yiluheihei/NFP.svg?branch=master)](https://travis-ci.org/yiluheihei/NFP)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/NFP)](https://cran.r-project.org/package=NFP)
[![Coverage Status](https://img.shields.io/codecov/c/github/yiluheihei/NFP/master.svg)](https://codecov.io/github/yiluheihei/NFP?branch=master)

This package implements the network fingerprint framework. A biomedical network is characterized as a spectrum-like vector called “network fingerprint”, which contains similarities to basic networks. This knowledge-based multidimensional characterization provides a more intuitive way to decipher molecular networks, especially for large-scale network comparisons and clustering analyses

**Prerequisites**

**NFP** is free available on [CRAN](https://cran.r-project.org).  To install **NFP**, please note especially two depencies of **NFP**, **graph** and **KEGGgraph** are only available from [Bioconductor](https://www.bioconductor.org). Appanrantly, function `install.packages` can not insall Biocondutor packages. There is a function `install`, a wrapper around `install.packages`
provided by Bioconductor, can be used to install both CRAN and Bioconductor
packages simply. Thus, users can install NFP
install the latest released version directly as flowing:


```R
if (!requireNamespace("BiocManager"))
    install.packages("BiocManager")
BiocManager::install("NFP")
```

or install the  Bioconductor dependencies package first:

```R 
BiocManager::install(c("graph","KEGGgraph"))
install.packages("NFP")
```

It also allows users to install the latest development version from github, which requires  **devtools** package has been installed on your system (or can be installed using `install.packages("devtools")`). Note that devtools sometimes needs some extra non-R software on your system -- more specifically, an Rtools download for Windows or Xcode for OS X. There's more information about devtools
[here](https://github.com/r-lib/devtools).
  
```R
## install NFP from github, require biocondutor dependencies package pre-installed
if (!require(devtools) 
  install.packages("devtools") 
devtools::install_github("yiluheihei/NFP") 
```


After installation, you can load **NFP** into current workspace by typing or pasting the following codes:

 ```R
library("NFP")
 ```

 Moreover, gene similarity data used in our **NFP** package is stored in a external data repository [NFPdata](https://github.com/yiluheihei/datarepo) for the large size (about 16 MB). More details on how to construct External Data Repositories using the Additional\_repositories field see The Coatless Professor [blog post](https://thecoatlessprofessor.com/programming/r-data-packages-in-external-data-repositories-using-the-additional\_repositories-field/). Thus, users must install the **NFPdata** before the networkfinger print analyis as following code.

```R
if (!require("NFPdata")) {
    install_data_package()
}
```

## Contributing

For very simple changes such as fixing typos, you can just edit the file by clicking the button `Edit`. 
For more complicated changes, you will have to manually create a pull request after forking this repository.
 
## License

`NFP` is a free and open source software, licensed under GPL 2.0.

## Reference

[Cui X, He H, He F, et al. Network fingerprint: a knowledge-based characterization of biomedical networks. Scientific reports, 2015, 5.](https://www.nature.com/articles/srep13286)

