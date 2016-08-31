# NFP
Network fingerprint analysis in R

This package implements the network fingerprint framework. A biomedical network is characterized as a spectrum-like vector called “network fingerprint”, which contains similarities to basic networks. This knowledge-based multidimensional characterization provides a more intuitive way to decipher molecular networks, especially for large-scale network comparisons and clustering analyses

**Prerequisites**

**NFP** is free available on [CRAN](https://cran.r-project.org). You can 
install the latest released version as following:

```{r,eval=FALSE} 
install.packages("NFP")
```

or the latest development version from github. To install packages from GitHub,
you first need install the **devtools** package on your system with 
`install.packages("devtools")`. Note that devtools sometimes needs some 
extra non-R software on your system -- more specifically, an Rtools download for
Windows or Xcode for OS X. There's more information about devtools
[here](https://github.com/hadley/devtools).
  
```{r,eval=FALSE} 
if (!require(devtools) 
  install.packages("devtools") 
devtools::install_github("yiluheihei/NFP") 
```


After installation, you can load **NFP** into current workspace by typing or pasting the following codes:

 ```R
library("NFP")
 ```
## Contributing

For very simple changes such as fixing typos, you can just edit the file by clicking the button `Edit`. 
For more complicated changes, you will have to manually create a pull request after forking this repository.
 
##License

`NFP` is a free and open source software, licensed under GPL 2.0.

##Reference

[Cui X, He H, He F, et al. Network fingerprint: a knowledge-based characterization of biomedical networks. Scientific reports, 2015, 5.](http://www.nature.com/articles/srep13286)

