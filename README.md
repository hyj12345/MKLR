# MKLR

<!-- badges: start -->

[![R-CMD-check](https://github.com/kaneplusplus/bis620/workflows/R-CMD-check/badge.svg)](https://github.com/hyj12345/MKLR/actions)
[![INFO](https://img.shields.io/badge/YJ-Homepage-orange)](https://github.com/hyj12345/MKLR)

<!-- badges: end -->

Hi,guys! Welcome to MKLP package homepage. I used the knowledge about how to create a R package in BIS620 to solve a problem in BIS555.

Multi-class kernel logistic regression. This function fit a Multi-class Kernel Logistic Regression model to the data. The return list contains the estimated kernel parameters and logistic parameters.There are two types of kernel, they are `RBF` and `polynomial`.


I refer to [KLR](https://github.com/fontaine618/KLR) and the [Multinomial Kernel Logistic Regression via Bound Optimization Approach](https://scienceon.kisti.re.kr/srch/selectPORSrchArticle.do?cn=JAKO200709906203322) for the calculation and use gradient descent (not dual or fix the parameter) to get all parameters.

And I will keep updating the package and try to provide methods like CV, LOOCV and more for better model selection.

## Installation

You can install the released version of bubblematrix from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
library(devtools)
devtools::install_github("hyj12345/MKLR")
```

## Example

I use the data for my course homework as a simple example. 

### View the built-in dataset

```r
library(readr)
library(magrittr)
train_data <- read_csv("~/Desktop/21Fall/BIS555/vowel/training")%>%.[2:12]
test_data <- read_csv("~/Desktop/21Fall/BIS555/vowel/test")%>%.[2:12]
```

* Train

```r
library(MKLR)
##Train the model
model_mklr<-MKLR(train_data$y,train_data[,-1],max_iter=1000,threshold=1.0e-5,lr=0.5,kernel = 'RBF')
```

* Predict 

return classes or probabilities

```r
pre_mklr<-MKLR::predict.MKLR(model_mklr,test_data[,-1],response = 'class')
```


