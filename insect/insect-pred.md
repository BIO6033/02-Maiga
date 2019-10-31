---
title: "insect pred"
author: "Maiga"
date: "30/10/2019"
output:
  html_document:
    keep_md: yes
  pdf_document: default
---
#packages

```r
library(tidyverse)
```

```
## -- Attaching packages --------------------------------------- tidyverse 1.2.1 --
```

```
## v ggplot2 3.2.1     v purrr   0.3.2
## v tibble  2.1.3     v dplyr   0.8.3
## v tidyr   1.0.0     v stringr 1.4.0
## v readr   1.3.1     v forcats 0.4.0
```

```
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(caret)
```

```
## Loading required package: lattice
```

```
## 
## Attaching package: 'caret'
```

```
## The following object is masked from 'package:purrr':
## 
##     lift
```

```r
library(ggplot2)
library(e1071)
```

#data

```r
insectSize = readr::read_tsv("https://raw.githubusercontent.com/shchurch/insect_egg_database_viz/master/data/dataviz_egg_database.csv")
```

```
## Parsed with column specification:
## cols(
##   .default = col_double(),
##   bibtex = col_character(),
##   bib_author = col_character(),
##   family = col_character(),
##   superfamily = col_character(),
##   subfamily = col_character(),
##   suborder = col_character(),
##   order = col_character(),
##   tribe = col_character(),
##   genus = col_character(),
##   species = col_character(),
##   name = col_character(),
##   new_name = col_character(),
##   image = col_character(),
##   group = col_character()
## )
```

```
## See spec(...) for full column specifications.
```

```r
head (insectSize)
```

```
## # A tibble: 6 x 48
##      ID bibtex bib_author  year family superfamily subfamily suborder order
##   <dbl> <chr>  <chr>      <dbl> <chr>  <chr>       <chr>     <chr>    <chr>
## 1    17 Herna~ Hernández~  2013 Pieri~ Papilionoi~ Pierinae  Glossata Lepi~
## 2    18 Herna~ Hernández~  2013 Pieri~ Papilionoi~ Pierinae  Glossata Lepi~
## 3    19 Herna~ Hernández~  2013 Pieri~ Papilionoi~ Pierinae  Glossata Lepi~
## 4    20 Herna~ Hernández~  2013 Pieri~ Papilionoi~ Pierinae  Glossata Lepi~
## 5    21 Peter~ Peterson,~  1968 Geome~ Geometroid~ Geometri~ Glossata Lepi~
## 6    22 Peter~ Peterson,~  1968 Geome~ Geometroid~ <NA>      Glossata Lepi~
## # ... with 39 more variables: tribe <chr>, genus <chr>, species <chr>,
## #   name <chr>, new_name <chr>, image <chr>, txtX1 <dbl>, txtX2 <dbl>,
## #   txtvol <dbl>, txtar <dbl>, txtel <dbl>, logtxtar <dbl>,
## #   logtxtX1 <dbl>, logtxtX2 <dbl>, logtxtvol <dbl>, imX1 <dbl>,
## #   imX2 <dbl>, logimX1 <dbl>, logimX2 <dbl>, imvol <dbl>, imar <dbl>,
## #   imel <dbl>, logimar <dbl>, asym <dbl>, curv <dbl>, sqasym <dbl>,
## #   sqcurv <dbl>, X1 <dbl>, X2 <dbl>, X3 <dbl>, logX1 <dbl>, logX2 <dbl>,
## #   logX3 <dbl>, ar <dbl>, el <dbl>, vol <dbl>, logar <dbl>, logvol <dbl>,
## #   group <chr>
```

#Columns selection

```r
insectSizeGroup =insectSize %>% 
  select(logX1,logX2, logvol, group)
```

#missing values


```r
length(which(!complete.cases(insectSizeGroup)))
```

```
## [1] 2453
```
#Proportion

```r
table(insectSizeGroup$group)
```

```
## 
## Amphiesmenoptera      Antliophora       Apterygota    Condylognatha 
##             2978             1026               20             1259 
##      Hymenoptera   Neuropteroidea      Palaeoptera     Polyneoptera 
##             1768              870              314             1550 
##         Psocodea 
##               64
```

# No NA


```r
insectSizeClean <- na.omit(insectSizeGroup)
length(which(!complete.cases(insectSizeClean)))
```

```
## [1] 0
```

```r
head(insectSizeClean)
```

```
## # A tibble: 6 x 4
##    logX1  logX2  logvol group           
##    <dbl>  <dbl>   <dbl> <chr>           
## 1  0.233 -0.229 -0.507  Amphiesmenoptera
## 2  0.191 -0.228 -0.546  Amphiesmenoptera
## 3  0.184 -0.246 -0.590  Amphiesmenoptera
## 4  0.400 -0.107 -0.0956 Amphiesmenoptera
## 5 -0.187 -0.398 -1.47   Amphiesmenoptera
## 6 -0.398 -0.523 -1.72   Amphiesmenoptera
```


#Normalization

```r
scale2 <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, TRUE)
insectSizeMeans <- insectSizeClean %>% 
                          mutate_if(is.double, scale2)
head(insectSizeMeans)
```

```
## # A tibble: 6 x 4
##    logX1  logX2 logvol group           
##    <dbl>  <dbl>  <dbl> <chr>           
## 1  0.578  0.267  0.399 Amphiesmenoptera
## 2  0.478  0.270  0.365 Amphiesmenoptera
## 3  0.460  0.226  0.328 Amphiesmenoptera
## 4  0.979  0.569  0.752 Amphiesmenoptera
## 5 -0.432 -0.150 -0.427 Amphiesmenoptera
## 6 -0.939 -0.459 -0.647 Amphiesmenoptera
```

#Trainnin Set

```r
insectSizeTrain <- insectSizeMeans %>%
                  select("logX1",'logX2', "logvol")


head(insectSizeTrain)
```

```
## # A tibble: 6 x 3
##    logX1  logX2 logvol
##    <dbl>  <dbl>  <dbl>
## 1  0.578  0.267  0.399
## 2  0.478  0.270  0.365
## 3  0.460  0.226  0.328
## 4  0.979  0.569  0.752
## 5 -0.432 -0.150 -0.427
## 6 -0.939 -0.459 -0.647
```

```r
insectSizeLabels <- insectSizeMeans$group
```

#Model

```r
k <- data.frame(k = 5)
model_knn <- train(x = data.frame(insectSizeTrain),
                   y = insectSizeLabels,
                   method='knn',
                   tuneGrid = k)
head(model_knn)
```

```
## $method
## [1] "knn"
## 
## $modelInfo
## $modelInfo$label
## [1] "k-Nearest Neighbors"
## 
## $modelInfo$library
## NULL
## 
## $modelInfo$loop
## NULL
## 
## $modelInfo$type
## [1] "Classification" "Regression"    
## 
## $modelInfo$parameters
##   parameter   class      label
## 1         k numeric #Neighbors
## 
## $modelInfo$grid
## function(x, y, len = NULL, search = "grid"){
##                     if(search == "grid") {
##                       out <- data.frame(k = (5:((2 * len)+4))[(5:((2 * len)+4))%%2 > 0])
##                     } else {
##                       by_val <- if(is.factor(y)) length(levels(y)) else 1
##                       out <- data.frame(k = sample(seq(1, floor(nrow(x)/3), by = by_val), size = len, replace = TRUE))
##                     }
##                     out
##                   }
## 
## $modelInfo$fit
## function(x, y, wts, param, lev, last, classProbs, ...) {
##                     if(is.factor(y))
##                     {
##                       knn3(as.matrix(x), y, k = param$k, ...)
##                     } else {
##                       knnreg(as.matrix(x), y, k = param$k, ...)
##                     }
##                   }
## <bytecode: 0x000000001ae94a10>
## 
## $modelInfo$predict
## function(modelFit, newdata, submodels = NULL) {
##                     if(modelFit$problemType == "Classification")
##                     {
##                       out <- predict(modelFit, newdata,  type = "class")
##                     } else {
##                       out <- predict(modelFit, newdata)
##                     }
##                     out
##                   }
## <bytecode: 0x000000001b0878b8>
## 
## $modelInfo$predictors
## function(x, ...) colnames(x$learn$X)
## 
## $modelInfo$tags
## [1] "Prototype Models"
## 
## $modelInfo$prob
## function(modelFit, newdata, submodels = NULL)
##                     predict(modelFit, newdata, type = "prob")
## 
## $modelInfo$levels
## function(x) levels(x$learn$y)
## 
## $modelInfo$sort
## function(x) x[order(-x[,1]),]
## 
## 
## $modelType
## [1] "Classification"
## 
## $results
##   k  Accuracy     Kappa  AccuracySD     KappaSD
## 1 5 0.5253782 0.4329895 0.007560774 0.008821859
## 
## $pred
## NULL
## 
## $bestTune
##   k
## 1 5
```
#Prediction

```r
newObs <- data.frame(logX1= 0.577,logX2=0.267, logvol= 0.398)

predict(object = model_knn, newdata = newObs)
```

```
## [1] Antliophora
## 9 Levels: Amphiesmenoptera Antliophora Apterygota ... Psocodea
```

#Visualization


```r
new_logX1 <- seq(from = min(insectSizeTrain$logX1), to = max(insectSizeTrain$logX1), length.out = 10)
new_logX2 <- seq(from = min(insectSizeTrain$logX2), to = max(insectSizeTrain$logX2), length.out = 10)
new_logvol <- seq(from = min(insectSizeTrain$logvol), to = median(insectSizeTrain$logvol), length.out = 10)

grid_data <- expand_grid(logX1 = new_logX1,
                         logX2 = new_logX2,
                         logvol = new_logvol)

grid_data$group <- predict(object = model_knn, newdata = as.data.frame(grid_data))

insectSizeMeans %>% 
  ggplot(aes(x = logX1, y = logvol, color = group, fill = group)) + 
  #geom_point(data = grid_data, alpha = 0.5)+ 
  geom_point(alpha = 0.7, pch = 21, color = "black")+ 
  scale_color_brewer(type = "qual")
```

![](insect-pred_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
  #scale_fill_brewer(type = "qual")
```

# Data partition

```r
set.seed(500) # makes the random selection of rows reproducible
train <- insectSizeMeans$group %>%
          createDataPartition(p = 0.70, list = FALSE)
head(train)
```

```
##      Resample1
## [1,]         2
## [2,]         4
## [3,]         9
## [4,]        10
## [5,]        11
## [6,]        12
```

```r
insectSizeTrain2 <- insectSizeMeans[train,]%>%
                        select("logX1",'logX2', "logvol")
insectSizeTrainTest2 <- insectSizeMeans[-train,]%>%
                          select("logX1",'logX2', "logvol")

insectSizeLabels2 <- insectSizeMeans[train,]$group
                        #select('group')

head(insectSizeLabels2)
```

```
## [1] "Amphiesmenoptera" "Amphiesmenoptera" "Amphiesmenoptera"
## [4] "Amphiesmenoptera" "Amphiesmenoptera" "Amphiesmenoptera"
```

```r
typeof(insectSizeLabels2)
```

```
## [1] "character"
```
#Model from partition

```r
k <- data.frame(k =5)
model_knn2 <- train(x = data.frame(insectSizeTrain2),
                   y = insectSizeLabels2,
                   method='knn',
                   tuneGrid = k)
head(model_knn2)
```

```
## $method
## [1] "knn"
## 
## $modelInfo
## $modelInfo$label
## [1] "k-Nearest Neighbors"
## 
## $modelInfo$library
## NULL
## 
## $modelInfo$loop
## NULL
## 
## $modelInfo$type
## [1] "Classification" "Regression"    
## 
## $modelInfo$parameters
##   parameter   class      label
## 1         k numeric #Neighbors
## 
## $modelInfo$grid
## function(x, y, len = NULL, search = "grid"){
##                     if(search == "grid") {
##                       out <- data.frame(k = (5:((2 * len)+4))[(5:((2 * len)+4))%%2 > 0])
##                     } else {
##                       by_val <- if(is.factor(y)) length(levels(y)) else 1
##                       out <- data.frame(k = sample(seq(1, floor(nrow(x)/3), by = by_val), size = len, replace = TRUE))
##                     }
##                     out
##                   }
## 
## $modelInfo$fit
## function(x, y, wts, param, lev, last, classProbs, ...) {
##                     if(is.factor(y))
##                     {
##                       knn3(as.matrix(x), y, k = param$k, ...)
##                     } else {
##                       knnreg(as.matrix(x), y, k = param$k, ...)
##                     }
##                   }
## <bytecode: 0x000000001dedc278>
## 
## $modelInfo$predict
## function(modelFit, newdata, submodels = NULL) {
##                     if(modelFit$problemType == "Classification")
##                     {
##                       out <- predict(modelFit, newdata,  type = "class")
##                     } else {
##                       out <- predict(modelFit, newdata)
##                     }
##                     out
##                   }
## <bytecode: 0x000000001b7d3e18>
## 
## $modelInfo$predictors
## function(x, ...) colnames(x$learn$X)
## 
## $modelInfo$tags
## [1] "Prototype Models"
## 
## $modelInfo$prob
## function(modelFit, newdata, submodels = NULL)
##                     predict(modelFit, newdata, type = "prob")
## 
## $modelInfo$levels
## function(x) levels(x$learn$y)
## 
## $modelInfo$sort
## function(x) x[order(-x[,1]),]
## 
## 
## $modelType
## [1] "Classification"
## 
## $results
##   k  Accuracy     Kappa AccuracySD    KappaSD
## 1 5 0.5108502 0.4155547 0.01263492 0.01481557
## 
## $pred
## NULL
## 
## $bestTune
##   k
## 1 5
```
#Test samples

```r
prediction = predict(object = model_knn2, newdata = insectSizeTrainTest2)
prediction
```

```
##    [1] Antliophora      Hymenoptera      Amphiesmenoptera Amphiesmenoptera
##    [5] Neuropteroidea   Palaeoptera      Amphiesmenoptera Amphiesmenoptera
##    [9] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##   [13] Amphiesmenoptera Condylognatha    Amphiesmenoptera Condylognatha   
##   [17] Condylognatha    Neuropteroidea   Neuropteroidea   Amphiesmenoptera
##   [21] Polyneoptera     Condylognatha    Condylognatha    Amphiesmenoptera
##   [25] Amphiesmenoptera Amphiesmenoptera Neuropteroidea   Amphiesmenoptera
##   [29] Amphiesmenoptera Neuropteroidea   Amphiesmenoptera Amphiesmenoptera
##   [33] Neuropteroidea   Amphiesmenoptera Amphiesmenoptera Neuropteroidea  
##   [37] Condylognatha    Neuropteroidea   Polyneoptera     Amphiesmenoptera
##   [41] Amphiesmenoptera Amphiesmenoptera Hymenoptera      Condylognatha   
##   [45] Neuropteroidea   Condylognatha    Amphiesmenoptera Amphiesmenoptera
##   [49] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##   [53] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##   [57] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##   [61] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##   [65] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##   [69] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##   [73] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##   [77] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Hymenoptera     
##   [81] Condylognatha    Neuropteroidea   Amphiesmenoptera Amphiesmenoptera
##   [85] Polyneoptera     Polyneoptera     Amphiesmenoptera Amphiesmenoptera
##   [89] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##   [93] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##   [97] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [101] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [105] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [109] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [113] Amphiesmenoptera Amphiesmenoptera Condylognatha    Amphiesmenoptera
##  [117] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [121] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Neuropteroidea  
##  [125] Amphiesmenoptera Neuropteroidea   Amphiesmenoptera Amphiesmenoptera
##  [129] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [133] Amphiesmenoptera Neuropteroidea   Amphiesmenoptera Amphiesmenoptera
##  [137] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Neuropteroidea  
##  [141] Condylognatha    Polyneoptera     Polyneoptera     Polyneoptera    
##  [145] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [149] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Condylognatha   
##  [153] Condylognatha    Neuropteroidea   Amphiesmenoptera Condylognatha   
##  [157] Neuropteroidea   Amphiesmenoptera Amphiesmenoptera Hymenoptera     
##  [161] Amphiesmenoptera Condylognatha    Condylognatha    Condylognatha   
##  [165] Condylognatha    Neuropteroidea   Hymenoptera      Hymenoptera     
##  [169] Hymenoptera      Antliophora      Neuropteroidea   Polyneoptera    
##  [173] Polyneoptera     Hymenoptera      Polyneoptera     Polyneoptera    
##  [177] Hymenoptera      Polyneoptera     Polyneoptera     Polyneoptera    
##  [181] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [185] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [189] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [193] Polyneoptera     Polyneoptera     Hymenoptera      Polyneoptera    
##  [197] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [201] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [205] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [209] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [213] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [217] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [221] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [225] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [229] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [233] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [237] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [241] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [245] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [249] Polyneoptera     Hymenoptera      Polyneoptera     Polyneoptera    
##  [253] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [257] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [261] Condylognatha    Hymenoptera      Hymenoptera      Polyneoptera    
##  [265] Hymenoptera      Hymenoptera      Hymenoptera      Amphiesmenoptera
##  [269] Hymenoptera      Polyneoptera     Hymenoptera      Polyneoptera    
##  [273] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [277] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [281] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [285] Polyneoptera     Neuropteroidea   Polyneoptera     Polyneoptera    
##  [289] Polyneoptera     Polyneoptera     Condylognatha    Neuropteroidea  
##  [293] Amphiesmenoptera Neuropteroidea   Palaeoptera      Neuropteroidea  
##  [297] Condylognatha    Amphiesmenoptera Polyneoptera     Neuropteroidea  
##  [301] Palaeoptera      Palaeoptera      Polyneoptera     Polyneoptera    
##  [305] Polyneoptera     Polyneoptera     Neuropteroidea   Polyneoptera    
##  [309] Polyneoptera     Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [313] Polyneoptera     Amphiesmenoptera Polyneoptera     Polyneoptera    
##  [317] Psocodea         Polyneoptera     Hymenoptera      Antliophora     
##  [321] Hymenoptera      Polyneoptera     Polyneoptera     Polyneoptera    
##  [325] Amphiesmenoptera Condylognatha    Neuropteroidea   Neuropteroidea  
##  [329] Polyneoptera     Hymenoptera      Neuropteroidea   Condylognatha   
##  [333] Antliophora      Antliophora      Antliophora      Antliophora     
##  [337] Condylognatha    Neuropteroidea   Antliophora      Condylognatha   
##  [341] Hymenoptera      Antliophora      Antliophora      Antliophora     
##  [345] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
##  [349] Antliophora      Antliophora      Hymenoptera      Hymenoptera     
##  [353] Hymenoptera      Hymenoptera      Antliophora      Hymenoptera     
##  [357] Antliophora      Psocodea         Hymenoptera      Antliophora     
##  [361] Condylognatha    Condylognatha    Hymenoptera      Polyneoptera    
##  [365] Hymenoptera      Polyneoptera     Hymenoptera      Neuropteroidea  
##  [369] Antliophora      Antliophora      Hymenoptera      Antliophora     
##  [373] Condylognatha    Antliophora      Antliophora      Antliophora     
##  [377] Antliophora      Antliophora      Antliophora      Antliophora     
##  [381] Condylognatha    Condylognatha    Antliophora      Antliophora     
##  [385] Antliophora      Antliophora      Antliophora      Antliophora     
##  [389] Antliophora      Antliophora      Antliophora      Antliophora     
##  [393] Neuropteroidea   Antliophora      Palaeoptera      Palaeoptera     
##  [397] Palaeoptera      Antliophora      Hymenoptera      Antliophora     
##  [401] Antliophora      Antliophora      Hymenoptera      Antliophora     
##  [405] Antliophora      Antliophora      Palaeoptera      Hymenoptera     
##  [409] Antliophora      Antliophora      Hymenoptera      Hymenoptera     
##  [413] Hymenoptera      Hymenoptera      Amphiesmenoptera Hymenoptera     
##  [417] Condylognatha    Condylognatha    Condylognatha    Condylognatha   
##  [421] Antliophora      Hymenoptera      Antliophora      Hymenoptera     
##  [425] Antliophora      Antliophora      Antliophora      Antliophora     
##  [429] Antliophora      Hymenoptera      Antliophora      Antliophora     
##  [433] Antliophora      Antliophora      Hymenoptera      Psocodea        
##  [437] Antliophora      Palaeoptera      Hymenoptera      Palaeoptera     
##  [441] Palaeoptera      Palaeoptera      Palaeoptera      Palaeoptera     
##  [445] Palaeoptera      Polyneoptera     Polyneoptera     Palaeoptera     
##  [449] Amphiesmenoptera Polyneoptera     Palaeoptera      Palaeoptera     
##  [453] Palaeoptera      Hymenoptera      Hymenoptera      Hymenoptera     
##  [457] Antliophora      Polyneoptera     Palaeoptera      Hymenoptera     
##  [461] Palaeoptera      Hymenoptera      Palaeoptera      Palaeoptera     
##  [465] Antliophora      Palaeoptera      Palaeoptera      Palaeoptera     
##  [469] Palaeoptera      Antliophora      Hymenoptera      Palaeoptera     
##  [473] Palaeoptera      Neuropteroidea   Palaeoptera      Palaeoptera     
##  [477] Palaeoptera      Hymenoptera      Antliophora      Polyneoptera    
##  [481] Palaeoptera      Polyneoptera     Polyneoptera     Hymenoptera     
##  [485] Palaeoptera      Palaeoptera      Palaeoptera      Palaeoptera     
##  [489] Palaeoptera      Palaeoptera      Palaeoptera      Polyneoptera    
##  [493] Antliophora      Neuropteroidea   Polyneoptera     Neuropteroidea  
##  [497] Amphiesmenoptera Condylognatha    Hymenoptera      Palaeoptera     
##  [501] Polyneoptera     Amphiesmenoptera Condylognatha    Neuropteroidea  
##  [505] Hymenoptera      Amphiesmenoptera Condylognatha    Amphiesmenoptera
##  [509] Antliophora      Amphiesmenoptera Polyneoptera     Condylognatha   
##  [513] Condylognatha    Neuropteroidea   Hymenoptera      Hymenoptera     
##  [517] Neuropteroidea   Condylognatha    Condylognatha    Condylognatha   
##  [521] Antliophora      Polyneoptera     Amphiesmenoptera Amphiesmenoptera
##  [525] Antliophora      Neuropteroidea   Antliophora      Hymenoptera     
##  [529] Antliophora      Neuropteroidea   Neuropteroidea   Condylognatha   
##  [533] Amphiesmenoptera Antliophora      Hymenoptera      Condylognatha   
##  [537] Amphiesmenoptera Amphiesmenoptera Neuropteroidea   Condylognatha   
##  [541] Neuropteroidea   Hymenoptera      Neuropteroidea   Antliophora     
##  [545] Hymenoptera      Amphiesmenoptera Hymenoptera      Antliophora     
##  [549] Neuropteroidea   Neuropteroidea   Condylognatha    Hymenoptera     
##  [553] Amphiesmenoptera Antliophora      Antliophora      Condylognatha   
##  [557] Condylognatha    Palaeoptera      Condylognatha    Condylognatha   
##  [561] Neuropteroidea   Neuropteroidea   Condylognatha    Amphiesmenoptera
##  [565] Amphiesmenoptera Amphiesmenoptera Condylognatha    Condylognatha   
##  [569] Amphiesmenoptera Condylognatha    Condylognatha    Condylognatha   
##  [573] Neuropteroidea   Amphiesmenoptera Condylognatha    Condylognatha   
##  [577] Condylognatha    Amphiesmenoptera Condylognatha    Condylognatha   
##  [581] Amphiesmenoptera Condylognatha    Condylognatha    Polyneoptera    
##  [585] Condylognatha    Condylognatha    Condylognatha    Antliophora     
##  [589] Condylognatha    Antliophora      Condylognatha    Condylognatha   
##  [593] Neuropteroidea   Neuropteroidea   Neuropteroidea   Hymenoptera     
##  [597] Antliophora      Neuropteroidea   Neuropteroidea   Hymenoptera     
##  [601] Condylognatha    Amphiesmenoptera Condylognatha    Antliophora     
##  [605] Antliophora      Condylognatha    Hymenoptera      Condylognatha   
##  [609] Hymenoptera      Antliophora      Antliophora      Antliophora     
##  [613] Palaeoptera      Condylognatha    Psocodea         Condylognatha   
##  [617] Condylognatha    Condylognatha    Hymenoptera      Hymenoptera     
##  [621] Hymenoptera      Condylognatha    Antliophora      Antliophora     
##  [625] Hymenoptera      Condylognatha    Psocodea         Condylognatha   
##  [629] Condylognatha    Condylognatha    Antliophora      Condylognatha   
##  [633] Condylognatha    Condylognatha    Antliophora      Condylognatha   
##  [637] Amphiesmenoptera Hymenoptera      Condylognatha    Condylognatha   
##  [641] Antliophora      Condylognatha    Polyneoptera     Polyneoptera    
##  [645] Hymenoptera      Hymenoptera      Condylognatha    Hymenoptera     
##  [649] Antliophora      Hymenoptera      Hymenoptera      Hymenoptera     
##  [653] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
##  [657] Hymenoptera      Hymenoptera      Polyneoptera     Hymenoptera     
##  [661] Hymenoptera      Hymenoptera      Hymenoptera      Antliophora     
##  [665] Antliophora      Neuropteroidea   Hymenoptera      Hymenoptera     
##  [669] Condylognatha    Hymenoptera      Hymenoptera      Hymenoptera     
##  [673] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
##  [677] Antliophora      Antliophora      Palaeoptera      Hymenoptera     
##  [681] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
##  [685] Antliophora      Hymenoptera      Polyneoptera     Hymenoptera     
##  [689] Condylognatha    Hymenoptera      Hymenoptera      Condylognatha   
##  [693] Hymenoptera      Antliophora      Hymenoptera      Hymenoptera     
##  [697] Antliophora      Antliophora      Hymenoptera      Hymenoptera     
##  [701] Neuropteroidea   Condylognatha    Hymenoptera      Hymenoptera     
##  [705] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
##  [709] Hymenoptera      Hymenoptera      Hymenoptera      Condylognatha   
##  [713] Amphiesmenoptera Hymenoptera      Hymenoptera      Condylognatha   
##  [717] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
##  [721] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
##  [725] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
##  [729] Hymenoptera      Hymenoptera      Antliophora      Condylognatha   
##  [733] Hymenoptera      Hymenoptera      Hymenoptera      Polyneoptera    
##  [737] Polyneoptera     Hymenoptera      Polyneoptera     Hymenoptera     
##  [741] Hymenoptera      Hymenoptera      Hymenoptera      Polyneoptera    
##  [745] Hymenoptera      Hymenoptera      Condylognatha    Hymenoptera     
##  [749] Amphiesmenoptera Condylognatha    Condylognatha    Hymenoptera     
##  [753] Hymenoptera      Antliophora      Hymenoptera      Hymenoptera     
##  [757] Hymenoptera      Polyneoptera     Polyneoptera     Polyneoptera    
##  [761] Hymenoptera      Hymenoptera      Hymenoptera      Amphiesmenoptera
##  [765] Condylognatha    Hymenoptera      Hymenoptera      Hymenoptera     
##  [769] Polyneoptera     Hymenoptera      Hymenoptera      Hymenoptera     
##  [773] Hymenoptera      Hymenoptera      Polyneoptera     Hymenoptera     
##  [777] Neuropteroidea   Polyneoptera     Hymenoptera      Condylognatha   
##  [781] Amphiesmenoptera Amphiesmenoptera Neuropteroidea   Amphiesmenoptera
##  [785] Amphiesmenoptera Condylognatha    Amphiesmenoptera Amphiesmenoptera
##  [789] Condylognatha    Amphiesmenoptera Condylognatha    Condylognatha   
##  [793] Amphiesmenoptera Condylognatha    Condylognatha    Amphiesmenoptera
##  [797] Palaeoptera      Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [801] Palaeoptera      Condylognatha    Amphiesmenoptera Condylognatha   
##  [805] Amphiesmenoptera Amphiesmenoptera Condylognatha    Condylognatha   
##  [809] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [813] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [817] Amphiesmenoptera Neuropteroidea   Amphiesmenoptera Neuropteroidea  
##  [821] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [825] Neuropteroidea   Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [829] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [833] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [837] Neuropteroidea   Amphiesmenoptera Amphiesmenoptera Polyneoptera    
##  [841] Polyneoptera     Antliophora      Amphiesmenoptera Polyneoptera    
##  [845] Condylognatha    Amphiesmenoptera Amphiesmenoptera Polyneoptera    
##  [849] Amphiesmenoptera Neuropteroidea   Polyneoptera     Amphiesmenoptera
##  [853] Condylognatha    Condylognatha    Amphiesmenoptera Amphiesmenoptera
##  [857] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [861] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [865] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Neuropteroidea  
##  [869] Condylognatha    Neuropteroidea   Neuropteroidea   Polyneoptera    
##  [873] Amphiesmenoptera Polyneoptera     Hymenoptera      Polyneoptera    
##  [877] Polyneoptera     Polyneoptera     Condylognatha    Neuropteroidea  
##  [881] Palaeoptera      Neuropteroidea   Neuropteroidea   Amphiesmenoptera
##  [885] Condylognatha    Antliophora      Condylognatha    Polyneoptera    
##  [889] Hymenoptera      Polyneoptera     Hymenoptera      Condylognatha   
##  [893] Neuropteroidea   Polyneoptera     Polyneoptera     Antliophora     
##  [897] Condylognatha    Polyneoptera     Hymenoptera      Polyneoptera    
##  [901] Polyneoptera     Condylognatha    Neuropteroidea   Amphiesmenoptera
##  [905] Condylognatha    Neuropteroidea   Neuropteroidea   Neuropteroidea  
##  [909] Antliophora      Neuropteroidea   Neuropteroidea   Condylognatha   
##  [913] Neuropteroidea   Amphiesmenoptera Neuropteroidea   Hymenoptera     
##  [917] Hymenoptera      Hymenoptera      Hymenoptera      Neuropteroidea  
##  [921] Hymenoptera      Antliophora      Condylognatha    Hymenoptera     
##  [925] Palaeoptera      Neuropteroidea   Neuropteroidea   Condylognatha   
##  [929] Hymenoptera      Hymenoptera      Hymenoptera      Condylognatha   
##  [933] Hymenoptera      Hymenoptera      Hymenoptera      Amphiesmenoptera
##  [937] Hymenoptera      Antliophora      Antliophora      Amphiesmenoptera
##  [941] Polyneoptera     Hymenoptera      Hymenoptera      Hymenoptera     
##  [945] Hymenoptera      Hymenoptera      Hymenoptera      Neuropteroidea  
##  [949] Hymenoptera      Antliophora      Hymenoptera      Hymenoptera     
##  [953] Antliophora      Hymenoptera      Hymenoptera      Polyneoptera    
##  [957] Hymenoptera      Condylognatha    Neuropteroidea   Neuropteroidea  
##  [961] Polyneoptera     Antliophora      Antliophora      Antliophora     
##  [965] Condylognatha    Hymenoptera      Hymenoptera      Hymenoptera     
##  [969] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
##  [973] Hymenoptera      Neuropteroidea   Hymenoptera      Condylognatha   
##  [977] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
##  [981] Neuropteroidea   Condylognatha    Antliophora      Hymenoptera     
##  [985] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
##  [989] Hymenoptera      Hymenoptera      Polyneoptera     Neuropteroidea  
##  [993] Hymenoptera      Hymenoptera      Antliophora      Hymenoptera     
##  [997] Amphiesmenoptera Antliophora      Polyneoptera     Hymenoptera     
## [1001] Antliophora      Polyneoptera     Polyneoptera     Polyneoptera    
## [1005] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1009] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1013] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1017] Polyneoptera     Polyneoptera     Neuropteroidea   Condylognatha   
## [1021] Polyneoptera     Neuropteroidea   Amphiesmenoptera Amphiesmenoptera
## [1025] Amphiesmenoptera Neuropteroidea   Amphiesmenoptera Amphiesmenoptera
## [1029] Polyneoptera     Hymenoptera      Hymenoptera      Neuropteroidea  
## [1033] Condylognatha    Hymenoptera      Hymenoptera      Polyneoptera    
## [1037] Polyneoptera     Polyneoptera     Hymenoptera      Hymenoptera     
## [1041] Condylognatha    Hymenoptera      Polyneoptera     Antliophora     
## [1045] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1049] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1053] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1057] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1061] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1065] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1069] Polyneoptera     Polyneoptera     Polyneoptera     Neuropteroidea  
## [1073] Polyneoptera     Polyneoptera     Hymenoptera      Hymenoptera     
## [1077] Hymenoptera      Hymenoptera      Hymenoptera      Condylognatha   
## [1081] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
## [1085] Hymenoptera      Hymenoptera      Hymenoptera      Antliophora     
## [1089] Hymenoptera      Antliophora      Polyneoptera     Hymenoptera     
## [1093] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
## [1097] Condylognatha    Polyneoptera     Polyneoptera     Polyneoptera    
## [1101] Amphiesmenoptera Hymenoptera      Palaeoptera      Antliophora     
## [1105] Amphiesmenoptera Antliophora      Condylognatha    Hymenoptera     
## [1109] Hymenoptera      Condylognatha    Condylognatha    Neuropteroidea  
## [1113] Condylognatha    Amphiesmenoptera Hymenoptera      Hymenoptera     
## [1117] Hymenoptera      Polyneoptera     Polyneoptera     Polyneoptera    
## [1121] Polyneoptera     Hymenoptera      Condylognatha    Antliophora     
## [1125] Hymenoptera      Condylognatha    Condylognatha    Antliophora     
## [1129] Polyneoptera     Condylognatha    Hymenoptera      Hymenoptera     
## [1133] Amphiesmenoptera Palaeoptera      Hymenoptera      Hymenoptera     
## [1137] Hymenoptera      Amphiesmenoptera Antliophora      Condylognatha   
## [1141] Neuropteroidea   Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1145] Amphiesmenoptera Polyneoptera     Amphiesmenoptera Amphiesmenoptera
## [1149] Antliophora      Amphiesmenoptera Amphiesmenoptera Polyneoptera    
## [1153] Condylognatha    Condylognatha    Polyneoptera     Polyneoptera    
## [1157] Amphiesmenoptera Palaeoptera      Hymenoptera      Polyneoptera    
## [1161] Antliophora      Antliophora      Antliophora      Antliophora     
## [1165] Antliophora      Condylognatha    Antliophora      Polyneoptera    
## [1169] Hymenoptera      Neuropteroidea   Condylognatha    Condylognatha   
## [1173] Condylognatha    Condylognatha    Condylognatha    Neuropteroidea  
## [1177] Neuropteroidea   Condylognatha    Neuropteroidea   Condylognatha   
## [1181] Antliophora      Antliophora      Neuropteroidea   Condylognatha   
## [1185] Neuropteroidea   Hymenoptera      Hymenoptera      Hymenoptera     
## [1189] Hymenoptera      Condylognatha    Polyneoptera     Neuropteroidea  
## [1193] Polyneoptera     Polyneoptera     Condylognatha    Polyneoptera    
## [1197] Hymenoptera      Hymenoptera      Neuropteroidea   Amphiesmenoptera
## [1201] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1205] Condylognatha    Amphiesmenoptera Condylognatha    Amphiesmenoptera
## [1209] Amphiesmenoptera Amphiesmenoptera Polyneoptera     Polyneoptera    
## [1213] Palaeoptera      Antliophora      Polyneoptera     Polyneoptera    
## [1217] Polyneoptera     Antliophora      Polyneoptera     Antliophora     
## [1221] Neuropteroidea   Neuropteroidea   Neuropteroidea   Antliophora     
## [1225] Hymenoptera      Antliophora      Condylognatha    Hymenoptera     
## [1229] Amphiesmenoptera Neuropteroidea   Polyneoptera     Amphiesmenoptera
## [1233] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1237] Neuropteroidea   Amphiesmenoptera Neuropteroidea   Amphiesmenoptera
## [1241] Neuropteroidea   Polyneoptera     Polyneoptera     Neuropteroidea  
## [1245] Condylognatha    Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1249] Amphiesmenoptera Condylognatha    Amphiesmenoptera Hymenoptera     
## [1253] Condylognatha    Condylognatha    Condylognatha    Condylognatha   
## [1257] Amphiesmenoptera Condylognatha    Condylognatha    Amphiesmenoptera
## [1261] Amphiesmenoptera Condylognatha    Palaeoptera      Condylognatha   
## [1265] Condylognatha    Condylognatha    Condylognatha    Amphiesmenoptera
## [1269] Amphiesmenoptera Condylognatha    Neuropteroidea   Polyneoptera    
## [1273] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1277] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1281] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1285] Amphiesmenoptera Polyneoptera     Polyneoptera     Polyneoptera    
## [1289] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1293] Condylognatha    Condylognatha    Polyneoptera     Polyneoptera    
## [1297] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1301] Polyneoptera     Polyneoptera     Amphiesmenoptera Polyneoptera    
## [1305] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1309] Polyneoptera     Condylognatha    Amphiesmenoptera Neuropteroidea  
## [1313] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1317] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1321] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1325] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1329] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1333] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1337] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1341] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1345] Polyneoptera     Polyneoptera     Neuropteroidea   Hymenoptera     
## [1349] Antliophora      Condylognatha    Condylognatha    Antliophora     
## [1353] Neuropteroidea   Condylognatha    Hymenoptera      Hymenoptera     
## [1357] Antliophora      Amphiesmenoptera Neuropteroidea   Neuropteroidea  
## [1361] Condylognatha    Hymenoptera      Hymenoptera      Hymenoptera     
## [1365] Hymenoptera      Antliophora      Hymenoptera      Hymenoptera     
## [1369] Antliophora      Hymenoptera      Hymenoptera      Condylognatha   
## [1373] Condylognatha    Condylognatha    Polyneoptera     Neuropteroidea  
## [1377] Antliophora      Amphiesmenoptera Polyneoptera     Condylognatha   
## [1381] Hymenoptera      Antliophora      Hymenoptera      Antliophora     
## [1385] Amphiesmenoptera Polyneoptera     Antliophora      Hymenoptera     
## [1389] Hymenoptera      Antliophora      Hymenoptera      Hymenoptera     
## [1393] Neuropteroidea   Condylognatha    Hymenoptera      Hymenoptera     
## [1397] Polyneoptera     Condylognatha    Antliophora      Palaeoptera     
## [1401] Hymenoptera      Hymenoptera      Palaeoptera      Hymenoptera     
## [1405] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
## [1409] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
## [1413] Hymenoptera      Hymenoptera      Antliophora      Palaeoptera     
## [1417] Hymenoptera      Antliophora      Condylognatha    Condylognatha   
## [1421] Condylognatha    Condylognatha    Hymenoptera      Hymenoptera     
## [1425] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
## [1429] Antliophora      Hymenoptera      Hymenoptera      Hymenoptera     
## [1433] Hymenoptera      Condylognatha    Hymenoptera      Hymenoptera     
## [1437] Hymenoptera      Antliophora      Condylognatha    Hymenoptera     
## [1441] Hymenoptera      Antliophora      Hymenoptera      Neuropteroidea  
## [1445] Hymenoptera      Hymenoptera      Amphiesmenoptera Amphiesmenoptera
## [1449] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1453] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1457] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1461] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1465] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1469] Amphiesmenoptera Palaeoptera      Amphiesmenoptera Neuropteroidea  
## [1473] Hymenoptera      Palaeoptera      Palaeoptera      Hymenoptera     
## [1477] Hymenoptera      Hymenoptera      Palaeoptera      Hymenoptera     
## [1481] Condylognatha    Amphiesmenoptera Condylognatha    Neuropteroidea  
## [1485] Hymenoptera      Antliophora      Hymenoptera      Antliophora     
## [1489] Neuropteroidea   Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1493] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Polyneoptera    
## [1497] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1501] Hymenoptera      Polyneoptera     Polyneoptera     Amphiesmenoptera
## [1505] Condylognatha    Condylognatha    Condylognatha    Antliophora     
## [1509] Hymenoptera      Neuropteroidea   Condylognatha    Antliophora     
## [1513] Amphiesmenoptera Amphiesmenoptera Antliophora      Neuropteroidea  
## [1517] Amphiesmenoptera Condylognatha    Amphiesmenoptera Hymenoptera     
## [1521] Hymenoptera      Condylognatha    Condylognatha    Neuropteroidea  
## [1525] Condylognatha    Hymenoptera      Amphiesmenoptera Hymenoptera     
## [1529] Polyneoptera     Neuropteroidea   Neuropteroidea   Neuropteroidea  
## [1533] Neuropteroidea   Condylognatha    Neuropteroidea   Hymenoptera     
## [1537] Antliophora      Antliophora      Antliophora      Antliophora     
## [1541] Hymenoptera      Hymenoptera      Antliophora      Hymenoptera     
## [1545] Condylognatha    Hymenoptera      Hymenoptera      Polyneoptera    
## [1549] Hymenoptera      Condylognatha    Neuropteroidea   Amphiesmenoptera
## [1553] Condylognatha    Condylognatha    Condylognatha    Polyneoptera    
## [1557] Amphiesmenoptera Neuropteroidea   Antliophora      Polyneoptera    
## [1561] Amphiesmenoptera Antliophora      Condylognatha    Amphiesmenoptera
## [1565] Antliophora      Polyneoptera     Palaeoptera      Palaeoptera     
## [1569] Condylognatha    Antliophora      Condylognatha    Condylognatha   
## [1573] Polyneoptera     Polyneoptera     Hymenoptera      Antliophora     
## [1577] Antliophora      Antliophora      Palaeoptera      Antliophora     
## [1581] Neuropteroidea   Neuropteroidea   Antliophora      Hymenoptera     
## [1585] Antliophora      Hymenoptera      Antliophora      Condylognatha   
## [1589] Antliophora      Antliophora      Antliophora      Condylognatha   
## [1593] Hymenoptera      Antliophora      Antliophora      Condylognatha   
## [1597] Antliophora      Antliophora      Hymenoptera      Condylognatha   
## [1601] Condylognatha    Amphiesmenoptera Amphiesmenoptera Palaeoptera     
## [1605] Condylognatha    Hymenoptera      Hymenoptera      Amphiesmenoptera
## [1609] Condylognatha    Hymenoptera      Antliophora      Condylognatha   
## [1613] Antliophora      Antliophora      Hymenoptera      Neuropteroidea  
## [1617] Hymenoptera      Antliophora      Hymenoptera      Hymenoptera     
## [1621] Antliophora      Antliophora      Antliophora      Condylognatha   
## [1625] Hymenoptera      Antliophora      Hymenoptera      Condylognatha   
## [1629] Antliophora      Neuropteroidea   Hymenoptera      Hymenoptera     
## [1633] Antliophora      Condylognatha    Hymenoptera      Condylognatha   
## [1637] Condylognatha    Antliophora      Condylognatha    Antliophora     
## [1641] Hymenoptera      Antliophora      Condylognatha    Condylognatha   
## [1645] Amphiesmenoptera Amphiesmenoptera Neuropteroidea   Amphiesmenoptera
## [1649] Antliophora      Hymenoptera      Condylognatha    Neuropteroidea  
## [1653] Condylognatha    Condylognatha    Condylognatha    Neuropteroidea  
## [1657] Neuropteroidea   Neuropteroidea   Amphiesmenoptera Condylognatha   
## [1661] Hymenoptera      Antliophora      Condylognatha    Neuropteroidea  
## [1665] Polyneoptera     Polyneoptera     Neuropteroidea   Hymenoptera     
## [1669] Antliophora      Antliophora      Antliophora      Neuropteroidea  
## [1673] Condylognatha    Neuropteroidea   Neuropteroidea   Hymenoptera     
## [1677] Amphiesmenoptera Amphiesmenoptera Hymenoptera      Condylognatha   
## [1681] Condylognatha    Polyneoptera     Amphiesmenoptera Condylognatha   
## [1685] Amphiesmenoptera Polyneoptera     Hymenoptera      Condylognatha   
## [1689] Neuropteroidea   Polyneoptera     Polyneoptera     Neuropteroidea  
## [1693] Neuropteroidea   Neuropteroidea   Neuropteroidea   Polyneoptera    
## [1697] Neuropteroidea   Amphiesmenoptera Amphiesmenoptera Neuropteroidea  
## [1701] Condylognatha    Neuropteroidea   Antliophora      Amphiesmenoptera
## [1705] Neuropteroidea   Polyneoptera     Condylognatha    Neuropteroidea  
## [1709] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Palaeoptera     
## [1713] Neuropteroidea   Hymenoptera      Amphiesmenoptera Hymenoptera     
## [1717] Condylognatha    Amphiesmenoptera Polyneoptera     Amphiesmenoptera
## [1721] Condylognatha    Amphiesmenoptera Neuropteroidea   Amphiesmenoptera
## [1725] Condylognatha    Neuropteroidea   Neuropteroidea   Polyneoptera    
## [1729] Amphiesmenoptera Hymenoptera      Antliophora      Palaeoptera     
## [1733] Antliophora      Antliophora      Polyneoptera     Polyneoptera    
## [1737] Antliophora      Hymenoptera      Polyneoptera     Antliophora     
## [1741] Antliophora      Neuropteroidea   Polyneoptera     Condylognatha   
## [1745] Hymenoptera      Condylognatha    Antliophora      Hymenoptera     
## [1749] Amphiesmenoptera Neuropteroidea   Amphiesmenoptera Condylognatha   
## [1753] Hymenoptera      Hymenoptera      Antliophora      Condylognatha   
## [1757] Antliophora      Amphiesmenoptera Antliophora      Hymenoptera     
## [1761] Amphiesmenoptera Amphiesmenoptera Neuropteroidea   Neuropteroidea  
## [1765] Neuropteroidea   Hymenoptera      Condylognatha    Neuropteroidea  
## [1769] Amphiesmenoptera Polyneoptera     Amphiesmenoptera Polyneoptera    
## [1773] Amphiesmenoptera Hymenoptera      Antliophora      Condylognatha   
## [1777] Amphiesmenoptera Amphiesmenoptera Condylognatha    Amphiesmenoptera
## [1781] Condylognatha    Amphiesmenoptera Neuropteroidea   Polyneoptera    
## [1785] Amphiesmenoptera Amphiesmenoptera Condylognatha    Hymenoptera     
## [1789] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1793] Amphiesmenoptera Polyneoptera     Amphiesmenoptera Neuropteroidea  
## [1797] Condylognatha    Amphiesmenoptera Neuropteroidea   Neuropteroidea  
## [1801] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1805] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1809] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1813] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1817] Amphiesmenoptera Amphiesmenoptera Condylognatha    Amphiesmenoptera
## [1821] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1825] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1829] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1833] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1837] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1841] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1845] Amphiesmenoptera Amphiesmenoptera Condylognatha    Amphiesmenoptera
## [1849] Amphiesmenoptera Amphiesmenoptera Neuropteroidea   Amphiesmenoptera
## [1853] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1857] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1861] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1865] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1869] Amphiesmenoptera Amphiesmenoptera Polyneoptera     Amphiesmenoptera
## [1873] Palaeoptera      Amphiesmenoptera Condylognatha    Condylognatha   
## [1877] Condylognatha    Hymenoptera      Hymenoptera      Polyneoptera    
## [1881] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1885] Polyneoptera     Neuropteroidea   Polyneoptera     Polyneoptera    
## [1889] Polyneoptera     Antliophora      Hymenoptera      Amphiesmenoptera
## [1893] Antliophora      Amphiesmenoptera Neuropteroidea   Condylognatha   
## [1897] Amphiesmenoptera Condylognatha    Condylognatha    Antliophora     
## [1901] Condylognatha    Amphiesmenoptera Neuropteroidea   Polyneoptera    
## [1905] Condylognatha    Condylognatha    Condylognatha    Hymenoptera     
## [1909] Hymenoptera      Hymenoptera      Neuropteroidea   Hymenoptera     
## [1913] Condylognatha    Hymenoptera      Hymenoptera      Hymenoptera     
## [1917] Palaeoptera      Condylognatha    Hymenoptera      Condylognatha   
## [1921] Condylognatha    Amphiesmenoptera Polyneoptera     Hymenoptera     
## [1925] Condylognatha    Hymenoptera      Neuropteroidea   Hymenoptera     
## [1929] Neuropteroidea   Condylognatha    Condylognatha    Neuropteroidea  
## [1933] Hymenoptera      Condylognatha    Polyneoptera     Polyneoptera    
## [1937] Amphiesmenoptera Amphiesmenoptera Hymenoptera      Amphiesmenoptera
## [1941] Amphiesmenoptera Amphiesmenoptera Condylognatha    Condylognatha   
## [1945] Amphiesmenoptera Condylognatha    Condylognatha    Hymenoptera     
## [1949] Hymenoptera      Condylognatha    Neuropteroidea   Neuropteroidea  
## [1953] Hymenoptera      Hymenoptera      Condylognatha    Hymenoptera     
## [1957] Condylognatha    Hymenoptera      Hymenoptera      Hymenoptera     
## [1961] Hymenoptera      Hymenoptera      Antliophora      Hymenoptera     
## [1965] Hymenoptera      Hymenoptera      Antliophora      Hymenoptera     
## [1969] Condylognatha    Hymenoptera      Hymenoptera      Hymenoptera     
## [1973] Neuropteroidea   Antliophora      Antliophora      Hymenoptera     
## [1977] Antliophora      Hymenoptera      Antliophora      Hymenoptera     
## [1981] Hymenoptera      Hymenoptera      Antliophora      Condylognatha   
## [1985] Hymenoptera      Hymenoptera      Antliophora      Hymenoptera     
## [1989] Condylognatha    Hymenoptera      Hymenoptera      Hymenoptera     
## [1993] Palaeoptera      Palaeoptera      Amphiesmenoptera Hymenoptera     
## [1997] Hymenoptera      Hymenoptera      Condylognatha    Amphiesmenoptera
## [2001] Neuropteroidea   Amphiesmenoptera Neuropteroidea   Polyneoptera    
## [2005] Polyneoptera     Neuropteroidea   Neuropteroidea   Amphiesmenoptera
## [2009] Neuropteroidea   Hymenoptera      Hymenoptera      Hymenoptera     
## [2013] Hymenoptera      Antliophora      Hymenoptera      Hymenoptera     
## [2017] Polyneoptera     Condylognatha    Hymenoptera      Hymenoptera     
## [2021] Amphiesmenoptera Hymenoptera      Amphiesmenoptera Hymenoptera     
## [2025] Hymenoptera      Neuropteroidea   Antliophora      Amphiesmenoptera
## [2029] Condylognatha    Antliophora      Hymenoptera      Hymenoptera     
## [2033] Neuropteroidea   Hymenoptera      Hymenoptera      Antliophora     
## [2037] Polyneoptera     Polyneoptera     Hymenoptera      Hymenoptera     
## [2041] Hymenoptera      Hymenoptera      Hymenoptera      Polyneoptera    
## [2045] Amphiesmenoptera Polyneoptera     Hymenoptera      Antliophora     
## [2049] Hymenoptera      Condylognatha    Amphiesmenoptera Condylognatha   
## [2053] Condylognatha    Condylognatha    Hymenoptera      Polyneoptera    
## [2057] Hymenoptera      Hymenoptera      Hymenoptera      Amphiesmenoptera
## [2061] Palaeoptera      Hymenoptera      Polyneoptera     Polyneoptera    
## [2065] Polyneoptera     Hymenoptera      Hymenoptera      Hymenoptera     
## [2069] Condylognatha    Hymenoptera      Antliophora      Hymenoptera     
## [2073] Hymenoptera      Polyneoptera     Antliophora      Neuropteroidea  
## [2077] Hymenoptera      Hymenoptera      Hymenoptera      Antliophora     
## [2081] Condylognatha    Hymenoptera      Hymenoptera      Palaeoptera     
## [2085] Palaeoptera      Hymenoptera      Amphiesmenoptera Amphiesmenoptera
## [2089] Condylognatha    Condylognatha    Neuropteroidea   Amphiesmenoptera
## [2093] Neuropteroidea   Condylognatha    Antliophora      Polyneoptera    
## [2097] Condylognatha    Polyneoptera     Hymenoptera      Amphiesmenoptera
## [2101] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [2105] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Neuropteroidea  
## [2109] Amphiesmenoptera Antliophora      Hymenoptera      Condylognatha   
## [2113] Antliophora      Amphiesmenoptera Condylognatha    Amphiesmenoptera
## [2117] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [2121] Polyneoptera     Hymenoptera      Amphiesmenoptera Amphiesmenoptera
## [2125] Polyneoptera     Condylognatha    Amphiesmenoptera Condylognatha   
## [2129] Amphiesmenoptera Amphiesmenoptera Neuropteroidea   Condylognatha   
## [2133] Neuropteroidea   Antliophora      Condylognatha    Antliophora     
## [2137] Amphiesmenoptera Neuropteroidea   Hymenoptera      Condylognatha   
## [2141] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
## [2145] Hymenoptera      Hymenoptera      Polyneoptera     Polyneoptera    
## [2149] Condylognatha    Condylognatha    Antliophora      Hymenoptera     
## [2153] Hymenoptera      Hymenoptera      Amphiesmenoptera Amphiesmenoptera
## [2157] Amphiesmenoptera Condylognatha    Polyneoptera     Hymenoptera     
## [2161] Polyneoptera     Amphiesmenoptera Neuropteroidea   Hymenoptera     
## [2165] Palaeoptera      Hymenoptera      Hymenoptera      Antliophora     
## [2169] Condylognatha    Neuropteroidea   Amphiesmenoptera Amphiesmenoptera
## [2173] Hymenoptera      Neuropteroidea   Amphiesmenoptera Amphiesmenoptera
## [2177] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [2181] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [2185] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [2189] Amphiesmenoptera Amphiesmenoptera Hymenoptera      Condylognatha   
## [2193] Neuropteroidea   Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [2197] Palaeoptera      Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [2201] Antliophora      Hymenoptera      Condylognatha    Antliophora     
## [2205] Hymenoptera      Hymenoptera      Hymenoptera      Neuropteroidea  
## [2209] Polyneoptera     Antliophora      Antliophora      Amphiesmenoptera
## [2213] Antliophora      Antliophora      Antliophora     
## 9 Levels: Amphiesmenoptera Antliophora Apterygota ... Psocodea
```

#insectSizeMeans into numeric

```r
# Convert character vector to factor
comp = insectSizeMeans[-train,]$group 
        #%>%
       #select('group')
tableCompare<- factor(comp, levels = as.character(unique(comp)))

typeof(comp)
```

```
## [1] "character"
```

```r
head(tableCompare)
```

```
## [1] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [5] Amphiesmenoptera Amphiesmenoptera
## 9 Levels: Amphiesmenoptera Polyneoptera Antliophora ... Apterygota
```



#ConfusionMatrix


```r
confusionMatrix(prediction,tableCompare)
```

```
## Warning in confusionMatrix.default(prediction, tableCompare): Levels are
## not in the same order for reference and data. Refactoring data to match.
```

```
## Confusion Matrix and Statistics
## 
##                   Reference
## Prediction         Amphiesmenoptera Polyneoptera Antliophora
##   Amphiesmenoptera              348           21          13
##   Polyneoptera                   18          288           3
##   Antliophora                     4            3         107
##   Neuropteroidea                 36           16          14
##   Psocodea                        0            0           2
##   Condylognatha                  43           12          36
##   Palaeoptera                     5            5           9
##   Hymenoptera                     6           21          65
##   Apterygota                      0            0           0
##                   Reference
## Prediction         Neuropteroidea Psocodea Condylognatha Palaeoptera
##   Amphiesmenoptera             27        0            59          10
##   Polyneoptera                 38        0            14          11
##   Antliophora                  20        1            46           7
##   Neuropteroidea               54        3            52           1
##   Psocodea                      0        1             2           0
##   Condylognatha                56        3           114           4
##   Palaeoptera                   5        0             3          38
##   Hymenoptera                  27        4            54          17
##   Apterygota                    0        0             0           0
##                   Reference
## Prediction         Hymenoptera Apterygota
##   Amphiesmenoptera          12          1
##   Polyneoptera              32          1
##   Antliophora               54          0
##   Neuropteroidea            22          0
##   Psocodea                   0          0
##   Condylognatha             43          1
##   Palaeoptera                9          1
##   Hymenoptera              292          1
##   Apterygota                 0          0
## 
## Overall Statistics
##                                           
##                Accuracy : 0.5607          
##                  95% CI : (0.5398, 0.5815)
##     No Information Rate : 0.2095          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.4727          
##                                           
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: Amphiesmenoptera Class: Polyneoptera
## Sensitivity                           0.7565              0.7869
## Specificity                           0.9185              0.9367
## Pos Pred Value                        0.7088              0.7111
## Neg Pred Value                        0.9350              0.9569
## Prevalence                            0.2077              0.1652
## Detection Rate                        0.1571              0.1300
## Detection Prevalence                  0.2217              0.1828
## Balanced Accuracy                     0.8375              0.8618
##                      Class: Antliophora Class: Neuropteroidea
## Sensitivity                     0.42972               0.23789
## Specificity                     0.93133               0.92757
## Pos Pred Value                  0.44215               0.27273
## Neg Pred Value                  0.92803               0.91423
## Prevalence                      0.11242               0.10248
## Detection Rate                  0.04831               0.02438
## Detection Prevalence            0.10926               0.08939
## Balanced Accuracy               0.68053               0.58273
##                      Class: Psocodea Class: Condylognatha
## Sensitivity                0.0833333              0.33140
## Specificity                0.9981843              0.89417
## Pos Pred Value             0.2000000              0.36538
## Neg Pred Value             0.9950226              0.87914
## Prevalence                 0.0054176              0.15530
## Detection Rate             0.0004515              0.05147
## Detection Prevalence       0.0022573              0.14086
## Balanced Accuracy          0.5407588              0.61278
##                      Class: Palaeoptera Class: Hymenoptera
## Sensitivity                     0.43182             0.6293
## Specificity                     0.98260             0.8886
## Pos Pred Value                  0.50667             0.5996
## Neg Pred Value                  0.97664             0.9005
## Prevalence                      0.03973             0.2095
## Detection Rate                  0.01716             0.1318
## Detection Prevalence            0.03386             0.2199
## Balanced Accuracy               0.70721             0.7590
##                      Class: Apterygota
## Sensitivity                   0.000000
## Specificity                   1.000000
## Pos Pred Value                     NaN
## Neg Pred Value                0.997743
## Prevalence                    0.002257
## Detection Rate                0.000000
## Detection Prevalence          0.000000
## Balanced Accuracy             0.500000
```

#Cross validation

```r
trainCross = trainControl(method = 'repeatedcv',
                          number = 10,
                          repeats = 1,
                          search='grid')

#tuneGrid
k <- data.frame(k =3:10)
model_knnVal <- train(x = data.frame(insectSizeTrain2),
                   y = insectSizeLabels2,
                   method='knn',
                   tuneGrid = k,
                   trControl = trainCross)
head(model_knnVal)
```

```
## $method
## [1] "knn"
## 
## $modelInfo
## $modelInfo$label
## [1] "k-Nearest Neighbors"
## 
## $modelInfo$library
## NULL
## 
## $modelInfo$loop
## NULL
## 
## $modelInfo$type
## [1] "Classification" "Regression"    
## 
## $modelInfo$parameters
##   parameter   class      label
## 1         k numeric #Neighbors
## 
## $modelInfo$grid
## function(x, y, len = NULL, search = "grid"){
##                     if(search == "grid") {
##                       out <- data.frame(k = (5:((2 * len)+4))[(5:((2 * len)+4))%%2 > 0])
##                     } else {
##                       by_val <- if(is.factor(y)) length(levels(y)) else 1
##                       out <- data.frame(k = sample(seq(1, floor(nrow(x)/3), by = by_val), size = len, replace = TRUE))
##                     }
##                     out
##                   }
## 
## $modelInfo$fit
## function(x, y, wts, param, lev, last, classProbs, ...) {
##                     if(is.factor(y))
##                     {
##                       knn3(as.matrix(x), y, k = param$k, ...)
##                     } else {
##                       knnreg(as.matrix(x), y, k = param$k, ...)
##                     }
##                   }
## <bytecode: 0x000000001ac0e8c0>
## 
## $modelInfo$predict
## function(modelFit, newdata, submodels = NULL) {
##                     if(modelFit$problemType == "Classification")
##                     {
##                       out <- predict(modelFit, newdata,  type = "class")
##                     } else {
##                       out <- predict(modelFit, newdata)
##                     }
##                     out
##                   }
## <bytecode: 0x000000001a630a80>
## 
## $modelInfo$predictors
## function(x, ...) colnames(x$learn$X)
## 
## $modelInfo$tags
## [1] "Prototype Models"
## 
## $modelInfo$prob
## function(modelFit, newdata, submodels = NULL)
##                     predict(modelFit, newdata, type = "prob")
## 
## $modelInfo$levels
## function(x) levels(x$learn$y)
## 
## $modelInfo$sort
## function(x) x[order(-x[,1]),]
## 
## 
## $modelType
## [1] "Classification"
## 
## $results
##    k  Accuracy     Kappa AccuracySD    KappaSD
## 1  3 0.5253977 0.4333623 0.01855269 0.02137676
## 2  4 0.5358102 0.4448551 0.01771813 0.02080216
## 3  5 0.5423963 0.4526794 0.02242939 0.02606702
## 4  6 0.5450741 0.4558574 0.01845411 0.02169558
## 5  7 0.5421828 0.4517944 0.02021296 0.02362046
## 6  8 0.5549228 0.4667896 0.01957966 0.02308748
## 7  9 0.5522245 0.4634951 0.01906210 0.02239982
## 8 10 0.5535710 0.4649551 0.02411506 0.02815625
## 
## $pred
## NULL
## 
## $bestTune
##   k
## 6 8
```
#Use K=8 for training


```r
k <- data.frame(k =8)
model_knn3 <- train(x = data.frame(insectSizeTrain2),
                   y = insectSizeLabels2,
                   method='knn',
                   tuneGrid = k)
head(model_knn3)
```

```
## $method
## [1] "knn"
## 
## $modelInfo
## $modelInfo$label
## [1] "k-Nearest Neighbors"
## 
## $modelInfo$library
## NULL
## 
## $modelInfo$loop
## NULL
## 
## $modelInfo$type
## [1] "Classification" "Regression"    
## 
## $modelInfo$parameters
##   parameter   class      label
## 1         k numeric #Neighbors
## 
## $modelInfo$grid
## function(x, y, len = NULL, search = "grid"){
##                     if(search == "grid") {
##                       out <- data.frame(k = (5:((2 * len)+4))[(5:((2 * len)+4))%%2 > 0])
##                     } else {
##                       by_val <- if(is.factor(y)) length(levels(y)) else 1
##                       out <- data.frame(k = sample(seq(1, floor(nrow(x)/3), by = by_val), size = len, replace = TRUE))
##                     }
##                     out
##                   }
## 
## $modelInfo$fit
## function(x, y, wts, param, lev, last, classProbs, ...) {
##                     if(is.factor(y))
##                     {
##                       knn3(as.matrix(x), y, k = param$k, ...)
##                     } else {
##                       knnreg(as.matrix(x), y, k = param$k, ...)
##                     }
##                   }
## <bytecode: 0x000000001a987168>
## 
## $modelInfo$predict
## function(modelFit, newdata, submodels = NULL) {
##                     if(modelFit$problemType == "Classification")
##                     {
##                       out <- predict(modelFit, newdata,  type = "class")
##                     } else {
##                       out <- predict(modelFit, newdata)
##                     }
##                     out
##                   }
## <bytecode: 0x000000001d2c39c0>
## 
## $modelInfo$predictors
## function(x, ...) colnames(x$learn$X)
## 
## $modelInfo$tags
## [1] "Prototype Models"
## 
## $modelInfo$prob
## function(modelFit, newdata, submodels = NULL)
##                     predict(modelFit, newdata, type = "prob")
## 
## $modelInfo$levels
## function(x) levels(x$learn$y)
## 
## $modelInfo$sort
## function(x) x[order(-x[,1]),]
## 
## 
## $modelType
## [1] "Classification"
## 
## $results
##   k  Accuracy     Kappa  AccuracySD    KappaSD
## 1 8 0.5302296 0.4380846 0.009304374 0.01082189
## 
## $pred
## NULL
## 
## $bestTune
##   k
## 1 8
```
#Test with K=8

```r
prediction2 = predict(object = model_knn3, newdata = insectSizeTrainTest2)
prediction2
```

```
##    [1] Hymenoptera      Hymenoptera      Amphiesmenoptera Amphiesmenoptera
##    [5] Amphiesmenoptera Neuropteroidea   Amphiesmenoptera Amphiesmenoptera
##    [9] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##   [13] Amphiesmenoptera Condylognatha    Amphiesmenoptera Condylognatha   
##   [17] Condylognatha    Neuropteroidea   Neuropteroidea   Amphiesmenoptera
##   [21] Polyneoptera     Condylognatha    Condylognatha    Neuropteroidea  
##   [25] Amphiesmenoptera Condylognatha    Neuropteroidea   Amphiesmenoptera
##   [29] Amphiesmenoptera Hymenoptera      Amphiesmenoptera Amphiesmenoptera
##   [33] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Neuropteroidea  
##   [37] Condylognatha    Hymenoptera      Polyneoptera     Amphiesmenoptera
##   [41] Amphiesmenoptera Amphiesmenoptera Hymenoptera      Amphiesmenoptera
##   [45] Neuropteroidea   Condylognatha    Amphiesmenoptera Amphiesmenoptera
##   [49] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##   [53] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##   [57] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##   [61] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##   [65] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##   [69] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##   [73] Amphiesmenoptera Amphiesmenoptera Condylognatha    Amphiesmenoptera
##   [77] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Hymenoptera     
##   [81] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##   [85] Polyneoptera     Polyneoptera     Amphiesmenoptera Amphiesmenoptera
##   [89] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##   [93] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##   [97] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [101] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [105] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [109] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [113] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [117] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [121] Amphiesmenoptera Amphiesmenoptera Condylognatha    Neuropteroidea  
##  [125] Amphiesmenoptera Polyneoptera     Amphiesmenoptera Amphiesmenoptera
##  [129] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [133] Amphiesmenoptera Condylognatha    Amphiesmenoptera Amphiesmenoptera
##  [137] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Neuropteroidea  
##  [141] Condylognatha    Polyneoptera     Polyneoptera     Polyneoptera    
##  [145] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [149] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Condylognatha   
##  [153] Condylognatha    Neuropteroidea   Amphiesmenoptera Condylognatha   
##  [157] Neuropteroidea   Amphiesmenoptera Amphiesmenoptera Hymenoptera     
##  [161] Amphiesmenoptera Condylognatha    Condylognatha    Condylognatha   
##  [165] Condylognatha    Neuropteroidea   Amphiesmenoptera Hymenoptera     
##  [169] Hymenoptera      Condylognatha    Neuropteroidea   Polyneoptera    
##  [173] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [177] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [181] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [185] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [189] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [193] Polyneoptera     Polyneoptera     Hymenoptera      Polyneoptera    
##  [197] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [201] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [205] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [209] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [213] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [217] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [221] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [225] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [229] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [233] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [237] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [241] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [245] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [249] Polyneoptera     Hymenoptera      Polyneoptera     Polyneoptera    
##  [253] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [257] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [261] Polyneoptera     Hymenoptera      Hymenoptera      Polyneoptera    
##  [265] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
##  [269] Neuropteroidea   Polyneoptera     Hymenoptera      Polyneoptera    
##  [273] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [277] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [281] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
##  [285] Polyneoptera     Neuropteroidea   Polyneoptera     Polyneoptera    
##  [289] Polyneoptera     Polyneoptera     Condylognatha    Neuropteroidea  
##  [293] Amphiesmenoptera Condylognatha    Palaeoptera      Neuropteroidea  
##  [297] Condylognatha    Amphiesmenoptera Condylognatha    Palaeoptera     
##  [301] Palaeoptera      Palaeoptera      Polyneoptera     Polyneoptera    
##  [305] Polyneoptera     Polyneoptera     Neuropteroidea   Polyneoptera    
##  [309] Polyneoptera     Amphiesmenoptera Antliophora      Amphiesmenoptera
##  [313] Polyneoptera     Amphiesmenoptera Polyneoptera     Polyneoptera    
##  [317] Condylognatha    Neuropteroidea   Condylognatha    Antliophora     
##  [321] Condylognatha    Polyneoptera     Polyneoptera     Polyneoptera    
##  [325] Amphiesmenoptera Condylognatha    Neuropteroidea   Antliophora     
##  [329] Polyneoptera     Hymenoptera      Hymenoptera      Condylognatha   
##  [333] Antliophora      Antliophora      Antliophora      Neuropteroidea  
##  [337] Antliophora      Condylognatha    Condylognatha    Condylognatha   
##  [341] Hymenoptera      Antliophora      Antliophora      Hymenoptera     
##  [345] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
##  [349] Hymenoptera      Antliophora      Antliophora      Hymenoptera     
##  [353] Hymenoptera      Hymenoptera      Antliophora      Hymenoptera     
##  [357] Antliophora      Amphiesmenoptera Hymenoptera      Antliophora     
##  [361] Condylognatha    Neuropteroidea   Hymenoptera      Polyneoptera    
##  [365] Hymenoptera      Polyneoptera     Hymenoptera      Hymenoptera     
##  [369] Antliophora      Antliophora      Hymenoptera      Antliophora     
##  [373] Antliophora      Antliophora      Antliophora      Antliophora     
##  [377] Antliophora      Antliophora      Antliophora      Antliophora     
##  [381] Condylognatha    Condylognatha    Antliophora      Antliophora     
##  [385] Antliophora      Antliophora      Antliophora      Antliophora     
##  [389] Antliophora      Antliophora      Antliophora      Antliophora     
##  [393] Neuropteroidea   Antliophora      Palaeoptera      Palaeoptera     
##  [397] Palaeoptera      Antliophora      Hymenoptera      Antliophora     
##  [401] Antliophora      Antliophora      Hymenoptera      Antliophora     
##  [405] Antliophora      Hymenoptera      Palaeoptera      Hymenoptera     
##  [409] Antliophora      Antliophora      Hymenoptera      Hymenoptera     
##  [413] Hymenoptera      Hymenoptera      Condylognatha    Hymenoptera     
##  [417] Condylognatha    Condylognatha    Antliophora      Antliophora     
##  [421] Antliophora      Hymenoptera      Antliophora      Hymenoptera     
##  [425] Antliophora      Antliophora      Antliophora      Antliophora     
##  [429] Hymenoptera      Hymenoptera      Hymenoptera      Antliophora     
##  [433] Hymenoptera      Antliophora      Palaeoptera      Psocodea        
##  [437] Antliophora      Palaeoptera      Hymenoptera      Palaeoptera     
##  [441] Palaeoptera      Palaeoptera      Palaeoptera      Hymenoptera     
##  [445] Palaeoptera      Polyneoptera     Polyneoptera     Palaeoptera     
##  [449] Polyneoptera     Polyneoptera     Palaeoptera      Palaeoptera     
##  [453] Palaeoptera      Hymenoptera      Hymenoptera      Palaeoptera     
##  [457] Palaeoptera      Polyneoptera     Palaeoptera      Palaeoptera     
##  [461] Palaeoptera      Hymenoptera      Palaeoptera      Palaeoptera     
##  [465] Antliophora      Palaeoptera      Palaeoptera      Palaeoptera     
##  [469] Palaeoptera      Antliophora      Hymenoptera      Palaeoptera     
##  [473] Palaeoptera      Palaeoptera      Palaeoptera      Palaeoptera     
##  [477] Polyneoptera     Hymenoptera      Antliophora      Amphiesmenoptera
##  [481] Palaeoptera      Polyneoptera     Polyneoptera     Hymenoptera     
##  [485] Palaeoptera      Palaeoptera      Palaeoptera      Palaeoptera     
##  [489] Palaeoptera      Palaeoptera      Palaeoptera      Polyneoptera    
##  [493] Hymenoptera      Neuropteroidea   Polyneoptera     Neuropteroidea  
##  [497] Amphiesmenoptera Condylognatha    Hymenoptera      Palaeoptera     
##  [501] Condylognatha    Amphiesmenoptera Condylognatha    Neuropteroidea  
##  [505] Hymenoptera      Amphiesmenoptera Condylognatha    Amphiesmenoptera
##  [509] Antliophora      Amphiesmenoptera Polyneoptera     Condylognatha   
##  [513] Condylognatha    Condylognatha    Hymenoptera      Hymenoptera     
##  [517] Neuropteroidea   Amphiesmenoptera Condylognatha    Amphiesmenoptera
##  [521] Antliophora      Polyneoptera     Amphiesmenoptera Condylognatha   
##  [525] Antliophora      Neuropteroidea   Antliophora      Antliophora     
##  [529] Antliophora      Neuropteroidea   Neuropteroidea   Condylognatha   
##  [533] Amphiesmenoptera Hymenoptera      Condylognatha    Neuropteroidea  
##  [537] Neuropteroidea   Amphiesmenoptera Condylognatha    Amphiesmenoptera
##  [541] Neuropteroidea   Hymenoptera      Neuropteroidea   Antliophora     
##  [545] Hymenoptera      Condylognatha    Hymenoptera      Antliophora     
##  [549] Neuropteroidea   Neuropteroidea   Hymenoptera      Hymenoptera     
##  [553] Neuropteroidea   Antliophora      Antliophora      Condylognatha   
##  [557] Condylognatha    Hymenoptera      Hymenoptera      Condylognatha   
##  [561] Neuropteroidea   Neuropteroidea   Condylognatha    Amphiesmenoptera
##  [565] Condylognatha    Amphiesmenoptera Condylognatha    Condylognatha   
##  [569] Condylognatha    Condylognatha    Condylognatha    Condylognatha   
##  [573] Neuropteroidea   Amphiesmenoptera Condylognatha    Condylognatha   
##  [577] Condylognatha    Amphiesmenoptera Amphiesmenoptera Condylognatha   
##  [581] Amphiesmenoptera Condylognatha    Condylognatha    Polyneoptera    
##  [585] Condylognatha    Neuropteroidea   Condylognatha    Hymenoptera     
##  [589] Condylognatha    Antliophora      Condylognatha    Condylognatha   
##  [593] Condylognatha    Neuropteroidea   Neuropteroidea   Hymenoptera     
##  [597] Condylognatha    Neuropteroidea   Neuropteroidea   Hymenoptera     
##  [601] Hymenoptera      Amphiesmenoptera Condylognatha    Antliophora     
##  [605] Antliophora      Condylognatha    Hymenoptera      Condylognatha   
##  [609] Hymenoptera      Polyneoptera     Hymenoptera      Palaeoptera     
##  [613] Palaeoptera      Condylognatha    Condylognatha    Hymenoptera     
##  [617] Condylognatha    Condylognatha    Hymenoptera      Condylognatha   
##  [621] Hymenoptera      Condylognatha    Condylognatha    Antliophora     
##  [625] Hymenoptera      Condylognatha    Condylognatha    Condylognatha   
##  [629] Condylognatha    Condylognatha    Condylognatha    Condylognatha   
##  [633] Condylognatha    Hymenoptera      Antliophora      Hymenoptera     
##  [637] Condylognatha    Condylognatha    Condylognatha    Condylognatha   
##  [641] Hymenoptera      Condylognatha    Polyneoptera     Palaeoptera     
##  [645] Hymenoptera      Hymenoptera      Condylognatha    Palaeoptera     
##  [649] Condylognatha    Hymenoptera      Hymenoptera      Hymenoptera     
##  [653] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
##  [657] Polyneoptera     Hymenoptera      Polyneoptera     Hymenoptera     
##  [661] Antliophora      Hymenoptera      Hymenoptera      Antliophora     
##  [665] Hymenoptera      Polyneoptera     Hymenoptera      Hymenoptera     
##  [669] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
##  [673] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
##  [677] Hymenoptera      Antliophora      Palaeoptera      Antliophora     
##  [681] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
##  [685] Antliophora      Hymenoptera      Polyneoptera     Antliophora     
##  [689] Condylognatha    Hymenoptera      Hymenoptera      Condylognatha   
##  [693] Hymenoptera      Antliophora      Hymenoptera      Hymenoptera     
##  [697] Hymenoptera      Antliophora      Hymenoptera      Hymenoptera     
##  [701] Hymenoptera      Condylognatha    Hymenoptera      Antliophora     
##  [705] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
##  [709] Hymenoptera      Hymenoptera      Hymenoptera      Condylognatha   
##  [713] Neuropteroidea   Hymenoptera      Antliophora      Condylognatha   
##  [717] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
##  [721] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
##  [725] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
##  [729] Antliophora      Neuropteroidea   Palaeoptera      Condylognatha   
##  [733] Hymenoptera      Condylognatha    Hymenoptera      Polyneoptera    
##  [737] Polyneoptera     Hymenoptera      Polyneoptera     Polyneoptera    
##  [741] Polyneoptera     Hymenoptera      Hymenoptera      Polyneoptera    
##  [745] Hymenoptera      Hymenoptera      Condylognatha    Hymenoptera     
##  [749] Condylognatha    Condylognatha    Condylognatha    Condylognatha   
##  [753] Condylognatha    Antliophora      Hymenoptera      Hymenoptera     
##  [757] Hymenoptera      Polyneoptera     Polyneoptera     Polyneoptera    
##  [761] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
##  [765] Condylognatha    Antliophora      Hymenoptera      Hymenoptera     
##  [769] Polyneoptera     Hymenoptera      Hymenoptera      Hymenoptera     
##  [773] Hymenoptera      Hymenoptera      Polyneoptera     Hymenoptera     
##  [777] Polyneoptera     Polyneoptera     Antliophora      Condylognatha   
##  [781] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [785] Amphiesmenoptera Condylognatha    Amphiesmenoptera Amphiesmenoptera
##  [789] Condylognatha    Amphiesmenoptera Amphiesmenoptera Condylognatha   
##  [793] Amphiesmenoptera Amphiesmenoptera Condylognatha    Amphiesmenoptera
##  [797] Neuropteroidea   Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [801] Palaeoptera      Condylognatha    Amphiesmenoptera Condylognatha   
##  [805] Amphiesmenoptera Amphiesmenoptera Condylognatha    Amphiesmenoptera
##  [809] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [813] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [817] Amphiesmenoptera Neuropteroidea   Condylognatha    Neuropteroidea  
##  [821] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [825] Neuropteroidea   Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [829] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [833] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [837] Neuropteroidea   Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [841] Polyneoptera     Condylognatha    Amphiesmenoptera Polyneoptera    
##  [845] Condylognatha    Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [849] Amphiesmenoptera Neuropteroidea   Polyneoptera     Amphiesmenoptera
##  [853] Amphiesmenoptera Condylognatha    Amphiesmenoptera Amphiesmenoptera
##  [857] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [861] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
##  [865] Amphiesmenoptera Amphiesmenoptera Condylognatha    Neuropteroidea  
##  [869] Neuropteroidea   Palaeoptera      Neuropteroidea   Polyneoptera    
##  [873] Amphiesmenoptera Polyneoptera     Hymenoptera      Polyneoptera    
##  [877] Neuropteroidea   Polyneoptera     Condylognatha    Neuropteroidea  
##  [881] Palaeoptera      Condylognatha    Amphiesmenoptera Amphiesmenoptera
##  [885] Condylognatha    Condylognatha    Condylognatha    Polyneoptera    
##  [889] Hymenoptera      Condylognatha    Hymenoptera      Condylognatha   
##  [893] Hymenoptera      Polyneoptera     Polyneoptera     Antliophora     
##  [897] Hymenoptera      Polyneoptera     Polyneoptera     Polyneoptera    
##  [901] Amphiesmenoptera Condylognatha    Neuropteroidea   Amphiesmenoptera
##  [905] Condylognatha    Neuropteroidea   Neuropteroidea   Neuropteroidea  
##  [909] Neuropteroidea   Neuropteroidea   Neuropteroidea   Condylognatha   
##  [913] Neuropteroidea   Amphiesmenoptera Neuropteroidea   Hymenoptera     
##  [917] Hymenoptera      Hymenoptera      Hymenoptera      Polyneoptera    
##  [921] Condylognatha    Antliophora      Antliophora      Hymenoptera     
##  [925] Palaeoptera      Neuropteroidea   Hymenoptera      Condylognatha   
##  [929] Hymenoptera      Hymenoptera      Hymenoptera      Condylognatha   
##  [933] Hymenoptera      Hymenoptera      Hymenoptera      Antliophora     
##  [937] Hymenoptera      Antliophora      Antliophora      Palaeoptera     
##  [941] Polyneoptera     Hymenoptera      Hymenoptera      Hymenoptera     
##  [945] Hymenoptera      Hymenoptera      Hymenoptera      Neuropteroidea  
##  [949] Hymenoptera      Antliophora      Hymenoptera      Hymenoptera     
##  [953] Antliophora      Hymenoptera      Hymenoptera      Hymenoptera     
##  [957] Hymenoptera      Condylognatha    Neuropteroidea   Hymenoptera     
##  [961] Polyneoptera     Antliophora      Antliophora      Hymenoptera     
##  [965] Condylognatha    Hymenoptera      Hymenoptera      Hymenoptera     
##  [969] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
##  [973] Hymenoptera      Hymenoptera      Polyneoptera     Condylognatha   
##  [977] Hymenoptera      Palaeoptera      Hymenoptera      Hymenoptera     
##  [981] Hymenoptera      Condylognatha    Antliophora      Hymenoptera     
##  [985] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
##  [989] Hymenoptera      Hymenoptera      Polyneoptera     Neuropteroidea  
##  [993] Hymenoptera      Hymenoptera      Antliophora      Hymenoptera     
##  [997] Amphiesmenoptera Antliophora      Polyneoptera     Condylognatha   
## [1001] Antliophora      Polyneoptera     Polyneoptera     Polyneoptera    
## [1005] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1009] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1013] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1017] Polyneoptera     Polyneoptera     Neuropteroidea   Condylognatha   
## [1021] Polyneoptera     Polyneoptera     Amphiesmenoptera Amphiesmenoptera
## [1025] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1029] Polyneoptera     Hymenoptera      Hymenoptera      Neuropteroidea  
## [1033] Polyneoptera     Hymenoptera      Hymenoptera      Polyneoptera    
## [1037] Polyneoptera     Polyneoptera     Hymenoptera      Hymenoptera     
## [1041] Condylognatha    Hymenoptera      Polyneoptera     Antliophora     
## [1045] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1049] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1053] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1057] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1061] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1065] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1069] Polyneoptera     Polyneoptera     Polyneoptera     Neuropteroidea  
## [1073] Polyneoptera     Polyneoptera     Hymenoptera      Hymenoptera     
## [1077] Hymenoptera      Hymenoptera      Hymenoptera      Condylognatha   
## [1081] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
## [1085] Hymenoptera      Hymenoptera      Hymenoptera      Antliophora     
## [1089] Hymenoptera      Antliophora      Polyneoptera     Antliophora     
## [1093] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
## [1097] Hymenoptera      Polyneoptera     Polyneoptera     Polyneoptera    
## [1101] Amphiesmenoptera Hymenoptera      Palaeoptera      Antliophora     
## [1105] Amphiesmenoptera Antliophora      Condylognatha    Condylognatha   
## [1109] Hymenoptera      Condylognatha    Condylognatha    Neuropteroidea  
## [1113] Condylognatha    Amphiesmenoptera Hymenoptera      Hymenoptera     
## [1117] Hymenoptera      Polyneoptera     Polyneoptera     Polyneoptera    
## [1121] Polyneoptera     Hymenoptera      Condylognatha    Antliophora     
## [1125] Condylognatha    Hymenoptera      Condylognatha    Condylognatha   
## [1129] Polyneoptera     Condylognatha    Condylognatha    Neuropteroidea  
## [1133] Amphiesmenoptera Palaeoptera      Hymenoptera      Hymenoptera     
## [1137] Hymenoptera      Hymenoptera      Condylognatha    Polyneoptera    
## [1141] Neuropteroidea   Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1145] Amphiesmenoptera Polyneoptera     Amphiesmenoptera Amphiesmenoptera
## [1149] Antliophora      Amphiesmenoptera Amphiesmenoptera Polyneoptera    
## [1153] Neuropteroidea   Amphiesmenoptera Neuropteroidea   Polyneoptera    
## [1157] Amphiesmenoptera Hymenoptera      Palaeoptera      Polyneoptera    
## [1161] Antliophora      Antliophora      Antliophora      Antliophora     
## [1165] Antliophora      Condylognatha    Antliophora      Polyneoptera    
## [1169] Hymenoptera      Neuropteroidea   Condylognatha    Condylognatha   
## [1173] Condylognatha    Condylognatha    Neuropteroidea   Neuropteroidea  
## [1177] Neuropteroidea   Condylognatha    Condylognatha    Amphiesmenoptera
## [1181] Antliophora      Antliophora      Neuropteroidea   Condylognatha   
## [1185] Neuropteroidea   Antliophora      Hymenoptera      Hymenoptera     
## [1189] Hymenoptera      Condylognatha    Polyneoptera     Condylognatha   
## [1193] Neuropteroidea   Polyneoptera     Condylognatha    Amphiesmenoptera
## [1197] Hymenoptera      Hymenoptera      Amphiesmenoptera Polyneoptera    
## [1201] Condylognatha    Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1205] Amphiesmenoptera Amphiesmenoptera Condylognatha    Amphiesmenoptera
## [1209] Amphiesmenoptera Amphiesmenoptera Polyneoptera     Palaeoptera     
## [1213] Palaeoptera      Apterygota       Polyneoptera     Polyneoptera    
## [1217] Palaeoptera      Antliophora      Palaeoptera      Antliophora     
## [1221] Neuropteroidea   Neuropteroidea   Condylognatha    Hymenoptera     
## [1225] Hymenoptera      Antliophora      Condylognatha    Hymenoptera     
## [1229] Amphiesmenoptera Amphiesmenoptera Polyneoptera     Amphiesmenoptera
## [1233] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1237] Neuropteroidea   Hymenoptera      Neuropteroidea   Amphiesmenoptera
## [1241] Condylognatha    Polyneoptera     Polyneoptera     Neuropteroidea  
## [1245] Condylognatha    Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1249] Amphiesmenoptera Condylognatha    Amphiesmenoptera Hymenoptera     
## [1253] Condylognatha    Condylognatha    Condylognatha    Condylognatha   
## [1257] Amphiesmenoptera Condylognatha    Condylognatha    Amphiesmenoptera
## [1261] Amphiesmenoptera Condylognatha    Palaeoptera      Condylognatha   
## [1265] Condylognatha    Condylognatha    Amphiesmenoptera Amphiesmenoptera
## [1269] Amphiesmenoptera Hymenoptera      Neuropteroidea   Polyneoptera    
## [1273] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1277] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1281] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1285] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1289] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1293] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1297] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1301] Polyneoptera     Polyneoptera     Neuropteroidea   Polyneoptera    
## [1305] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1309] Polyneoptera     Polyneoptera     Amphiesmenoptera Neuropteroidea  
## [1313] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1317] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1321] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1325] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1329] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1333] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1337] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1341] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1345] Polyneoptera     Polyneoptera     Neuropteroidea   Neuropteroidea  
## [1349] Neuropteroidea   Condylognatha    Neuropteroidea   Antliophora     
## [1353] Neuropteroidea   Condylognatha    Hymenoptera      Hymenoptera     
## [1357] Antliophora      Amphiesmenoptera Neuropteroidea   Neuropteroidea  
## [1361] Condylognatha    Hymenoptera      Hymenoptera      Hymenoptera     
## [1365] Hymenoptera      Antliophora      Hymenoptera      Hymenoptera     
## [1369] Antliophora      Hymenoptera      Hymenoptera      Condylognatha   
## [1373] Neuropteroidea   Condylognatha    Polyneoptera     Neuropteroidea  
## [1377] Antliophora      Amphiesmenoptera Polyneoptera     Condylognatha   
## [1381] Hymenoptera      Antliophora      Hymenoptera      Antliophora     
## [1385] Hymenoptera      Polyneoptera     Hymenoptera      Hymenoptera     
## [1389] Hymenoptera      Antliophora      Condylognatha    Hymenoptera     
## [1393] Neuropteroidea   Condylognatha    Hymenoptera      Hymenoptera     
## [1397] Polyneoptera     Condylognatha    Antliophora      Hymenoptera     
## [1401] Antliophora      Hymenoptera      Palaeoptera      Hymenoptera     
## [1405] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
## [1409] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
## [1413] Hymenoptera      Hymenoptera      Antliophora      Palaeoptera     
## [1417] Hymenoptera      Hymenoptera      Hymenoptera      Condylognatha   
## [1421] Condylognatha    Condylognatha    Hymenoptera      Hymenoptera     
## [1425] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
## [1429] Antliophora      Hymenoptera      Hymenoptera      Hymenoptera     
## [1433] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
## [1437] Hymenoptera      Antliophora      Condylognatha    Hymenoptera     
## [1441] Hymenoptera      Antliophora      Hymenoptera      Neuropteroidea  
## [1445] Hymenoptera      Hymenoptera      Amphiesmenoptera Amphiesmenoptera
## [1449] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1453] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1457] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1461] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1465] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1469] Amphiesmenoptera Antliophora      Amphiesmenoptera Hymenoptera     
## [1473] Hymenoptera      Palaeoptera      Palaeoptera      Hymenoptera     
## [1477] Hymenoptera      Hymenoptera      Palaeoptera      Palaeoptera     
## [1481] Antliophora      Hymenoptera      Condylognatha    Neuropteroidea  
## [1485] Hymenoptera      Antliophora      Antliophora      Antliophora     
## [1489] Antliophora      Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1493] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Polyneoptera    
## [1497] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1501] Hymenoptera      Polyneoptera     Polyneoptera     Neuropteroidea  
## [1505] Condylognatha    Neuropteroidea   Neuropteroidea   Antliophora     
## [1509] Hymenoptera      Neuropteroidea   Condylognatha    Antliophora     
## [1513] Antliophora      Neuropteroidea   Antliophora      Neuropteroidea  
## [1517] Amphiesmenoptera Condylognatha    Neuropteroidea   Hymenoptera     
## [1521] Hymenoptera      Neuropteroidea   Condylognatha    Neuropteroidea  
## [1525] Condylognatha    Hymenoptera      Amphiesmenoptera Hymenoptera     
## [1529] Polyneoptera     Hymenoptera      Neuropteroidea   Amphiesmenoptera
## [1533] Neuropteroidea   Condylognatha    Hymenoptera      Hymenoptera     
## [1537] Antliophora      Antliophora      Condylognatha    Antliophora     
## [1541] Hymenoptera      Hymenoptera      Antliophora      Hymenoptera     
## [1545] Neuropteroidea   Antliophora      Hymenoptera      Polyneoptera    
## [1549] Antliophora      Condylognatha    Polyneoptera     Amphiesmenoptera
## [1553] Neuropteroidea   Condylognatha    Hymenoptera      Polyneoptera    
## [1557] Amphiesmenoptera Amphiesmenoptera Condylognatha    Polyneoptera    
## [1561] Amphiesmenoptera Antliophora      Condylognatha    Amphiesmenoptera
## [1565] Antliophora      Polyneoptera     Palaeoptera      Hymenoptera     
## [1569] Condylognatha    Antliophora      Condylognatha    Condylognatha   
## [1573] Polyneoptera     Polyneoptera     Hymenoptera      Antliophora     
## [1577] Hymenoptera      Antliophora      Palaeoptera      Condylognatha   
## [1581] Neuropteroidea   Amphiesmenoptera Antliophora      Hymenoptera     
## [1585] Antliophora      Antliophora      Hymenoptera      Antliophora     
## [1589] Antliophora      Antliophora      Antliophora      Antliophora     
## [1593] Hymenoptera      Hymenoptera      Condylognatha    Condylognatha   
## [1597] Antliophora      Antliophora      Hymenoptera      Antliophora     
## [1601] Hymenoptera      Amphiesmenoptera Amphiesmenoptera Palaeoptera     
## [1605] Condylognatha    Antliophora      Antliophora      Antliophora     
## [1609] Condylognatha    Hymenoptera      Antliophora      Hymenoptera     
## [1613] Antliophora      Antliophora      Antliophora      Antliophora     
## [1617] Hymenoptera      Antliophora      Hymenoptera      Hymenoptera     
## [1621] Antliophora      Antliophora      Hymenoptera      Condylognatha   
## [1625] Hymenoptera      Antliophora      Antliophora      Condylognatha   
## [1629] Antliophora      Neuropteroidea   Antliophora      Hymenoptera     
## [1633] Antliophora      Condylognatha    Hymenoptera      Condylognatha   
## [1637] Condylognatha    Neuropteroidea   Condylognatha    Condylognatha   
## [1641] Neuropteroidea   Neuropteroidea   Condylognatha    Amphiesmenoptera
## [1645] Amphiesmenoptera Amphiesmenoptera Neuropteroidea   Condylognatha   
## [1649] Antliophora      Hymenoptera      Condylognatha    Neuropteroidea  
## [1653] Condylognatha    Condylognatha    Condylognatha    Neuropteroidea  
## [1657] Neuropteroidea   Neuropteroidea   Amphiesmenoptera Neuropteroidea  
## [1661] Condylognatha    Antliophora      Neuropteroidea   Neuropteroidea  
## [1665] Neuropteroidea   Polyneoptera     Neuropteroidea   Hymenoptera     
## [1669] Antliophora      Antliophora      Antliophora      Condylognatha   
## [1673] Condylognatha    Neuropteroidea   Neuropteroidea   Condylognatha   
## [1677] Condylognatha    Amphiesmenoptera Hymenoptera      Condylognatha   
## [1681] Condylognatha    Amphiesmenoptera Amphiesmenoptera Neuropteroidea  
## [1685] Amphiesmenoptera Polyneoptera     Hymenoptera      Antliophora     
## [1689] Hymenoptera      Polyneoptera     Polyneoptera     Neuropteroidea  
## [1693] Neuropteroidea   Condylognatha    Neuropteroidea   Polyneoptera    
## [1697] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Neuropteroidea  
## [1701] Condylognatha    Neuropteroidea   Neuropteroidea   Amphiesmenoptera
## [1705] Amphiesmenoptera Polyneoptera     Condylognatha    Neuropteroidea  
## [1709] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Hymenoptera     
## [1713] Neuropteroidea   Hymenoptera      Neuropteroidea   Hymenoptera     
## [1717] Condylognatha    Neuropteroidea   Polyneoptera     Condylognatha   
## [1721] Condylognatha    Neuropteroidea   Neuropteroidea   Neuropteroidea  
## [1725] Neuropteroidea   Neuropteroidea   Neuropteroidea   Antliophora     
## [1729] Neuropteroidea   Hymenoptera      Condylognatha    Condylognatha   
## [1733] Hymenoptera      Neuropteroidea   Polyneoptera     Polyneoptera    
## [1737] Condylognatha    Hymenoptera      Polyneoptera     Antliophora     
## [1741] Antliophora      Neuropteroidea   Polyneoptera     Condylognatha   
## [1745] Hymenoptera      Condylognatha    Antliophora      Hymenoptera     
## [1749] Amphiesmenoptera Neuropteroidea   Amphiesmenoptera Amphiesmenoptera
## [1753] Hymenoptera      Hymenoptera      Antliophora      Antliophora     
## [1757] Hymenoptera      Condylognatha    Antliophora      Hymenoptera     
## [1761] Amphiesmenoptera Amphiesmenoptera Antliophora      Neuropteroidea  
## [1765] Neuropteroidea   Palaeoptera      Condylognatha    Neuropteroidea  
## [1769] Amphiesmenoptera Polyneoptera     Condylognatha    Polyneoptera    
## [1773] Amphiesmenoptera Antliophora      Antliophora      Condylognatha   
## [1777] Amphiesmenoptera Amphiesmenoptera Condylognatha    Amphiesmenoptera
## [1781] Condylognatha    Amphiesmenoptera Neuropteroidea   Polyneoptera    
## [1785] Amphiesmenoptera Amphiesmenoptera Hymenoptera      Hymenoptera     
## [1789] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1793] Amphiesmenoptera Polyneoptera     Amphiesmenoptera Amphiesmenoptera
## [1797] Condylognatha    Polyneoptera     Neuropteroidea   Condylognatha   
## [1801] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1805] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1809] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1813] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1817] Amphiesmenoptera Amphiesmenoptera Condylognatha    Amphiesmenoptera
## [1821] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1825] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1829] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1833] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1837] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1841] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1845] Amphiesmenoptera Amphiesmenoptera Condylognatha    Amphiesmenoptera
## [1849] Amphiesmenoptera Amphiesmenoptera Neuropteroidea   Amphiesmenoptera
## [1853] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1857] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1861] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1865] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1869] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [1873] Amphiesmenoptera Amphiesmenoptera Condylognatha    Condylognatha   
## [1877] Condylognatha    Antliophora      Antliophora      Polyneoptera    
## [1881] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [1885] Polyneoptera     Hymenoptera      Polyneoptera     Polyneoptera    
## [1889] Polyneoptera     Antliophora      Condylognatha    Neuropteroidea  
## [1893] Neuropteroidea   Amphiesmenoptera Neuropteroidea   Amphiesmenoptera
## [1897] Amphiesmenoptera Condylognatha    Condylognatha    Antliophora     
## [1901] Condylognatha    Amphiesmenoptera Neuropteroidea   Polyneoptera    
## [1905] Amphiesmenoptera Condylognatha    Condylognatha    Polyneoptera    
## [1909] Hymenoptera      Hymenoptera      Neuropteroidea   Hymenoptera     
## [1913] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
## [1917] Palaeoptera      Palaeoptera      Hymenoptera      Condylognatha   
## [1921] Condylognatha    Amphiesmenoptera Hymenoptera      Condylognatha   
## [1925] Condylognatha    Condylognatha    Neuropteroidea   Hymenoptera     
## [1929] Neuropteroidea   Condylognatha    Condylognatha    Neuropteroidea  
## [1933] Antliophora      Condylognatha    Condylognatha    Polyneoptera    
## [1937] Amphiesmenoptera Amphiesmenoptera Hymenoptera      Amphiesmenoptera
## [1941] Amphiesmenoptera Amphiesmenoptera Condylognatha    Condylognatha   
## [1945] Amphiesmenoptera Condylognatha    Neuropteroidea   Condylognatha   
## [1949] Hymenoptera      Amphiesmenoptera Hymenoptera      Condylognatha   
## [1953] Antliophora      Hymenoptera      Condylognatha    Hymenoptera     
## [1957] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
## [1961] Hymenoptera      Hymenoptera      Antliophora      Hymenoptera     
## [1965] Hymenoptera      Antliophora      Antliophora      Hymenoptera     
## [1969] Condylognatha    Hymenoptera      Hymenoptera      Hymenoptera     
## [1973] Hymenoptera      Antliophora      Antliophora      Hymenoptera     
## [1977] Hymenoptera      Hymenoptera      Antliophora      Hymenoptera     
## [1981] Hymenoptera      Hymenoptera      Hymenoptera      Condylognatha   
## [1985] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
## [1989] Condylognatha    Antliophora      Hymenoptera      Hymenoptera     
## [1993] Palaeoptera      Palaeoptera      Amphiesmenoptera Hymenoptera     
## [1997] Hymenoptera      Hymenoptera      Polyneoptera     Antliophora     
## [2001] Neuropteroidea   Amphiesmenoptera Hymenoptera      Polyneoptera    
## [2005] Polyneoptera     Neuropteroidea   Neuropteroidea   Amphiesmenoptera
## [2009] Hymenoptera      Hymenoptera      Hymenoptera      Hymenoptera     
## [2013] Hymenoptera      Antliophora      Hymenoptera      Hymenoptera     
## [2017] Hymenoptera      Condylognatha    Hymenoptera      Hymenoptera     
## [2021] Amphiesmenoptera Hymenoptera      Antliophora      Hymenoptera     
## [2025] Hymenoptera      Neuropteroidea   Antliophora      Neuropteroidea  
## [2029] Condylognatha    Condylognatha    Hymenoptera      Hymenoptera     
## [2033] Neuropteroidea   Hymenoptera      Hymenoptera      Antliophora     
## [2037] Polyneoptera     Polyneoptera     Hymenoptera      Hymenoptera     
## [2041] Hymenoptera      Hymenoptera      Hymenoptera      Polyneoptera    
## [2045] Amphiesmenoptera Polyneoptera     Hymenoptera      Neuropteroidea  
## [2049] Neuropteroidea   Condylognatha    Amphiesmenoptera Condylognatha   
## [2053] Condylognatha    Condylognatha    Hymenoptera      Polyneoptera    
## [2057] Hymenoptera      Hymenoptera      Hymenoptera      Amphiesmenoptera
## [2061] Amphiesmenoptera Hymenoptera      Polyneoptera     Polyneoptera    
## [2065] Polyneoptera     Hymenoptera      Hymenoptera      Hymenoptera     
## [2069] Condylognatha    Hymenoptera      Antliophora      Hymenoptera     
## [2073] Hymenoptera      Polyneoptera     Antliophora      Condylognatha   
## [2077] Hymenoptera      Hymenoptera      Hymenoptera      Antliophora     
## [2081] Hymenoptera      Hymenoptera      Hymenoptera      Polyneoptera    
## [2085] Amphiesmenoptera Hymenoptera      Neuropteroidea   Amphiesmenoptera
## [2089] Condylognatha    Amphiesmenoptera Neuropteroidea   Neuropteroidea  
## [2093] Neuropteroidea   Condylognatha    Hymenoptera      Polyneoptera    
## [2097] Condylognatha    Polyneoptera     Hymenoptera      Amphiesmenoptera
## [2101] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [2105] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Neuropteroidea  
## [2109] Amphiesmenoptera Antliophora      Hymenoptera      Amphiesmenoptera
## [2113] Antliophora      Amphiesmenoptera Condylognatha    Amphiesmenoptera
## [2117] Polyneoptera     Polyneoptera     Polyneoptera     Polyneoptera    
## [2121] Polyneoptera     Polyneoptera     Amphiesmenoptera Amphiesmenoptera
## [2125] Polyneoptera     Hymenoptera      Amphiesmenoptera Hymenoptera     
## [2129] Amphiesmenoptera Amphiesmenoptera Neuropteroidea   Condylognatha   
## [2133] Neuropteroidea   Hymenoptera      Condylognatha    Condylognatha   
## [2137] Condylognatha    Neuropteroidea   Hymenoptera      Condylognatha   
## [2141] Condylognatha    Hymenoptera      Hymenoptera      Hymenoptera     
## [2145] Hymenoptera      Hymenoptera      Polyneoptera     Polyneoptera    
## [2149] Neuropteroidea   Hymenoptera      Hymenoptera      Hymenoptera     
## [2153] Hymenoptera      Hymenoptera      Amphiesmenoptera Amphiesmenoptera
## [2157] Amphiesmenoptera Condylognatha    Polyneoptera     Hymenoptera     
## [2161] Polyneoptera     Amphiesmenoptera Neuropteroidea   Hymenoptera     
## [2165] Palaeoptera      Hymenoptera      Hymenoptera      Antliophora     
## [2169] Condylognatha    Neuropteroidea   Amphiesmenoptera Amphiesmenoptera
## [2173] Hymenoptera      Polyneoptera     Amphiesmenoptera Amphiesmenoptera
## [2177] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [2181] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [2185] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [2189] Amphiesmenoptera Amphiesmenoptera Hymenoptera      Condylognatha   
## [2193] Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [2197] Palaeoptera      Amphiesmenoptera Amphiesmenoptera Amphiesmenoptera
## [2201] Antliophora      Condylognatha    Amphiesmenoptera Antliophora     
## [2205] Antliophora      Hymenoptera      Hymenoptera      Polyneoptera    
## [2209] Polyneoptera     Antliophora      Antliophora      Amphiesmenoptera
## [2213] Antliophora      Antliophora      Hymenoptera     
## 9 Levels: Amphiesmenoptera Antliophora Apterygota ... Psocodea
```
#Confusion matrix from model K=8

```r
confusionMatrix(prediction2,tableCompare)
```

```
## Warning in confusionMatrix.default(prediction2, tableCompare): Levels are
## not in the same order for reference and data. Refactoring data to match.
```

```
## Confusion Matrix and Statistics
## 
##                   Reference
## Prediction         Amphiesmenoptera Polyneoptera Antliophora
##   Amphiesmenoptera              361           15          12
##   Polyneoptera                   18          295           3
##   Antliophora                     1            3         111
##   Neuropteroidea                 27           13          10
##   Psocodea                        0            0           1
##   Condylognatha                  43            8          35
##   Palaeoptera                     1            8          11
##   Hymenoptera                     9           23          66
##   Apterygota                      0            1           0
##                   Reference
## Prediction         Neuropteroidea Psocodea Condylognatha Palaeoptera
##   Amphiesmenoptera             28        0            55          12
##   Polyneoptera                 36        0            12          11
##   Antliophora                  13        0            35           6
##   Neuropteroidea               64        3            59           2
##   Psocodea                      0        0             0           0
##   Condylognatha                52        5           120           1
##   Palaeoptera                   3        0             3          41
##   Hymenoptera                  31        4            60          15
##   Apterygota                    0        0             0           0
##                   Reference
## Prediction         Hymenoptera Apterygota
##   Amphiesmenoptera           4          1
##   Polyneoptera              36          1
##   Antliophora               61          0
##   Neuropteroidea            13          0
##   Psocodea                   0          0
##   Condylognatha             46          2
##   Palaeoptera               12          1
##   Hymenoptera              292          0
##   Apterygota                 0          0
## 
## Overall Statistics
##                                           
##                Accuracy : 0.5797          
##                  95% CI : (0.5588, 0.6003)
##     No Information Rate : 0.2095          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.4951          
##                                           
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: Amphiesmenoptera Class: Polyneoptera
## Sensitivity                           0.7848              0.8060
## Specificity                           0.9276              0.9367
## Pos Pred Value                        0.7398              0.7160
## Neg Pred Value                        0.9427              0.9606
## Prevalence                            0.2077              0.1652
## Detection Rate                        0.1630              0.1332
## Detection Prevalence                  0.2203              0.1860
## Balanced Accuracy                     0.8562              0.8714
##                      Class: Antliophora Class: Neuropteroidea
## Sensitivity                     0.44578               0.28194
## Specificity                     0.93947               0.93612
## Pos Pred Value                  0.48261               0.33508
## Neg Pred Value                  0.93048               0.91947
## Prevalence                      0.11242               0.10248
## Detection Rate                  0.05011               0.02889
## Detection Prevalence            0.10384               0.08623
## Balanced Accuracy               0.69263               0.60903
##                      Class: Psocodea Class: Condylognatha
## Sensitivity                0.0000000              0.34884
## Specificity                0.9995461              0.89738
## Pos Pred Value             0.0000000              0.38462
## Neg Pred Value             0.9945799              0.88229
## Prevalence                 0.0054176              0.15530
## Detection Rate             0.0000000              0.05418
## Detection Prevalence       0.0004515              0.14086
## Balanced Accuracy          0.4997730              0.62311
##                      Class: Palaeoptera Class: Hymenoptera
## Sensitivity                     0.46591             0.6293
## Specificity                     0.98166             0.8812
## Pos Pred Value                  0.51250             0.5840
## Neg Pred Value                  0.97799             0.8997
## Prevalence                      0.03973             0.2095
## Detection Rate                  0.01851             0.1318
## Detection Prevalence            0.03612             0.2257
## Balanced Accuracy               0.72379             0.7553
##                      Class: Apterygota
## Sensitivity                  0.0000000
## Specificity                  0.9995475
## Pos Pred Value               0.0000000
## Neg Pred Value               0.9977416
## Prevalence                   0.0022573
## Detection Rate               0.0000000
## Detection Prevalence         0.0004515
## Balanced Accuracy            0.4997738
```

