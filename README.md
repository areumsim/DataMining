# DataMining

---
title: "Templete"

author: by Stoney

date: '`r paste0(Sys.Date())`' # current time

output:

  html_document:
  
    df_print: paged # data frame printing (default, kable, tibble, paged)
    theme: paper # from Bootswatch library
    toc: TRUE # table of contents
    toc_float: TRUE
    toc_depth: 3 # depth of headers
---




<style type="text/css">
.main-container {
  max-width: 1000px;
  margin-left: 80px;
  margin-right: auto;
}
</style>


<style type="text/css">
body { /* Normal  */
  font-size: 18px;
}
td {  /* Table  */
  font-size: 16px;
}
h1.title {
  font-size: 46px;
  color: DarkRed;
}
h1 { /* Header 1 */
  font-size: 36px;
  color: DarkBlue;
}
h2 { /* Header 2 */
  font-size: 30px;
  color: DarkBlue;
}
<!-- h3 { /* Header 3 */ -->
<!--   font-size: 26px; -->
<!--   font-family: "Times New Roman", Times, serif; -->
<!--   color: DarkBlue; -->
<!-- } -->

code.r{ /* Code block */
  font-size: 16px;
}
pre { /* Code block - determines code spacing between lines */
  font-size: 16px;
}
</style>



<div style="margin-bottom:50px;">


```{r setup, echo = FALSE}
knitr::opts_chunk$set(eval = TRUE, # running the code in chunk
                      echo = TRUE, # showing R code
                      include =TRUE, # showing the result of R code
                      cache = TRUE)  #  knitr will reuse the results
```




<!---------------------------------->


<br>
<br>

<font size = 5> Title: Supervised Fuzzy Partitioning <br> 
Authors: Ashtari et al.  <br>
Year: 2020  <br>
Journal: Pattern Recognition </font>
</div>

# Topic
+ Clustering
  - high-dimensional
  
1. aaa
2. bbb

<br>

# Motivation
+ aaa

<br>

# Proposed method
+ aaa

<br>

# Experiments
+ aaa

<br>

# References
+ aaa

<br>

> 링크 넣기 예시

[SFP paper](https://www.sciencedirect.com/science/article/pii/S0031320319303164)

<br>

> 수식 예시

$$\sum_{n=1}^{10} \sum_{k=1}^{5} n \times k $$

<br>
