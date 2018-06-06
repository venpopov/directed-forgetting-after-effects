Contains the experimental software, data and analysis scripts for the results reported in:

Popov, V., Marevic, I., Rummel, J. & Reder, L. (2018). Forgetting is a Feature, not a Bug: Intentionally Forgetting Some Things Helps Us Remember Others by Freeing up Working Memory Resources. *PsyArXiv*; https://dx.doi.org/10.17605/OSF.IO/YSJZU

The companion OSF project is available at: https://osf.io/5qd94/files/

The code runs with paths relative to the parent folder, which is set-up in every script by the here() package and the setwd(here()) command, which finds the .Rproj file and sets that as the working dir.

All code runs under the following R, OS and packages versions:

```
R version 3.4.0 (2017-04-21)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows >= 8 x64 (build 9200)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252 LC_NUMERIC=C                           LC_TIME=English_United States.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] lme4_1.1-13     Matrix_1.2-9    bindrcpp_0.2    cowplot_0.9.2   brms_2.1.0      Rcpp_0.12.11    here_0.1        dplyr_0.7.1     purrr_0.2.2.2   readr_1.1.1     tidyr_0.6.3     tibble_1.3.3   
[13] ggplot2_2.2.1   tidyverse_1.1.1
```