# README #

"[![DOI](https://img.shields.io/badge/doi-10.1093/icesjms/fsab074-blue.svg)](https://doi.org/10.1093/icesjms/fsab074)"

## Author, maintainer and contact

**Alexandre M. S. Machado**: alexandre.marcel@posgrad.ufsc.br       

*Laboratório de Mamíferos Aquáticos, Universidade Federal de Santa Catarina, CCB/ECZ, Florianópolis, SC 88010-970, Brazil.*       
*Department of Collective Behaviour, Max Planck Institute of Animal Behaviour, Konstanz, Germany*


## Description

This repository includes the R scripts to reproduce the analyses and plot the figures of the manuscript:     

Alexandre M S Machado, Eduardo L Hettwer Giehl, Luiza Pacheco Fernandes, Simon N Ingram, Fábio G Daura-Jorge, Alternative data sources can fill the gaps in data-poor fisheries, ICES Journal of Marine Science, Volume 78, Issue 5, August 2021, Pages 1663–1671, https://doi.org/10.1093/icesjms/fsab074     

### Note on figures and supplementary material
Figures were saved as vectorial files and edited in Inkscape for fine-tuning and fixing typos in the 'beach seine community' labels. The supplementary material can be reproduced by knitting the R Markdown file in `supplementary_material/supplementary_material.Rmd`.

## Instructions

Scripts contain relative paths to source functions and load data. Open an R session and set the working directory to the root of the project for better compatibility with relative paths.       

**Setup:** Run the code in `R/setup.R` to install and load the required dependencies, custom functions, color palettes and ggplot themes.      

**Data:** The formatted data set of mullet catch data and sea surface temperature (SST) data are in the files `data/dataMullet.csv` and `data/sst.csv`, respectively. Should you choose to download and prepare the SST data again, use the code in `R/obtain_sst.R`.      

**Analyses:** All the code to reproduce the analyses are in `R/analyses.R`. One can also run the analyses and reproduce the supplementary material at once by knitting the R Markdown file in `supplementary_material/supplementary_material.Rmd`.      

**Figures:** Use the code in `R/figure_correlations.R` to create Figure 2, `R/figure_bsts_cumulative_errors.R` to create Figure 3, and `R/figure_bsts.R` to create Figure 4.      


## System info

R code is available to reproduce the analyses and figures. We used the following software and associated packages:

```
R version 4.0.3 (2020-10-10)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS Big Sur 10.16

Matrix products: default
BLAS:   /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRblas.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib

locale:
[1] pt_BR.UTF-8/pt_BR.UTF-8/pt_BR.UTF-8/C/pt_BR.UTF-8/pt_BR.UTF-8

attached base packages:
[1] grid      stats     graphics  grDevices utils     datasets  methods  
[8] base     

other attached packages:
 [1] ggrepel_0.9.1       see_0.6.0           RColorBrewer_1.1-2 
 [4] raster_3.4-5        ncdf4_1.17          rerddap_0.7.0      
 [7] ggsn_0.5.0          rgdal_1.5-18        sp_1.4-4           
[10] bsts_0.9.5          xts_0.12.1          BoomSpikeSlab_1.2.3
[13] Boom_0.9.6          MASS_7.3-53         zoo_1.8-8          
[16] cowplot_1.1.0       corrplot_0.84       forcats_0.5.0      
[19] stringr_1.4.0       dplyr_1.0.2         purrr_0.3.4        
[22] readr_1.4.0         tidyr_1.1.2         tibble_3.0.6       
[25] ggplot2_3.3.3       tidyverse_1.3.0     kableExtra_1.3.1   

loaded via a namespace (and not attached):
 [1] bitops_1.0-6         fs_1.5.0             sf_0.9-6            
 [4] lubridate_1.7.9.2    insight_0.11.0.1     webshot_0.5.2       
 [7] httr_1.4.2           tools_4.0.3          backports_1.2.0     
[10] R6_2.5.0             KernSmooth_2.23-17   DBI_1.1.0           
[13] colorspace_2.0-0     withr_2.4.1          tidyselect_1.1.0    
[16] curl_4.3             compiler_4.0.3       cli_2.3.0           
[19] rvest_0.3.6          xml2_1.3.2           labeling_0.4.2      
[22] bayestestR_0.7.5.1   bookdown_0.21        scales_1.1.1        
[25] classInt_0.4-3       ggridges_0.5.2       rappdirs_0.3.1      
[28] digest_0.6.27        foreign_0.8-80       rmarkdown_2.5       
[31] jpeg_0.1-8.1         pkgconfig_2.0.3      htmltools_0.5.1.1   
[34] highr_0.8            dbplyr_2.0.0         rlang_0.4.10        
[37] readxl_1.3.1         httpcode_0.3.0       rstudioapi_0.13     
[40] farver_2.0.3         generics_0.1.0       jsonlite_1.7.2      
[43] magrittr_2.0.1       parameters_0.10.0    Rcpp_1.0.6          
[46] munsell_0.5.0        lifecycle_0.2.0      stringi_1.5.3       
[49] yaml_2.2.1           plyr_1.8.6           maptools_1.0-2      
[52] crayon_1.4.1         lattice_0.20-41      haven_2.3.1         
[55] hms_0.5.3            knitr_1.31           pillar_1.4.7        
[58] rjson_0.2.20         effectsize_0.4.0.001 reshape2_1.4.4      
[61] codetools_0.2-16     crul_1.0.0           reprex_0.3.0        
[64] glue_1.4.2           evaluate_0.14        hoardr_0.5.2        
[67] data.table_1.13.2    modelr_0.1.8         png_0.1-7           
[70] vctrs_0.3.6          RgoogleMaps_1.4.5.3  cellranger_1.1.0    
[73] gtable_0.3.0         assertthat_0.2.1     xfun_0.21           
[76] broom_0.7.2          e1071_1.7-4          class_7.3-17        
[79] viridisLite_0.3.0    units_0.6-7          ggmap_3.0.0         
[82] ellipsis_0.3.1      
```
