# Species’ traits drive amphibian tolerance to anthropogenic habitat modification

This repository contains data and code to reproduce Liu et al. (2021) Species’ traits drive amphibian tolerance to anthropogenic habitat modification. Global Change Biology. The complete raw dataset is not fully available due to sensitivities in relation to locations of rare or threatened species and citizen scientist information, but the FrogID data, with sensitive species’ localities removed or buffered, are made available annually (Rowley & Callaghan, 2020; https://doi.org/10.3897/zookeys.912.38253). Here, we provide processed species’ anthropogenic modification indices and code to reproduce the main figures and analyses in the manuscript. The following R information was used at the time of analysis:

sessionInfo()

R version 3.6.2 (2019-12-12)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 18363)

Matrix products: default

locale:
[1] LC_COLLATE=English_Australia.1252  LC_CTYPE=English_Australia.1252   
[3] LC_MONETARY=English_Australia.1252 LC_NUMERIC=C                      
[5] LC_TIME=English_Australia.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] car_3.0-6       carData_3.0-2   phylolm_2.6     MuMIn_1.43.15   broom_0.5.2     ggtree_2.0.1   
 [7] phylosignal_1.3 phylobase_0.8.6 ape_5.3         corrplot_0.84   lme4_1.1-21     Matrix_1.2-18  
[13] MASS_7.3-51.6   cowplot_1.0.0   forcats_0.4.0   stringr_1.4.0   dplyr_0.8.4     purrr_0.3.3    
[19] readr_1.3.1     tidyr_1.0.2     tibble_2.1.3    ggplot2_3.2.1   tidyverse_1.3.0

loaded via a namespace (and not attached):
  [1] minqa_1.2.4         colorspace_1.4-1    seqinr_3.6-1        deldir_0.1-23      
  [5] rio_0.5.16          ellipsis_0.3.0      class_7.3-15        fs_1.3.1           
  [9] rstudioapi_0.10     listenv_0.8.0       farver_2.0.3        fansi_0.4.1        
 [13] lubridate_1.7.4     xml2_1.2.2          codetools_0.2-16    splines_3.6.2      
 [17] ade4_1.7-13         jsonlite_1.6        nloptr_1.2.1        cluster_2.1.0      
 [21] dbplyr_1.4.2        shiny_1.4.0         BiocManager_1.30.10 compiler_3.6.2     
 [25] httr_1.4.1          rvcheck_0.1.7       backports_1.1.5     assertthat_0.2.1   
 [29] fastmap_1.0.1       lazyeval_0.2.2      cli_2.0.1           later_1.0.0        
 [33] htmltools_0.4.0     prettyunits_1.1.0   tools_3.6.2         igraph_1.2.4.2     
 [37] coda_0.19-3         gtable_0.3.0        glue_1.3.1          reshape2_1.4.3     
 [41] gmodels_2.18.1      Rcpp_1.0.3          cellranger_1.1.0    vctrs_0.3.2        
 [45] spdep_1.1-3         gdata_2.18.0        nlme_3.1-142        globals_0.12.5     
 [49] adephylo_1.1-11     openxlsx_4.1.4      rvest_0.3.5         mime_0.8           
 [53] lifecycle_0.1.0     gtools_3.8.1        XML_3.99-0.3        future_1.16.0      
 [57] LearnBayes_2.15.1   scales_1.1.0        hms_0.5.3           promises_1.1.0     
 [61] parallel_3.6.2      expm_0.999-4        curl_4.3            stringi_1.4.5      
 [65] tidytree_0.3.1      e1071_1.7-3         permute_0.9-5       zip_2.0.4          
 [69] boot_1.3-23         spData_0.3.2        rlang_0.4.7         pkgconfig_2.0.3    
 [73] rncl_0.8.3          lattice_0.20-38     sf_0.8-0            treeio_1.10.0      
 [77] labeling_0.3        tidyselect_1.1.0    plyr_1.8.5          magrittr_1.5       
 [81] R6_2.4.1            generics_0.0.2      DBI_1.1.0           foreign_0.8-72     
 [85] pillar_1.4.3        haven_2.2.0         withr_2.1.2         mgcv_1.8-31        
 [89] units_0.6-5         abind_1.4-5         sp_1.3-2            future.apply_1.4.0 
 [93] modelr_0.1.5        crayon_1.3.4        uuid_0.1-2          KernSmooth_2.23-16 
 [97] progress_1.2.2      RNeXML_2.4.0        adegenet_2.1.2      grid_3.6.2         
[101] readxl_1.3.1        data.table_1.12.8   vegan_2.5-6         reprex_0.3.0       
[105] digest_0.6.23       classInt_0.4-2      xtable_1.8-4        httpuv_1.5.2       
[109] stats4_3.6.2        munsell_0.5.0       viridisLite_0.3.0   sessioninfo_1.1.1  

