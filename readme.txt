Replication Files for Yuehong Cassandra Tai, Yue Hu, & Frederick Solt, "Democracy, Public Support, and Measurement Uncertainty", APSR


The `dcpo_demsupport.Rmd` file reproduces all the results in the main text and online supplementary materials.


# Setup

First of all, please set the working directory to where the rmd file is located, e.g., 

```r
setwd(~/THE ACTUAL PATH/dataverse_files)
```
To smoothly compile the file also requires the followng software environment:

- R version >= 4.1.2
- Pandoc 2.16.2
- R packages
    - rlang_1.0.1 # Very important
    - rmarkdown_2.11

Then extract the `renv.zip` in the current directory as a folder of renv/.
Make sure you have the directory structure as below to process the following steps:

    ~/
    |   analysisData.R
    |   apsr.bst
    |   claassen_m5_rep.R
    |   customFunctions.R
    |   dcpo_demsupport.Rmd
    |   dcpo_demsupport_app.bib
    |   dcpo_demsupport_text.bib
    |   exp_claassen_m5.R
    |   exp_dcpo.R
    |   multiple-bibliographies.lua
    |   readme.txt
    |   renv.lock
    |   supdem.stan.mod5.stan
    |   
    +---data
    |       claassen_input_raw.csv
    |       claassen_replication_output.rda
    |       correct_cls_ajps.rda
    |       correct_cls_apsr.rda
    |       dcpo_ajps.rda
    |       dcpo_apsr.rda
    |       dcpo_input_raw.csv
    |       dem_mood_apsr.RData
    |       expcor_cls_ajps.rda
    |       expcor_cls_apsr.rda
    |       exp_claassen_input.rda
    |       exp_claassen_output.rda
    |       exp_dcpo_input.rda
    |       exp_dcpo_output.rda
    |       raw_data_controls.RData
    |       supdem raw survey marginals.tab
    |       Support_democracy_ajps.csv
    |       
    +---output
    |       estimates_clsMeanAJPS.RDS
    |       estimates_clsMeanAPSR.RDS
    |       estimates_moc_correctAJPS.RDS
    |       estimates_moc_correctAPSR.RDS
    |       estimates_moc_dcpoAJPS.RDS
    |       estimates_moc_dcpoAPSR.RDS
    |       estimates_moc_expcorAJPS.RDS
    |       estimates_moc_expcorAPSR.RDS
    |       
    \---renv
        |   .gitignore
        |   activate.R
        |   settings.dcf
        |   
        \---library
            \*
        
Based on the above setting, one can render the file through the following command in R: 

```r
if(!require(renv)) install.packages("renv")
renv::restore()

rmarkdown::render('dcpo_demsupport.Rmd',  encoding = 'UTF-8')
```



# Replicating the results in the manuscript and supplementary materials
 
`dcpo_demsupport.Rmd` requires the following files to produce results:

- customFunctions.R

- Files to compile the PDF (saving them at the same directory as the rmd file)
    - multiple-bibligraphies.lua
    - dcpo_demsupport_text.bib
    - dcpo_demsupport_app.bib
    - apsr.bst

- data/
    - Support_democracy_ajps.csv
    - supdem raw survey marginals.tab
    - dem_mood_apsr.RData
    
    - correct_cls_ajps.rda
    - correct_cls_apsr.rda
    - expcor_cls_ajps.rda
    - expcor_cls_apsr.rda
    - exp_claassen_input.rda
    
    - dcpo_ajps.rda
    - dcpo_apsr.rda
    - exp_dcpo_input.rda

- output/
    - estimates_clsMeanAPSR.RDS
    - estimates_clsMeanAJPS.RDS
    - estimates_moc_correctAJPS.RDS
    - estimates_moc_expcorAJPS.RDS
    - estimates_moc_correctAPSR.RDS
    - estimates_moc_expcorAPSR.RDS
    - estimates_moc_dcpoAPSR.RDS
    - estimates_moc_dcpoAJPS.RDS
    - estimates_moc_dcpoAPSR.RDS
    - estimates_moc_dcpoAJPS.RDS

Note: 

the codes for creating the files in output/ are available in the rmd file. 
Readers can recreate them based on the needs by turning the code chunks with `eval = FALSE` options to `eval = TRUE`. 
We provide the established files just for speeding the compiling process up.




# Recreating the source files

To make the analysis fully transparent, we also provide codes to recreate the files in data/, although you do not have to go through them for compiling the rmd file and produce the figures and tables in the paper.
Within the files in data/, three of them can be downloaded from Claassen 2020 & 2020a at https://doi.org/10.7910/DVN/FECIO3 and https://doi.org/10.7910/DVN/HWLW0J.
We include the codes to automatically download them with the `dataverse` package, but one needs to have an API key from the dataverse website first.
The rest files can be reproduced by "analysisData.R".
Warning that it may take a relatively long time.

If readers want to go even further to recreate the source-data files called by `analysisData.R`, here is a list of the files and the codes and sources how we get them:

- `mood_dem.csv`: the raw data to measure democratic support.  

- `exp_claassen_input.rda`: the expanded data used in measuring democratic support
    - Codes: `exp_claassen_m5.R` with `mood_dem.csv`. Note that to set up input raw data, one needs raw survey datasets. See https://github.com/fsolt/DCPOtools for more information.

- `correct_cls_ajps.rda` and `correct_cls_apsr.rda`: data for replicating original data with uncertainty.
    - Codes: 
        - `analysisData.R` 
        - `claassen_m5_rep.R`(Optional, needed only when replicating the `claassen_replication_input.rda` and `claassen_replication_output.rda`) .
    - Source Data: 
        - `raw_data_controls.RData`, including raw data for creating variables used in analysis. 
        - `claassen_replication_input.rda`
        - `claassen_replication_output.rda`. 
             
- `expcor_cls_ajps.rda` and `expcor_cls_apsr.rda`: expanded data with uncertainty.
    - Codes: 
        - `analysisData.R`
        - `exp_claassen_m5.R`(Optional, needed only when replicating the `exp_claassen_input.rda` and `exp_claassen_output.rda`) 
    - Source Data: 
        - `raw_data_controls.RData`
        - `exp_claassen_input.rda`
        - `exp_claassen_output.rda`

- `dcpo_ajps.rda` and `dcpo_apsr.rda`: data of new measures produced by Dynamic Comparative Public Opinion (DCPO) model on expanded data with uncertainty. 
    - Codes:
        - `analysisData.R`
        - `exp_dcpo.R` (Optional, needed only when replicating `exp_dcpo_input.rda` and `exp_dcpo_outpu.rda`)
    - Source codes:
        - `raw_data_controls.RData`
        - `exp_dcpo_input.rda`
        - `exp_dcpo_outpu.rda`


The estimations of the measurements for the public support of democracy also require the file `supdem.stan.mod5.stan` and each run usually needs a couple of days to converge. 
Replicators are recommended to use high-performance computing clusters.
	
Basic working environment:
- R version >= 4.1.2, 
- `Rcpp` 1.0.0, 
- `DCPOtools` 0.1.0.9000,
- `rstan` 2.18.2.
- `DCPO`(Optional, needed only when estimating the DCPO estimates used in the online supplementary materials)
