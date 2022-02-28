Replication Files for Yuehong Cassandra Tai, Yue Hu, & Frederick Solt, "Democracy, Public Support, and Measurement Uncertainty", APSR

The `dcpo_demsupport.Rmd` file permits replication of the reanalyses reported by Tai, Hu, and Solt.
 
Here are the data files called by `dcpo_demsupport.Rmd`: 
- `mood_dem.csv`: the raw data to measure democratic support.  

- `exp_claassen_input.rda`: the expanded data that used in measuring democratic support
    - Codes: `exp_claassen_m5.R` with `mood_dem.csv`. Note that to setup input raw data, one needs raw survey datasets. See https://github.com/fsolt/DCPOtools for more information.

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



Note: 

The estimations of the public support of democracy also requires the file `supdem.stan.mod5.stan` and each of them usually need a couple of days to converge. 
Replicators are recommanded to use high-performance computing cluster.
	
Basic working environment:
- R version >= 3.5.1, 
- `Rcpp` 1.0.0, 
- `DCPOtools` 0.1.0.9000,
- `rstan` 2.18.2.
-  `DCPO`(Optional, needed only when estimating the DCPO estimates used in the online supplementary materials)



