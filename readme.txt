Replication Files for Yuehong Cassandra Tai, Yue Hu, & Frederick Solt, "Democracy, Public Support, and Measurement Uncertainty", APSR

The files in this folder permit replication of the reanalyses reported by Tai, Hu, and Solt. 


The file "supdem raw survey marginals.tab" is Claassen's raw data to measure democratic support.
The file "Support_democracy_ajps.csv" is the analysis data used in Claassen's (2020a) AJPS paper. 
The file "dem_mood_apsr.RData"        is the analysis data used in Claassen's (2020) APSR paper. 

The file "mood_dem.csv" is our expanded raw data to measure democratic support.  
The file "correct_cls_ajps.rda" is the replicated analysis data for Claassen's (2020a) AJPS result, following Claassens' data producing process.
The file "correct_cls_apsr.rda" is the replicated analysis data for Claassen's (2020) APSR result, following Claassens' data producing process..
The file "expcor_cls_ajps.rda" is the expanded analysis data with  country-year for replicating Claassen's (2020a) AJPS result.
The file "expcor_cls_apsr.rda" is the expanded analysis data with  country-year for replicatingn Claassen's (2020) APSR result.

The file "dcpo_cls_ajps.rda" is the analysis data with expanded country-year for replicating Claassen's (2020a) AJPS result, using Dynamic Comparative Public Opinion measurement method.
The file "dcpo_cls_apsr.rda" is the analysis data with expanded country-year for replicatingn Claassen's (2020) APSR result,
using Dynamic Comparative Public Opinion measurement method.

With these files, you can replicate our results, figures and tables through dcpo_demsupport.Rmd. 


If you want to replicate our results in full, you need to have the following folder structure on your local directory. Once you download the files from the dataverse and unzip them, you should have the following directories automatically:
	data
	data-raw
	output
	paper
	R

To replicate analysis data used in Rmd file, run the files in the "R" folder in this order:

1) Run measurement models to estimate public support for democracy
   claassen_replication_input.R   
   exp_claassen_input.R
   dcpo_input.R
   argon/dcpo_demsupport_rep/R/claassen_m5_rep.R
   argon/dcpo_demsupport_rep/R/claassen_m5_exp.R
   argon/dcpo_demsupport_rep/R/dcpo_exp.R
Note: it will take several days to fully run these files to replicate measurement outputs from scratch. 

However, we provide measure outputs on the dataverse, so you can use them directly in the following codes 
in order to replicate analysis data.
1) Run vdem_variables.R to create Vdem variables with uncertainty.
2) Run controlv.R to create control variables with uncertainty.
3) Run analysisData.R create analysis data for replication.


	




