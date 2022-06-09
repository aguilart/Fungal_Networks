# Fungal_Networks

Code for the analysis and data visualization of fungal network data as published in https://www.nature.com/articles/s43705-021-00085-1

1. "ISMEJC_data_assembly.Rmd" 
 Includes the code to translate mycelial pictures into graph objects. The input are data matrices produced by the "Fungal Networks" GUI Matlab (avilable to download in https://doi.org/10.5281/zenodo.5187933.) that does the image processing. This matrices are all included in the folder "ISMEJ_C_data". The output are graph objects and data frames that can readily analyzed using common multivariate statistic approaches as described in the second RMD below
 
 
2.  "ISMEJC_analysis&Figures.Rmd"
 Contains the code for the statiscal analysis of the data extracted from the graph objects and plots in published manuscript. Data figures are stored in the folder "ISMEJ_C_Figures"

