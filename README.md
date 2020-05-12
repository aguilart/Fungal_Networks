# Fungal_Networks

Here I will update the multivariate analysis of Network data.
## Chronological order of what has been done.

1. "Protocol_multivariate.R" and "Multivariate.Rmd": the first analysis where we explore the distribution of all variables produced from the matlab app, their correlation among ecah other and an ordination of the species based on the mean of those traits. You can see the results here: https://aguilart.github.io/Fungal_Networks/Multivariate.html

2. "analysis_igraph.Rmd": is "shift" to analyzing the network using igraph from r. Two main things where done: a) Calculate (again) varialbes like accessibility, route efficiency in igraph and compared them with ones produced by the FungalNetworks app (and they match pefectly); b) consutruct toy models and calculate network parameters from them. You can see the results here: https://aguilart.github.io/Fungal_Networks/analysis_igraph.html

3. "FungalNetworkDiversity.Rmd" is the content (both text and code) for the first manuscript coming from this analysis. This is the code that was used to create the googledocs FungalNetworkDiversity

## Issues/pending work

* __Width measures have a fixed scale! It turns out that the app always reports 5.6 as the max hyphal width regardless of species. That cannot be true because I manually measured some with the zeiss software.__
* Accessibility is based on resistance_2 and not resistance_2ave.
* I have not yet use betweeness in the analysis because I still do not understand their values
* I have to used Global efficiency as it is a very computing demanding task (for my laptop) but soon to be done at a desktop at FU.
* Robustness is still not included in any analysis
* Anlaysis so far includes only Mucoromyctan fungi.
