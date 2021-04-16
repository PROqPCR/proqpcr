
![](https://raw.githubusercontent.com/mathildesautreuil/proqpcr/master/www/proqpcr.png)

## PROqPCR: shiny application for the processing of qRT-PCR data

https://qpcrapp.shinyapps.io/proqpcr/

**PROqPCR** enables the processing of qRT-PCR data. From Ct and Efficiency files                                               for each gene, a serie of steps of analysis is performed to getting several plots.
The different successive steps of analysis are the:

- average on  the technical replicates

- calculation the normalized relative quantity for one gene given for each biological replicate (for more information go to Help)

- average on the biological replicates

- calculation the log-ratio for all the compared conditions.

For each gene, we get therefore in the end a value of normalized and averaged Ct on the technical and biological replicates per condition.

The tables of results are downloadable at each step. Differents plots are available:

- Barplot of conditions for a given gene

- Barplot of genes for a given condition

- Comparison of barplots for choose genes in all the conditions.

We can note the Ct files are the output files got when the PCR instrument 
is realizing the calculation of Cycle threshold. Cycle threshold (Ct) is defined as 
the fractional PCR cycle number at which the reporter fluorescence is greater than 
the threshold (3-5 times of the standard deviation of the signal noise above background). 
We can note the Efficiency files are the output files got when the PCR instrument is running 
serial dilutions with 5-log dilutions.

Moreover, the tab "RNA-Seq" allows users to compare the results of qRT-PCR with that of RNA-Seq. In this goal, 
differents plots are available:

- Correlation plot between the log-ratio of qRT-PCR and that of RNA-Seq

- Barplot of conditions for a given gene between the two experiments.

