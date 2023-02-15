EnsembleTFPredictor is an ensemble approach to rank putative causal transcription factors that regulate a set of query genes by prediction confidence. This repository provides source code and documentation of the Shiny R version of the application.

1. Download the source code and directories from the GitHub repository, for example using the following command:

 git clone https://github.com/GuoyanZhao-Lab/EnsembleTFPredictor

A directory named EnsembleTFPredictor will appear.

2. Install the R package Shiny, for example using the following command in RStudio:

install.packages("shiny")

3. Set your working directory to the directory containing the EnsembleTFPredictor directory, run the following command:

runApp("EnsembleTFPredictor")

The App will pop up in a new window. 

4. Check the boxes of the tools you wish to upload results from. To test that the app is working appropriately, you can use the files included in the ExampleInput directory.

5. Use the appropriate inputs to upload the raw results from each tool. You can adjust p-value cutoffs for each tool prior to running the app by editing the MultiRank.R script. The defaults for each tool are listed below:

MORA = 0.05
AME = 0.05
HOMER = 0.1
PSCAN = 0.05
BART2 = 0.05
LISA2 = 0.05

6. Once all results have been uploaded, click the "Submit All Results" button. On the right, the app will generate a table displaying the file names of the files that were uploaded along with the corresponding tool the results are from. If any files were uploaded to the wrong tool by mistake, simply hit the "Reset Input" button for that tool and re-upload the correct results.

7. When you have confirmed that all results were uploaded correctly, hit the "Generate Multi-Method Rank Table" button which will then display the table to the right showing all TFs which were predicted by more than 2 of the tools used. If you wish to see full data table with all TFs predicted by at least one tool, you can download the .csv file using the "Download Full Table CSV File" button located below the table.



