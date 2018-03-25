---
output: html_document
---

launch app with shiny::runApp(launch.browser = TRUE)

**The Pepliner app is for viewing peptide or protein elution profiles**

- In a fractionation-MS experiment, a protein sample is separated across a gradient into multiple fractions.
- Proteins are identified from each fraction independently.
- It's often necessary to visualized where a protein is detected across fractions.
- Pepliner is a suite of R functions and a shiny app for viewing patterns of detection of peptides or proteins across multiple fractions.
- The main functions of this shiny are to:
- 1. Interactively select a series of proteins to plot (protein-level visualization)
- 2. Interactively plot peptides from a single protein (peptide-level visualization)

- *Explore* the app's features with the example data set pre-loaded by clicking on the tabs above.
- *Upload* your data in the "Input Data" tab.

### <a name="features"></a> Features

*Visualize your data:*

- Visualize elutions of peptides from single proteins 
- Visualize elutions of multiple proteins across fractions


### <a name="dataformats"></a> Data Format

- Must be a .CSV *comma-separated-value* file (you may export from Excel).
- File must have a header row. 
- Depending on whether uploading protein-level or peptide-level data format as follows:
 
#### Protein data

- Must be a .CSV *comma-separated-value* file (you may export from Excel).
- Upload data either in wide or tidy format 
- [Tutorial on wide vs. tidy data](http://r4ds.had.co.nz/tidy-data.html)


##### Wide format
- Must contain a column called ID, with remaining columns rightward representing individual fractions
- Each row is a proteins's detection across fractions

| ID  | Fraction1  | Fraction2  | Fraction3  |
|---|---|---|---|---|
| ProteinA  | 0  | 1  | 3  |
| ProteinB  | 5  | 25  | 10  |

##### Tidy format
- Must contain columns ID, ProteinCount, FractionID (in any order)
- Each row is a protein's detection in a single fraction

| ID | FractionID | ProteinCount |
|---|---|---|
| ProteinA  | Fraction1  | 0  |
| ProteinA  | Fraction2  | 1  |
| ProteinA  | Fraction3  | 3  |
| ProteinB  | Fraction1  | 5  |
| ProteinB  | Fraction2  | 25  |
| ProteinB  | Fraction3  | 10  |

#### Peptide data

- Must be a .CSV *comma-separated-value* file (you may export from Excel).


##### Wide format
- Must contain a columns called Peptide and ID, with remaining rightward columns representing individual fractions
- Each row is a peptide's detection across fractions.

|Peptide| ID  | Fraction1  | Fraction2  | Fraction3  |
|---|---|---|---|---|---|
|AAGTEPR| ProteinA  | 0  | 1  | 1  |
|CNMETLLK| ProteinA  | 0  | 0  | 2  |
|IPRELVK| ProteinB  | 2  | 12  | 2  |
|MITFPELLR| ProteinB  | 3  | 13  | 8  |
##### Tidy format
- Must contain columns Peptide, ID, PeptideCount, FractionID (in any order)
- Each row is a peptide's detection in a single fraction

|Peptide| ID | FractionID | ProteinCount |
|---|---|---|---|
|AAGTEPR| ProteinA  | Fraction2  | 1  |
|AAGTEPR| ProteinA  | Fraction3  | 1  |
|CNMETLLK| ProteinA  | Fraction3  | 2  |
|IPRELVK| ProteinB  | Fraction1  | 2  |
|IPRELVK| ProteinB  | Fraction2  | 12  |
|IPRELVK| ProteinB  | Fraction3  | 2  |
|MITFPELLR| ProteinB  | Fraction3  | 3  |
|MITFPELLR| ProteinB  | Fraction3  | 13  |
|MITFPELLR| ProteinB  | Fraction3  | 8  |


#### <a name="savedata"></a> *TIP*: Save Data for Future Upload

After uploading your data to the pepliner app, click red button
![](ex_click_rdata.png)

to download an .RData file to upload your data to the pepline app with one click.

Next time use the "Input Data" tab --> "Pepliner RData file" option.


#### <a name="help"></a> More Help and Info

Additional help information and more detailed instructions are provided under the "Instructions" tab.

#### App Info


#### Provenance

The source code for pepliner is available on [Github](https://github.com/marcotte_lab/pepliner).

A paper describing use of peptide elutions to identify proteoforms will be posted on [BioRxiv](biorxiv.org).

Pepliner was modified from the START app developed by Jessica Minnier, Jiri Sklenar, Anthony Paul Barnes, and Jonathan Nelson
of Oregon Health & Science University, Knight Cardiovascular Institute and School of Public Health.

[Nelson, JW, Sklenar J, Barnes AP, Minnier J. (2016) "The START App: A Web-Based RNAseq Analysis and Visualization Resource." Bioinformatics.  doi: 10.1093/bioinformatics/btw624.](http://bioinformatics.oxfordjournals.org/content/early/2016/09/27/bioinformatics.btw624.abstract)

The original source code of START is available on [Github](https://github.com/jminnier/STARTapp).



