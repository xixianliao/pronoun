This repository contains the materials, scripts and a supplementary file for the following paper:

*Comparing models of pronoun production and interpretation with observational and experimental evidence*.

To ensure smooth execution of the scripts, it is highly recommended that you **download the ENTIRE repository**. Note that:

 * Analysis scripts are written in __R version 4.1.2 (2021-11-01)__
 * Python scripts are written in __Python version 3.7.4 (2019-08-13)__ 
 * Required to run scripts in the script directory



# Corpus analyses

`extraction_scripts`: contains python codes for the extraction of corpus passages. which requires three corpora under `extraction_scripts/corpora/`: 

* [OntoNotes Release 5.0](https://catalog.ldc.upenn.edu/LDC2013T19)
* [RST Discourse Treebank](https://catalog.ldc.upenn.edu/LDC2002T07)
* [The ARRAU Corpus of Anaphoric Information](https://catalog.ldc.upenn.edu/LDC2013T22)

`extracted_passages`: contains four `.csv` files with extracted corpus passages for analysis. 

* `ontonotes_extracted_passages.csv`: generated by running the file `extraction_scripts/ontonotes_extraction.py`
* `rstdt_extracted_passages.csv`: generated by running the file `extraction_scripts/rstdt_extraction.py`
* `tpv_extracted_passages.csv`: generated by running the file `extraction_scripts/tpv_extraction.py`
* `icv_extracted_passages.csv`: generated by running the file `extraction_scripts/icv_extraction.py`

`analyses`: contains R scripts for the statistical analyses and plots reported in the section "Observational examination of Strong Bayes: corpus analyses" and Appendix A "More information in the corpus analyses with verb types".



# Passage continuation experiment

`stimuli`: contains experimental stimuli and fillers extracted from the OntoNotes corpus.

`data`: contains python script for data preprocessing, processed data used for analysis, and results of manual examination for participants' own next-mention annotation and automatically-annotated pronominalization.  

* `preprocess_responses.py`: script for preprocessing reponses including the automatic annotation of pronominalization, as well as generating a dataframe of all the model predictions `model_predictions.csv` for model comparisons. 
* `allResponses.csv`: continuations gathered from 200 Prolific participants. All responses are annotated with next mention and pronominalization for analysis. The participant IDs have been "encrypted"/anonymized.
* `model_predictions.csv`: generated by `preprocess_responses.py` and used for analysis.
* `nextMention_check_result.csv`: manual examination result of participants' own next mention annotations
* `pronominalization_check_result.csv`: manual examination result of automatically-annotated pronominalization

`analyses`: contains R scripts for the statistical analyses, plots and model comparisons reported in the section "Experimental evaluation of Weak and Strong Bayes: Corpus passage continuation".  


# Supplementary file
 * Section A: More information in the corpus analyses with verb types
 * Section B: More information in the analyses with discourse relations
 * Section C: Analysis results excluding participants with low variation in referring expressions
 * Section D: Comparison of pronoun interpretation models using Bayesian methods



