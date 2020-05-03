# Rosenberg_JNeuro2020
Code associated with Rosenberg et al., 2020, Journal of Neuroscience

ABCD201_baseline_behavioral_analyses.R: Loads curated ABCD data files (which can be downloaded from https://nda.nih.gov/abcd in accordance with an approved Data Use Certification [https://nda.nih.gov/abcd/request-access]), performs behavioral analyses reported in the text, and generates "raw" versions of manuscript Figures 1-3. To assess cross-release consistency (i.e., consistency between results from ABCD curated releases 1.1 and 2.0.1), the script loads the file release1point1_ndars.csv, which contains a list of all NDARS from the full included sample (i.e., data.merge) included in curated release 1.1. This csv has two columns: column 1 is a vector of the integers 1:4397 and column 2 is a vector of the unique NDAR_INV######## identifiers from release 1.1. The column header for the second column is assumed to be "x".
