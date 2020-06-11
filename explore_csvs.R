# Dependencies
library(data.table)
library(plyr)

# Load CSV
example <- fread(file='/data/arrhythmia/skhurshid/flat_files/loyalty_cohort_hf_multiprocessed.csv')

# Explore Code_Type
count(example[data_type=='Code_Type']$value)

# Explore Diagnosis_Name
diagnoses <- example[data_type=='Diagnosis_Name']$value