# Dependencies
library(data.table)
library(plyr)

###################################################### Step 1: Cross discrepant set to get EMPIs
# Load discrepant
discrep <- fread(file='/data/cvrepo/ndiamant/in_flatfiles_mart_hf_no_hf_icd_for_shaan.csv')

# Load linker
linker <- fread(file='/data/arrhythmia/skhurshid/flat_files/incid_hf_mart_empi_to_linker_id_for_nate.txt')

# ID'ed set
setkey(discrep,linker_id); setkey(linker,linker_id)
discrep <- discrep[linker,nomatch=0]

# Output IDs for mart/review
write.csv(discrep,file='/data/arrhythmia/skhurshid/flat_files/hf_discrepant_for_review.csv')

###################################################### Step 2: Load mart data for review
## Load data
discrep_review <- fread(file='/data/arrhythmia/skhurshid/flat_files/hf_mart_review_wide.txt') # Mart data 

## Join on EMPIs
setkey(mart_linker,patient_num); setkey(discrep_review,patient_num)
discrep_review <- discrep_review[mart_linker,':='(empi = i.empi)]

## Sample and narrow
set.seed(1)
sample_review <- discrep_review[sample(1:nrow(discrep_review),100)][,c('patient_num','empi','linker_id','birth_date','first_enc','start_fu','last_enc','prev_hf','incd_hf','first_hf','time_to_hf')]

## Date cleaner
# Remove times from dates
names <- c('birth_date','first_enc','start_fu','last_enc','first_hf')
for (j in (names)){set(sample_review,j=j,value=strtrim(sample_review[[j]],width=10))}

# Set to POSIX time
for (j in (names)){set(sample_review,j=j,value=as.Date(sample_review[[j]],format='%Y-%m-%d'))}

## Join and get the things we need
write.csv(sample_review,file='/data/arrhythmia/skhurshid/flat_files/hf_sample_review.csv')

###################################################### Step 3: Link reviewed files back
reviewed <- fread(file='/data/arrhythmia/skhurshid/flat_files/hf_sample_review.csv')

a = reviewed[!is.na(`HF?`)]
b = a[,c("linker_id","Code?")]

write.csv(b,file='/data/cvrepo/ndiamant/hf_discrepant.csv')

