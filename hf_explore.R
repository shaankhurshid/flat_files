# Script to analyze summarized C3PO data

# Dependencies
library(data.table)
library(plyr)
library(prodlim)

# Load summary data
hf <- fread(file='/data/cvrepo/ndiamant/hf_definition_cohort.tsv')

# Create any HF variables
hf[,has_hf := ifelse(inpatient_hf_age != 0 | outpatient_hf_age != 0,1,0)]
hf[has_hf==1,':='(any_hf_age = ifelse(inpatient_hf_age==0,outpatient_hf_age,
                             ifelse(outpatient_hf_age==0,inpatient_hf_age,pmin(inpatient_hf_age,outpatient_hf_age))))]

# Create 5-year incident HF variables
## Events
hf[,':='(hf_5y_inpatient_primary = ifelse(c((inpatient_primary_hf_age - age_start_fu > 0) & (inpatient_primary_hf_age - age_start_fu <= 5)),1,0),
         hf_5y_outpatient = ifelse(c((outpatient_hf_age - age_start_fu > 0) & (outpatient_hf_age - age_start_fu <= 5)),1,0),
         hf_5y_inpatient = ifelse(c((inpatient_hf_age - age_start_fu > 0) & (inpatient_hf_age - age_start_fu <= 5)),1,0),
         hf_5y_any = ifelse(c((any_hf_age - age_start_fu > 0) & (any_hf_age - age_start_fu <= 5)),1,0))]
## Time
hf[,':='(hf_5y_inpatient_primary.t = ifelse(hf_5y_inpatient_primary==1,inpatient_primary_hf_age-age_start_fu,5),
         hf_5y_outpatient.t = ifelse(hf_5y_outpatient==1,outpatient_hf_age-age_start_fu,5),
         hf_5y_inpatient.t = ifelse(hf_5y_inpatient==1,inpatient_hf_age-age_start_fu,5),
         hf_5y_any.t = ifelse(hf_5y_any==1,any_hf_age-age_start_fu,5))]

## Prevalent disease
hf[,':='(hf_prev_inpatient_primary = ifelse(c(inpatient_primary_hf_age - age_start_fu <= 0),1,0),
         hf_prev_outpatient = ifelse((outpatient_hf_age - age_start_fu <= 0),1,0),
         hf_prev__inpatient = ifelse((inpatient_hf_age - age_start_fu <= 0),1,0),
         hf_prev_any = ifelse((any_hf_age - age_start_fu <= 0),1,0))]

# No HF
no_hf <- hf[has_hf==0]

# Plot CI of HF
## Any
hf_any <- prodlim(Hist(hf_5y_any.t,hf_5y_any)~1,data=hf[c(!is.na(hf_prev_any) & hf_prev_any==0)])

pdf(file='/data/arrhythmia/skhurshid/flat_files/hf_any.pdf',height=3,width=5,
         pointsize=5)
par(oma=c(3,1,1,1),mar=c(3,1,1,1))
plot(hf_any,"cuminc",ylim=c(0,1),
     axis2.at=seq(0,1,0.1),axis2.las=2,lwd=1.5,axis1.cex.axis=1.5,axis2.cex.axis=1.5,background=F,
     atrisk.times=c(0,1,2,3,4,5),col=c("#253494"),atrisk.col='black',confint=FALSE,
     atrisk.title=("                     "),atrisk.pos=-0.35,atrisk.line=c(1.2,2.5),
     atrisk.cex=1.3,atrisk.interspace=1.4,xlab='',ylab='')
mtext("Cumulative incidence of HF (%)",side=2,line=-1.2,at=0.5,cex=1.5)
mtext("Years",side=1, line=-0.6,cex=1.5)
dev.off()

## Inpatient-Primary
