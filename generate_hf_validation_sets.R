# Script to explore HF code instances

# Dependencies
library(data.table)
library(plyr)
library(stringr)
library(lubridate)

# Load concatenated HF long file
hf_long <- fread('/data/cvrepo/pbatra/hf/2020-06-08/concat_longs.tsv')

# Load linker for start_fu
linker <- fread(file='/data/arrhythmia/skhurshid/flat_files/c3po_startfu.csv')
for (j in c('start_fu','birth_date')){set(linker,j=j,value=as.Date(linker[[j]],format='%Y-%m-%d',origin='1970-01-01'))}
linker[,':='(age_startfu = as.numeric(start_fu - birth_date)/365.25)]

# Load linker for ID
id_linker <- fread(file='/data/arrhythmia_source/data/2020_loyalty/linker/old_linkers/patient_linker_file_04-01-20.csv')

# Set keys and join
setkey(id_linker,empi); setkey(linker,empi)
linker[id_linker,linker_id := i.linker_id]

setkey(linker,linker_id); setkey(hf_long,id)
hf_long[linker,':='(start_fu = i.start_fu)]

# Date cleanup
for (j in 'start_fu'){set(hf_long,j=j,value=as.Date(hf_long[[j]],format='%Y-%m-%d',origin='1970-01-01'))}

# Define year of HF diagnosis
## Define date of HF code
hf_long[,':='(code_age = as.numeric(str_extract(age,'^[[:digit:]]+'))/365.25)]
hf_long[,':='(code_date = as.Date(start_fu + as.numeric(code_age-age_start_fu)*365.25))]

## Get first HF date for each person
first_hf_inpt <- hf_long[c(c(!is.na(Dia.Code) & Dia.Code != '') & inpatient_hf==1),
            lapply(.SD,min),by='linker_id',.SDcols='code_date']
first_hf_outpt <- hf_long[c(c(!is.na(Dia.Code) & Dia.Code != '') & outpatient_hf==1),
                    lapply(.SD,min),by='linker_id',.SDcols='code_date']
first_hf_any <- hf_long[c(c(!is.na(Dia.Code) & Dia.Code != '') & c(outpatient_hf==1 | inpatient_hf==1)),
                          lapply(.SD,min),by='linker_id',.SDcols='code_date']

## Join back to master file
setkey(hf_dates,linker_id); setkey(hf_long,linker_id)
hf_long[hf_dates,first_hf_date := i.code_date]

## Simplify date to year
hf_long$first_hf_yr = as.numeric(strtrim(floor_date(hf_long$first_hf_date,unit='year'),width=4))

# Obtain count and frequency of LVEF type by first HF year
# Widen
hf_wide <- unique(hf_long,by='linker_id')

# Get counts
hfref <- hf_long[c(!is.na(lvef_type) & lvef_type==1),.N,by=first_hf_yr]
hfpef <- hf_long[c(!is.na(lvef_type) & lvef_type==2),.N,by=first_hf_yr]
no_ef <- hf_long[is.na(lvef_type),.N,by=first_hf_yr]
all <- hf_long[,.N,by=first_hf_yr]

# Unite
setkey(hfref,first_hf_yr); setkey(hfpef,first_hf_yr); setkey(no_ef,first_hf_yr); setkey(all,first_hf_yr); 

hf_type_by_yr <- all[hfref,hfref:=i.N][hfpef,hfpef:=i.N][no_ef,no_ef:=i.N]

# Barplot
hf_type_by_yr.plot <- hf_type_by_yr[!is.na(first_hf_yr)]

pdf(file='/data/arrhythmia/skhurshid/flat_files/hf_type_by_yr.pdf',pointsize=5,
    height=4,width=6)
par(oma=c(2,3,1,1),mar=c(2,3,1,1))

coords <- barplot(hf_type_by_yr.plot$N,yaxt='n',ylim=c(0,225000),col='#f03b20')
par(new=TRUE)
barplot(hf_type_by_yr.plot$N-hf_type_by_yr.plot$hfref,yaxt='n',col='#4575b4',ylim=c(0,225000))
par(new=TRUE)
barplot(hf_type_by_yr.plot$N-(hf_type_by_yr.plot$hfref+hf_type_by_yr.plot$hfpef),
                              yaxt='n',col='lightgray',ylim=c(0,225000))
par(xpd=TRUE)
legend(0.5,225000,legend=c('HFrEF','HFpEF','Unknown EF'),
       col=c('#f03b20','#4575b4','lightgray'),pch=15,bty='n',cex=1.5)

# X-axis
n <- as.numeric(str_sub(hf_type_by_yr.plot$first_hf_yr,3,4))
mtext("Year",1,line=2.5,cex=1.7)
par(xpd=TRUE)
for (i in 1:length(coords)){
  text(x=coords[i],y=-7000,labels=paste0(n[i]),cex=1.3)
}

# Y-axis
axis(2,at=seq(0,225000,25000),las=2,cex.axis=1.3,pos=0)
mtext("Case Count",2,line=4,cex=1.7)

dev.off()

# Save output
write.csv(hf_type_by_yr,file='/data/arrhythmia/skhurshid/flat_files/hf_type_by_yr.csv')
