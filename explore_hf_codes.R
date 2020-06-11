# Script to explore HF code instances

# Dependencies
library(data.table)
library(plyr)

# Load HF codes file
all_hf_codes <- fread(file='/data/cvrepo/ndiamant/all_hf_codes_for_shaan.tsv')

# Load linker for start_fu
linker <- fread(file='/data/arrhythmia/skhurshid/flat_files/c3po_startfu.csv')
for (j in c('start_fu','birth_date')){set(linker,j=j,value=as.Date(linker[[j]],format='%Y-%m-%d',origin='1970-01-01'))}
linker[,':='(age_startfu = as.numeric(start_fu - birth_date)/365.25)]

# Load linker for ID
id_linker <- fread(file='/data/arrhythmia_source/data/2020_loyalty/linker/old_linkers/patient_linker_file_04-01-20.csv')

# Set keys and join
setkey(id_linker,empi); setkey(linker,empi)
linker[id_linker,linker_id := i.linker_id]

setkey(linker,linker_id); setkey(all_hf_codes,id)
all_hf_codes[linker,':='(start_fu = i.start_fu,age_startfu=i.age_startfu)]

# Date cleanup
for (j in 'start_fu'){set(all_hf_codes,j=j,value=as.Date(all_hf_codes[[j]],format='%Y-%m-%d',origin='1970-01-01'))}

# Define date of HF code
all_hf_codes[,':='(hf_date = start_fu + (age-age_startfu)*365.25)]
all_hf_codes$hf_date_yr = as.numeric(strtrim(floor_date(all_hf_codes$hf_date,unit='year'),width=4))

# Code prefix
all_hf_codes[,':='(dia_prefix = strtrim(Dia.Code,width=3))]

# Histograms
## Plot 1: Temporal frequency of each code over year
for (i in unique(all_hf_codes$dia_prefix)){
  pdf(paste0('/data/arrhythmia/skhurshid/flat_files/hf_code_hist/',i,'_hist.pdf'))
  plot <- hist(all_hf_codes[dia_prefix==i]$hf_date_yr, 
               breaks=(min(all_hf_codes[dia_prefix==i]$hf_date_yr,na.rm=T)-1):(max(all_hf_codes[dia_prefix==i]$hf_date_yr,na.rm=T)), 
               xaxt="n",main='',xlab='')
  axis(1, at=plot$mids, 
       labels=min(all_hf_codes[dia_prefix==i]$hf_date_yr,na.rm=T):max(all_hf_codes[dia_prefix==i]$hf_date_yr,na.rm=T))
  mtext('Year',1,line=3)
  mtext(paste0('Histogram of Prefix ',i),line=1.5,cex=2)
  dev.off()
}

## Plot 2: Count of most frequent code by year using prefix
most_popular_code <- function(x){
  code <- names(sort(table(x),decreasing=TRUE))[1]
  return(code)
}

most_popular_freq <- function(x){
  freq <- (sort(table(x),decreasing=TRUE)[1]) / length(x)
  return(freq)
}

code_mode <- all_hf_codes[,lapply(.SD,most_popular_code),by=hf_date_yr,.SDcols='dia_prefix']
code_mode <- code_mode[!is.na(hf_date_yr)]
code_freq <- all_hf_codes[,lapply(.SD,most_popular_freq),by=hf_date_yr,.SDcols='dia_prefix']
code_freq <- code_freq[!is.na(hf_date_yr)]
setkey(code_mode,hf_date_yr); setkey(code_freq,hf_date_yr)

# Make the plot
pdf(file='/data/arrhythmia/skhurshid/flat_files/hf_code_hist/popular_prefix_year.pdf',
    height=3,width=5,pointsize=5)
par(mar=c(2,2,2,1),oma=c(2,2,2,1))
col=ifelse(code_mode$dia_prefix=='428','#014636','#4575b4')
coords <- barplot(code_freq$dia_prefix[!is.na(code_freq$hf_date_yr)],col=col)
n <- as.numeric(str_sub(code_freq$hf_date_yr,3,4))
mtext("Year",1,line=2)

par(xpd=TRUE)
legend(coords[length(coords)-3],1.1,col=c('#014636','#4575b4'),legend=c('428','I50'),
       bty='n',lty=1,lwd=2)
for (i in 1:length(coords)){
  text(x=coords[i],y=-0.03,labels=paste0(n[i]))
}
dev.off()

## Plot 3: Count of most frequent code by year using code
most_popular_code <- function(x){
  code <- names(sort(table(x),decreasing=TRUE))[1]
  return(code)
}

most_popular_freq <- function(x){
  freq <- (sort(table(x),decreasing=TRUE)[1]) / length(x)
  return(freq)
}

code_mode <- all_hf_codes[,lapply(.SD,most_popular_code),by=hf_date_yr,.SDcols='Dia.Code']
code_mode <- code_mode[!is.na(hf_date_yr)]
code_freq <- all_hf_codes[,lapply(.SD,most_popular_freq),by=hf_date_yr,.SDcols='Dia.Code']
code_freq <- code_freq[!is.na(hf_date_yr)]
setkey(code_mode,hf_date_yr); setkey(code_freq,hf_date_yr)

# Make the plot
pdf(file='/data/arrhythmia/skhurshid/flat_files/hf_code_hist/popular_code_year.pdf',
    height=3,width=5,pointsize=5)
par(mar=c(2,2,2,1),oma=c(2,2,2,1))
col=ifelse(code_mode$Dia.Code=='428.0','#014636',
           ifelse(code_mode$Dia.Code=='I50.9','#4575b4','#fc4e2a'))
coords <- barplot(code_freq$Dia.Code[!is.na(code_freq$hf_date_yr)],col=col)
n <- as.numeric(str_sub(code_freq$hf_date_yr,3,4))
mtext("Year",1,line=2)

par(xpd=TRUE)
legend(coords[length(coords)-3],1.1,col=c('#014636','#4575b4','#fc4e2a'),legend=c('428.0','I50.9','I50.32'),
       bty='n',lty=1,lwd=2)
for (i in 1:length(coords)){
  text(x=coords[i],y=-0.03,labels=paste0(n[i]))
}
dev.off()