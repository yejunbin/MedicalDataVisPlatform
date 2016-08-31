# Get data

if(!file.exists("data/homepage.Rdata")){
	homepage=data.frame(read.csv('data/homepages.csv',stringsAsFactors=FALSE))
	save(homepage,file='data/homepage.Rdata')
}

load('data/homepage.Rdata')

# Calculate the number of total hospitals, patients, diseases

homepage$uid=with(homepage,paste(P4,P5,P6))
nhospital=length(unique(homepage$P6891))
npatient=length(unique(homepage$uid))
ndisease=length(unique(homepage$P321))

# data frame: number of patients over time
# accession

accession=as.data.frame(table(substring(as.Date(homepage$P22,format="%Y/%m/%d"),1,7)))
discharge=as.data.frame(table(substring(as.Date(homepage$P25,format="%Y/%m/%d"),1,7)))

acc_dis=merge(accession,discharge,by.x="Var1",by.y="Var1",all=T)
colnames(acc_dis)=c("Date","Accession","Discharge")
acc_dis=as.data.frame(apply(acc_dis,2,function(x)ifelse(is.na(x),0,x)))
acc_dis$Accession=as.numeric(acc_dis$Accession)
acc_dis$Discharge=as.numeric(acc_dis$Discharge)
#acc_dis$Discharge=jitter(acc_dis$Discharge)
acc_dis=melt(acc_dis,id="Date",measure=c("Accession","Discharge"))
acc_dis$Date=as.Date(paste(acc_dis$Date,'-01',sep=""))

## data frame: department

hp.depart=homepage[,c('P23','year')]
rc023=read.csv('data/RC023.csv',header=T)

idx=match(hp.depart$P23,rc023[[1]])
dp=as.character(rc023[[2]][idx])
dp=ifelse(is.na(dp),as.character(hp.depart$P23),dp)
dp=sub('专业','',dp)
dp=ifelse(dp=='普外科','普通外科',dp)
dp=ifelse(dp=='内分泌','内分泌科',dp)
dp=ifelse(dp %in% c("新生儿","新生儿科"),'儿科',dp)
dp=ifelse(dp=='老年病','老年病科',dp)
dp=ifelse(dp=='感染性疾病科','传染科',dp)
dp=ifelse(dp=='耳鼻喉','耳鼻咽喉科',dp)
dp=ifelse(dp %in% c('骨科一病区','骨科二病区','骨伤科','正骨科'),'骨科',dp)
dp=ifelse(dp=='计划生育','计划生育科',dp)
dp=ifelse(dp=='肾病学','肾内科',dp)
dp=ifelse(dp=='整形科','整形外科',dp)
dp=ifelse(dp=='肿瘤内科','肿瘤科',dp)
dp=ifelse(dp %in% c("胸外二科","胸外科"),"心胸外科",dp)
dp=ifelse(dp %in% c('放射治疗','放射科'),"医学影像科",dp)
dp=ifelse(dp=='\\\\172.','缺失',dp)
hp.depart$P23=dp
