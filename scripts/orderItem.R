# find all items of patient "534605"

# create data
orders=read.csv('data/Pat534605Order.txt',sep=";",stringsAsFactors=FALSE,header=F)
orders=orders[orders$V204!="",]
orders=orders[,c("V1","V177","V178","V195","V196","V204","V205","V207","V208","V211","V214")]
items=as.data.frame(do.call(rbind,strsplit(orders$V204,'&&')))
colnames(items)=c("code","itemname","codeId")
orders=cbind(orders,items)
orders$type=sub("[0-9].*","",orders$V204)

#read meta
meta=read.csv("data/meta/OrderCategory.csv",header=F)
idx=match(orders$type,meta$V1)
#orders=orders[orders$V208=="E&&执行&&Y",]
orders$type=meta$V3[idx]
orders=orders[!duplicated(orders),]
orders$starttime=paste(orders$V177,orders$V178)
orders$endtime=paste(orders$V195,orders$V196)
orders$endtime=ifelse(orders$endtime==" ",orders$starttime,orders$endtime)
write.csv(orders,"data/patient534605Orders.csv",row.names=F)


