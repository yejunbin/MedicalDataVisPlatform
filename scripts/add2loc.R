library('RCurl')
library('rjson')

setwd('"C:/Users/junbin/Desktop/医疗数据项目/病历数据/Shiny"')
load("data/homepage.Rdata")

addr.raw=with(homepage,data.frame(P101,P14,P17,P801,year))

addr.format=formatLoc(addr.raw$P801)
addr.clean=unique(addr.format)


AK="s95xZne3dvdiXNmQKesbaGjvuwo6xGIH"

addr.loc=c()
for(i in 1:length(addr.clean)){
	loc=tryCatch({addr2loc(addr.clean[i])},error=function(e){""})
	addr.loc[i]=ifelse(length(loc)==0,"",loc)
}

idx=match(addr.format,addr.clean)

addr.loc.all=addr.loc[idx]

addr.loc.stat=as.data.frame(table(addr.loc.all[!addr.loc.all==""]))
addr.loc.stat=cbind(as.data.frame(do.call(rbind,strsplit(as.character(addr.loc.stat$Var1),","))),addr.loc.stat[,2])
colnames(addr.loc.stat)=c("lat","lng","freq")
write.csv(addr.loc.stat,'data/location.csv',row.names=F)


## functions
formatLoc=function(addr.raw){
	addr.clean=gsub(" ","",addr.raw)
	addr.clean=ifelse(grepl("^[0-9]|[a-zA-Z]|\\+|-|\\/",addr.clean),"",addr.clean)
	addr.clean=ifelse(is.na(addr.clean),"",addr.clean)
	addr.clean=ifelse(addr.clean!=""&!grepl("市|省",addr.clean),paste("长治市",addr.clean,sep=""),addr.clean)
	return(addr.clean)
}

addr2loc=function(location){
	url=paste("http://api.map.baidu.com/geocoder/v2/?ak=",AK,"&output=json&address=",location, sep = "")
	url_string <- URLencode(url) 
	connect <- url(url_string)
	temp_geo <- fromJSON(paste(readLines(connect,warn = F), collapse = ""))
	temp_lat<-temp_geo$result$location$lat
	temp_lng<-temp_geo$result$location$lng
	return(paste(temp_lat,temp_lng,sep=","))
}