setwd('C:/Users/junbin/Desktop/医疗数据项目/病历数据/Shiny')

library('igraph')
library('network')
library('networkD3')
if(!exists("homepage")){
	load('data/homepage.Rdata')
}


# create complication data frame from homepage 
cols=c("P321","P322","P324","P325","P327","P328","P3291","P3292",
 "P3294","P3295","P3297","P3298","P3284","P3285","P3287","P3288","P3271",
"P3272","P3274","P3275")

dignostics=homepage[,cols]
dignostics=dignostics[,1:12]
dig.codes=dignostics[,c(1,3,5,7,9,11)]
dig.desc=dignostics[,c(2,4,6,8,10,12)]

dig.codes.rf=data.frame()
for(i in 1:5){
	for(j in (i+1):6){
		tmp=dig.codes[,c(i,j)]
		colnames(tmp)=c('dis1','dis2')
		dig.codes.rf=rbind(dig.codes.rf,tmp)

	}
}

dig.desc.rf=data.frame()
for(i in 1:5){
	for(j in (i+1):6){
		tmp=dig.desc[,c(i,j)]
		colnames(tmp)=c('dis1','dis2')
		dig.desc.rf=rbind(dig.desc.rf,tmp)

	}
}

# remove those without complication
idx1=with(dig.codes.rf,which(dis1=='NULL'|dis2=='NULL'|dis1==""|dis2==""))
idx2=with(dig.desc.rf,which(dis1=='NULL'|dis2=='NULL'|dis1==""|dis2==""))

dig.codes.rf=dig.codes.rf[-idx1,]
dig.desc.rf=dig.desc.rf[-idx2,]

# correct the ICD10 codes
dig.codes.rf$dis1=sub(" {1,15}$","",dig.codes.rf$dis1)
dig.codes.rf$dis2=sub(" {1,15}$","",dig.codes.rf$dis2)
dig.codes.rf$dis1=sub(' {1,5}0','.x0',dig.codes.rf$dis1)
dig.codes.rf$dis2=sub(' {1,5}1','.x1',dig.codes.rf$dis2)
dig.codes.rf$dis1Cat=substring(dig.codes.rf$dis1,1,3)
dig.codes.rf$dis2Cat=substring(dig.codes.rf$dis2,1,3)
code2desc=data.frame(dis=with(dig.desc.rf,c(dis1,dis2)),code=with(dig.codes.rf,c(dis1,dis2)))
code2desc=code2desc[!duplicated(code2desc),]

source("scripts/code2DiseaseCategory.R",encoding="utf-8")

lev1=formatD(icd10Cat)$lev1
tmp=lapply(strsplit(lev1,"："),function(x)x[1])
tmp=lapply(tmp,function(x)strsplit(x,"-"))
tmp=lapply(tmp,function(x)x[[1]])
tmp=lapply(tmp,codeSeq)
lev1=data.frame(code=unlist(tmp),dis=rep(lev1,unlist(lapply(tmp,length))))

createNetwork=function(sel.codes,occurN){
	idx=unique(c(which(dig.codes.rf$dis1Cat %in% sel.codes),which(dig.codes.rf$dis2Cat %in% sel.codes)))
	s=dig.codes.rf[idx,c("dis1","dis2")]
	#idx=match(nodes$dis,code2desc$dis)
	#nodes$icd10=code2desc$code[idx]
	#nodes=cbind(id=code2desc$id[idx],nodes)
	edges=as.data.frame(table(with(s,paste(dis1,dis2,sep=";"))),stringsAsFactors=FALSE)
	edges=cbind(edges,as.data.frame(do.call(rbind,strsplit(as.character(edges$Var1),";"))))
	edges=edges[,c("V1","V2","Freq")]
	colnames(edges)=c("dis1","dis2","Count")
	edges=edges[edges$Count>=occurN,]
	nodes=as.data.frame(table(with(edges,c(as.character(dis1),as.character(dis2)))))
	colnames(nodes)=c("dis","freq")
	nodes$type=lev1$dis[match(substring(nodes$dis,1,3),lev1$code)]
	nodes$dis=code2desc$dis[match(nodes$dis,code2desc$code)]
	nodes=cbind(idn=factor(nodes$dis,levels=nodes$dis),nodes)
	edges=cbind(with(code2desc,data.frame(from=dis[match(edges$dis1,code)],to=dis[match(edges$dis2,code)])),edges)
	edges=data.frame(from=as.numeric(factor(edges$from)),
	to=as.numeric(factor(edges$to)),weight=log(edges$Count)+1)
	net=list(nodes=nodes,edges=edges)
	return(net)

}


###
