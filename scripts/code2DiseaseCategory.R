#setwd('C:/Users/junbin/Desktop/医疗数据项目/病历数据/Shiny')

# create ICD10 category dictionary
# find a disease cateogory by code and level


icd10Cat=readLines('../../知识库/ICD10Cat.txt')
formatD=function(icd10Cat){
	idx=regexpr('[A-Z]',icd10Cat)
	idx.lev1=which(idx==5)
	idx.lev2=which(idx==9)
	idx.lev3=which(idx==13)
	idx.lev4=which(idx==17)
	# find category
	cat1=findCat(idx.lev1,idx.lev2,idx)
	cat2=findCat(idx.lev2,idx.lev3,idx)
	cat3=findCat(idx.lev3,idx.lev4,idx)
	cat.df1=data.frame(lev1=icd10Cat[cat1],lev2=icd10Cat[idx.lev2])
	cat.df2=data.frame(lev2=icd10Cat[cat2],lev3=icd10Cat[idx.lev3])
	cat.df3=data.frame(lev3=icd10Cat[cat3],lev4=icd10Cat[idx.lev4])
	cat.df=merge(cat.df1,cat.df2,by.x='lev2',by.y='lev2',all=T)
	cat.df=merge(cat.df,cat.df3,by.x='lev3',by.y='lev3',all=T)
	cat.df=cat.df[,c("lev1","lev2","lev3","lev4")]
	cat.df=apply(cat.df,2,function(x)gsub(" ","",x))
	cat.df=apply(cat.df,2,function(x)ifelse(is.na(x),"",x))
	cat.df=as.data.frame(cat.df,stringsAsFactors=FALSE)
	return(cat.df)
}

createTreeList=function(cat.df){
	list1=with(cat.df,split(lev2,lev1))
	list1=lapply(list1,function(x){cat.df[which(cat.df$lev2 %in% x),c("lev2","lev3")]})
	list2=lapply(list1,function(x)with(x,split(lev3,lev2)))
	list2=lapply(list2,function(x)lapply(x,function(y)cat.df[which(cat.df$lev3 %in% y),c("lev3","lev4")]))
	list3=lapply(list2,function(x)lapply(x,function(y)with(y,split(lev4,lev3))))
	list4=lapply(list3,function(x)lapply(x,function(y)lapply(y,function(z)ifelse(unique(z)=="","",z))))
	for(i in 1:length(list4)){
		for(j in 1:length(list4[[i]])){
			if("" %in% list4[[i]][[j]][[1]]){
				list4[[i]][[j]]=""
				}
			}
	}
	return(list4)
}

findDisByCode=function(code,level){
	cat.df=formatD(icd10Cat)
	# create list map
	lev1.map=codeMap(as.character(unique(cat.df$lev1)))
	lev2.map=codeMap(as.character(unique(cat.df$lev2)))
	lev3.map=codeMap(as.character(unique(cat.df$lev3)))
	lev4.map=codeMap(as.character(unique(cat.df$lev4)))
	lev.map=list(level1=lev1.map,level2=lev2.map,level3=lev3.map,level4=lev4.map)
	lev.map[[paste("level",level,sep="")]][[code]]
}


## find category 
findCat=function(idx.lev1,idx.lev2,idx){
	cat1=vector()
	idx.lev1=c(idx.lev1,length(idx))
	for(i in 1:length(idx.lev2)){
		for(j in 1:(length(idx.lev1)-1)){
			if(idx.lev2[i] %in% idx.lev1[j]:idx.lev1[j+1]){
				cat1[i]=idx.lev1[j]
			}
		}
	}
	return(cat1)
}


# create code sequences: eg:C00-C05 ----> C00,C01,C02,C03,C04,C05
codeSeq=function(x){
	codeseq=vector()
	l=unique(substring(x,1,1))
	n=as.numeric(substring(x,2,3))
	idx=match(l,LETTERS)
	l=LETTERS[idx[1]:(idx[1]+length(idx)-1)]
	if(length(l)==1){
		codeseq=paste(l,substring(formatC(min(n):max(n),digit=2,flag=0),2,3),sep="")
	}else{
		for(i in 1:length(l)){
		if(i==1){
			codeseq=c(codeseq,paste(l[i],substring(formatC(min(n):99,digit=2,flag=0),2,3),sep=""))
		}else if(i==length(l)){
			codeseq=c(codeseq,paste(l[i],substring(formatC(0:max(n),digit=2,flag=0),2,3),sep=""))
		}else{
			codeseq=c(codeseq,paste(l[i],substring(formatC(0:99,digit=2,flag=0),2,3),sep=""))
		}
		}
	}
	return(codeseq)
}

# reverse list 
reverseSplit=function (inList){
	lens = sapply(inList, length)
	nms = rep(names(inList), lens)
	vals = unlist(inList)
	split(nms, vals)
}

# map code to disease category
codeMap=function(lev){
	lev.df=do.call(rbind,strsplit(as.character(lev),"："))
	lev.codes=strsplit(lev.df[,1],"-")
	lev.map=lapply(lev.codes,codeSeq)
	names(lev.map)=lev.df[,2]
	reverseSplit(lev.map)
}


