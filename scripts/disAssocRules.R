library("arules")
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

data=list()
for(i in 1:nrow(dig.desc)){
	c=as.vector(dig.desc[i,])
	c=c[c!="NULL"]
	data[[i]]=c
}

#data=lapply(data,function(x){sub(" {1,5}1",".x1",sub(" {1,5}0",".x0",sub(" {1,15}$","",x)))})
data=lapply(data,function(x)unique(x[x!=""]))

# Association Rules

trans=as(data,"transactions")
freq.sets=eclat(trans,parameter=list(support=0.004,maxlen=10))

#inspect(freq.sets[1:10])
#inspect(sort(freq.sets,by="support")[1:10])

#rules
#rules=apriori(trans,parameter=list(support=0.004,confidence=0.01))
#summary(rules)
#inspect(rules)

# plots

#library("arulesViz")

#plot(rules, method="grouped",control=list(k=100,col = rev(brewer.pal(9, "Greens")[4:9])))  

#plot(rules, measure="confidence", method="graph",   control=list(type="items"), shading = "lift")