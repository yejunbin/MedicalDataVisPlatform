library(shiny)
library(ggplot2)
library(reshape2)
library(treemap)
library(plotly)
library(leaflet)
library(ggvis)
library(igraph)
library(network)
library(networkD3)

# dashboard
source("scripts/dashboard.R",encoding="utf-8")

# locations

locations=read.csv("data/location.csv")

# orders

orders=read.csv('data/patient534605Orders.csv',header=T)
orders$id=1:nrow(orders)
orders$starttime=as.POSIXct(orders$starttime)
orders$endtime=as.POSIXct(orders$endtime)

# LIS data
LIS.data=read.csv('data/LIS534605Res.csv',header=FALSE,stringsAsFactors=FALSE)


# Define required server logic

shinyServer(function(input,output,session){

	# create summary box

	output$hospitalBox=renderValueBox({
		valueBox(nhospital,"医院",color="green")
		})
	output$patientBox=renderValueBox({
		valueBox(npatient,"患者",color="purple")
		})
	output$diseaseBox=renderValueBox({
		valueBox(ndisease,"疾病",color="yellow")
		})

	# Accession and Discharge Trend plot
	output$AccessionDischargeTrend=renderPlotly({
		idx=acc_dis$Date>=input$ADTyear[1] & acc_dis$Date<=input$ADTyear[2]
		AD_plot=ggplot(acc_dis[idx,],aes(Date,value,color=variable))
		AD_plot=AD_plot+geom_point(size=2)+geom_line(size=1)
		AD_plot=AD_plot+xlab("")+ylab("数量")
		AD_plot=AD_plot+scale_color_manual("入/出院",labels=c("入院","出院"),values=c("turquoise3","lightcoral"))
		AD_plot+theme(axis.title.x=element_text(vjust=-5))
		})

	# Accession patients by Age groups and gender
	observe({
		years=unique(homepage$year)
		updateSelectInput(session,'ADyear',choices=c('All',years))
		})
	subdata1=subset(homepage[,c("P5","P7","year")])

	output$AgeDistribution=renderPlotly({
		if(input$ADyear=="All"){
		subdata1_1=subdata1[,c("P5","P7")]
	}else{
		subdata1_1=subset(subdata1,year==input$ADyear,select=c("P5","P7"))
	}
		AD_plot2=ggplot(subdata1_1,aes(P7,color=factor(P5)))+geom_histogram(position="dodge",binwidth=3)
		AD_plot2=AD_plot2+xlab("")+ylab("计数")
		AD_plot2=AD_plot2+scale_color_manual("性别",labels=c("男","女","其他"),values=c("turquoise3","lightcoral","white"))	
		AD_plot2+theme(axis.title.x=element_text(vjust=-1))
		})
	output$DepartmentTreemap=renderPlot({
		if(input$DTyear=="All"){
			hp.depart=hp.depart
		}else{
			hp.depart=subset(hp.depart,year=input$DTyear)
		}
		depart=as.data.frame(table(hp.depart$P23))
		colnames(depart)=c("depart","count")
		treemap(depart,index="depart",vSize="count")
		})

	# Creat the map
	output$map=renderLeaflet({
		leaflet(locations) %>% addTiles() %>% setView(lng = 113.1203, lat = 36.20166, zoom = 11) %>% addCircles(lng=~lng,lat=~lat,radius=~freq/10)
		})

	# Create timeline
	lb=linked_brush(keys = 1:nrow(orders), fill="red")

	ttip=function(x){
		with(orders[x$id,],as.character(itemname))
		}
	timeline=ggvis(orders,x=~starttime,y=~type,fill=~type,key:=~id) %>% 
	layer_points(size.brush:=400)%>% 
	hide_legend("fill") %>%
	add_axis("x",title="",format="%m月%d",tick_padding=10,properties=axis_props(
		labels=list(angle=0,fontSize=15))) %>%
	add_axis("y",title="",properties=axis_props(
		labels=list(fontSize=15))) %>%
	set_options(height=800,width=1200) %>%
	lb$input() %>% 
	add_tooltip(ttip)
	bind_shiny(timeline,"timeline")

	# create LIS data views

	source("scripts/multiPlot.R")

	output$LISitemPlot=renderPlot({

		lis.data=LIS.data[grep('^[0-9]',LIS.data$V8),]
		lis.data=lis.data[lis.data$V12!="None",]
		lis.data=lis.data[LIS.data$V6==input$LISpatientId,c("V4","V8","V12","V13")]
		
		lis.data$V12=as.Date(lis.data$V12)
		lis.data$V8=as.numeric(lis.data$V8)
		xmin=min(lis.data$V12)
		xmax=max(lis.data$V12)

		sparklines=function(variable=""){
			p=ggplot(lis.data[lis.data$V4==variable,],aes(x=V12,y=V8))
			if(nrow(lis.data[lis.data$V4==variable,])<2){
				p=p+geom_point(size=4,colour="red")
			}else{
				p=p+geom_line(size=1)+geom_point(size=4,colour="red")
			}
			p+ylab(variable)+xlab("")
		}

		drawCovs=function(cd){
			plots=list()
			plots[[1]]=sparklines(variable=input$LIScheckGroup[1])
			for(i in 2:length(cd$covmat)){
				plots[[i]]=sparklines(variable=input$LIScheckGroup[i])
			}
			multiplot(plotlist=plots)
		}

		cd=list()
		cd$covmat=input$LIScheckGroup
		if (length(input$LIScheckGroup)==1){
			sparklines(variable=input$LIScheckGroup[1])}
		#else if(length(input$LIScheckGroup)==2){}
		else{
			drawCovs(cd)
			}
		},width=1000,height=1000)

	# Disease Trend selected by Category

	source("scripts/code2DiseaseCategory.R",encoding="utf-8")
	tree.list=createTreeList(formatD(icd10Cat))
	dis=homepage[,c("P22","P321")]
	names(dis)=c("Date","Code")
	dis$Date=substring(as.Date(homepage$P22,format="%Y/%m/%d"),1,7)
	dis$Date=as.Date(paste(dis$Date,'-01',sep=""))
	dis$Code=substring(dis$Code,1,3)
	dis=dis[dis$Date>="2013-01-01",]

	# ICD-10 category tree


	output$icd10Tree1=renderTree({
	tree.list$`C00-D48：肿瘤`=structure(tree.list$`C00-D48：肿瘤`,stselected=TRUE)
	tree.list		
	})

    output$selectedDis=renderPlot({
    	selected.nodes=get_selected(input$icd10Tree1,format="name")
    	sel.nodes=unlist(selected.nodes)

    	if(length(sel.nodes)!=0){
    		sel.codes=lapply(strsplit(sapply(strsplit(sel.nodes,"："),function(x)x[1]),"-"),codeSeq)
    		names(sel.codes)=sel.nodes
    		##
    		sel.codes
    		sparklines=function(nodes,title){
    			s.dis=dis[which(dis$Code %in% nodes),]$Date
    			s.dis=as.data.frame(table(s.dis))
    			if(length(s.dis)==1){
    				p=ggplot()
    			}else{
    				colnames(s.dis)=c("Date","Count")
    				s.dis$Date=as.Date(s.dis$Date)
    				p=ggplot(s.dis,aes(x=Date,y=Count))
    				if(nrow(s.dis)<2){
    					p=p+geom_point(size=4,colour="red")
    					}else{
    						p=p+geom_line(size=1)+geom_point(size=4,colour="red")
    					}	
    			}
    			p+ylab("")+xlab("")+labs(title=title)
		}

		if (length(sel.codes)==1){
			sparklines(sel.codes[[1]],names(sel.codes[1]))
		}
		else{
			plots=list()
			for(i in 1:length(sel.codes)){
				plots[[i]]=sparklines(sel.codes[[i]],names(sel.codes)[i])
			}
			multiplot(plotlist=plots)
			}
    		
		}})


    # diease network analysis
    source("scripts/disNetwork.R",encoding="utf-8")
    output$icd10Tree2=renderTree({
	tree.list$`C00-D48：肿瘤`=structure(tree.list$`C00-D48：肿瘤`,stselected=TRUE)
	tree.list		
	})

    output$disNetGraph=renderForceNetwork({
    	selected.nodes=get_selected(input$icd10Tree2,format="name")
    	sel.nodes=unlist(selected.nodes)
    	n=as.numeric(input$occurNumber)
    	if(length(sel.nodes)!=0){
    		sel.codes=lapply(strsplit(sapply(strsplit(sel.nodes,"："),function(x)x[1]),"-"),codeSeq)
    		sel.nodes=unique(unlist(sel.codes))
    		net=createNetwork(sel.nodes,n)
    		nodes=net$nodes
    		edges=net$edges
    		forceNetwork(Links = edges, Nodes = nodes, Source="from", Target="to",
               NodeID = "idn", Group = "type",linkWidth =edges$weight ,
               linkColour = "#afafaf", fontSize=24, zoom=T, legend=T,
               Nodesize=3, opacity = 0.8, charge=-300, 
               width = 500, height = 1000)
    	}
    	})

    #disease association rules

    require("arules")
    require("arulesViz")
    require("RColorBrewer")

    source("scripts/disAssocRules.R")
    output$itemFreq=renderPlot({
    	itemFrequencyPlot(trans, topN=input$topn, horiz=T)
    	})
    output$scatterRules=renderPlot({
    	rules=apriori(trans,parameter=list(support=input$support1,confidence=input$confidence1))
    	plot(rules,control=list(jitter=2,col=rev(brewer.pal(9, "Greens")[4:9])),shading="lift")    
    	})
    output$matrixRules=renderPlot({
    	rules=apriori(trans,parameter=list(support=input$support2,confidence=input$confidence2))
    	plot(rules,method="grouped",control=list(k=100,col = rev(brewer.pal(9,"Greens")[4:9])))  
    	})
    output$graphRules=renderPlot({
    	rules=apriori(trans,parameter=list(support=input$support3,confidence=input$confidence3))
    	plot(rules, measure="confidence", method="graph", control=list(type="items"), shading = "lift")
    	})


})