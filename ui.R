library(shiny)
library(shinydashboard)
library(shinyTree)
library(ggplot2)
library(leaflet)
library(ggvis)
library(plotly)
library(networkD3)
header=dashboardHeader(title="医疗数据分析平台")

sidebar=dashboardSidebar(sidebarMenu(
			menuItem("仪表板",
				menuSubItem("基本信息",tabName="dashboard"),
				menuSubItem("区域来源",tabName="patLoc")),
			menuItem("患者数据分析",
				menuSubItem("时间线",tabName="patTimeline"),
				menuSubItem("检验指标",tabName="LISitem")),
			menuItem("疾病数据分析",
				menuSubItem("疾病数据变化趋势",tabName="disTrend"),
				menuSubItem("疾病网络分析",tabName="disNet"),
				menuSubItem("疾病关联规则分析",tabName="disARules")),
			menuItem("医疗质量分析",tabName="quality"),
			menuItem("费用分析"),tabName=""))


tab.dashboard=tabItem(tabName="dashboard",
			fluidRow(
				valueBoxOutput("hospitalBox"),
				valueBoxOutput("patientBox"),
				valueBoxOutput("diseaseBox")
				),
			fluidRow(
				box(
					title="医院出入院患者变化趋势",
					status="primary",
					solidHeader=TRUE,
					background=NULL,
					collapsible=TRUE,
					height="370px",
					width="20px",
					plotlyOutput("AccessionDischargeTrend",height="200px"),
					sliderInput("ADTyear","选择区间",min=as.Date("2013-01-01"),max=Sys.Date(),value=c(as.Date("2013-01-01"),Sys.Date()))
					),
				box(
					title="入院患者年龄分布",
					status="primary",
					solidHeader=TRUE,
					collapsible=TRUE,
					height="320px",
					width="20px",
					plotlyOutput("AgeDistribution",height="200px"),
					selectInput("ADyear","选择年份","")
				),
				box(
					title="科室treemap",
					status="primary",
					solidHeader=TRUE,
					collapsible=TRUE,
					height="320px",
					width="20px",
					plotOutput("DepartmentTreemap",height="200px"),
					selectInput("DTyear","选择年份",""))

			)
			)

## patient location distributin
tab.map=tabItem(tabName="patLoc",
		leafletOutput("map",height="1100px"))

## patient timeline (data from Physician's Orders)
tab.patient=tabItem(tabName="patTimeline",
	fluidPage(
		sidebarLayout(
			sidebarPanel(
				selectInput("patientId","选择患者ID：",choices=c("356896"))),
			wellPanel(ggvisOutput("timeline"))
	))
	)

## patient LIS 
LIS.data=read.csv("data/LIS534605Res.csv",header=F,stringsAsFactors=F) 
tab.LIS=tabItem(tabName="LISitem",
	fluidPage(
		sidebarLayout(
			sidebarPanel(
				selectInput("LISpatientId","选择患者ID:",choices=unique(LIS.data$V6)),
				checkboxGroupInput("LIScheckGroup",
					label=h3("检验指标"),
					choices=unique(LIS.data$V4),
					selected=unique(LIS.data$V4)[1]),
					width=2
				),
			mainPanel(
				plotOutput("LISitemPlot",width=1400,height=800))
			)
		)
	)


## disease trend

tab.disTrend=tabItem(tabName="disTrend",
	fluidPage(
		sidebarLayout(
			sidebarPanel(
				tags$head(
					tags$style(type="text/css","jstree-node{max-width:50px;}")
					),
				shinyTree("icd10Tree1",checkbox=TRUE)
				),
			mainPanel(
				plotOutput("selectedDis",height=1000))
			)
		)
	)

## disease network

#tab.disNet=tabItem(tabName="disNet",
	#forceNetworkOutput("disNetGraph"))
#	fluidPage(
#		sidebarLayout(
#			sidebarPanel(
#				tags$head(
#					includeCSS("style.css"),
#					tags$style(HTML( 
#						".jstree-node{overflow:hidden;
#						white-space: nowrap;
#						width:380px;}"
#					))
#					),
#				shinyTree("icd10Tree2",checkbox=TRUE)
#				),
#			mainPanel(forceNetworkOutput("disNetGraph",height=1000)))
#		)
#	)


tab.disNet=tabItem(tabName="disNet",
			
			div(class="outer",
				tags$head(
        # Include our custom CSS
        includeCSS("style.css"),
        includeScript("script.js")
        ),

      wellPanel(forceNetworkOutput("disNetGraph", width="100%", height=1000)),

      # Shiny versions prior to 0.11 should use class="modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",
        numericInput("occurNumber","疾病关联最少次数：",10),
        shinyTree("icd10Tree2",checkbox=TRUE)
        )
      )
			)


tab.disARules=tabItem(tabName="disARules",
			fluidRow(
				box(
					title="TopN频繁项集",
					status="primary",
					solidHeader=TRUE,
					background=NULL,
					collapsible=TRUE,
					height="600px",
					#width="20px",
					plotOutput("itemFreq",height="400px"),
					numericInput("topn","选择TopN频繁项：",10)
					),
				box(
					title="规则散点图",
					status="primary",
					solidHeader=TRUE,
					collapsible=TRUE,
					height="600px",
					#width="20px",
					plotOutput("scatterRules",height="400px"),
					numericInput("support1","支持度:",0.01),
					numericInput("confidence1","置信度:",0.005)
				),
				box(
					title="规则矩阵图",
					status="primary",
					solidHeader=TRUE,
					collapsible=TRUE,
					height="900px",
					#width="20px",
					plotOutput("matrixRules",height="700px"),
					numericInput("support2","支持度:",0.005),
					numericInput("confidence2","置信度:",0.01)
					),
				box(
					title="规则网络图",
					status="primary",
					solidHeader=TRUE,
					collapsible=TRUE,
					height="900px",
					#width="20px",
					plotOutput("graphRules",height="700px"),
					numericInput("support3","支持度:",0.005),
					numericInput("confidence3","置信度:",0.01)
					)
			)
			)	


	
body=dashboardBody(tags$head(tags$title("mAnalytics")),
	tabItems(
		tab.dashboard,
		tab.map,
		tab.patient,
		tab.LIS,
		tab.disTrend,
		tab.disNet,
		tab.disARules
		)
	)

dashboardPage(
	header,
	sidebar,
	body
	)