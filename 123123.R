#Programa para generar graficos proyecto BID----- 
2 #cargar librerias---- 
3 library(reshape) 
4 library(ggplot2) 
5 library(plyr) 
6 library(grid) 
7 library(gridExtra) 
8 library(dplyr) 
9 library(tidyr) 
10 library(broom) 
11 

12 

13 # Definir directorio de trabajo------------- 
14 setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ResultsIMPACT/AggregationBID") 
15 

16 # Direci?n graficos----------------------- 
17 copy<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ResultsIMPACT/AggregationBID/graphs/") 
18 grp<- c("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/ResultsIMPACT/AggregationBID/Phase2/Test/") 
19 

20 #Cargar marco de datos principal 
21 md<-read.csv("Phase2/TestRegionsBID.csv",header=T) 
22 

23 Parameters<- c("TYldXAgg -- Total Yield", "QSXAgg -- Total Production","TAreaXAgg -- Total Area", "QNXAgg -- Net Trade","QDXAgg -- Total Demand") 
24 crops<- c("jbean", "jrice", "jwhea" , "cmaiz", "cs", "jmaiz", "js", "cbean", "crice", "cwhea") 
25 

26 #Hacer un subconjunto que s?lo contenga las variables de mi inter?s y los 5 contenga los cinco cultivos analizados 
27 mdsub<- filter(md, impactparameter %in% Parameters ) %>% filter(., commodity %in% crops) 
28 mdsub$impactparameter<-revalue(mdsub$impactparameter, c("QDXAgg -- Total Demand"="Total Demand", 
                                                           29                                                         "QNXAgg -- Net Trade"="Net Trade", 
                                                           30                                                         "TYldXAgg -- Total Yield"= "Total Yield", 
                                                           31                                                         "QSXAgg -- Total Production"="Total Production", 
                                                           32                                                         "TAreaXAgg -- Total Area"="Total Area")) 
33 

34 mdsub$impactparameter<- as.character(mdsub$impactparameter) 
35 mdsub$scenario<- as.character(mdsub$scenario) 
36 mdsub$commodity<- as.character(mdsub$commodity) 
37 mdsub$region<- as.character(mdsub$region) 
38 mdsub$productiontype<- as.character(mdsub$productiontype) 
39 

40 #Hacer un subconjunto de md que s?lo contenga los paises de LAC 
41 alc<- mdsub 
42 alc<- alc %>% spread(year, Val) 
43 alc<- alc[,-c(6:20)] 
44 alc$Percentage_Change<-((alc$`2050`- alc$`2020`)/alc$`2020`)*100 
45 

46 

47 alc$commodity<-revalue(alc$commodity, c("cbean"= "Bean", 
                                           48                                         "cmaiz"="Maize", 
                                           49                                         "crice"="Rice", 
                                           50                                         "cs"="Soybean", 
                                           51                                         "cwhea"="Wheat", 
                                           52                                         "jbean"="Bean", 
                                           53                                         "jmaiz"="Maize", 
                                           54                                         "jrice"="Rice", 
                                           55                                         "js"="Soybean", 
                                           56                                         "jwhea"="Wheat")) 
57 

58 alc<- data.frame(alc,"Cat"=ifelse(alc$scenario=="NoCC","NoCC","CC")) 
59 

60 

61 #Mediana de los cambios porcentuales por categorias. 
62 anal_data<- alc[,-c(6:36)] 
63 

64 anal_datag<- aggregate(anal_data[,"Percentage_Change"], 
                          65                        by=list(anal_data$region,anal_data$impactparameter,anal_data$commodity 
                                                            