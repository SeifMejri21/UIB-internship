
library(readr)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(RColorBrewer)
library(questionr)
library(readxl)
library(dplyr)
library(likert) 

data<-read_excel("C:\\Users\\TOSHIBA\\Desktop\\EnquÃªte UIB\\finaux\\Simplified(named).xlsx")

#Extraction des donnÃ©es
data1 = data %>% filter(Attribution == "BIAN" |Attribution == "MOA" |
                          Attribution == "GAH" | Attribution == "EDITIQUE" | Attribution == "ORG")


dfIT<- data1[,c(68:80,115)]

pcaIT<-PCA(dfIT[,1:13],graph = F)

#### interprÃ©tation des axes
round(pcaIT$var$cos2,3)

#Carte des individus
fviz_pca_ind(pcaIT,
             geom.ind = c("point"),
             col.ind = data1$Attribution, 
             palette = c("#00B0F6","#00BF7D","#F8766D","#A3A500","#E76BF3"),
             addEllipses = F, # 
             legend.title = "Groups")


#Pourcentage par attibution

round(prop.table(table(data$Attribution,data$`MOA/Buisness Analyst`),1),3)*100
round(prop.table(table(data$Attribution,data$`MOE`),1),3)*100
round(prop.table(table(data$Attribution,data$`TEST`),1),3)*100
round(prop.table(table(data$Attribution,data$`QUALITE`),1),3)*100
round(prop.table(table(data$Attribution,data$`PMO`),1),3)*100
round(prop.table(table(data$Attribution,data$`Coach Agile`),1),3)*100
round(prop.table(table(data$Attribution,data$`Scrum Master`),1),3)*100
round(prop.table(table(data$Attribution,data$`Architecte IT`),1),3)*100
round(prop.table(table(data$Attribution,data$`Architecte Data`),1),3)*100
round(prop.table(table(data$Attribution,data$`Architecte Processus`),1),3)*100
round(prop.table(table(data$Attribution,data$`Architecte Fonctionnel et Applicatif`),1),3)*100
round(prop.table(table(data$Attribution,data$`Administration de base de donnÃ©es`),1),3)*100
round(prop.table(table(data$Attribution,data$`Extraction de donnÃ©es et reporting`),1),3)*100

###################################################################""

dfIT<-as.data.frame(dfIT)
dfIT[1:13] <- lapply(dfIT[1:13], factor, levels=1:5 )


likt2 <- likert(dfIT[,c(1:13)])

likert.bar.plot(likt2,
                low.color = "#2c7fb8",
                high.color = "#E60028",
                neutral.color = "grey90",
                neutral.color.ramp = "white",
                colors = NULL,
                plot.percent.low = TRUE,
                plot.percent.high = TRUE,
                plot.percent.neutral = TRUE,
                plot.percents = FALSE,
                text.size = 4,
                text.color = "black", 
                centered = TRUE, 
                include.center = TRUE,
                ordered = TRUE,
                wrap = 25,
                wrap.grouping = 100,
                legend = "Niveau de connaissance",
                legend.position = "top",
                panel.arrange = "v",
                panel.strip.color = "#E60028")

plot(likt2,
     low.color = "#2c7fb8",
     high.color = "#E60028",
     text.size = 5,
     legend.position = "top"
     )

likert.heat.plot(likt2,
                 low.color = "white",
                 high.color = "#ad171c",
                 neutral.color = "grey90",
                 neutral.color.ramp = "white",
                 colors = NULL,
                 plot.percent.low = TRUE,
                 plot.percent.high = TRUE,
                 plot.percent.neutral = TRUE,
                 plot.percents = FALSE,
                 text.size = 10,
                 text.color = "black", 
                 centered = TRUE, 
                 include.center = TRUE,
                 ordered = TRUE,
                 wrap = 20,
                 wrap.grouping = 100,
                 legend = "Niveau de connaissance",
                 legend.position = "top",
                 panel.arrange = "v",
                 panel.strip.color = "#f1f1f1")



######################
dfIT[1:13] <- lapply(dfIT[1:13], factor, levels=1:5 )
likt1 <- likert(dfIT[,c(1:13)], grouping = dfIT$Attribution)

plot(likt1,
     low.color = "#2c7fb8",
     high.color = "#E60028",
     neutral.color = "grey90",
     plot.percent.low = TRUE,
     plot.percent.high = TRUE,
     plot.percent.neutral = TRUE,
     plot.percents = TRUE,
     text.size = 3,
     text.color = "black", 
     centered = TRUE, 
     include.center = TRUE,
     ordered = TRUE,
     wrap = 250,
     wrap.grouping = 1000,
     legend = "Niveau de connaissance",
     legend.position = "top",
     panel.arrange = "v",
     panel.strip.color = "grey")
dev.off()


