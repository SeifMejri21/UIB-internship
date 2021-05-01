
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

##############3"MÃ©tiers de la banque

###Finance
df_finance<- data1[,c(32:36,115)]

pca_finance<-PCA(df_finance[1:5],graph = F)
round(pca_finance$var$cos2,3)
#Carte des individus
fviz_pca_ind(pca_finance,
             geom.ind = "point",
             col.ind = data1$Attribution, 
             palette = c("#00B0F6","#00BF7D","#F8766D","#A3A500","#E76BF3"),
             addEllipses = F, # 
             legend.title = "Groups")

df_finance<-as.data.frame(df_finance)

df_finance[1:5] <- lapply(df_finance[1:5], factor, levels=1:5 )
liktA1 <- likert(df_finance[,c(1:5)])
plot(liktA1)

df_finance[1:5] <- lapply(df_finance[1:5], factor, levels=1:5 )
liktA2 <- likert(df_finance[,c(1:5)], grouping= df_finance$Attribution)
plot(liktA2)



###Controle et conformitÃ©
df_controle_et_conformitÃ©<- data1[,37:39]

pca_controle_et_conformitÃ©<-PCA(df_controle_et_conformitÃ©,graph = F)
round(pca_controle_et_conformitÃ©$var$cos2,3)
#Carte des individus
fviz_pca_ind(pca_controle_et_conformitÃ©,
             geom.ind = "point",
             col.ind = data1$Attribution, 
             palette = c("#00B0F6","#00BF7D","#F8766D","#A3A500","#E76BF3"),
             addEllipses = F, # 
             legend.title = "Groups")


###Marketing
df_Marketing<- data1[,40:43]

pca_Marketing<-PCA(df_Marketing,graph = F)
round(pca_Marketing$var$cos2,3)
#Carte des individus
fviz_pca_ind(pca_Marketing,
             geom.ind = "point",
             col.ind = data1$Attribution, 
             palette = c("#00B0F6","#00BF7D","#F8766D","#A3A500","#E76BF3"),
             addEllipses = F, # 
             legend.title = "Groups")


### Risque
df_risque<- data1[,44:48]

pca_risque<-PCA(df_risque,graph = F)
round(pca_risque$var$cos2,3)
#Carte des individus
fviz_pca_ind(pca_risque,
             geom.ind = "point",
             col.ind = data1$Attribution, 
             palette = c("#00B0F6","#00BF7D","#F8766D","#A3A500","#E76BF3"),
             addEllipses = F, # 
             legend.title = "Groups")


###Commercial
df_commercial<- data1[,49:52]
pca_commercial<-PCA(df_risque,graph = F)
round(pca_commercial$var$cos2,3)
#Carte des individus
fviz_pca_ind(pca_commercial,
             geom.ind = "point",
             col.ind = data1$Attribution, 
             palette = c("#00B0F6","#00BF7D","#F8766D","#A3A500","#E76BF3"),
             addEllipses = F, # 
             legend.title = "Groups")


### Back-office
df_back_office<- data1[,53:59]
pca_back_office<-PCA(df_back_office,graph = F)
round(pca_back_office$var$cos2,3)
#Carte des individus
fviz_pca_ind(pca_back_office,
             geom.ind = "point",
             col.ind = data1$Attribution, 
             palette = c("#00B0F6","#00BF7D","#F8766D","#A3A500","#E76BF3"),
             addEllipses = F, # 
             legend.title = "Groups")


### Support
df_support<- data1[,61:67]
pca_support<-PCA(df_support,graph = F)
round(pca_support$var$cos2,3)
#Carte des individus
fviz_pca_ind(pca_support,
             geom.ind = "point",
             col.ind = data1$Attribution, 
             palette = c("#00B0F6","#00BF7D","#F8766D","#A3A500","#E76BF3"),
             addEllipses = F, # 
             legend.title = "Groups")


###############################################################################
###############################################################################
###############################################################################

#################################################################
#################################################################
###Finance

df_finance<- data1[,c(32:36,115)]
df_finance<-as.data.frame(df_finance)
df_finance[1:5] <- lapply(df_finance[1:5], factor, levels=1:5 )

likta1 <- likert(df_finance[,c(1:5)])
likert.bar.plot(likta1)
likert.heat.plot(likta1)

likta2 <- likert(df_finance[,c(1:5)], grouping = df_finance$Attribution)
plot(likta2)

######################

#################################################################
#################################################################
###Controle et conformitÃ©

df_controle_et_conformitÃ©<- data1[,c(37:39,115)]
df_controle_et_conformitÃ©<-as.data.frame(df_controle_et_conformitÃ©)
df_controle_et_conformitÃ©[1:3] <- lapply(df_controle_et_conformitÃ©[1:3], factor, levels=1:5 )

liktb1 <- likert(df_controle_et_conformitÃ©[,c(1:3)])
likert.bar.plot(liktb1)

liktb2 <- likert(df_controle_et_conformitÃ©[,c(1:3)], grouping =df_controle_et_conformitÃ©$Attribution)
plot(liktb2)

#################################################################
#################################################################
###Marketing

df_Marketing<- data1[,c(40:43,115)]
df_Marketing<-as.data.frame(df_Marketing)
df_Marketing[1:4] <- lapply(df_Marketing[1:4], factor, levels=1:5 )

liktc1 <- likert(df_Marketing[,c(1:4)])
likert.bar.plot(liktc1)

liktc2 <- likert(df_Marketing[,c(1:4)], grouping = df_Marketing$Attribution)
plot(liktc2)

#################################################################
#################################################################
### Risque

df_risque<- data1[,c(44:48,115)]
df_risque<-as.data.frame(df_risque)
df_risque[1:5] <- lapply(df_risque[1:5], factor, levels=1:5 )

liktd1 <- likert(df_risque[,c(1:5)])
likert.bar.plot(liktd1)

liktd2 <- likert(df_risque[,c(1:5)], grouping=df_risque$Attribution)
plot(liktd2)
#################################################################
#################################################################
###Commercial

df_commercial<- data1[,c(49:52,115)]
df_commercial<-as.data.frame(df_commercial)
df_commercial[1:4] <- lapply(df_commercial[1:4], factor, levels=1:5 )

likte1 <- likert(df_commercial[,c(1:4)])
likert.bar.plot(likte1)

likte2 <- likert(df_commercial[,c(1:4)], grouping = df_commercial$Attribution)
plot(likte2)

#################################################################
#################################################################
### Back-office

df_back_office<- data1[,c(53:59,115)]
df_back_office<-as.data.frame(df_back_office)
df_back_office[1:7] <- lapply(df_back_office[1:7], factor, levels=1:5 )

liktf1 <- likert(df_back_office[,c(1:7)])
likert.bar.plot(liktf1)

liktf2 <- likert(df_back_office[,c(1:7)], grouping = df_back_office$Attribution)
plot(liktf2)


#################################################################
#################################################################
### Support

df_support<- data1[,c(61:67,115)]
df_support<-as.data.frame(df_support)
df_support[1:7] <- lapply(df_support[1:7], factor, levels=1:5 )

liktg1 <- likert(df_support[,c(1:7)])
likert.bar.plot(liktg1)

liktg2 <- likert(df_support[,c(1:7)], grouping = df_support$Attribution)
plot(liktg2)



############################Ou bien#####################################
#################################################################
#################################################################
#################################################################
#################################################################

####Extraction de DonnÃ©es

df_finance<- data1[,c(32:36,115)]
df_finance<-as.data.frame(df_finance)
df_finance[1:5] <- lapply(df_finance[1:5], factor, levels=1:5 )

df_controle_et_conformitÃ©<- data1[,c(37:39,115)]
df_controle_et_conformitÃ©<-as.data.frame(df_controle_et_conformitÃ©)
df_controle_et_conformitÃ©[1:3] <- lapply(df_controle_et_conformitÃ©[1:3], factor, levels=1:5 )

df_Marketing<- data1[,c(40:43,115)]
df_Marketing<-as.data.frame(df_Marketing)
df_Marketing[1:4] <- lapply(df_Marketing[1:4], factor, levels=1:5 )

df_risque<- data1[,c(44:48,115)]
df_risque<-as.data.frame(df_risque)
df_risque[1:5] <- lapply(df_risque[1:5], factor, levels=1:5 )

df_commercial<- data1[,c(49:52,115)]
df_commercial<-as.data.frame(df_commercial)
df_commercial[1:4] <- lapply(df_commercial[1:4], factor, levels=1:5 )

df_back_office<- data1[,c(53:59,115)]
df_back_office<-as.data.frame(df_back_office)
df_back_office[1:7] <- lapply(df_back_office[1:7], factor, levels=1:5 )

df_support<- data1[,c(61:67,115)]
df_support<-as.data.frame(df_support)
df_support[1:7] <- lapply(df_support[1:7], factor, levels=1:5 )

####Graph GÃ©nÃ©ral
png(paste("Graph_Finance.png"),width = 700, height = 500)

likta1 <- likert(df_finance[,c(1:5)])
likert.bar.plot(likta1)

dev.off()
png(paste("Graph_controle et conformitÃ©.png"),width = 700, height = 500)

liktb1 <- likert(df_controle_et_conformitÃ©[,c(1:3)])
likert.bar.plot(liktb1)
dev.off()
png(paste("Graph_Marketing.png"),width = 700, height = 500)

liktc1 <- likert(df_Marketing[,c(1:4)])
likert.bar.plot(liktc1)
dev.off()
png(paste("Graph_Rique.png"),width = 700, height = 500)

liktd1 <- likert(df_risque[,c(1:5)])
likert.bar.plot(liktd1)
dev.off()
png(paste("Graph_Commercial.png"),width = 700, height = 500)

likte1 <- likert(df_commercial[,c(1:4)])
likert.bar.plot(likte1)
dev.off()
png(paste("Graph_Back_office.png"),width = 700, height = 500)

liktf1 <- likert(df_back_office[,c(1:7)])
likert.bar.plot(liktf1)
dev.off()
png(paste("Graph_Support.png"),width = 700, height = 500)

liktg1 <- likert(df_support[,c(1:7)])
likert.bar.plot(liktg1)
dev.off()

###############################################

likta1 <- likert(df_finance[,c(1:5)])
likert.bar.plot(likta1,
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

liktb1 <- likert(df_controle_et_conformitÃ©[,c(1:3)])
likert.bar.plot(liktb1,
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

liktc1 <- likert(df_Marketing[,c(1:4)])
likert.bar.plot(liktc1,
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

liktd1 <- likert(df_risque[,c(1:5)])
likert.bar.plot(liktd1,
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

likte1 <- likert(df_commercial[,c(1:4)])
likert.bar.plot(likte1,
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

liktf1 <- likert(df_back_office[,c(1:7)])
likert.bar.plot(liktf1,
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

liktg1 <- likert(df_support[,c(1:7)])
likert.bar.plot(liktg1,
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


###############################################


####Graph Par atrribution


pdf("Graphes connaissances Business.png")

likta2 <- likert(df_finance[,c(1:5)], grouping = df_finance$Attribution)
plot(likta2,
     low.color = "#2c7fb8",
     high.color = "#E60028",
     neutral.color = "grey90",
     plot.percent.low = TRUE,
     plot.percent.high = TRUE,
     plot.percent.neutral = TRUE,
     plot.percents = TRUE,
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
     panel.strip.color = "grey")

liktb2 <- likert(df_controle_et_conformitÃ©[,c(1:3)], grouping =df_controle_et_conformitÃ©$Attribution)
plot(liktb2,
     low.color = "#2c7fb8",
     high.color = "#E60028",
     neutral.color = "grey90",
     plot.percent.low = TRUE,
     plot.percent.high = TRUE,
     plot.percent.neutral = TRUE,
     plot.percents = TRUE,
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
     panel.strip.color = "grey")

liktc2 <- likert(df_Marketing[,c(1:4)], grouping = df_Marketing$Attribution)
plot(liktc2,
     low.color = "#2c7fb8",
     high.color = "#E60028",
     neutral.color = "grey90",
     plot.percent.low = TRUE,
     plot.percent.high = TRUE,
     plot.percent.neutral = TRUE,
     plot.percents = TRUE,
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
     panel.strip.color = "grey")

liktd2 <- likert(df_risque[,c(1:5)], grouping=df_risque$Attribution)
plot(liktd2,
     low.color = "#2c7fb8",
     high.color = "#E60028",
     neutral.color = "grey90",
     plot.percent.low = TRUE,
     plot.percent.high = TRUE,
     plot.percent.neutral = TRUE,
     plot.percents = TRUE,
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
     panel.strip.color = "grey")

likte2 <- likert(df_commercial[,c(1:4)], grouping = df_commercial$Attribution)
plot(likte2,
     low.color = "#2c7fb8",
     high.color = "#E60028",
     neutral.color = "grey90",
     plot.percent.low = TRUE,
     plot.percent.high = TRUE,
     plot.percent.neutral = TRUE,
     plot.percents = TRUE,
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
     panel.strip.color = "grey")

liktf2 <- likert(df_back_office[,c(1:7)], grouping = df_back_office$Attribution)
plot(liktf2,
     low.color = "#2c7fb8",
     high.color = "#E60028",
     neutral.color = "grey90",
     plot.percent.low = TRUE,
     plot.percent.high = TRUE,
     plot.percent.neutral = TRUE,
     plot.percents = TRUE,
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
     panel.strip.color = "grey")

liktg2 <- likert(df_support[,c(1:7)], grouping = df_support$Attribution)
plot(liktg2,
     low.color = "#2c7fb8",
     high.color = "#E60028",
     neutral.color = "grey90",
     plot.percent.low = TRUE,
     plot.percent.high = TRUE,
     plot.percent.neutral = TRUE,
     plot.percents = TRUE,
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
     panel.strip.color = "grey")

dev.off()

#####################################################################################
####Combined Business

df_business <- data1[,c(32:36,37:39,40:43,44:48,49:52,53:59,61:67,115)]
df_business<-as.data.frame(df_business)


df_business[1:35] <- lapply(df_business[1:35], factor, levels=1:5 )


liktg1 <- likert(df_business[,c(1:35)])
likert.bar.plot(liktg1,
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


#####################################################################################
#### Means data & Heatmap 

c("BIAN" , "MOA" , "GAH" , "EDITIQUE" , "ORG")

df_bian <- df_business %>% filter(Attribution == "BIAN" )
df_moa <- df_business %>% filter(Attribution == "MOA" )
df_gah <- df_business %>% filter(Attribution == "GAH" )
df_editique <- df_business %>% filter(Attribution == "EDITIQUE" )
df_org <- df_business %>% filter(Attribution == "ORG" )

bian<-c()
moa<-c()
gah<-c()
editique<-c()
org<-c()

for (i in 1:35) {
        bian[i]<-(mean(df_bian[,i]))
        moa[i]<-(mean(df_moa[,i]))
        gah[i]<-(mean(df_gah[,i]))
        editique[i]<-(mean(df_editique[,i]))
        org[i]<-(mean(df_org[,i]))
}

means_df<-data.frame(rbind(bian,moa,gah,editique,org))



cols<-c("Planification stratÃ©gique","Ãvaluation financiÃ¨re","Pilotage financier",
        "Analyse financiÃ¨res","Reporting et Ã©tats financiers","Reporting et dÃ©clarations",
        "Risque opÃ©rationnel","Audit et Ãvaluation","Marketing stratÃ©gique",
        "Marketing relationnel","Marketing analytique","Marketing opÃ©rationnel",
        "Risque stratÃ©gique","Ãvaluation du risque","Gestion du risque","Analyse du risque",
        "Reporting du risque","Gestion opÃ©rationnelle des forces de vente",
        "Gestion de la performance","Gestion des canaux",
        "Veille concurrentielle et Ã©tude de marchÃ©","Recouvrement","Moyens de paiement",
        "Bancaire Ã©tranger","Engagement","MarchÃ©","Juridique","Autres back-offices",
        "Ressources humaines","Achat","Moyens gÃ©nÃ©raux","Logistique","SÃ©curitÃ©","Informatique",
        "ConformitÃ© et qualitÃ©")

 ################################################################################
data_business <-read_excel("C:\\Users\\TOSHIBA\\Desktop\\EnquÃªte UIB\\finaux\\Nouveau dossier\\Business1.0.xlsx")

data_business<-as.data.frame(data_business)

df_business2 <- data_business[1:7]


###ACP Business
pca_business<-PCA(df_business2,graph = F)
round(pca_business$var$cos2,3)
#Carte des individus
fviz_pca_ind(pca_business,
             geom.ind = "point",
             col.ind = data_business$Attribution, 
             palette = c("#00B0F6","#00BF7D","#F8766D","#A3A500","#E76BF3"),
             addEllipses = F, # 
             legend.title = "Groups")

################################################################################

data_business3 <-read_excel("C:\\Users\\TOSHIBA\\Desktop\\EnquÃªte UIB\\finaux\\Nouveau dossier\\Business4.0.xlsx")
data_business3<-as.data.frame(data_business3)

rownames(df_general)<-c("1","2","3","4","5")

df_general<-data_business3[,2:8]
df_MOA1<-data_business3[,9:15]
df_GAH1<-data_business3[,16:22]
df_BIAN1<-data_business3[,23:29]
df_EDITIQUE1<-data_business3[,30:36]
df_ORG1<-data_business3[,37:43]

View(df_MOA1)
View(df_GAH1)
View(df_BIAN1)
View(df_EDITIQUE1)
View(df_ORG1)





library(gplots)
# Import data and prepare chart
data(Titanic)
myData <- as.data.frame(Titanic) # convert to 1 entry per row &
format
attach(myData)
myColours<-Titanic
myColours[,,,"Yes"]<-"LightSkyBlue"
myColours[,,,"No"]<-"plum1"
myColours<-as.character(as.data.frame(myColours)$Freq)

# Create chart
balloonplot(x=list(Age,Sex),main="",
            y=list(Class=Class,
            Survived=gdata::reorder.factor(Survived,new.order=c(2,1))),
            z=Freq,dotsize=18,
            zlab="Number of Passengers",
            sort=T,
            dotcol=myColours,
            show.zeros=T,
            show.margins=T)
# Titling
mtext("Titanic â Passenger and Crew Statistics",3,line=0,adj=0,
              cex=2,family="Lato Black",outer=T)
mtext("Balloon Plot for Age, Sex by Class, Survived",3,line= -2,
              adj=0,cex=1.25,font=3,outer=T)
mtext("Source: R library gplots",1,line=1,adj=1.0,cex=1.25,font = 3,outer=T)
mtext("Area is proportional to Number of Passengers",1,line=1,
              adj=0,cex=1.25,font=3,outer=T)


###############################################################################
data_business5 <-read_excel("C:\\Users\\TOSHIBA\\Desktop\\EnquÃªte UIB\\finaux\\Nouveau dossier\\Business5.0.xlsx")
data_business5<-as.data.frame(data_business5)

df10<-data_business5[,2:8]
rownames(df10)<-c("1","2","3","4","5")

my_cols<-sequential("#E20028")

ggballoonplot(t(round(df10,2)),
                size.range = c(1, 20),
                shape= 21,fill = "value" ,
                show.label = TRUE,
                font.label = c(12, "bold", "black"))+
                scale_fill_gradientn(colors = my_cols)






ggballoonplot(t(round(df10,2)),
              size.range = c(1, 30),
              shape= 21,fill = "value" ,
              show.label = TRUE,
              font.label = c(18, "bold", "black"))+
                        scale_fill_gradientn(colors = my_cols)+
                        xlab("Niveau de connaissances ")+
                        ylab("CompÃ©tences")+
                        theme(text = element_text(size = 22))     
















