

library(circlize)
library(igraph)
library(ggplot2)
library(dplyr)
library(gapminder)
library(readr)
library(networkD3)
library(readxl)
library(RColorBrewer)
 
######################################################################################

round(prop.table(table(data$Attribution,data$Genre),1),3)*100
table(data$Attribution,data$Genre)

######################################################################################
#####CIRCULAR PLOT

#Extracting data
data<-read_excel("C:\\Users\\Administrator\\Desktop\\ESSAI\\2Ã¨me\\EnquÃªte UIB\\finaux\\Simplified(named).xlsx")

#Extraction des donnÃ©es
data1 = data %>% filter(Attribution == "BIAN" |Attribution == "MOA" |
                          Attribution == "GAH" | Attribution == "EDITIQUE" | Attribution == "ORG")




data2<-read_excel("C:\\Users\\Administrator\\Desktop\\ESSAI\\2Ã¨me\\EnquÃªte UIB\\finaux\\Nouveau dossier\\projets en cours_comptÃ©.xlsx")

data2<- as.data.frame(data2)

data3<-data2[,1:2]

tab <- table(data2$Attribution...2,data2$Projet)

data4<- data2[1:11,2:6]

m<-matrix(data4)

names= c("CBS Amplitude V11","E-TRADE","Cash Management","DIGI-CREDIT","GED V6","RPA",
         "FCCR","E-Signature","Leasing&Factoring ","MERCI V5","Reporting rÃ¨glementaire BCT",
         "GPM","Especes tracker","Gestion des Procurations","Gestion des rÃ©clamations",
         "Tableaux de Bord ","service pack",
         "ParamÃ©trage crÃ©dit","PAYMEE","SP-Engagement ")

names2= c("CBS Amplitude V11","E-TRADE","Cash Management","DIGI-CREDIT","GED V6","RPA",
         "FCCR","E-Signature","Leasing&Factoring ","MERCI V5","Reporting rÃ¨glementaire BCT")


data <- matrix( m, ncol=5)
rownames(data4) <- names2

# Make the circular plot
chordDiagram(data4, transparency = 0.8, big.gap )

######################################################################################

# create data:
links=data.frame(
  source=c("A","A", "A", "A", "A","J", "B", "B", "C", "C", "D","I"),
  target=c("B","B", "C", "D", "J","A","E", "F", "G", "H", "I","I")
)

# Turn it into igraph object
network <- graph_from_data_frame(d=links, directed=F) 

# Count the number of degree for each node:
deg <- degree(network, mode="all")

# Plot
plot(network, vertex.size=deg*6, vertex.color=rgb(0.1,0.7,0.8,0.5) )

######################################################################################
###Bubble plot

# The dataset is provided in the gapminder library
library(gapminder)
data <- gapminder %>% filter(year=="2007") %>% dplyr::select(-year)

# Most basic bubble plot
ggplot(data, aes(x=gdpPercap, y=lifeExp, size = pop)) +
  geom_point(alpha=0.7)



#####################################################################################

###FLOW plot

data5<-read_excel("C:\\Users\\Administrator\\Desktop\\ESSAI\\2Ã¨me\\EnquÃªte UIB\\finaux\\Nouveau dossier\\projets en cours flow.xlsx")
data5<-as.data.frame(data5)


# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(data5$source), as.character(data5$target)) %>% 
    unique()
)
p <- sankeyNetwork(Links = data5, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "valeur", NodeID = "name", 
                   sinksRight=FALSE, fontSize = 18)
p

saveNetwork(p, "file", selfcontained = TRUE)
####################################
data55<-read_excel("C:\\Users\\Administrator\\Desktop\\ESSAI\\2ème\\Enquête UIB\\finaux\\Nouveau dossier\\Charge nov.xlsx")

data55<-

#####################################################################################

sankeyNetwork(Links = links, Nodes = nodes, Source = 'origin_id', Target = 'dest_id', 
              Value = 'count', NodeID = 'name', fontSize = 14, nodeWidth = 20, height = 1000, width = 800)


#####################################################################################
### HEAT map
library(lattice)
library(grid)

data7 <- as.matrix(t(means_df))
heatmap(data7)

coul <- colorRampPalette(brewer.pal(8, "RdPu"))(25)

heatmap(data7, Colv = NA, Rowv = NA, scale = "column",col = coul,
        margins = c(10,5),
        main = "Heatmap des compÃ©tences en mÃ©tiers Business")

legend(x=-2,y=-2, legend=c("min", "ave", "max"), 
       fill=coul)


##############################################################################################

library(ggplot2)



data <- data.frame(
  x= c("ITIL","ISTQB","SCRUM"	,"PMI/PMP","LEAN Management"	),
  y= c(7,10,33,8,4)
)

# Plot
ggplot(data, aes(x=x, y=y)) +
  geom_segment( aes(x=x, xend=x, y=0, yend=y), color="grey") +
  geom_point( color="orange", size=4) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("Value of Y")


ggplot(data, aes(x=x, y=y)) +
  geom_segment( aes(x=x, xend=x, y=0, yend=y)) +
  geom_point( size=15, color="red", fill=alpha("#E60028", 0.3), alpha=0.7, shape=21, stroke=2) +
  ylim(0,47)+
  labs()+  
  ylab("Nbre de rÃ©pondants par Oui ")+
  xlab("Formations")+
  ggtitle("Formations effectuÃ©es")+
  theme(text = element_text(size = 30))     

colnames(data)<-c("Formation","Freq")


ggplot(data, aes(Freq, Formation, label = paste0(round(Freq, 0)))) +
  geom_segment(aes(x = 0, y = Formation, xend = Freq, yend = Formation), color = "grey50") +
  geom_point(size = 15, color="red", fill="#E60028", shape=21, stroke=2) +
  geom_text(color = "white", size = 8.3 )+  
  xlab("Nbre de rÃ©pondants par Oui ")+
  ylab("Formations")+
  ggtitle("Formations effectuÃ©es")+
  theme(text = element_text(size = 22))     

  
###############################################################################

library(treemap)

group <- c(rep("group-1",4),rep("group-2",2),rep("group-3",3))
subgroup <- paste("subgroup" , c(1,2,3,4,1,2,1,2,3), sep="-")
value <- c(13,5,22,12,11,7,3,1,23)
data <- data.frame(group,subgroup,value)

# Custom labels:
treemap(data, index=c("group","subgroup"),     vSize="value", type="index",
        
        fontsize.labels=c(15,12),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
        fontcolor.labels=c("white","orange"),    # Color of labels
        fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
        bg.labels=c("transparent"),              # Background color of labels
        align.labels=list(
          c("center", "center"), 
          c("right", "bottom")
        ),                                   # Where to place labels in the rectangle?
        overlap.labels=0.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
        inflate.labels=F,                        # If true, labels are bigger when rectangle is bigger.
        border.col=c("black","white"),             # Color of borders of groups, of subgroups, of subsubgroups ....
        border.lwds=c(7,2),                         # Width of colors
        palette = "Set1",                        # Select your color palette from the RColorBrewer presets or make your own.
        title="My Treemap",                      # Customize your title
        fontsize.title=12,  
)


#############################################################################################

###RADAR charts

library(fmsb)

data8<-read_excel("C:\\Users\\Administrator\\Desktop\\ESSAI\\2Ã¨me\\EnquÃªte UIB\\finaux\\Nouveau dossier\\means3.0.xlsx")

data<-data8[,2:6]
data<- as.data.frame(data)


rownames(data) <- c("Moyenne","GAH","MOA","BIAN","EDITIQUE","ORG")

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot!
data <- rbind(rep(5,5) , rep(0,5) , data)

# plot with default options:

colors_border=c( rgb(0.2,0.5,0.5,0.9),"#E20028"  ) 
colors_in=c( rgb(0.2,0.5,0.5,0.4), "#FFE1E6" )

#########################___GAH____#######################

radarchart(data[c("1","2","Moyenne","GAH"),],
           seg= 5,
           maxmin = TRUE,
           pcol=colors_border , plwd=6 , plty=1,
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           vlabels = c("Outils et Solutions","Digital","Programmation","Business","IT"),
           vlcex = 1.5,
           centerzero= TRUE
             )

legend(x=1, y=1, legend = c("Moyenne","GAH"), bty = "n", pch=20 ,
       col=colors_border , text.col = "black", cex=1.5, pt.cex=2)

#########################___MOA____#######################

radarchart(data[c("1","2","Moyenne","MOA"),],
           seg= 5,
           maxmin = TRUE,
           pcol=colors_border , plwd=6 , plty=1,
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           vlabels = c("Outils et Solutions","Digital","Programmation","Business","IT"),
           vlcex = 1.5,
           centerzero= TRUE
)

legend(x=1, y=1, legend = c("Moyenne","MOA"), bty = "n", pch=20 ,
       col=colors_border , text.col = "black", cex=1.5, pt.cex=2)


#########################___BIAN____#######################

radarchart(data[c("1","2","Moyenne","BIAN"),],
           maxmin = TRUE,
           seg= 5,
           pcol=colors_border , plwd=6 , plty=1,
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           vlabels = c("Outils et Solutions","Digital","Programmation","Business","IT"),
           vlcex = 1.5,
           centerzero= TRUE
)

legend(x=1, y=1, legend = c("Moyenne","BIAN"), bty = "n", pch=20 ,
       col=colors_border , text.col = "black", cex=1.5, pt.cex=2)

#########################___EDITIQUE____#######################

radarchart(data[c("1","2","Moyenne","EDITIQUE"),],
           maxmin = TRUE,
           seg= 5,
           pcol=colors_border , plwd=6 , plty=1,
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           vlabels = c("Outils et Solutions","Digital","Programmation","Business","IT"),
           vlcex = 1.5,
           centerzero= TRUE
)

legend(x=1, y=1, legend = c("Moyenne","EDITIQUE"), bty = "n", pch=20 ,
       col=colors_border , text.col = "black", cex=1.5, pt.cex=2)


#########################___ORG____#######################

radarchart(data[c("1","2","Moyenne","ORG"),],
           maxmin = TRUE,
           seg= 5,
           pcol=colors_border , plwd=6 , plty=1,
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           vlabels = c("Outils et Solutions","Digital","Programmation","Business","IT"),
           vlcex = 1.5,
           centerzero= TRUE
)

legend(x=1, y=1, legend = c("Moyenne","ORG"), bty = "n", pch=20 ,
       col=colors_border , text.col = "black", cex=1.5, pt.cex=2)

