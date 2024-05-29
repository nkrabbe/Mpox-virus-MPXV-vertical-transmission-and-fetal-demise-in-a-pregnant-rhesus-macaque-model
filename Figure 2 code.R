#mpox data for manuscript and presentation

list.files() 

install.packages("tidyverse")
install.packages("readxl")
install.packages("tibble")
install.packages("dplyr")
install.packages("ggplot")
install.packages("writexl")
install.packages("palmerpenguins")
install.packages("devtools")
install.packages("ggthemr")
install.packages("Polychrome")
install.packages("stringr")
install.packages("ggbreak")
install.packages("forcats")
install.packages("scales")
install.packages("glue")
install.packages("ggtext")
install.packages("cowplot")
install.packages("grid")
install.packages("gridExtra")
install.packages("ggpubr")
update.packages()
library(readxl)
library(dplyr)
library(ggplot2)
library(writexl)
library(tidyverse)
library(stringr)
library(ggbreak) 
library(forcats)
library(scales)
library(glue)
library(ggtext)
library(cowplot)
library(grid)
library(gridExtra)
library(ggpubr)


fluidVL <- read_excel("C:\\Users\\Nkrab\\Downloads\\Figure 2 Raw Data.xlsx", sheet = 1, col_names = TRUE)
fluidVL$ID <- as.character(fluidVL$ID)
tissueVL <- read_excel("C:\\Users\\Nkrab\\Downloads\\Figure 2 Raw Data.xlsx", sheet = 2, col_names = TRUE)
tissueVL <- tissueVL[!is.na(tissueVL$Tissue),]
tissueVL$ID <- as.character(tissueVL$ID)





#graph the body fluid viral loads


fluidVLfetal <- fluidVL[fluidVL$Source == "Fetal",]
fluidVLMFI <- fluidVL[fluidVL$Source == "MFI",]
fluidVLmaternal <- fluidVL[fluidVL$Source == "Maternal",]

fluidVLfetalgraph <- ggplot(data = fluidVLfetal, aes(x=`Body fluid`, y=`Viral DNA copies/ml`, group=`ID`, fill=`ID`))+
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.8,color="black")+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))+
  theme_classic()+
  scale_x_discrete(labels = label_wrap(10)) +
  theme(legend.position = "none")+
  scale_color_manual(values=c("101"="#374e55","102"="#79af97","103"="#54083E"))+
  scale_fill_manual(values=c("101"="#374e55","102"="#79af97","103"="#54083E"))
fluidVLfetalgraph


ggsave(filename="fludVLfetalgraph.jpg", plot=fluidVLfetalgraph, 
       width = 16, height = 16, units = "cm", dpi = 300)



fluidVLMFIgraph <- ggplot(data = fluidVLMFI, aes(x=`Body fluid`, y=`Viral DNA copies/ml`, group=`ID`, fill=`ID`))+
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.8,color="black")+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))+
  theme_classic()+
  scale_x_discrete(labels = label_wrap(10)) +
  theme(legend.position = "none")+
  scale_color_manual(values=c("101"="#374e55","102"="#79af97","103"="#54083E"))+
  scale_fill_manual(values=c("101"="#374e55","102"="#79af97","103"="#54083E"))
fluidVLMFIgraph
ggsave(filename="fluidVLMFI.jpg", plot=fluidVLMFIgraph, 
       width = 16, height = 16, units = "cm", dpi = 300)


fluidVLmaternalgraph <- ggplot(data = fluidVLmaternal, aes(x=`Body fluid`, y=`Viral DNA copies/ml`, group=`ID`, fill=`ID`))+
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.8,color="black")+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))+
  theme_classic()+
  scale_x_discrete(labels = label_wrap(10)) +
  theme(legend.position = "none")+
  scale_color_manual(values=c("101"="#374e55","102"="#79af97","103"="#54083E"))+
  scale_fill_manual(values=c("101"="#374e55","102"="#79af97","103"="#54083E"))
fluidVLmaternalgraph
ggsave(filename="fluidVLmaternal.jpg", plot=fluidVLmaternalgraph, 
       width = 16, height = 16, units = "cm", dpi = 300)


fluidVLgraph <- ggplot(data = fluidVL, aes(x=`Body fluid`, y=`Viral DNA copies/ml`, group=`ID`, fill=`ID`))+
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.8,color="black")+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)), limits=c(1,1000000000))+
  theme_classic()+
  theme(axis.title.x = element_text(size=18, face="bold"),
        axis.title.y = element_text(size=18, face="bold"),
        axis.text.x = element_text(size=10, hjust=1),
        axis.text.y = element_text(size = 10))+
  theme(legend.position = "none")+
  scale_color_manual(values=c("101"="#374e55","102"="#79af97","103"="#54083E"))+
  scale_fill_manual(values=c("101"="#374e55","102"="#79af97","103"="#54083E"))+
  facet_wrap(. ~ Source, scales="free", dir="v")+
  theme(strip.background = element_rect(color="black", fill="grey", size=1.5, linetype="solid"))+
  theme(strip.text.x = element_text(size = 14, color = "black", face = "bold"))
fluidVLgraph
ggsave(filename="fluidVLgraph.jpg", plot=fluidVLgraph, 
       width = 16, height = 16, units = "cm", dpi = 300)


#graph the tissue viral loads
tissueVL <- read_excel("C:\\Users\\Nkrab\\Downloads\\Figure 2 Raw Data.xlsx", sheet = 2, col_names = TRUE)
tissueVL <- tissueVL[!is.na(tissueVL$Tissue),]
tissueVL <- tissueVL[!is.na(tissueVL$Source),]
tissueVL$ID <- as.character(tissueVL$ID)
tissueVLfetal <- tissueVL[tissueVL$Source == "Fetal",]
tissueVLMFI <- tissueVL[tissueVL$Source == "MFI",]
tissueVLmaternal <- tissueVL[tissueVL$Source == "Maternal",]


tissueVLfetalgraph <- ggplot(data = tissueVLfetal, aes(x=`Tissue`, y=`Viral DNA copies/mg`, group=`ID`, fill=`ID`))+
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.8,color="black")+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))+
  theme_classic()+
  theme(legend.position = "none")+
  scale_color_manual(values=c("101"="#374e55","102"="#79af97","103"="#54083E"))+
  scale_fill_manual(values=c("101"="#374e55","102"="#79af97","103"="#54083E"))
tissueVLfetalgraph
ggsave(filename="TissuefetalVLgraph.jpg", plot=tissueVLfetalgraph, 
       width = 32, height = 16, units = "cm", dpi = 300)


tissueVLmaternalgraph <- ggplot(data = tissueVLmaternal, aes(x=`Tissue`, y=`Viral DNA copies/mg`, group=`ID`, fill=`ID`))+
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.8,color="black")+
  geom_errorbar(aes(x=`Tissue`, ymin = `Viral DNA copies/mg`, ymax=`Viral copies + SD`), width=0.5, alpha=1, size=0.5, position = position_dodge(0.8, preserve = "single"))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))+
  theme_classic()+
  scale_x_discrete(labels = label_wrap(10)) +
  theme(legend.position = "none")+
  scale_color_manual(values=c("101"="#374e55","102"="#79af97","103"="#54083E"))+
  scale_fill_manual(values=c("101"="#374e55","102"="#79af97","103"="#54083E"))
tissueVLmaternalgraph
ggsave(filename="TissuematernalVLgraph.jpg", plot=tissueVLmaternalgraph, 
       width = 32, height = 16, units = "cm", dpi = 300)


tissueVLMFIgraph <- ggplot(data = tissueVLMFI, aes(group=`ID`))+
  geom_bar(aes(x=`Tissue`, y=`Viral DNA copies/mg`, fill=`ID`), stat = "identity", position = position_dodge(preserve = "single"), width = 0.8, color="black")+
  geom_errorbar(aes(x=`Tissue`, ymin = `Viral DNA copies/mg`, ymax=`Viral copies + SD`), width=0.5, alpha=1, size=0.5, position = position_dodge(0.8, preserve = "single"))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))+
  theme_classic()+
  scale_x_discrete(labels = label_wrap(10)) +
  theme(legend.position = "none")+
  scale_color_manual(values=c("101"="#374e55","102"="#79af97","103"="#54083E"))+
  scale_fill_manual(values=c("101"="#374e55","102"="#79af97","103"="#54083E"))
tissueVLMFIgraph 
ggsave(filename="TissueMFIVLgraph.jpg", plot=tissueVLMFIgraph, 
       width = 32, height = 16, units = "cm", dpi = 300)

############Make faceted graphs of all fluids and tissues
tissueVLgraph <- ggplot(data = tissueVL, aes(x=`Tissue`, y=`Viral DNA copies/mg`, group=`ID`, fill=`ID`))+
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.8,color="black")+
  geom_errorbar(aes(x=`Tissue`, ymin = `Viral DNA copies/mg`, ymax=`Viral copies + SD`), width=0.5, alpha=1, linewidth=0.5, position = position_dodge(0.8, preserve = "single"))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)), limits=c(1,10000000000))+
  theme_classic()+
  theme(axis.title.x = element_text(size=12, face="bold"),
        axis.title.y = element_text(size=12, face="bold"),
        axis.text.x = element_text(size=6, angle = 45, hjust=1),
        axis.text.y = element_text(size = 6))+
  theme(legend.position = "none")+
  scale_color_manual(values=c("101"="#374e55","102"="#79af97","103"="#54083E"))+
  scale_fill_manual(values=c("101"="#374e55","102"="#79af97","103"="#54083E"))+
  facet_wrap(. ~ Source, scales="free", dir="v")+
  theme(strip.background = element_rect(color="black", fill="grey", size=1.5, linetype="solid"))+
  theme(strip.text.x = element_text(size = 10, color = "black", face = "bold"))
tissueVLgraph
ggsave(filename="TissueVLgraph.jpg", plot=tissueVLgraph, 
       width = 32, height = 32, units = "cm", dpi = 300)

fluidVLgraph <- ggplot(data = fluidVL, aes(x=`Body fluid`, y=`Viral DNA copies/ml`, group=`ID`, fill=`ID`))+
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.8,color="black")+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)), limits=c(1,10000000000))+
  theme_classic()+
  theme(axis.title.x = element_text(size=12, face="bold"),
        axis.title.y = element_text(size=12, face="bold"),
        axis.text.x = element_text(size=6, angle = 45, hjust=1),
        axis.text.y = element_text(size = 6))+
  theme(legend.position = "none")+
  scale_color_manual(values=c("101"="#374e55","102"="#79af97","103"="#54083E"))+
  scale_fill_manual(values=c("101"="#374e55","102"="#79af97","103"="#54083E"))+
  facet_wrap(. ~ Source, scales="free", dir="v")+
  theme(strip.background = element_rect(color="black", fill="grey", size=1.5, linetype="solid"))+
  theme(strip.text.x = element_text(size = 10, color = "black", face = "bold"))
fluidVLgraph
ggsave(filename="fluidVLgraph.jpg", plot=fluidVLgraph, 
       width = 16, height = 16, units = "cm", dpi = 300)



######################
#ggarrange
combinedVLgraph<- ggarrange(tissueVLgraph, fluidVLgraph, 
          labels = c('A', 'B'),
          ncol=2, 
          common.legend=TRUE,
          widths=c(2.5,1),
          align = "hv")
combinedVLgraph
ggsave(filename="combinedVLgraph.jpg", plot=combinedVLgraph, 
       width = 16, height = 18, units = "cm", dpi = 300)




