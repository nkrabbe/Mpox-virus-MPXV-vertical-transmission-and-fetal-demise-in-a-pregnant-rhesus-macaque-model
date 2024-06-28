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
library(ggpubr)


swabVL <- read_excel("C:\\Users\\Nkrab\\Downloads\\Figure 1 Raw Data.xlsx", sheet = 1, col_names = TRUE)
swabVL$ID <- as.character(swabVL$ID)

lesioncount <- read_excel("C:\\Users\\Nkrab\\Downloads\\Figure 1 Raw Data.xlsx", sheet = 2, col_names = TRUE)
lesioncount$'Days post infection' <- as.numeric(lesioncount$'Days post-inoculation')
lesioncount$'Number of skin lesions' <- as.numeric(lesioncount$'Number of skin lesions')
lesioncount$ID <- as.character(lesioncount$ID)

bloodVL <- read_excel("C:\\Users\\Nkrab\\Downloads\\Figure 1 Raw Data.xlsx", sheet = 3, col_names = TRUE)
bloodVL$ID <- as.character(bloodVL$ID)

weights<- read_excel("C:\\Users\\Nkrab\\Downloads\\Figure 1 Raw Data.xlsx", sheet = 4, col_names = TRUE)
weights$ID <- as.character(weights$ID)

temperatures<- read_excel("C:\\Users\\Nkrab\\Downloads\\Figure 1 Raw Data.xlsx", sheet = 5, col_names = TRUE)
temperatures$ID <- as.character(temperatures$ID)

skinplaqueassays<- read_excel("C:\\Users\\Nkrab\\Downloads\\Figure 1 Raw Data.xlsx", sheet = 6, col_names = TRUE)
skinplaqueassays$ID <- as.character(skinplaqueassays$ID)


#graph the skin lesion counts

Lesioncountgraph <- ggplot(data = lesioncount, aes(x=`Days post-inoculation`, y=`Number of skin lesions`, group=`ID`, color=`ID`))+
  geom_line(size=0.75)+
  geom_point(size =2)+
  scale_y_continuous()+
  theme_classic()+
  scale_color_manual(values=c("101"="#374e55","102"="#79af97","103"="#54083E")) +
  theme(axis.title = element_text(size=11), axis.text = element_text(size=11))
Lesioncountgraph

ggsave(filename="lesioncountgraph.jpg", plot=Lesioncountgraph, 
       width = 16, height = 16, units = "cm", dpi = 300)


#graph the swab viral loads
swabVL$`Days post-inoculation` <- as.factor(swabVL$`Days post-inoculation`)

swabVLgraph <- ggplot(data = swabVL, aes(x=`Days post-inoculation`, y=`Viral DNA copies/ml of media`, fill=`ID`))+
  geom_boxplot(lwd = 1, position = position_dodge2(preserve = "single"), outlier.shape = NA)+
  geom_dotplot(binaxis='y', stackdir='centerwhole', binwidth = 0.1, position = position_dodge2(preserve = "single"))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))+
  scale_x_discrete()+
  xlab("Days post-inoculation")+
  ylab("Skin lesion vDNA titer (copies/ml)")+
  theme_classic()+
  scale_color_manual(values=c("#374e55ff", "#79af97ff"))+
  scale_fill_manual(values=c("#374e55ff", "#79af97ff")) +
  theme(axis.title = element_text(size=11), axis.text = element_text(size=11))
swabVLgraph
ggsave(filename="SwabVLgraph.jpg", plot=swabVLgraph, 
       width = 16, height = 16, units = "cm", dpi = 300)



#graph the blood viral loads

BloodVLgraph <- ggplot(data = bloodVL, aes(x=`Days post-inoculation`, y=`Viral DNA copies/ml whole blood`, group=`ID`, color=`ID`))+
  geom_line(size=0.75)+
  geom_point(size = 2)+
  scale_x_continuous(n.breaks = 8)+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))+
  theme_classic()+
  ylab("Whole blood vDNA titer (copies/ml)")+
  scale_color_manual(values=c("101"="#374e55","102"="#79af97","103"="#54083E")) +
  theme(axis.title = element_text(size=11), axis.text = element_text(size=11))
BloodVLgraph
ggsave(filename="BloodVLgraph.jpg", plot=BloodVLgraph, 
       width = 16, height = 16, units = "cm", dpi = 300)


#graph the skin lesion plaque assays at 7 dpi

skinplaqueassaysgraph <- ggplot(data = skinplaqueassays, aes(x=`ID`, y=`Viral titer (PFU/ml)`, fill=`ID`))+
    geom_boxplot(lwd = 1, position = position_dodge2(preserve = "single"), outlier.shape = NA)+
    geom_dotplot(binaxis='y', stackdir='centerwhole', binwidth = 0.03, position = position_dodge2(preserve = "single"), dotsize=5)+
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))+
    scale_x_discrete()+
    xlab("Animal ID")+
    ylab("Skin lesion viral titer (PFU/ml)")+
    theme_classic()+
    scale_color_manual(values=c("#374e55ff", "#79af97ff"))+
    scale_fill_manual(values=c("#374e55ff", "#79af97ff")) +
    theme(axis.title = element_text(size=11), axis.text = element_text(size=11))
skinplaqueassaysgraph  
ggsave(filename="skinplaqueassaysgraph.jpg", plot=skinplaqueassaysgraph, 
         width = 16, height = 16, units = "cm", dpi = 300)




#graph the weights
weightgraph <- ggplot(data = weights, aes(x=`Days post-inoculation`, y=`Weight (kg)`, group=`ID`, color=`ID`))+
  geom_line(size=0.75)+
  geom_point(size = 2)+
  ylim(8,14)+
  theme_classic()+
  scale_color_manual(values=c("101"="#374e55","102"="#79af97","103"="#54083E")) +
  theme(axis.title = element_text(size=11), axis.text = element_text(size=11))
weightgraph
ggsave(filename="weightgraph.jpg", plot=weightgraph, 
       width = 16, height = 16, units = "cm", dpi = 300)

#graph the temperatures
tempgraph <- ggplot(data = temperatures, aes(x=`Days post-inoculation`, y=`Temperature (°F)`, group=`ID`, color=`ID`))+
  geom_line(size=0.75)+
  geom_point(size = 2)+
  ylim(96, 103)+
  theme_classic()+
  scale_color_manual(values=c("101"="#374e55","102"="#79af97","103"="#54083E"))+
  geom_hline(yintercept=96.8, linetype="dashed", color="grey", size=2)+
  geom_hline(yintercept=102.2, linetype="dashed", color="grey", size=2) +
  theme(axis.title = element_text(size=11), axis.text = element_text(size=11))
tempgraph
ggsave(filename="tempgraph.jpg", plot=tempgraph, 
       width = 16, height = 16, units = "cm", dpi = 300)


######################
#ggarrange
combinedgraph<- ggarrange(BloodVLgraph, Lesioncountgraph, swabVLgraph, skinplaqueassaysgraph, weightgraph, tempgraph, 
                            labels = c('A', 'B', 'C', 'D', 'E', 'F'),
                            nrow=2,
                            ncol=3, 
                            common.legend=TRUE)
                            
ggsave(filename='Figure 1.tiff', plot=combinedgraph, 
       width = 18, height = 17.25, units = "cm", dpi = 600)

