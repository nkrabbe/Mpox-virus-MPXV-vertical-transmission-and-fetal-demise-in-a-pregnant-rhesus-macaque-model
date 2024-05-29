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
library(ggimage)
### code for amniotic fluid and placental tissue plaque assays

#placental tissue plaque assays
placenta_titers <- read_excel("C:\\Users\\Nkrab\\Downloads\\Figure S3 Raw Data.xlsx", sheet= 2, col_names= TRUE)
placenta_titers$public_ID = as.character(placenta_titers$public_ID)
placenta_titers$public_ID <- factor(placenta_titers$public_ID, levels= c('101', '102'))
#amniotic fluid plaque assays
af_titers <- read_excel("C:\\Users\\Nkrab\\Downloads\\Figure S3 Raw Data.xlsx", sheet= 1, col_names= TRUE)
af_titers$public_ID = as.character(af_titers$public_ID)
af_titers$public_ID <- factor(af_titers$public_ID, levels= c('101', '102'))
af_titers$viral_titer = as.numeric(af_titers$viral_titer)
#soniction comparison
soni_comparison <- read_excel("C:\\Users\\Nkrab\\Downloads\\Figure S3 Raw Data.xlsx", sheet= 3, col_names= TRUE)
soni_comparison$sonication <- factor(soni_comparison$sonication, levels= c('No Sonication', 'Sonication'))
#placental tissue plaque assay figure
placenta_titers_figure <-
  ggplot() +
  geom_bar(data=placenta_titers, aes(x= public_ID, y= viral_titer_normalized_to_1g, color= public_ID, fill= public_ID), stat = 'identity', width= 0.25) +
  theme_classic() +
  scale_y_log10(breaks= trans_breaks('log10', function(x) 10^x), labels= trans_format('log10', math_format(10^.x)), expand= c(0,0), limits= c(1,10^8), name= 'Placental Tissue Infectious Virus Titers (PFU/g)') +
  scale_x_discrete(name='ID') +
  scale_color_manual(values = c('#374e55', '#79af97'), breaks= c('101', '102')) +
  scale_fill_manual(values = c('#374e55', '#79af97'), breaks= c('101', '102')) +
  guides(color = guide_legend('ID'), fill= guide_legend('ID')) +
  theme(plot.title = element_text(hjust= 0.5, face= 'bold'), aspect.ratio= 1) 
placenta_titers_figure
#af plaque assay figure
af_titers_figure <-
  ggplot() +
  geom_bar(data=af_titers, aes(x= public_ID, y= viral_titer, color= public_ID, fill= public_ID), stat = 'identity', width= 0.25) +
  theme_classic() +
  scale_y_log10(breaks= trans_breaks('log10', function(x) 10^x), labels= trans_format('log10', math_format(10^.x)), expand= c(0,0), limits= c(1,10^8), name= 'Amniotic Fluid Infectious Virus Titers (PFU/mL)') +
  scale_x_discrete(name='ID') +
  scale_color_manual(values = c('#374e55', '#79af97'), breaks= c('101', '102')) +
  scale_fill_manual(values = c('#374e55', '#79af97'), breaks= c('101', '102')) +
  guides(color = guide_legend('ID'), fill= guide_legend('ID')) +
  theme(plot.title = element_text(hjust= 0.5, face= 'bold'), aspect.ratio= 1) +
  annotate("text", x= 1, y= 2, label= 'ND') +
  annotate("text", x=2, y=2, label= 'ND')
af_titers_figure

#sonication comparison figure
soni_comparison_figure <-
  ggplot() +
  geom_bar(data=soni_comparison, aes(x= sonication, y= viral_titer, color= sonication, fill= sonication), stat= 'identity', width= 0.25, alpha= 0.25) +
  theme_classic() +
  scale_x_discrete(name= 'Sonication Status') +
  annotation_logticks(base = 10, sides= 'l', scaled= TRUE, short = unit(-0.5, 'cm'), mid = unit(-0.5, 'cm'), long = unit(-0.5, 'cm')) +
  scale_y_log10(name= 'Skin Lesion Infectious Virus Titers (PFU/mL)', breaks= trans_breaks('log10', function(x) 10^x), labels = trans_format('log10', math_format(10^.x)), expand = c(0,0), limits = c(1,10^6)) +
  scale_color_manual(values = c('#7b0000', '#000088'), breaks= c('No Sonication', 'Sonication')) +
  scale_fill_manual(values = c('#7b0000', '#000088'), breaks= c('No Sonication', 'Sonication')) +
  theme(aspect.ratio = 1, plot.title= element_text(face= 'bold', hjust= 0.5)) + guides(color= 'none', fill= 'none') + 
  geom_image(aes(image = "C:\\Users\\Nkrab\\OneDrive\\Documentos\\Mohr Lab\\skin lesion plaque assay (-soni).jpg", x= sonication, y= viral_titer), data = tibble(sonication = 1, viral_titer= 100000), size= 0.4) +
  geom_image(aes(image = "C:\\Users\\Nkrab\\OneDrive\\Documentos\\Mohr Lab\\skin lesion plaque assay (+soni).jpg", x= sonication, y= viral_titer), data= tibble(sonication= 2, viral_titer= 100000), size= 0.4)
soni_comparison_figure

tiff('Supplemental Figure.tiff', units='in', height=5, width=12, res=300)
ggarrange(placenta_titers_figure, af_titers_figure, soni_comparison_figure, labels = c('A', 'B', 'C'), nrow= 1, ncol= 3, align = 'hv', common.legend = TRUE)
dev.off()
