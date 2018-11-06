#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
#                                                                                   #
#                                COMPARISON SCRIPT                                  #
#                                                                                   #    
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#

setwd(dir = "/Users/simonmorvan/Desktop/UdeM/GIT/ASV-OTU/")
load(file="sample.names.RData")

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
#                                                                                   #
#####                                   Middle                                  #####
#                                                                                   #    
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#

setwd(dir = "/Users/simonmorvan/Desktop/UdeM/GIT/ASV-OTU/Middle/")

list.files()

sample.names <- as.data.frame(sample.names)
sample.names2<- gsub('_', '.S',sample.names$sample.names)

load(file = "M.track.RData")
load(file = "M.seqtab.nochim.con.RData")
load(file = "M.seqtab.nochim.bin.RData")

list.files()

###### 97 % OTUs #####
# M_OTU<-read.delim( "middle_user_otus.txt")
# colnames(M_OTU)<-c("E_val","Seq_ID","OTU_ID","Seq_size","OTU_size","Seq","OTU")
# M_OTU<-as.data.frame(M_OTU)
load("M_OTU_97.RData")

length(unique(M_OTU$OTU_ID)) # Gives the number of OTUs 
# unique retrieves the unique OTUS and length gets the number of unique OTUS
# 7929 unique sequences 

for (i in 1:48) {
  a <- M_OTU[grep(sample.names2[i],M_OTU$Seq_ID),]
  M_track$`# of OTU / sample`[[i]]<- length(unique(a$OTU_ID))
}
M_track
rownames(M_track) <- sample.names2


###### 100 % OTUs #####
load(file="M_OTU_100.RData")
length(unique(M_OTU_100$OTU_ID)) # Gives the number of OTUs 
# unique retrieves the unique OTUS and length gets the number of unique OTUS
# 11481 Unique OTUs 

for (i in 1:48) {
  a <- M_OTU_100[grep(sample.names2[i],M_OTU_100$Seq_ID),]
  M_track$`# of 100% OTU / sample`[[i]]<- length(unique(a$OTU_ID))
}
M_track

colnames(M_track)[which(names(M_track) == "# of OTU / sample")] <- "# of 97% OTU / sample"

save(M_track,file="M.track.RData")

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
#                                                                                   #
#####                                   Stringent                                ####
#                                                                                   #    
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#

setwd(dir = "/Users/simonmorvan/Desktop/UdeM/GIT/ASV-OTU/Stringent/")

list.files()

sample.names <- as.data.frame(sample.names)
sample.names2<- gsub('_', '.S',sample.names$sample.names)
load(file = "S.track.RData")

###### 97 % OTUs #####
load(file="S_OTU_97.RData")

length(unique(S_OTU_97$OTU_ID)) # Gives the number of OTUs 
# unique retrieves the unique OTUS and length gets the number of unique OTUS
# 10367

for (i in 1:48) {
  a <- S_OTU[grep(sample.names2[i],S_OTU$Seq_ID),]
  S_track$`# of OTU / sample`[[i]]<- length(unique(a$OTU_ID))
}
S_track
rownames(S_track) <- sample.names2

###### 100 % OTUs #####
load(file="S_OTU_100.RData")

length(unique(S_OTU_100$OTU_ID)) # Gives the number of OTUs 
# unique retrieves the unique OTUS and length gets the number of unique OTUS
# 13160

for (i in 1:48) {
  a <- S_OTU_100[grep(sample.names2[i],S_OTU_100$Seq_ID),]
  S_track$`# of 100% OTU / sample`[[i]]<- length(unique(a$OTU_ID))
}

colnames(S_track)[which(names(S_track) == "# of OTU / sample")] <- "# of 97% OTU / sample"

S_track

save(S_track,file="S.track.RData")

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
#                                                                                   #
#####                                   Not Stringent                            ####
#                                                                                   #    
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
sample.names <- as.data.frame(sample.names)
sample.names2<- gsub('_', '.S',sample.names$sample.names)

setwd(dir = "/Users/simonmorvan/Desktop/UdeM/GIT/ASV-OTU/NotStringent/")


list.files()

load(file = "NS.track.RData")

###### 97 % OTUs #####
load(file="NS_OTU_97.RData")

length(unique(NS_OTU$OTU_ID)) # Gives the number of OTUs 
# unique retrieves the unique OTUS and length gets the number of unique OTUS
# 9327 OTUs

for (i in 1:48) {
  a <- NS_OTU[grep(sample.names2[i],NS_OTU$Seq_ID),]
  NS_track$`# of OTU / sample`[[i]]<- length(unique(a$OTU_ID))
}
NS_track
rownames(NS_track) <- sample.names2

###### 1OO % OTUs #####
load(file="NS_OTU_100.RData")

length(unique(NS_OTU_100$OTU_ID)) # Gives the number of OTUs 
# unique retrieves the unique OTUS and length gets the number of unique OTUS
# 12608 OTUs

for (i in 1:48) {
  a <- NS_OTU_100[grep(sample.names2[i],NS_OTU_100$Seq_ID),]
  NS_track$`# of 100% OTU / sample`[[i]]<- length(unique(a$OTU_ID))
}

colnames(NS_track)[which(names(NS_track) == "# of OTU / sample")] <- "# of 97% OTU / sample"

NS_track
rownames(NS_track) <- sample.names2


save(NS_track,file="NS.track.RData")


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
#                                                                                   #
#####                                   Abundance                               #####
#                                                                                   #    
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#

setwd(dir ="/Users/simonmorvan/Desktop/UdeM/GIT/ASV-OTU/")
load(file="Total.ab.RData")

library(ggplot2)
library(reshape2)


Total.ab$ID <- rownames(Total.ab)

lgtrack <- melt(Total.ab, id.vars="ID")
bar_track <- ggplot(lgtrack ,aes(x=ID, y=as.numeric(value), fill=variable)) +
  geom_bar(stat="identity", position = "dodge") + 
  theme_classic() + # Thème
  ggtitle("Total unique sequence abundance")+ # Titre principal
  theme(plot.title = element_text(hjust = 0.5))+ # (centres the titles)
  theme(axis.ticks.length=unit(0.1,"cm")) + # Taille de taquets
  theme(axis.text.x = element_text(angle=0))+ # Change l'orientation des  x labels  & supprime le titre de la légende
  scale_x_discrete(name ="Method", limits=rownames(Total.ab))+ # Change le titre de l'axe x et le pas de l'axe
  scale_fill_brewer(name = "Filtering parameters", palette="Accent")+
  scale_y_continuous(name="Abundance", breaks=seq(from = 0, to = 150000, by = 1000)) # Change le titre de l'axe y et le pas de l'axe.
bar_track 



#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
#                                                                                   #
#####                                   Plots                                   #####
#                                                                                   #    
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
library(ggplot2)


setwd(dir = "/Users/simonmorvan/Desktop/UdeM/GIT/ASV-OTU/")
load(file="sample.names.RData")
sample.names <- as.data.frame(sample.names)
sample.names2<- gsub('_', '.S',sample.names$sample.names)


setwd(dir = "/Users/simonmorvan/Desktop/UdeM/GIT/ASV-OTU/NotStringent/")
load("NS.track.RData")

NS_track$`# of ASV / sample` <- as.numeric(NS_track$`# of ASV / sample`)
NS_track$`# of 97% OTU / sample`<- as.numeric(NS_track$`# of 97% OTU / sample`)


setwd(dir = "/Users/simonmorvan/Desktop/UdeM/GIT/ASV-OTU/Stringent/")
load("S.track.RData")

S_track$`# of ASV / sample` <- as.numeric(S_track$`# of ASV / sample`)
S_track$`# of 97% OTU / sample`<- as.numeric(S_track$`# of 97% OTU / sample`)

setwd(dir = "/Users/simonmorvan/Desktop/UdeM/GIT/ASV-OTU/Middle/")
load("M.track.RData")

M_track$`# of ASV / sample` <- as.numeric(M_track$`# of ASV / sample`)
M_track$`# of 97% OTU / sample`<- as.numeric(M_track$`# of 97% OTU / sample`)


ggplot()+
  geom_point(aes(y = sample.names2, x= log(S_track$`# of ASV / sample`)),
             color="darkgreen",shape=15)+
  geom_point(aes(y = sample.names2, x= log(S_track$`# of 97% OTU / sample`)),
             color="blue",shape=15)+
  geom_point(aes(y = sample.names2, x= log(NS_track$`# of ASV / sample`)),
             color="red",shape=17)+
  geom_point(aes(y = sample.names2, x= log(NS_track$`# of 97% OTU / sample`)),
             color="orange",shape=17)+
  geom_point(aes(y = sample.names2, x= log(M_track$`# of ASV / sample`)),
             color="purple",shape=19)+
  geom_point(aes(y = sample.names2, x= log(M_track$`# of 97% OTU / sample`)),
             color="pink",shape=19)+
  labs(title= "OTU vs ASV Count Sq=S, Ci=M, Tr=D",x="Counts (log_scale)", y="Samples")+
  theme(legend.position="top")+
  theme(axis.text.y = element_text(angle = 0, hjust = 0.5))



ggplot()+
  geom_point(aes(y = sample.names2, x= log(S_track$`# of ASV / sample`)),
             color="Stringent_ASV",shape=15)+
  geom_point(aes(y = sample.names2, x= log(S_track$`# of OTU / sample`)),
             color="Stringent_OTU",shape=15)+
  geom_point(aes(y = sample.names2, x= log(NS_track$`# of ASV / sample`)),
             color="Not_Stringent_ASV",shape=17)+
  geom_point(aes(y = sample.names2, x= log(NS_track$`# of OTU / sample`)),
             color="Not_Stringent_OTU",shape=17)+
  geom_point(aes(y = sample.names2, x= log(M_track$`# of ASV / sample`)),
             color="Middle_ASV",shape=19)+
  geom_point(aes(y = sample.names2, x= log(M_track$`# of OTU / sample`)),
             color="Middle_OTU",shape=19)+
  labs(title= "OTU vs ASV Count Sq=S, Ci=M, Tr=D",x="Counts (log_scale)", y="Samples")+
  theme(legend.position="top")+
  theme(axis.text.y = element_text(angle = 0, hjust = 0.5))+
  scale_colour_manual(name="Line Color",
                    values=c(Stringent_ASV="darkgreen", 
                             Stringent_OTU="blue", 
                             Not_Stringent_ASV="red",
                             Not_Stringent_OTU="orange",
                             Middle_ASV="purple",
                             Middle_OTU="pink"))


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
#                                                                                   #
#####                                   Similarity                              #####
#                                                                                   #    
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
library(dplyr)
library(VennDiagram)


#### Middle ####

setwd("/Users/simonmorvan/Desktop/UdeM/GIT/ASV-OTU/Middle/")
load(file="M.seqtab.nochim.con.RData")
M.seqtab <- t(seqtab.nochim)
M.seqtab <- as.data.frame(M.seqtab)

load("M_OTU_97.RData")

M_seq_OTU_97 <- as.data.frame(unique(M_OTU_97$OTU))
M_seq_OTU_97 <- mutate_all(M_seq_OTU_97, toupper)
M_seq_OTU_97$`unique(M_OTU_97$OTU)` <- as.character(M_seq_OTU_97$`unique(M_OTU_97$OTU)`)

M_seq_ASV <- as.data.frame(rownames(M.seqtab))
M_seq_ASV$`rownames(M.seqtab)` <- as.character(M_seq_ASV$`rownames(M.seqtab)`)


M.Common.seq.ASV.97 <- intersect(M_seq_ASV$`rownames(M.seqtab)`,M_seq_OTU_97$`unique(M_OTU_97$OTU)`)
M.Common.seq.ASV.97 <- as.data.frame(M.Common.seq.ASV.97)
# 4665 common sequences between ASV and 97% OTUs

load("M_OTU_100.RData")
M_seq_OTU_100 <- as.data.frame(unique(M_OTU_100$OTU))
M_seq_OTU_100 <- mutate_all(M_seq_OTU_100, toupper)
M_seq_OTU_100$`unique(M_OTU_100$OTU)` <- as.character(M_seq_OTU_100$`unique(M_OTU_100$OTU)`)

M.Common.seq.ASV.100 <- intersect(M_seq_ASV$`rownames(M.seqtab)`,M_seq_OTU_100$`unique(M_OTU_100$OTU)`)
M.Common.seq.ASV.100 <- as.data.frame(M.Common.seq.ASV.100)
# 6137 common sequences between ASV and 100% OTU 


M.Common.seq.97.100 <- intersect(M_seq_OTU_97$`unique(M_OTU_97$OTU)`,M_seq_OTU_100$`unique(M_OTU_100$OTU)`)
M.Common.seq.97.100 <- as.data.frame(M.Common.seq.97.100)
# 7342 common sequences between 97% and 100% OTU

M.Common.all <- intersect(M.Common.seq.ASV.97$M.Common.seq.ASV.97,M_seq_OTU_100$`unique(M_OTU_100$OTU)`)
M.Common.all <- as.data.frame(M.Common.all)
# 4475 common sequences between ASV, 97% and 100% OTUs

grid.newpage()
M.Venn.diag <- draw.triple.venn(
  area1=12025 , # ASV
  area2=7929 , # 97% OTU
  area3=11481 , # 100% OTU
  n12=4665 , # ASV_97
  n23=7342 , # 97-100
  n13=6137 , # ASV-100
  n123=4475,# Common
  category = c("ASV (12025)", "97% OTU (7929)", "100% OTU (11481)"),
  fill = c("#7fc97f", "#beaed4", "#fdc086"),
  lty = "blank",
  cex = 1.2,
  cat.fontface = 2,
  cat.cex = 1.2,
  cat.dist = -0.05)

# NEEEDS A TITLE 
# title <- ggdraw() + draw_label("Similarity between methods - Middle", fontface='bold')
# plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1)) # rel_heights values control title margins
# https://cran.r-project.org/web/packages/cowplot/vignettes/plot_annotations.html


#### Not Stringent  ####

setwd("/Users/simonmorvan/Desktop/UdeM/GIT/ASV-OTU/NotStringent/")
load(file="NS.seqtab.nochim.con.RData")
NS.seqtab <- t(seqtab.nochim)
NS.seqtab <- as.data.frame(NS.seqtab)

load("S_OTU_97.RData")

S_seq_OTU_97 <- as.data.frame(unique(S_OTU_97$OTU))
S_seq_OTU_97 <- mutate_all(S_seq_OTU_97, toupper)
S_seq_OTU_97$`unique(S_OTU_97$OTU)` <- as.character(S_seq_OTU_97$`unique(S_OTU_97$OTU)`)

S_seq_ASV <- as.data.frame(rownames(S.seqtab))
S_seq_ASV$`rownames(S.seqtab)` <- as.character(S_seq_ASV$`rownames(S.seqtab)`)

S.Common.seq.ASV.97 <- intersect(S_seq_ASV$`rownames(S.seqtab)`,S_seq_OTU_97$`unique(S_OTU_97$OTU)`)
S.Common.seq.ASV.97 <- as.data.frame(S.Common.seq.ASV.97)
# 0 common sequences between ASV and 97% OTUs

load("S_OTU_100.RData")
S_seq_OTU_100 <- as.data.frame(unique(S_OTU_100$OTU))
S_seq_OTU_100 <- mutate_all(S_seq_OTU_100, toupper)
S_seq_OTU_100$`unique(S_OTU_100$OTU)` <- as.character(S_seq_OTU_100$`unique(S_OTU_100$OTU)`)

S.Common.seq.ASV.100 <- intersect(S_seq_ASV$`rownames(S.seqtab)`,S_seq_OTU_100$`unique(S_OTU_100$OTU)`)
S.Common.seq.ASV.100 <- as.data.frame(S.Common.seq.ASV.100)
# 0 common sequences between ASV and 100% OTU 

S.Common.seq.97.100 <- intersect(S_seq_OTU_97$`unique(S_OTU_97$OTU)`,S_seq_OTU_100$`unique(S_OTU_100$OTU)`)
S.Common.seq.97.100 <- as.data.frame(S.Common.seq.97.100)
# 8959 common sequences between 97% and 100% OTU

S.Common.all <- intersect(S.Common.seq.ASV.97$S.Common.seq.ASV.97,S_seq_OTU_100$`unique(S_OTU_100$OTU)`)
S.Common.all <- as.data.frame(S.Common.all)
# 0 common sequences between ASV, 97% and 100% OTUs

grid.newpage()
S.Venn.diag <- draw.triple.venn(
  area1=10145 , # ASV
  area2=9327 , # 97% OTU
  area3=12608 , # 100% OTU
  n12=0 , # ASV_97
  n23=8959 , # 97-100
  n13=0 , # ASV-100
  n123=0,# Common
  category = c("ASV (10145)", "97% OTU (9327)", "100% OTU (12608)"),
  fill = c("#7fc97f", "#beaed4", "#fdc086"),
  lty = "blank",
  cex = 1.2,
  cat.fontface = 2,
  cat.cex = 1.2,
  cat.dist = -0.05)

# ???????

####  Stringent  ####

setwd("/Users/simonmorvan/Desktop/UdeM/GIT/ASV-OTU/Stringent/")
load(file="S.seqtab.nochim.con.RData")
S.seqtab <- t(seqtab.nochim)
S.seqtab <- as.data.frame(S.seqtab)

load("S_OTU_97.RData")

S_seq_OTU_97 <- as.data.frame(unique(S_OTU_97$OTU))
S_seq_OTU_97 <- mutate_all(S_seq_OTU_97, toupper)
S_seq_OTU_97$`unique(S_OTU_97$OTU)` <- as.character(S_seq_OTU_97$`unique(S_OTU_97$OTU)`)

S_seq_ASV <- as.data.frame(rownames(S.seqtab))
S_seq_ASV$`rownames(S.seqtab)` <- as.character(S_seq_ASV$`rownames(S.seqtab)`)

S.Common.seq.ASV.97 <- intersect(S_seq_ASV$`rownames(S.seqtab)`,S_seq_OTU_97$`unique(S_OTU_97$OTU)`)
S.Common.seq.ASV.97 <- as.data.frame(S.Common.seq.ASV.97)
# 49 common sequences between ASV and 97% OTUs

load("S_OTU_100.RData")
S_seq_OTU_100 <- as.data.frame(unique(S_OTU_100$OTU))
S_seq_OTU_100 <- mutate_all(S_seq_OTU_100, toupper)
S_seq_OTU_100$`unique(S_OTU_100$OTU)` <- as.character(S_seq_OTU_100$`unique(S_OTU_100$OTU)`)

S.Common.seq.ASV.100 <- intersect(S_seq_ASV$`rownames(S.seqtab)`,S_seq_OTU_100$`unique(S_OTU_100$OTU)`)
S.Common.seq.ASV.100 <- as.data.frame(S.Common.seq.ASV.100)
# 55 common sequences between ASV and 100% OTU 

S.Common.seq.97.100 <- intersect(S_seq_OTU_97$`unique(S_OTU_97$OTU)`,S_seq_OTU_100$`unique(S_OTU_100$OTU)`)
S.Common.seq.97.100 <- as.data.frame(S.Common.seq.97.100)
# 10288 common sequences between 97% and 100% OTU

S.Common.all <- intersect(S.Common.seq.ASV.97$S.Common.seq.ASV.97,S_seq_OTU_100$`unique(S_OTU_100$OTU)`)
S.Common.all <- as.data.frame(S.Common.all)
# 49 common sequences between ASV, 97% and 100% OTUs

grid.newpage()
S.Venn.diag <- draw.triple.venn(
  area1=95 , # ASV
  area2=10368 , # 97% OTU
  area3=13161 , # 100% OTU
  n12=49 , # ASV_97
  n23=10288 , # 97-100
  n13=55, # ASV-100
  n123=49,# Common
  category = c("ASV (95)", "97% OTU (10368)", "100% OTU (13161)"),
  fill = c("#7fc97f", "#beaed4", "#fdc086"),
  lty = "blank",
  cex = 1.2,
  cat.fontface = 2,
  cat.cex = 1.2,
  cat.dist = -0.05)



