#Bravo et al. 2020
library(vegan)
library(pairwiseAdonis)
library(rich)
library(ggplot2)
library(cowplot)
library(doBy)
library(reshape)

#DATA---------------------------------------------------------

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#Cover-percentage Data
Cover.data<- read.csv("Percent_Cover_Data.csv")

#Density Data
Density.data <- read.csv("Density_Data.csv")
#take out row (photos) with cero organisms on it 
Density.data <- Density.data[which(rowSums(Density.data[,-(1:20)]) > 0),]

#Presence-absence Data
Presence.absence.data <- read.csv("Presence_Absence_Data.csv")
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

#FIGURE 2
#nMDS---------------------------------------------------------
# (Comparations of benthic assambles among rocky surface orientations on 3 depth levels)

#Subset data by depth 

#Shallow reefs
Cover.data_shallow <- subset(Cover.data,Depth=="shallow")
#nMDS calculations (no transformation + Bray-Curtis)
nMDSshallow=metaMDS(Cover.data_shallow[,-(1:20)],k=2,trymax=10,try = 10,distance ="bray",autotransform = FALSE)
NMDS1.shallow <-nMDSshallow$points[,1] 
NMDS2.shallow <- nMDSshallow$points[,2]
MDS.plot<-cbind(Cover.data_shallow[,-(1:20)], NMDS1.shallow, NMDS2.shallow,Cover.data_shallow$reef.area) 
#nMDS plot shallow
nMDSshallowplot <- ggplot(MDS.plot, aes(NMDS1.shallow, NMDS2.shallow, color=Cover.data_shallow$reef.area,shape=Cover.data_shallow$reef.area))+geom_point(position=position_jitter(.1))+stat_ellipse(type='t',size =1) +theme_bw() + theme(legend.position = "none",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank()) + annotate("text", x=max(NMDS1.shallow)-0.5, y=min(NMDS2.shallow)-0.5, label=paste('Stress =',round(nMDSshallow$stress,3)))+ggtitle("   Shallow Rocky Reefs")+ scale_color_grey(name = "Reef surface orientation", labels = c("Cavefloor", "Horizontal", "Overhang","Vertical"))
#For color plot include at the end--->  scale_color_brewer(palette="Spectral",name = "Reef surface orientation", labels = c("Cavefloor", "Horizontal", "Overhang","Vertical"))
#For BW plot include at the end --->  scale_color_grey(name = "Reef surface orientation", labels = c("Cavefloor", "Horizontal", "Overhang","Vertical"))



#Medium reefs
Cover.data_medium <- subset(Cover.data,Depth=="medium")
#nMDS calculations (no transformation + Bray)
nMDSmedium=metaMDS(Cover.data_medium[,-(1:20)],k=2,trymax=10,try = 10,distance ="bray",autotransform = FALSE)
NMDS1.medium <-nMDSmedium$points[,1] 
NMDS2.medium <- nMDSmedium$points[,2]
MDS.plot<-cbind(Cover.data_medium[,-(1:20)], NMDS1.medium, NMDS2.medium,Cover.data_medium$reef.area) 
#nMDS plot medium
nMDSmediumplot <- ggplot(MDS.plot, aes(NMDS1.medium, NMDS2.medium, color=Cover.data_medium$reef.area,shape=Cover.data_medium$reef.area))+geom_point(position=position_jitter(.1))+stat_ellipse(type='t',size =1) +theme_bw() + theme(legend.position = "none",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank())+ annotate("text", x=max(NMDS1.medium)-0.5, y=min(NMDS2.medium), label=paste('Stress =',round(nMDSmedium$stress,3)))+ggtitle("   Medium Rocky Reefs") + scale_color_grey(name = "Reef surface orientation", labels = c("Cavefloor", "Horizontal", "Overhang","Vertical"))
#For color plot include at the end--->  scale_color_brewer(palette="Spectral",name = "Reef surface orientation", labels = c("Cavefloor", "Horizontal", "Overhang","Vertical"))
#For BW plot include at the end --->  scale_color_grey(name = "Reef surface orientation", labels = c("Cavefloor", "Horizontal", "Overhang","Vertical"))




#Deep reefs
Cover.data_deep <- subset(Cover.data,Depth=="deep")
#nMDS calculations (no transformation + Bray)
nMDSdeep=metaMDS(Cover.data_deep[,-(1:20)],k=2,trymax=10,try = 10,distance ="bray",autotransform = FALSE)
NMDS1.deep <-nMDSdeep$points[,1] 
NMDS2.deep <- nMDSdeep$points[,2]
MDS.plot<-cbind(Cover.data_deep[,-(1:20)], NMDS1.deep, NMDS2.deep,Cover.data_deep$reef.area) 
#nMDS plot deep
nMDSdeepplot <- ggplot(MDS.plot, aes(NMDS1.deep, NMDS2.deep, color=Cover.data_deep$reef.area,shape=Cover.data_deep$reef.area))+geom_point(position=position_jitter(.1))+stat_ellipse(type='t',size =1) +theme_bw() + theme(legend.position = "none",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank())+ annotate("text", x=max(NMDS1.deep)-0.5, y=min(NMDS2.deep)-0.2, label=paste('Stress =',round(nMDSdeep$stress,3)))+ggtitle("   Deep Rocky Reefs") + scale_color_grey(name = "Reef surface orientation", labels = c("Cavefloor", "Horizontal", "Overhang","Vertical"))

#For color plot include at the end--->  scale_color_brewer(palette="Spectral",name = "Reef surface orientation", labels = c("Cavefloor", "Horizontal", "Overhang","Vertical"))
#For BW plot include at the end --->  scale_color_grey(name = "Reef surface orientation", labels = c("Cavefloor", "Horizontal", "Overhang","Vertical"))



# Plot legend
legend <- ggplot(MDS.plot, aes(NMDS1.deep, NMDS2.deep, color=Cover.data_deep$reef.area,shape=Cover.data_deep$reef.area))+geom_point(position=position_jitter(.1),size=3)+stat_ellipse(type='t',size =1) +theme_bw() + theme(legend.position = "bottom",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank())+ scale_shape_manual(name = "Reef surface orientation", labels = c("Cavefloor", "Horizontal", "Overhang","Vertical"),values = c(16,17,15,3)) + theme(legend.key.size = unit(3,"line")) + scale_color_grey(name = "Reef surface orientation", labels = c("Cavefloor", "Horizontal", "Overhang","Vertical"))
#For color plot include at the end--->  scale_color_brewer(palette="Spectral",name = "Reef surface orientation", labels = c("Cavefloor", "Horizontal", "Overhang","Vertical"))
#For BW plot include at the end --->  scale_color_grey(name = "Reef surface orientation", labels = c("Cavefloor", "Horizontal", "Overhang","Vertical"))


#Create plot grid and save
legend_plot <- get_legend(legend)#take legend
nMDSbydepth <- plot_grid(nMDSshallowplot,nMDSmediumplot,nMDSdeepplot,ncol = 1, align = "v")
nMDSbydepthfig2 <- plot_grid(nMDSbydepth, legend_plot, ncol = 1, rel_heights = c(1,.09))

#save plot with format for Frontiers (max with=180 mm or half page image = )
ggsave(filename = "figure2.eps",plot =nMDSbydepthfig2,width=180,height =300,units = "mm", device="eps")


#PERMANOVA----------------------------------------------------
#Prior to PERMANOVA I used betadisper() to test for homogeneity of multivariate dispersion.  
#If PERMANOVA is significant but PERMDISP IS NOT, then you can infer that there is only a location effect. If both tests are significant, then there is a dispersion effect for sure and there might also be (not always) a location effect.
# Perform both tests (PERMANOVA and PERMDISP) will help to determine the nature of the difference between any pair of groups, whether it be due to location, spread, or a combination of the two.Function adonis studied the differences in the group means, but function betadisper studies the differences in group homogeneities.

#MODEL- Effects of reef surface orientationson benthic communities -->Factor= Reef.area (Fixed), levels= Horizontal,Vertical, Overhang, cavefloor


#Shallow
Cover.data_shallow.bc <- vegdist(Cover.data_shallow[,-(1:20)],method = "bray")

#PERMDISP
PERMDISP.shallow <- betadisper(Cover.data_shallow.bc, Cover.data_shallow$reef.area)
PERMDISP.shallow
anova(PERMDISP.shallow)
permutest(PERMDISP.shallow, pairwise = TRUE, permutations = 999)
(PERMDISP.shallow.HSD <- TukeyHSD(PERMDISP.shallow))

#PERMANOVA
adonis(Cover.data_shallow[,-(1:20)]~Cover.data_shallow$reef.area,sim.method = "bray")

#PARWISE PERMANOVA
pairwise.adonis(Cover.data_shallow[,-(1:20)],Cover.data_shallow$reef.area,sim.method ="bray")

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

#Medium
Cover.data_medium.bc <- vegdist(Cover.data_medium[,-(1:20)],method = "bray")

#PERMDISP
PERMDISP.medium <- betadisper(Cover.data_medium.bc, Cover.data_medium$reef.area)
PERMDISP.medium
anova(PERMDISP.medium)
permutest(PERMDISP.medium, pairwise = TRUE, permutations = 999)
(PERMDISP.medium.HSD <- TukeyHSD(PERMDISP.medium))

#PERMANOVA
adonis(Cover.data_medium[,-(1:20)]~Cover.data_medium$reef.area,sim.method = "bray")

#PAIRWISE PERMANOVA
pairwise.adonis(Cover.data_medium[,-(1:20)],Cover.data_medium$reef.area,sim.method ="bray")
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


#Deep
Cover.data_deep.bc <- vegdist(Cover.data_deep[,-(1:20)],method = "bray")

#PERMDISP
PERMDISP.deep <- betadisper(Cover.data_deep.bc, Cover.data_deep$reef.area)
PERMDISP.deep
anova(PERMDISP.deep)
permutest(PERMDISP.deep, pairwise = TRUE, permutations = 999)
(PERMDISP.deep.HSD <- TukeyHSD(PERMDISP.deep))

#PERMANOVA
adonis(Cover.data_deep[,-(1:20)]~Cover.data_deep$reef.area,sim.method = "bray")

#PAIRWISE PERMANOVA
pairwise.adonis(Cover.data_deep[,-(1:20)],Cover.data_deep$reef.area,sim.method ="bray")

  

#Richness Analysis-------------------------------------------
#Comparisons among reef surface orientations

#Factors
depth <- Presence.absence.data$Depth
reefname <- Presence.absence.data$reef.name
reefarea <- Presence.absence.data$reef.area
presence_absence<-Presence.absence.data[,-(1:20)]
sppnumber <- specnumber(presence_absence)
specpool(presence_absence)
#Data Frame with spp number per photoquadrat
spp <- data.frame(reefname,reefarea,depth,sppnumber)


#All reefs and orientations from presence absence matrix 
totalrichness <- rich(Presence.absence.data[,-(1:20)],nrandom=499,verbose=TRUE)

#All reefs and orientations from cover matrix
totalrichness_cover <- rich(Cover.data[,-(1:20)],nrandom=499,verbose=TRUE)


#Subset data by reef depth
Presence.absence.data.shallow <- subset(Presence.absence.data, Depth=="shallow")
Presence.absence.data.shallow <- Presence.absence.data.shallow[,-(1:20)]
Presence.absence.data.medium <- subset(Presence.absence.data, Depth=="medium")
Presence.absence.data.medium <- Presence.absence.data.medium[,-(1:20)]
Presence.absence.data.deep<- subset(Presence.absence.data, Depth=="deep")
Presence.absence.data.deep <- Presence.absence.data.deep[,-(1:20)]

#some richness data by depth 
specpool(Presence.absence.data.shallow)
specpool(Presence.absence.data.medium)
specpool(Presence.absence.data.deep)

#Create alist with data from the 3 depth levels
list.data.spp.depth<-list(Presence.absence.data.shallow,Presence.absence.data.medium,Presence.absence.data.deep)
names(list.data.spp.depth)<-c("shallow","medium","deep")

#Table of shared spp by depth-----
shared(list.data.spp.depth)



#Calculate richness statistics using "rich" for each depth 
rich.results.depth <- lapply(list.data.spp.depth, function (j) {
  richness.results <- rich(matrix=j, nrandom=499,verbose=TRUE)
})


#create .csv with observations data by depth
Observations_depth <- as.data.frame(rich.results.depth[[1]]$sumcol)
Observations_depth$S <- Observations_depth$`rich.results.depth[[1]]$sumcol`
Observations_depth$M <- as.numeric(rich.results.depth[[2]]$sumcol)
Observations_depth$D <- as.numeric(rich.results.depth[[3]]$sumcol)

write.csv(x=Observations_depth, file="Observations_depth.csv")


#comparison using "c2cv" and "c2m"
#shallow[1] vs medium[2]
c2cv(com1=rich.results.depth[[1]]$matrix,com2=rich.results.depth[[2]]$matrix,nrandom=999,verbose=F)
x<-rich(rich.results.depth[[1]]$matrix,nrandom=50,verbose=TRUE)
y<-rich(rich.results.depth[[2]]$matrix,nrandom=50,verbose=TRUE)
c2m(pop1=x$sumrow,pop2=y$sumrow,nrandom=999,verbose=F)

#shallow[1] vs deep[3]
c2cv(com1=rich.results.depth[[1]]$matrix,com2=rich.results.depth[[3]]$matrix,nrandom=999,verbose=F)
x<-rich(rich.results.depth[[1]]$matrix,nrandom=50,verbose=TRUE)
y<-rich(rich.results.depth[[3]]$matrix,nrandom=50,verbose=TRUE)
c2m(pop1=x$sumrow,pop2=y$sumrow,nrandom=999,verbose=F)

#medium[2] vs deep[3]
c2cv(com1=rich.results.depth[[2]]$matrix,com2=rich.results.depth[[3]]$matrix,nrandom=999,verbose=F)
x<-rich(rich.results.depth[[2]]$matrix,nrandom=50,verbose=TRUE)
y<-rich(rich.results.depth[[3]]$matrix,nrandom=50,verbose=TRUE)
c2m(pop1=x$sumrow,pop2=y$sumrow,nrandom=999,verbose=F)


 
#data subset by surface orientation
Presence.absence.data.horizontal <- subset(Presence.absence.data, reef.area=="horizontal")
Presence.absence.data.horizontal <- Presence.absence.data.horizontal[,-(1:20)]
Presence.absence.data.vertical <- subset(Presence.absence.data, reef.area=="vertical")
Presence.absence.data.vertical <- Presence.absence.data.vertical[,-(1:20)]
Presence.absence.data.overhang <- subset(Presence.absence.data, reef.area=="overhang")
Presence.absence.data.overhang <- Presence.absence.data.overhang[,-(1:20)]
Presence.absence.data.cavefloor <- subset(Presence.absence.data, reef.area=="cavefloor")
Presence.absence.data.cavefloor <- Presence.absence.data.cavefloor[,-(1:20)]

list.data.spp<-list(Presence.absence.data.horizontal,Presence.absence.data.vertical,Presence.absence.data.overhang,Presence.absence.data.cavefloor)
names(list.data.spp)<-c("horizontal","vertical","overhang","cavefloor")

#table of shared spp for surfaces orientations-----
shared(list.data.spp)


#Richness stats by surface orientation 
rich.results <- lapply(list.data.spp, function (j) {
  richness.results <- rich(matrix=j, nrandom=499,verbose=TRUE)
})
# observed cumulative species richness
rich.results[[1]]$cr
# observed mean value of species richness over the n samples
rich.results[[1]]$mr
#rare species
rich.results[[1]]$uniques
rich.results[[1]]$duplicates
rich.results[[2]]$uniques
rich.results[[2]]$duplicates
rich.results[[3]]$uniques
rich.results[[3]]$duplicates
rich.results[[4]]$uniques
rich.results[[4]]$duplicates

#create csv with observation data by orientation
Observations_orientations <- as.data.frame(rich.results[[1]]$sumcol)
Observations_orientations$H <- Observations_orientations$`rich.results[[1]]$sumcol`
Observations_orientations$V <- as.numeric(rich.results[[2]]$sumcol)
Observations_orientations$O <- as.numeric(rich.results[[3]]$sumcol)
Observations_orientations$C <- as.numeric(rich.results[[4]]$sumcol)

write.csv(x=Observations_orientations, file="Observations_orientation.csv")



#horizontal[1] vs vertical[2]
c2cv(com1=rich.results[[1]]$matrix,com2=rich.results[[2]]$matrix,nrandom=999,verbose=FALSE)
x<-rich(rich.results[[1]]$matrix,nrandom=50,verbose=TRUE)
y<-rich(rich.results[[2]]$matrix,nrandom=50,verbose=TRUE)
c2m(pop1=x$sumrow,pop2=y$sumrow,nrandom=999,verbose=FALSE)

#horizontal[1] vs overhang [3]
c2cv(com1=rich.results[[1]]$matrix,com2=rich.results[[3]]$matrix,nrandom=999,verbose=FALSE)
x<-rich(rich.results[[1]]$matrix,nrandom=50,verbose=TRUE)
y<-rich(rich.results[[3]]$matrix,nrandom=50,verbose=TRUE)
c2m(pop1=x$sumrow,pop2=y$sumrow,nrandom=999,verbose=FALSE)

#horizontal[1] vs cavefloor [4]
c2cv(com1=rich.results[[1]]$matrix,com2=rich.results[[4]]$matrix,nrandom=999,verbose=FALSE)
x<-rich(rich.results[[1]]$matrix,nrandom=50,verbose=TRUE)
y<-rich(rich.results[[4]]$matrix,nrandom=50,verbose=TRUE)
c2m(pop1=x$sumrow,pop2=y$sumrow,nrandom=999,verbose=FALSE)

#vertical[2] vs overhang [3]
c2cv(com1=rich.results[[2]]$matrix,com2=rich.results[[3]]$matrix,nrandom=999,verbose=FALSE)
x<-rich(rich.results[[2]]$matrix,nrandom=50,verbose=TRUE)
y<-rich(rich.results[[3]]$matrix,nrandom=50,verbose=TRUE)
c2m(pop1=x$sumrow,pop2=y$sumrow,nrandom=999,verbose=FALSE)

#vertical[2] vs cavefloor [4]
c2cv(com1=rich.results[[2]]$matrix,com2=rich.results[[4]]$matrix,nrandom=999,verbose=FALSE)
x<-rich(rich.results[[2]]$matrix,nrandom=50,verbose=TRUE)
y<-rich(rich.results[[4]]$matrix,nrandom=50,verbose=TRUE)
c2m(pop1=x$sumrow,pop2=y$sumrow,nrandom=999,verbose=FALSE)

#overhang [3] vs cavefloor [4]
c2cv(com1=rich.results[[3]]$matrix,com2=rich.results[[4]]$matrix,nrandom=999,verbose=FALSE)
x<-rich(rich.results[[3]]$matrix,nrandom=50,verbose=TRUE)
y<-rich(rich.results[[4]]$matrix,nrandom=50,verbose=TRUE)
c2m(pop1=x$sumrow,pop2=y$sumrow,nrandom=999,verbose=FALSE)


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

#Figure4
#Boxplot for spp richnnes by depth--------------
richnessBYdepth <- ggplot(data=spp, mapping=aes(x=depth, y=sppnumber)) +geom_boxplot()  + scale_x_discrete(limits=c("shallow", "medium","deep"),labels=c("Shallow", "Medium","Deep")) + labs(title="",x="Depth", y = "Taxa richness per quadrat") +theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),text = element_text(size=12)) + annotate("text", x=c(1,2,3), y=c(12.3,14.1,16), label="*",size=8)

#save plot for half page 
ggsave(filename = "figure4.eps",plot = richnessBYdepth,width=85,height =85,units = "mm", device="eps")

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

#Figure6
#Boxplot for spp richnnes by surfaceorientations-------------
richnessBYorientation <- ggplot(data=spp, mapping=aes(x=reefarea, y=sppnumber)) +geom_boxplot()  + scale_x_discrete(limits=c("horizontal", "vertical","overhang","cavefloor"),labels=c("Horizontal", "Vertical","Overhang","Cavefloor")) + labs(title="",x="Reef surface orientation", y = "Taxa richness per quadrat")+theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),text = element_text(size=12)) + annotate("text", x=2, y=16, label="*",size=8)

#save plot for half page 
ggsave(filename = "figure6.eps",plot = richnessBYorientation,width=85,height =85,units = "mm", device="eps")


# Table Richness by orientation
spp <- data.frame(reefname,depth,sppnumber,reefarea)
richnes.reefarea <-  summaryBy(sppnumber ~ reefarea ,data = spp, FUN = function(x) { c(mean = mean(x), SD=sd(x),SE = sqrt(var(x)/length(x)),min=min(x),max=max(x))})


#Observations-------------------------------------------------

#all obs for table orientations vs depth
Observations <- as.data.frame(colSums(Presence.absence.data[,-(1:20)]))
#shallow vs surfaces 
Presence.absence.data.shallow.h <- subset(Presence.absence.data, Depth=="shallow"& reef.area=="horizontal")
Observations$SH <- as.numeric(colSums(Presence.absence.data.shallow.h [,-(1:20)]))
Presence.absence.data.shallow.v <- subset(Presence.absence.data, Depth=="shallow"& reef.area=="vertical")
Observations$SV <- as.numeric(colSums(Presence.absence.data.shallow.v [,-(1:20)]))
Presence.absence.data.shallow.o <- subset(Presence.absence.data, Depth=="shallow"& reef.area=="overhang")
Observations$SO <- as.numeric(colSums(Presence.absence.data.shallow.o [,-(1:20)]))
Presence.absence.data.shallow.c <- subset(Presence.absence.data, Depth=="shallow"& reef.area=="cavefloor")
Observations$SC <- as.numeric(colSums(Presence.absence.data.shallow.c [,-(1:20)]))

#medium vs surfaces 
Presence.absence.data.medium.h <- subset(Presence.absence.data, Depth=="medium"& reef.area=="horizontal")
Observations$MH <- as.numeric(colSums(Presence.absence.data.medium.h [,-(1:20)]))
Presence.absence.data.medium.v <- subset(Presence.absence.data, Depth=="medium"& reef.area=="vertical")
Observations$MV <- as.numeric(colSums(Presence.absence.data.medium.v [,-(1:20)]))
Presence.absence.data.medium.o <- subset(Presence.absence.data, Depth=="medium"& reef.area=="overhang")
Observations$MO <- as.numeric(colSums(Presence.absence.data.medium.o [,-(1:20)]))
Presence.absence.data.medium.c <- subset(Presence.absence.data, Depth=="medium"& reef.area=="cavefloor")
Observations$MC <- as.numeric(colSums(Presence.absence.data.medium.c [,-(1:20)]))


#deep vs surfaces 
Presence.absence.data.deep.h <- subset(Presence.absence.data, Depth=="deep"& reef.area=="horizontal")
Observations$DH <- as.numeric(colSums(Presence.absence.data.deep.h [,-(1:20)]))
Presence.absence.data.deep.v <- subset(Presence.absence.data, Depth=="deep"& reef.area=="vertical")
Observations$DV <- as.numeric(colSums(Presence.absence.data.deep.v [,-(1:20)]))
Presence.absence.data.deep.o <- subset(Presence.absence.data, Depth=="deep"& reef.area=="overhang")
Observations$DO <- as.numeric(colSums(Presence.absence.data.deep.o [,-(1:20)]))
Presence.absence.data.deep.c <- subset(Presence.absence.data, Depth=="deep"& reef.area=="cavefloor")
Observations$DC <- as.numeric(colSums(Presence.absence.data.deep.c [,-(1:20)]))


write.csv(x=Observations, file="Observations.csv")


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

#Species accumulation curves ---------------------------------

horizontal<- read.csv("Presence_Absence_Data.csv")
horizontal<- subset(horizontal,reef.area== "horizontal")
horizontal<- horizontal[,-(1:20)]
horizontalaccucurve <- specaccum(horizontal,"rarefaction")
vertical<- read.csv("Presence_Absence_Data.csv")
vertical<- subset(vertical,reef.area== "vertical")
vertical<- vertical[,-(1:20)]
verticalaccucurve <- specaccum(vertical,"rarefaction")
overhang<- read.csv("Presence_Absence_Data.csv")
overhang<- subset(overhang,reef.area== "overhang")
overhang<- overhang[,-(1:20)]
overhangaccucurve <- specaccum(overhang,"rarefaction")
cavefloor<- read.csv("Presence_Absence_Data.csv")
cavefloor<- subset(cavefloor,reef.area== "cavefloor")
cavefloor<- cavefloor[,-(1:20)]
caveflooraccucurve <- specaccum(cavefloor,"rarefaction")


plot(horizontalaccucurve, ci.type="line", col="#C77CFF", lwd=2, ci.lty=0, ci.col="lightblue",ylab="Número de especies", xlab = "Número de foto-cuadrantes",xlim=c(0,150),ylim=c(0,60))
plot(verticalaccucurve,ci.type="line", col="#7CAE00", lwd=2, ci.lty=0, ci.col="lightblue",add=T)
plot(overhangaccucurve, ci.type="line", col="#00BFC4", lwd=2, ci.lty=0, ci.col="lightblue",add=T)
plot(caveflooraccucurve, ci.type="line", col="#F8766D", lwd=2, ci.lty=0, ci.col="lightblue",add=T)
legend("bottomright", legend=c("horizontal", "vertical","overhang","cavefloor"),col=c("#C77CFF", "#7CAE00","#00BFC4","#F8766D"),lty=1,box.lty=0)
abline(v=20, col="red")

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

#Funtional Groups---------------------------------------------------------
#Create colums with functional groups
#all seaweed (11)
Cover.data$algae <- as.numeric(paste(Cover.data$Macroalgae..Filamentous +
                                         Cover.data$Lomentaria.clavellosa +
                                         Cover.data$Dictyota.dichotoma + 
                                         Cover.data$Hymenena.Phycodrys + 
                                         Cover.data$Ulva.sp.+ 
                                         Cover.data$Juvenile.Undaria.pinnatifida+
                                         Cover.data$Codium.vermilara.fragile+
                                         Cover.data$Colpomenia.sinuosa+
                                         Cover.data$brown.encrusting.algae+
                                         Cover.data$Crustose.coralline.algae+
                                    Cover.data$Corallina.officinalis..substratum.))

#Filter feeders (23)
Cover.data$filterfeeder <- as.numeric(paste(Cover.data$Diplosoma.listerianum+ 
                                              Cover.data$Lissoclinum.fragile + 
                                              Cover.data$Aplidium.sp. + 
                                              Cover.data$Colonial.tunicate+
                                              Cover.data$Ascidiella.aspersa + 
                                              Cover.data$Asterocarpa.humilis + 
                                              Cover.data$Corella.eumyota + 
                                              Cover.data$Ciona.intestinalis + 
                                              Cover.data$Ciona.robusta+ 
                                              Cover.data$Paramolgula.gregaria + 
                                              Cover.data$Cliona.sp. + 
                                              Cover.data$Sponge..Encrusting + 
                                              Cover.data$Sponge..Repent + 
                                              Cover.data$Sponge..Tubular + 
                                              Cover.data$Sponge..massive.violet + 
                                              Cover.data$Sponge..Masive + 
                                              Cover.data$Darwinella.cf..rosacea+ 
                                              Cover.data$Clathria.sp. +
                                              Cover.data$Sponge..Calcareous+ 
                                              Cover.data$Aulacomya.atra + 
                                              Cover.data$Aequipecten.tehuelchus + 
                                              Cover.data$Magellania.venosa  + 
                                              Cover.data$Leiosolenus.patagonicus))

#Suspensive feeders (11)
Cover.data$suspensivefeeders <- as.numeric(paste(Cover.data$Anthothoe.chilensis  +
                                  Cover.data$Corynactis.carnea+
                                  Cover.data$Halcurias.sp. + 
                                  Cover.data$Metridium.senile + 
                                  Cover.data$Parabunodactis.imperfecta +
                                  Cover.data$Tripalea.clavaria + 
                                  Cover.data$Hydrozoan + 
                                  Cover.data$Myxicola + 
                                  Cover.data$Bryozoan + 
                                  Cover.data$Austromegabalanus.psittacus +
                                  Cover.data$Terebellidae+
                                  Cover.data$Worms..Polychaetes..Tube.worms))

#Baresubstrate (1)
Cover.data$baresubstrate <- Cover.data$Bare.Substrate 


#Predators/scavenger (11)
Cover.data$P.S <- as.numeric(paste(Cover.data$Allostichaster.capensis+ 
                                  Cover.data$Arbacia.dufresnii + 
                                  Cover.data$Cosmasterias.lurida + 
                                  Cover.data$Cycethra.verrucosa+
                                  Cover.data$Odontaster.penicillatus +                                                Cover.data$Pseudechinus.magellanicus + 
                                  Cover.data$Diaulula.punctuolata + 
                                  Cover.data$Doris.fontainii + 
                                  Cover.data$Fissurella.radiosa.tixierae+                                             Cover.data$Fissurellidea.patagonica + 
                                  Cover.data$Pleurobranchaea.maculata))

#Filamentous algae (2)
Cover.data$filamentous.algae <- as.numeric(paste(Cover.data$Macroalgae..Filamentous +Cover.data$Lomentaria.clavellosa))

#Laminarian algae (6)
Cover.data$laminarian.algae <-  as.numeric(paste(Cover.data$Dictyota.dichotoma +
                                          Cover.data$Hymenena.Phycodrys +
                                          Cover.data$Ulva.sp.  +
                                          Cover.data$Juvenile.Undaria.pinnatifida +                                           Cover.data$Codium.vermilara.fragile+
                                          Cover.data$Colpomenia.sinuosa))

#Crustose algae (2)
Cover.data$crustose.algae <- as.numeric(paste(Cover.data$brown.encrusting.algae +                                           Cover.data$Crustose.coralline.algae))

#algas coralinas erectas
#Cover.data$coralinas.erectas <- Cover.data$Corallina.officinalis..substratum.

#data frame functional groups 
Cover.data.gruposfuncionales <- Cover.data[,c(1:20,81:85)]

#cover porcentage by feeding modes 
library(doBy)
Cover.data.gruposfuncionales.byreefarea <- summaryBy(Cover.data.gruposfuncionales[,-(1:20)] ~ reef.area,   data =Cover.data.gruposfuncionales, FUN = function(x) { c(mean = mean(x),SD=sd(x),SE = sqrt(var(x)/length(x)),n=length(x)) })

Cover.data.gruposfuncionales.bydepth <- summaryBy(Cover.data.gruposfuncionales[,-(1:20)] ~ Depth,   data =Cover.data.gruposfuncionales, FUN = function(x) { c(mean = mean(x),SD=sd(x),SE = sqrt(var(x)/length(x)),n=length(x)) })

Cover.data.gruposfuncionales.byreefarea.depth <- summaryBy(Cover.data.gruposfuncionales[,-(1:20)] ~ reef.area + Depth,   data =Cover.data.gruposfuncionales, FUN = function(x) { c(mean = mean(x),SD=sd(x),SE = sqrt(var(x)/length(x)),n=length(x)) })


#Create data.frame with mean, SD, SE of funtional groups
library(reshape)
datacoverfunctiongroup <- melt(Cover.data.gruposfuncionales.byreefarea.depth, c("reef.area", "Depth"), c("algae.mean","filterfeeder.mean","suspensivefeeders.mean","baresubstrate.mean"))
datacoverfunctiongroup.SE <- melt(Cover.data.gruposfuncionales.byreefarea.depth, c("reef.area", "Depth"), c("algae.SE","filterfeeder.SE","suspensivefeeders.SE","baresubstrate.SE"))
datacoverfunctiongroup.SD <- melt(Cover.data.gruposfuncionales.byreefarea.depth, c("reef.area", "Depth"), c("algae.SD","filterfeeder.SD","suspensivefeeders.SD","baresubstrate.SD"))

datacoverfunctiongroup$SE <- datacoverfunctiongroup.SE$value
datacoverfunctiongroup$SD <- datacoverfunctiongroup.SD$value

#Plots functional groups--------------

legend.horizontal <- ggplot(subset(datacoverfunctiongroup,reef.area=="horizontal"), aes(x=Depth, y=value, group=variable,shape=variable, linetype=variable)) +
  geom_point(position = position_dodge(0.2),size=2.5)+
  geom_line(position = position_dodge(0.2))+
  geom_errorbar(linetype=1,aes(ymin=value - SE, ymax=value + SE),position = position_dodge(0.2), width = 0.2)+
  scale_x_discrete(limits=c("shallow","medium","deep"),labels=c("Shallow","Medium","Deep"))+ 
  scale_y_continuous(limits = c(0,100))+ 
  scale_shape_manual(name = "",labels = c("Algae", "Filter feeder", "Suspension feeders", "Bare Substrate"),values = c(15,16,17,7))+
  scale_linetype_manual(name = "",labels = c("Algae", "Filter feeder", "Suspension feeders", "Bare Substrate"),values = c("solid","dotted","longdash","dotdash"))+
  labs(fill = "",x = "", y = "Cover (%)", title = "Horizontal") +theme_bw()+ theme(legend.position = "bottom",legend.title =element_blank(),legend.key.size = unit(4,"line"))

horizontal <- ggplot(subset(datacoverfunctiongroup,reef.area=="horizontal"), aes(x=Depth, y=value, group=variable,shape=variable, linetype=variable)) +
  geom_point(position = position_dodge(0.2),size=2.5)+
  geom_line(position = position_dodge(0.2))+
  geom_errorbar(linetype=1,aes(ymin=value - SE, ymax=value + SE),position = position_dodge(0.2), width = 0.2)+
  scale_x_discrete(limits=c("shallow","medium","deep"),labels=c("Shallow","Medium","Deep"))+ 
  scale_y_continuous(limits = c(0,100))+ 
  scale_shape_manual(name = "",labels = c("Algae", "Filter feeder", "Suspensive Feeder", "Bare Substrate"),values = c(15,16,17,7))+
  scale_linetype_manual(name = "",labels = c("Algae", "Filter feeder", "Suspensive Feeder", "Bare Substrate"),values = c("solid","dotted","longdash","dotdash"))+
  labs(fill = "",x = "", y = "Cover (%)", title = "Horizontal") +theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),legend.position = "none",legend.title =element_blank(),legend.key.size = unit(4,"line"))

vertical <- ggplot(subset(datacoverfunctiongroup,reef.area=="vertical"), aes(x=Depth, y=value, group=variable,shape=variable, linetype=variable)) +
  geom_point(position = position_dodge(0.2),size=2.5)+
  geom_line(position = position_dodge(0.2))+
  geom_errorbar(linetype=1,aes(ymin=value - SE, ymax=value + SE),position = position_dodge(0.2), width = 0.2)+
  scale_x_discrete(limits=c("shallow","medium","deep"),labels=c("Shallow","Medium","Deep"))+ 
  scale_y_continuous(limits = c(0,100))+ 
  scale_shape_manual(name = "",labels = c("Algae", "Filter feeder", "Suspensive Feeder", "Bare Substrate"),values = c(15,16,17,7))+
  scale_linetype_manual(name = "",labels = c("Algae", "Filter feeder", "Suspensive Feeder", "Bare Substrate"),values = c("solid","dotted","longdash","dotdash"))+labs(fill = "",x = "", y = "Cover (%)", title = "Vertical") +theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),legend.position = "none",legend.title =element_blank(),legend.key.size = unit(4,"line"))

overhang <- ggplot(subset(datacoverfunctiongroup,reef.area=="overhang"), aes(x=Depth, y=value, group=variable,shape=variable, linetype=variable)) +
  geom_point(position = position_dodge(0.2),size=2.5)+
  geom_line(position = position_dodge(0.2))+
  geom_errorbar(linetype=1,aes(ymin=value - SE, ymax=value + SE),position = position_dodge(0.2), width = 0.2)+
  scale_x_discrete(limits=c("shallow","medium","deep"),labels=c("Shallow","Medium","Deep"))+ 
  scale_y_continuous(limits = c(0,100))+ 
  scale_shape_manual(name = "",labels = c("Algae", "Filter feeder", "Suspensive Feeder", "Bare Substrate"),values = c(15,16,17,7))+
  scale_linetype_manual(name = "",labels = c("Algae", "Filter feeder", "Suspensive Feeder", "Bare Substrate"),values = c("solid","dotted","longdash","dotdash"))+labs(fill = "",x = "", y = "Cover (%)", title = "Overhang") +theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),legend.position = "none",legend.title =element_blank(),legend.key.size = unit(4,"line"))

cavefloor <- ggplot(subset(datacoverfunctiongroup,reef.area=="cavefloor"), aes(x=Depth, y=value, group=variable,shape=variable, linetype=variable)) +
  geom_point(position = position_dodge(0.2),size=2.5)+
  geom_line(position = position_dodge(0.2))+
  geom_errorbar(linetype=1,aes(ymin=value - SE, ymax=value + SE),position = position_dodge(0.2), width = 0.2)+
  scale_x_discrete(limits=c("shallow","medium","deep"),labels=c("Shallow","Medium","Deep"))+ 
  scale_y_continuous(limits = c(0,100))+ 
  scale_shape_manual(name = "",labels = c("Algae", "Filter feeder", "Suspensive Feeder", "Bare Substrate"),values = c(15,16,17,7))+
  scale_linetype_manual(name = "",labels = c("Algae", "Filter feeder", "Suspensive Feeder", "Bare Substrate"),values = c("solid","dotted","longdash","dotdash"))+labs(fill = "",x = "", y = "Cover (%)", title = "Cavefloor") +theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),legend.position = "none",legend.title =element_blank(),legend.key.size = unit(4,"line"))

legend_plot <- get_legend(legend.horizontal)

#plot grid with 4 plots 
prow <- plot_grid(horizontal,vertical,overhang,cavefloor,ncol=2,align = "vh")
# add the legend underneath the row. Give it 10% of the height of one plot (via rel_heights).
plotcove <- plot_grid(prow, legend_plot, ncol = 1, rel_heights = c(1, .1))

#saveplot for full page size
ggsave(filename = "figure3.eps",plot =plotcove,width=180,height =250,units = "mm", device="eps")

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


#VennDiagram %-------------------------------------------------------------
library(VennDiagram)

# plot venn diagram and add some margin and enclosing box
venndigram <- ggdraw(draw.triple.venn(48,61,58,43, 52, 40, 38, category =c("Shallow (48)", "Medium (61)","Deep (58)"),lty=c("dotted","longdash","solid"),col=c('grey',"black","darkgrey"),fontfamily="Arial",cat.fontfamily="Arial",cex=1.2,cat.cex=1,cat.pos = c(-20, 20, 180))) 


#save plot for half page 
ggsave(filename = "figure5.eps",plot = venndigram,width=85,height =85,units = "mm", device="eps")

