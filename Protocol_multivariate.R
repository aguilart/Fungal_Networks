################################################################################################################
################################################################################################################
##################    PROTOCOL TO ANALYZE NETWORK PARAMETER USING MULTIVARITE ANALYSIS  ########################
################################################################################################################
################################################################################################################

#cleaning the working directory
rm(list=ls())

# The question addressed here is whether network traits "cluster" according to species identity
# For this we are using:
#           -PCA: To visualize whether there is any clustering
#           -RDA: To obtain a p-value associated to those clustering (in other words whether those 
#                 cluster are "significantly" different from each other). That said the wording 
#                 "significantly different" is becoming controversial

#Packages required. R comes with default functions and in principle one could do everything with such 
#functions. However, several "packages" have been developed that make easier performing certain tasks
#These packages have to be installed. For example the package vegan can be installed through R studio 
#or you can write:

install.packages("vegan")# Once you do this, vegan is installed in your library. Next time it is not
#necessary to run this command.

#However, to "load" those functions one needs to run the function "library" (see below). Loading
#has to be done everytime you open the R session.



#In the following each step I used for conducting both PCA and CCA



#1. Loading files. Note: one can load directly excel files (.xlxs) using R studio. This is a relatively new
#feature. I am used to the function "read.csv" for which I need to save the excel sheet I am interested as
#a csv file first. In this case, I coverted the "edge" sheet of each excel file of each of the 7 species

#These lines will load all files saved as .csv in the folder "processedData" and then merging them ("row bind")
temp = list.files(path="processedData\\",pattern="*-Edge.csv")
temp<-paste("processedData\\",temp,sep = "")
myfiles = lapply(temp,function(x){read.csv(x,header = TRUE,stringsAsFactors = FALSE)} )
#myfiles[[4]]<-myfiles[[4]][,c(1:42)]
Edge_Traits<-do.call(rbind,myfiles)

Edge_Traits$Species<-NA
Edge_Traits$Species[grep("C34",Edge_Traits$name)]<-"Mortierella elongata"
Edge_Traits$Species[grep("C35",Edge_Traits$name)]<-"Umbelopsis isabellina"
Edge_Traits$Species[grep("DF19",Edge_Traits$name)]<-"Mortierella alpina"
Edge_Traits$Species[grep("DF25",Edge_Traits$name)]<-"Mortierella elongata2"
Edge_Traits$Species[grep("DF56",Edge_Traits$name)]<-"Mucor fragilis"
Edge_Traits$Species[grep("M",Edge_Traits$name)]<-"Mortierella alpina2"

#Assuming the Or_ij can be expressed from 0 to 360 degrees
Edge_Traits$Or_ij[which(Edge_Traits$Or_ij<0)]<-(Edge_Traits$Or_ij[which(Edge_Traits$Or_ij<0)]*-1)+180

Edge_Traits<-Edge_Traits[which(Edge_Traits$Type=="E"),]

#I am removing seven values with Width=0
Edge_Traits<-Edge_Traits[-which(Edge_Traits$Width==0),]



#Network summary values
temp = list.files(path="processedData\\",pattern="*-summaryTable.csv")
temp<-paste("processedData\\",temp,sep = "")
myfiles = lapply(temp,function(x){read.csv(x,header = TRUE,stringsAsFactors = FALSE)} )
MyceliaNetw_Traits<-do.call(rbind,myfiles)

#Subsetting the edge traits of the summary
trial<-
MyceliaNetw_Traits[,c("cords_mean_Width","cords_mean_Length","cords_mean_Area","cords_mean_Volume",
                      "cords_mean_Resistance_2ave","cords_mean_Tortuosity","cords_mean_Distance",
                      "cords_mean_Accessibility","cords_mean_Betweenness","cords_mean_Route_factor",
                      "cords_mean_Or_ij" 
                      )];trial<-round(trial,digits = 3)

trial2[,-1]<-round(trial2[,-1],digits = 3)

trial1==trial2[,-1]#For some reason Resistance_2 is not the mean but instead a constant value, othe than that all o the values
#in the summary table are a simple mean of the the edge values.



#2. Getting to know the data.

#2.1 How R sees the data. It is useful to have an idea on how R recognize the data. R is an "object oriented"
#program language. That is, every data is an object which has certain properties. R uses these properties to 
#categorized the object. It is useful to know what kind of object is your data because some functions only work
#on one object type but not in others.

str(Edge_Traits)
#From this one learns that the entire table is a "dataframe" object composed of 47 vectors which are of different type
# For example the vector "name" is a character vector, "channel" is an integer (only whole numbers), and "length" is 
# a "numerical" vector.


#2.2. Looking at the distribution of the data. More impotantly, what is the distribution of the data? 

#To simply get means, min, max per fungal species:

aggregate(
        Edge_Traits[,
                         c("Width","Length","Area","Volume","Resistance_2ave","Tortuosity","Distance",
                           "Accessibility","Betweenness","Route_factor","Or_ij")],
        by=list(Edge_Traits[which(Edge_Traits$Type=="E"),]$name),
        mean)#Here you  can write different functions: min, max, median. range and summmary is also good because it
                #gets you more than one summary statistics, but it can be confusing with so much data.


#I had some issues understanding what Resistance is. Playing with the following code allowed me to determine that 
#Resistance_2ave is the 10*(Length)/width^2

summary(Edge_Traits$Resistance_2ave)
length(which(Edge_Traits$Width_intact==0))
Edge_Traits%>%
        #filter(Area!=0)%>%
        #filter(Resistance_2ave>500)%>%
        mutate(MyResistance=Length/(Width/2)^2)%>%
        mutate(Factor=Resistance_2ave/MyResistance)%>%
        select(name,Length,Width,Resistance_2ave)%>%
        ggplot()+
        aes(Species,Resistance_2ave)+
        geom_boxplot()

length(which(Edge_Traits$Resistance_2ave>500))

#Visualinzing as simple historgrams helps.
#To make those, I use the suite of functions in the package tidyverse (check if it needs to be installed).

library(tidyverse)

#Reorganizing the dataframe so the figures are made easier

Re_org<-
        Edge_Traits%>%
        #filter(Area!=0)%>%
        #mutate(MyResistance=Length/(Width/2)^2)%>%
        select(c("name","Species","Width","Length","Area","Volume","Resistance_2ave","Tortuosity","Distance",
                 "Accessibility","Betweenness","Route_factor","Or_ij"))%>%
        group_by(name)%>%
        gather(key=variable,
               value=value,Width:Or_ij)
        
        

#Visualizng simple histograms
# I am grouping the variables in different sets. Each set represent (to me) groups that make sense

Hist_Width_Length_Distance_1<-
Hist_Width_Length_Area_Vol_2<-
Hist_Width_Length_Resistance_3<-
#Hist_Length_Tortuosity_4<-For this one see below
Hist_Resistance_Accessibility_Betweenness_5<-
Hist_Distance_Route_factor_6<-
        
        #Re_org%>%
  
  Edge_Traits%>%
  #filter(Tortuosity<1.5)%>%
  select(c("name","Species","Width","Length","Area","Volume","Resistance_2ave","Tortuosity","Distance",
           "Accessibility","Betweenness","Route_factor","Or_ij"))%>%
  mutate(Log_Resistance_2ave=log10(Resistance_2ave))%>%
  mutate(Log_Betweenness=log10(Betweenness+1))%>%
  group_by(name)%>%
  gather(key=variable,
         value=value,Width:Log_Betweenness)%>%
  filter(Species%in%c("Mortierella elongata","Mucor fragilis"))%>%
        #filter(variable %in% c("Width","Length","Distance"))%>%#1_The completely indepedent variables
        #filter(variable %in% c("Width","Length","Area","Volume"))%>%#2_The geometric ones depending on width and length
        #filter(variable %in% c("Width","Length","Resistance_2ave"))%>%#3_The "transport" one depending on width and length
        filter(variable %in% c("Log_Resistance_2ave","Accessibility","Log_Betweenness"))%>%#5_The "transport" ones depending all on Resistance to some extent
        #filter(variable %in% c("Route_factor","Distance"))%>%#6_The only that depends on distance
        
        ggplot()+
        aes(value,fill=name)+
        geom_histogram()+
        facet_wrap(Species ~ variable, scales = "free",nrow = 2,ncol = 3)+
        #theme(legend.position = "none")+
  ggtitle(label = "Transport variables")+
  theme(title = element_text(size = 28),
        axis.title.x=element_blank(),
        axis.text.x = element_text(size = 20,angle = 45,hjust = 1),
        axis.text.y = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        legend.position = "none")

#About tortuosity

tapply(Edge_Traits$Tortuosity,Edge_Traits$name,summary)
tapply(Edge_Traits$Tortuosity,Edge_Traits$name,function(x){length(which(x>1.5))})
tapply(Edge_Traits$Tortuosity,Edge_Traits$name,function(x){length(which(x>1.5))/length(x)})
tapply(Edge_Traits$Tortuosity,Edge_Traits$name,function(x){length(which(x>1.5))})
tapply(Edge_Traits$Tortuosity,Edge_Traits$name,length)

Hist_Length_Tortuosity_4<-
Edge_Traits%>%
        filter(Tortuosity<1.5)%>%
        select(c("name","Species","Width","Length","Area","Volume","Resistance_2ave","Tortuosity","Distance",
                 "Accessibility","Betweenness","Route_factor","Or_ij"))%>%
        group_by(name)%>%
        gather(key=variable,
               value=value,Width:Or_ij)%>%
        
        filter(variable %in% c("Length","Tortuosity"))%>%#4_Tortuosity has little variation and depends on Length
        ggplot()+
        aes(value,fill=name)+
        geom_histogram()+
        facet_wrap(Species ~ variable, scales = "free",nrow = 6,ncol = 2)+
        theme(legend.position = "none")


#Form these plots one learns that:
#       -Some variables are mathematically correlated:
#       -Area= (pi/4)*Width^2
#       -Volume= (pi/4)*(Width^2)*Length
#       -Resitance= 10*Length/Width^2

#       -Thus for subsequent analysis I will only use variables that are not (at least so easily) related to each other:
#       -Width, Lenght, Tortuosity, Distance, Accessibility, Betweenness, Route Factor
#
#       -The distribution of the data for a  variable is similar across ose species. In summary:

#       -Width: Varies, from almost uniform to normal to bimodal to right skewed, and this depends on the species
#       -Length: Right skewed
#       -Torturosity: "A bit" normal, still data is concentrated to the left. It is completely left skewed with tortuosity values higher than 1.5 which only represent less than 1% of the data
#       -Distance:Normally distributed
#       -Accessibility: A bit" normal, still data is concentrated to the left
#       -Betwenness: Highly right skewed
#       -Route Factor: A bit" normal, still data is concentrated to the left


#What kind of transformations are needed for betweeness, Length (the ones that are clearly left-skewed) and Tortuosity

summary(Edge_Traits$Betweenness)
tapply(Edge_Traits$Betweenness,Edge_Traits$name,summary)
tapply(Edge_Traits$Betweenness,Edge_Traits$name,function(x){length(which(x>3*(quantile(x)[4])))})
tapply(Edge_Traits$Betweenness,Edge_Traits$name,function(x){length(which(x>3*(quantile(x)[4])))/length(x)})
Edge_Traits%>%
        ggplot()+
        aes(Species,log10(Betweenness+1),color=name)+
        geom_jitter(alpha=0.8)+
        facet_wrap(. ~ Species, scales = "free",nrow = 6,ncol = 3)

#Betweenness can be log transformed (adding 1)
Edge_Traits%>%
        #filter(Tortuosity<1.5)%>%
        select(c("name","Species","Width","Length","Area","Volume","Resistance_2ave","Tortuosity","Distance",
                 "Accessibility","Betweenness","Route_factor","Or_ij"))%>%
        mutate(Log_Betweenness=log10(Betweenness+1))%>%
        group_by(name)%>%
        gather(key=variable,
               value=value,Width:Log_Betweenness)%>%
        
        filter(variable %in% c("Log_Betweenness","Distance"))%>%#4_Tortuosity has little variation and depends on Length
        ggplot()+
        aes(value,fill=name)+
        geom_histogram()+
        facet_wrap(Species ~ variable, scales = "free",nrow = 6,ncol = 2)+
        theme(legend.position = "none")

#Length can be log transformed and that makes it very normally ditributed
Edge_Traits%>%
        #filter(Tortuosity<1.5)%>%
        select(c("name","Species","Width","Length","Area","Volume","Resistance_2ave","Tortuosity","Distance",
                 "Accessibility","Betweenness","Route_factor","Or_ij"))%>%
        mutate(Log_Length=log10(Length))%>%
        group_by(name)%>%
        gather(key=variable,
               value=value,Width:Log_Length)%>%
        
        filter(variable %in% c("Log_Length","Distance"))%>%#4_Tortuosity has little variation and depends on Length
        ggplot()+
        aes(value,fill=name)+
        geom_histogram()+
        facet_wrap(Species ~ variable, scales = "free",nrow = 6,ncol = 2)+
        theme(legend.position = "none")


#Interesting relationships
Edge_Traits_c<-Edge_Traits

Edge_Traits<-do.call(rbind,
lapply(
split(Edge_Traits,Edge_Traits$name),head))

#Before doing a correlogram of everything with everythin. Here I want to test specific relationships that I think 
#might be interesting biologically

#LENGTH AND WIDTH

# Log10(Width)~Log10(Length)
Log_Length_Width<-#The only think I can tell from this one is that the thinnest hyphae have medium size length. Really long hyphae cannot be thin. There seems to be a lower boundary: the thinnest hypahe get longer up to a point
                  #where it is impossible to go thinniest and long. Note! Assuming this follows the two are connected by a power law
  
#Length_Width<-
  Edge_Traits%>%
  ggplot()+
  aes(x=log10(Width),y=log10(Length),color=name)+
  #aes(x=Width,y=Length,color=name)+
  geom_point(alpha=0.8)+
  facet_wrap(. ~ Species, scales = "free",nrow = 6,ncol = 3)+
  theme(legend.position = "none")


#LENGTH; WIDTH AND DISTANCE

# Log10(Length)~Distance and Length~Distance
Log_Length_Distance<-#Out of this one is clear that there is no relationship between length and sitance
  Edge_Traits%>%
        ggplot()+
        aes(x=Distance,y=log10(Length),color=name)+
        geom_point(alpha=0.8)+
        facet_wrap(. ~ Species, scales = "free",nrow = 6,ncol = 3)+
        theme(legend.position = "none")

Distance_Lengt_Width<-
  Edge_Traits%>%
  filter(Area!=0)%>%
  ggplot()+
  aes(x=Distance,y=Length,color=Width)+
  #aes(Distance,Volume,color=Volume)+
  geom_point()+
  #geom_hex() +
  #geom_bin2d()+
  scale_color_continuous(type = "viridis")+
  #scale_fill_continuous(type = "viridis")+
  theme_bw()+
  facet_wrap(.~Species, scales = "free",nrow = 6,ncol = 3)

library(hexbin)

Distance_Volume<-
        Edge_Traits%>%
        filter(Area!=0)%>%
        ggplot()+
        aes(x=Distance,y=Volume)+
        #aes(Distance,Volume,color=Volume)+
        #geom_point()+
        geom_hex() +
        #geom_bin2d()+
        #scale_color_continuous(type = "viridis")+
        scale_fill_continuous(type = "viridis")+
        theme_bw()+
        facet_wrap(.~Species, scales = "free",nrow = 6,ncol = 3)

#RESISTANCE WIDTH AND LENGTH (THIS IS MAINLY TO TEST THE MATHEMATICAL RELATIONSHIP AMONG THEM)

Resistance_Width_Length<-
Log_Resistance_Width_Length<-
        Edge_Traits%>%
        ggplot()+
        #aes(x=Distance,y=Volume)+
        aes(x=log10(Length),y=log10(Resistance_2ave),color=log10(Width))+
        geom_point()+
        #geom_hex() +
        #geom_bin2d()+
        scale_color_continuous(type = "viridis")+
        #scale_fill_continuous(type = "viridis")+
        theme_bw()+
        facet_wrap(.~Species, scales = "free",nrow = 6,ncol = 3)

#BETWEENNESS AND DISTANCE

Log_Betweenness_Distance<-
        Edge_Traits%>%
        ggplot()+
        aes(y=log10(Betweenness+1),x=Distance)+
        #aes(Distance,Volume,color=Volume)+
        #geom_point()+
        geom_hex() +
        #geom_bin2d()+
        #scale_color_continuous(type = "viridis")+
        scale_fill_continuous(type = "viridis")+
        theme_bw()+
        facet_wrap(.~Species, scales = "free",nrow = 2,ncol = 3)


#ROUTE FACTOR AND WIDTH

RouteFactor_Width<-
  Edge_Traits%>%
  ggplot()+
  aes(y=Width,x=Route_factor,color=name)+
    geom_point()+
  #geom_hex() +
  #geom_bin2d()+
  geom_point(alpha=0.8,size=2)+
  facet_wrap(. ~ Species, scales = "free",nrow = 6,ncol = 3)+
  theme(legend.position = "none")+
  theme_light()


#Testing some 3D plots
library(rayshader)
plot_gg(Distance_Volume)
render_camera(fov = 70, zoom = 0.5, theta = 130, phi = 35)
Sys.sleep(0.2)
render_snapshot(clear = TRUE)

plot_gg(Resistance_Width_Length)
plot_gg(Log_Resistance_Width_Length)
render_camera(fov = 70, zoom = 0.5, theta = 130, phi = 35)
Sys.sleep(0.2)
render_snapshot(clear = TRUE)

cloud(Resistance_2ave~Length,data=Edge_Traits%>%
              filter(Area!=0)%>%
              filter(name=="C34(1)_t11"))






#Correlograms

library(GGally)

Edge_Traits%>%
        
        select(c("Width","Length","Tortuosity","Distance",
                 "Accessibility","Betweenness","Route_factor","Species"))%>%
        ggpairs(title="correlogram with ggpairs()",columns = 1:7,ggplot2::aes(color=Species))







#3. Preparing the functions and data for PCA

#3.1 Loading (and installing) vegan package
#Note: Other functions from other packages are also available to do a PCA. rda from vegan is the one I am used to.


#library(vegan)


#3.2 Subsetting and transforming the data 

#To run a simple PCA with the rda function is necessary to only feed an object of type matrix or dataframe with the 
#varialbes one is interested and that are appropriate for a PCA (statistically speaking). 
#In general PCA is well suited for data that is continuous and that follows more or less normal distribution. 
#This excludes: count data, proportions, categorical variables.

#Using raw data
Hyphae_char_raw<- Edge_Traits[which(Edge_Traits$Type=="E"),#Removing the edges in the feature
        c("Length","Width","Area","Volume","Resistance_2","Tortuosity","Or_ij","Betweenness","Accessibility")]
          #selecting only "descriptive" hyphal parameters that are numeric

#Using transformed data (as described above)
Hyphae_char_trans<-Edge_Traits%>%
        filter(Type=="E")%>%
        select(c("Length","Width","Area","Volume","Resistance_2",
                 "Tortuosity","Or_ij","Betweenness","Accessibility"))%>%
        mutate(Betweenness=log10(Betweenness+0.325))%>%
        mutate(Area=sqrt(Area+0.325))%>%
        mutate(Volume=log10(Volume+0.325))%>%
        mutate_at(c("Length","Resistance_2","Tortuosity"),log10)


#Creating a unique ID
Hyphae_ID<-Edge_Traits[which(Edge_Traits$Type=="E"),c("name","Name")]
Hyphae_ID<-paste(Hyphae_ID$name,Hyphae_ID$Name,sep = "_")
#Adding this ID as rownames
rownames(Hyphae_char_raw)<-Hyphae_ID

rownames(Hyphae_char_trans)<-Hyphae_ID

#3. Performing a PCA on raw data and transformed data

Hyphae_pca<-rda(Hyphae_char_raw,scale = TRUE)
#scale =TRUE standarize variances since each variable has a different scale

Hyphae_pca_trans<-rda(Hyphae_char_trans,scale = TRUE)

#5. Plotting the results

#Vegan comes with its own ploting functions. One is biplot.rda (or simply biplot)
#The plots are not so nice looking. I will use later on other functions

#First create a factor vector with the names of the seven fungi used so far
#Fungal_names<-as.factor(Edge_Traits[which(Edge_Traits$Type=="E"),c("name")])

Fungal_names<-as.factor(Edge_Traits[which(Edge_Traits$Type=="E"),c("Species")])
Fungal_names_<-as.factor(Edge_Traits[which(Edge_Traits$Type=="E"),c("name")])

#And then adding "color" to those fungi
library(RColorBrewer)#You might need to install this package
colvec<-brewer.pal(n = 7, name = "Set1")#This is pure aesthetics, 
#I like the color combinations in "Set1" that come in the package
#RcolorBrewer. I chose 7 color because there are 7 fungi

#5.1 Raw data

#Making a biplot

#The typical biplot contains two axes. These axes are called the "Principal components" (PC). They are mathematically
#derived out variance-covariance matrix from a set of variables. In our case, this variance-covariance matrix comes
#from the table of network traits.

#Regardless of the exact formula behind the calculation of the axes, one can
#think of each axis as a new variable that is composed out of all original variables. This new variable does not have
#units and the actual scale is not that informative. 
#The objective is to reduce the amount of variables so information can be plotted in a simpler way.

#By information, I mean the values contained in the columns and rows in our table of network traits

# The columns are the variables (which in our a case are the network traits like length, betweennes, etc )
# and are usually represented as arrows.
# These arrows indicate:
#   a) how variables are related to each other. You can see this by the angle and direction of the arrows
#   For exampble, arrows going in opposite direction negatively co-vary while
#   arrows orthogonal to each other are independent of each other)
#   b) the contribution of each variable to a given PC. You can see this by the length of each arrow. Thus the longest
#   an arrow is, the more important in building the PC´s. 


# The rows are the experimental units (which in our case is each edge/hypha in the table and are represented as points
# The position of the points reflects (more or less) how similar is an experimental unit to each other. For us it would b
# how similar each edge is to each other.

#However, while making the biplot one cannot display accurately both types of information (ie. how variables are related
#to each other and the position of the experimental units). One has to choose what is the most important thing one would like
#to visualize. I should stress, though, the word accurately. One can always see some aspect either information. It is just
#that one is favoring one over the other. In the comments below I mark where these decisions are made

#Visualizing how variables are related to each other as arrows

biplot(Hyphae_pca,
        display = "species",#here, display allows you to chose what will be displayed as arrows.
       #There are two options:
       #-"species" refers to columns (in our case the network traits), 
       #-"sites" refer to rows which in our case would correspond to a single hypha. 
       # In our case (as in most cases), the colums (the traits) is what we want to see. Thus "species"
       scaling = 2, #here is where one needs to decide what will be most accurately displayed.
                    # 1 is for the position of the points (in our cases the edges/hypha)
                    # 2 is for the angles and length of the arrows (in our case the network traits)
       
       type = "text")#This add text at the end of the rows indicating the name fo the variables

#Visualizing each edge as a point
points(Hyphae_pca,
       display = "sites", #as indicated above
       col = colvec[Fungal_names],
       scaling = 2,#as indicated above
       pch = 21, 
       bg = colvec[Fungal_names])

#Adding a lenged with the names of the fungi (colored)
legend("topright", legend = levels(Fungal_names), bty = "n",
       col = colvec, pch = 21, pt.bg = colvec,xjust=1,pt.cex = 2)

#5.2. Transformed data (the same as above, just using the transformed data table)

#The network traits as arrows
biplot(Hyphae_pca_trans,
       scaling = 2,
       display = "species", 
       type = "text")

#The edges as points
points(Hyphae_pca_trans, 
       display = "sites", 
       col = colvec[Fungal_names],
       scaling = 2,
       pch = 21,
       bg = colvec[Fungal_names])

#Adding a lenged with the names of the fungi (colored)
legend("topright", legend = levels(Fungal_names), bty = "n",
       col = colvec, pch = 21, pt.bg = colvec,xjust=1,pt.cex = 2)

#What are we learning from the biplots

#Relationships among network traits (from the arrows)

#-Length and width are uncorrelated (arrows are almost orthogonal)
#-Resistance, tortuosity and length covary negatively with angle, accessibility and betweenness
#-Area and width are (not surprinsingly) strongly correlated
#- Volume increases with as length and width increases (again not surpringingly)
#-Almost all variables equally contribute to buildings the PC´s except for Accessibility and angle that
#contribute very little

#Position of the points

#-the most striking thing is that all fungi seem to be very close to each other


#Given all points cluster very tightly I am going to do a visualizing trick to space them apart to each other

biplot(Hyphae_pca_trans,
       scaling = 2,
       display = "species", 
       type = "text")

#Adding the spaced points
colonies<-scores(Hyphae_pca_trans,display = "sites")
points(colonies[,1]*10,colonies[,2]*10,
       #display = "sites", 
       #col = colvec[Fungal_names],
       col = colvec[Fungal_names_],
       #scaling = 2, 
       pch = 21, 
       bg = colvec[Fungal_names_])
       #bg = colvec[Fungal_names])

#Adding a lenged with the names of the fungi (colored)
legend("topright", legend = levels(Fungal_names), bty = "n",
       col = colvec, pch = 21, pt.bg = colvec,xjust=1,pt.cex = 2)



install.packages("ggfortify")
library(ggfortify)

autoplot(prcomp(Hyphae_char_trans,scale. = T),
                data=Edge_Traits[which(Edge_Traits$Type=="E"),],
                colour="Species",alpha=0.3,loadings=T,
         loadings.label=T,loadings.colour="black"
                )

### Statistical analysis

#The analysis below (MANOVA and PERMANOVA) test whether values coming from experimental units 
#grouped into distinct categoeries differed (statiscially speaking) from a null hypothesis
#where all experimental untis come from the same group.

#For my specific dataset, the question is whether the values coming from different species 
#differ significantly from the null hypothesis that species identity does not matter and instead
#all values can be seen as coming from the same species. 

#This is the same idea as an univariate ANOVA where one test whether the means among groups are statitically
#different from the grand mean. The difference in multivariat statistics is that instead of focusing on differences
#among individual means; one test whether the "centroids" among groups statistically differ from an single "centroid"

#One can think of a "centroid" as a multivariate mean and differences are measured in terms of distances in mutivariate
#space. Thus one needs to select which distance is the most appropriate. In this analysis (I think) the best choice
#is the euclidean distance among centroids.


# 1.  MANOVA

#MANOVA is parametric analyis where p values are obtained by contrasting the results to a known distribution (i.e. the normal distribution)
#As in ANOVA, it is important in this case to follow a normal distribution (which we do not). Still it would 
#be informative the outcome of the analysis

#Given Area is tightly correlated with width I will not use it in the analysis anymore (that is,
#I am only using width)

Hyphae_char_trans_m<-Hyphae_char_trans
Hyphae_char_trans_m$Area<-NULL
Hyphae_char_trans_m$Species<-as.factor(Edge_Traits[which(Edge_Traits$Type=="E"),c("Species")])
Hyphae_char_trans_m$Replicate<-as.factor(Edge_Traits[which(Edge_Traits$Type=="E"),c("name")])

manova_analysis <- manova(
                        cbind(Length,Width,Volume,Resistance_2,Tortuosity,Or_ij,Betweenness,Accessibility)~
                                Fungus, Hyphae_char_trans_m)

summary(manova_analysis)
anova(manova_analysis)

# 2. PERMANOVA

#PERMANOVA is the equivalent non-parametric permutation based counterpart of MANOVA. Here p values are obtained 
#by a distribution coming from permutation of the original data. As such there is no need to follow a standard
#distribution.


#NOTE: In principle PERMANOVA could also be done using data coming from each edge (as I have done so far).
#However, because it relies in permutation my laptop does not have enough capacity to run it (I tried!)
#Such analyss coud be run as follows (if someone wants to give it a try):
adonis(Hyphae_char_trans_m[,c(1:8)] ~ Fungus, data=Hyphae_char_trans_m,method = "euclidean", permutations=99)
#in my computer it produces an error becuase it is too lage


#For this reason, I am now doing an analysis using only the means for each variable per fungal species

#2.1. Creating a table of means
Fungi_means<-
        do.call("rbind",
                lapply(
                split(Hyphae_char_trans_m[,c("Length","Width","Volume","Resistance_2",
                                         "Tortuosity","Or_ij","Betweenness")],Hyphae_char_trans_m$Replicate),
                sapply,mean))
#tranforming it into a dataframe
Fungi_means<-as.data.frame(Fungi_means)
#and adding a column with the names of the fungi
Fungi_means$Replicate<-rownames(Fungi_means)
Fungi_means$Species<-NA
Fungi_means$Species[grep("C34",Fungi_means$Replicate)]<-"Mortierella elongata"
Fungi_means$Species[grep("C35",Fungi_means$Replicate)]<-"Umbelopsis isabellina"
Fungi_means$Species[grep("DF19",Fungi_means$Replicate)]<-"Mortierella alpina"
Fungi_means$Species[grep("DF25",Fungi_means$Replicate)]<-"Mortierella elongata2"
Fungi_means$Species[grep("DF56",Fungi_means$Replicate)]<-"Mucor fragilis"
Fungi_means$Species[grep("M",Fungi_means$Replicate)]<-"Mortierella alpina2"

adonis(Fungi_means[,c("Length","Width","Volume","Resistance_2",
                      "Tortuosity","Or_ij","Betweenness")] ~ Species, data=Fungi_means,
       method = "euclidean", permutations=99)


p<-
autoplot(prcomp(Fungi_means[,c(1:7)],scale. = T),
         data=Fungi_means,
         colour="Species",loadings=T,size=10,shape="Species",
         loadings.label=T,loadings.colour="blue",loadings.label.size=5
)

p+
        scale_color_brewer(palette="Set1")+
        scale_shape_manual(values = c(15,16,17,18,19,11))+
        theme(title = element_text(size = 18),
              #axis.title.x=element_blank(),
              axis.text.x = element_text(size = 20,angle = 45,hjust = 1),
              axis.text.y = element_text(size = 20),
              strip.text.x = element_text(size = 20),
              )#legend.position = "none")
        
#Add Omaj-min from nodes dataset, include Accessibility

#The above example is not that meaningful because I do not have replication per each colony. To this point I have only
#worked with a signle colony per fungal species. As I extract more network data from more colonies this analysis 
#will become more meaningful.

# Use one of the ‘join’ functions in dplyr to merge with the rest of your data.
# The code I shared here used forward.sel() for deciding which variables to use, which I think is depreciated. Use ordistep(), you can find an example in the ‘analysis of microbial communities’ document for R.



# tree<-read.tree(‘eucalyptTree.phy’)
# tree.dist<-as.dist(cophenetic(tree))  # don’t forget to convert to class ‘dist’, otherwise treats it as raw data and runs it through vegdist()
# tree.pco<-capscale(tree.dist~1)  # principle coordinates matrix is in scores(tree.pco,1:8,‘sites’)
# tree.scores<-scores(tree.pco,1:8,‘sites’)
# For reading tree and converting it principle coordinates. I misremembered, PCNM is only for the spatial data, not the phylogenetic data.


data(dune)
data(dune.env)
###############################################


#OLD

# #Right skewed (but not so much)
# Re_org%>%
#         filter(variable %in% c("Accessibility","Area","Length","Or_ij","Width"))%>%
#         ggplot()+
#         aes(value,fill=name)+
#         geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
#         geom_density(alpha=0.6)+
#         facet_wrap(name ~ variable, scales = "free",nrow = 7,ncol = 5)+
#         theme(legend.position = "none",
#               strip.text.x = element_blank())#This is just to remove the title on each plot. I do this to save those 
# #then add them manually in illustrator or inkscape
# 
# #Highly right skewed (like Poisson distribution)
# Re_org%>%
#         filter(variable %in% c("Betweenness","Resistance_2","Tortuosity","Volume"))%>%
#         ggplot()+
#         aes(value,fill=name)+
#         geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
#         geom_density(alpha=0.6)+
#         facet_wrap(name ~ variable, scales = "free",nrow = 7,ncol = 4)+
#         theme(legend.position = "none",
#               strip.text.x = element_blank())#This is just to remove the title on each plot. I do this to save those 
# #then add them manually in illustrator or inkscape

# 
# #Visualizng density plots
# Re_org%>%
#   ggplot()+
#   aes(value,fill=name)+
#   geom_density(alpha=0.6)+
#   facet_wrap(Species ~ variable, scales = "free",nrow = 6,ncol = 11)+
#   theme(legend.position = "none")
# 
# #Combining histograms and density plots
# Re_org%>%
#   ggplot()+
#   aes(value,fill=name)+
#   geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
#   geom_density(alpha=0.6)+
#   facet_wrap(Species ~ variable, scales = "free",nrow = 6,ncol = 11)+
#   theme(legend.position = "none",
#         strip.text.x = element_blank())#because I want to save it I am removing the labels in eachpanel,
# #then I can use illustrator or inkscape to add better lables
# 
# 
# 
# 
# 
# 
# 
# 
# #So, I tried the log10 (Betweenness,Length,Volume,Reistance,Tortuosity) and sqrt (Area) tranformation. 
# ##It does not make it normal but it helps making them less skewed:
# 
# 
# Edge_Traits%>%
#   filter(Type=="E")%>%
#   select(c("name","Length","Width","Area","Volume","Resistance_2",
#            "Tortuosity","Or_ij","Betweenness","Accessibility"))%>%
#   mutate(Betweenness=log10(Betweenness+0.325))%>%
#   mutate(Area=sqrt(Area+0.325))%>%
#   mutate(Volume=log10(Volume+0.325))%>%
#   mutate_at(c("Length","Resistance_2","Tortuosity"),log10)%>%
#   group_by(name)%>%
#   gather(key=variable,
#          value=value,Length:Accessibility)%>%
#   ggplot()+
#   aes(value,fill=name)+
#   #geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
#   geom_density(alpha=0.6)+
#   facet_wrap(name ~ variable, scales = "free",nrow = 7,ncol = 9)+
#   theme(legend.position = "none")
