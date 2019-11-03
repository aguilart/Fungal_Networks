################################################################################################################
################################################################################################################
##################    PROTOCOL TO ANALYZE NETWORK PARAMETER USING MULTIVARITE ANALYSIS  ########################
################################################################################################################
################################################################################################################

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
temp = list.files(path="processedData\\",pattern="*.csv")
temp<-paste("processedData\\",temp,sep = "")
myfiles = lapply(temp,function(x){read.csv(x,header = TRUE,stringsAsFactors = FALSE)} )
Fung_Netw_Traits<-do.call(rbind,myfiles)
#So, Fung_Netw_Traits contains edge data for all 7 species

#2. Getting to know the data.

#2.1 How R sees the data. It is useful to have an idea on how R recognize the data. R is an "object oriented"
#program language. That is, every data is an object which has certain properties. R uses these properties to 
#categorized the object. It is useful to know what kind of object is your data because some functions only work
#on one object type but not in others.

str(Fung_Netw_Traits)
#From this one learns that the entire table is a "dataframe" object composed of 47 vectors which are of different type
# For example the vector "name" is a character vector, "channel" is an integer (only whole numbers), and "length" is 
# a "numerical" vector.


#2.2. Looking at the distribution of the data. More impotantly, what is the distribution of the data? 

#To simply get means, min, max per fungal species:
aggregate(
        Fung_Netw_Traits[which(Fung_Netw_Traits$Type=="E"),#Removing the edges in the feature
                         c("Length","Width","Area","Volume","Resistance_2","Tortuosity","Or_ij","Betweenness","Accessibility")],
        by=list(Fung_Netw_Traits[which(Fung_Netw_Traits$Type=="E"),]$name),
        range)#Here you  can write different functions: min, max, median. range and summmary is also good because it
                #gets you more than one summary statistics, but it can be confusing with so much data.


#Visualinzing as simple historgrams helps.
#To make those, I use the suite of functions in the package tidyverse (check if it needs to be installed).

library(tidyverse)

#Reorganizing the dataframe so the figures are made easier

Re_org<-
        Fung_Netw_Traits%>%
        filter(Type=="E")%>%
        select(c("name","Length","Width","Area","Volume","Resistance_2",
                 "Tortuosity","Or_ij","Betweenness","Accessibility"))%>%
        group_by(name)%>%
        gather(key=variable,
               value=value,Length:Accessibility)
        
        

#Visualizng simple histograms (because there are too many variables, zoom the graphic to better visualize)
Re_org%>%
        ggplot()+
        aes(value,fill=name)+
        geom_histogram()+
        facet_wrap(name ~ variable, scales = "free",nrow = 7,ncol = 9)+
        theme(legend.position = "none")

#Visualizng density plots
Re_org%>%
        ggplot()+
        aes(value,fill=name)+
        geom_density(alpha=0.6)+
        facet_wrap(name ~ variable, scales = "free",nrow = 7,ncol = 9)+
        theme(legend.position = "none")

#Combining histograms and density plots
Re_org%>%
        ggplot()+
        aes(value,fill=name)+
        geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
        geom_density(alpha=0.6)+
        facet_wrap(name ~ variable, scales = "free",nrow = 7,ncol = 9)+
        theme(legend.position = "none",
              strip.text.x = element_blank())#because I want to save it I am removing the labels in eachpanel,
                                            #then I can use illustrator or inkscape to add better lables
#Form these plots one learns that:
#       -The distribution of the data for a  variable is similar across ose species.
#       -Accessibility, Area, Betweenness, Length, Resistance, toruosity, volume are hihgly right skewed. 
#       -Orientation has three peaks (this one will be tricky)
#       -Width is left skewed as well but also bimodal

#The following are the same plots, just splitted 

#Right skewed (but not so much)
Re_org%>%
        filter(variable %in% c("Accessibility","Area","Length","Or_ij","Width"))%>%
        ggplot()+
        aes(value,fill=name)+
        geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
        geom_density(alpha=0.6)+
        facet_wrap(name ~ variable, scales = "free",nrow = 7,ncol = 5)+
        theme(legend.position = "none",
              strip.text.x = element_blank())#This is just to remove the title on each plot. I do this to save those 
                                                #then add them manually in illustrator or inkscape

#Highly right skewed (like Poisson distribution)
Re_org%>%
        filter(variable %in% c("Betweenness","Resistance_2","Tortuosity","Volume"))%>%
        ggplot()+
        aes(value,fill=name)+
        geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
        geom_density(alpha=0.6)+
        facet_wrap(name ~ variable, scales = "free",nrow = 7,ncol = 4)+
        theme(legend.position = "none",
              strip.text.x = element_blank())#This is just to remove the title on each plot. I do this to save those 
                                                #then add them manually in illustrator or inkscape


#So, I tried the log10 (Betweenness,Length,Volume,Reistance,Tortuosity) and sqrt (Area) tranformation. 
##It does not make it normal but it helps making them less skewed:


Fung_Netw_Traits%>%
        filter(Type=="E")%>%
        select(c("name","Length","Width","Area","Volume","Resistance_2",
                 "Tortuosity","Or_ij","Betweenness","Accessibility"))%>%
        mutate(Betweenness=log10(Betweenness+0.325))%>%
        mutate(Area=sqrt(Area+0.325))%>%
        mutate(Volume=log10(Volume+0.325))%>%
        mutate_at(c("Length","Resistance_2","Tortuosity"),log10)%>%
        group_by(name)%>%
        gather(key=variable,
               value=value,Length:Accessibility)%>%
        ggplot()+
        aes(value,fill=name)+
        #geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
        geom_density(alpha=0.6)+
        facet_wrap(name ~ variable, scales = "free",nrow = 7,ncol = 9)+
        theme(legend.position = "none")



#3. Preparing the functions and data for PCA

#3.1 Loading (and installing) vegan package
#Note: Other functions from other packages are also available to do a PCA. rda from vegan is the one I am used to.


library(vegan)


#3.2 Subsetting and transforming the data 

#To run a simple PCA with the rda function is necessary to only feed an object of type matrix or dataframe with the 
#varialbes one is interested and that are appropriate for a PCA (statistically speaking). 
#In general PCA is well suited for data that is continuous and that follows more or less normal distribution. 
#This excludes: count data, proportions, categorical variables.

#Using raw data
Hyphae_char_raw<- Fung_Netw_Traits[which(Fung_Netw_Traits$Type=="E"),#Removing the edges in the feature
        c("Length","Width","Area","Volume","Resistance_2","Tortuosity","Or_ij","Betweenness","Accessibility")]
          #selecting only "descriptive" hyphal parameters that are numeric

#Using transformed data (as described above)
Hyphae_char_trans<-Fung_Netw_Traits%>%
        filter(Type=="E")%>%
        select(c("Length","Width","Area","Volume","Resistance_2",
                 "Tortuosity","Or_ij","Betweenness","Accessibility"))%>%
        mutate(Betweenness=log10(Betweenness+0.325))%>%
        mutate(Area=sqrt(Area+0.325))%>%
        mutate(Volume=log10(Volume+0.325))%>%
        mutate_at(c("Length","Resistance_2","Tortuosity"),log10)


#Creating a unique ID
Hyphae_ID<-Fung_Netw_Traits[which(Fung_Netw_Traits$Type=="E"),c("name","Name")]
Hyphae_ID<-paste(Hyphae_ID$name,Hyphae_ID$Name,sep = "_")
#Adding this ID as rownames
rownames(Hyphae_char_raw)<-Hyphae_ID

rownames(Hyphae_char_trans)<-Hyphae_ID

#3. Performing a PCA

Hyphae_pca<-rda(Hyphae_char_raw,scale = TRUE)
#scale =TRUE standarize variances since each variable has a different scale

Hyphae_pca_trans<-rda(Hyphae_char_trans,scale = TRUE)

#5. Plotting the results

#Vegan comes with its own ploting functions. One is biplot.rda (or simply biplot)
#The plots are not so nice looking. I will use later on other functions

#First create a factor vector with the names of the seven fungi used so far
Fungi<-as.factor(Fung_Netw_Traits[which(Fung_Netw_Traits$Type=="E"),c("name")])
#And then adding "color" to those fungi
library(RColorBrewer)#You might need to install this package
colvec<-brewer.pal(n = 7, name = "Set1")#This is pure aesthetics, 
#I like the color combinations in "Set1" that come in the package
#RcolorBrewer. I chose 7 color because there are 7 fungi

#5.1 Raw data
#Making the basic PCA plot
biplot(Hyphae_pca,scaling = 2,
        display = "species", 
        type = "text")
       
#Adding a lenged with the names of the fungi (colored)
legend("topright", legend = levels(Fungi), bty = "n",
       col = colvec, pch = 21, pt.bg = colvec,xjust=1,pt.cex = 2)

# Adding the points, which in this case correspon to each hyphae
points(Hyphae_pca, display = "sites", 
       col = colvec[Fungi],
       scaling = 2, pch = 21, bg = colvec[Fungi])

#5.2. Transformed data
#Making the basic PCA plot
biplot(Hyphae_pca_trans,scaling = 2,
       display = "species", 
       type = "text")

#Adding a lenged with the names of the fungi (colored)
legend("topright", legend = levels(Fungi), bty = "n",
       col = colvec, pch = 21, pt.bg = colvec,xjust=1,pt.cex = 2)

#Adding the points, which in this case correspon to each hyphae
points(Hyphae_pca, display = "sites", 
       col = colvec[Fungi],
       scaling = 2, pch = 21, bg = colvec[Fungi])
