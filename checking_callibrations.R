#Freyas Data (calibrated myself)
testing<-Edge_Traits[which(Edge_Traits$name%in%Edge_Traits_Bn$name),c("name","Length","Length_scaled","Width","Width_scaled")]

all_data_scaled<-readRDS("all_data_scaled.RDS")

testing_area<-all_data_scaled[which(all_data_scaled$name_col%in%Edge_Traits_Bn$name),]

#Marc 1 (uncalibrated)
temp = list.files(path="BasidiomyceteData\\",pattern="*-Edge.csv")
temp<-paste("BasidiomyceteData\\",temp,sep = "")
Edge_Traits_M1 = lapply(temp,function(x){read.csv2(x,header = TRUE,stringsAsFactors = FALSE)} )
Edge_Traits_M1<-bind_rows(Edge_Traits_M1)
Edge_Traits_M1<-Edge_Traits_M1[which(Edge_Traits_M1$name%in%testing$name),]

Edge_Traits_M1$Length_scaled<-NA
Edge_Traits_M1$Length_scaled[grep("Pi_ctrl1_d8_1",Edge_Traits_M1$name)]<-Edge_Traits_M1$Length[grep("Pi_ctrl1_d8_1",Edge_Traits_M1$name)]*155
Edge_Traits_M1$Length_scaled[grep("Pv_ctrl1_d8_1",Edge_Traits_M1$name)]<-Edge_Traits_M1$Length[grep("Pv_ctrl1_d8_1",Edge_Traits_M1$name)]*80
Edge_Traits_M1$Length_scaled[grep("Rb_ctrl1_d8_1",Edge_Traits_M1$name)]<-Edge_Traits_M1$Length[grep("Rb_ctrl1_d8_1",Edge_Traits_M1$name)]*88

Edge_Traits_M1$Width_scaled<-NA
Edge_Traits_M1$Width_scaled[grep("Pi_ctrl1_d8_1",Edge_Traits_M1$name)]<-Edge_Traits_M1$Width[grep("Pi_ctrl1_d8_1",Edge_Traits_M1$name)]*155
Edge_Traits_M1$Width_scaled[grep("Pv_ctrl1_d8_1",Edge_Traits_M1$name)]<-Edge_Traits_M1$Width[grep("Pv_ctrl1_d8_1",Edge_Traits_M1$name)]*80
Edge_Traits_M1$Width_scaled[grep("Rb_ctrl1_d8_1",Edge_Traits_M1$name)]<-Edge_Traits_M1$Width[grep("Rb_ctrl1_d8_1",Edge_Traits_M1$name)]*88


temp = list.files(path="BasidiomyceteData\\",pattern="*-summaryTable.csv")
temp<-paste("BasidiomyceteData\\",temp,sep = "")
summary_Traits_M1 = lapply(temp,function(x){read.csv2(x,header = TRUE,stringsAsFactors = FALSE)} )

summary_Traits_M1<-bind_rows(summary_Traits_M1)
summary_Traits_M1$name_col<-gsub("BasidiomyceteData\\\\+","",temp)
summary_Traits_M1$name_col<-gsub("\\-summaryTable\\.csv","",summary_Traits_M1$name)
summary_Traits_M1<-summary_Traits_M1[which(summary_Traits_M1$name_col%in%testing$name),]

#Marc 2 (calibrated 90)
temp = list.files(path="BasidiomyceteData_calibrated\\",pattern="*-Edge.csv")
temp<-paste("BasidiomyceteData_calibrated\\",temp,sep = "")
Edge_Traits_M90 = lapply(temp,function(x){read.csv2(x,header = TRUE,stringsAsFactors = FALSE)} )
Edge_Traits_M90<-bind_rows(Edge_Traits_M90)

Edge_Traits_M90$Length_scaled<-NA
Edge_Traits_M90$Length_scaled[grep("Pi_ctrl1_d8_1",Edge_Traits_M90$name)]<-Edge_Traits_M90$Length[grep("Pi_ctrl1_d8_1",Edge_Traits_M90$name)]*1.73
Edge_Traits_M90$Length_scaled[grep("Pv_ctrl1_d8_1",Edge_Traits_M90$name)]<-Edge_Traits_M90$Length[grep("Pv_ctrl1_d8_1",Edge_Traits_M90$name)]*0.89
Edge_Traits_M90$Length_scaled[grep("Rb_ctrl1_d8_1",Edge_Traits_M90$name)]<-Edge_Traits_M90$Length[grep("Rb_ctrl1_d8_1",Edge_Traits_M90$name)]*0.98


Edge_Traits_M90$Width_scaled<-NA
Edge_Traits_M90$Width_scaled[grep("Pi_ctrl1_d8_1",Edge_Traits_M90$name)]<-Edge_Traits_M90$Width[grep("Pi_ctrl1_d8_1",Edge_Traits_M90$name)]*1.73
Edge_Traits_M90$Width_scaled[grep("Pv_ctrl1_d8_1",Edge_Traits_M90$name)]<-Edge_Traits_M90$Width[grep("Pv_ctrl1_d8_1",Edge_Traits_M90$name)]*0.89
Edge_Traits_M90$Width_scaled[grep("Rb_ctrl1_d8_1",Edge_Traits_M90$name)]<-Edge_Traits_M90$Width[grep("Rb_ctrl1_d8_1",Edge_Traits_M90$name)]*0.98


temp = list.files(path="BasidiomyceteData_calibrated\\",pattern="*-summaryTable.csv")
temp<-paste("BasidiomyceteData_calibrated\\",temp,sep = "")
summary_Traits_M90 = lapply(temp,function(x){read.csv2(x,header = TRUE,stringsAsFactors = FALSE)} )

summary_Traits_M90<-bind_rows(summary_Traits_M90)
summary_Traits_M90$name_col<-gsub("BasidiomyceteData_calibrated\\\\+","",temp)
summary_Traits_M90$name_col<-gsub("\\-summaryTable\\.csv","",summary_Traits_M90$name)

#Marc 3 (calibrated ind)
temp = list.files(path="BasidiomyceteData_calibrated2\\",pattern="*-Edge.csv")
temp<-paste("BasidiomyceteData_calibrated2\\",temp,sep = "")
Edge_Traits_M_ind = lapply(temp,function(x){read.csv2(x,header = TRUE,stringsAsFactors = FALSE)} )
Edge_Traits_M_ind<-bind_rows(Edge_Traits_M_ind)

Edge_Traits_M_ind$Length_scaled<-Edge_Traits_M_ind$Length
#Edge_Traits_M_ind$Length_scaled[grep("Pi_ctrl1_d8_1",Edge_Traits_M_ind$name)]<-Edge_Traits_M_ind$Length[grep("Pi_ctrl1_d8_1",Edge_Traits_M_ind$name)]*155
#Edge_Traits_M_ind$Length_scaled[grep("Pv_ctrl1_d8_1",Edge_Traits_M_ind$name)]<-Edge_Traits_M_ind$Length[grep("Pv_ctrl1_d8_1",Edge_Traits_M_ind$name)]*80
#Edge_Traits_M_ind$Length_scaled[grep("Rb_ctrl1_d8_1",Edge_Traits_M_ind$name)]<-Edge_Traits_M_ind$Length[grep("Rb_ctrl1_d8_1",Edge_Traits_M_ind$name)]*88


Edge_Traits_M_ind$Width_scaled<-Edge_Traits_M_ind$Width
#Edge_Traits_M_ind$Width_scaled[grep("Pi_ctrl1_d8_1",Edge_Traits_M90$name)]<-Edge_Traits_M90$Width[grep("Pi_ctrl1_d8_1",Edge_Traits_M90$name)]*155
#Edge_Traits_M_ind$Width_scaled[grep("Pv_ctrl1_d8_1",Edge_Traits_M90$name)]<-Edge_Traits_M90$Width[grep("Pv_ctrl1_d8_1",Edge_Traits_M90$name)]*80
#Edge_Traits_M_ind$Width_scaled[grep("Rb_ctrl1_d8_1",Edge_Traits_M90$name)]<-Edge_Traits_M90$Width[grep("Rb_ctrl1_d8_1",Edge_Traits_M90$name)]*88


temp = list.files(path="BasidiomyceteData_calibrated2\\",pattern="*-summaryTable.csv")
temp<-paste("BasidiomyceteData_calibrated2\\",temp,sep = "")
summary_Traits_M_ind = lapply(temp,function(x){read.csv2(x,header = TRUE,stringsAsFactors = FALSE)} )

summary_Traits_M_ind<-bind_rows(summary_Traits_M_ind)
summary_Traits_M_ind$name_col<-gsub("BasidiomyceteData_calibrated2\\\\+","",temp)
summary_Traits_M_ind$name_col<-gsub("\\-summaryTable\\.csv","",summary_Traits_M_ind$name)


testing$batch<-"1_Freyas"
Edge_Traits_M1$batch<-"2_Mark unc."
Edge_Traits_M90$batch<-"3_Mark 90"
Edge_Traits_M_ind$batch<-"4_Mark ind"

#Lengths

bind_rows(testing[,c("name","Length","Width","batch")],
          Edge_Traits_M1[,c("name","Length","Width","batch")],
          Edge_Traits_M90[,c("name","Length","Width","batch")],
          Edge_Traits_M_ind[,c("name","Length","Width","batch")]) %>% 
  ggplot()+
  aes(x=name,y=Length,fill=batch)+
  geom_boxplot()+
  ggtitle(label = "Length raw")

bind_rows(testing[,c("name","Length_scaled","Width","batch")],
           Edge_Traits_M1[,c("name","Length_scaled","Width","batch")],
          Edge_Traits_M90[,c("name","Length_scaled","Width","batch")],
          Edge_Traits_M_ind[,c("name","Length_scaled","Width","batch")]) %>% 
  ggplot()+
  aes(x=name,y=Length_scaled,fill=batch)+
  geom_boxplot()+
  ggtitle(label = "Length all scaled")


#Widths

bind_rows(testing[,c("name","Length","Width","batch")],
          Edge_Traits_M1[,c("name","Length","Width","batch")],
          Edge_Traits_M90[,c("name","Length","Width","batch")],
          Edge_Traits_M_ind[,c("name","Length","Width","batch")]) %>% 
  ggplot()+
  aes(x=name,y=Width,fill=batch)+
  geom_boxplot()+
  ggtitle(label = "Width raw")

bind_rows(testing[,c("name","Length_scaled","Width_scaled","batch")],
          Edge_Traits_M1[,c("name","Length_scaled","Width_scaled","batch")],
          Edge_Traits_M90[,c("name","Length_scaled","Width_scaled","batch")],
          Edge_Traits_M_ind[,c("name","Length_scaled","Width_scaled","batch")]) %>% 
  ggplot()+
  aes(x=name,y=Width_scaled,fill=batch)+
  geom_boxplot()+
  ggtitle(label = "Width all scaled")

#Areas
testing_area$batch<-"1_Freyas"
summary_Traits_M1$batch<-"2_Mark unc."
summary_Traits_M90$batch<-"3_Mark 90"
summary_Traits_M_ind$batch<-"4_Mark ind"

names(summary_Traits_M1)[72]<-"Mycelial_area"
names(summary_Traits_M90)[73]<-"Mycelial_area"
names(summary_Traits_M_ind)[73]<-"Mycelial_area"

#Area: 155^2micrometer squres= 0.024025 milimeter square
testing_area$Mycelial_area_scaled<-testing_area$Mycelial_area*c(0.024025,0.0064,0.0077)
summary_Traits_M1$Mycelial_area_scaled<-summary_Traits_M1$Mycelial_area*c(0.024025/0.0081,0.0064/0.0081,0.0077/0.0081)
summary_Traits_M90$Mycelial_area_scaled<-summary_Traits_M90$Mycelial_area*c(0.024025/0.0081,0.0064/0.0081,0.0077/0.0081)
summary_Traits_M_ind$Mycelial_area_scaled<-summary_Traits_M_ind$Mycelial_area



bind_rows(testing_area[,c("name_col","Mycelial_area","batch")],
          summary_Traits_M1[,c("name_col","Mycelial_area","batch")],
          summary_Traits_M90[,c("name_col","Mycelial_area","batch")],
          summary_Traits_M_ind[,c("name_col","Mycelial_area","batch")]) %>% 
  ggplot()+
  aes(x=batch,y=Mycelial_area,col=batch)+
  geom_jitter(size=5)+
  facet_grid(.~name_col)+
  ggtitle(label = "Area raw")


bind_rows(testing_area[,c("name_col","Mycelial_area_scaled","batch")],
          summary_Traits_M1[,c("name_col","Mycelial_area_scaled","batch")],
          summary_Traits_M90[,c("name_col","Mycelial_area_scaled","batch")],
          summary_Traits_M_ind[,c("name_col","Mycelial_area_scaled","batch")]) %>% 
  ggplot()+
  aes(x=batch,y=Mycelial_area_scaled,col=batch)+
  geom_jitter(size=5)+
  facet_grid(.~name_col)+
  ggtitle(label = "Area all scaled")


#Testing calllibrations in microscopic fungi

#This one correspond to a run of C41(1)_t25_c that I used the right micrometer/pixel. Erroneously
#I used 0.79, but the right one was 2.587
Edge_C41_1_n<-read.csv2("processedData_New_temp\\C41(1)_t25_c-Edge.csv",header = TRUE,stringsAsFactors = FALSE)
Edge_C41_1_o<-Edge_Traits_AZ[[5]]

Node_C41_1_n<-read.csv2("processedData_New_temp\\C41(1)_t25_c-Node.csv",header = TRUE,stringsAsFactors = FALSE)
Node_C41_1_o<-Node_Traits_AZ[[5]]

summary_C41_1_n<-read.csv2("processedData_New_temp\\C41(1)_t25_c-summaryTable.csv",header = TRUE,stringsAsFactors = FALSE)
summary_C41_1_o<-summary_Traits_AZ[5,]


all(Edge_C41_1_n$Length==Edge_C41_1_o$Length)

all(Node_C41_1_n$node_Accessibility==Node_C41_1_o$node_Accessibility)

summary_C41_1_n$summary_mean_area/(summary_C41_1_o$summary_mean_area)

#Here it shows that for area, I need first to divided by 0.79^2 and then multiply it by 2.587^2. The 1e+06
#corresponds to the conversion factor for square micrometers to square milimeters
summary_C41_1_n$summary_mean_area/
((2.587^2/1e+06)*(summary_C41_1_o$summary_mean_area/(0.6241/1e+06)))

#simplifying:
summary_C41_1_n$summary_mean_area/
  ((2.587^2)*(summary_C41_1_o$summary_mean_area/(0.6241)))

#Understanding resistance

#It turns out that resistance_2, the one that it is used to estimate distances in the graph
#and also the MST does not correlate with resistance_2ave. But this only happens with the 
#microscopic fungi. Mark explains the reason in an email for this. In principle the two values
#should corrrelate well, and it seems the discrpancy is due to some decission in the calculat
#ion of resistance_2. he already corrected that in a newer version of the app. However as for
#3rd or March 2021, I am relying on results from a previous version, thus I had to verify
#the calculation of resistance 2ave and use it to re-esitmate distances for all networks.
#Below is the explanation on how I came up with the "recallibration" decission Used in 
#Assembling_Network_Trait_Data.Rmd

#Here one can tell what the problem is:
plot(Edge_Traits_B[[1]]$Resistance_2,Edge_Traits_B[[1]]$Resistance_2ave)#Here is ok
plot(Edge_Traits_AZ[[1]]$Resistance_2,Edge_Traits_AZ[[1]]$Resistance_2ave)#Here is not!

#So, it is left to ask Mark why this would be the problem only with mines!

hist(Edge_C41_1_n$Resistance_2[which(Edge_C41_1_n$Resistance_2<5000)])

####

#Excluding feature
plot(Edge_C41_1_n$Resistance_2[which(Edge_C41_1_n$Type=="E")],
     Edge_C41_1_n$Resistance_2ave[which(Edge_C41_1_n$Type=="E")])

#How to calibrate

#First checking that Resistance_2ave is calculated as Length/Width^2
plot((Edge_C41_1_n$Length[-2226]/(Edge_C41_1_n$Width[-2226]/2)^2),Edge_C41_1_n$Resistance_2ave[-2226])

unique((Edge_C41_1_n$Length[-2226]/(Edge_C41_1_n$Width[-2226]/2)^2)/Edge_C41_1_n$Resistance_2ave[-2226])
#From this one can tell that the relationship is not 1:1 but instead it needs to get 
#multiplied by pi/8. This makes sense because as Mark explained later in an email:
# that resistance_2ave was originally calculated as 8*Length(pi*r^2) because he was trying to
#correct for resistance flow. Somehow this is not necessary anymore

(Edge_C41_1_n$Length[-2226]/(Edge_C41_1_n$Width[-2226]/2)^2)*(8/pi)/
     (Edge_C41_1_n$Resistance_2ave[-2226])#this one here shows that resitance2ave was not
#callibrated



#How to make calibration

#To avoid dividing width/2 once can play with the formula and obtain that the results
#needs only to be mutiplied by 4
all(
(Edge_C41_1_n$Length[-2226]/(Edge_C41_1_n$Width[-2226]/2)^2)==
  (Edge_C41_1_n$Length[-2226]/(Edge_C41_1_n$Width[-2226])^2)*4

)


#Standardizing all variables. C41_1 has a conversion factor of 2.587 um/pixel

Edge_C41_1_n$Length_c<-Edge_C41_1_n$Length*2.587
Edge_C41_1_n$Width_c<-Edge_C41_1_n$Width*2.587

#Now to standardize resitance one plays with the formula and obtain that it only needs
#to get multiplied by 4/conversion factor, thus in this case 4/2.587

all(
  (Edge_C41_1_n$Length_c/(Edge_C41_1_n$Width_c/2)^2)==
    ( (Edge_C41_1_n$Length*2.587) /( (Edge_C41_1_n$Width*2.587) /2)^2)
)

#Here it shows that to rescale some resistance calculation it is enough to multiplyi it by
#4/the callibartion factor
p1<-(Edge_C41_1_n$Length) / (Edge_C41_1_n$Width)^2
p1<-p1*(4/2.587)

  (Edge_C41_1_n$Length_c/(Edge_C41_1_n$Width_c/2)^2)/
    p1


##





(Edge_C41_1_n$Length/(Edge_C41_1_n$Width/2)^2)*(8/pi)/
  (Edge_C41_1_n$Resistance_2ave)


##
(Edge_C41_1_n$Length/(Edge_C41_1_n$Width/2)^2)/
  ((Edge_C41_1_n$Resistance_2ave)/(2.546479))

(Edge_C41_1_n$Length/(Edge_C41_1_n$Width/2)^2)/
  ((Edge_C41_1_n$Resistance_2ave)*(pi/8))

#After a lot of trials with, below is the proof that, for recalibrating Resistace 2ave
#one has to 1) divided it by (8/pi) and 2) divided again by 2.58 

c<-2.587

p1<-(Edge_C41_1_n$Length) / (Edge_C41_1_n$Width/2)^2
p1<-p1*(1/2.587)

(Edge_C41_1_n$Length_c/(Edge_C41_1_n$Width_c/2)^2)/
  p1

##
p2<-((Edge_C41_1_n$Resistance_2ave)/(8/pi))*(1/c)

((Edge_C41_1_n$Length_c)/((Edge_C41_1_n$Width_c/2))^2)/
  (((Edge_C41_1_n$Resistance_2ave)/(8/pi))*(1/c))
p2

##
p2<-(Edge_C41_1_n$Resistance_2ave)
p2<- p2*(0.3926991)
p2<- p2*(1.546193)

((Edge_C41_1_n$Length_c)/(Edge_C41_1_n$Width_c/2)^2)/
  p2

#Checking for basidiomycetes

(Edge_C41_1_n$Length[-2226]/(Edge_C41_1_n$Width[-2226]/2)^2)*(8/pi)/
  (Edge_C41_1_n$Resistance_2ave[-2226])#this one here shows that resitance2ave was not
#callibrated

#In principle this dataset is already callibrated, so the calculation of resistance is
#straightforward
basidio_exmp<-Edge_Traits_B[[1]]

(basidio_exmp$Length/(basidio_exmp$Width/2)^2)*(8/pi)/
  (basidio_exmp$Resistance_2ave)#and indeed they are!! So no need to callibrate these ones


#checking global efficiency cacluations
#First I need to calculate again the network. I cannot use the one of the official analysis
#because for that one I used resistance_2ave to calculate distances, the app of Mark uses
#Resistance_2. 

colonies_prueba<-
  
  mapply(function(x,z){
    y<-graph_from_edgelist(as.matrix(x[,c("EndNodes_1","EndNodes_2")]),directed = F)
    E(y)$name<-x$name
    E(y)$weight<-x$Resistance_2
    E(y)$length<-x$Length
    E(y)$width<-x$Width
    
    V(y)$Degrees<-degree(y)
    V(y)$Accessibility<-distances(y,v=as.numeric(V(y)[Degrees==max(V(y)$Degrees)])
                                  
                                  
    )
    
    V(y)$angle<-z$node_Omin_Omid
    
    E(y)$type<-"Main"
    
    E(y)$e_distance<-(x$Length/x$Tortuosity)
    
    E(y)[incident(y,
                  as.numeric(V(y)[Degrees==max(V(y)$Degrees)])
    )]$type<-"Inoculum"
    
    y
  },Edge_Traits[c(1,17,24)],Node_Traits[c(1,17,24)],SIMPLIFY = F)

#Marks App values
summary_Traits_table_AZB[c(1,17,24),c("name_col","summary_mean_Geff")]


#self made function
self_made_Geff<-do.call(rbind,
                      lapply(colonies_prueba,function(net_ed){
                        data.frame(
                          name_col=unique(E(net_ed)$name),
                          summary_mean_Geff=sum(1/distances(net_ed)[distances(net_ed)>0])*(1/(length(V(net_ed))*length(V(net_ed))-1))
                          
                        )
                      })
)


#efficiency fucntion from braingraph
from_braingraph<-
sapply(colonies_prueba,function(net_ed){
  a<-efficiency(net_ed,type = "global")})


#function that uses different parallell and doParallel functions
g<-colonies_prueba[[1]]
g<-colonies_prueba[[2]]
g<-colonies_prueba[[3]]

get_eff <- function(i){return((1/(length(V(g)) - 1))*sum(1/distances(g, V(g)[i])[-i]))}
no_cores <- detectCores() - 1 
cl       <- makeCluster(no_cores)
registerDoParallel(cl)  
result <- foreach(i = seq_along(V(g)), .combine = c, .packages = "igraph") %dopar% get_eff(i)
stopCluster(cl)
rm(cl)
#paralelo_1 <- mean(result)
#paralelo_2 <- mean(result)
paralelo_3 <- mean(result)

c(paralelo_1,paralelo_2,paralelo_3)

#As one can tell from these results, all these methods provide the same results!!!!


#Now plotting the global efficiency using resistance_2 and resistance 2ave

summary_Traits_table_AZB[c(1,17,24),c("name_col","summary_mean_Geff")]

#Few points, but it kind of looks that they are correlated! Anyways, for the basidios the 
#numbers should be similar as both resistance_2 and resitance_2ave are correlated.
plot(
summary_Traits_table_AZB$summary_mean_Geff[c(1,17)],
c(Real_summary_Geff1[1],Real_summary_Geff14_35[4]))


