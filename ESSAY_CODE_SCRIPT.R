#############################################################################
##############################ESSAY_CODE_FINAL###############################
#############################################################################
#ΦΟΡΤΩΣΗ ΠΑΚΕΤΩΝ
x <- c("ggmap","rgdal","rgeos","maptools","dplyr","tidyr",
"tmap","sf","tmap","rgdal","lctools","ggplot2","rgeos")
lapply(x, library, character.only = TRUE)
#############################################################################
#ΦΟΡΤΩΝΩ ΤΟ SHAPEFILE ΚΑΙ ΤΙΣ ΣΥΝΤΕΤΑΓΜΕΝΕΣ
#ΤΩΝ ΚΕΝΤΡΩΝ ΤΩΝ ΧΩΡΩΝ
coun<- readOGR(dsn = ".\\USA_States",
layer = "USA_States")

coords<-read.csv(".\\centroids\\latitude_and_longitude_values.csv")
useful_cols<-c("usa_state","usa_state_latitude","usa_state_longitude")
coords<-coords[useful_cols][1:52,]
colnames(coords)<-c("state","latitude","longitude")
#############################################################################
#ΕΠΕΞΕΡΓΑΣΙΑ ΓΙΑ ΤΙΣ ΣΥΝΤΕΤΑΓΜΕΝΕΣ
states<-coun@data$STATE_NAME
#states
is.element(coords$state,states)
#ΠΑΡΑΤΗΡΩ ΠΩΣ ΣΤΗΝ ΘΈΣΗ 40 ΕΧΩ FALSE
#ΕΚΕΙ ΒΡΙΣΚΕΤΑΙ ΤΟ "Puerto Rico"
#ΠΟΥ ΔΕΝ ΥΠΑΡΧΕΙ ΣΤΟ states
#ΘΑ ΑΦΑΙΡΕΘΕΙ ΑΠΟ ΤΟ dataframe ΤΩΝ ΣΥΝΤΕΤΑΓΜΕΝΩΝ
coords<-coords[which(coords$state!="Puerto Rico"),]
#ΚΑΝΩ RE-INDEXING
rownames(coords)<- 1:nrow(coords)
#ΤΣΕΚΑΡΩ ΞΑΝΑ ΓΙΑ ΝΑ ΔΩ ΑΝ ΕΙΝΑΙ ΟΚ
#ΤΑ ΟΝΟΜΑΤΑ ΚΑΙ ΒΛΕΠΩ ΠΩΣ ΕΙΝΑΙ
is.element(coords$state,states)
is.element(states,coords$state)
#ΔΙΑΤΑΣΩ ΤΙΣ ΣΥΝΤΕΤΑΓΜΕΝΕΣ ΤΟΥ COORDS ΣΥΜΦΩΝΑ ΜΕ ΤΗΝ ΣΕΙΡΑ ΤΟΥ SHP
coords<-coords[order(match(coords$state,coun$STATE_NAME)),]
#ΕΝΩΝΩ ΤΑ ΔΕΔΟΜΕΝΑ ΜΟΥ
mydata_tmp<-merge(coords,coun,by.x="state",by.y="STATE_NAME",
sort=FALSE, all=TRUE)
#ΒΓΑΖΩ ΤΙΣ ΣΤΗΛΕΣ "STATE_FIPS" ΚΑΙ "STATE_ABBR"
mydata_tmp<-mydata_tmp[1:3]
#ΦΤΙΑΧΝΩ ΜΕ ΤΑ ΠΑΡΑΠΑΝΩ ΔΕΔΟΜΕΝΑ ΤΑ data TOY coun
coun@data<-mydata_tmp
#############################################################################
#############################################################################
#ΚΑΤΑΣΚΕΥΑΖΩ ΑΡΧΕΙΑ PDF ME ΧΑΡΤΕΣ
#ΚΑΙ PLOTS ΓΙΑ ΚΑΘΕ ΧΡΟΝΙΑ
#2005-2011
filenames<-list.files(".\\suicide_file")
filenames<-paste(".\\suicide_file\\",filenames,sep="")
dfs_r<-data.frame(index=1:50)
dfm_gen<-data.frame(index=1:49)
dfm_loc<-data.frame(index=1:50)
perv<-c()
perc<-c()
year<-2005
#ΒΡΟΧΟΣ ΕΠΑΝΑΛΗΨΗΣ
for(i in filenames){ 
label<-paste("Plots_of_",paste(year,".pdf",sep=""),sep="")
pdf(file=label)
suicides_file<-read.csv(paste(".\\suicide_file\\",
paste(year,"csv",sep="."),sep=""))
useful_cols<-c("state","vet_pop", "overall_pop_18",
"vet_suicides","all_suicides","vet_rate","civ_rate")
suicides_file<-suicides_file[useful_cols]
############################################################################
#ΕΠΕΞΕΡΓΑΣΙΑ ΤΩΝ ΔΕΔΟΜΕΝΩΝ
states<-coun@data$state
states
is.element(suicides_file$state,states)
#ΥΠΑΡΧΟΥΝ ΔΙΑΦΟΡΕΣ ΣΤΑ ΟΝΟΜΑΤΑ
#ΤΩΝ ΠΟΛΙΤΕΙΩΝ ΚΑΙ ΓΙΝΕΤΑΙ ΔΙΟΡΘΩΣΗ
suicides_file$state[9]
#Florida* 
suicides_file$state[9]<-"Florida"
suicides_file$state[25]
#Missouri***
suicides_file$state[25]<-"Missouri"
suicides_file$state[34]
#North Dakota**
suicides_file$state[34]<-"North Dakota"
is.element(suicides_file$state,states)
is.element(states,suicides_file$state)
#ΕΜΦΑΝΙΖΕΙ ΛΑΘΟΣ ΣΤΗΝ ΘΕΣΗ 27 ΠΟΥ ΕΙΝΑΙ
#ΤΟ "District of Columbia".ΛΟΓΙΚΟ ΚΑΘΩΣ ΤΑ ΔΕΔΟΜΕΝΑ ΜΟΥ
#ΗΤΑΝ 50 ΚΑΙ ΤΟ ΑΡΧΕΙΟ SHP ΕΙΧΕ 51 ΠΟΛΙΤΕΙΕΣ
#ΑΠΛΩΣ ΘΑ ΑΦΑΙΡΕΣΩ ΤΟ "District of Columbia" 
#ΑΠΟ ΤΟ SHAPEFILE
country<-subset(coun,is.element(coun$state,suicides_file$state))
#ΠΛΕΟΝ Η ΜΕΤΑΒΛΗΤΗ ΜΕ ΤΟ SHP ΕΧΕΙ 50 ΠΟΛΙΤΕΙΕΣ
#ΓΙΑ ΤΙΣ ΟΠΟΙΕΣ ΔΙΑΘΕΤΩ ΤΑ ΑΝΤΙΣΤΟΙΧΑ ΔΕΔΟΜΕΝΑ
############################################################################
#ΔΙΑΤΑΣΩ ΤΑ ΔΕΔΟΜΕΝΑ ΤΟΥ DATAFRAME ΣΥΜΦΩΝΑ ΜΕ ΑΥΤΑ ΤΟΥ SHP
suicides_file<-suicides_file[order(match(suicides_file$state,country$state)),]
############################################################################
#ΕΝΩΝΩ ΚΑΙ ΠΡΟΣΘΕΤΩ ΤΙΣ ΠΛΗΡΟΦΟΡΙΕΣ
mydata_tmp<-merge(suicides_file,country,by.x="state",by.y="state",
sort=FALSE,all=TRUE)
country@data<-mydata_tmp
#############################################################################
##################################VISUALIZATON###############################
#ΟΛΙΚΟΣ ΔΕΙΚΤΗΣ MORANS ΓΙΑ ΚΑΘΕ k
data<-country@data
Coords<-cbind(data$latitude, data$longitude)
bandwidth<-c(1:49)
moran<-matrix(data=NA,nrow=length(bandwidth),ncol=7)
counter<-1
for(b in bandwidth){
moranI<-moransI(Coords,b,data$vet_rate)
moran[counter,1]<-counter
moran[counter,2]<-b 
moran[counter,3]<-moranI$Morans.I 
moran[counter,4]<-moranI$z.resampling 
moran[counter,5]<-moranI$p.value.resampling 
moran[counter,6]<-moranI$z.randomization 
moran[counter,7]<-moranI$p.value.randomization 
counter<-counter+1} 
colnames(moran)<-c("ID","k", paste("Moran’s I",year), "Z resampling",
"P-value resampling", "Z randomization", "P-value randomization")
#ΤΟΠΙΚΟΙ ΔΕΙΚΤΕΣ MORANS
#ΚΑΤΑΣΚΕΥΑΖΕΙ ΚΑΙ ΔΙΑΓΡΑΜΜΑ ΔΙΑΣΠΟΡΑΣ ΤΩΝ ΖΕΥΓΩΝ
#ΚΑΝΟΝΙΚΟΠΟΙΗΜΕΝΩΝ ΤΙΜΏΝ ΡΥΘΜΩΝ ΑΥΤΟΚΤΟΝΙΩΝ ΒΕΤΕΡΑΝΩΝ
#ΚΑΙ ΚΑΝΟΝΙΚΟΠΟΙΗΜΕΝΩΝ ΣΤΑΘΜΙΣΜΕΝΩΝ ΑΘΡΟΙΣΜΆΤΟΝ ΤΩΝ
#ΡΥΘΜΩΝ ΑΥΤΟΚΤΟΝΙΩΝ ΓΙΑ 5 ΚΟΝΤΙΝΕΣ ΠΟΛΙΤΕΙΕΣ
#ΓΙΑ ΚΆΘΕ ΠΟΛΙΤΕΊΑ
l.moran<-l.moransI(Coords,5,data$vet_rate)
#############################################################################
#ΣΥΝΔΕΣΗ ΔΕΔΟΜΕΝΩΝ ΜΕ ΧΑΡΤΗ
#############################################################################
#ΚΑΤΑΣΚΕΥΗ ΧΑΡΤΗ Moran's I Cluster Map
country@data$Idx<-seq_len(nrow(country@data)) 
mydata_tmp<-merge(country@data, l.moran, by.x="Idx",
by.y="ID", sort=FALSE, all=TRUE) 
country@data<-mydata_tmp[order(mydata_tmp$Idx),] 

map.f <- fortify(country, region = "Idx")
map.f <- merge(map.f, country@data, by.x = "id", by.y = "Idx")

map <- ggplot(map.f, aes(long, lat, group = group)) +
geom_polygon(colour="gray80", aes(fill=as.factor(Cluster))) +
scale_fill_manual(values=c("white", "red", "blue", "turquoise", "pink")) +
coord_equal() +
labs(x = "Easting (m)", y = "Northing (m)", fill = "Class") +
ggtitle(paste("Moran's I Cluster Map",year))
plot(map)
#############################################################################
#CHOROPLETH
choro<- ggplot(map.f, aes(long, lat, group = group)) +
geom_polygon(colour="gray80", aes(fill=vet_rate)) +
coord_equal() +
labs(x = "Easting (m)", y = "Northing (m)", fill = "Class") +
ggtitle(paste("VETERAN SUICIDE RATE MAP OF",year))
plot(choro)
#############################################################################
#ΙΣΤΟΓΡΑΜΜΑΤΑ-ΡΑΒΔΟΓΡΑΜΜΑΤΑ
#HISTOGRAMS
hist_per<-hist(country@data$vet_rate, breaks=10,
col="blue",main=paste("Veteran suicide rate Histogram",year), 
xlab="Veteran suicide rate", 
ylab="Number of States")
#BARPLOTS
a<-country@data$vet_rate
names(a)<-country@data$state
barplot<-barplot(sort(a,decreasing=TRUE),
las=2,cex.names=0.60,
main=paste("Veteran suicide rate of States",year))
#############################################################################
#BOXPLOTS
df<-data.frame(Veteran_Suicide_Rate=country@data$vet_rate, 
Civilian_Suicide_Rate=country@data$civ_rate)

boxplot<-boxplot(df, col="orange",main=paste("Suicide Rate Boxplot",year))
perv<-append(perv,
(sum(country@data$vet_suicides)/sum(country@data$vet_pop))*1000)
perc<-append(perc,
((sum(country@data$all_suicides)-sum(country@data$vet_suicides))/
(sum(country@data$overall_pop_18)-sum(country@data$vet_pop)))*1000)
dfs_r<-cbind(dfs_r,country@data$vet_rate)
dfm_gen<-cbind(dfm_gen,moran[,3])
dfm_loc<-cbind(dfm_loc,l.moran[,2])
#############################################################################
dev.off()
year<-year+1
}#ΕΔΩ ΚΛΕΙΝΕΙ Η for
#############################################################################
#LINE_PLOT
pdf(file="2005-2011period.pdf",width=10,height=5)
df<-data.frame(vet_rate=perv,civ_rate=perc,year=2005:2011)
suicide_line_diagram<-ggplot()+
geom_line(data=df,mapping=aes(x=year,y=vet_rate),col="blue")+
geom_point(data=df,mapping=aes(x=year,y=vet_rate),col="black")+
geom_line(data=df,mapping=aes(x=year,y=civ_rate),col="red")+
geom_point(data=df,mapping=aes(x=year,y=civ_rate),col="red")+
labs(x="YEAR",y="SUICIDE RATES IN USA",
title="LINE DIAGRAM OF SUICIDE RATES IN USA YEARS 2005-2011")
plot(suicide_line_diagram)
#############################################################################
#ΕΠΕΞΕΡΓΑΣΙΑ ΒΟΗΘΗΤΙΚΩΝ ΠΙΝΑΚΩΝ
colnames(dfm_gen)<-c("Κ",paste("MORANS_I_",2005:2011,sep=""))
colnames(dfm_loc)<-c("index",paste("Ii_",2005:2011,sep=""))
dfs_r<-dfs_r[,2:8]#αφαιρώ την πρώτη στήλη
colnames(dfs_r)<-c(paste("vet_rate",2005:2011,sep="_"))
#############################################################################
#ALL vet_rate BOXPLOTS 
boxplot<-boxplot(dfs_r, col="orange", main="Suicide Rate Boxplot")
dfs_r<-cbind(dfs_r,state=country@data$state)
#############################################################################
#ΥΠΟΛΟΓΙΖΕΙ ΚΑΙ ΠΡΟΣΘΈΤΕΙ ΤΗΝ ΣΤΗΛΗ ΜΕ
#ΤΟΝ ΜΕΣΟ ΡΥΘΜΟ ΑΥΤΟΚΤΟΝΙΩΝ ΤΩΝ ΒΕΤΕΡΑΝΩΝ
#ΓΙΑ ΚΑΘΕ ΧΩΡΑ ΓΙΑ ΤΑ ΕΤΗ 2005-2011
dfs_r<-cbind(dfs_r,mean_vet_rate=rowSums(dfs_r[,1:7])/7)
#############################################################################
country<-subset(coun,is.element(coun$state,suicides_file$state))
suicides_file<-suicides_file[order(match(suicides_file$state,country$state)),]
#ΕΝΩΝΩ ΚΑΙ ΠΡΟΣΘΕΤΩ ΤΙΣ ΠΛΗΡΟΦΟΡΙΕΣ
mydata_tmp<-merge(dfs_r[8:9],country,by.x="state",by.y="state",
sort=FALSE,all=TRUE)
country@data<-mydata_tmp
#############################################################################
#ΧΑΡΤΕΣ ΓΙΑ ΟΛΗ ΤΗΝ ΧΡΟΝΙΚΗ ΠΕΡΙΟΔΟ 2005-2011
data<-country@data
Coords<-cbind(data$latitude, data$longitude)
bandwidth<-c(1:49)
moran<-matrix(data=NA,nrow=length(bandwidth),ncol=7)
counter<-1
for(b in bandwidth){
moranI<-moransI(Coords,b,country$mean_vet_rate)
moran[counter,1]<-counter
moran[counter,2]<-b 
moran[counter,3]<-moranI$Morans.I 
moran[counter,4]<-moranI$z.resampling 
moran[counter,5]<-moranI$p.value.resampling 
moran[counter,6]<-moranI$z.randomization 
moran[counter,7]<-moranI$p.value.randomization 
counter<-counter+1} 
colnames(moran)<-c("ID","k", paste("Moran’s I",2005-2011), "Z resampling",
"P-value resampling", "Z randomization", "P-value randomization")
#ΟΛΙΚΟΣ ΔΕΙΚΤΗΣ MORANS ΓΙΑ ΚΑΘΕ k
#ΤΟΠΙΚΟΙ ΔΕΙΚΤΕΣ MORANS
l.moran<-l.moransI(Coords,5,country$mean_vet_rate)
#############################################################################
#ΣΥΝΔΕΣΗ ΔΕΔΟΜΕΝΩΝ ΜΕ ΧΑΡΤΗ
#############################################################################
#ΚΑΤΑΣΚΕΥΗ ΧΑΡΤΗ Moran's I Cluster Map
country@data$Idx<-seq_len(nrow(country@data)) 
mydata_tmp<-merge(country@data, l.moran, by.x="Idx",
by.y="ID", sort=FALSE, all=TRUE) 
country@data<-mydata_tmp[order(mydata_tmp$Idx),] 

map.f <- fortify(country, region = "Idx")
map.f <- merge(map.f, country@data, by.x = "id", by.y = "Idx")

map <- ggplot(map.f, aes(long, lat, group = group)) +
geom_polygon(colour="gray80", aes(fill=as.factor(Cluster))) +
scale_fill_manual(values=c("white", "red", "blue", "turquoise", "pink")) +
coord_equal() +
labs(x = "Easting (m)", y = "Northing (m)", fill = "Class") +
ggtitle(paste("Moran's I Cluster Map","2005-2011"))
plot(map)
#############################################################################
#CHOROPLETH
choro<- ggplot(map.f, aes(long, lat, group = group)) +
geom_polygon(colour="gray80", aes(fill=mean_vet_rate)) +
coord_equal() +
labs(x = "Easting (m)", y = "Northing (m)", fill = "Class") +
ggtitle(paste("VETERAN SUICIDE RATE MAP OF","2005-2011"))
plot(choro)
#############################################################################
#BARPLOT
a<-country$mean_vet_rate
names(a)<-country@data$state
barplot<-barplot(sort(a,decreasing=TRUE),
las=2,cex.names=0.60,
main=paste("Veteran suicide rate of States","2005-2011"))
dev.off()
#############################################################################
###############################ΤΕΛΟΣ_ΚΩΔΙΚΑ##################################
#############################################################################

