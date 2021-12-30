#############################################
#CASE STUDY - DATA SCIENTIST/ENGINEER @ATMS
#############################################
#STANISLAV KOVAC
#2021-12-30
#############################################
#TASKS:
#1. PRE-PROCESSING 
#2. GEOSPATIAL VISUALIZATION
#3. PROBLEM + ML SOLUTION
#############################################
#SOLUTION 1. PRE-PROCESSING
#loading libraries
libraries = c("dplyr", "skimr", "maps", "ggplot2", "ggmap",
              "sf", "raster", "spData", "tmap", "sp","data.table", "tidyverse",
              "scales", "randomForest", "caTools", "stringr")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)
#loading data and first check
dataset<-read.csv(choose.files()) #load WeatherEvents_Jan2016-Dec2020.csv
skim(dataset)
#data type change
datasetWork<-dataset
datasetWork$StartTime.UTC.<-as.POSIXct(datasetWork$StartTime.UTC.,tz="UTC")
datasetWork$EndTime.UTC.<-as.POSIXct(datasetWork$EndTime.UTC.,tz="UTC")
#creating a new variable - duration of an event
datasetWork$duration<-datasetWork$EndTime.UTC.-datasetWork$StartTime.UTC.
datasetWork$Location <- paste(datasetWork$LocationLng, datasetWork$LocationLat)
#more processing when it's required due to visualization or ML
##############################################
#SOLUTION 2. GEOSPATIAL VISUALISATION
#Start: baseline
datasetWorkSpatial<- datasetWork %>% distinct(LocationLat, LocationLng,.keep_all = TRUE)
coordinates(datasetWorkSpatial)<-~LocationLng+LocationLat
tmap_mode("view")
ashape=tm_shape(us_states)+
  tm_polygons(id="NAME")
#location of the airports
apLocD=tm_shape(datasetWorkSpatial)+
  tm_dots(size=0.001)
apLoc=ashape+apLocD
apLoc #projection warning, OK
#End: baseline
#Start: plot type duration
datasetWorkLongestSumDum <- datasetWork
datasetWorkLongestSumDum$Location <- paste(datasetWork$LocationLng, datasetWork$LocationLat)
setDT(datasetWorkLongestSumDum)
datasetWorkLongestSumDum[,myorder:=1:.N, by=.(Location,Type,Severity)]
datasetWorkLongestSumSeverity <- aggregate(x=datasetWorkLongestSumDum$duration,
                                           by=list(datasetWorkLongestSumDum$Location, datasetWorkLongestSumDum$Type, datasetWorkLongestSumDum$Severity),
                                           FUN=sum)
datasetWorkLongestSumSeverity<-NULL
datasetWorkLongestSumType<- aggregate(x=datasetWorkLongestSumDum$duration,
                                      by=list(datasetWorkLongestSumDum$Location, datasetWorkLongestSumDum$Type),
                                      FUN=sum)
datasetWorkLongestSumType$y <- datasetWorkLongestSumType$x/((3*365+2*366)*24*60*60)
datasetWorkLongestSumType$yPerc<-label_percent()(as.numeric(datasetWorkLongestSumType$x/((3*365+2*366)*24*60*60)))
type<-unique(datasetWorkLongestSumType$Group.2)
for(i in 1:length(type)) {
  do.call("<-", list(noquote(paste("dsWLST",as.character(type[i]), sep=".")), datasetWorkLongestSumType %>% filter(Group.2==type[i])))
  do.call("<-", list(noquote(paste("dsWLST",as.character(type[i]), sep=".")), 
                     eval(parse(text=paste("dsWLST",as.character(type[i]), sep="."))) %>% separate(Group.1, c("Lng", "Lat")," ") %>% st_as_sf(coords=c("Lng", "Lat"))))
  do.call("<-", list(noquote(paste("co.plot",as.character(type[i]), sep=".")), 
                     tm_shape(eval(parse(text=paste("dsWLST",as.character(type[i]), sep="."))))+
                       tm_dots(col="y", palette="Blues", border.alpha=0, alpha=0.5,size=0.01, legend.show = FALSE)+
                       tm_layout(title = as.character(type[i]))))
  do.call("<-", list(noquote(paste("plot",as.character(type[i]), sep=".")), 
                     ashape+eval(parse(text=paste("co.plot",as.character(type[i]), sep=".")))))
}
tmap_mode("view")
tmap_arrange(plot.Storm, plot.Fog, plot.Cold, plot.Hail, plot.Rain, plot.Precipitation, ncol=3, nrow=2)
#End: plot type duration
############################################
#SOLUTION 3. PROBLEM + ML SOLUTION
#PROBLEM: WEATHER EVENT'S PREDICTION IN FLORIDA
#ML: RANDOM FOREST
datasetWorkMLDum <- datasetWork %>% filter(State=="FL" & duration<=30*24*60*60 & duration>0) #longer than a month OUT
datasetWorkMLSample<-transform(
  datasetWorkMLDum[,c("Type", "StartTime.UTC.", "EndTime.UTC.",  "TimeZone", "City","County","State","duration","Location")],
  Type=as.factor(Type),
  City=as.numeric(factor(City)), #too many cities
  Location=as.numeric(factor(Location)),
  duration=as.numeric(duration),
  TimeZone=as.factor(TimeZone),
  County=as.numeric(factor(County)),
  State=as.factor(State)
)
#new set - add OK weather (EndTime[row-1] - StartTime[row] for each location)- check for two years
datasetWorkMLSampleOKWeather <- datasetWorkMLSample %>%filter(StartTime.UTC.<=as.POSIXct(strptime("2017-12-31 23:59:59", "%Y-%m-%d %H:%M:%S")))
datasetWorkMLSampleOKWeatherDif <- data.frame()
dummyA<-NULL
rowShift=0
#note: this bit takes quite a long time (basically creating a new Type: "OK", which represents all other types of events which have not been included - creating the time continuum)
for (i in unique(datasetWorkMLSampleOKWeather$Location)){
  dummyA<-datasetWorkMLSampleOKWeather %>% filter(Location == i)
  for (j in 2:nrow(dummyA)){
    datasetWorkMLSampleOKWeatherDif[j-1+rowShift,1]<-"OK"
    datasetWorkMLSampleOKWeatherDif[j-1+rowShift,2]<-dummyA[j-1,3]
    datasetWorkMLSampleOKWeatherDif[j-1+rowShift,3]<-dummyA[j,2]
    datasetWorkMLSampleOKWeatherDif[j-1+rowShift,4:7]<-dummyA[j,4:7]
    datasetWorkMLSampleOKWeatherDif[j-1+rowShift,8]<-datasetWorkMLSampleOKWeatherDif[j-1+rowShift,3]-datasetWorkMLSampleOKWeatherDif[j-1+rowShift,2]
    datasetWorkMLSampleOKWeatherDif[j-1+rowShift,9]<-i
  }
  rowShift=rowShift+nrow(dummyA)-1
}
colnames(datasetWorkMLSampleOKWeatherDif)<-colnames(datasetWorkMLSampleOKWeather)
#col. 8 in minutes, change to second as numeric
datasetWorkMLSampleOKWeatherDif$duration<-as.numeric(datasetWorkMLSampleOKWeatherDif$duration*60)
datasetWorkMLSampleOKWeatherJoin<-dplyr::bind_rows(datasetWorkMLSampleOKWeather,datasetWorkMLSampleOKWeatherDif %>% filter(duration>0))
apply(datasetWorkMLSampleOKWeatherJoin, 2, function(x) any(is.na(x)))
sapply(datasetWorkMLSampleOKWeatherJoin,class)
datasetWorkMLSampleOKWeatherJoin<-transform(
  datasetWorkMLSampleOKWeatherJoin,
  Type=as.factor(Type),
  duration=as.numeric(duration)
)
datsetWorkMLSampleSplit=sample.split(datasetWorkMLSampleOKWeatherJoin$Type, SplitRatio=0.90)
train=subset(datasetWorkMLSampleOKWeatherJoin, datsetWorkMLSampleSplit==TRUE)
test=subset(datasetWorkMLSampleOKWeatherJoin,datsetWorkMLSampleSplit==FALSE)
#End: sampling
rf<-randomForest(Type~., data=train)
pred<-predict(rf,newdata=test)
predi=as.vector.factor(pred)
cm<-test[,1]==predi[]
nTrue<-sum(str_count(cm,"TRUE"))
nFalse<-sum(str_count(cm, "FALSE"))
nFalse/dim(test)[1]
testCheck<-cbind(test,cm)
testCheckType<-testCheck 
setDT(testCheckType)
testCheckType[,myorder:=1:.N, by=.(Location,Type, cm)]  
testCheckType<-aggregate(x=testCheckType$myorder,
                         by=list(testCheckType$Location, testCheckType$Type, testCheckType$cm),
                         FUN=max)
testCheckType<-aggregate(x=testCheckType$x,
                         by=list(testCheckType$Group.2,testCheckType$Group.3 ),
                         FUN=sum)
testCheckType
