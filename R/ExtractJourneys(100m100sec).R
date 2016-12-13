#' Identifies all potential legs of individual journeys (using 100m/100sec)
#'
#' Determines legs of an individual journey, by excluding jumps in location for consecutive measurements. Sorts the data by timestamp first. Allows the user to specify a threshold time and/or distance (optionally with individual values per method used) below which the segments are considered as part of the same journey.
#' Assigns a unique journey ID to each identified individual journey. Writes the output for each agent to a *.kml file and optionally (if feasible) to one overall file.
#'

#'
#' @param inputPath character; the path to the folder where the input *.csv files are located
#' @param inputName character (optional); select only file names which contain the string specified; default is "Cleaned_Agent"
#' @param timeStamp character; name of column containing the numeric time stamp of the measurements for each agent (should contain date and time); default is "ts"
#' @param origin character; name of column containing c(x, y) coordinates; default is c("to_locx","to_locy")
#' @param destination character; name of column containing c(x, y) coordinates; default is c("from_locx","from_locy")
#' @param method character; name of column containing method of transportation used; default is "method"
#' @param duration character; name of column containing duration of (leg of) journey in seconds; default is "duration"
#' @param distance character; name of column containing distance of (leg of) journey in meters; default is "distGeo"
#' @param methodThreshold character vector (optional); vector of the names of the methods ("car", "notfoundinindex", "pedestrian", "unknown", "bicycle", "publicTransport") in the order their method-specific duration and/or distance thresholds will be supplied; default is NA
#' @param durThreshold integer (optionally vector); threshold duration in seconds below which observations are considered to belong to the same journey; if methods are specified in methodThreshold, can supply method-specific duration thresholds as an integer vector; set to NA to select only based on distance threshold; if both are supplied, only one of the conditions needs to be met; default is 100
#' @param distThreshold integer (optionally vector); threshold distance in meters below which observations are considered to belong to the same journey; if methods are specified in methodThreshold, can supply method-specific distance thresholds as an integer vector; set to NA to select only based on duration threshold; if both are supplied, only one of the conditions needs to be met; default is 100
#' @param ignoreMethodChanges boolean (optional); ignore method changes when identifying potential legs of the same journey; must be FALSE if specifying methodThresholds; default is FALSE
#' @param ignoreStops boolean (optional); disregard stationary moments when identifying potential legs of the same journey; default is TRUE
#' @param outputPath character (optional); the path to the folder to write the output *.csv and *.kml files to, set to NA to not write output files; default is the inputPath
#' @param outputName character (optional); if outputPath is not NA, will prefix the output filenames ("Agent-ID#.extension") with this; default is "Journeys(100m100sec)", set to NA to just have the AgentIDs
#'
#' @export

extractJourneysTraditional<-function(inputPath, inputName="ReclassedMethods_Agent", agentID="agentID", timeStamp="ts",
                                     origin=c("from_locx","from_locy"), destination=c("to_locx","to_locy"), method="method_desc", duration="duration", distance="distGeo", methodThreshold=NA, durThreshold=100, distThreshold=100,
                                     ignoreMethodChanges=TRUE, ignoreStops=FALSE, outputPath="/Users/Nikee/Documents/Work/Projects/CatchTravelAI/Data/NewerAgents/travelai_august2014-2016/JourneyData/",
                                     outputCSVPath=inputPath, outputName="Journeys(100m100sec)"){

  # read in filename(s)
  fileNames<-list.files(inputPath)
  fileNames<-fileNames[grep(inputName,fileNames)]


  for(i in 1:length(fileNames)){
    # read in file i
    inputDataFrame<-read.csv(paste(inputPath,fileNames[i],sep=""),as.is=T)
    print(paste("Working on agent ",i," out of ",length(fileNames)," with agent ID ",inputDataFrame[1,agentID],sep=""))

    # sort by timestamp
    inputDataFrame<-inputDataFrame[order(inputDataFrame[,timeStamp]),]

    # create column indicating stationary moments
    inputDataFrame$stationary<-FALSE
    inputDataFrame[which(useful::compare.list(inputDataFrame[,"from_locx"],inputDataFrame[,"to_locx"])==TRUE&useful::compare.list(inputDataFrame[,"from_locy"],inputDataFrame[,"to_locy"])==TRUE),]$stationary<-rep(TRUE,length(which(useful::compare.list(inputDataFrame[,"from_locx"],inputDataFrame[,"to_locx"])==TRUE&useful::compare.list(inputDataFrame[,"from_locy"],inputDataFrame[,"to_locy"])==TRUE)))

    agentsID<-inputDataFrame[1,]$agentID

    if(nrow(inputDataFrame)>1){
      # calculate 'deltaTime' (time between start of next event and end duration of current event)
      inputDataFrame$deltaTime<-inputDataFrame$calcDuration-inputDataFrame$duration
      duration="deltaTime"

      # create prevToLocX, prevToLocY and prevMethod colums for calculations (should already have nextFromLocX, nextFromLocY from ReadData)
      inputDataFrame$prevToLocX <- c(inputDataFrame[1,destination[1]],inputDataFrame[1:(nrow(inputDataFrame)-1),destination[1]]); inputDataFrame$prevToLocX[1]<-NA
      inputDataFrame$prevToLocY <- c(inputDataFrame[1,destination[2]],inputDataFrame[1:(nrow(inputDataFrame)-1),destination[2]]); inputDataFrame$prevToLocY[1]<-NA

      inputDataFrame$prevMethod <- c(NA,inputDataFrame[1:(nrow(inputDataFrame)-1),method])
      inputDataFrame$nextMethod <- c(inputDataFrame[2:(nrow(inputDataFrame)),method],NA)

      # determine location jumps, and method changes
      inputDataFrame$methodChange <- NA; inputDataFrame[which(inputDataFrame[,method]==inputDataFrame$prevMethod),]$methodChange<-rep(FALSE,length(which(inputDataFrame[,method]==inputDataFrame$prevMethod)))
      inputDataFrame[which(inputDataFrame[,method]!=inputDataFrame$prevMethod),]$methodChange<- rep(TRUE, length(which(inputDataFrame[,method]!=inputDataFrame$prevMethod)))


      inputDataFrame$sameJourney<-rep(FALSE,nrow(inputDataFrame))

      # apply (method-specific if applicable) distance and/or duration thresholds for identifying potential legs of the same journey
      if(length(methodThreshold)==1&&is.na(methodThreshold)){
        if(length(distThreshold)==1&&is.na(distThreshold)){
          if(ignoreMethodChanges==FALSE){
            if(ignoreStops==TRUE){
              inputDataFrame$sameJourney<-FALSE; if(length(which(inputDataFrame$methodChange == FALSE & inputDataFrame[,duration]<durThreshold))>0){inputDataFrame[which(inputDataFrame$methodChange == FALSE & inputDataFrame[,duration]<durThreshold),]$sameJourney<-TRUE}
            } else{
              inputDataFrame$sameJourney<-FALSE; if(length(which(inputDataFrame$methodChange == FALSE & inputDataFrame$stationary == FALSE &  inputDataFrame[,duration]<durThreshold))>0){inputDataFrame[which(inputDataFrame$methodChange == FALSE & inputDataFrame$stationary == FALSE &  inputDataFrame[,duration]<durThreshold),]$sameJourney<-TRUE}
            }
          } else{
            if(ignoreStops==TRUE){
              inputDataFrame$sameJourney<-FALSE; if(length(which(inputDataFrame[,duration]<durThreshold))>0){inputDataFrame[which(inputDataFrame[,duration]<durThreshold),]$sameJourney<-TRUE}
            } else{
              inputDataFrame$sameJourney<-FALSE; if(length(which(inputDataFrame$stationary == FALSE & inputDataFrame[,duration]<durThreshold))>0){inputDataFrame[which(inputDataFrame$stationary == FALSE & inputDataFrame[,duration]<durThreshold),]$sameJourney<-TRUE}
            }
          }
        } else {
          if(length(durThreshold)==1&&is.na(durThreshold)){
            if(ignoreMethodChanges==FALSE){
              if(ignoreStops==TRUE){
                inputDataFrame$sameJourney<-FALSE; if(length(which(inputDataFrame$methodChange == FALSE & inputDataFrame[,distance]<distThreshold))>0){inputDataFrame[which(inputDataFrame$methodChange == FALSE & inputDataFrame[,distance]<distThreshold),]$sameJourney<-TRUE}
              } else{
                inputDataFrame$sameJourney<-FALSE; if(length(which(inputDataFrame$methodChange == FALSE & inputDataFrame$stationary == FALSE &  inputDataFrame[,distance]<distThreshold))>0){inputDataFrame[which(inputDataFrame$methodChange == FALSE & inputDataFrame$stationary == FALSE &  inputDataFrame[,distance]<distThreshold),]$sameJourney<-TRUE}
              }
            } else{
              if(ignoreStops==TRUE){
                inputDataFrame$sameJourney<-FALSE; if(length(which(inputDataFrame[,distance]<distThreshold))>0){inputDataFrame[which(inputDataFrame[,distance]<distThreshold),]$sameJourney<-TRUE}
              } else{
                inputDataFrame$sameJourney<-FALSE; if(length(which(inputDataFrame$stationary == FALSE & inputDataFrame[,distance]<distThreshold))>0){inputDataFrame[which(inputDataFrame$stationary == FALSE & inputDataFrame[,distance]<distThreshold),]$sameJourney<-TRUE}
              }
            }
          } else {
            if(ignoreMethodChanges==FALSE){
              if(ignoreStops==TRUE){
                inputDataFrame$sameJourney<-FALSE; if(length(which(inputDataFrame$methodChange == FALSE & (inputDataFrame[,distance]<distThreshold | inputDataFrame[,duration]<durThreshold)))>0){inputDataFrame[which(inputDataFrame$methodChange == FALSE & (inputDataFrame[,distance]<distThreshold | inputDataFrame[,duration]<durThreshold)),]$sameJourney<-TRUE}
              } else{
                inputDataFrame$sameJourney<-FALSE; if(length(which(inputDataFrame$methodChange == FALSE & inputDataFrame$stationary == FALSE & (inputDataFrame[,distance]<distThreshold | inputDataFrame[,duration]<durThreshold)))>0){inputDataFrame[which(inputDataFrame$methodChange == FALSE & inputDataFrame$stationary == FALSE & (inputDataFrame[,distance]<distThreshold | inputDataFrame[,duration]<durThreshold)),]$sameJourney<-TRUE}
              }
            } else{
              if(ignoreStops==TRUE){
                inputDataFrame$sameJourney<-FALSE; if(length(which(inputDataFrame[,distance]<distThreshold | inputDataFrame[,duration]<durThreshold))>0){inputDataFrame[which(inputDataFrame[,distance]<distThreshold | inputDataFrame[,duration]<durThreshold),]$sameJourney<-TRUE}
              } else{
                inputDataFrame$sameJourney<-FALSE; if(length(which(inputDataFrame$stationary == FALSE & (inputDataFrame[,distance]<distThreshold | inputDataFrame[,duration]<durThreshold)))>0){inputDataFrame[which(inputDataFrame$stationary == FALSE & (inputDataFrame[,distance]<distThreshold | inputDataFrame[,duration]<durThreshold)),]$sameJourney<-TRUE}
              }
            }
          }
        }
      }


      # assign unique trip ID for each journey
      inputDataFrame$tripID<-NA; inputDataFrame[which(inputDataFrame$sameJourney==TRUE),]$tripID<-cumsum(c(1,(diff(inputDataFrame$sameJourney)>0)))[which(inputDataFrame$sameJourney==TRUE)]

      # assign "single-leg" journeys (according to the thresholds specified) a trip ID > 100k
      if(length(which(inputDataFrame$sameJourney==FALSE & inputDataFrame$stationary==FALSE))>0){
        inputDataFrame[which(inputDataFrame$sameJourney==FALSE&inputDataFrame$stationary==FALSE),]$tripID<-100000+seq(1,length(which(inputDataFrame$sameJourney==FALSE & inputDataFrame$stationary==FALSE)),1)
      }

      if(nrow(inputDataFrame)>1){
        # include the first measurement of the journeys by assigning them the same trip ID
        inputDataFrame$journeyShift<-NA; inputDataFrame$journeyShift<-c(inputDataFrame[2:nrow(inputDataFrame),]$sameJourney,NA)
        inputDataFrame$tripIDShift<-NA; inputDataFrame$tripIDShift<-c(inputDataFrame[2:nrow(inputDataFrame),]$tripID,NA)

        inputDataFrame[which(inputDataFrame$sameJourney==FALSE & inputDataFrame$journeyShift==TRUE),]$tripID<-inputDataFrame[which(inputDataFrame$sameJourney==FALSE & inputDataFrame$journeyShift==TRUE),]$tripIDShift
      }

      # assign a negative tripID to the stationary points
      if(length(which(inputDataFrame$sameJourney==FALSE&inputDataFrame$stationary==TRUE))>0){
        inputDataFrame[which(inputDataFrame$sameJourney==FALSE&inputDataFrame$stationary==TRUE),]$tripID<-(-9000)-seq(1,length(which(inputDataFrame$sameJourney==FALSE & inputDataFrame$stationary==TRUE)),1)
      }

      #  summarise the overall lines stats
      tripID<-unique(inputDataFrame[which(inputDataFrame$tripID>0),]$tripID)
      startDate<-tapply(inputDataFrame[which(inputDataFrame$tripID>0),]$date, inputDataFrame[which(inputDataFrame$tripID>0),]$tripID, dplyr::first)
      endDate<-tapply(inputDataFrame[which(inputDataFrame$tripID>0),]$date, inputDataFrame[which(inputDataFrame$tripID>0),]$tripID, dplyr::last)
      startTime<-tapply(inputDataFrame[which(inputDataFrame$tripID>0),]$decimalTime, inputDataFrame[which(inputDataFrame$tripID>0),]$tripID, dplyr::first)
      endTime<-tapply(inputDataFrame[which(inputDataFrame$tripID>0),]$decimalTime, inputDataFrame[which(inputDataFrame$tripID>0),]$tripID, dplyr::last)
      totDur<-tapply(inputDataFrame[which(inputDataFrame$tripID>0),]$duration, inputDataFrame[which(inputDataFrame$tripID>0),]$tripID, sum)
      totDist<-tapply(inputDataFrame[which(inputDataFrame$tripID>0),]$distGeo, inputDataFrame[which(inputDataFrame$tripID>0),]$tripID, sum)
      minSpeed<-tapply(inputDataFrame[which(inputDataFrame$tripID>0),]$speed, inputDataFrame[which(inputDataFrame$tripID>0),]$tripID, min)
      maxSpeed<-tapply(inputDataFrame[which(inputDataFrame$tripID>0),]$speed, inputDataFrame[which(inputDataFrame$tripID>0),]$tripID, max)
      avgSpeed<-3.6*totDist/totDur
      methods<-rep(NA,length(tripID))
      for(j in 1:length(tripID)){
        tripMethods<-unique(inputDataFrame[which(inputDataFrame$tripID==tripID[j]),]$method_desc)
        for(k in 1:length(tripMethods)){
          if(k==1){
            allMethods<-tripMethods[k]
          } else {
            allMethods<-paste(allMethods, ", ", tripMethods[k],sep="")
          }
        }
        methods[j]<-allMethods
      }
      startLocX<-tapply(inputDataFrame[which(inputDataFrame$tripID>0),]$from_locx, inputDataFrame[which(inputDataFrame$tripID>0),]$tripID, dplyr::first)
      startLocY<-tapply(inputDataFrame[which(inputDataFrame$tripID>0),]$from_locy, inputDataFrame[which(inputDataFrame$tripID>0),]$tripID, dplyr::first)
      endLocX<-tapply(inputDataFrame[which(inputDataFrame$tripID>0),]$to_locx, inputDataFrame[which(inputDataFrame$tripID>0),]$tripID, dplyr::last)
      endLocY<-tapply(inputDataFrame[which(inputDataFrame$tripID>0),]$to_locy, inputDataFrame[which(inputDataFrame$tripID>0),]$tripID, dplyr::last)

      lineStats<-as.data.frame(cbind(tripID,startDate,endDate,startTime,endTime,totDur,totDist,minSpeed,maxSpeed,avgSpeed,methods,startLocX,startLocY,endLocX,endLocY))#; rm(tripID,totDur,totDist,minSpeed,maxSpeed,avgSpeed,methods,startLocX,startLocY,endLocX,endLocY)

      write.csv(inputDataFrame,paste(outputCSVPath,outputName,"-",agentsID,".csv",sep=""),row.names=F)
      write.csv(lineStats,paste(outputCSVPath,"LineStats/LineStats-",outputName,"-",agentsID,".csv",sep=""),row.names=F)


      # extract journey lines from TripIDs > 0 (using Line and then adding them all into a SpatialLinesDatFrame)
      if(length(unique(inputDataFrame[which(inputDataFrame$tripID>0),]$tripID))>0){

        tripIDs<-unique(inputDataFrame[which(inputDataFrame$tripID>0),]$tripID)
        sALines<-list()

        for(j in 1:length(tripIDs)){

          startCoords<-cbind(inputDataFrame[which(inputDataFrame$tripID==tripIDs[j]),origin[1]], inputDataFrame[which(inputDataFrame$tripID==tripIDs[j]),origin[2]])
          destCoords<-cbind(inputDataFrame[which(inputDataFrame$tripID==tripIDs[j]),destination[1]], inputDataFrame[which(inputDataFrame$tripID==tripIDs[j]),destination[2]])
          sALines[[j]]<-sp::Lines(list(sp::Line(rbind(startCoords, destCoords[nrow(destCoords),]))), tripIDs[j])
        }

        # convert to spatial lines
        sASPLines<-sp::SpatialLines(sALines)


        # create spatial lines dataframe
        sASLDF<-sp::SpatialLinesDataFrame(sASPLines, data=lineStats, match.ID="tripID")


        # convert to BNG projection
        bng<-"+init=epsg:27700"
        wgs84<-"+init=epsg:4326"

        sp::proj4string(sASLDF)<-sp::CRS(wgs84)
        sASLDF_bng<-sp::spTransform(sASLDF,sp::CRS(wgs84))

        if(!is.na(outputPath)){
          if(!is.na(outputName)){
            outputFileName<-paste(outputPath,outputName,"_Agent-",inputDataFrame[1,agentID],".kml",sep="")
            out2<-paste(outputPath,outputName,"_Agent-",inputDataFrame[1,agentID],sep="")
          } else{
            outputFileName<-paste(outputPath,"_Agent-",inputDataFrame[1,agentID],".kml",sep="")
            out2<-paste(outputPath,"_Agent-",inputDataFrame[1,agentID],sep="")
          }
          rgdal::writeOGR(sASLDF_bng, outputFileName, out2, driver="KML")
        }
      } # end of if there are any tripIDs with ID>0
    } else {
      write.csv(inputDataFrame,paste(outputCSVPath,outputName,"-",agentsID,".csv",sep=""),row.names=F)

      # add something to maybe write out the single "journey" as a .kml file too
    } # end of if the inputDataFrame contains more than one entry
  } # end of loop over all agent files
}
