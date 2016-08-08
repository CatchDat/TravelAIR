#' Identifies all potential legs of individual journeys
#'
#' Determines legs of an individual journey, by excluding jumps in location for consecutive measurements. Sorts the data by timestamp first. Allows the user to specify a threshold time and/or distance (optionally with individual values per method used) below which the segments are considered as part of the same journey.
#' Assigns a unique journey ID to each identified individual journey. Writes the output for each agent to a *.kml file with summary statistics for that (leg of the) journey (total duration, total distance, minimum speed, maximum speed, average speed, method used and origin & destination x and y coordinates).
#'
#'
#' @param inputPath character; the path to the folder where the input *.csv files are located
#' @param inputName character (optional); select only file names which contain the string specified; default is "Cleaned_Agent"
#' @param agentID character; name of the column containing the agent IDs; default is "agentID"
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
#' @param outputName character (optional); if outputPath is not NA, will prefix the output filenames ("Agent-ID#.extension") with this; default is "Journeys", set to NA to just have the AgentIDs
#'
#' @export

identifyJourneys<-function(inputPath, inputName="Cleaned_Agent",agentID="agentID", timeStamp="ts",
                           origin=c("from_locx","from_locy"), destination=c("to_locx","to_locy"), method="method_desc", duration="duration", distance="distGeo", methodThreshold=NA, durThreshold=100, distThreshold=100,
                           ignoreMethodChanges=FALSE, ignoreStops=FALSE, outputPath=paste(inputPath,"JourneyData/",sep=""), outputName="Journeys"){

  # read in filename(s)
  fileNames<-list.files(inputPath)
  fileNames<-fileNames[grep(inputName,fileNames)]

  for(i in 1:length(fileNames)){

    if(!is.na(methodThreshold)){
      ignoreMethodChanges<-FALSE
    }

    # read in file i
    inputDataFrame<-read.csv(paste(inputPath,fileNames[i],sep=""),as.is=T)
    print(paste("Working on agent ",i," out of ",length(fileNames)," with agent ID ",inputDataFrame[1,agentID],sep=""))

    # remove the last observation which has NA coordinates for their destination x & y coords
    inputDataFrame<-inputDataFrame[which(!is.na(inputDataFrame$to_locx)),]

    # sort by timestamp
    inputDataFrame<-inputDataFrame[order(inputDataFrame[,timeStamp]),]

    # create column indicating stationary moments
    inputDataFrame$stationary<-FALSE
    inputDataFrame[which(useful::compare.list(inputDataFrame[,"from_locx"],inputDataFrame[,"to_locx"])==TRUE&useful::compare.list(inputDataFrame[,"from_locy"],inputDataFrame[,"to_locy"])==TRUE),]$stationary<-rep(TRUE,length(which(useful::compare.list(inputDataFrame[,"from_locx"],inputDataFrame[,"to_locx"])==TRUE&useful::compare.list(inputDataFrame[,"from_locy"],inputDataFrame[,"to_locy"])==TRUE)))

    # create prevToLocX, prevToLocY, nextFromLocX, nextFromLocY, and prevMethod colums for calculations
    inputDataFrame$prevToLocX <- c(inputDataFrame[1,destination[1]],inputDataFrame[1:(nrow(inputDataFrame)-1),destination[1]]); inputDataFrame$prevToLocX[1]<-NA
    inputDataFrame$prevToLocY <- c(inputDataFrame[1,destination[2]],inputDataFrame[1:(nrow(inputDataFrame)-1),destination[2]]); inputDataFrame$prevToLocY[1]<-NA

    inputDataFrame$nextFromLocX <- c(inputDataFrame[2:nrow(inputDataFrame),origin[1]],inputDataFrame[nrow(inputDataFrame),origin[1]]); inputDataFrame[nrow(inputDataFrame),]$nextFromLocX<-inputDataFrame[nrow(inputDataFrame),destination[1]]
    inputDataFrame$nextFromLocY <- c(inputDataFrame[2:nrow(inputDataFrame),origin[2]],inputDataFrame[nrow(inputDataFrame),origin[2]]); inputDataFrame[nrow(inputDataFrame),]$nextFromLocY<-inputDataFrame[nrow(inputDataFrame),destination[2]]

    inputDataFrame$prevMethod <- c(NA,inputDataFrame[1:(nrow(inputDataFrame)-1),method])



    # determine location jumps, and method changes
    inputDataFrame$methodChange <- NA; inputDataFrame[which(inputDataFrame[,method]==inputDataFrame$prevMethod),]$methodChange<-rep(FALSE,length(which(inputDataFrame[,method]==inputDataFrame$prevMethod)))
      inputDataFrame[which(inputDataFrame[,method]!=inputDataFrame$prevMethod),]$methodChange<- rep(TRUE, length(which(inputDataFrame[,method]!=inputDataFrame$prevMethod)))

    inputDataFrame$locJump <- NA; inputDataFrame[which(inputDataFrame$prevToLocX==inputDataFrame$from_locx & inputDataFrame$prevToLocY==inputDataFrame$from_locy),]$locJump<-rep(FALSE, length(which(inputDataFrame$prevToLocX==inputDataFrame$from_locx & inputDataFrame$prevToLocY==inputDataFrame$from_locy)))
      inputDataFrame[which(inputDataFrame$prevToLocX!=inputDataFrame$from_locx | inputDataFrame$prevToLocY!=inputDataFrame$from_locy),]$locJump <- rep(TRUE,length(which(inputDataFrame$prevToLocX!=inputDataFrame$from_locx | inputDataFrame$prevToLocY!=inputDataFrame$from_locy)))

    # apply (method-specific if applicable) distance and/or duration thresholds for identifying potential legs of the same journey

    if(is.na(methodThreshold)){
      if(is.na(distThreshold)){
        if(ignoreMethodChanges==FALSE){
          if(ignoreStops==TRUE){
            inputDataFrame$sameJourney<-FALSE; inputDataFrame[which(inputDataFrame$methodChange == FALSE & inputDataFrame$locJump == FALSE & inputDataFrame[,duration]<durThreshold),]$sameJourney<-TRUE
            #inputDataFrame[which(inputDataFrame$methodChange == TRUE | inputDataFrame$locJump == TRUE |  inputDataFrame[,duration]>durThreshold),]$sameJourney<-FALSE
          } else{
            inputDataFrame$sameJourney<-FALSE; inputDataFrame[which(inputDataFrame$methodChange == FALSE & inputDataFrame$locJump == FALSE & inputDataFrame$stationary == FALSE &  inputDataFrame[,duration]<durThreshold),]$sameJourney<-TRUE
            #inputDataFrame[which(inputDataFrame$methodChange == TRUE | inputDataFrame$locJump == TRUE | inputDataFrame$stationary == TRUE | inputDataFrame[,duration]>durThreshold),]$sameJourney<-FALSE
          }
        } else{
          if(ignoreStops==TRUE){
            inputDataFrame$sameJourney<-FALSE; inputDataFrame[which(inputDataFrame$locJump == FALSE & inputDataFrame[,duration]<durThreshold),]$sameJourney<-TRUE
            #inputDataFrame[which(inputDataFrame$locJump == TRUE | inputDataFrame[,duration]>durThreshold),]$sameJourney<-FALSE
          } else{
            inputDataFrame$sameJourney<-FALSE; inputDataFrame[which(inputDataFrame$locJump == FALSE & inputDataFrame$stationary == FALSE & inputDataFrame[,duration]<durThreshold),]$sameJourney<-TRUE
            #inputDataFrame[which(inputDataFrame$locJump == TRUE | inputDataFrame$noTravel == TRUE | inputDataFrame[,duration]>durThreshold),]$sameJourney<-FALSE
          }
        }
      } else {
       if(is.na(durThreshold)){
         if(ignoreMethodChanges==FALSE){
           if(ignoreStops==TRUE){
             inputDataFrame$sameJourney<-FALSE; inputDataFrame[which(inputDataFrame$methodChange == FALSE & inputDataFrame$locJump == FALSE & inputDataFrame[,distance]<distThreshold),]$sameJourney<-TRUE
             #inputDataFrame[which(inputDataFrame$methodChange == TRUE | inputDataFrame$locJump == TRUE |  inputDataFrame[,distance]>distThreshold),]$sameJourney<-FALSE
           } else{
             inputDataFrame$sameJourney<-FALSE; inputDataFrame[which(inputDataFrame$methodChange == FALSE & inputDataFrame$locJump == FALSE & inputDataFrame$stationary == FALSE &  inputDataFrame[,distance]<distThreshold),]$sameJourney<-TRUE
             #inputDataFrame[which(inputDataFrame$methodChange == TRUE | inputDataFrame$locJump == TRUE | inputDataFrame$stationary == TRUE | inputDataFrame[,distance]>distThreshold),]$sameJourney<-FALSE
           }
         } else{
           if(ignoreStops==TRUE){
             inputDataFrame$sameJourney<-FALSE; inputDataFrame[which(inputDataFrame$locJump == FALSE & inputDataFrame[,distance]<distThreshold),]$sameJourney<-TRUE
             #inputDataFrame[which(inputDataFrame$locJump == TRUE | inputDataFrame[,distance]>distThreshold),]$sameJourney<-FALSE
           } else{
             inputDataFrame$sameJourney<-FALSE; inputDataFrame[which(inputDataFrame$locJump == FALSE & inputDataFrame$stationary == FALSE & inputDataFrame[,distance]<distThreshold),]$sameJourney<-TRUE
             #inputDataFrame[which(inputDataFrame$locJump == TRUE | inputDataFrame$noTravel == TRUE | inputDataFrame[,distance]>distThreshold),]$sameJourney<-FALSE
           }
         }
       } else {
         if(ignoreMethodChanges==FALSE){
           if(ignoreStops==TRUE){
             inputDataFrame$sameJourney<-FALSE; inputDataFrame[which(inputDataFrame$methodChange == FALSE & inputDataFrame$locJump == FALSE & (inputDataFrame[,distance]<distThreshold | inputDataFrame[,duration]<durThreshold)),]$sameJourney<-TRUE
             #inputDataFrame[which(inputDataFrame$methodChange == TRUE | inputDataFrame$locJump == TRUE |  (inputDataFrame[,distance]>distThreshold &  inputDataFrame[,duration]>durThreshold)),]$sameJourney<-FALSE
           } else{
             inputDataFrame$sameJourney<-FALSE; inputDataFrame[which(inputDataFrame$methodChange == FALSE & inputDataFrame$locJump == FALSE & inputDataFrame$stationary == FALSE & (inputDataFrame[,distance]<distThreshold | inputDataFrame[,duration]<durThreshold)),]$sameJourney<-TRUE
             #inputDataFrame[which(inputDataFrame$methodChange == TRUE | inputDataFrame$locJump == TRUE | inputDataFrame$stationary == TRUE | (inputDataFrame[,distance]>distThreshold &  inputDataFrame[,duration]>durThreshold)),]$sameJourney<-FALSE
           }
         } else{
           if(ignoreStops==TRUE){
             inputDataFrame$sameJourney<-FALSE; inputDataFrame[which(inputDataFrame$locJump == FALSE & (inputDataFrame[,distance]<distThreshold | inputDataFrame[,duration]<durThreshold)),]$sameJourney<-TRUE
             #inputDataFrame[which(inputDataFrame$locJump == TRUE | (inputDataFrame[,distance]>distThreshold &  inputDataFrame[,duration]>durThreshold)),]$sameJourney<-FALSE
           } else{
             inputDataFrame$sameJourney<-FALSE; inputDataFrame[which(inputDataFrame$locJump == FALSE & inputDataFrame$stationary == FALSE & (inputDataFrame[,distance]<distThreshold | inputDataFrame[,duration]<durThreshold)),]$sameJourney<-TRUE
             #inputDataFrame[which(inputDataFrame$locJump == TRUE | inputDataFrame$noTravel == TRUE | (inputDataFrame[,distance]>distThreshold & inputDataFrame[,duration]>durThreshold)),]$sameJourney<-FALSE
           }
         }
       }
      }

      # assign unique trip ID for each journey
      inputDataFrame$tripID<-NA; inputDataFrame[which(inputDataFrame$sameJourney==TRUE),]$tripID<-cumsum(c(1,(diff(inputDataFrame$sameJourney)>0)))[which(inputDataFrame$sameJourney==TRUE)]
    } else {
      for(j in 1:length(methodThreshold)){
        if(is.na(distThreshold)){
          if(ignoreStops==TRUE){
            inputDataFrame$sameJourney<-FALSE; inputDataFrame[which(inputDataFrame[,method]==methodThreshold[j] & inputDataFrame$locJump == FALSE & inputDataFrame[,duration]<durThreshold[j]),]$sameJourney<-TRUE
            #inputDataFrame[which(inputDataFrame[,method]!=methodThreshold[j] | inputDataFrame$locJump == TRUE |  inputDataFrame[,duration]>durThreshold[j]),]$sameJourney<-FALSE
          } else{
            inputDataFrame$sameJourney<-FALSE; inputDataFrame[which(inputDataFrame[,method]==methodThreshold[j] & inputDataFrame$locJump == FALSE & inputDataFrame$stationary == FALSE &  inputDataFrame[,duration]<durThreshold[j]),]$sameJourney<-TRUE
            #inputDataFrame[which(inputDataFrame[,method]!=methodThreshold[j] | inputDataFrame$locJump == TRUE | inputDataFrame$stationary == TRUE | inputDataFrame[,duration]>durThreshold[j]),]$sameJourney<-FALSE
          }
        } else {
          if(is.na(durThreshold)){
            if(ignoreStops==TRUE){
              inputDataFrame$sameJourney<-FALSE; inputDataFrame[which(inputDataFrame[,method]==methodThreshold[j] & inputDataFrame$locJump == FALSE & inputDataFrame[,distance]<distThreshold[j]),]$sameJourney<-TRUE
              #inputDataFrame[which(inputDataFrame[,method]!=methodThreshold[j] | inputDataFrame$locJump == TRUE |  inputDataFrame[,distance]>distThreshold[j]),]$sameJourney<-FALSE
            } else{
              inputDataFrame$sameJourney<-FALSE; inputDataFrame[which(inputDataFrame[,method]==methodThreshold[j] & inputDataFrame$locJump == FALSE & inputDataFrame$stationary == FALSE &  inputDataFrame[,distance]<distThreshold[j]),]$sameJourney<-TRUE
              #inputDataFrame[which(inputDataFrame[,method]!=methodThreshold[j] | inputDataFrame$locJump == TRUE | inputDataFrame$stationary == TRUE | inputDataFrame[,distance]>distThreshold[j]),]$sameJourney<-FALSE
            }
          } else {
            if(ignoreStops==TRUE){
              inputDataFrame$sameJourney<-FALSE; inputDataFrame[which(inputDataFrame[,method]==methodThreshold[j] & inputDataFrame$locJump == FALSE & (inputDataFrame[,distance]<distThreshold[j] | inputDataFrame[,duration]<durThreshold[j])),]$sameJourney<-TRUE
              #inputDataFrame[which(inputDataFrame[,method]!=methodThreshold[j] | inputDataFrame$locJump == TRUE |  (inputDataFrame[,distance]>distThreshold[j] &  inputDataFrame[,duration]>durThreshold[j])),]$sameJourney<-FALSE
            } else{
              inputDataFrame$sameJourney<-FALSE; inputDataFrame[which(inputDataFrame[,method]==methodThreshold[j] & inputDataFrame$locJump == FALSE & inputDataFrame$stationary == FALSE & (inputDataFrame[,distance]<distThreshold[j] | inputDataFrame[,duration]<durThreshold[j])),]$sameJourney<-TRUE
              #inputDataFrame[which(inputDataFrame[,method]!=methodThreshold[j] | inputDataFrame$locJump == TRUE | inputDataFrame$stationary == TRUE | (inputDataFrame[,distance]>distThreshold[j] &  inputDataFrame[,duration]>durThreshold[j])),]$sameJourney<-FALSE
            }
          }
        }

        # assign unique trip ID for each journey according to each specific method threshold
        inputDataFrame$tripID<-NA; inputDataFrame[which(inputDataFrame$sameJourney==TRUE),]$tripID<-(10000*j)+cumsum(c(1,(diff(inputDataFrame$sameJourney)>0)))[which(inputDataFrame$sameJourney==TRUE)]

      }
    }

    # assign "single-leg" journeys (according to the thresholds specified) a trip ID > 100k
    inputDataFrame[which(inputDataFrame$sameJourney==FALSE&inputDataFrame$stationary==FALSE),]$tripID<-100000+seq(1,length(which(inputDataFrame$sameJourney==FALSE & inputDataFrame$stationary==FALSE)),1)

    # include the first measurement of the journeys by assigning them the same trip ID
    inputDataFrame$journeyShift<-NA; inputDataFrame$journeyShift<-c(inputDataFrame[2:nrow(inputDataFrame),]$sameJourney,NA)
    inputDataFrame$tripIDShift<-NA; inputDataFrame$tripIDShift<-c(inputDataFrame[2:nrow(inputDataFrame),]$tripID,NA)

    inputDataFrame[which(inputDataFrame$sameJourney==FALSE & inputDataFrame$journeyShift==TRUE),]$tripID<-inputDataFrame[which(inputDataFrame$sameJourney==FALSE & inputDataFrame$journeyShift==TRUE),]$tripIDShift

    # assign a negative tripID to the stationary points
    if(length(which(inputDataFrame$sameJourney==FALSE&inputDataFrame$stationary==TRUE))>0){
      inputDataFrame[which(inputDataFrame$sameJourney==FALSE&inputDataFrame$stationary==TRUE),]$tripID<-(-9000)-seq(1,length(which(inputDataFrame$sameJourney==FALSE & inputDataFrame$stationary==TRUE)),1)
    }


    #  summarise the overall lines stats
    tripID<-unique(inputDataFrame[which(inputDataFrame$tripID>0),]$tripID)
    totDur<-tapply(inputDataFrame[which(inputDataFrame$tripID>0),]$duration, inputDataFrame[which(inputDataFrame$tripID>0),]$tripID, sum)
    totDist<-tapply(inputDataFrame[which(inputDataFrame$tripID>0),]$distGeo, inputDataFrame[which(inputDataFrame$tripID>0),]$tripID, sum)
    minSpeed<-tapply(inputDataFrame[which(inputDataFrame$tripID>0),]$speed, inputDataFrame[which(inputDataFrame$tripID>0),]$tripID, min)
    maxSpeed<-tapply(inputDataFrame[which(inputDataFrame$tripID>0),]$speed, inputDataFrame[which(inputDataFrame$tripID>0),]$tripID, max)
    avgSpeed<-3.6*totDist/totDur
    method<-tapply(inputDataFrame[which(inputDataFrame$tripID>0),]$method_desc, inputDataFrame[which(inputDataFrame$tripID>0),]$tripID, unique)
    startLocX<-tapply(inputDataFrame[which(inputDataFrame$tripID>0),]$from_locx, inputDataFrame[which(inputDataFrame$tripID>0),]$tripID, dplyr::first)
    startLocY<-tapply(inputDataFrame[which(inputDataFrame$tripID>0),]$from_locy, inputDataFrame[which(inputDataFrame$tripID>0),]$tripID, dplyr::first)
    endLocX<-tapply(inputDataFrame[which(inputDataFrame$tripID>0),]$to_locx, inputDataFrame[which(inputDataFrame$tripID>0),]$tripID, dplyr::last)
    endLocY<-tapply(inputDataFrame[which(inputDataFrame$tripID>0),]$to_locy, inputDataFrame[which(inputDataFrame$tripID>0),]$tripID, dplyr::last)

    lineStats<-as.data.frame(cbind(tripID,totDur,totDist,minSpeed,maxSpeed,avgSpeed,method,startLocX,startLocY,endLocX,endLocY)); rm(tripID,totDur,totDist,minSpeed,maxSpeed,avgSpeed,method,startLocX,startLocY,endLocX,endLocY)

    # extract journey lines from TripIDs > 0 (using Line and then adding them all into a SpatialLinesDatFrame)
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

  } # end of loop over all agent files
}
