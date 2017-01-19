#' Randomly pick 10 journeys for a sample dataset
#'
#' Picks a random agent, then picks a random trip ID from this for a sample dataset of 10 journeys
#' Optionally can set a minimum number of observations required for each journey
#'
#' @param inputPath character; the path to the folder where the agents' journey files are located
#' @param inputName character; (part of) the agents' filename(s) to randomly select the journeys from; default is "TSC"
#' @param numberOfSampleJourneys numeric (optional); specify a number greater than or equal to 1 for the number of desired sample journeys to be generated; default is 10
#' @param minObs numeric (optional); minimum number of observations required per journey; default is 100
#' @param outputPath character (optional); the path to the existing folder where the selected routes should be written to; default is inputPath + "RandJourneySel/"
#' @param outputName character (optional); the name of the output kml files of the randomly selected routes; default is "randJourney" + agentID + tripID
#'
#' @export


selectRandomJourneys<-function(inputPath, inputName="TSC",noOfSampleJourneys=10, minObs=100, outputPath=paste(inputPath,"RandJourneySel/",sep=""), outputName="randJourneys"){

  # read in filename(s)
  fileNames<-list.files(inputPath)
  fileNames<-fileNames[grep(inputName,fileNames)]


  noRandJourneysPicked<-0
  while(noRandJourneysPicked<=noOfSampleJourneys){
    randAgentID<-ceiling(runif(1,0,1)*length(fileNames))
    inputDataFrame<-read.csv(paste(inputPath,fileNames[randAgentID],sep=""),as.is=T)

    if(nrow(inputDataFrame)>100){
      if(max(tapply(inputDataFrame$dateTime,inputDataFrame$tripID,length))>minObs){
        qualTripIDs<-as.numeric(tapply(inputDataFrame$tripID,inputDataFrame$tripID,unique)[which(tapply(inputDataFrame$dateTime,inputDataFrame$tripID,length)>=minObs)])
        #inputDataFrame<-inputDataFrame[which(inputDataFrame$tripID%in%qualTripIds),]
        randTripID<-qualTripIDs[ceiling(runif(1,0,1)*length(qualTripIDs))]
        inputDataFrame<-inputDataFrame[which(inputDataFrame$tripID==randTripID),]
        inputDataFrame<-inputDataFrame[,which(colnames(inputDataFrame)%in%c("device_id","agentID","tripID","from_locx","from_locy","to_locx","to_locy","date","dateTime","weekDay","decimalTIme","distance","distGeo","duration","calcDuration","speed","method_desc"))]
        inputDataFrame<-inputDataFrame[,c(3,5,16,4,6:12,3,14,2,13,15)]

        #write out the randomly selected journey as csv file
        write.csv(inputDataFrame,paste(outputPath,outputName,"_Agent-",inputDataFrame[1,]$agentID,"_Trip-",inputDataFrame[1,]$tripID,".csv",sep=""),row.names=F)

        # convert the dataframe to a spatial one for writing out as kml file

        #  summarise the overall lines stats
        tripID<-inputDataFrame[1,]$tripID
        startDate<-inputDataFrame[1,]$date
        endDate<-inputDataFrame[nrow(inputDataFrame),]$date
        startTime<-inputDataFrame[1,]$decimalTime
        endTime<-inputDataFrame[nrow(inputDataFrame),]$decimalTime
        totDur<-sum(inputDataFrame$duration)
        totDist<-sum(inputDataFrame$distance)
        totCalcDur<-sum(inputDataFrame$calcDuration)
        totCalcDistGeo<-sum(inputDataFrame$distGeo)
        minSpeed<-min(inputDataFrame$speed)
        maxSpeed<-max(inputDataFrame$speed)
        avgSpeed<-3.6*totDist/totDur
        avgCalcSpeed<-3.6*totCalcDistGeo/totCalcDur
        methods<-NA
        tripMethods<-unique(inputDataFrame$method_desc)
        for(k in 1:length(tripMethods)){
          if(k==1){
            methods<-tripMethods[k]
          } else {
            methods<-paste(methods, ", ", tripMethods[k],sep="")
          }
        }
        startLocX<-inputDataFrame[1,]$from_locx
        startLocY<-inputDataFrame[1,]$from_locy
        endLocX<-inputDataFrame[nrow(inputDataFrame),]$to_locx
        endLocY<-inputDataFrame[nrow(inputDataFrame),]$to_locy

        lineStats<-as.data.frame(cbind(tripID,startDate,endDate,startTime,endTime,totDur,totCalcDur,totDist,totCalcDistGeo,minSpeed,maxSpeed,avgSpeed,avgCalcSpeed,methods,startLocX,startLocY,endLocX,endLocY))

        sALines<-list()
        startCoords<-cbind(startLocX,startLocY)
        destCoords<-cbind(inputDataFrame[which(!is.na(inputDataFrame$to_locx)),]$to_locx,inputDataFrame[which(!is.na(inputDataFrame$to_locy)),]$to_locy)
        sALines[[1]]<-sp::Lines(list(sp::Line(rbind(startCoords, destCoords))), tripID)

        # convert to spatial lines
        sASPLines<-sp::SpatialLines(sALines)

        # create spatial lines dataframe
        sASLDF<-sp::SpatialLinesDataFrame(sASPLines, data=lineStats, match.ID="tripID")

        # convert to BNG projection
        bng<-"+init=epsg:27700"
        wgs84<-"+init=epsg:4326"

        sp::proj4string(sASLDF)<-sp::CRS(wgs84)
        sASLDF_bng<-sp::spTransform(sASLDF,sp::CRS(wgs84))

        # write out the kml file
        outputFileName<-paste(outputPath,outputName,"_Agent-",inputDataFrame[1,]$agentID,"_Trip-",inputDataFrame[1,]$tripID,".kml",sep="")
        out2<-paste(outputPath,outputName,"_Agent-",inputDataFrame[1,]$agentID,"_Trip-",inputDataFrame[1,]$tripID,sep="")

        rgdal::writeOGR(sASLDF_bng, outputFileName, out2, driver="KML")

        noRandJourneysPicked<-noRandJourneysPicked+1
      }
    }
  }

}
