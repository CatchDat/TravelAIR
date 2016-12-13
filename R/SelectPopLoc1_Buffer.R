#' Select popular locations for each of the agents - part 1: Buffer measurement locations
#'
#' Creates a buffer of a set distance around the measurement locations, in order to be able to determine in the subsequent part (2: Identify popular locations) where agents spend most of their time, which should include their "home", "work" and other popular locations
#'
#' ----- NOTE! ----- uses spatstat library, so requires R version 3.3.1 (Sept 2016), but subsequent steps require rgeos library, which doesn't work under 3.3.1
#'

#
#' @param inputPath character; the path to the folder where the input *.csv files are located
#' @param inputName character (optional); select only file names which contain the string specified; default is "Cleaned_Agent"
#' @param proximityDistance numeric; set a distance in decimal degrees, which will be used as a radius to determine the number of close by locations; default is 20 meters
#' @param maxLocations numeric (optional); will split up the input agent files into chunks of this size, to keep the processing manageable; default is 25000
#' @param popLoc character (optional); name of the popular location to be determined, presets (for specific values under 'decimalTimeSlot' and 'weekdays' below) include "home" and "work"; default is "home"
#' @param decimalTimeSlot numeric vector (optional); specify times between which to find the popular locations; default is c(0.0,4.0) for identifying "home" locations; default is c(9.0,17.0) for "work" locations; set to NA to not override default settings
#' @param weekdays character vector (optional); the days of the week the popular locations will be calculated for for (e.g. c("Sunday","Wednesday","Thursday") or alternatively use "midweek" (Tuesday to Thursday), "week" (Monday to Friday), "weekend" (Saturday and Sunday) or NA to skip this step altogether; for popLoc='home' "weekdays" is used (default); for popLoc='work' "weekdays" is the default; set to NA to not override default settings
#' @param outputPath character (optional); the path to the folder to write the output spatial polygon buffer files to, set to NA to not write output files; default is the inputPath
#' @param outputName character (optional); if outputPath is not NA, will prefix the output filenames ("Agent-ID#.extension") with this; default is "Buffer", set to NA to just have the AgentIDs
#'
#' @export

selectPopLocsBuffer<-function(inputPath, inputName="ReclassedMethods_Agent", proximityDistance=0.0005, maxLocations=15000,
                              popLoc="home", decimalTimeSlot=c(0.0,4.0), weekdays=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"),
                              outputPath=inputPath, outputName="Buffers"){

  fileNames<-list.files(inputPath)
  fileNames<-fileNames[grep(inputName,fileNames)]

  if(popLoc=="home"|popLoc=="Home"){
    if(length(decimalTimeSlot==1)&&is.na(decimalTimeSlot)){
      decimalTimeSlot=c(0.0,4.0)
    }
    if(length(weekdays==1)&&is.na(weekdays)){
      weekdays=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
    }
  }

  if(popLoc=="work"|popLoc=="Work"){
    if(length(decimalTimeSlot==1)&&is.na(decimalTimeSlot)){
      decimalTimeSlot=c(9.0,17.0)
    }
    if(length(weekdays==1)&&is.na(weekdays)){
      weekdays=c("Monday","Tuesday","Wednesday","Thursday","Friday")
    }
  }

  if(length(weekdays)==1&&!is.na(weekdays)){
    # define mideweek as Tue-Thu, weekdays as Mon-Fri and weekend as Sat-Sun
    if(weekdays=="midweek"|weekdays=="Midweek"|weekdays=="mid-week"|weekdays=="Mid-week"|weekdays=="midWeek"|weekdays=="MidWeek"){
      weekdays<-c("Tuesday","Wednesday","Thursday")
    }
    if(length(weekdays==1)&(weekdays=="week"|weekdays=="Week")&&!is.na(weekdays)){
      weekdays<-c("Monday","Tuesday","Wednesday","Thursday","Friday")
    } else if(length(weekdays==1)&(weekdays=="weekend"|weekdays=="Weekend")&&!is.na(weekdays)){
      weekdays<-c("Saturday","Sunday")
    }
  }

  # keep track of agents which have been skipped due to no measurements in the time slot specified and which have been processed
  noOfSkippedAgents<-0
  skippedAgentIDs<-NA
  noOfProcessedAgents<-0
  processedAgentIDs<-NA

  for(i in 1:length(fileNames)){

    # read in file i
    inputDataFrame<-read.csv(paste(inputPath,fileNames[i],sep=""),as.is=T)
    agentID<-inputDataFrame[1,"agentID"]
    print(paste("Working on agent ",i," out of ",length(fileNames)," with agent ID ",agentID,", which has ",nrow(inputDataFrame), " measurements.",sep=""))

    inputDataFrame$decimalTimePrevDay<-inputDataFrame$decimalTime
    if(length(which((inputDataFrame$decimalTime+(inputDataFrame$duration/3600))>24))>0){
      inputDataFrame[which((inputDataFrame$decimalTime+(inputDataFrame$duration/3600))>24),]$decimalTimePrevDay<-inputDataFrame[which((inputDataFrame$decimalTime+(inputDataFrame$duration/3600))>24),]$decimalTime-24
    }

    # need to build in a bit on what to do if the agent doesn't have any measurements in the time slot specified - or at the very least to skip the agent altogether
    if(length(which((inputDataFrame$decimalTimePrevDay<=decimalTimeSlot[2]&(inputDataFrame$decimalTimePrevDay+(inputDataFrame$duration/3600))>=decimalTimeSlot[1])|
                    ((inputDataFrame$decimalTimePrevDay<=decimalTimeSlot[1])&(inputDataFrame$decimalTimePrevDay+(inputDataFrame$duration/3600))>=decimalTimeSlot[2])))>0){

      noOfProcessedAgents<-noOfProcessedAgents+1
      if(noOfProcessedAgents>1){
        processedAgentIDs<-c(processedAgentIDs,agentID)
      } else{
        processedAgentIDs<-agentID
      }

      # limit the times to those specified in 'decimalTimeslot'
      # so, include measurement as falling between thresh1-thresh2 if:
      #   startTime > thresh1 & endTime < thresh2
      #   (startTime > thresh1 & startTime < thresh2) & endTime > thresh2
      #   startTime < thresh1 & (endTime > thresh1 & endTime < thresh2)
      #   startTime < thresh1 & endTime > thresh2
      # where startTime = decimalTimePrevDay, endTime = (decimalTimePrevDay + (duration/3600)), thresh1 = decimalTimeSlot[1], thresh2 = decimalTimeSlot[2]
      # or, more compactly (first three combined):
      #   startTime < thresh2 & endTime > thresh1
      #   startTime < thresh1 & endTime > thresh2
      # using decTimePrevDay this should also take into account if the measurement interval started on the 'previous' day

      #inputDataFrame<-inputDataFrame[which((inputDataFrame$decimalTimePrevDay>=decimalTimeSlot[1]&(inputDataFrame$decimalTimePrevDay+(inputDataFrame$duration/3600))<=decimalTimeSlot[2])|
      #                                       ((inputDataFrame$decimalTimePrevDay>=decimalTimeSlot[1]&inputDataFrame$decimalTimePrevDay<=decimalTimeSlot[2])&(inputDataFrame$decimalTimePrevDay+(inputDataFrame$duration/3600))>=decimalTimeSlot[2])|
      #                                       (inputDataFrame$decimalTimePrevDay<=decimalTimeSlot[1]&((inputDataFrame$decimalTimePrevDay+(inputDataFrame$duration/3600))>=decimalTimeSlot[1]&(inputDataFrame$decimalTimePrevDay+(inputDataFrame$duration/3600))<=decimalTimeSlot[2]))|
      #                                       ((inputDataFrame$decimalTimePrevDay<=decimalTimeSlot[1])&(inputDataFrame$decimalTimePrevDay+(inputDataFrame$duration/3600))>=decimalTimeSlot[2])),]
      inputDataFrame<-inputDataFrame[which((inputDataFrame$decimalTimePrevDay<=decimalTimeSlot[2]&(inputDataFrame$decimalTimePrevDay+(inputDataFrame$duration/3600))>=decimalTimeSlot[1])|
                                             ((inputDataFrame$decimalTimePrevDay<=decimalTimeSlot[1])&(inputDataFrame$decimalTimePrevDay+(inputDataFrame$duration/3600))>=decimalTimeSlot[2])),]


      # limit the weekdays to those specified in 'weekdays'
      if(length(weekdays)>1||!is.na(weekdays)){
        inputDataFrame<-inputDataFrame[which(inputDataFrame$weekDay%in%weekdays),]
      }
      # create a minimalistic version of the input dataframe (only date, dateTime, weekDay, decimalTime, distGeo, duration, method_desc, from_locx, from_locy)
      fullInputDataFrame<-inputDataFrame[,c("date","dateTime","weekDay","decimalTime","distGeo","duration","method_desc","from_locx","from_locy")]
      #fullInputDataFrame<-inputDataFrame[,c("from_locx","from_locy")]

      noOf15kRuns<-ceiling(nrow(fullInputDataFrame)/maxLocations)

      for(k in 1:noOf15kRuns){
        if(k!=noOf15kRuns){
          inputDataFrame<-fullInputDataFrame[((k-1)*maxLocations+1):((k*maxLocations)-1),]
        } else{
          inputDataFrame<-fullInputDataFrame[((k-1)*maxLocations+1):nrow(fullInputDataFrame),]
        }

        # summarise the origin point statistics
        pointStats<-as.data.frame(cbind(row.names(inputDataFrame),inputDataFrame))

        colnames(pointStats)[1]<-"pointRowID"

        # convert the origin points to spatialMultiPoints
        sPoints<-list()

        for(j in 1:nrow(inputDataFrame)){
          startCoords<-cbind(inputDataFrame$from_locx, inputDataFrame$from_locy)
          sPoints[[j]]<-list(startCoords)
        }

        sSPPoints<-sp::SpatialMultiPoints(sPoints)

        # create spatial points dataframe
        sSPDF<-sp::SpatialMultiPointsDataFrame(sSPPoints, data=pointStats)

        # define projection
        proj_wgs84<-"+init=epsg:3395"
        sp::proj4string(sSPDF)<-sp::CRS(proj_wgs84)

        # create a circular buffer without rgeos as per http://grokbase.com/t/r/r-sig-geo/102f4eqgtr/compute-buffer-from-point-shapefile-to-have-shapefile
        allBuffs<-list()
        for(j in 1:nrow(inputDataFrame)){
          circBuff<-spatstat::disc(radius=0.0005, centre=c(inputDataFrame[j,]$from_locx,inputDataFrame[j,]$from_locy))
          polyBuff<-sp::Polygon(rbind(cbind(circBuff$bdry[[1]]$x,circBuff$bdry[[1]]$y),c(circBuff$bdry[[1]]$x[1],y=circBuff$bdry[[1]]$y[1])))
          allBuffs<-c(allBuffs,polyBuff)
        }

        sAllBuffs<-list()

        for(j in 1:length(allBuffs)){
          sPolyBuff<-sp::Polygons(list(allBuffs[[j]]),ID=row.names(inputDataFrame)[j])
          sAllBuffs<-c(sAllBuffs,sPolyBuff)
        }

        polyBuffs<-sp::SpatialPolygons(sAllBuffs)

        sBuffDF<-sp::SpatialPolygonsDataFrame(polyBuffs, data=pointStats)

        wgs84 = "+proj=longlat +datum=WGS84"

        sp::proj4string(sBuffDF)<-sp::CRS(wgs84)

        # need geographical projection for writing out kml files
        wgs84 = "+proj=longlat +datum=WGS84"

        #sp::proj4string(sSPDF)<-sp::CRS(wgs84)
        sp::proj4string(sBuffDF)<-sp::CRS(wgs84)

        # write out the spatial polygon dataframe buffers to kml
        if(!is.na(outputPath)){
          if(!is.na(outputName)){
            outputFileNamePolygons<-paste(outputPath,outputName,"-",popLoc,"-Polygons_Agent-",agentID,"_Part-",k,".kml",sep="")
            out2Polygons<-paste(outputPath,outputName,"-",popLoc,"-Polygons_Agent-",agentID,"_Part-",k,sep="")
          } else{
            outputFileNamePolygons<-paste(outputPath,outputName,"-",popLoc,"-Polygons_Agent-",agentID,"_Part-",k,".kml",sep="")
            out2Polygons<-paste(outputPath,outputName,"-",popLoc,"-Polygons_Agent-",agentID,"_Part-",k,sep="")
          }
          rgdal::writeOGR(sBuffDF, outputFileNamePolygons, out2Polygons, driver="KML")
        } else {
          noOfSkippedAgents<-noOfSkippedAgents+1
          if(noOfSkippedAgents>1){
            skippedAgentIDs<-c(skippedAgentIDs,agentID)
          } else {
            skippedAgentIDs<-agentID
          }
        }
      } # end of loop over runs of 15k each
    } # end of if-else statement to skip those that don't have measurements in the time slot specified
  } # end of loop over agents
  if(!is.na(outputPath)){
    if(!is.na(outputName)){
      outputFileNameSkipped<-paste(summaryPath,outputName,"-",popLoc,"-TimeSlot_",decimalTimeSlot[1],"-",decimalTimeSlot[2],"-SkippedAgentIDs.csv",sep="")
      outputFileNameProcessed<-paste(summaryPath,outputName,"-",popLoc,"-TimeSlot_",decimalTimeSlot[1],"-",decimalTimeSlot[2],"-ProcessedAgentIDs.csv",sep="")
    } else{
      outputFileNameSkipped<-paste(summaryPath,outputName,"-",popLoc,"-TimeSlot_",decimalTimeSlot[1],"-",decimalTimeSlot[2],"-SkippedAgentIDs.csv",sep="")
      outputFileNameProcessed<-paste(summaryPath,outputName,"-",popLoc,"-TimeSlot_",decimalTimeSlot[1],"-",decimalTimeSlot[2],"-ProcessedAgentIDs.csv",sep="")
    }
    write.csv(skippedAgentIDs,outputFileNameSkipped,row.names=F)
    write.csv(processedAgentIDs,outputFileNameProcessed,row.names=F)
  }
}


