#' Read in the 'new' Catch! TravellAI geojson data (Aug '14 - Aug '16) in the 'original' format and export to csv files
#'
#' Adds individual x and y coordinate columns (for both 'from' and 'to' locations), day of the week, decimal time of day, unique agent ID column, geodesic and haversine distances between origin and destination locations, and speeds (in km/h).
#' Removes all duplicate entries, since most entries seem to be listed in triplicate.
#' Tidies up "duration" (removes negative entries, sets it to the duration between two consecutive measurements (if listed duration exceeds this), deals with '0' values)
#' Optionally creates a summary file with the device ID, assigned agent ID, number of (unique) measurements, number of duplicate(/triplicate) entries removed, number of location jumps for each processed agent, min/mean/median/max X and Y coordinates, first/last observation date and the number of days between them, total observation duration (days), and relative observation duration for each weekday and mode of transport
#' Optionally can be used to select only a specific date range.
#'
#' @param inputPath character; the path to the folder where the json files are located
#' @param minObsDate character (optional); "YYYY-MM-DD" of minimum observation date to include; default is "2014-01-01", set to NA to exclude
#' @param maxObsDate character (optional); "YYYY-MM-DD" of maximum observation date to include; default is "2016-10-01", set to NA to exclude
#' @param saveOutput boolean (optional); write out the data frame with the added columns; default is TRUE
#' @param outputPath character (optional); the path to the folder where the updated csv files should be saved if saveOutput is set to TRUE; default is inputPath
#' @param saveSummary boolean (optional); write out a summary table, the number of (unique) measurements, duplicate(/triplicate) entries removed, location jumps for each processed device/agent; default is TRUE
#' @param summaryName character (optional); name for the summary file; default is "Aug14-16_Summary.csv"
#' @param summaryPath character (optional); the path to the folder where the summary file should be saved if saveOutput is set to TRUE; default is inputPath
#'
#' @export

readData<-function(inputPath, minObsDate="2014-01-01", maxObsDate="2016-10-01",
                   saveOutput=TRUE, outputPath=inputPath,
                   saveSummary=TRUE, summaryName="Aug14-16_Summary", summaryPath=inputPath){
  # read in filename(s)
  fileNames<-list.files(inputPath)

  deviceID<-rep(NA,length(fileNames)); agentID<-rep(NA,length(fileNames)); noUniqObsv<-rep(NA,length(fileNames)); noRemDuplObsv<-rep(NA,length(fileNames)); noLocJumps<-rep(NA,length(fileNames));
  emptyFile<-rep(NA,length(fileNames)); minX<-rep(NA,length(fileNames)); minY<-rep(NA,length(fileNames)); meanX<-rep(NA,length(fileNames)); meanY<-rep(NA,length(fileNames)); medianX<-rep(NA,length(fileNames)); medianY<-rep(NA,length(fileNames)); maxX<-rep(NA,length(fileNames)); maxY<-rep(NA,length(fileNames))
  minDate<-rep(NA,length(fileNames)); maxDate<-rep(NA,length(fileNames)); dateRange<-rep(NA,length(fileNames)); totDur<-rep(NA,length(fileNames)); relSunDur<-rep(NA,length(fileNames)); relMonDur<-rep(NA,length(fileNames)); relTueDur<-rep(NA,length(fileNames)); relWedDur<-rep(NA,length(fileNames)); relThuDur<-rep(NA,length(fileNames)); relFriDur<-rep(NA,length(fileNames)); relSatDur<-rep(NA,length(fileNames))
  relCarDur<-rep(NA,length(fileNames)); relBikeDur<-rep(NA,length(fileNames)); relWalkDur<-rep(NA,length(fileNames)); relStatDur<-rep(NA,length(fileNames)); relTrChDur<-rep(NA,length(fileNames)); relMetDur<-rep(NA,length(fileNames)); relNatRailDur<-rep(NA,length(fileNames)); relBusDur<-rep(NA,length(fileNames)); relUnknDur<-rep(NA,length(fileNames)); relShWalkDur<-rep(NA,length(fileNames)); relCarPetrSmDur<-rep(NA,length(fileNames)); relPlaneDur<-rep(NA,length(fileNames)); relIntRailDur<-rep(NA,length(fileNames))
  summaryTable<-as.data.frame(cbind(deviceID,agentID, noUniqObsv, noRemDuplObsv, noLocJumps,emptyFile,minX,minY,meanX,meanY,medianX,medianY,maxX,maxY,minDate,maxDate,dateRange,totDur,relSunDur,relMonDur,relTueDur,relWedDur,relThuDur,relFriDur,relSatDur,relCarDur,relBikeDur,relWalkDur,relStatDur,relTrChDur,relMetDur,relNatRailDur,relBusDur,relUnknDur,relShWalkDur,relCarPetrSmDur,relPlaneDur,relIntRailDur))
  rm(deviceID,agentID, noUniqObsv, noRemDuplObsv, noLocJumps,emptyFile,minX,minY,meanX,meanY,medianX,medianY,maxX,maxY,minDate,maxDate,dateRange,totDur,relSunDur,relMonDur,relTueDur,relWedDur,relThuDur,relFriDur,relSatDur,relCarDur,relBikeDur,relWalkDur,relStatDur,relTrChDur,relMetDur,relNatRailDur,relBusDur,relUnknDur,relShWalkDur,relCarPetrSmDur,relPlaneDur,relIntRailDur)

  if(!is.na(minObsDate)){
    minDateVarName<-paste("noObsvBef",minObsDate,sep="")
    noObsBefMinDate<-rep(NA,length(fileNames))
    summaryTable<-cbind(summaryTable,noObsBefMinDate)
    colnames(summaryTable)[which(colnames(summaryTable)=="noObsBefMinDate")]<-minDateVarName
  }
  if(!is.na(maxObsDate)){
    maxDateVarName<-paste("noObsvAft",maxObsDate,sep="")
    noObsBefMaxDate<-rep(NA,length(fileNames))
    summaryTable<-cbind(summaryTable,noObsBefMaxDate)
    colnames(summaryTable)[which(colnames(summaryTable)=="noObsBefMaxDate")]<-maxDateVarName
  }

  relWeekdayDurSeq<-c("relSunDur","relMonDur","relTueDur","relWedDur","relThuDur","relFriDur","relSatDur")
  relMethDurSeq<-c("relCarDur","relBikeDur","relWalkDur","relStatDur","relTrChDur","relMetDur","relNatRailDur","relBusDur","relUnknDur","relShWalkDur","relCarPetrSmDur","relPlaneDur","relIntRailDur")
  methodsSeq<-c("Car", "Bicycle", "Walk", "Stationary", "TrainChange", "Metro", "NationalRail","Bus", "Unknown", "ShortWalk", "CarPetrolSmall", "Aeroplane", "InternationalRail")

  for(i in 1:length(fileNames)){
    # read in file i
    catchFile<-jsonlite::fromJSON(paste(inputPath,fileNames[i],sep=""))

    # deal witih "empty" files
    if(is.data.frame(catchFile)){
      summaryTable[i,]$deviceID<-catchFile$device_id[1];summaryTable[i,]$agentID<-i;summaryTable[i,]$noUniqObsv<-nrow(unique(catchFile));summaryTable[i,]$noRemDuplObsv<-nrow(catchFile)-nrow(unique(catchFile))

      catchFile<-unique(catchFile)

      # add agent ID
      catchFile$agentID<-i

      print(paste("Working on agent ",i," out of ",length(fileNames),", which has ",nrow(catchFile)," observation(s)",sep=""))

      # add individual coordinate columns (could make optional)
      catchFile$from_locx <- sapply(catchFile$from_loc, `[[`, 1)
      catchFile$from_locy <- sapply(catchFile$from_loc, `[[`, 2)
      catchFile$to_locx <- sapply(catchFile$to_loc, `[[`, 1)
      catchFile$to_locy <- sapply(catchFile$to_loc, `[[`, 2)

      # display all digits of ts variable
      options(digits=13)

      # add date, weekdays, and decimal time
      catchFile$date<-as.Date(catchFile$ts/86400000, origin="1970-01-01")
      catchFile$dateTime <- as.POSIXlt(catchFile$ts/1000, format="%Y-%m-%d %H:%M:%S", origin="1970-01-01 00:00:00")
      catchFile$wday<- unclass(catchFile$dateTime)$wday # ranges from 0-6 where Monday is 1 and Sunday is 0 (so Saturday 6)
      catchFile$weekDay<-NA; weekSeq<-seq(0,6,1); daySeq<-c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
      for(j in 1:length(weekSeq)){
        if(length(which(catchFile$wday==weekSeq[j]))>0){
          catchFile[which(catchFile$wday==weekSeq[j]),]$weekDay<-daySeq[j]
        }
      }; rm(j)
      catchFile$decimalTime<-unclass(catchFile$dateTime)$hour+(unclass(catchFile$dateTime)$min/60)+(unclass(catchFile$dateTime)$sec/3600)

      # (optional) select specified min/max date range
      if(!is.na(minObsDate)){
        summaryTable[i,minDateVarName]<-length(which(catchFile$date<as.Date(minObsDate)))
        summaryTable[i,]$noUniqObsv<-summaryTable[i,]$noUniqObsv-length(which(catchFile$date<as.Date(minObsDate)))
        catchFile<-catchFile[which(catchFile$date>=as.Date(minObsDate)),]
      }
      if(!is.na(maxObsDate)){
        summaryTable[i,maxDateVarName]<-length(which(catchFile$date>as.Date(maxObsDate)))
        summaryTable[i,]$noUniqObsv<-summaryTable[i,]$noUniqObsv-length(which(catchFile$date>as.Date(maxObsDate)))
        catchFile<-catchFile[which(catchFile$date<=as.Date(maxObsDate)),]
      }

      # remove observations with a negative duration
      if(length(which(catchFile$duration<0))>0){
        catchFile<-catchFile[which(catchFile$duration>=0),]
      }

      # there are agents with only 1 observation, so need to allow for this in the "nextTimeStamp" and "locJump" calculations
      if(nrow(catchFile)>1){
        # calculate duration (in seconds) as the difference between the current timestamp and the next timestamp and convert from milliseconds to seconds
        catchFile<-catchFile[order(catchFile$ts),]
        catchFile$nextTimeStamp<-c(catchFile[2:(nrow(catchFile)),"ts"], catchFile[nrow(catchFile),"ts"]); catchFile$nextTimeStamp[nrow(catchFile)]<-NA
        catchFile$calcDuration<-(catchFile$nextTimeStamp-catchFile$ts)/1000

        # replace "duration" with calcDuration if duration > calcDuration (because duration of a leg of a journey between two consecutive measurements can't be more than the duration between them)
        catchFile[which(catchFile$duration>catchFile$calcDuration),]$duration<-catchFile[which(catchFile$duration>catchFile$calcDuration),]$calcDuration

        # determine if there has been a location "jump" (catchFile[i+1,]$from_loc!=catchFile[i,]$to_loc)
        catchFile$nextFromLocX<-c(catchFile[2:(nrow(catchFile)),"from_locx"], catchFile[nrow(catchFile),"from_locx"]); catchFile$nextFromLocX[nrow(catchFile)]<-NA
        catchFile$nextFromLocY<-c(catchFile[2:(nrow(catchFile)),"from_locy"], catchFile[nrow(catchFile),"from_locy"]); catchFile$nextFromLocY[nrow(catchFile)]<-NA
        catchFile$locJump<-rep(0,nrow(catchFile))
        if(length(which(catchFile$to_locx!=catchFile$nextFromLocX&catchFile$to_locy!=catchFile$nextFromLocY))>1){
          catchFile[which(catchFile$to_locx!=catchFile$nextFromLocX&catchFile$to_locy!=catchFile$nextFromLocY),]$locJump<-1
        }
        summaryTable[i,]$noLocJumps<-length(which(catchFile$locJump==1))
        # noOfLocJumps will be NA for files with only one entry; more appropriate than 0
      }








      # (optional) select agents based on whether they have measurements within certain (optionally preset) location bounds









      # calculate geodesic and haversine distances in meters from to-from coordinates
      catchFile$distGeo<-geosphere::distGeo(cbind(catchFile$from_locx, catchFile$from_locy), cbind(catchFile$to_locx, catchFile$to_locy))
      catchFile$distHaversine<-geosphere::distHaversine(cbind(catchFile$from_locx, catchFile$from_locy), cbind(catchFile$to_locx, catchFile$to_locy))

      # calculate the speeds (in km/h) based on the device distances and durations, unless the device duration is listed as "0", in which case use "1"
      catchFile$speed<-rep(NA,nrow(catchFile))
      if(length(which(catchFile$duration>0))>0){catchFile[which(catchFile$duration>0),]$speed<-3.6*catchFile[which(catchFile$duration>0),]$distance/as.numeric(catchFile[which(catchFile$duration>0),]$duration)}
      if(length(which(catchFile$duration==0))>0){catchFile[which(catchFile$duration==0),]$speed<-3.6*catchFile[which(catchFile$duration==0),]$distance/1}

      # write out the results if saveOutput==TRUE
      if(saveOutput==TRUE){
        # convert the to_loc and from_loc lists to character fields to enable writing of the output file
        catchFile[,"to_loc"]<-paste("[",unlist(catchFile[,"to_loc"])[1],",",unlist(catchFile[,"to_loc"])[2],"]",sep="")
        catchFile[,"from_loc"]<-paste("[",unlist(catchFile[,"from_loc"])[1],",",unlist(catchFile[,"from_loc"])[2],"]",sep="")

        outFile<-paste(outputPath,"Base_Agent-",i,".csv",sep="")
        write.csv(catchFile,outFile,row.names=F)
      }

      summaryTable[i,]$emptyFile<-0

      # generate some more summary statistics like min/mean/max X/Y coords, min/max date, tot duration, relative observation duration for each weekday and mode of transport
      if(saveSummary==TRUE){
        summaryTable[i,]$minX<-min(catchFile$from_locx)
        summaryTable[i,]$minY<-min(catchFile$from_locy)
        summaryTable[i,]$meanX<-mean(catchFile$from_locx)
        summaryTable[i,]$meanY<-mean(catchFile$from_locy)
        summaryTable[i,]$medianX<-median(catchFile$from_locx)
        summaryTable[i,]$medianY<-median(catchFile$from_locy)
        summaryTable[i,]$maxX<-max(catchFile$from_locx)
        summaryTable[i,]$maxY<-max(catchFile$from_locy)
        summaryTable[i,]$minDate<-as.character(min(catchFile$date))
        summaryTable[i,]$maxDate<-as.character(max(catchFile$date))
        summaryTable[i,]$dateRange<-round(difftime(max(catchFile$date),min(catchFile$date),units = "days"),0)
        summaryTable[i,]$totDur<-sum(catchFile$duration)/(60*60*24)
        for(j in 1:length(weekSeq)){
          summaryTable[i,relWeekdayDurSeq[j]]<-sum(catchFile[which(catchFile$wday==(j-1)),]$duration)/sum(catchFile$duration)
        }; rm(j)
        for(j in 1:length(methodsSeq)){
          summaryTable[i,relMethDurSeq[j]]<-sum(catchFile[which(catchFile$method_desc==methodsSeq[j]),]$duration)/sum(catchFile$duration)
        }; rm(j)
      }
    } else{
      summaryTable[i,]$deviceID<-substr(fileNames[i],1,36)
      summaryTable[i,]$emptyFile<-1
    } # end of "if not an empty file" statement
  } ; rm(i) # end of loop over all devices (i)

  # write out the summary file results if saveSummary==TRUE
  if(saveSummary==TRUE){
    summaryFile<-paste(summaryPath,summaryName,".csv",sep="")
    write.csv(summaryTable,summaryFile,row.names=F)
  }
}
