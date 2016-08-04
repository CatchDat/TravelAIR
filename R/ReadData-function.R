#' Read in the 'new' Catch! TravellAI geojson data (Aug '14 - Aug '15)
#' 
#' Adds individual x and y coordinate columns (for both 'from' and 'to' locations; the latter is calculated as the origin of the next observation, 
#'  since currently the origins and destinations are identical), day of the week, decimal time of day, duration, unique agent ID column, 
#'  geodesic and haversine distances between origin and destination locations (since the current distance column only has 0 values due to origin 
#'  and destination coordinates being the same), and speeds (in km/h) based on the calculated distances and durations.
#' Removes all entries with duration = 0, since they seem to be duplicate entries.
#' Optionally can be used to select only a specific date range.
#' 
#' @param inputPath character; the path to the folder where the json files are located
#' @param minDate integer (optional); YYMMDD of minimum observation date to include; default is NA
#' @param maxDate integer (optional); YYMMDD of maximum observation date to include; default (currently) is 160131, set to NA to exclude
#' @param saveOutput boolean (optional); write out the data frame with the added columns; default is TRUE
#' @param outputPath character (optional); the path to the folder where the updated json files should be saved if saveOutput is set to TRUE
#' 

readData<-function(inputPath="/Users/Nikee/Documents/Work/Projects/CatchTravelAI/Data/TravelAi_WeCycle_Aug14-Aug15/", minDate=NA, maxDate=NA, 
                   saveOutput=TRUE, outputPath="/Users/Nikee/Documents/Work/Projects/CatchTravelAI/Data/NewAgents/BasicData/"){
  # read in filename(s)
  fileNames<-list.files(inputPath)
  
  for(i in 1:length(fileNames)){
    print(paste("Working on agent ",i," out of ",length(fileNames)," with agent ID ",inputDataFrame[1,agentID],sep=""))
    
    # read in file i
    catchFile<-jsonlite::fromJSON(paste(inputPath,fileNames[i],sep=""))
    
    # remove the pointless distance column
    catchFile$distance<-NULL
    
    # add agent ID
    catchFile$agentID<-i
    
    # add individual coordinate columns (could make optional)
    catchFile$from_locx <- sapply(catchFile$from_loc, `[[`, 1)
    catchFile$from_locy <- sapply(catchFile$from_loc, `[[`, 2)
    
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
    
    # calculate duration (in seconds) as the difference between the current timestamp and the next timestamp
    catchFile<-catchFile[order(catchFile$ts),]
    catchFile$nextDateTime<-c(catchFile[2:(nrow(catchFile)),"dateTime"], catchFile[nrow(catchFile),"dateTime"]); catchFile$nextDateTime[nrow(catchFile)]<-NA
    catchFile$duration<-as.numeric(difftime(catchFile$nextDateTime,catchFile$dateTime,units="secs"))
    
    # remove all (apparently duplicate) entries where duration = 0
    catchFile<-catchFile[which(catchFile$duration>0),]
    
    # calculate the destination coordinates so can calculate the distances
    catchFile$to_locx<-c(catchFile[2:(nrow(catchFile)),"from_locx"], catchFile[nrow(catchFile),"from_locx"]); catchFile$to_locx[nrow(catchFile)]<-NA
    catchFile$to_locy<-c(catchFile[2:(nrow(catchFile)),"from_locy"], catchFile[nrow(catchFile),"from_locy"]); catchFile$to_locy[nrow(catchFile)]<-NA
    
    # (optional) select specified min/max date range
    if(!is.na(minDate)){
      catchFile<-catchFile[which(catchFile$d>=minDate),]
    }
    if(!is.na(maxDate)){
      catchFile<-catchFile[which(catchFile$d<=maxDate),]
    }
    
    
    
    
    
    
    # (optional) select agents based on whether they have measurements within certain (optionally preset) location bounds
    
    
    
    
    
    
    
    
    
    # calculate geodesic and haversine distances in meters from to-from coordinates
    catchFile$distGeo<-geosphere::distGeo(cbind(catchFile$from_locx, catchFile$from_locy), cbind(catchFile$to_locx, catchFile$to_locy))
    catchFile$distHaversine<-geosphere::distHaversine(cbind(catchFile$from_locx, catchFile$from_locy), cbind(catchFile$to_locx, catchFile$to_locy))
    
    # calculate the speeds (in km/h) based on the calculated distances and durations
    catchFile$speed<-3.6*catchFile$distGeo/as.numeric(catchFile$duration)
    
    # write out the results if saveOutput==TRUE
    if(saveOutput==TRUE){
      # convert the to_loc and from_loc lists to character fields to enable writing of the output file
      catchFile[,"to_loc"]<-paste("[",unlist(catchFile[,"to_loc"])[1],",",unlist(catchFile[,"to_loc"])[2],"]",sep="") 
      catchFile[,"from_loc"]<-paste("[",unlist(catchFile[,"from_loc"])[1],",",unlist(catchFile[,"from_loc"])[2],"]",sep="") 
      
      outFile<-paste(outputPath,"Base_Agent-",i,".csv",sep="")
      write.csv(catchFile,outFile,row.names=F)
    }
  } ; rm(i) # end of loop over all devices (i)
}