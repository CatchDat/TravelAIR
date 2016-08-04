#' Remove entries with unrealistic speeds (optionally for the method in question) and update the next measurement accordingly (with duration/destination data)
#'
#'
#'
#'
#' @param inputPath character; the path to the folder where the input *.csv files are located
#' @param inputName character (optional); specify a string of characters the filenames which should be used all contain (e.g. "Base"); default is "WeekSel"
#' @param thresholdSpeed numeric (vector if using 'methodSpecific'); specify the maximum speed(s) in km/h above which (optionally method-specific) speeds are deemed unrealistic; default is 240
#' @param methodSpecific character vector (optional); sequence of the method names ("car", "notfoundinindex", "pedestrian", "unknown", "bicycle", "publicTransport") in the order the maximum speeds have been provided above; default is NA (just one maximum value)
#' @param unrealMerge boolean; if TRUE, assumes GPS positioning inaccuracy is responsible and treats the entries preceding and following the 'unrealistic' entries as the actual leg of the journey and recalculates the distance, duration and speed accordingly; default is FALSE (just deletes the entries without (so there will be gaps in the data))
#' @param removeMethodUnknown boolean (optional); remove entries where the method is "unknown"; default is FALSE
#' @param removeMethodNotFound boolean (optional); remove entries where the method is "notfoundinindex"; default is FALSE
#' @param outputPath character (optional); the path to the folder to write the output csv files to, set to NA to not write output files; default is the inputPath
#' @param outputName character (optional); if outputPath is not NA, will prefix the output filenames ("AgentID.csv") with this; default is "WeekSel", set to NA to just have the AgentIDs
#'
#' @export
#'

cleanData<-function(inputPath="/Users/Nikee/Documents/Work/Projects/CatchTravelAI/Data/NewAgents/BasicData/", inputName="WeekSel", thresholdSpeed=240, methodSpecific=NA, unrealMerge=TRUE, removeMethodUnknown=FALSE, removeMethodNotFound=FALSE, outputPath=inputPath, outputName="Cleaned"){

  # read in filename(s)
  fileNames<-list.files(inputPath)
  fileNames<-fileNames[grep(inputName,fileNames)]

  # create an output file to store the overall stats in
  agentID<-rep(NA,length(fileNames)); removed<-rep(NA,length(fileNames)); original<-rep(NA,length(fileNames)); percentRemoved<-rep(NA,length(fileNames))
  overallStats<-as.data.frame(cbind(agentID,removed,original,percentRemoved)); rm(agentID,removed, original,percentRemoved)

  for(i in 1:length(fileNames)){
    # read in file i
    inputDataFrame<-read.csv(paste(inputPath,fileNames[i],sep=""))
    overallStats[i,]$agentID<-inputDataFrame[1,"agentID"]

    print(paste("Working on agent ",i," out of ",length(fileNames)," with agent ID ",inputDataFrame[1,"agentID"],sep=""))

    # keep track of how many entries are removed because of unrealistic values
    unrealEntries<-length(which(inputDataFrame$speed>thresholdSpeed)); overallStats[i,]$removed<-unrealEntries
    originalEntries<-nrow(inputDataFrame); overallStats[i,]$original<-originalEntries
    overallStats[i,]$percentRemoved<-100*unrealEntries/originalEntries

    if(unrealMerge==FALSE){
      if(is.na(methodSpecific)){
        inputDataFrame<-inputDataFrame[-which(inputDataFrame$speed>thresholdSpeed),]
      } else {
        for(j in 1:length(methodSpecific)){
          inputDataFrame<-inputDataFrame[-which(inputDataFrame$speed>thresholdSpeed[j]&inputDataFrame$method_desc==methodSpecific[j]),]
        }; rm(j)
      }
    } else {
      # add a column for 'flagging' the unrealistic values and their unique IDs, plus keep track of whether merging has actually succeeded in removing the entries
      inputDataFrame$unrealistic<-rep(FALSE,nrow(inputDataFrame))
      inputDataFrame$unrealID<-rep(NA,nrow(inputDataFrame))
      testSuccess<-NA

      if(!is.na(methodSpecific)){
        # do the method-specific speed stuff
        for(j in 1:length(methodSpecific)){
          inputDataFrame[which(inputDataFrame$speed>thresholdSpeed[j]&inputDataFrame$method_desc==methodSpecific[j]),]$unrealistic<-rep(TRUE,length(which(inputDataFrame$speed>thresholdSpeed[j]&inputDataFrame$method_desc==methodSpecific[j])))
          testSuccess[j]<-length(which(inputDataFrame$speed>thresholdSpeed[j]&inputDataFrame$method_desc==methodSpecific[j]))
        }; rm(j)
      } else {
        testSuccess<-length(which(inputDataFrame$speed>thresholdSpeed))
        # need to use rep in case there's 0 'unrealistic' speeds, like for agent ID 201
        inputDataFrame[which(inputDataFrame$speed>thresholdSpeed),]$unrealistic<-rep(TRUE,length(which(inputDataFrame$speed>thresholdSpeed)))
      }

      # add a column assigning a unique ID to each 'unrealistic' stint
      inputDataFrame[which(inputDataFrame$unrealistic==TRUE),]$unrealID<-cumsum(c(1,(diff(inputDataFrame$unrealistic)>0)))[which(inputDataFrame$unrealistic==TRUE)]

      # loop over the unique 'unrealistic' IDs, recalculate all the required parameters, then remove all unrealistic entries; start is 'which.min(unrealID==k)-1', end is 'which.max(unrealID==k)-1'
      # need to remove NA from the unique IDs first though
      uniqueUnrealIDs<-unique(inputDataFrame$unrealID); uniqueUnrealIDs<-uniqueUnrealIDs[-which(is.na(uniqueUnrealIDs))]
      if(length(uniqueUnrealIDs)>0){
        for(k in 1:length(uniqueUnrealIDs)){
          if(k == 1 & (which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])-1)>1){
            if(k == length(uniqueUnrealIDs) & (which.max(inputDataFrame$unrealID==uniqueUnrealIDs[k])+1) < nrow(inputDataFrame)){
              inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])-1),]$to_locx<-inputDataFrame[(which.max(inputDataFrame$unrealID==uniqueUnrealIDs[k])+1),]$from_locx
              inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])-1),]$to_locy<-inputDataFrame[(which.max(inputDataFrame$unrealID==uniqueUnrealIDs[k])+1),]$from_locy
              inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])-1),]$duration<--as.numeric(difftime(inputDataFrame[(which.max(inputDataFrame$unrealID==uniqueUnrealIDs[k])+1),]$dateTime,inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])-1),]$dateTime,units="secs"))
              inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])-1),]$distGeo<-geosphere::distGeo(cbind(inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])-1),]$from_locx, inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])-1),]$from_locy), cbind(inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])-1),]$to_locx, inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])-1),]$to_locy))
              inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])-1),]$distHaversine<-geosphere::distHaversine(cbind(inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])-1),]$from_locx, inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])-1),]$from_locy), cbind(inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])-1),]$to_locx, inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])-1),]$to_locy))
              inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])-1),]$speed<-3.6*inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])-1),]$distGeo/as.numeric(inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])-1),]$duration)
              inputDataFrame<-inputDataFrame[-which(inputDataFrame$unrealID==uniqueUnrealIDs[k]),]
            }
            else {
              inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])-1),]$to_locx<-inputDataFrame[(which.max(inputDataFrame$unrealID==uniqueUnrealIDs[k])),]$from_locx
              inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])-1),]$to_locy<-inputDataFrame[(which.max(inputDataFrame$unrealID==uniqueUnrealIDs[k])),]$from_locy
              inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])-1),]$duration<--as.numeric(difftime(inputDataFrame[(which.max(inputDataFrame$unrealID==uniqueUnrealIDs[k])),]$dateTime,inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])-1),]$dateTime,units="secs"))
              inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])-1),]$distGeo<-geosphere::distGeo(cbind(inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])-1),]$from_locx, inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])-1),]$from_locy), cbind(inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])-1),]$to_locx, inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])-1),]$to_locy))
              inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])-1),]$distHaversine<-geosphere::distHaversine(cbind(inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])-1),]$from_locx, inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])-1),]$from_locy), cbind(inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])-1),]$to_locx, inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])-1),]$to_locy))
              inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])-1),]$speed<-3.6*inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])-1),]$distGeo/as.numeric(inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])-1),]$duration)
              inputDataFrame<-inputDataFrame[-which(inputDataFrame$unrealID==uniqueUnrealIDs[k]),]
            }
          } else {
            inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])),]$to_locx<-inputDataFrame[(which.max(inputDataFrame$unrealID==uniqueUnrealIDs[k])+1),]$from_locx
            inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])),]$to_locy<-inputDataFrame[(which.max(inputDataFrame$unrealID==uniqueUnrealIDs[k])+1),]$from_locy
            inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])),]$duration<--as.numeric(difftime(inputDataFrame[(which.max(inputDataFrame$unrealID==uniqueUnrealIDs[k])+1),]$dateTime,inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])),]$dateTime,units="secs"))
            inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])),]$distGeo<-geosphere::distGeo(cbind(inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])),]$from_locx, inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])),]$from_locy), cbind(inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])),]$to_locx, inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])),]$to_locy))
            inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])),]$distHaversine<-geosphere::distHaversine(cbind(inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])),]$from_locx, inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])),]$from_locy), cbind(inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])),]$to_locx, inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])),]$to_locy))
            inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])),]$speed<-3.6*inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])),]$distGeo/as.numeric(inputDataFrame[(which.min(inputDataFrame$unrealID==uniqueUnrealIDs[k])),]$duration)
            inputDataFrame<-inputDataFrame[-which(inputDataFrame$unrealID==uniqueUnrealIDs[k]),]
          }
        }
      }

      if(!is.na(methodSpecific)){
        for(j in 1:length(methodSpecific)){
          testSuccess[j]<-length(which(inputDataFrame$speed>thresholdSpeed[j]&inputDataFrame$method_desc==methodSpecific[j]))
        }; rm(j)
      } else {
        testSuccess<-length(which(inputDataFrame$speed>thresholdSpeed))
      }

      # check if the results are still unrealistic; if so, recommend to run with unrealMerge = FALSE
      if(max(testSuccess)==0){
        print("Great, merging the unrealistic entries succeeded in removing all unrealisitc speeds, or there were none to remove.")
      } else{
        print(paste("Sorry, merging the unrealistic entries hasn't been successful in removing all unrealistic speeds. ",testSuccess," Entries with unrealistic speeds were created during the merging. Consider rerunning with 'unrealMerge=FALSE' to remove these entries altogether",sep=""))
      }
    }

    # print out how many entries were removed/merged because of the unrealistic speed(s) specified
    print(paste("Removed ",unrealEntries," out of ",originalEntries, ", which is ",(round(100*(unrealEntries/originalEntries),1)),"%",sep=""))

    # remove entries where the method is "unknown"
    if(removeMethodUnknown==TRUE&length(which(inputDataFrame$method_desc=="unknown"))){
      print(paste("Removed ",length(which(inputDataFrame$method_desc=="unknown"))," entries with method description 'unknown'.",sep=""))
      inputDataFrame<-inputDataFrame[-which(inputDataFrame$method_desc=="unknown"),]
    }

    # remove entries where the method is "notfoundinindex"
    if(removeMethodUnknown==TRUE&length(which(inputDataFrame$method_desc=="notfoundinindex"))){
      print(paste("Removed ",length(which(inputDataFrame$method_desc=="notfoundinindex"))," entries with method description 'notfoundinindex'.",sep=""))
      inputDataFrame<-inputDataFrame[-which(inputDataFrame$method_desc=="notfoundinindex"),]
    }

    # write out the results to the specified output path with the specified name, if applicable
    if(!is.na(outputPath)){
      if(!is.na(outputName)){
        outputFileName<-paste(outputPath,outputName,"_Agent-",inputDataFrame$agentID[1],".csv",sep="")
      } else{
        outputFileName<-paste(outputPath,"Agent-",inputDataFrame$agentID[1],".csv",sep="")
      }
      write.csv(inputDataFrame,outputFileName,row.names=F)
    }
  }
  write.csv(overallStats,"/Users/Nikee/Documents/Work/Projects/CatchTravelAI/Data/NewAgents/OverallStatsWeekSelMergeTrue.csv",row.names=F)
}

