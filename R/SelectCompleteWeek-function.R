#' Select only agents with observations on each day of the week
#'
#' Removes agents which have weekdays without any observations.
#' Optionally can be used to select agents which have observations on the specified days of the week (or weekdays/weekends only) and specify a minimum total number of observations required.
#'
#' @param inputPath character; the path to the folder where the input *.csv files are located
#' @param weekdays character vector (optional); the desired days of the week each agent should have measurements for (e.g. c("Sunday","Wednesday","Thursday") or alternatively use "midweek" (Tuesday to Thursday), "week" (Monday to Friday), "weekend" (Saturday and Sunday) or NA to skip this step altogether; default will require measurements on every day of the week (Monday to Sunday)
#' @param strictSelection boolean; if TRUE will remove observations on other days than those specified, if FALSE (default) will select agents which have measurements on the days specified, but also maintain their observations on other days
#' @param minObsvsReq numeric (optional); minimum required number of observations for each agent to be retained; default is NA (no minimum required)
#' @param outputPath character (optional); the path to the folder to write the output csv files to, set to NA to not write output files; default is the inputPath
#' @param outputName character (optional); if outputPath is not NA, will prefix the output filenames ("AgentID.csv") with this; default is "WeekSel", set to NA to just have the AgentIDs
#'
#' @export

selectWeek<-function(inputPath="", weekdays=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"),
                     strictSelection=FALSE, minObsvsReq=NA, outputPath=inputPath, outputName="WeekSel"){

  # read in filename(s)
  fileNames<-list.files(inputPath)
  fileNames<-fileNames[grep("Base",fileNames)]

  for(i in 1:length(fileNames)){
    print(paste("Working on agent ",i," out of ",length(fileNames)," with agent ID ",inputDataFrame[1,agentID],sep=""))

    # read in file i
    inputDataFrame<-read.csv(paste(inputPath,fileNames[i],sep=""))

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


    # keep track of whether to remove the current agent or not and how many agents have been removed in total
    rmAgent<-FALSE; if(i==1){noOfAgentsRemoved<-0}

    # determine whether to exclude agent based on strict terms or just on activity present on the desired weekdays of interest
    for(j in 1:length(weekdays)){
      if(length(weekdays)>1||!is.na(weekdays)){
        if(length(which(inputDataFrame$weekDay==weekdays[j]))==0){
          rmAgent<-TRUE
        }
      }
    }; rm(j)

    if(rmAgent==FALSE){
      # if strict selection is TRUE, exclude the observations on the days which aren't listed in "weekdays"
      if(length(weekdays)>1||!is.na(weekdays)){
        if(strictSelection==TRUE){
          inputDataFrame<-inputDataFrame[which(inputDataFrame$weekDay%in%weekdays),]
        }
      }

      # (optional) remove agent if (after strict selection, if applicable) it has fewer entries than specified in minObsvsReq, if not NA
      if(!is.na(minObsvsReq)){
        if(nrow(inputDataFrame)<minObsvsReq){
          rmAgent<-TRUE
          noOfAgentsRemoved<-noOfAgentsRemoved+1
        }
      }
    } else {noOfAgentsRemoved<-noOfAgentsRemoved+1}

    if(rmAgent==FALSE&!is.na(outputPath)){
      if(!is.na(outputName)){
        outputFileName<-paste(outputPath,outputName,"_Agent-",inputDataFrame$agentID[1],".csv",sep="")
      } else{
        outputFileName<-paste(outputPath,"Agent-",inputDataFrame$agentID[1],".csv",sep="")
      }
      write.csv(inputDataFrame,outputFileName,row.names=F)
    }

  } # end of loop over agents
  print(paste("Removed ",noOfAgentsRemoved," agents because of the selection criteria specified.",sep=""))
} # end of function



