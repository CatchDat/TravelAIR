#' Reclass methods (reduce)
#'
#' @param inputPath character; the path to the folder where the input *.csv files are located
#' @param inputName character (optional); specify a string of characters the filenames which should be used all contain; default is "Cleaned_Agent"
#' @param reclassIn character vector; list the method names to be reclassed; default is c("ShortWalk","TrainChange","NationalRail","InternationalRail","CarPetrolSmall")
#' @param reclassOut character vector; list of the method names reclass to; default is c("Walk","Train","Train","Train","Car")
#' @param outputPath character (optional); the path to the folder to write the output csv files to, set to NA to not write output files; default is the inputPath
#' @param outputName character (optional); if outputPath is not NA, will prefix the output filenames ("AgentID.csv") with this; default is "ReclassedMethods", set to NA to just have the AgentIDs
#'
#' @export


reclassMethods<-function(inputPath, inputName="Cleaned_Agent",
                         reclassIn=c("ShortWalk","TrainChange","NationalRail","InternationalRail","CarPetrolSmall"), reclassOut=c("Walk","Train","Train","Train","Car"),
                         outputPath=inputPath, outputName="ReclassedMethods"){

  # read in filename(s)
  fileNames<-list.files(inputPath)
  fileNames<-fileNames[grep(inputName,fileNames)]

  for(i in 1:length(fileNames)){
    # read in file i
    inputDataFrame<-read.csv(paste(inputPath,fileNames[i],sep=""),as.is=T)

    print(paste("Working on agent ",i," out of ",length(fileNames)," with agent ID ",inputDataFrame[1,"agentID"],sep=""))

    for(j in 1:length(reclassIn)){
      if(length(which(inputDataFrame$method_desc==reclassIn[j]))>0){
        inputDataFrame[which(inputDataFrame$method_desc==reclassIn[j]),]$method_desc<-rep(reclassOut[j],length(which(inputDataFrame$method_desc==reclassIn[j])))
      }
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
}
