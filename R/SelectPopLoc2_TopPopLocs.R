#' Select popular locations for each of the agents - part 2: Extract 'home' and 'work' locations
#'
#' Extracts the 'home' and 'work' locations from the buffers created in part 1.
#'
#'  ----- NOTE! ----- requires rgeos library, which doesn't work under 3.3.1 (Sept 2016), so need to roll back to 3.2.1 after running part 1.

#
#' @param inputPath character; the path to the folder where the input *.kml buffer and *.csv observation location files are located
#' @param bufferInputName character (optional); select only *.kml buffer file names which contain the string specified; default is "Buffer"
#' @param pointsInputName character (optional); select only *.csv observation location file names which contain the string specified; default is "Cleaned_Agent"
#' @param popLocs character vector (optional); select the popular locations the buffers have been created for/saved as in part 1; default is c("home","work")
#' @param noOfPopLocs numeric vector (optional); calculate the specified number of popular locations for each popular location as specified in 'popLocs'; default is c(1,3)
#' @param minReqObsvs numeric (optional); specify the minimum number of observations required within the buffer in order for the location to qualify as a popular location; default is 0
#' @param outputPath character (optional); the path to the folder to write the output spatial point files to, set to NA to not write output files; default is the inputPath
#' @param outputName character (optional); if outputPath is not NA, will prefix the output filenames ("Agent-ID#.extension") with this; default is "PopLoc", set to NA to just have the AgentIDs
#'
#' @export

selectTopPopLocs<-function(inputPath, bufferInputName="Buffers", pointsInputName="ReclassedMethods_Agent",
                           popLocs=c("home","work"), noOfPopLocs=c(1,3), minReqObsvs=0, outputPath=inputPath, outputName="Multiple-PopLocs"){

  fileNames<-list.files(inputPath)
  bufferFileNames<-fileNames[grep(bufferInputName,fileNames)]
  pointsFileNames<-fileNames[grep(pointsInputName,fileNames)]

  # --- replace this bit with the reading in of the agent numbers from file later on ---
  for(j in 1:length(popLocs)){

    popLocFiles<-bufferFileNames[grep(paste(bufferInputName,"-",popLocs[j],"-","Polygons",sep=""),bufferFileNames)]
    allAgentIDs<-rep(NA,length(popLocFiles))

    for(k in 1: length(popLocFiles)){
      allAgentIDs[k]<-as.numeric(substr(popLocFiles[k],stringr::str_locate_all(popLocFiles[k],"[0-9]+")[[1]][1,"start"],stringr::str_locate_all(popLocFiles[k],"[0-9]+")[[1]][1,"end"]))
    }

    # create a table to store all the results in
    nrows<-length(allAgentIDs)*noOfPopLocs[j]
    popLoc<-rep(NA,nrows); popLocId<-rep(NA,nrows); popLocX<-rep(NA,nrows); popLocY<-rep(NA,nrows); noOfObsvs<-rep(NA,nrows); totDur<-rep(NA,nrows); relDur<-rep(NA,nrows)
    agentStats<-as.data.frame(cbind(popLoc,popLocId,popLocX,popLocY,noOfObsvs, totDur, relDur)); rm(popLoc, popLocX, popLocY, noOfObsvs, totDur, relDur)
    agentStats$agentID<-rep(NA,nrow(agentStats));agentStats$popLoc<-popLocs[j]


    for(i in 1:length(allAgentIDs)){

      agentFiles<-popLocFiles[grep(paste("-",allAgentIDs[i],"_",sep=""),popLocFiles)]

      # read in the original data points in order to be able to see which fall within the pop loc buffers
      inputDataFrame<-read.csv(paste(inputPath,pointsFileNames[grep(paste("-",allAgentIDs[i],".csv",sep=""),pointsFileNames)],sep=""),as.is=T)
      overallTotDur<-sum(inputDataFrame$duration)
      agentID<-inputDataFrame[1,"agentID"]

      print(paste("Working on agent ",i," out of ",length(allAgentIDs)," with agent ID ",agentID,", which has ",nrow(inputDataFrame), " measurements.",sep=""))


      # loop over the number of pop locs to be calculated
      for(n in 1:noOfPopLocs[j]){
        if(nrow(inputDataFrame)>1){
          # keep track of the buffer containing the most observations across the different file "parts"
          maxPtsInBuff<-0
          maxPtsBuffID<-0


          # add the appropriate loops for calculating these top x locations
          # and make sure to remove them properly before calculating the next (think about how best to do this) and then also make sure that it will only run while there are still points remaining (so it
          # won't crash if you are running it for 10 popular locations on an agent that only has 5 observations) -> should somehow do the removing with the overlapping buffer stuff, though guess that would
          # get rid of people going to the sandwich shop next door during lunch or so but hey, whatever






          # okay, I guess the best approach would be to remove all the points that fall in the buffer with the most points from the points dataset, then count again how many points there are in each buffer?
          # not too sure about the second step, might be a better approach perhaps

          for(l in 1:length(agentFiles)){
            # work on pop loc buffer file part l of agent i
            writeOutResults<-FALSE

            # the kml file layer names are the input path + filename - extension where all non alphanumeric characters have been replaced with an underscore
            inName<-paste(inputPath,substr(agentFiles[l],1,nchar(agentFiles[l])-4),sep="")
            subPositions<-stringr::invert_match(stringr::str_locate_all(inName,"[A-Z0-9a-z]+")[[1]])
            for(m in 1:nrow(subPositions)){
              substr(inName,subPositions[m,"start"],subPositions[m,"end"])<-"_"
            }

            # read in the pop loc buffer kml file as spatial polygons data frame
            buffSPDF<-rgdal::readOGR(dsn=paste(inputPath,agentFiles[l],sep=""),layer=inName)

            # ---> possibly add saving the spatial points dataframe from part 1 as kml file to part 1, so don't need to repeat here


            # retrieve the coordinates for the buffer polygons and use these to determine the number of points falling within them
            for(o in 1:nrow(buffSPDF)){
              tempBuffSPCoords<-buffSPDF@polygons[[o]]@Polygons[[1]]@coords
              ptsInBuff<-sum(sp::point.in.polygon(inputDataFrame$from_locx,inputDataFrame$from_locy,tempBuffSPCoords[,1],tempBuffSPCoords[,2]))

              # build in the bit where there's a minumum number of observations required in order to qualify
              #         (and what to do if no points qualify)

              if(ptsInBuff>maxPtsInBuff){
                maxPtsInBuff<-ptsInBuff
                maxPtsBuffID<-o
                ptsInBuffIDs<-sp::point.in.polygon(inputDataFrame$from_locx,inputDataFrame$from_locy,tempBuffSPCoords[,1],tempBuffSPCoords[,2])
                totDur<-sum(inputDataFrame[which(ptsInBuffIDs==TRUE),]$duration)
                writeOutResults<-TRUE
              }
            }

            # write out the results to the agents' table if they have changed
            if(writeOutResults==TRUE){
              agentStats[(((i-1)*noOfPopLocs[j])+n),]$popLoc<-popLocs[j]
              agentStats[(((i-1)*noOfPopLocs[j])+n),]$popLocId<-n
              agentStats[(((i-1)*noOfPopLocs[j])+n),]$noOfObsvs<-maxPtsInBuff
              tempBuffSPCoords<-buffSPDF@polygons[[maxPtsBuffID]]@Polygons[[1]]@coords
              agentStats[(((i-1)*noOfPopLocs[j])+n),]$popLocX<-mean(tempBuffSPCoords[(2:nrow(tempBuffSPCoords)),1])
              agentStats[(((i-1)*noOfPopLocs[j])+n),]$popLocY<-mean(tempBuffSPCoords[(2:nrow(tempBuffSPCoords)),2])
              agentStats[(((i-1)*noOfPopLocs[j])+n),]$totDur<-totDur
              agentStats[(((i-1)*noOfPopLocs[j])+n),]$relDur<-totDur/overallTotDur
              agentStats[((((i-1)*noOfPopLocs[j])+1):(i*noOfPopLocs[j])),]$agentID<-agentID
            }


            # generate some duration stats etc too for the points that fall within this buffer (how much time is spent there, on what days, etc)









          } # end of for loop for processing multiple buffer file parts for one agent ("*-Part-2.kml" etc)

          # remove the pop loc ID==n points from the inputdataframe for the enxt pop loc ID iteration
          if(length(which(ptsInBuffIDs==TRUE))>0){
            inputDataFrame<-inputDataFrame[-which(ptsInBuffIDs==TRUE),]
          }
        } # end of if inputDataFrame has more than 1 observation
      } # end of loop over number of pop locs to calculate
    } # end of loop over agents



    # save the agents' output pop loc points as kml file


    if(!is.na(outputPath)){
      if(!is.na(outputName)){
        outputFileName<-paste(outputPath,outputName,"-",popLocs[j],".csv",sep="")
      } else{
        outputFileName<-paste(outputPath,outputName,"-",popLocs[j],".csv",sep="")
      }
      write.csv(unique(agentStats),outputFileName,row.names=F)
    }
  } # end of loop over PopLocs
}


