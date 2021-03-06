% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CleanData-function.R
\name{cleanData}
\alias{cleanData}
\title{Clean up unrealistic values}
\usage{
cleanData(inputPath, inputName = "WeekSel", thresholdSpeed = 240,
  methodSpecific = NA, unrealMerge = TRUE, removeMethodUnknown = FALSE,
  removeMethodNotFound = FALSE, outputPath = inputPath,
  outputName = "Cleaned")
}
\arguments{
\item{inputPath}{character; the path to the folder where the input *.csv files are located}

\item{inputName}{character (optional); specify a string of characters the filenames which should be used all contain (e.g. "Base"); default is "WeekSel"}

\item{thresholdSpeed}{numeric (vector if using 'methodSpecific'); specify the maximum speed(s) in km/h above which (optionally method-specific) speeds are deemed unrealistic; default is 240}

\item{methodSpecific}{character vector (optional); sequence of the method names ("car", "notfoundinindex", "pedestrian", "unknown", "bicycle", "publicTransport") in the order the maximum speeds have been provided above; default is NA (just one maximum value)}

\item{unrealMerge}{boolean; if TRUE, assumes GPS positioning inaccuracy is responsible and treats the entries preceding and following the 'unrealistic' entries as the actual leg of the journey and recalculates the distance, duration and speed accordingly; default is FALSE (just deletes the entries without (so there will be gaps in the data))}

\item{removeMethodUnknown}{boolean (optional); remove entries where the method is "unknown"; default is FALSE}

\item{removeMethodNotFound}{boolean (optional); remove entries where the method is "notfoundinindex"; default is FALSE}

\item{outputPath}{character (optional); the path to the folder to write the output csv files to, set to NA to not write output files; default is the inputPath}

\item{outputName}{character (optional); if outputPath is not NA, will prefix the output filenames ("AgentID.csv") with this; default is "Cleaned", set to NA to just have the AgentIDs}
}
\description{
Remove entries with unrealistic speeds (optionally for the method in question) and update the next measurement accordingly (with duration/destination data)
}

