writeToFile <- function(fileName, dataframe) {
  newdataframe <- data.frame(matrix(nrow=nrow(dataframe),ncol=0))
  # This can't actually be the best way to do this...
  for (i in 1:nrow(dataframe)) {
    newdataframe$status[i] <- dataframe$status[[i]]
    newdataframe$error[i] <- dataframe$error[[i]]
    newdataframe$Findex[i] <- dataframe$Findex[[i]]
    newdataframe$MEindex[i] <- dataframe$MEindex[[i]]
    newdataframe$Tindex[i] <- dataframe$Tindex[[i]]
    newdataframe$data.postPosition[i] <- dataframe$data.postPosition[[i]]
    newdataframe$data.shape[i] <- dataframe$data.shape[[i]]
    newdataframe$data.eta[i] <- dataframe$data.eta[[i]]
    newdataframe$data.marketElasticity[i] <- dataframe$data.marketElasticity[[i]]
    newdataframe$data.efficiencies[i] <- toString(dataframe$data.efficiencies[[i]])
    newdataframe$data.PoDposition[i] <- dataframe$data.PoDposition[[i]]
  }
  newdataframe$iteration <- as.integer(rownames(newdataframe))
  write.csv(newdataframe, file=fileName, row.names=FALSE)
}