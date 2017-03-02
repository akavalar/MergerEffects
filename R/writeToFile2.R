writeToFile <- function(fileName, dataframe) {
  newdataframe <- dataframe
  newdataframe$data.efficiencies <- Map(toString,newdataframe$data.efficiencies)
  newdataframe <- newdataframe[,c("status","error","Findex","MEindex","Tindex",
                               "data.postPosition","data.shape","data.eta",
                               "data.marketElasticity","data.efficiencies",
                               "data.PoDposition")]
  newdataframe$iteration <- as.integer(rownames(newdataframe))
  write.csv(newdataframe, file=fileName, row.names=FALSE)
}