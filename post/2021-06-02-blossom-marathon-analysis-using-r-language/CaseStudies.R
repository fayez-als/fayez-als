




findLocs = function(spaceRow) {
  
  rowLength = nchar(spaceRow)
  searchLocs <- c(0,gregexpr(" ",spaceRow)[[1]])
  
  
  if (substring(spaceRow, rowLength, rowLength) != " ")
   return(c(searchLocs,rowLength +1))
  else return(searchLocs)
    
}





MakeColumns <- function(rowData,year){
  
  indexEq <-grep('^==',rowData)
  headerRow <- rowData[indexEq-1]
  body <- rowData[indexEq+1:length(rowData)]
  spaceRow <- rowData[indexEq]
  headerRow <- tolower(headerRow)
  searchLocs <- findLocs(spaceRow)
  df <- mapply(substr,list(body),start=searchLocs[-length(searchLocs)]+1,
        stop = searchLocs[-1]-1)
  colnames(df)<-mapply(substr,list(headerRow),start=searchLocs[-length(searchLocs)]+1,
                       stop = searchLocs[-1]-1)
  
 

  df <- as.data.frame(df)
  colnames(df) <- gsub(" ","",colnames(df))
  colnames(df) <- gsub("guntim","time",colnames(df))
  colnames(df) <- gsub("gunti","time",colnames(df))
  colnames(df) <- gsub("gun","time",colnames(df))
  colnames(df) <- gsub("^a$","ag",colnames(df))
  
  df <- select(df,c('place','ag','name','hometown','time'))
  
  
  
 

  
  return(df)
}




### Applying MakeColumns to All males text files except for 2006


mfilenames = paste("MenTxt/", c(1999:2005,2007:2012), ".txt", sep = "")

menFiles = lapply(mfilenames, readLines)

menFrames<- lapply(menFiles,MakeColumns)



### Adding the corresponding years

menFrames[[1]]$year <-1999
menFrames[[2]]$year <-2000
menFrames[[3]]$year <-2001
menFrames[[4]]$year <-2002
menFrames[[5]]$year <-2003
menFrames[[6]]$year <-2004
menFrames[[7]]$year <-2005
menFrames[[8]]$year <-2007
menFrames[[9]]$year <-2008
menFrames[[10]]$year <-2009
menFrames[[11]]$year <-2010
menFrames[[12]]$year <-2011
menFrames[[13]]$year <-2012


fullData <- menFrames[[1]]


###Binding all the data Together (Can be done easily with loops)

fullData <- rbind(fullData,menFrames[[2]])
fullData <- rbind(fullData,menFrames[[3]])
fullData <- rbind(fullData,menFrames[[4]])
fullData <- rbind(fullData,menFrames[[5]])
fullData <- rbind(fullData,menFrames[[6]])
fullData <- rbind(fullData,menFrames[[7]])
fullData <- rbind(fullData,menFrames[[8]])
fullData <- rbind(fullData,menFrames[[9]])
fullData <- rbind(fullData,menFrames[[10]])
fullData <- rbind(fullData,menFrames[[11]])
fullData <- rbind(fullData,menFrames[[12]])
fullData <- rbind(fullData,menFrames[[13]])






#### A function for fixing 2006 text file (same as Make columns with minor tweeks)

Fix2006 <- function(rowData){
  
  indexEq <-grep('^==',rowData)
  headerRow <- rowData[indexEq-1]
  body <- rowData[indexEq+1:length(rowData)]
  spaceRow <- rowData[indexEq]
  headerRow <- tolower(headerRow)
  searchLocs <- findLocs(spaceRow)
  df <- mapply(substr,list(body),start=searchLocs[-length(searchLocs)]+1,
               stop = searchLocs[-1]-1)
  colnames(df)<-mapply(substr,list(headerRow),start=searchLocs[-length(searchLocs)]+1,
                       stop = searchLocs[-1]-1)
  
  
  
  df <- as.data.frame(df)
  colnames(df) <- gsub(" ","",colnames(df))
  colnames(df) <- gsub("guntim","time",colnames(df))
  colnames(df) <- gsub("gunti","time",colnames(df))
  colnames(df) <- gsub("gun","time",colnames(df))
  colnames(df) <- gsub("^a$","ag",colnames(df))
  
  return(df)
}





### Fixing 2006
df2006 <- readLines('./MenTxt/2006.txt')

df2006 <- Fix2006(df2006)
df2006$year <-2006

df2006 <- separate(df2006,hometownnettim,into=c('hometown','nettim'),sep="  ")

df2006 <- select(df2006,c('place','ag','name','hometown','time','year'))

fullData <- rbind(fullData,df2006)


fullData$gender <- 'Male'




####################################



### Here we have the same approach for female text files

## 2001 file has issues, 

#2006 file has the same issue is males 2006(we can apply the same previous function)


ffilenames = paste("WomenTxt/", c(1999:2000,2002:2005,2007:2012), ".txt", sep = "")

WFiles = lapply(ffilenames, readLines)


WFrames <- lapply(WFiles,MakeColumns)
WFrames[[1]]$year <-1999
WFrames[[2]]$year <-2000
WFrames[[3]]$year <-2002
WFrames[[4]]$year <-2003
WFrames[[5]]$year <-2004
WFrames[[6]]$year <-2005
WFrames[[7]]$year <-2007
WFrames[[8]]$year <-2008
WFrames[[9]]$year <-2009
WFrames[[10]]$year <-2010
WFrames[[11]]$year <-2011
WFrames[[12]]$year <-2012




fullWData <- WFrames[[1]]
fullWData <- rbind(fullWData,WFrames[[2]])
fullWData <- rbind(fullWData,WFrames[[3]])
fullWData <- rbind(fullWData,WFrames[[4]])
fullWData <- rbind(fullWData,WFrames[[5]])
fullWData <- rbind(fullWData,WFrames[[6]])
fullWData <- rbind(fullWData,WFrames[[7]])
fullWData <- rbind(fullWData,WFrames[[8]])
fullWData <- rbind(fullWData,WFrames[[9]])
fullWData <- rbind(fullWData,WFrames[[10]])
fullWData <- rbind(fullWData,WFrames[[11]])
fullWData <- rbind(fullWData,WFrames[[12]])



## function for fixing 2001 file

Fix2001 <- function(rowData){
  indexEq <-grep('^==',rowData)
  headerRow <- rowData[indexEq-1]
  body <- rowData[indexEq+1:length(rowData)]
  spaceRow <- rowData[indexEq]
  headerRow <- tolower(headerRow)
  searchLocs <- findLocs(spaceRow)
  df <- mapply(substr,list(body),start=searchLocs[-length(searchLocs)]+1,
               stop = searchLocs[-1]-1)
  colnames(df)<-mapply(substr,list(headerRow),start=searchLocs[-length(searchLocs)]+1,
                       stop = searchLocs[-1]-1)
  
  
  
  df <- as.data.frame(df)
  colnames(df) <- gsub(" ","",colnames(df))
  colnames(df) <- gsub("guntim","time",colnames(df))
  colnames(df) <- gsub("gunti","time",colnames(df))
  colnames(df) <- gsub("gun","time",colnames(df))
  colnames(df) <- gsub("^a$","ag",colnames(df))
  return(df)
}



### Here we apply the function

df2001<- readLines('./WomenTxt/2001.txt')
df2001 <- Fix2001(df2001)
df2001 <- select(df2001,c('ag','name','hometown','time'))
df2001$year <- 2001


# turns out place columns is missing, we will create NA column

df2001$place <- NA
fullWData<-rbind(fullWData,df2001)



#############################################



# Fixing the 2006 file (using fix2006 function)

df2006 <- readLines('./WomenTxt/2006.txt')
df2006 <- Fix2006(df2006)
df2006$year <-2006
df2006 <- separate(df2006,hometownnettim,into=c('hometown','nettim'),sep="  ")
df2006 <- select(df2006,c('place','ag','name','hometown','time','year'))
fullWData <- rbind(fullWData,df2006)





# Last step for combining the two gender frames together

fullWData$gender <- 'Female'
Df <- rbind(fullData,fullWData)

colnames(Df) <- gsub("ag","age",colnames(df))

write.csv(Df,"BlossomDF.csv", row.names = FALSE)







#########################################################
