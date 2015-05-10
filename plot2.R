plot2 <- function() {
  myData <- read.table("household_power_consumption.txt", header=T, sep=";", stringsAsFactors=FALSE, na.strings = "?")
  myData <- na.omit(myData)
  myData <- subset(myData, Date == "1/2/2007" | Date == "2/2/2007")
  myData[, 3:9] <- sapply(myData[, 3:9], as.numeric)
  
  dateTime <- vector()
  
  for(i in 1:nrow(myData)) {
    dateTime[i] <- paste(myData[i, 1], myData[i, 2], sep=" ")
  }
  
  dateTime <- data.frame(dateTime, stringsAsFactors = F)
  
  dateTime <- lapply(dateTime, function(x) strptime(x, "%d/%m/%Y %H:%M:%S"))
  
  x <- data.frame(dateTime, myData$Global_active_power)
  
  jpeg(file = "plot2.jpg")
  with(x, plot(dateTime, myData.Global_active_power, pch=".", xlab="", ylab="Global Active Power (kilowatts)"))
  with(x, lines(dateTime, myData.Global_active_power, type="l"))
  dev.off()
}