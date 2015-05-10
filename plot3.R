plot3 <- function() {
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
  
  x <- data.frame(dateTime, myData$Sub_metering_1, myData$Sub_metering_2, myData$Sub_metering_3)
  
  jpeg(file = "plot3.jpg")
  with(x, plot(dateTime, myData.Sub_metering_1, pch=".", xlab="", ylab="Energy sub metering"))
  with(x, lines(dateTime, myData.Sub_metering_1, type="l"))
  with(x, lines(dateTime, myData.Sub_metering_2, type="l", col="Red"))
  with(x, lines(dateTime, myData.Sub_metering_3, type="l", col="Blue"))
  with(x, legend("topright", legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), pch="-", lwd=3, col=c("black","red","blue")))
  dev.off()
}