plot4 <- function() {
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
  
  graph1 <- data.frame(dateTime, myData$Global_active_power)
  graph2 <- data.frame(dateTime, myData$Voltage)
  graph3 <- data.frame(dateTime, myData$Sub_metering_1, myData$Sub_metering_2, myData$Sub_metering_3)
  graph4 <- data.frame(dateTime, myData$Global_reactive_power)
  
  jpeg(file = "plot4.jpg")
  par(mfrow = c(2, 2))
  
  with(graph1, plot(dateTime, myData.Global_active_power, pch=".", xlab="", ylab="Global Active Power"))
  with(graph1, lines(dateTime, myData.Global_active_power, type="l"))
  
  with(graph2, plot(dateTime, myData.Voltage, pch=".", ylab="Voltage"))
  with(graph2, lines(dateTime, myData.Voltage, type="l"))
  
  with(graph3, plot(dateTime, myData.Sub_metering_1, pch=".", xlab="", ylab="Energy sub metering"))
  with(graph3, lines(dateTime, myData.Sub_metering_1, type="l"))
  with(graph3, lines(dateTime, myData.Sub_metering_2, type="l", col="Red"))
  with(graph3, lines(dateTime, myData.Sub_metering_3, type="l", col="Blue"))
  with(graph3, legend("topright", legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), pch="-", lwd=3, col=c("black","red","blue")))
  
  with(graph4, plot(dateTime, myData.Global_reactive_power, pch=".", ylab="Global Reactive Power"))
  with(graph4, lines(dateTime, myData.Global_reactive_power, type="l"))
  
  dev.off()
}