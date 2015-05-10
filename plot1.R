plot1 <- function() {
  myData <- read.table("household_power_consumption.txt", header=T, sep=";", stringsAsFactors=FALSE, na.strings = "?")
  myData <- na.omit(myData)
  myData <- subset(myData, Date == "1/2/2007" | Date == "2/2/2007")
  myData[, 3:9] <- sapply(myData[, 3:9], as.numeric)
  
  jpeg(file = "plot1.jpg")
  with(myData, hist(Global_active_power, col="Red", xlab="Global Active Power (kilowatts)", ylab="Frequency", main="Global Active Power"))
  dev.off()
}