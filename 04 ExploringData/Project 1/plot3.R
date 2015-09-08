##Plot 3
    #plot3 is another line chart but with all three sub meterings displayed across the two day timeframe.

##Load the Data
    data <- read.table("./exdata-data-household_power_consumption/household_power_consumption.txt", sep=";", header=TRUE, na.strings="?")
    data$Date <- as.Date(data$Date, format = "%d/%m/%Y")
    PlotData <- subset(data, Date>=as.Date("2007-02-01") & Date <= as.Date("2007-02-02"))
    PlotData$DateTime <- as.POSIXct(paste(PlotData$Date, PlotData$Time), format="%Y-%m-%d %H:%M:%S")
    
##Create the chart
    png("plot3.png", width=480, height=480, units="px") #Save the graph as plot3
    plot(x=PlotData$DateTime, y=PlotData$Sub_metering_1, type ="l", xlab="", ylab="Energy sub metering")
    lines(x=PlotData$DateTime, y=PlotData$Sub_metering_2, col="red")
    lines(x=PlotData$DateTime, y=PlotData$Sub_metering_3, col="blue")
    legend("topright", pch=1, col=c("black","red","blue"), legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
    dev.off() #close the chart
    