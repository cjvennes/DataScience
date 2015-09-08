##Plot 4
    #plot4 is a combination of 4 different charts in a two by two grid.
        #1. Global Active Power over Time (Plot2)
        #2. Voltage over DateTime
        #3. Sub_Metering usage by type over Time (Plot3)
        #4. Global Reactive Power over datetime

##Load the Data
    data <- read.table("./exdata-data-household_power_consumption/household_power_consumption.txt", sep=";", header=TRUE, na.strings="?")
    data$Date <- as.Date(data$Date, format = "%d/%m/%Y")
    PlotData <- subset(data, Date>=as.Date("2007-02-01") & Date <= as.Date("2007-02-02"))
    PlotData$DateTime <- as.POSIXct(paste(PlotData$Date, PlotData$Time), format="%Y-%m-%d %H:%M:%S")
    
##Create the chart
    png("plot4.png", width=480, height=480, units="px") #Save the graph as plot3
      par(mfrow=c(2,2)) #arrange in 2 by 2 grid
      #Chart 1
        plot(x=PlotData$DateTime, y=PlotData$Global_active_power, type ="l", xlab="", ylab="Global Active Power")
      #Chart 2
        plot(x=PlotData$DateTime, y=PlotData$Voltage, type ="l", xlab="datetime", ylab="Voltage")
      #Chart 3
        plot(x=PlotData$DateTime, y=PlotData$Sub_metering_1, type ="l", xlab="", ylab="Energy sub metering")
        lines(x=PlotData$DateTime, y=PlotData$Sub_metering_2, col="red")
        lines(x=PlotData$DateTime, y=PlotData$Sub_metering_3, col="blue")
        legend("topright", pch=1, col=c("black","red","blue"), legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
      #Chart 4
        plot(x=PlotData$DateTime, y=PlotData$Global_reactive_power, type ="l", xlab="datetime", ylab="Global_reactive_power")
    dev.off() #close the chart
    