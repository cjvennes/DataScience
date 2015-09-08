##Plot 1
    #plot1 is a histogram showing global active power of kilowatts and frequency the measurement occurs across the two day timeframe.

##Load the Data
    data <- read.table("./exdata-data-household_power_consumption/household_power_consumption.txt", sep=";", header=TRUE, na.strings="?")
    data$Date <- as.Date(data$Date, format = "%d/%m/%Y")
    PlotData <- subset(data, Date>=as.Date("2007-02-01") & Date <= as.Date("2007-02-02"))
    PlotData$DateTime <- as.POSIXct(paste(PlotData$Date, PlotData$Time), format="%Y-%m-%d %H:%M:%S")
    
##Create the chart
    png("plot1.png", width=480, height=480, units="px") #Save the graph as plot1
    hist(PlotData$Global_active_power, col = "Red", main="Global Active Power", xlab="Global Active Power (kilowatts)")
    dev.off() #close the chart
    