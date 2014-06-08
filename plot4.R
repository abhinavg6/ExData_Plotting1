plot4 <- function() {
    
    # Read the data file into a data table
    hhpowerDT <- fread("household_power_consumption.txt", na.strings="?")
    # Remove the NAs from the data table
    hhpowerDTwoNA <- na.omit(hhpowerDT)
    # Get the data subset we're interested in
    hhpowerDTwoNASub <- subset(hhpowerDTwoNA, Date=="1/2/2007" | 
                                   Date=="2/2/2007")
    # Combine Date and Time to create a new DateTime column
    hhpowerDTwoNASub[, DateTime := paste(Date, Time)]
    
    # Create the area for 2x2 plots
    par(mfrow = c(2,2), mar = c(5.1, 4.1, 4.1, 2.1))
    
    # Create the 1x1 plot
    par(cex = 0.5)
    with(hhpowerDTwoNASub, plot(strptime(DateTime, format="%d/%m/%Y %H:%M:%S"), 
                                Global_active_power, 
                                type="n",
                                ylab="Global Active Power (kilowatts)",
                                xlab=""))
    # Add line to the 1x1 plot
    with(hhpowerDTwoNASub, lines(strptime(DateTime, format="%d/%m/%Y %H:%M:%S"),
                                 Global_active_power))
    
    # Create the 1x2 plot
    par(cex = 0.5)
    with(hhpowerDTwoNASub, plot(strptime(DateTime, format="%d/%m/%Y %H:%M:%S"), 
                                Voltage, 
                                type="n",
                                ylab="Voltage",
                                xlab="datetime"))
    # Add line to the 1x2 plot
    with(hhpowerDTwoNASub, lines(strptime(DateTime, format="%d/%m/%Y %H:%M:%S"),
                                 Voltage))
    
    # Create the 2x1 plot
    par(cex = 0.5)
    with(hhpowerDTwoNASub, plot(strptime(DateTime, format="%d/%m/%Y %H:%M:%S"), 
                                Sub_metering_1, 
                                type="n",
                                ylab="Energy sub metering",
                                xlab=""))
    # Add sub metering lines to the 2x1 plot
    with(hhpowerDTwoNASub, lines(strptime(DateTime, format="%d/%m/%Y %H:%M:%S"),
                                 Sub_metering_1))
    with(hhpowerDTwoNASub, lines(strptime(DateTime, format="%d/%m/%Y %H:%M:%S"),
                                 Sub_metering_2, col="red"))
    with(hhpowerDTwoNASub, lines(strptime(DateTime, format="%d/%m/%Y %H:%M:%S"),
                                 Sub_metering_3, col="blue"))
    # Add the legend to the 2x1 plot
    legend("topright", col = c("black", "red", "blue"), 
           lty=1, lwd=1, 
           legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
    
    # Create the 2x2 plot
    par(cex = 0.5)
    with(hhpowerDTwoNASub, plot(strptime(DateTime, format="%d/%m/%Y %H:%M:%S"), 
                                Global_reactive_power, 
                                type="n",
                                ylab="Global_reactive_power",
                                xlab="datetime"))
    # Add line to the 2x2 plot
    with(hhpowerDTwoNASub, lines(strptime(DateTime, format="%d/%m/%Y %H:%M:%S"),
                                 Global_reactive_power))
    
    # Copy the plots to a png device
    dev.copy(png, file="plot4.png")
    # Turn the png device off
    dev.off()
}