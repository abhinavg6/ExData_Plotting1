plot3 <- function() {
    
    # Read the data file into a data table
    hhpowerDT <- fread("household_power_consumption.txt", na.strings="?")
    # Remove the NAs from the data table
    hhpowerDTwoNA <- na.omit(hhpowerDT)
    # Get the data subset we're interested in
    hhpowerDTwoNASub <- subset(hhpowerDTwoNA, Date=="1/2/2007" | 
                                   Date=="2/2/2007")
    # Combine Date and Time to create a new DateTime column
    hhpowerDTwoNASub[, DateTime := paste(Date, Time)]
    
    # Create the area for 1x1 plot
    par(mfrow = c(1,1), mar = c(5.1, 4.1, 4.1, 2.1))
    # Change the magnification size of the text and symbols
    par(cex = 0.75)
    # Create the empty plot
    with(hhpowerDTwoNASub, plot(strptime(DateTime, format="%d/%m/%Y %H:%M:%S"), 
                                Sub_metering_1, 
                                type="n",
                                ylab="Energy sub metering",
                                xlab=""))
    # Add sub metering lines to the plot
    with(hhpowerDTwoNASub, lines(strptime(DateTime, format="%d/%m/%Y %H:%M:%S"),
                                 Sub_metering_1))
    with(hhpowerDTwoNASub, lines(strptime(DateTime, format="%d/%m/%Y %H:%M:%S"),
                                 Sub_metering_2, col="red"))
    with(hhpowerDTwoNASub, lines(strptime(DateTime, format="%d/%m/%Y %H:%M:%S"),
                                 Sub_metering_3, col="blue"))
    # Add the legend to the plot
    legend("topright", col = c("black", "red", "blue"), 
           lty=1, lwd=1, 
           legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
    
    # Copy the plot to a png device
    dev.copy(png, file="plot3.png")
    # Turn the png device off
    dev.off()
}