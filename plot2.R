plot2 <- function() {
    
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
                                Global_active_power, 
                                type="n",
                                ylab="Global Active Power (kilowatts)",
                                xlab=""))
    # Add line to the plot
    with(hhpowerDTwoNASub, lines(strptime(DateTime, format="%d/%m/%Y %H:%M:%S"),
                                 Global_active_power))
    
    # Copy the plot to a png device
    dev.copy(png, file="plot2.png")
    # Turn the png device off
    dev.off()
}