plot1 <- function() {
    
    # Read the data file into a data table
    hhpowerDT <- fread("household_power_consumption.txt", na.strings="?")
    # Remove the NAs from the data table
    hhpowerDTwoNA <- na.omit(hhpowerDT)
    # Get the data subset we're interested in
    hhpowerDTwoNASub <- subset(hhpowerDTwoNA, Date=="1/2/2007" | 
                                            Date=="2/2/2007")
    
    # Create the area for 1x1 plot
    par(mfrow = c(1,1), mar = c(5.1, 4.1, 4.1, 2.1))
    # Change the magnification size of the text and symbols
    par(cex = 0.75)
    # Create the plot, and add a title
    hist(as.numeric(hhpowerDTwoNASub$Global_active_power), col="red", 
         main="Global Active Power", xlab="Global Active Power (kilowatts)")
    
    # Copy the plot to a png device
    dev.copy(png, file="plot1.png")
    # Turn the png device off
    dev.off()
}