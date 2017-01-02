
data = read.csv("household_power_consumption.txt", header = TRUE, sep = ";", stringsAsFactors = FALSE)

#convert the Date variable to a date class

data$Date <-  as.Date(data$Date, format="%d/%m/%Y")
class(data$Date)

head(data$Date)

#subset to dates  2007-02-01 and 2007-02-02 and remove the raw data
#frame to clear disk memory
dataSmall = data[data$Date == "2007-02-01" |data$Date =="2007-02-02",]
rm(data)

#create an array that concatenates the day and time
dateAndTime <- paste(as.character(dataSmall$Date), dataSmall$Time)

#convert the new variable to a time class
dataSmall$Time <- strptime(dateAndTime, "%Y-%m-%d %H:%M:%S")
summary(dataSmall$Time)
rm(dateAndTime)

#convert variables 3-9 to numeric
numericArray <- apply(dataSmall[,3:9], 2, as.numeric)
vector <- 3:9

#create a temp dataset to duplicate dataSmall's data
dataTest <- dataSmall

#run a for loop to replace the values of col 3-9 by its numeric
#counter parts stored in 'numericArray'
for(i in seq_along(vector)){
    dataTest[,vector[i]] <- numericArray[,i]
}

#rm variabels and data frames that will no longer be used
epc <- dataTest
rm(numericArray)
rm(vector)
rm(i)
rm(dataSmall)
rm(dataTest)
##### Plot 4 - Plot 4 plots at the same time ######
#set the screen device to display 4 plots in a 2 by 2 matrix format
par(mfrow=c(2,2))

with(epc,
     {
         #plot c(1,1)
         plot(Time, Global_active_power, type="l",
              xlab="",
              ylab="Global Active Power")
         
         #plot c(1,2)
         plot(Time, Voltage,
              type="l",
              xlab="datetime",
              ylab="Voltage")
         #plot c(2,1)
         plot(Time,Sub_metering_1, type="n",
              xlab = "",
              ylab = "Energy sub metering")
         
         points(epc$Time, epc$Sub_metering_1,
                col = "black",
                type = "l")
         points(epc$Time, epc$Sub_metering_2,
                col = "red",
                type = "l")
         points(epc$Time, epc$Sub_metering_3,
                col = "blue",
                type = "l")
         legend("topright", legend = names(epc[,7:9]),
                col =c("black", "red","blue"),
                lty =1,
                bty = "n",
                y.intersp = 0.5,
                cex=0.5)
         
         #plot c(2,2)
         plot(Time, Global_reactive_power,
              type="l",
              xlab="datetime")
     }
)

dev.copy(png, "plot4.png")
dev.off()

