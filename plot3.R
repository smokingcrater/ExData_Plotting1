retrieve_data <- function(file_raw_data, date_start, date_end)
{
	cat(paste("Retrieving data for", date_start, "to", date_end, "from", file_raw_data, "\n"))
	# User Unix cut command to read only first column of the raw data file.
	con <- pipe(paste("cut -f1 -d\";\"", file_raw_data))
	# Load first column of raw data file into a character vector.
	preview <- scan(con, what="character()", skip=1)
	close(con)
	# Determine the starting and ending rows.
	row_start <- match(date_start, preview)
	row_end <- match(date_end, preview)
	# Read all columns for only the desired dates from the raw data file.
	df_raw <- read.table(file_raw_data, skip=row_start, nrows=row_end-row_start, sep=";")
	# Combine the date and time columns and convert them to a date-friendly format.
	date_time <- strptime(paste(df_raw[,1], df_raw[,2]), format="%d/%m/%Y %H:%M:%S")
	# Generate a data.frame with the processed dates and give the columns useful names.
	df_processed <- data.frame(date_time, df_raw[,3:9])
	colnames(df_processed) <- c("date_time", "global_active_power", "global_reactive_power", "voltage", "global_intensity", "sub_metering1", "sub_metering2", "sub_metering3")
	return(df_processed)
}

generate_plot3 <- function(file_raw_data, date_start, date_end, file_output)
{
	# Read the data.
	df_epc <- retrieve_data(file_raw_data, date_start, date_end)
	# Generate the plot.
	cat(paste("Writing plot to", file_output, "\n"))
	png(file=file_output)
	plot(df_epc[,"date_time"], df_epc[,"sub_metering1"], col="black", xlab="", ylab="Energy sub metering", type="l")
	lines(df_epc[,"date_time"], df_epc[,"sub_metering2"], col="red")
	lines(df_epc[,"date_time"], df_epc[,"sub_metering3"], col="blue")
	legend("topright", lty=c(1, 1, 1), col=c("black", "red", "blue"), legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
	dev.off()
}

generate_plot3("household_power_consumption.txt", "1/2/2007", "3/2/2007", "plot3.png")
