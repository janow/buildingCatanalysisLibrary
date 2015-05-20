## Kevin Sparks 3/4/15
## Functions taken from "catanalysis.R" and "catAnalysisExtension.R" from "jinlong25_catanalysis" repo (cross-referenced with "catanalysis2501" (geo terms))

## Notes
# There seemed to be two sets of functions being defined among various catanalysis scripts. The first, denoted by 6 lines of "#"s and called "Function list Pt. 1" 
# (followed with a set of executables running those functions denoted by three lines of "#"s), and the second, denoted by 6 lines of "#"s and called "Functions list Pt. 2"

# "clusterval" package not available in R 3.0.0 









# Instruction
# 1. Create a folder with the name of the experiment;
# 2. In the experiment folder, create a folder named "zip" and put all participant zip files into the "zip" folder;
# 3. Run the entire script ("function_list_pre.R"), then run "function_list_1.R", followed by "function_list_2.R";
# 4. To create dendrograms at different solutions, manually change the number in the last line of the script.
# 5. Go find the result in the experiment folder

# KlipArt Instruction
# A klipart folder will be created inside the experiment folder. In the klipart folder:
# 1. Zip the matrices folder into a zip file;
# 2. Put a icons.zip file that contains all the icon files.


#install.packages("gplots")
require(gplots)
#install.packages("vegan")
require(vegan)
#install.packages("clusteval")
require(clusteval)
require(grid)

# Clear the workspace
rm(list=ls())

# Begin user input #
# Path to where the experiment folder is
path <- "/Users/sparks/Google Drive/Alex/R_PackageCreation/catLibTests"

# Checks if "/" exists after path. If not, one is added
if(substr(path, nchar(path), nchar(path)) != "/") {
	path <- paste(path, "/", sep = "")
}

setwd(path)

# Define the name of the experiment
scenario.name <- "2501 geotermsN"

# # Define the max number of clusters
# max.cluster <- 9

# End user input #




##########
##########
##########




CatDirectorySetup <- function(path) {
	# Auto-create two subfolders "ism" and "matrices"
	dir.create(paste(path, "ism/", sep=""))
	klipart.path <- paste(path, scenario.name, "-klipart/", sep = "")
	dir.create(klipart.path)
	dir.create(paste(klipart.path, "matrices/", sep="")) 
	dir.create(paste(path, "matrices/", sep="")) 
}

CatDirectorySetup(path)




#Icon counter: count the number of icons(items) used in the experiment
IconCounter <- function(path) {
	#Construct the zip folder path and list all the zip files
	zip.path <- paste(path, "zip/", sep = "")
	files <- list.files(zip.path)

	#Get the participant number for the first participant
	first.p.number <- substring(files[1], 1, nchar(files[1]) - 4)
	
	#Unzip the file
	first.p <- unzip(paste(zip.path, files[1], sep = ""))
	
	#Construct the full file name for icons.csv
	icons.csv <- paste("./", first.p.number, "/", substring(files[1],1,8), "icons.csv", sep = "")
	
	#Read in icons.csv
	icons <- read.csv(icons.csv, header = F)
	
	#Get the number of icons used in the experiment
	n.icons <- nrow(icons)
	
	#Return the number of icons
	return(n.icons)
}

number.of.icons <- IconCounter(path)




# Icon list getter: get a list of icon names
# It also saves the icon.csv needed for KlipArt
IconListGetter <- function(path) {
	
	# Construct the zip folder path and list all the zip files 
	zip.path <- paste(path, "zip/", sep = "")
	files <- list.files(zip.path)
	
	# Unzip the zip file from the 1st participant
	first.p <- unzip(paste(zip.path, files[1], sep = ""))
	
	# Get the participant number for the first participant
	first.p.number <- substring(files[1], 1, nchar(files[1]) - 4)
	
	# Construct the full file name for icons.csv
	icons.csv <- paste("./", first.p.number, "/", substring(files[1],1,8), "icons.csv", sep = "")
	
	# Read in icons.csv
	icons <- read.csv(icons.csv, header = F, stringsAsFactors = F)
	
	# Reorder icon names by icon index
	icons <- icons[order(icons[, 1]), ]
	
	# Extract the icon names from the table (excluding the path name and file extensions)
	icon.list <- icons[, 2]
	for(i in 1:length(icon.list)) {
		end <- regexpr("\\.[^\\.]*$", icon.list[i])[1]
		icon.list[i] <- substr(icon.list[i], 9, end - 1)
	}
	
	# Extract the icon names with file type (e.g. .jpg) for KlipArt
	icon.list.klipart <- icons
	for(j in 1:nrow(icon.list.klipart)) {
		icon.list.klipart[j, 2] <- substr(icon.list.klipart[j, 2], 9, nchar(icon.list.klipart[j, 2]))
	}
	colnames(icon.list.klipart) <- c("index", "icon_names")
	
	# Sort the icon list by index
	icon.list.klipart <- icon.list.klipart[order(icon.list.klipart$index), ]
	
	# Export the list as a csv file
	write.table(icon.list.klipart, file = paste(paste(path, scenario.name, "-klipart/", sep = ""), "icon.csv", sep = ""),
			sep = ",", row.names = F,  col.names = F)
	
	#Return the icon list as a vector
	return(sort(icon.list))
}

icon.names <- IconListGetter(path)




# OSM and ISM Generator: extract all individual similarity matrices (ISMs) 
# and generate the overall similarity matrix(OSM) by summing up all ISMs
OsmIsmGenerator <- function(path) {
	# Construct the zip folder path and list all zip files
	zip.path <- paste(path, "zip/", sep = "")
	files <- list.files(zip.path)
	
	# Initialize osm with the 1st ISM
	participant1 <- unzip(paste(zip.path, files[1], sep = ""))
	
	# Get the participant number for the first participant
	first.p.number <- substring(files[1], 1, nchar(files[1]) - 4)
	
	# Construct the full file name for ISM file (mtrx file)
	first.ism <- paste("./", first.p.number, "/", substring(files[1],1,8), ".mtrx", sep = "")
	
	# Read in the ISM from the 1st participant and exclude the non-ism info from the .mtrx file
	first.matrix <- read.delim(first.ism, header = FALSE, sep = " ", stringsAsFactors = F)
	first.matrix <- data.matrix(first.matrix[1:IconCounter(path), ])
	
	# Export the first ISM
	write.table(first.matrix, file = paste(path, "ism/", "participant", 
                                        substr(files[1], 1, nchar(files[1]) - 4),
                                        ".mtrx",sep = ""), sep = " ", row.names = F, col.names = F)
	
	write.table(first.matrix, file = paste(path, "matrices/", "participant", 
                                        substr(files[1], 1, nchar(files[1]) - 4),
                                        ".mtrx",sep = ""), sep = " ", row.names = F, col.names = F)
	
	# Summing up all ISMs for OSM and export each ISM
	osm <- first.matrix
	
	# Process the ISMs of the rest of participants
	for(i in 2:length(files)) {
		# Unzip the participant's zip file
		participant.i <- unzip(paste(zip.path, files[i], sep = ""))
		
		# Get the participant number
		participant.number <- substring(files[i], 1, nchar(files[i]) - 4)
		
		# Construct the full file name for .mtrx file
		matrix.i.name <- paste("./", participant.number, "/", substring(files[i],1,8), ".mtrx", sep = "")
		
		# Read in the ISM from a participant and exclude the non-ism info from the .mtrx file
		matrix.i <- read.delim(matrix.i.name, header = F, sep = " ", stringsAsFactors = F)
		matrix.i <- data.matrix(matrix.i[1:IconCounter(path), ])
		
		# Export the ISM as .mtrx for KlipArt and .csv for catanalysis
		write.table(matrix.i, file = paste(path, "ism/", "participant", 
						substr(files[i], 1, nchar(files[i]) - 4),
						".mtrx", sep = ""), sep = " ", 
						row.names = F, col.names = F)
		
		write.table(matrix.i, file = paste(paste(path, scenario.name, "-klipart/", sep = ""), "matrices/", "participant", 
						substr(files[i], 1, nchar(files[i]) - 4), 
						".mtrx", sep = ""), sep = " ",
						row.names = F, col.names = F)
		
		write.table(matrix.i, file = paste(path, "matrices/", "participant", 
						substr(files[i], 1, nchar(files[i]) - 4), 
						".mtrx", sep = ""), sep = " ",
						row.names = F, col.names = F)
		
		# Add the ISM to OSM
		osm <- osm + matrix.i
	}
	
	# Export OSM
	write.table(osm, file = paste(path, "matrices/", "total.mtrx", sep = ""), 
			sep = " ", row.names = F,  col.names = F)
	
	write.table(osm, file = paste(paste(path, scenario.name, "-klipart/", sep = ""), "matrices/", "total.mtrx", sep = ""), 
			sep = " ", row.names = F,  col.names = F)
	
	osm <- cbind(IconListGetter(path), osm)
	write.table(osm, file = paste(path, "osm.csv", sep = ""), 
			sep = ",", row.names = F,  col.names = F)
}

OsmIsmGenerator(path)




# AssignmentGetter: generate the assignment.csv for KlipArt
AssignmentGetter <- function(path) {
	# Create an empty dataframe
	df <- data.frame()
	
	#List all zip files
	zip.path <- paste(path, "zip/", sep = "")
	files <- list.files(zip.path)
	
	for(i in 1:length(files)) {
		# Read in the assignment.csv file
		participant.i <- unzip(paste(zip.path, files[i], sep = ""))
		
		# Get the participant number for the first participant
		participant.number <- substring(files[i], 1, nchar(files[i]) - 4)
		
		# Construct the full file name for assignment.csv file
		participant.assignment <- paste("./", participant.number, "/", substring(files[i],1,8), 
				"assignment.csv", sep = "")
		
		assignment <- read.delim(participant.assignment, header = F, sep = ",", stringsAsFactors = F)
		df <- rbind(df, assignment)
	}
	# Export the assignment.csv
	write.table(df, file = paste(paste(path, scenario.name, "-klipart/", sep = ""), "assignment.csv", sep = ""),
			sep = ",", row.names = F,  col.names = F)
}

AssignmentGetter(path)




# Participant info: collect demographic info and basic experiment info (# of groups created
# and time spent in seconds)
ParticipantInfo <- function(path) {
	
	# Read in the zip file
	zip.path <- paste(path, "zip/", sep = "")
	files <- list.files(zip.path)
	
	# Read in to demographic info for the 1st participant
	participant1 <- unzip(paste(zip.path, files[1],sep = ""))
	
	# Get the participant number for the first participant
	first.p.number <- substring(files[1], 1, nchar(files[1]) - 4)
	
	# Construct the full file name for the participant.csv file for the 1st participant
	first.demo <- paste("./", first.p.number, "/", substring(files[1],1,8), "participant.csv", sep = "")
	
	# Read in the participant.csv for the 1st participant
	demo1 <- read.delim(first.demo, header = F, sep = ",",stringsAsFactors = F)
	
	# Aggregate eduction background for participant who use comma(s) in their eduction 
	# background (e.g., geography, education, business)
	while(length(demo1) > 13) {
		demo1[7] <- paste(demo1[7], demo1[8], sep = ",")
		demo1 <- demo1[-8]
	}

	colnames(demo1) <- 1:13
	
	# Initialize the dataframe for demographic info
	demographic <- demo1
	
	# Add demographic info from the rest of participants to the dataframe "demographic"
	for(i in 2:length(files)) {
		participant.i <- unzip(paste(zip.path, files[i], sep=""))
		
		# Get the participant number for the first participant
		participant.number <- substring(files[i], 1, nchar(files[i]) - 4)
		
		# Construct the full file name for participant.csv file
		participant.demo <- paste("./", participant.number, "/", substring(files[i],1,8), 
				"participant.csv", sep = "")
		
		# Read in the participant.csv
		demo <- read.delim(participant.demo, header = F, sep = ",", stringsAsFactors = F)
		while(length(demo) > 13) {
			demo[7] <- paste(demo[7], demo[8], sep = ",")
			demo <- demo[-8]
		}
		colnames(demo) <- 1:13
		demographic <- rbind(demographic, demo)
	}
	
	# Create two vectors to store the # of groups created and time spent (in seconds)
	groups.created <- c()
	time.spent <- c()
	
	for(i in 1:length(files)) {
		# Read in the assignment.csv file
		participant.i <- unzip(paste(zip.path, files[i], sep = ""))
		
		# Get the participant number for the first participant
		participant.number <- substring(files[i], 1, nchar(files[i]) - 4)
		
		# Construct the full file name for assignment.csv file
		participant.groups <- paste("./", participant.number, "/", substring(files[i],1,8), 
				"assignment.csv", sep = "")
		
		groups <- read.delim(participant.groups, header = F, sep = ",", stringsAsFactors = F)
		
		# Count the number of rows in the assignment file and convert it to the # of groups created
		groups <- length(unique(groups[, 2]))
		
		# Append the # of groups created to the vector "groups.created"
		groups.created <- append(groups.created, groups)
	}
	
	
	for(i in 1:length(files)) {
		# Read in the log file
		participant.i <- unzip(paste(zip.path, files[i], sep=""))
		
		# Get the participant number for the first participant
		participant.number <- substring(files[i], 1, nchar(files[i]) - 4)
		
		# Construct the full file name for .log file
		participant.log <- paste("./", participant.number, "/", substring(files[i],1,8), ".log", sep = "")
		
		# Read in the log file
		log <- read.delim(participant.log, header = F, sep=",", stringsAsFactors = F)
		
		# Get the time spent
		time <- log[nrow(log), ]
		time <- substr(time, 33, nchar(time))
		
		# Append the time spent to the vector "time.spent"
		time.spent <- append(time.spent, time)
	}
	
	# Append two vectors (i.e., two columns) - groups.created and time.spent to the demographic dataframe
	demographic <- cbind(demographic, groups.created)
	demographic <- cbind(demographic, time.spent)
	
	# Export the demographic dataframe as a csv file
	write.table(demographic, file = paste(path, "participant.csv", sep = ""),
			sep = ",", row.names = F,  col.names = F)
	
	# Export the participant.csv for KlipArt
	write.table(demographic, file = paste(paste(path, scenario.name, "-klipart/", sep = ""), "participant.csv", sep = ""), sep = ",", row.names = F,  col.names = F)
}

ParticipantInfo(path)




# DescriptionGetter: extract the linguistic labels (both long and short) from all participants and store in a single csv file
DescriptionGetter <- function(path) {
	
	# Construct the path for the zip folder and list all the zip files
	zip.path <- paste(path, "zip/", sep = "")
	files <- list.files(zip.path)
	
	# Unzip the zip file from the 1st participant
	participant1 <- unzip(paste(zip.path, files[1], sep =""))
	
	# Get the participant number for the first participant
	participant.number <- substring(files[1], 1, nchar(files[1]) - 4)
	
	# Construct the full file name for the batch.csv file
	batch <- paste("./", participant.number, "/", substring(files[1],1,8), "batch.csv", sep = "")
	
	description1 <- read.csv(batch, header = F, stringsAsFactors = F)
	
	# Aggregate participants' long descriptions when they use comma in the descriptions.
	while(length(description1) > 4) {
		description1[, 4] <- paste(description1[, 4], description1[,5], sep = ",")
		description1 <- description1[-5]
	}
	
	# Create dummy column names for the dataframe (will not be included when exported)
	colnames(description1) <- 1:4
	
	# Initialize a dataframe for all descriptions
	description <- description1
	
	# Read in the batch.csv for the rest of participants and extract the descriptions
	for(i in 2:length(files)) {
		
		# Read in the zip files
		participant.i <- unzip(paste(zip.path, files[i], sep = ""))
		description.i <- read.csv(sort(participant.i)[5], header = F, stringsAsFactors = F)
		
		# Aggregate participants' long descriptions when they use comma in the descriptions.
		while(length(description.i) > 4) {
			description.i[4] <- paste(description.i[, 4], description.i[, 5], sep = ",")
			description.i <- description.i[-5]
		}
		
		# Create dummy column names for the dataframe (will not be included when exported)
		colnames(description.i) <- 1:4
		
		# Combine descriptions from all participant into a dataframe (row-bind)
		description <- rbind(description, description.i)
	}
	
	# Export the description dataframe as a csv file
	write.table(description, file = paste(path, "description.csv", sep = ""), 
			sep = ",", row.names = F,  col.names = F)
	
	# Export batch.csv for Klipart
	write.table(description, file=paste(paste(path, scenario.name, "-klipart/", sep = ""), "batch.csv", sep = ""), sep = ",", col.names=  F, row.names = F)
}

DescriptionGetter(path)




# Participant counter: count the number of participants
ParticipantCounter <- function(path) {
	
	#Construct the zip folder path and list all zip files
	zip.path <- paste(path, "zip/", sep = "")
	files <- list.files(zip.path)
	
	#Get the total number of participants (zip files)
	np <- length(files)
	
	#Return the total number of participants as an integer
	return(np)
}

number.of.participants <- ParticipantCounter(path)











 