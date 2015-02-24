## Kevin Sparks 2/24/15
## Functions taken from "catanalysis.R" and "catAnalysisExtension.R" from "jinlong25_catanalysis" repo (cross-referenced with "catanalysis2501" (geo terms))

## Notes
# There seemed to be two sets of functions being defined among various catanalysis scripts. The first, denoted by 6 lines of "#"s and called "Function list Pt. 1" 
# (followed with a set of executables running those functions denoted by three lines of "#"s), and the second, denoted by 6 lines of "#"s and called "Functions list Pt. 2"

# "clusterval" package not available in R 3.0.0

# Directly below is the set up for testing the functions. This is where you set paths, create sub-directories, and load packages. 










# Instruction
# 1. Create a folder with the name of the experiment;
# 2. In the experiment folder, create a folder named "zip" and put all participant zip files into the "zip" folder;
# 3. Change the PATH & SCENARIO NAME at the beginning of the script;
# 4. Run the entire script;
# 5. To create dendrograms at different solutions, manually change the number in the last line of the script.
# 6. Go find the result in the experiment folder

# KlipArt Instruction
# A klipart folder will be created inside the experiment folder. In the klipart folder:
# 1. Zip the matrices folder into a zip file;
# 2. Put a icons.zip file that contains all the icon files.

# Clear the workspace
rm(list=ls())

# Define the name of the experiment
scenario.name <- "2501 geotermsN"

# Define the path to the experiment folder (with a closing "/" or "\")

# path <- "E:/My Documents/Dropbox/qstr_collaboration/Spatial Cognition and Computation - Directions/analysis_jinlong/birdseye/red/"
# path <- "E:/My Documents/Dropbox/qstr_collaboration/Spatial Cognition and Computation - Directions/analysis_jinlong/sideview/black/"
# path <- "/Users/jinlong/Dropbox/Catscan experiments/Experiments/1200 mturk planes birdseye/analysis/birdseye_30/"
# path <- "E:/My Documents/Dropbox/qstr_collaboration/Catscan experiments/Experiments/2501 mturk geo terms new/"
path <- "C:/Users/Sparks/Google Drive/Alex/R_PackageCreation/catLibTests/"

# Checks if "/" exists after path. If not, one is added
if(substr(path, nchar(path), nchar(path)) != "/"){
	path <- paste(path, "/", sep = "")
}

# Define the max number of clusters
max.cluster <- 9

# Auto-create two subfolders "ism" and "matrices"
dir.create(paste(path, "ism/", sep=""))
klipart.path <- paste(path, scenario.name, "-klipart/", sep = "")
dir.create(klipart.path)
dir.create(paste(klipart.path, "matrices/", sep="")) 
dir.create(paste(path, "matrices/", sep="")) 

# Uncomment the install.package() functions if you haven't installed these packages
#install.packages("gplots")
require("gplots")
#install.packages("vegan")
require("vegan")
#install.packages("clusterval")
require("clusteval")














# BEGIN: Function list PRE
#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################















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
	write.table(icon.list.klipart, file = paste(klipart.path, "icon.csv", sep = ""),
			sep = ",", row.names = F,  col.names = F)
	
	#Return the icon list as a vector
	return(icon.list)
}















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
		
		write.table(matrix.i, file = paste(klipart.path, "matrices/", "participant", 
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
	
	write.table(osm, file = paste(klipart.path, "matrices/", "total.mtrx", sep = ""), 
			sep = " ", row.names = F,  col.names = F)
	
	osm <- cbind(IconListGetter(path), osm)
	write.table(osm, file = paste(path, "osm.csv", sep = ""), 
			sep = ",", row.names = F,  col.names = F)
}















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
	write.table(df, file = paste(klipart.path, "assignment.csv", sep = ""),
			sep = ",", row.names = F,  col.names = F)
}















# exe
n.icons <- IconCounter(path)

all.icons <- sort(IconListGetter(path))

np <- ParticipantCounter(path)

OsmIsmGenerator(path)

AssignmentGetter(path)