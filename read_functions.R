# These "read functions" access the local drive to read in necessary data. 
# Some of these functions are used to create variables that are useful for analysis functions.




# ParticipantCounter: count the number of participants
# Parameters
# path: string, path to experiment directory
ParticipantCounter <- function(path) {

	# Checks if "/" exists after path. If not, one is added
	if(substr(path, nchar(path), nchar(path)) != "/") {
		path <- paste(path, "/", sep = "")
	}
	
	#Construct the zip folder path and list all zip files
	zip.path <- paste(path, "zip/", sep = "")
	files <- list.files(zip.path)
	
	#Get the total number of participants (zip files)
	np <- length(files)
	
	#Return the total number of participants as an integer
	return(np)
}

number.of.participants <- ParticipantCounter(path)




#IconCounter: count the number of icons(items) used in the experiment
# Parameters
# path: string, path to experiment directory
IconCounter <- function(path) {

	# Checks if "/" exists after path. If not, one is added
	if(substr(path, nchar(path), nchar(path)) != "/") {
		path <- paste(path, "/", sep = "")
	}

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

	unlink(substring(paste(getwd(), "/", files[1], sep = ""), 1, nchar(paste(getwd(), "/", files[1], sep = "")) - 4), recursive = TRUE)
	
	#Return the number of icons
	return(n.icons)
}

number.of.icons <- IconCounter(path)




# IconNamesGetter: get a list of icon names
# It also saves the icon.csv needed for KlipArt
# Parameters
# path: string, path to experiment directory
# scenario.name: string, name of the experiment
IconNamesGetter <- function(path, scenario.name) {

	# Checks if "/" exists after path. If not, one is added
	if(substr(path, nchar(path), nchar(path)) != "/") {
		path <- paste(path, "/", sep = "")
	}
	
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

	# create klipart folder if not already created
	klipart.path <- paste(path, scenario.name, "-klipart/", sep = "")
	dir.create(klipart.path, showWarnings = FALSE)
	
	# Export the list as a csv file
	write.table(icon.list.klipart, file = paste(path, scenario.name, "-klipart/", "icon.csv", sep = ""),
			sep = ",", row.names = F,  col.names = F)

	unlink(substring(paste(getwd(), "/", files[1], sep = ""), 1, nchar(paste(getwd(), "/", files[1], sep = "")) - 4), recursive = TRUE)
	
	#Return the icon list as a vector
	return(sort(icon.list))
}

icon.names <- IconNamesGetter(path, scenario.name)




# ExtractIsms: Unzipps the participant folders and copies each participants individual similarity matrix into the "ism" folder created in CatDirectorySetup.
# Also writes files to klipart folder (creates a klipart folder is one is not already created)
# Parameters
# path: string, path to experiment directory
# path: string, path to experiment directory
# number.of.icons: integer, the total number of icons in the experiment created by IconCounter
ExtractIsms <- function(path, scenario.name, number.of.icons) {

	# Checks if "/" exists after path. If not, one is added
	if(substr(path, nchar(path), nchar(path)) != "/") {
		path <- paste(path, "/", sep = "")
	}

	# Construct the zip folder path and list all zip files
	zip.path <- paste(path, "zip/", sep = "")
	files <- list.files(zip.path)

	# create ism folder if not already created
	ism.path <- paste(path, "ism/", sep = "")
	dir.create(ism.path, showWarnings = FALSE)

	# create klipart folder if not already created
	klipart.path <- paste(path, scenario.name, "-klipart/", sep = "")
	dir.create(klipart.path, showWarnings = FALSE)
	dir.create(paste(klipart.path, "matrices/", sep=""), showWarnings = FALSE)

	# create matrices folder if not already created
	matrices.path <- paste(path, "matrices/", sep = "")
	dir.create(matrices.path, showWarnings = FALSE)


	# Process the ISMs of the rest of participants
	for(i in 1:length(files)) {
		# Unzip the participant's zip file
		participant.i <- unzip(paste(zip.path, files[i], sep = ""))
		
		# Get the participant number
		participant.number <- substring(files[i], 1, nchar(files[i]) - 4)
		
		# Construct the full file name for .mtrx file
		matrix.i.name <- paste("./", participant.number, "/", substring(files[i],1,8), ".mtrx", sep = "")
		
		# Read in the ISM from a participant and exclude the non-ism info from the .mtrx file
		matrix.i <- read.delim(matrix.i.name, header = F, sep = " ", stringsAsFactors = F)
		matrix.i <- data.matrix(matrix.i[1:number.of.icons, ])
		
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

	}


	# Deletes all unzipped files as they are no longer needed
	folder.names <- substr(files, 1, nchar(files)-4)
  	for(folder in folder.names) {
    	unlink(paste(getwd(), "/", folder, sep = ""), recursive = TRUE)
  	}

}

ExtractIsms(path, scenario.name, number.of.icons)




# ReadIsms: Takes all the isms from the ism folder (the ism folder is populated via ExtractIsms) and reads them into R and returns a list of isms
# Parameters
# ism.path: string, full path to the ism directory 
# ism.list: character vector, list of all the isms you want included (if you want to include all of them, list.files(ism.path) will work)
# number.of.icons: integer, the total number of icons in the experiment created by IconCounter
ReadIsms <- function(ism.path, ism.list, number.of.icons) {

  # Checks if "/" exists after ism.path. If not, one is added
  if(substr(ism.path, nchar(ism.path), nchar(ism.path)) != "/") {
    ism.path <- paste(ism.path, "/", sep = "")
  }

  # creates an empty list to store imported isms
  isms <- list()
    
  for (i in 1:length(ism.list)) {

    # gathers the current ism file name from the ism.list parameter in the for loop
    matrix.i.name <- ism.list[i]

    # reads in the current ism file
    matrix.i <- read.delim(paste(ism.path, matrix.i.name, sep=""), header = F, sep = " ", stringsAsFactors = F)

    # converts the ism file to a matrix
    matrix.i <- data.matrix(matrix.i[1:number.of.icons, ])
        
    # adds that ism matrix to the isms list created at the begining of the function
    isms[[i]] <- matrix.i
  }

  # returns a list of the isms
  return(isms)

}

ism.path <- paste(path, "/ism", sep="")
isms <- ReadIsms(ism.path, list.files(ism.path), number.of.icons)















