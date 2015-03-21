## Kevin Sparks 3/4/15
# A file to test out new functions to later be implemented 

#install.packages("tm")
require(tm)
#install.packages("wordcloud")
require(wordcloud)
#install.packages("SnowballC")
require(SnowballC)
#install.packages("RColorBrewer")
require(RColorBrewer)





## Takes the zip folders, creates a temporary unzipped folder, unzips all 
## the folders and stores them in the temporary unzipped folder, and then 
## begins to extract relevant information from each participant's "batch.csv"
## file, builds a master txt file and stores all participant's info from "batch.csv"
## in the txt, exports it to a new "wordcloud" directory created, and then 
## the temporary unzipped folder is deleted. 
CatWordcloudTextCreator <- function(dir.path) {

  if(substr(dir.path, nchar(dir.path), nchar(dir.path)) != "/") {
    dir.path <- paste(dir.path, "/", sep = "")
  }

  ##Read in files
  files <- list.files(paste(dir.path, "zip/", sep=""))
  zip.path <- paste(dir.path, "zip/", sep="")
  unzip.path <- paste(dir.path, "unzipped/", sep="")
  dir.create(unzip.path)

  text <- c()

  #Sets the working directory to the unzipped folder 
  setwd(unzip.path)

  ##Begins for loop to loop through participants' zip folders
  for(p in files){

    ##Unzippes participant folder
    participant <- unzip(paste(zip.path, p, sep=""))

    ##Gathers linguistic responses from participants 
    # batch <- unzip(paste(zip.path, p, sep=""))
    check <- participant[6]
    if(substr(check, nchar(check) - 13, nchar(check)) != "batch.csv"){
      check <- participant[5]
    }
    
    ##Reads in linguistic response data
    batch_file <- read.delim(check, header=FALSE, sep=",",stringsAsFactors=F)
    linguistic_info <- batch_file[,3:4]

    text <- c(text, linguistic_info$V3, linguistic_info$V4)

  }

  # remove word selected
  text <- removeWords(text, "selected")

  dir.create(paste(dir.path, "wordcloud/", sep=""))

  write.table(text, paste(dir.path, "wordcloud/", "wordcloudtext.txt", sep=""))

  unlink(unzip.path, recursive=TRUE)

}

## Run CatWordcloudTextCreator
dir.path <- "/Users/sparks/Google Drive/Alex/R_PackageCreation/catLibTests/"
CatWordcloudTextCreator(dir.path)



## Takes the master txt file created from CatWordcloudTextCreator and creates
## a wordcloud that gets stored in the "wordcloud" directory as a jpeg. Text
## cleanup takes place to remove stop words punctuation, and other undesireble words.
CatWordcloud <- function(wordcloud.path) {

  # Checks if "/" exists after path. If not, one is added
  if(substr(wordcloud.path, nchar(wordcloud.path), nchar(wordcloud.path)) != "/") {
    wordcloud.path <- paste(wordcloud.path, "/", sep = "")
  }

  word.data <- Corpus(DirSource(wordcloud.path))
  #inspect(word.data)

  # cleaning the txt file (could also use removeNumbers and removePunctuation)

  # strip unnecessary whitespace
  word.data <- tm_map(word.data, stripWhitespace)
  #inspect(word.data)

  #convert to lowercase
  word.data <- tm_map(word.data, tolower)
  # inspect(word.data)

  # removes common undesired words like "the" (stopwords)
  word.data <- tm_map(word.data, removeWords, stopwords("english"))
  # inspect(word.data)

  # remove numbers
  word.data <- tm_map(word.data, removeNumbers)

  # remove punctuation
  word.data <- tm_map(word.data, removePunctuation)

  # resolves formatting issues

  word.data <- tm_map(word.data, stemDocument)
  # inspect(word.data)

  word.data <- tm_map(word.data, PlainTextDocument)
  # inspect(word.data)

  # Create jpeg of wordcould output 
  jpeg(paste(wordcloud.path, "/wordcloud.jpeg", sep=""), width = 480, height = 480, units = "px", pointsize = 12)
  wordcloud(word.data, scale=c(5,0.5), min.freq=3, max.words=100, 
            random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, 
            colors=brewer.pal(5, "Dark2"))
  dev.off()

}


# Has to be the directory of where the .txt file is, not the .txt file itself.
# Also, no other files can be in the directory where the .txt file is.
wordcloud.path <- "/Users/Sparks/Google Drive/Alex/R_PackageCreation/catLibTests/wordcloud"
CatWordcloud(wordcloud.path)










# require(Cairo)
require(png)
require(gridExtra)

IconGroupViz <- function() {
	##Checks if "/" exists after path. If not, one is added
  if(substr(path, nchar(path), nchar(path)) != "/"){
    path <- paste(path, "/", sep = "")
  }

  ##Creates a folder "icon" within the path to save the icon names csv to 
  klipart_path <- paste(path, "icon/", sep = "")
  dir.create(klipart_path)

  ##Creates a folder "heatmaps" within the path to save the heatmap images to 
  # heatmap_path <- paste(path, "heatmaps/", sep = "")
  # dir.create(heatmap_path)

  ##Creates a folder "unzipped" within the path to save the unzipped folders to 
  unzipped_path <- paste(path, "unzipped/", sep = "")
  dir.create(unzipped_path)

  ##Icon list getter: get a list of icon names
  ##It also saves the icon.csv needed for KlipArt
  icon_list_getter <- function(path){
    
    #Construct the zip folder path and list all the zip files 
    zip_path <- paste(path, "zip/", sep = "")
    files <- list.files(zip_path)
    
    #Sets the working directory to the unzipped folder 
    setwd(unzipped_path)

    #Unzip the zip file from the 1st participant
    first_p <- unzip(paste(zip_path, files[1], sep = ""))
    
    #Sets the working directory to the unzipped folder 
    setwd(unzipped_path)

    #Get the participant number for the first participant
    first_p_number <- substring(files[1], 1, nchar(files[1]) - 4)
    
    #Construct the full file name for icons.csv
    icons_csv <- paste("./", first_p_number, "/", first_p_number, "icons.csv", sep = "")
    
    #Read in icons.csv
    icons <- read.csv(icons_csv, header = F, stringsAsFactors = F)
    
    #Reorder icon names by icon index
    icons <- icons[order(icons[, 1]),]
    
    #Extract the icon names from the table (excluding the path name and file extensions)
    icon_list <- icons[, 2]
    for(i in 1:length(icon_list)){
      end <- regexpr("\\.[^\\.]*$", icon_list[i])[1]
      icon_list[i] <- substr(icon_list[i], 9, end - 1)
    }

    #Extract the icon names with file type (e.g. .jpg) for KlipArt
    icon_list_klipart <- icons
    for(j in 1:nrow(icon_list_klipart)){
      icon_list_klipart[j, 2] <- substr(icon_list_klipart[j, 2], 9, nchar(icon_list_klipart[j, 2]))
    }
    colnames(icon_list_klipart) <- c("index", "icon_names")
    
    #Sort the icon list by index
    icon_list_klipart <- icon_list_klipart[order(icon_list_klipart$index) , ]
    
    #Export the list as a csv file
    write.table(icon_list_klipart, file = paste(klipart_path, "icon.csv", sep = ""),
                sep = ",", row.names = F,  col.names = F)
    
    #Return the icon list as a vector
    return(icon_list)
  }


  ##Defines variable "all_icons" as a list of icon names
  all_icons <- sort(icon_list_getter(path))


  #####IconHeatmap_Viz#####

  ##Read in files
  zip_path <- paste(path, "zip/", sep="")
  files <- list.files(zip_path)

  ##Begins for loop to loop through participants' zip folders
  for(p in files){

    #Sets the working directory to the unzipped folder 
    setwd(unzipped_path)

    ##Unzippes participant folder
    participant <- unzip(paste(zip_path, p, sep=""))

    ##Looks for the "assignment.csv" file within the now unzipped folder
    check <- participant[6]
    if(substr(check, nchar(check) - 13, nchar(check)) != "assignment.csv"){
      check <- participant[4]
    }
    
    ##reads in "assignnmet.csv" file
    d <- read.delim(check, header=FALSE, sep=",",stringsAsFactors=F)
    
    d <- d[order(d[,3]),] 
    
    
    ##Gathers linguistic responses from participants 
    batch <- unzip(paste(zip_path, p, sep=""))
    check <- batch[6]
    if(substr(check, nchar(check) - 13, nchar(check)) != "batch.csv"){
      check <- batch[5]
    }
    
    ##Reads in linguistic response data
    batch_file <- read.delim(check, header=FALSE, sep=",",stringsAsFactors=F)
    linguistic_info <- batch_file[,2:3]
    
    ##outputs the unique integers in column 2 of the csv
    groups <- unique(d[,2]) 
    
    ##creates an empty list called "all_groups"
    all_groups <- list()
    
    ##fills in the created "all_groups" list with the groups the participant created 
    for(group in groups){
      all_groups[[group+1]] <- d[d[,2] == group,] 
    }
    
    
    ################################

    ## Prints a list of the participant groups, with the first word
    # being their group name they have decided on, and the following
    # numbers being the index of the icons in that group. 

    # Prints group icons
    group.icon.numbers <- all_groups[[1]][,3]

    # Prints group name
    linguistic.index <- all_groups[[1]][1,2]
    group.name <- linguistic_info[linguistic.index+1, 2]


    p1.list <- vector("list", length(all_groups))
    for(i in 1:length(all_groups)){
    	# Prints group icons
    	group.icon.numbers <- all_groups[[i]][,3]+1

    	# Prints group name
    	linguistic.index <- all_groups[[i]][1,2]
    	group.name <- linguistic_info[linguistic.index+1, 2]

    	p1.list[[i]] <- c(group.name, group.icon.numbers)
    	
    }
    
#########################################

    ## Prints a list of the participant groups, with the first word
    # being their group name they have decided on, and the following
    # words being the name of the icons in that group. 

    all_icons[1]

    # Prints group icons
    group.icon.numbers <- all_groups[[1]][,3]

    # Prints group name
    linguistic.index <- all_groups[[1]][1,2]
    group.name <- linguistic_info[linguistic.index+1, 2]


    p1.list <- vector("list", length(all_groups))
    for(i in 1:length(all_groups)){
    	# Prints group icons
    	group.icon.numbers <- all_icons[all_groups[[i]][,3]+1]

    	# Prints group name
    	linguistic.index <- all_groups[[i]][1,2]
    	group.name <- linguistic_info[linguistic.index+1, 2]

    	p1.list[[i]] <- c(group.name, group.icon.numbers)
    	
    }



    png1 <- readPNG(paste(path, "testImages/athens1.png", sep=""))




    thePlots <- lapply (2:length(names(mtcars)), function(i) {
	  png("testgraph.png")
	  plot(mtcars[,1], mtcars[,i])

	  dev.off()
	  rasterGrob(readPNG("testgraph.png", native = FALSE),
	    interpolate = FALSE)
	})

	pdf(paste(path, "testgraph.pdf", sep=""))
	do.call(grid.arrange, c(thePlots, ncol = 3))
	dev.off()



    
  }
}






# Removes all the unzipped participant folders in working directory
CatCleanUp <- function(path) {
  zip.path <- paste(path, "zip/", sep = "")
  zip.files <- list.files(zip.path)
  folder.names <- substr(zip.files, 1, nchar(zip.files)-4)
  for(folder in folder.names) {
    unlink(paste(getwd(), "/", folder, sep = ""), recursive = TRUE)
  }
}

CatCleanUp(path)
















