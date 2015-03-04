#install.packages("tm")
require(tm)
#install.packages("wordcloud")
require(wordcloud)
#install.packages("SnowballC")
require(SnowballC)
#install.packages("RColorBrewer")
require(RColorBrewer)

CatWordcloud <- function(dir.path) {

	# Checks if "/" exists after path. If not, one is added
	if(substr(dir.path, nchar(dir.path), nchar(dir.path)) != "/") {
		dir.path <- paste(dir.path, "/", sep = "")
	}

	word.data <- Corpus(DirSource(dir.path))

	# inspect(word.data)

	# cleaning the txt file (could also use removeNumbers and removePunctuation)

	# strip unnecessary whitespace
	word.data <- tm_map(word.data, stripWhitespace)
	# inspect(word.data)

	#convert to lowercase
	word.data <- tm_map(word.data, tolower)
	# inspect(word.data)

	# removes common undesired words like "the" (stopwords)
	word.data <- tm_map(word.data, removeWords, stopwords("english"))
	# inspect(word.data)

	# resolves formatting issues

	word.data <- tm_map(word.data, stemDocument)
	# inspect(word.data)

	word.data <- tm_map(word.data, PlainTextDocument)
	# inspect(word.data)

	# Create jpeg of wordcould output 
	jpeg(paste(dir.path, "/wordcloud.jpeg", sep=""), width = 480, height = 480, units = "px", pointsize = 12)
	wordcloud(word.data, scale=c(5,0.5), min.freq=3, max.words=100, 
	          random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, 
	          colors=brewer.pal(8, "Dark2"))
	dev.off()

}


# Has to be the directory of where the .txt file is, not the .txt file itself.
# Also, no other files can be in the directory where the .txt file is.
dir.path <- "C:/Users/Sparks/Google Drive/Alex/R_PackageCreation/wordcloud"

CatWordcloud(dir.path)