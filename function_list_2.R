## Kevin Sparks 2/24/15
# Make sure to run "function_list_pre.R" and "function_list_1.R" before attempting to run the executables at the
# bottom of this file. In particular, some functions below rely on the "clusterval" package 
# that is not available in R version 3.0.0. Also, some executables rely on outputs from executeables from 
# "function_list_1.R".















# Creates a sample of participants of size sample_size and computes the hclust objects and cophenetic matrices for this sample.
# The return value is a 6 element list (hclust_ave,coph.ave,hclust_comp,coph.comp,hclust_ward,coph.ward)
SamplingRun <- function(path, ism.list, sample_size) {
	d <- read.csv(paste(path, "osm.csv", sep = ""), header = F)
	
	# Construct the zip folder path and list all zip files
	ism.path <- paste(path, "ism/", sep = "")
	files <- ism.list
	
	# Initialize OSM
	osm <- matrix(0,nrow(d),nrow(d))
	
	# create random sample numbers
	r <- sample(length(ism.list), size=sample_size, replace=FALSE)
	
	# create OSM for sample
	for (i in 1:sample_size) {
		matrix.i.name <- files[r[i]]
		# print(paste("reading",matrix.i.name))
		
		# Read in the ISM from a participant and exclude the non-ism info from the .mtrx file
		matrix.i <- read.delim(paste(ism.path,matrix.i.name,sep=""), header = F, sep = " ", stringsAsFactors = F)
		matrix.i <- data.matrix(matrix.i[1:nrow(d), ])
		
		osm <- osm + matrix.i
	}
	
	# create hclust objects for different methods
	ave <- hclust(method = "average", as.dist(sample_size - osm))
	comp <- hclust(method = "complete", as.dist(sample_size - osm))
	ward <- hclust(method = "ward", as.dist(sample_size - osm))
	
	# compute cophenetic matrices
	coph.ave <- as.matrix(cophenetic(ave)) 
	coph.comp <- as.matrix(cophenetic(comp)) 
	coph.ward <- as.matrix(cophenetic(ward)) 
	
	return(list(ave,coph.ave,comp,coph.comp,ward,coph.ward,r))
}















# Perform a complete sampling experiment in which an average cophenetic matrix for samples of participants is compared
# to that of the entire set of participnats. The number of trials is given by paramter 'trials' and a sample size 
# of 'sample_size' 
CopheneticSampling <- function(path, ism.list,trials, sample_size) {
	# read overall osm
	d <- read.csv(paste(path, "osm.csv", sep = ""), header = F)
	dm <- as.matrix(d[, -1])
	dimnames(dm) <- list(d[, 1], d[, 1])
	
	# derive cophenetic matrices for all participants
	ave.all <- hclust(method = "average", as.dist(ParticipantCounter(path) - dm))
	comp.all <- hclust(method = "complete", as.dist(ParticipantCounter(path) - dm))
	ward.all <- hclust(method = "ward", as.dist(ParticipantCounter(path) - dm))
	
	coph.ave.total <- matrix(0,nrow(d),nrow(d))
	coph.comp.total <- matrix(0,nrow(d),nrow(d))
	coph.ward.total <- matrix(0,nrow(d),nrow(d))
	
	# sample and sum up the cophenetic matrices for different clustering methods
	for (i in 1:trials) {
		result <- SamplingRun(path, ism.list, sample_size)
		coph.ave.total <- coph.ave.total + result[[2]]
		coph.comp.total <- coph.comp.total + result[[4]]
		coph.ward.total <- coph.ward.total + result[[6]]
	}
	
	# turn into average matrices
	coph.ave.total <- coph.ave.total / trials
	coph.comp.total <- coph.comp.total / trials
	coph.ward.total <- coph.ward.total / trials
	
	# write average matrices to file
	write.table(coph.ave.total, file = paste(path, "coph_matrix_sampled_ave.mtrx", sep = ""), sep = " ", row.names = F, col.names = F)
	write.table(coph.comp.total, file = paste(path, "coph_matrix_sampled_comp.mtrx", sep = ""), sep = " ", row.names = F, col.names = F)
	write.table(coph.ward.total, file = paste(path, "coph_matrix_sampled_ward.mtrx", sep = ""), sep = " ", row.names = F, col.names = F)
	
	# compute differences to all participant matrices
	diff.ave <- abs(as.matrix(cophenetic(ave.all)) - coph.ave.total)
	diff.comp <- abs(as.matrix(cophenetic(comp.all)) - coph.comp.total)
	diff.ward <- abs(as.matrix(cophenetic(ward.all)) - coph.ward.total)
	
	# print out average devigation per cell
	print (sum(diff.ave) / (nrow(d)^2))
	print (sum(diff.comp) / (nrow(d)^2))
	print (sum(diff.ward) / (nrow(d)^2))
}















# Perform complete sample experiments for deviations of clusterings resulting from different methods using Jaccard's index
# Parameters are:
# output.name: name of output file without extension
# ism.list: vector of .mtrx files from which to sample
# trials: number of runs averaged per sample size and cluster number
# sample.size.start: smallest sample size to be used
# sample.size.end: largest sample size to be used
# n.cluster.start: smallest number of clusters to used
# n.cluster.end: largest number of clusters to used
IndexSampling <- function(path, ism.list, output.name, trials, sample.size.start, sample.size.end, n.cluster.start, n.cluster.end) {
	
	# set up data frame for results
	log <- data.frame(col_names = c("cluster number", "sample size", "trial", "sample", "sim ave-comp jac", "sim ave-ward jac", "sim comp-ward jac", "sim avg diff jac", "sim ave-comp rand", "sim ave-ward rand", "sim comp-ward rand", "sim avg diff rand"), stringsAsFactors = FALSE)
	
	result.df.jac <- data.frame(row_names = c("sample size", c(sample.size.start:sample.size.end)), stringsAsFactors=FALSE)
	result.df.rand <- data.frame(row_names = c("sample size", c(sample.size.start:sample.size.end)), stringsAsFactors=FALSE)
	for (i in 1:(n.cluster.end-n.cluster.start+1)) {
		result.df.jac[1, ((i-1)*2)+2] <- paste("cluster=", n.cluster.start + i - 1, " avg", sep = "")
		result.df.jac[1, ((i-1)*2)+3] <- paste("cluster=", n.cluster.start + i - 1, " sd", sep = "")
		result.df.rand[1, ((i-1)*2)+2] <- paste("cluster=", n.cluster.start + i - 1, " avg", sep = "")
		result.df.rand[1, ((i-1)*2)+3] <- paste("cluster=", n.cluster.start + i - 1, " sd", sep = "")
	}
	
	# run experiments and enter average Jaccard similarity over all three cluster methods in the data frame
	count <- 1
	for (j in n.cluster.start:n.cluster.end) {
		for (l in sample.size.start:sample.size.end) {
			print(paste(((((j-n.cluster.start)*(sample.size.end-sample.size.start+1))+(l-sample.size.start+1)) / ((n.cluster.end-n.cluster.start+1) * (sample.size.end-sample.size.start+1)))*100,"% done" ))
			avg.jac <- 0 
			sq.jac <- 0
			avg.rand <- 0 
			sq.rand <- 0
			for (i in 1:trials) {
				result <- SamplingRun(path,ism.list,l)
				sim.ave.comp.jac <- cluster_similarity(cutree(result[[1]], k=j), cutree(result[[3]], k = j), similarity = c("jaccard"), method = "independence")
				sim.ave.ward.jac <- cluster_similarity(cutree(result[[1]], k=j), cutree(result[[5]], k = j), similarity = c("jaccard"), method = "independence")
				sim.comp.ward.jac <- cluster_similarity(cutree(result[[3]], k=j), cutree(result[[5]], k = j), similarity = c("jaccard"), method = "independence")
				sim.ave.comp.rand <- cluster_similarity(cutree(result[[1]], k=j), cutree(result[[3]], k = j), similarity = c("rand"), method = "independence")
				sim.ave.ward.rand <- cluster_similarity(cutree(result[[1]], k=j), cutree(result[[5]], k = j), similarity = c("rand"), method = "independence")
				sim.comp.ward.rand <- cluster_similarity(cutree(result[[3]], k=j), cutree(result[[5]], k = j), similarity = c("rand"), method = "independence")
				
				sim.avg.jac <- (sim.ave.comp.jac + sim.ave.ward.jac + sim.comp.ward.jac) / 3
				sim.avg.rand <- (sim.ave.comp.rand + sim.ave.ward.rand + sim.comp.ward.rand) / 3
				avg.jac <- avg.jac + sim.avg.jac
				sq.jac <- sq.jac + (sim.avg.jac)^2
				avg.rand <- avg.rand + sim.avg.rand
				sq.rand <- sq.rand + (sim.avg.rand)^2
				
				log[count, 1] <- j
				log[count, 2] <- l
				log[count, 3] <- i
				log[count, 4] <- paste(result[7], collapse = "")
				log[count, 5] <- sim.ave.comp.jac
				log[count, 6] <- sim.ave.ward.jac
				log[count, 7] <- sim.comp.ward.jac
				log[count, 8] <- sim.avg.jac
				log[count, 9] <- sim.ave.comp.rand
				log[count, 10] <- sim.ave.ward.rand
				log[count, 11] <- sim.comp.ward.rand
				log[count, 12] <- sim.avg.rand
				
				count <- count + 1
			}
			var.jac <- ((trials * sq.jac) - avg.jac^2) / (trials * (trials - 1))
			avg.jac <- avg.jac / trials
			
			var.rand <- ((trials * sq.rand) - avg.rand^2) / (trials * (trials - 1))
			avg.rand <- avg.rand / trials
			
			result.df.jac[l - sample.size.start + 2, (j - n.cluster.start) * 2 + 2] <- avg.jac
			result.df.jac[l - sample.size.start + 2, (j - n.cluster.start) * 2 + 3] <- sqrt(var.jac)
			result.df.rand[l - sample.size.start + 2, (j - n.cluster.start) * 2 + 2] <- avg.rand
			result.df.rand[l - sample.size.start + 2, (j - n.cluster.start) * 2 + 3] <- sqrt(var.rand)
		}
	}
	
	# write data frame to file
	write.table(result.df.jac, file = paste(path, output.name, "_jac.csv", sep = ""), sep = ",", row.names = F,  col.names = F)
	write.table(result.df.rand, file = paste(path, output.name, "_rand.csv", sep = ""), sep = ",", row.names = F,  col.names = F)
	write.table(log, file = paste(path, output.name, "_log.csv", sep = ""), sep = ",", row.names = T,  col.names = T)
	
	# produce plots
	cols <- c("blue", "green", "red", "brown", "yellow")
	
	pdf(file = paste(path, output.name, "_jac.pdf", sep = ""), onefile = T, width = 12, height = 4)
	# png(filename=paste(path, output.name, "_jac.png",sep=""), height=1600, width=1600, bg="white")
	for (i in 1:(n.cluster.end-n.cluster.start+1)) {
		if (i == 1) {
			par(mar = c(5.1, 4.1, 4.1, 5.1), xpd = TRUE)
			plot(result.df.jac[2:(sample.size.end-sample.size.start+2),2], type = "o", ylim = c(0, 1), col = cols[((i-1) %% length(cols)) + 1 ], pch = 21+i-1, lty = i, axes = FALSE, ann = FALSE, bty = 'L') 
			axis(1, at = 1:(sample.size.end-sample.size.start+1), labels = c(sample.size.start:sample.size.end))
			axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0"))
			box()
			title(main = "CMSI based on Jaccard coefficient", col = rgb(0, 0.5, 0))
		} else {
			lines(result.df.jac[2:(sample.size.end-sample.size.start+2), (i-1)*2+2], type="o", col = cols[((i-1) %% length(cols)) + 1 ], pch = 21+i-1, lty = i)
		}
	}
	legend("topright", inset = c(-0.08, 0), legend = c(n.cluster.start:n.cluster.end), cex = 0.8, col = cols, pch = 21:(21+(n.cluster.end-n.cluster.start)), lty = 1:(n.cluster.end-n.cluster.start+1))
	title(xlab = "Sample size", col.lab = rgb(0, 0.5, 0))
	title(ylab = "CMSI", col.lab = rgb(0, 0.5, 0))
	dev.off()
	
	pdf(file = paste(path, output.name, "_rand.pdf", sep = ""), onefile = T, width = 12, height = 4)
	#png(filename=paste(path, output.name, "_rand.png",sep=""), height=1600, width=1600, bg="white")
	for (i in 1:(n.cluster.end-n.cluster.start+1)) {
		if (i == 1) {
			par(mar = c(5.1, 4.1, 4.1,5.1), xpd = TRUE)
			plot(result.df.rand[2:(sample.size.end-sample.size.start+2), 2], type = "o", ylim = c(0, 1), col = cols[((i-1) %% length(cols)) + 1], pch = 21+i-1, lty = i, axes = FALSE, ann = FALSE) 
			axis(1, at = 1:(sample.size.end-sample.size.start+1), labels = c(sample.size.start:sample.size.end))
			axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0"))
			box()
			title(main = "CMSI based on Rand coefficient", col = rgb(0, 0.5, 0))
		} else {
			lines(result.df.rand[2:(sample.size.end-sample.size.start+2), (i-1)*2+2], type = "o", col = cols[((i-1) %% length(cols)) + 1], pch = 21+i-1, lty = i)
		}
	}
	legend("topright", inset = c(-0.08,0), legend = c(n.cluster.start:n.cluster.end), cex = 0.8, col = cols, pch = 21:(21+(n.cluster.end-n.cluster.start)), lty = 1:(n.cluster.end-n.cluster.start+1))
	title(xlab = "Sample size", col.lab = rgb(0, 0.5, 0))
	title(ylab = "CMSI", col.lab = rgb(0, 0.5, 0))
	dev.off()
}















# Participant similarity analysis
ParticipantSimilarityClusters <- function(path) {
	dm <- as.matrix(read.table(file = paste(path, "participant_similarity_hamming.csv", sep = ""), sep = " ",
			header = T))
	
	dm.jac <- as.matrix(read.table(file = paste(path, "participant_similarity_jaccard.csv", sep = ""), sep = " ",
			header = T))
	
	dm.rand <- as.matrix(read.table(file = paste(path, "participant_similarity_rand.csv", sep = ""), sep = " ",
			header = T))
	
	# Perform cluster analysis based on participant similarity matrix using Ward's method and construct a dendrogram
	cluster <- hclust(method = "ward", as.dist(dm))
	cluster.jac <- hclust(method = "ward", as.dist(dm.jac))
	cluster.rand <- hclust(method = "ward", as.dist(dm.rand))
	dend <- as.dendrogram(cluster)
	dend.jac <- as.dendrogram(cluster.jac)
	dend.rand <- as.dendrogram(cluster.rand)
	
	#Create overview table showing cluster membership for all possible numbers of clusters
	tree <- cutree(cluster, k = c(1:nrow(dm)))
	write.csv(tree, file=paste(path, "participant_similarity_ward_clusters", ".csv", sep = ""))
}















# Participant similarity analysis
ParticipantSimilarityVisualizations <- function(path) {
	
	dm <- as.matrix(read.table(file = paste(path, "participant_similarity_hamming.csv", sep = ""), sep = " ",
					header = T))
	
	dm.jac <- as.matrix(read.table(file = paste(path, "participant_similarity_jaccard.csv", sep = ""), sep = " ",
					header = T))
	
	dm.rand <- as.matrix(read.table(file = paste(path, "participant_similarity_rand.csv", sep = ""), sep = " ",
					header = T))
	
	# Perform cluster analysis based on participant similarity matrix using Ward's method and construct a dendrogram
	cluster <- hclust(method = "ward", as.dist(dm))
	cluster.jac <- hclust(method = "ward", as.dist(dm.jac))
	cluster.rand <- hclust(method = "ward", as.dist(dm.rand))
	dend <- as.dendrogram(cluster)
	dend.jac <- as.dendrogram(cluster.jac)
	dend.rand <- as.dendrogram(cluster.rand)
	
	# Export the dendrogram as a pdf file
	pdf(file = paste(path, "participant_similarity.pdf", sep = ""), onefile = T, width = 12, height = 4)
	#tiff(filename = paste(path, "participant_similarity.tiff", sep =""),
	#		width = 4000, height=4000, units="px",
	#		pointsize=5, compression = "none", bg = "white", res = 400)
	plot(dend)
	dev.off()
	
	pdf(file = paste(path, "participant_similarity_jac.pdf", sep = ""), onefile = T, width = 12, height = 4)
	#tiff(filename = paste(path, "participant_similarity_jac.tiff", sep =""),
	#		width = 4000, height=4000, units="px",
	#		pointsize=5, compression = "none", bg = "white", res = 400)
	plot(dend.jac)
	dev.off()
	
	pdf(file = paste(path, "participant_similarity_rand.pdf", sep = ""), onefile = T, width = 12, height = 4)
	#tiff(filename = paste(path, "participant_similarity_rand.tiff", sep =""),
	#		width = 4000, height=4000, units="px",
	#		pointsize=5, compression = "none", bg = "white", res = 400)
	plot(dend.rand)
	dev.off()
  
	# Create a cluster heatmap for participant similarities
	png(filename = paste(path, "HM-Clust-PartSimHam.png", sep = ""), width = 2000, height = 2000, units = "px",
	    pointsize = 5, bg = "white", res = 600)
	heatmap.2(as.matrix(dm), col=cm.colors(255), Rowv = dend, Colv = dend, 
	          margin = c(3,3), cexRow = 0.5, cexCol = 0.5, dendrogram = "both", 
	          revC = TRUE, trace = "none", key = TRUE)
	dev.off()
  
	# Generate the dendrogram using wards method for
	# Participant similarity using Jaccard coefficient
	dend <- as.dendrogram(cluster.jac)
	# Create a cluster heatmap for participant similarities
	png(filename = paste(path, "HM-Clust-PartSimJac.png", sep = ""), width = 2000, height = 2000, units = "px",
	    pointsize = 5, bg = "white", res = 600)
	heatmap.2(as.matrix(dm.jac), Rowv = dend, Colv = dend, 
	          margin = c(3, 3), cexRow = 0.5, cexCol = 0.5, dendrogram = "both", 
	          revC = TRUE, trace = "none", key = TRUE)
	dev.off()
}















# Numerical cluster validation
# cluster validation is accomplished by comparing cluster membership
# for a certain number of clusters across different clustering methods (ave, comp, ward)
# Parameters: path of experiment and k (maximum number of clusters)
# There could be an issue as cluster membership is a number that may not be the same
# across cluster method!!!
NumClusVal <- function(path, k) {
	# read in matrix and column/row names
	d <- read.csv(paste(path, "osm.csv", sep = ""), header = F)
	dm <- as.matrix(d[, -1])
	dimnames(dm) <- list(d[, 1], d[, 1])
	
	# Participants minus osm generates dissimilarity
	ave <- hclust(method = "average", as.dist(ParticipantCounter(path) - dm))
	comp <- hclust(method = "complete", as.dist(ParticipantCounter(path) - dm))
	ward <- hclust(method = "ward", as.dist(ParticipantCounter(path) - dm))
	# cluster validation
	cut.Results <- data.frame() # create empty data frame
	for (i in 2:k) {
		cut.ave <- as.data.frame(cutree(ave, i))
		cut.comp <- as.data.frame(cutree(comp, i))
		cut.ward <- as.data.frame(cutree(ward, i))
		cut.Results <- as.data.frame(cbind(cut.ave[, 1], cut.comp[, 1], cut.ward[, 1]))
		colnames(cut.Results) <- c(paste("ave", sep = ""), paste("comp", sep = ""), paste("ward", sep = ""))
		cut.Results$Equal[cut.Results$ave == cut.Results$comp & cut.Results$comp == cut.Results$ward] <- "Equal"
		cut.Results$Equal[cut.Results$ave != cut.Results$comp | cut.Results$comp != cut.Results$ward] <- "Dif"
		rownames(cut.Results) <- rownames(cut.ave)
		write.csv(cut.Results, file = paste(path, "cluVal", i, ".csv", sep = ""))
	} 
}















# print standard dendrograms
# Author: Alexander Klippel
# input variable: path
# OSM needs to be present
StanDen <- function(path) {
  d <- read.csv(paste(path, "osm.csv", sep = ""), header = F)
  dm <- as.matrix(d[, -1])
  dimnames(dm) <- list(d[, 1], d[, 1])
  clu.meth <- c("ave", "comp", "ward.D")
  for (i in clu.meth) {
    # ALTERNATIVE 1
      dummy <- hclust(method = i, as.dist(ParticipantCounter(path) - dm))
      png(file = paste(path, "dendro", i, ".png", sep=""), width = 1200, height = 1200, pointsize = 12)
      plot(dummy)
    # ALTERNATIVE 2
#     dummy = as.dendrogram(hclust(method = i, as.dist(ParticipantCounter(path) - dm)))
#     png(file = paste(path, "dendro", i, ".png", sep=""), width = 1400, height = 1200)
#     plot(dummy, type = "triangle", nodePar = list(pch = 10:1, cex = .5*4:1, col = 2:3),
#          edgePar = list(col = 1:2, lty = 2:3), 
#          horiz = TRUE, 
#          #center = FALSE, dLeaf = -2, edge.root = FALSE
#     )
    myTitle <- paste(scenario.name, i, sep="//")
    title(main = myTitle)
    dev.off()
    }
  }
# StanDen(path)















# ploting individual "heatmaps" as black/white images
# Author: Alexander Klippel
# input: path
# output: results for each participant are stored in folder 'indISM'
# required package:
VisIndIsm <- function(path) {
  # read in all ISMs and store as a list
  indISM <- as.list(list.files(paste(path, "ism/", sep = "")))
  dir.create(paste(path, "indISM/", sep = ""))
  indISM.Path <- paste(path, "indISM/", sep = "")
  #iterate through the list and plot each matrix using 'grid.raster'
  #individusal images are stored as png files
  for (i in indISM) {
    indISM.matrix <- read.delim(paste(path, "ism/", i, sep = ""), header = FALSE, sep = " ", stringsAsFactors = F)
    indISM.matrix <- data.matrix(indISM.matrix)
    png(paste(indISM.Path, i, ".png", sep = ""), width = 480, height = 480)
    grid.raster(as.raster(indISM.matrix), interpolate = FALSE)
    dev.off()
  }
}
# VisIndIsm(path)















# Not finished
# ploting reordered individual "heatmaps"
# read in all ISMs and store as a list
# indISM <- as.list(list.files(paste(path,"ism/",sep="")))
# dir.create(paste(path, "indISM-reordered/", sep=""))
# indISM.Path <- paste(path, "indISM-reordered/", sep="")
# #read in file names with new order
# my.names <- read.csv((paste(path, "newNameOrder.csv", sep = "")), header = TRUE)
# #iterate through the list and plot each matrix using 'grid.raster'
# #individusal images are stored as png files
# for (i in indISM) {
#   indISM.matrix <- read.delim(paste(path, "ism/", i, sep = ""), header = FALSE, sep = " ", stringsAsFactors = F)
#   indISM.matrix <- data.matrix(indISM.matrix)
#   colnames(indISM.matrix) <- my.names$new
#   rownames(indISM.matrix) <- my.names$new
#   new.indISM.matrix <- indISM.matrix[sort(rownames(indISM.matrix)),sort(colnames(indISM.matrix)), drop = F]
#   png(paste(indISM.Path, i, ".png", sep = ""), width = 480, height = 480)
#   grid.raster(as.raster(new.indISM.matrix), interpolate = FALSE)
#   dev.off()
# }















# Visualizing participant similarities by groups
# Author: Alexander Klippel
# Input: path, number of participants (np), number of clusters (k)
# TODO: np needs to be set manually at the moment!
PartSimGroupVis <- function(path, np, k) {
  # List all ISMs
  isms <- list.files(paste(path, "ism/", sep = ""))
  all.isms <- list()
  
  # Read in all ISMs and store them in a list named all.isms
  for (i in 1:length(isms)) {
    aism <- read.delim(paste(paste(path, "ism/", sep = ""), isms[i], sep = ""),
                       header = F, sep = " ", stringsAsFactors = F)
    all.isms <- c(all.isms, list(aism))
  }
  
  # Calculate participant similarity matrix (dm) of all pairs of partcipants based on the hamming distance of their ISMs
  dm <- matrix(0, ncol = np, nrow = np)
  for (i in 1: np) {
    for (j in 1: np) {
      dm[i, j] <- sum(abs(all.isms[[i]] - all.isms[[j]]))
    }
  }
  
  # Extract the participant number of all participants and store them in a vector named names
  names <- c()
  for (i in 1: length(isms)) {
    name <- isms[i]
    names <- append(names, substr(name, 12, nchar(name) - 5))
  }
  
  # Assign participants numbers as the row&column names of the participant similarity matrix (dm)
  colnames(dm) <- names
  rownames(dm) <- names
  
  # Perform cluster analysis based on participant similarity matrix using Ward's method and construct a dendrogram
  ward.P <- hclust(method = "ward", as.dist(dm))
  clus.mem <- cutree(ward.P, k)
  clus.mem <- as.data.frame(clus.mem)
  
  # store images of clusters in corresponding html files
  for (i in 1:k) {
    # select one cluster and store all the members in cluster.members
    cluster.members <- subset(clus.mem, (clus.mem %in% c(i))) 
    # Store all the row names (image names) of that cluster in iconNames
    part.names <- rownames(cluster.members)
    # define output file using the cluster number as a name variable
    output <- paste(k, "_partClus", i, ".html", sep = "")
    HTML.output<-file.path(path, output)
    # specify where the icons/images are located at
    icon.path <- paste(path, "indISM/", sep = "")
    # write all the images/icons of one cluster into the html file
    # MyHTMLInsertGraph is necessary as there is no parameter to switch off the line break
    for (i in part.names) {
      MyHTMLInsertGraph(paste(icon.path, "participant", i, ".mtrx", ".png", sep = ""), file = HTML.output, caption = i)
    }
  }
  
}















# Comparing results from 2 experiments / 2 OSMs
# Here: Substracting two OSMs from one another and visualizing the difference
# Author: Alexander Klippel
Dif2Osm <- function(path1,path2) {
  # load first OSM
  d1 <- read.csv(paste(path1, "osm.csv", sep = ""), header = F)
  dm1 <- as.matrix(d1[, -1])
  
  # load second OSM
  d2 <- read.csv(paste(path2, "osm.csv", sep = ""), header = F)
  dm2 <- as.matrix(d2[, -1])
  
  # substract the two OSMs
  dm.diff <- dm1 - dm2
  dimnames(dm.diff) <- list(d1[, 1], d1[, 1])
  # Output results
  tiff(filename = paste(path1, "heatDif.tiff", sep = ""), width = 2000, height = 2000, units = "px",
       pointsize = 5, compression = "none", bg = "white", res = 600)
  heatmap.2(as.matrix(dm.diff), Rowv = F, Colv = "Rowv", dendrogram = "none", 
            margin = c(3, 3), cexRow = 0.6, cexCol = 0.6, revC = F, trace = "none", key = TRUE)
  dev.off()
  min(dm.diff)
  #max(dm.diff)
}















# Function list Pt. 2 - Executables
#####################################################################################################
#####################################################################################################
#####################################################################################################


# generate output file for icon viewer containing mds results and prototype frequencies
mdsc <- cbind(mds, prototypes[3])
write.table(mdsc, file = paste(path, "mds.txt", sep = ""), sep = " ", quote = FALSE,
		row.names = T, col.names = T)

###Change the number here to create colored-dendrograms at different solutions
for(i in 2: max.cluster) {
	DetailedClusterAnalysis(path, i, scenario.name)
}

StanDen(path) # error
# Error in hclust(method = i, as.dist(ParticipantCounter(path) - dm)) : 
#   invalid clustering method

VisIndIsm(path) # requires the "clusterval" package 

CopheneticSampling(path, list.files(paste(path, "ism/", sep = "")), 100, 20)

IndexSampling(path, list.files(paste(path, "ism/", sep = "")), "CMSI-2500", 100, 5, 100, 2, 10) # requires the "clusterval" package


path1 <- "E:/My Documents/Dropbox/qstr_collaboration/Catscan experiments/Experiments/1209 mturk directions 3D mugs final 225deg/"
path2 <- path <- "E:/My Documents/Dropbox/qstr_collaboration/Catscan experiments/Experiments/1208 mturk directions 3D mugs final 0deg/"
Dif2Osm(path1, path2)