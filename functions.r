






#install.packages("tm")
require(tm)
#install.packages("wordcloud")
require(wordcloud)
#install.packages("SnowballC")
require(SnowballC)
#install.packages("RColorBrewer")
require(RColorBrewer)






# Creates a sample of participants of size sample.size and computes the hclust objects and cophenetic matrices for this sample.
# The return value is a 6 element list (hclust_ave,coph.ave,hclust_comp,coph.comp,hclust_ward,coph.ward)
SamplingRun <- function(path, osm.path, ism.list, sample.size) {
  d <- read.csv(osm.path, header = F)
  
  # Construct the zip folder path and list all zip files
  ism.path <- paste(path, "ism/", sep = "")
  files <- ism.list
  
  # Initialize OSM
  osm <- matrix(0, nrow(d), nrow(d))
  
  # create random sample numbers
  r <- sample(length(ism.list), size=sample.size, replace=FALSE)
  
  # create OSM for sample
  for (i in 1:sample.size) {
    matrix.i.name <- files[r[i]]
    # print(paste("reading",matrix.i.name))
    
    # Read in the ISM from a participant and exclude the non-ism info from the .mtrx file
    matrix.i <- read.delim(paste(ism.path, matrix.i.name, sep=""), header = F, sep = " ", stringsAsFactors = F)
    matrix.i <- data.matrix(matrix.i[1:nrow(d), ])
    
    osm <- osm + matrix.i
  }
  
  # create hclust objects for different methods
  ave <- hclust(method = "average", as.dist(sample.size - osm))
  comp <- hclust(method = "complete", as.dist(sample.size - osm))
  ward <- hclust(method = "ward", as.dist(sample.size - osm))
  
  # compute cophenetic matrices
  coph.ave <- as.matrix(cophenetic(ave)) 
  coph.comp <- as.matrix(cophenetic(comp)) 
  coph.ward <- as.matrix(cophenetic(ward)) 
  
  return(list(ave, coph.ave, comp, coph.comp, ward, coph.ward, r))
}





# Perform a complete sampling experiment in which an average cophenetic matrix for samples of participants is compared
# to that of the entire set of participnats. The number of trials is given by paramter 'trials' and a sample size 
# of 'sample.size' 
CopheneticSampling <- function(path, osm.path, ism.list, trials, sample.size) {
  # read overall osm
  d <- read.csv(osm.path, header = F)
  dm <- as.matrix(d[, -1])
  dimnames(dm) <- list(d[, 1], d[, 1])
  
  # derive cophenetic matrices for all participants
  ave.all <- hclust(method = "average", as.dist(ParticipantCounter(path) - dm))
  comp.all <- hclust(method = "complete", as.dist(ParticipantCounter(path) - dm))
  ward.all <- hclust(method = "ward", as.dist(ParticipantCounter(path) - dm))
  
  coph.ave.total <- matrix(0, nrow(d), nrow(d))
  coph.comp.total <- matrix(0, nrow(d), nrow(d))
  coph.ward.total <- matrix(0, nrow(d), nrow(d))
  
  # sample and sum up the cophenetic matrices for different clustering methods
  for (i in 1:trials) {
    result <- SamplingRun(path, osm.path, ism.list, sample.size)
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
  for (i in 1:(n.cluster.end-n.cluster.start + 1)) {
    result.df.jac[1, ((i - 1) * 2) + 2] <- paste("cluster=", n.cluster.start + i - 1, " avg", sep = "")
    result.df.jac[1, ((i - 1) * 2) + 3] <- paste("cluster=", n.cluster.start + i - 1, " sd", sep = "")
    result.df.rand[1, ((i - 1) * 2) + 2] <- paste("cluster=", n.cluster.start + i - 1, " avg", sep = "")
    result.df.rand[1, ((i - 1) * 2) + 3] <- paste("cluster=", n.cluster.start + i - 1, " sd", sep = "")

  }
  
  # run experiments and enter average Jaccard similarity over all three cluster methods in the data frame
  count <- 1
  for (j in n.cluster.start:n.cluster.end) {
    for (l in sample.size.start:sample.size.end) {
      print(paste(((((j - n.cluster.start) * (sample.size.end - sample.size.start + 1)) + (l - sample.size.start + 1)) / ((n.cluster.end - n.cluster.start + 1) * (sample.size.end - sample.size.start + 1))) * 100, "% done" ))
      avg.jac <- 0 
      sq.jac <- 0
      avg.rand <- 0 
      sq.rand <- 0
      for (i in 1:trials) {
        result <- SamplingRun(path,ism.list, l)
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
  for (i in 1:(n.cluster.end - n.cluster.start + 1)) {
    if (i == 1) {
      par(mar = c(5.1, 4.1, 4.1, 5.1), xpd = TRUE)
      plot(result.df.jac[2:(sample.size.end - sample.size.start + 2), 2], type = "o", ylim = c(0, 1), col = cols[((i - 1) %% length(cols)) + 1], pch = 21 + i - 1, lty = i, axes = FALSE, ann = FALSE, bty = 'L') 
      axis(1, at = 1:(sample.size.end - sample.size.start + 1), labels = c(sample.size.start:sample.size.end))
      axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0"))
      box()
      title(main = "CMSI based on Jaccard coefficient", col = rgb(0, 0.5, 0))
    } else {
      lines(result.df.jac[2:(sample.size.end - sample.size.start + 2), (i - 1) * 2 + 2], type="o", col = cols[((i - 1) %% length(cols)) + 1], pch = 21 + i - 1, lty = i)
    }

  }

  legend("topright", inset = c(-0.08, 0), legend = c(n.cluster.start:n.cluster.end), cex = 0.8, col = cols, pch = 21:(21 + (n.cluster.end - n.cluster.start)), lty = 1:(n.cluster.end - n.cluster.start + 1))
  title(xlab = "Sample size", col.lab = rgb(0, 0.5, 0))
  title(ylab = "CMSI", col.lab = rgb(0, 0.5, 0))
  dev.off()
  
  pdf(file = paste(path, output.name, "_rand.pdf", sep = ""), onefile = T, width = 12, height = 4)
  #png(filename=paste(path, output.name, "_rand.png",sep=""), height=1600, width=1600, bg="white")
  for (i in 1:(n.cluster.end - n.cluster.start + 1)) {
    if (i == 1) {
      par(mar = c(5.1, 4.1, 4.1,5.1), xpd = TRUE)
      plot(result.df.rand[2:(sample.size.end - sample.size.start + 2), 2], type = "o", ylim = c(0, 1), col = cols[((i - 1) %% length(cols)) + 1], pch = 21 + i - 1, lty = i, axes = FALSE, ann = FALSE) 
      axis(1, at = 1:(sample.size.end-sample.size.start + 1), labels = c(sample.size.start:sample.size.end))
      axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0"))
      box()
      title(main = "CMSI based on Rand coefficient", col = rgb(0, 0.5, 0))
    } else {
      lines(result.df.rand[2:(sample.size.end - sample.size.start + 2), (i - 1) * 2 + 2], type = "o", col = cols[((i - 1) %% length(cols)) + 1], pch = 21 + i - 1, lty = i)
    }

  }

  legend("topright", inset = c(-0.08, 0), legend = c(n.cluster.start:n.cluster.end), cex = 0.8, col = cols, pch = 21:(21 + (n.cluster.end - n.cluster.start)), lty = 1:(n.cluster.end - n.cluster.start + 1))
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
  #   width = 4000, height=4000, units="px",
  #   pointsize=5, compression = "none", bg = "white", res = 400)
  plot(dend)
  dev.off()
  
  pdf(file = paste(path, "participant_similarity_jac.pdf", sep = ""), onefile = T, width = 12, height = 4)
  #tiff(filename = paste(path, "participant_similarity_jac.tiff", sep =""),
  #   width = 4000, height=4000, units="px",
  #   pointsize=5, compression = "none", bg = "white", res = 400)
  plot(dend.jac)
  dev.off()
  
  pdf(file = paste(path, "participant_similarity_rand.pdf", sep = ""), onefile = T, width = 12, height = 4)
  #tiff(filename = paste(path, "participant_similarity_rand.tiff", sep =""),
  #   width = 4000, height=4000, units="px",
  #   pointsize=5, compression = "none", bg = "white", res = 400)
  plot(dend.rand)
  dev.off()
  
  # Create a cluster heatmap for participant similarities
  jpeg(filename = paste(path, "HM-Clust-PartSimHam.jpeg", sep = ""), width = 2000, height = 2000, units = "px",
      pointsize = 5, bg = "white", res = 600)
  heatmap.2(as.matrix(dm), col=cm.colors(255), Rowv = dend, Colv = dend, 
            margin = c(3,3), cexRow = 0.5, cexCol = 0.5, dendrogram = "both", 
            revC = TRUE, trace = "none", key = TRUE)
  dev.off()
  
  # Generate the dendrogram using wards method for
  # Participant similarity using Jaccard coefficient
  dend <- as.dendrogram(cluster.jac)
  # Create a cluster heatmap for participant similarities
  jpeg(filename = paste(path, "HM-Clust-PartSimJac.jpeg", sep = ""), width = 2000, height = 2000, units = "px",
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
NumClusVal <- function(path, osm.path, k) {
  # read in matrix and column/row names
  d <- read.csv(osm.path, header = F)
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
StanDen <- function(path, osm.path) {
  d <- read.csv(osm.path, header = F)
  dm <- as.matrix(d[, -1])
  dimnames(dm) <- list(d[, 1], d[, 1])
  clu.meth <- c("ave", "comp", "ward.D")
  for (i in clu.meth) {
    # ALTERNATIVE 1
      dummy <- hclust(method = i, as.dist(ParticipantCounter(path) - dm))
      jpeg(file = paste(path, "dendro", i, ".jpeg", sep=""), width = 1200, height = 1200, pointsize = 12)
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
    jpeg(paste(indISM.Path, i, ".jpeg", sep = ""), width = 480, height = 480)
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
      MyHTMLInsertGraph(paste(icon.path, "participant", i, ".mtrx", ".jpeg", sep = ""), file = HTML.output, caption = i)
    }
  }
  
}




# Comparing results from 2 experiments / 2 OSMs
# Here: Substracting two OSMs from one another and visualizing the difference
# Author: Alexander Klippel
Dif2Osm <- function(osm1.path, osm2.path) {
  # load first OSM
  d1 <- read.csv(osm1.path, header = F)
  dm1 <- as.matrix(d1[, -1])
  
  # load second OSM
  d2 <- read.csv(osm2.path, header = F)
  dm2 <- as.matrix(d2[, -1])
  
  # substract the two OSMs
  dm.diff <- dm1 - dm2
  dimnames(dm.diff) <- list(d1[, 1], d1[, 1])
  # Output results
  jpeg(filename = paste(substr(osm1.path, 1, nchar(osm1.path)-7), "heatDif.jpeg", sep = ""), width = 2000, height = 2000, units = "px",
       pointsize = 5, compression = "none", bg = "white", res = 600)
  heatmap.2(as.matrix(dm.diff), Rowv = F, Colv = "Rowv", dendrogram = "none", 
            margin = c(3, 3), cexRow = 0.6, cexCol = 0.6, revC = F, trace = "none", key = TRUE)
  dev.off()
  min(dm.diff)
  #max(dm.diff)
}




# Detailed cluster analysis
DetailedClusterAnalysis <- function(path, osm.path, k, title = "") {
  d <- read.csv(osm.path, header = F)
  dm <- as.matrix(d[, -1])
  dimnames(dm) <- list(d[, 1], d[, 1])
  # Old code: dm = as.matrix(d)
  # Jinlong: I'm pretty sure the code above won't work for this function
  
  # Participants minus osm generates dissimilarity
  ave <- hclust(method = "average", as.dist(ParticipantCounter(path) - dm))
  comp <- hclust(method = "complete", as.dist(ParticipantCounter(path) - dm))
  ward <- hclust(method = "ward", as.dist(ParticipantCounter(path) - dm))
  
  # load code of A2R function
  # Explain what this function is doing!
  source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
  
  # Create a color scheme from rainbow color scheme
  pre.colors <- rainbow(k)
  colors <- pre.colors[1: k]
  
  pdf(file = paste(path, "dendrograms_", k, "_cluster.pdf", sep=""),
      width = 6, height = 2.5, 
      bg = "white", pointsize = 0.5)
  
  A2Rplot(ave, k = k, boxes = F, col.up = "gray50", col.down = colors, 
      main = paste(title, " Average Linkage ", k, " clusters", sep = ""))
  A2Rplot(comp, k = k, boxes = F, col.up = "gray50", col.down = colors, 
      main = paste(title, " Complete Linkage ", k, " clusters", sep = ""))
  A2Rplot(ward, k = k, boxes = F, col.up = "gray50", col.down = colors, 
      main = paste(title, " Ward's Method ", k, " clusters", sep = ""))
  
  dev.off()
  
}














#Executables
#####################################################################################################
#####################################################################################################
#####################################################################################################

osm.path <- "C:/Users/Sparks/Google Drive/Alex/R_PackageCreation/catLibTests/osm.csv"

# generate output file for icon viewer containing mds results and prototype frequencies
mdsc <- cbind(mds, prototypes[3])
write.table(mdsc, file = paste(path, "mds.txt", sep = ""), sep = " ", quote = FALSE,
    row.names = T, col.names = T)

###Change the number here to create colored-dendrograms at different solutions
for(i in 2: max.cluster) {
  DetailedClusterAnalysis(path, osm.path, i, scenario.name)
}

StanDen(path, osm.path) 

VisIndIsm(path) 

CopheneticSampling(path, osm.path, list.files(paste(path, "ism/", sep = "")), 100, 20)

# Error in sample.int(x, size, replace, prob) : 
  # cannot take a sample larger than the population when 'replace = FALSE'
IndexSampling(path, list.files(paste(path, "ism/", sep = "")), "CMSI-2500", 100, 5, 100, 2, 10) # sluggish

# Can someone email me these files? Or dropbox me these files?
## NOTE THESE SHOULD NO LONGER BE PATHS TO DIRECTORIES, BUT PATHS TO EACH OSM FILE
path1 <- "E:/My Documents/Dropbox/qstr_collaboration/Catscan experiments/Experiments/1209 mturk directions 3D mugs final 225deg/"
path2 <- path <- "E:/My Documents/Dropbox/qstr_collaboration/Catscan experiments/Experiments/1208 mturk directions 3D mugs final 0deg/"
Dif2Osm(path1, path2)
#####################################################################################################
#####################################################################################################
#####################################################################################################








#Other functions






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







###################################################################
# print standard dendrograms
# Author: Alexander Klippel
# input variable: path
# OSM needs to be present
stanDen <- function(path)
{
  d <- read.csv(paste(path, "osm.csv", sep = ""), header = F)
  dm <- as.matrix(d[, -1])
  dimnames(dm) <- list(d[, 1],d[, 1])
  clu.meth = c("ave", "comp", "ward.D")
  for (i in clu.meth)
  {
    ##ALTERNATIVE 1
    dummy = hclust(method = i, as.dist(participant_counter(path) - dm))
    png(file = paste(path, "dendro", i, ".png", sep=""), width = 1200, height = 1200, pointsize = 12)
    plot(dummy)
    ##ALTERNATIVE 2
    #     dummy = as.dendrogram(hclust(method = i, as.dist(participant_counter(path) - dm)))
    #     png(file = paste(path, "dendro", i, ".png", sep=""), width = 1400, height = 1200)
    #     plot(dummy, type = "triangle", nodePar = list(pch = 10:1, cex = .5*4:1, col = 2:3),
    #          edgePar = list(col = 1:2, lty = 2:3), 
    #          horiz = TRUE, 
    #          #center = FALSE, dLeaf = -2, edge.root = FALSE
    #     )
    myTitle = paste(scenario_name, i, sep="//")
    title(main = myTitle)
    dev.off()
  }
}






































































## Calculate mean and variance for OSM rows
## Visualize row values

# Clear the workspace
# rm(list=ls())

# Set path
# path <- "E:/My Documents/Dropbox/qstr_collaboration/Catscan experiments/Experiments/2601 mturk ultras worldwide/"


# Visualize row values for the entire OSM
# VisRowValues <- function(path) {
#   # Visualizes row / column values of OSM as barplots
#   # Args:
#   # n/a
#   # Return:
#   # n/a

#   # Read in OSM
#   myOSM <- read.csv(paste(path, "osm.csv", sep = ""),header = F)
#   myOSM <- as.data.frame(myOSM)

#   ## Calculate the mean and variance for each row. Store in data frame.
#   myOSM$Mean <- apply(myOSM[,-1],1,mean,na.rm=TRUE) #mean for each row, add to new column "Mean"
#   # WARNING: Number of rows / columns is not assessed automatically!!
#   myOSM$Variance <- apply(myOSM[2:73],1,var,na.rm=TRUE)

#   # Icon names are in row 1. They are stored in myLabels
#   myLabels <- as.vector(myOSM[,1])
#   myMean <- round(myOSM[,74], 2)
#   myVar <- round(myOSM[,75], 2)

#   png(file = paste(path, "groupFreq_test", ".png", sep=""), width = 3000, height = 3200, pointsize = 12)
#   par(mfrow=c(9,8))
#   for(i in 2:73)
#   {
#     lab <- i-1
#     subT = paste(myMean[lab], myVar[lab], sep = '//')
#     barplot(myOSM[,i], main = paste(myLabels[lab], subT, sep = ': '), cex.main = 1.5 )
#   }
#   dev.off()

#   # Visualize row data for each row
#   for (i in 2:73){
#     lab <- i-1
#     png(file = paste(path, myLabels[lab], "_RF", ".png", sep=""),
#         width = 300, height = 300, pointsize = 10)
#     subT = paste(myMean[lab], myVar[lab], sep = '//')
#     barplot(myOSM[,i], main = paste(myLabels[lab], subT, sep = ': '), cex.main = 1.5 )
#     dev.off()
#   }

# }
# VisRowValues(path)










require(R2HTML)



# Visualize row values for the entire OSM
# MeanVarianceGraphics: visualize row values for the entire OSM
# Parameters
# osm: matrix, the osm matrix created by OsmGenerator
# icon.names: character vector, a list of the names of the icons created by IconListGetter
# icons.path: string, path to icons directory that holds the experiment icons (usually a subdirectory of the experiment directory)
MeanVarianceGraphics <- function(Osm, icon.names, icons.path) {

  # create a folder to hold individual graphics 
  graphics.path <- paste(getwd(), "MeanVarianceGraphics/", sep = "/")
  dir.create(graphics.path, showWarnings = FALSE)

  myOSM <- as.data.frame(Osm)

  ## Calculate the mean and variance for each row. Store in data frame.
  myOSM$Mean <- apply(myOSM[1:nrow(Osm)],1,mean,na.rm=TRUE) #mean for each row, add to new column "Mean"
  myOSM$Variance <- apply(myOSM[1:nrow(Osm)],1,var,na.rm=TRUE)

  # Icon names are in row 1. They are stored in myLabels
  myLabels <- as.vector(icon.names)
  myMean <- round(myOSM[,nrow(Osm)+1], 2)
  myVar <- round(myOSM[,nrow(Osm)+2], 2)

  # Visualize row data for each row
  for (i in 1:nrow(Osm)) {
    lab <- i
    png(file = paste(graphics.path, myLabels[lab], "_RF", ".png", sep=""),
        width = 300, height = 300, pointsize = 10)
    subT = paste(myMean[lab], myVar[lab], sep = '//')
    barplot(myOSM[,i], main = paste(myLabels[lab], subT, sep = ': '), cex.main = 1.5 )
    dev.off()

  }

  # Create bar plots for the lower quartile
  my25 <- quantile(myVar, .25)
  distinct25 <- c()
  png(file = paste(graphics.path, "groupFreq_25", ".png", sep=""), width = 1200, height = 1200, pointsize = 12)
  # Figures out the proper "par" parameters by calculating how many icons would be in a quartile, then taking the square root to determine how large the window needs to be 
  par(mfrow=c(ceiling((nrow(Osm)/4)^(1/2)),ceiling((nrow(Osm)/4)^(1/2))))
  for(i in 1:nrow(Osm)) {
    lab <- i
    if (myVar[lab] < my25) {
      distinct25 <- append(distinct25, myLabels[lab])
      subT = paste(myMean[lab], myVar[lab], sep = '//')
      barplot(myOSM[,i], main = paste(myLabels[lab], subT, sep = ': '), cex.main = 1.5 )
    }
  }
  dev.off()

  # Create bar plots for upper quartile
  # Set upper quartile
  my75 <- quantile(myVar, .75)
  distinct75 <- c()
  png(file = paste(graphics.path, "groupFreq_75", ".png", sep=""), width = 1200, height = 1200, pointsize = 12)
  # Figures out the proper "par" parameters by calculating how many icons would be in a quartile, then taking the square root to determine how large the window needs to be 
  par(mfrow=c(ceiling((nrow(Osm)/4)^(1/2)),ceiling((nrow(Osm)/4)^(1/2))))
  for(i in 1:nrow(Osm)) {
    lab <- i
    if (myVar[lab] > my75) {
      distinct75 <- append(distinct75, myLabels[lab])
      subT = paste(myMean[lab], myVar[lab], sep = '//')
      barplot(myOSM[,i], main = paste(myLabels[lab], subT, sep = ': '), cex.main = 1.5 )
    }
  }
  dev.off()


  # Find out what the extension is for the icons (i.e. ".png", ".jpeg", ".gif")
  icon.names <- list.files(icons.path)
  icon.extension <- strsplit(icon.names[1], "\\.")[[1]][2]
  icon.extension <- paste(".", icon.extension, sep="")

  # Make HMTL file with lower and upper quantile images
  #define output file
  output <- "LowerQuantiles25.html"
  HTMLoutput=file.path(graphics.path, output)
  for (i in distinct25) {
    HTMLInsertGraph(GraphFileName= paste(icons.path, i, icon.extension, sep = ""), file=HTMLoutput, Caption=i, Align="center", WidthHTML=200, HeightHTML=NULL)
  }

  output <- "UpperQuantiles75.html"
  HTMLoutput=file.path(graphics.path, output)
  for (i in distinct75) { 
    HTMLInsertGraph(GraphFileName= paste(icons.path, i, icon.extension, sep = ""), file=HTMLoutput, Caption=i, Align="center", WidthHTML=200, HeightHTML=NULL)
  }

}

icons.path <- paste(path, "/icons/", sep="")
MeanVarianceGraphics(Osm, icon.names, icons.path)








# Visualize row values for the entire OSM as a histogram
VisHistValues <- function(path) {
  # Visualizes row / column values of OSM as barplots
  # Args:
  # n/a
  # Return:
  # n/a

  # Read in OSM
  myOSM <- read.csv(paste(path, "osm.csv", sep = ""),header = F)
  myOSM <- as.data.frame(myOSM)

  ## Calculate the mean and variance for each row. Store in data frame.
  myOSM$Mean <- apply(myOSM[,-1],1,mean,na.rm=TRUE) #mean for each row, add to new column "Mean"
  myOSM$Variance <- apply(myOSM[2:55],1,var,na.rm=TRUE)

  # Icon names are in row 1. They are stored in myLabels
  myLabels <- as.vector(myOSM[,1])
  myMean <- round(myOSM[,56], 2)
  myVar <- round(myOSM[,57], 2)

  png(file = paste(path, "FreqHist_test", ".png", sep=""), width = 2600, height = 3200, pointsize = 12)
  par(mfrow=c(9,6))
  for(i in 2:55)
  {
    lab <- i-1
    subT = paste(myMean[lab], myVar[lab], sep = '//')
    barplot(table(myOSM[,i]), main = paste(myLabels[lab], subT, sep = ': '), cex.main = 1.5 )
  }
  dev.off()

}
VisHistValues(path)







hist(as.vector(myOSM[2]))
sum(myOSM[2])
boxplot(myOSM[2])
typeof(myOSM[2])
test <- as.vector(myOSM[2])
typeof(test)
hist(test)
barplot(table(myOSM[2]))
