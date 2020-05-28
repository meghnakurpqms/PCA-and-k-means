
  
#### Load libraries

require(jpeg)
require(RCurl)

#### Load image URL


url <-"https://www.ecopetit.cat/wpic/mpic/"
url <- paste(url, "19-191637_italian-food-images-hd.jpg", sep="")

#### Read Image

readImage <- readJPEG(getURLContent(url, binary=TRUE))

###head(readImage)
#dim(readImage)
#readImage[,,1]

#### Convert the image to a dataframe

dm <- dim(readImage)
rgbImage <- data.frame(
  x=rep(1:dm[2], each=dm[1]),
  y=rep(dm[1]:1, dm[2]),
  r.value=as.vector(readImage[,,1]),
  g.value=as.vector(readImage[,,2]),
  b.value=as.vector(readImage[,,3]))

#### Plot original image

plot(y ~ x, data=rgbImage, main="Pizza",
     col = rgb(rgbImage[c("r.value", "g.value", "b.value")]), 
     asp = 1, pch = ".")

## Color quantization using k-means with different values of k

#### Using k- means with 2 colors

kColors <- 2  
kMeans <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                 centers = kColors)
clusterColour <- rgb(kMeans$centers[kMeans$cluster, ])

#### Plotting after k-means

plot(y ~ x, data=rgbImage, main="Pizza",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="", xlab="k-means cluster analysis of 2 colours")

#### Using k- means with 3 colors

kColors <- 3  
kMeans <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                 centers = kColors)
clusterColour <- rgb(kMeans$centers[kMeans$cluster, ])

#### Plotting after k-means

plot(y ~ x, data=rgbImage, main="Pizza",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="", 
     xlab="k-means cluster analysis of 3 colours")

#### Using k- means with 5 colors

kColors <- 5  
kMeans <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                 centers = kColors)
clusterColour <- rgb(kMeans$centers[kMeans$cluster, ])

#### Plotting after k-means

plot(y ~ x, data=rgbImage, main="Pizza",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="", xlab="k-means cluster analysis of 5 colours")

#### Using k- means with 10 colors

kColors <- 10  
kMeans <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                 centers = kColors)
clusterColour <- rgb(kMeans$centers[kMeans$cluster, ])

#### Plotting after k-means

plot(y ~ x, data=rgbImage, main="Pizza",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="", xlab="k-means cluster analysis of 10 colours")

#### Using k- means with 15 colors

kColors <- 15  
kMeans <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                 centers = kColors)
clusterColour <- rgb(kMeans$centers[kMeans$cluster, ])

#### Plotting after k-means

plot(y ~ x, data=rgbImage, main="Pizza",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="", xlab="k-means cluster analysis of 15 colours")

#### Using k- means with 20 colors

kColors <- 20  
kMeans <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                 centers = kColors)
clusterColour <- rgb(kMeans$centers[kMeans$cluster, ])

#### Plotting after k-means

plot(y ~ x, data=rgbImage, main="Pizza",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="", xlab="k-means cluster analysis of 20 colours")

#### Using k- means with 50 colors

kColors <- 50  
kMeans <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                 centers = kColors)
clusterColour <- rgb(kMeans$centers[kMeans$cluster, ])

#### Plotting after k-means

plot(y ~ x, data=rgbImage, main="Pizza",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="", xlab="k-means cluster analysis of 50 colours")

#### Using k- means with 255 colors

kColors <- 255  
kMeans <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                 centers = kColors)
clusterColour <- rgb(kMeans$centers[kMeans$cluster, ])

#### Plotting after k-means

plot(y ~ x, data=rgbImage, main="Pizza",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="", xlab="k-means cluster analysis of 255 colours")

## Principal component Analysis

#### Load the jpeg library

library(jpeg)

#### Using the same image
#### Number of columns

ncol(readImage)

nrow(readImage)

#### The pizza image is now represented as three 1920x1080 matrices as an array with each matrix corresponding to the RGB color value scheme. Extract the individual color value matrices to perform PCA on each.


r <- readImage[,,1]
g <- readImage[,,2]
b <- readImage[,,3]

#### Principal component analysis is performed on each color value matrix. 

readImage.r.pca <- prcomp(r, center = FALSE)
readImage.g.pca <- prcomp(g, center = FALSE)
readImage.b.pca <- prcomp(b, center = FALSE)

#### Collect the PCA objects into a list.

rgb.pca <- list(readImage.r.pca, readImage.g.pca, readImage.b.pca)

#### The following loop reconstructs the original image using the projections of the data using increasing amounts of principal components.

for (i in seq.int(3, round(nrow(readImage) - 10), length.out = 10)) {
  pca.img <- sapply(rgb.pca, function(j) {
    compressed.img <- j$x[,1:i] %*% t(j$rotation[,1:i])
  }, simplify = 'array')
  str = paste('pizza_compressed', round(i,0), '.jpg')
  writeJPEG(pca.img,target = str)
}

#### We can check the compression ratio for each iteration compared to the original image with a quick loop.

original <- file.info('pizza.jpg')$size / 1000
imgs <- dir('PCA_images/')

for (i in imgs) {
  full.path <- paste('PCA_images/', i, sep='')
  print(paste(i, ' size: ', file.info(full.path)$size / 1000, ' original: ', original, ' % diff: ', round((file.info(full.path)$size / 1000 - original) / original, 2) * 100, '%', sep = ''))
}
