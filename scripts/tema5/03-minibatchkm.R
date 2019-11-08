install.packages(c("OpenImageR", "ClusterR"))
library(OpenImageR)
library(ClusterR)

imagename <- "../data/tema5/bird.jpg"

img <- readImage(imagename)

img.resize <- resizeImage(img, 350, 350, 
                          method = "bilinear")
imageShow(img)

#imagen como vector
img.vector <- apply(img, 3, as.vector)
dim(img.vector)


kmmb<-MiniBatchKmeans(img.vector, clusters = 5,
                      batch_size = 20, num_init = 5,
                      max_iters = 100, init_fraction = 0.2,
                      initializer = "kmeans++",
                      early_stop_iter = 10, verbose = F)
kmmb

prmb <- predict_MBatchKMeans(img.vector, kmmb$centroids)

get.cent.mb <- kmmb$centroids
new.img <- get.cent.mb[prmb,]
dim(new.img) <- c(nrow(img), ncol(img),3)
imageShow(new.img)
