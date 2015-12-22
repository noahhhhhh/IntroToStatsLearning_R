
#####################################################
## principal component analysis (PCA) ###############
#####################################################
states <- row.names(USArrests)
states
# [1] "Alabama"        "Alaska"         "Arizona"        "Arkansas"       "California"    
# [6] "Colorado"       "Connecticut"    "Delaware"       "Florida"        "Georgia"       
# [11] "Hawaii"         "Idaho"          "Illinois"       "Indiana"        "Iowa"          
# [16] "Kansas"         "Kentucky"       "Louisiana"      "Maine"          "Maryland"      
# [21] "Massachusetts"  "Michigan"       "Minnesota"      "Mississippi"    "Missouri"      
# [26] "Montana"        "Nebraska"       "Nevada"         "New Hampshire"  "New Jersey"    
# [31] "New Mexico"     "New York"       "North Carolina" "North Dakota"   "Ohio"          
# [36] "Oklahoma"       "Oregon"         "Pennsylvania"   "Rhode Island"   "South Carolina"
# [41] "South Dakota"   "Tennessee"      "Texas"          "Utah"           "Vermont"       
# [46] "Virginia"       "Washington"     "West Virginia"  "Wisconsin"      "Wyoming"  

# col names
names(USArrests)
# [1] "Murder"   "Assault"  "UrbanPop" "Rape"   

# examine the data
apply(USArrests, 2, mean)
# Murder  Assault UrbanPop     Rape 
# 7.788  170.760   65.540   21.232 
apply(USArrests, 2, sd)
# Murder   Assault  UrbanPop      Rape 
# 4.355510 83.337661 14.474763  9.366385 

# pca
pr.out <- prcomp(USArrests, scale = T)
names(pr.out)
# [1] "sdev"     "rotation" "center"   "scale"    "x" 

# rotation matrix provides the principal component loadings
pr.out$rotation
# PC1        PC2        PC3         PC4
# Murder   -0.5358995  0.4181809 -0.3412327  0.64922780
# Assault  -0.5831836  0.1879856 -0.2681484 -0.74340748
# UrbanPop -0.2781909 -0.8728062 -0.3780158  0.13387773
# Rape     -0.5434321 -0.1673186  0.8177779  0.08902432

# score
pr.out$x
# PC1         PC2         PC3          PC4
# Alabama        -0.97566045  1.12200121 -0.43980366  0.154696581
# Alaska         -1.93053788  1.06242692  2.01950027 -0.434175454
# Arizona        -1.74544285 -0.73845954  0.05423025 -0.826264240
# Arkansas        0.13999894  1.10854226  0.11342217 -0.180973554
# California     -2.49861285 -1.52742672  0.59254100 -0.338559240
# Colorado       -1.49934074 -0.97762966  1.08400162  0.001450164
# Connecticut     1.34499236 -1.07798362 -0.63679250 -0.117278736
# Delaware       -0.04722981 -0.32208890 -0.71141032 -0.873113315
# Florida        -2.98275967  0.03883425 -0.57103206 -0.095317042
# Georgia        -1.62280742  1.26608838 -0.33901818  1.065974459
# Hawaii          0.90348448 -1.55467609  0.05027151  0.893733198
# Idaho           1.62331903  0.20885253  0.25719021 -0.494087852
# Illinois       -1.36505197 -0.67498834 -0.67068647 -0.120794916
# Indiana         0.50038122 -0.15003926  0.22576277  0.420397595
# Iowa            2.23099579 -0.10300828  0.16291036  0.017379470
# Kansas          0.78887206 -0.26744941  0.02529648  0.204421034
# Kentucky        0.74331256  0.94880748 -0.02808429  0.663817237
# Louisiana      -1.54909076  0.86230011 -0.77560598  0.450157791
# Maine           2.37274014  0.37260865 -0.06502225 -0.327138529
# Maryland       -1.74564663  0.42335704 -0.15566968 -0.553450589
# Massachusetts   0.48128007 -1.45967706 -0.60337172 -0.177793902
# Michigan       -2.08725025 -0.15383500  0.38100046  0.101343128
# Minnesota       1.67566951 -0.62590670  0.15153200  0.066640316
# Mississippi    -0.98647919  2.36973712 -0.73336290  0.213342049
# Missouri       -0.68978426 -0.26070794  0.37365033  0.223554811
# Montana         1.17353751  0.53147851  0.24440796  0.122498555
# Nebraska        1.25291625 -0.19200440  0.17380930  0.015733156
# Nevada         -2.84550542 -0.76780502  1.15168793  0.311354436
# New Hampshire   2.35995585 -0.01790055  0.03648498 -0.032804291
# New Jersey     -0.17974128 -1.43493745 -0.75677041  0.240936580
# New Mexico     -1.96012351  0.14141308  0.18184598 -0.336121113
# New York       -1.66566662 -0.81491072 -0.63661186 -0.013348844
# North Carolina -1.11208808  2.20561081 -0.85489245 -0.944789648
# North Dakota    2.96215223  0.59309738  0.29824930 -0.251434626
# Ohio            0.22369436 -0.73477837 -0.03082616  0.469152817
# Oklahoma        0.30864928 -0.28496113 -0.01515592  0.010228476
# Oregon         -0.05852787 -0.53596999  0.93038718 -0.235390872
# Pennsylvania    0.87948680 -0.56536050 -0.39660218  0.355452378
# Rhode Island    0.85509072 -1.47698328 -1.35617705 -0.607402746
# South Carolina -1.30744986  1.91397297 -0.29751723 -0.130145378
# South Dakota    1.96779669  0.81506822  0.38538073 -0.108470512
# Tennessee      -0.98969377  0.85160534  0.18619262  0.646302674
# Texas          -1.34151838 -0.40833518 -0.48712332  0.636731051
# Utah            0.54503180 -1.45671524  0.29077592 -0.081486749
# Vermont         2.77325613  1.38819435  0.83280797 -0.143433697
# Virginia        0.09536670  0.19772785  0.01159482  0.209246429
# Washington      0.21472339 -0.96037394  0.61859067 -0.218628161
# West Virginia   2.08739306  1.41052627  0.10372163  0.130583080
# Wisconsin       2.05881199 -0.60512507 -0.13746933  0.182253407
# Wyoming         0.62310061  0.31778662 -0.23824049 -0.164976866

biplot(pr.out, scale = 0) # The scale=0 argument to biplot() ensures that the arrows are scaled to represent the loadings

# change the sign
pr.out$rotation <- -pr.out$rotation
pr.out$x <- - pr.out$x
biplot(pr.out, scale = 0)

# sd of each PC
pr.out$sdev
# [1] 1.5748783 0.9948694 0.5971291 0.4164494
# var of each PC
pr.var <- pr.out$sdev^2
pr.var
# [1] 2.4802416 0.9897652 0.3565632 0.1734301
# var explained by each PC
pve <- pr.var/sum(pr.var)
# [1] 0.62006039 0.24744129 0.08914080 0.04335752

# plot it
plot(pve , xlab =" Principal Component ", ylab=" Proportion of
Variance Explained ", ylim=c(0,1) ,type = 'b')
plot(cumsum(pve), xlab=" Principal Component ", ylab ="Cumulative Proportion of Variance Explained ", ylim=c(0,1) ,type = 'b')

#####################################################
## Clustering #######################################
#####################################################
# k-means
set.seed(2)
x <- matrix(rnorm(50 * 2), ncol = 2)
x[1:25, 1] <- x[1:25, 1] + 3
x[1:25, 2] <- x[1:25, 2] - 4

# k = 2
km.out <- kmeans(x, 2, nstart = 20)
km.out$cluster
# [1] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# [44] 1 1 1 1 1 1 1
plot(x, col = (km.out$cluster + 1), main = "K-Means Clustiner Results with K = 2"
     , xlab = ""
     , ylab = ""
     , pch = 20
     , cex = 2)

# k = 3
set.seed(4)
km.out <- kmeans(x, 3, nstart = 20)
km.out
plot(x, col = (km.out$cluster + 1), main="K-Means Clustering Results with K=3"
     , xlab ="", ylab="", pch = 20, cex = 2)

# perform 20 times of random initial cluster setting
set.seed(3)
km.out <- kmeans(x, 3, nstart = 1)
km.out$tot.withinss
# [1] 104.3319
km.out <- kmeans(x, 3, nstart = 20) # the best one will be selected over these 20 random init clusters
km.out$tot.withinss
# [1] 97.97927
# individual withiness
km.out$withinss
# [1] 25.74089 19.56137 52.67700

# hierarchical clustering
# use dist() to get a 50*50 euclidean distance matrix
hc.complete <- hclust(dist(x), method = "complete")
hc.average <- hclust(dist(x), method = "average")
hc.single <- hclust(dist(x), method = "single")

# plot
par(mfrow = c(1, 3))
plot(hc.complete, main = "Complete Linkage", xlab= "", sub = "", cex =.9)
plot(hc.average, main = "Average Linkage", xlab= "", sub = "", cex =.9)
plot(hc.single, main = "Single Linkage", xlab= "", sub = "", cex =.9)

# cut tree, k = 2
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)

# scale it before performing hc
xsc <- scale(x)
plot(hclust(dist(xsc), method = "complete"), main = "Hierarchical Clustering with Scaled Features")

# use coorelation distance
x <- matrix(rnorm(30*3), ncol = 3)
dd <- as.dist(1 - cor(t(x)))
plot(hclust(dd, method = "complete"), main = "Complete Linkage with Correlation-Based Distance")

#####################################################
## A NCI60 Data Example #############################
#####################################################
library(ISLR)
nci.labs <- NCI60$labs
nci.data <- NCI60$data
dim(nci.data)
# [1]   64 6830
nci.labs[1:4]
# [1] "CNS"   "CNS"   "CNS"   "RENAL"
table(nci.labs)
# BREAST         CNS       COLON K562A-repro K562B-repro    LEUKEMIA MCF7A-repro 
# 7           5           7           1           1           6           1 
# MCF7D-repro    MELANOMA       NSCLC     OVARIAN    PROSTATE       RENAL     UNKNOWN 
# 1           8           9           6           2           9           1

# PCA
pr.out <- prcomp(nci.data, scale = T)
Cols <- function(vec){
    cols <- rainbow(length(unique(vec)))
    return(cols[as.numeric(as.factor(vec))])
}
par(mfrow = c(1, 2))
plot(pr.out$x[, 1:2], col = Cols(nci.labs), pch = 19, xlab = "Z1", ylab = "Z2")
plot(pr.out$x[, 1:3], col = Cols(nci.labs), pch = 19, xlab = "Z1", ylab = "Z3")

summary(pr.out)
plot(pr.out)
# PVE
pve <- 100 * pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow = c(1, 2))
plot(pve, type = "o", ylab = "PVE", xlab = "PC", col = "blue")
plot(cumsum(pve), type = "o", ylab = "Cum PVE", xlab = "PC", col = "brown3")

# Clustering
# hcust
sd.data <- scale(nci.data)
par(mfrow = c(1, 3))
data.dist <- dist(sd.data)
plot(hclust(data.dist), labels = nci.labs, main = "Complete Linkage", xlab = "", ylab = "")
plot(hclust(data.dist, method = "average"), labels = nci.labs, main = "Average Linkage", xlab = "", ylab = "")
plot(hclust(data.dist, method = "single"), labels = nci.labs, main = "Single Linkage", xlab = "", ylab = "")

hc.out <- hclust(dist(sd.data))
hc.clusters <- cutree(hc.out, 4)
table(hc.clusters, nci.labs)

par(mfrow = c(1, 1))
plot(hc.out, labels = nci.labs)
abline(h = 139, col = "red")

hc.out
# Call:
#     hclust(d = dist(sd.data))
# 
# Cluster method   : complete 
# Distance         : euclidean 
# Number of objects: 64 

# kmeans
set.seed(2)
km.out <- kmeans(sd.data, 4, nstart = 20)
km.clusters <- km.out$cluster
table(km.clusters, hc.clusters)
#               hc.clusters
# km.clusters  1  2  3  4
#           1 11  0  0  9
#           2  0  0  8  0
#           3  9  0  0  0
#           4 20  7  0  0

# perform hcust on PCs
hc.out <- hclust(dist(pr.out$x[, 1:5]))
plot(hc.out, labels = nci.labs, main = "Hier.Cust. on First 5 Score Vectors")
table(cutree(hc.out, 4), nci.labs)
# Sometimes
# performing clustering on the first few principal component score vectors
# can give better results than performing clustering on the full data.




















