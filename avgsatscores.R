library(sf)
library(ggplot2)
library(ggthemes)
library(rgdal)
library(spdep)
library(spNetwork)
library(sp)
library(rio)
library(spdep)
library(raster)
library(terra)
library(reshape2)
library(tmap)
# install.packages("Rcpp")

setwd("~/Downloads/University of Waterloo/DRP")
data1 <- read.csv('scores 2.csv')
data1
file.exists("~/Downloads/University of Waterloo/DRP/scores.csv")

#############################################################################
########### BRAINSTORM ######################################################
#############################################################################

# qq plot
qqnorm(data1$`Average Score (SAT Math)`, pch = 1, frame = FALSE)
qqline(data1$`Average Score (SAT Math)`, col = "steelblue", lwd = 2)

# finding unique values for locations
unique(data1$City)
unique(data1$School.ID)
unique(data1$School.Name)
length(unique(data1$School.Name))


# performing linear regression
mod1 <- lm (data$Average.Score..SAT.Math. ~ data$School.ID + data$City)
summary(mod1)

mod2 <- lm(data$Average.Score..SAT.Writing. ~ data$School.ID)
summary(mod2)

plot(data$Average.Score..SAT.Math. ~ as.factor(data$City)) 

#############################################################################
########### SF File ######################################################
#############################################################################


# creating weighted matrix/adjacent linked list
# first create spatial graph
data_sf <- st_as_sf(data1, coords = c("Latitude", "Longitude"))
str(data_sf)
st_crs(data_sf) <- 4326 # assign WGS84 projection, which has EPSG code 4326
st_crs(data_sf)
st_is_valid(data_sf)
st_write(data_sf, 'scores.csv', driver = "ESRI Shapefile", delete_layer = TRUE) #saving object as a shapefile on our hard drive for later use
show_data_sf <-  st_read('scores.csv')
plot(show_data_sf, max.plot = 20)
plot(st_geometry(show_data_sf)) # to only plot the polygon boundaries, need to use the geometry column. Use st_geometry() to extract

philly_crimes_sp <- readOGR(data1, "Street.Address", verbose = FALSE) # verbose = FALSE omits the message on loading

spplot(philly_crimes_sp, "homic_rate")

# creating new subset data
data_math_score <- data1[,c('Latitude', 'Longitude', 'Average.Score..SAT.Math.')]
data_math_score

data2_sf <- st_as_sf(data_math_score, coords = c("Latitude", "Longitude"))
st_crs(data2_sf) <- 4326
st_write(data2_sf, 'scores.csv', driver = "ESRI Shapefile", delete_layer = TRUE) #saving object as a shapefile on our hard drive for later use
show_data_sf2 <-  st_read('scores.csv')
plot(show_data_sf2, max.plot = 20)

data_fullscore <- data_math_score
data_fullscore$math <- data1$Average.Score..SAT.Math.
data_fullscore$writing <- data1$Average.Score..SAT.Writing.
data_fullscore$reading <- data1$Average.Score..SAT.Reading.

data_fullscore$sat <- data_fullscore$math + data_fullscore$reading + data_fullscore$writing
data_fullscore$Average.Score..SAT.Math. <- NULL

data_fullscore <- data_fullscore[complete.cases(data_fullscore),] # only take rows that dont have NA

data3_sf <- st_as_sf(data_fullscore, coords = c("Latitude", "Longitude"))
st_crs(data3_sf) <- 4326
st_write(data3_sf, 'scores.csv', driver = "ESRI Shapefile", delete_layer = TRUE) #saving object as a shapefile on our hard drive for later use
show_data_sf3 <-  st_read('scores.csv')
plot(show_data_sf3, max.plot = 20)

# Plotting NY Geo shape on top of coordinates
# Download the shapefile. (note that I store it in a folder called DATA. You have to change that if needed.)
# download.file("https://www2.census.gov/geo/tiger/TIGER2016/COUSUB/tl_2016_36_cousub.zip" , destfile="/Users/jaimeeyeung/Downloads/University of Waterloo/DRP/NYsf/tl_2016_36_cousub.zip")
# system("unzip /Users/jaimeeyeung/Downloads/University of Waterloo/DRP/NYsf")

my_spdf2 <- sf::st_read(
  dsn= "/Users/jaimeeyeung/Downloads/University of Waterloo/DRP/tl_2016_36_cousub",
                    layer="tl_2016_36_cousub")

# Basic plot of this shape file:
par(mar=c(0,0,0,0))
plot(my_spdf2, col="#f2f2f2", bg="skyblue", lwd=0.25, border=0)

#############################################################################
# NY boroughs shape file ####################################################
#############################################################################
my_spdf3 <- sf::st_read(
  dsn= "/Users/jaimeeyeung/Downloads/University of Waterloo/DRP/nyu_2451_34490",
  layer="nyu_2451_34490")

# Basic plot of this shape file:
par(mar=c(0,0,0,0))
plot(my_spdf3, col="#f2f2f2", bg="skyblue", lwd=0.25, border=0)

#############################################################################
# NY 2015 public schools shape file #########################################
#############################################################################
my_spdf4 <- sf::st_read(
  dsn= "/Users/jaimeeyeung/Downloads/University of Waterloo/DRP/nyu_2451_34501",
  layer="nyu_2451_34501")

# Basic plot of this shape file:
par(mar=c(0,0,0,0))
plot(my_spdf4, col="#000000", bg="skyblue", lwd=0.25, border=0)


my_spdf5 <- sf::st_read(
  dsn= "/Users/jaimeeyeung/Downloads/University of Waterloo/DRP/nybb_23a",
  layer="nybb")

# Basic plot of this shape file:
par(mar=c(0,0,0,0))
plot(my_spdf5, col="#000000", lwd=0.25, border=0) # remove color inside

plot(my_spdf5, col="#000000", lwd=0.25, border=1, max.plot = 1) # remove color inside


#############################################################################
## using this guy's package from github ########
#############################################################################

devtools::install_github("zachcp/nycmaps")
library(nycmaps)
library(maps)
boundary <- map(database="nyc")
points_sf <- data3_sf

# plotting coords on map

# gg <- plot_nyc() 
# gg + geom_point(data=data_fullscore, aes(x=Longitude, y=Latitude, color = sat)) + 
#   scale_color_continuous()  + ## add diff colour palette 
#   theme(legend.position = c(0.2, 0.6)) + facet_grid(sat)


#Plot the results
# ggplot() +
#   geom_sf(data = boundary) +
#   geom_sf(data = points_sf)

nyc <- map_data("nyc")

# plotting full score
gg_fs  <- ggplot()
gg_fs  <- gg_fs +
  geom_map(
    data=nyc,
    map=nyc,
    aes(x=long, y=lat, map_id=region), fill = 'grey') + geom_point(data=data_fullscore, aes(x=Longitude, y=Latitude, color = sat)) + scale_color_gradient(low = "lavender", high = "darkblue")
gg_fs

# plotting reading 
gg_read  <- ggplot()
gg_read  <- gg_read +
  geom_map(
    data=nyc,
    map=nyc,
    aes(x=long, y=lat, map_id=region), fill = 'grey') + geom_point(data=data_fullscore, aes(x=Longitude, y=Latitude, color = reading)) + scale_color_gradient(low = "lavender", high = "darkblue")
gg_read

# plotting math
gg_math <- ggplot()
gg_math  <- gg_math +
  geom_map(
    data=nyc,
    map=nyc,
    aes(x=long, y=lat, map_id=region), fill = 'grey') + geom_point(data=data_fullscore, aes(x=Longitude, y=Latitude, color = math)) + scale_color_gradient(low = "lavender", high = "darkblue")
gg_math

# plotting writing
gg_write <- ggplot()
gg_write  <- gg_write +
  geom_map(
    data=nyc,
    map=nyc,
    aes(x=long, y=lat, map_id=region), fill = 'grey') + geom_point(data=data_fullscore, aes(x=Longitude, y=Latitude, color = writing)) + scale_color_gradient(low = "lavender", high = "darkblue")
gg_write

# 
# gg2 <- ggplot() + geom_sf(data = points_sf) + geom_map(
#                                                         data=nyc, 
#                                                         map=nyc,
#                                                         aes(x=long, y=lat, map_id=region))
# gg2
#   

######## creating euclidean distance matrix ######## 
# lon lat true when coordinates should be in degrees, false when they represent planar (euclidean) space ie units of meters

pts <- data_fullscore[,c('Longitude', 'Latitude')]

# calculating the distance between points 
gdis <- pointDistance(pts, lonlat=FALSE) 
gdis

# calculating the weighted matrix based on the distance
W <- 1/gdis # 1/distance = weight
round(W, 4)
W[!is.finite(W)] <- 0
rtot <- rowSums(W, na.rm=TRUE)
rtot

# normalizing the weighted matrix. sum of all weights = 1
W <- W / rtot

# sum of all weights of all schools in comparison to 1 other school. 1/distance = weight
rowSums(W, na.rm=TRUE)
colSums(W, na.rm=TRUE)

####### assigning weights to the neighbors ##########

# mat2listw converts a square spatial weights matrix to a weights list object
lw <- mat2listw(W, row.names = NULL, style='M') 

lw$weights[1] # weights in comparison to the first school

# compute the Moran's I statistic
I <- moran(data3_sf$sat, lw, nrow(data3_sf), sum(W))[1]
I

# Hypothesis Testing - assuming the data follows a Gaussian Distribution
moran.test(data3_sf$sat, lw,alternative="greater")

# ----------------  Monte Carlo Hypothesis Testing  -------------------
# alt = greater, calc area to the right of line
MC<- moran.mc(data3_sf$sat, lw, nsim=999, alternative="greater")

# View results (including p-value)
MC

plot(MC)

######################## Creating a scatter plot between SAT scores and their lagged counterpart ########################## 
# computing the average SAT score for each coordinate -> these are often referred to as spatially lagged values

inc.lag <- lag.listw(lw, data3_sf$sat)
inc.lag

# plotting relationship between income and its spatially lagged counterpart. fitted blue line is the result of 
# an OLS regression model

plot (inc.lag ~ data3_sf$sat, pch = 16, asp = 1) #### scale to zoom in more
M1 <- lm(inc.lag ~ data3_sf$sat)
abline(M1, col = "blue")

# the slope of the line is the Moran's I coefficient. Now we can extract its value from the model object M1 as follows
coef(M1)[2]
