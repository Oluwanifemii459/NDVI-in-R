### Calculating NDVI in R

## load the required packages
library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(viridis)
library(rasterVis)

## load the Planet imagery
Omo_FR <- raster("composite.tif")
Omo_FR

## Create an object for individual bands
b1 <- raster('composite.tif', band=1)
b2 <- raster('composite.tif', band=2)
b3 <- raster('composite.tif', band=3)
b4 <- raster('composite.tif', band=4)

compareRaster(b2, b3) ## they have same origin, coord etc

plot(b4)
## Note that the plot function only plots 100,000 pixels but image() strectches the view.
image(b4, col= viridis_pal(option="D")(10), main="Planet Image of Omo FR")

OmoFR_RGB <- stack(list(b4, b3, b2))  # creates raster stack
plotRGB(OmoFR_RGB, axes = TRUE, stretch = "lin", main = "Planet RGB colour composite")

## Load the planet imageries as a raster brick
OmoFR_brick <- brick("composite.tif")
plot(OmoFR_brick)

## Create a function to calculate NDVI
VI <- function(img, k, i) {
  bk <- img[[k]]
  bi <- img[[i]]
  vi <- (bk - bi) / (bk + bi)
  return(vi)
}

ndvi <- VI(OmoFR_brick, 4, 3)

png('ndviplot.png', width = 4, height = 4, units = "in", res = 300)
plot(ndvi, col = rev(terrain.colors(10)), main = 'Planet, Omo Biosphere Reserve-NDVI')
dev.off()


# Create histogram of NDVI data
png('ndvihist.png', width = 4, height = 4, units = "in", res = 300)
hist(ndvi,
     main = "Distribution of NDVI values",
     xlab = "NDVI",
     ylab= "Frequency",
     col = "aquamarine3",
     xlim = c(-0.5, 1),
     breaks = 30,
     xaxt = 'n')
axis(side = 1, at = seq(-0.5,1, 0.05), labels = seq(-0.5,1, 0.05))
dev.off()


# Mask cells that have NDVI of less than 0.4 (less likely to be vegetation)

png('ndvimask.png', width = 4, height = 4, units = "in", res = 300)

veg <- reclassify(ndvi, cbind(-Inf, 0.4, NA))
# We are reclassifying our object and making all values between negative infinity and 0.4 be NAs

plot(veg, main = 'Veg cover')
dev.off()


###############
# https://ourcodingclub.github.io/tutorials/spatial/
################