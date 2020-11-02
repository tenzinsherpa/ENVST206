#activity 8
install.packages(c("raster"))
library(raster)
library(ggplot2)
library(rgdal)

#set up directory for oneida data folder
dirR <- "/Users/TenzinSherpa/Documents/ENVST206 Data/a08/oneida"

#read in Sentinel data
rdatB2 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B02_20m.tif"))
rdatB3 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B03_20m.tif"))
rdatB4 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B04_20m.tif"))
rdatB8 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B08_20m.tif"))

plot(rdatB2/10000)

#stack red green blue
rgbS <- stack(rdatB4,rdatB3,rdatB2)/10000
#view raster, a few pixels in blue have reflectanc above 1 so set scale
plotRGB(rgbS, scale=2)

#don't need the scale argument when adding in the contrast stretch
plotRGB(rgbS, stretch="lin")

#full resolutions 
#get the total number of pixels by multiplying the number of rows and columns
#in the raster
plotRGB(rgbS, stretch="lin",maxpixels=rgbS@nrows*rgbS@ncols)


#colors we cannot see with our eyes
#infrared rdatB8 --> red color in the plot
#real life red --> give it a green color
#real life green --> blue rdatB3