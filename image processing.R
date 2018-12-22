
## 17.12.2017
# Subject: image processing using imager packege.

image_processing <- function(image_path,metadata_path){
  
  # uplowding 'imager packege'
  library(imager)
  #load a jpg image file
  foram1<-load.image(image_path)
  
  #change to gray scale
  gray_foram<-grayscale(foram1)
  
  # change to binary image (threshold)
  # above 0.1 turens into one (white), below =ziro (black)
  binary<-gray_foram > 0.1
  
  #seperate into segments
  #segmented is a list of images, each image contain different component 
  segmented<-split_connected(binary)
 
  max_comp<-find_max_comp(segmented)
 
  # find the component bounderies
  find_boundry<-bbox(max_comp)
  
  # crop the image around the component bounderies
  foram_boundry<-crop.bbox(max_comp,find_boundry)
  
  #call function (measurements) that give the width measurement in pixel
  source('C:/Users/user/Dropbox/Roni_PHD/P.nuttalli/measurements.R')
  specimen_width_px <- measuring_specimen_width(foram_boundry, metadata_path)

  # call function (scale)that give the px/um ratio
  source('C:/Users/user/Dropbox/Roni_PHD/P.nuttalli/XML_parse.R')
  scale_um <- scale(metadata_path)

  # convert messurments from pixel to micrometers
  specimen_width_um <- specimen_width_px / scale_um
 
  # plot(specimen_width_um,ylab = "width")
  return(specimen_width_um)

}


#find_max_comp returns the largest connected component 
find_max_comp<-function(image_list){
  sums<-lapply(image_list,function(x) sum(x))
  max_index<-which.max(sums)
  #plot(image_list[[max_index]])
  return(image_list[[max_index]])
}
