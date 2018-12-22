#19.12.2017
#subject: specimen measuring
measuring_specimen_width<-function(image,metadata_path){
  
  #defined image as matrix
  matrix <- as.matrix(image)
  
  #defining no. of measurement & steps size
  
  #option 1: Fix number of measurements
  #num_measurements <- 50
  #Defining the step size between measurement (total length divided into no. of measurements)
  #step_size<-as.integer(floor(dim(matrix)[2]/num_measurements))
  
  #option 2: Fix step size as 10 um.
  # call function (scale)that give the px/um ratio
  source('C:/Users/user/Dropbox/Roni_PHD/P.nuttalli/XML_parse.R')
  scale_um <- scale(metadata_path)
  # scale um is no. of pix that equal to 1 um
  
  
  #defined step size as 10 Microns in pix
  step_size <- 10*scale_um
  
  #Defining the no. of messurment (total length divided into step size)
  num_measurements <- as.integer(floor(dim(matrix)[2]/step_size))
  
  #creating vector that will contain the no. of required width messurments
  results<-numeric(num_measurements)
  
  #creating vector that will contain the logarithmic variables
  log_parameters<-vector()
  
  #creating vector that will contain the highet at each step size
  heights <- numeric(num_measurements)
  
  #creating vector that will contain the first white at each step size
  curve <- numeric(num_measurements)
  
  
  
  # lop that add steps size
  for(i in 1:num_measurements) {
    r<-i*step_size
    
    # put heights results of each step size into vector
    heights[i] <- r
    
    # defined the first column
    c_i <- as.integer(1)
    
    # go throgh each step size (row) on all columns, from one until whit (value "1") 
    while( matrix[c_i,r] < 1){
      # when whit go to the next step size
      c_i = c_i + 1
    }
    
    
    # defined the last column
    c_f <- as.integer(dim(matrix)[1])
    
    # go throgh each step size (row) on all columns, from the last one until wghit (value "1") 
    while( matrix[c_f,r] < 1){
      # when whigt go to the privius step size
      c_f = c_f - 1
    }
    
    #curve is the length of the image untile the first wghit at each step size
    curve[i] <- c_i
    
    # defind the length of the whight columns at each step size according to first and last whight column
    width <- c_f-c_i
    
    # put width results from each step size into vector
    results[i] <- width
  }
  
  
  # the red lines represent all messurments
   #  plot(image)
   #  abline(h=heights,col="red")

  #print(results)
  return(results)
 
}

