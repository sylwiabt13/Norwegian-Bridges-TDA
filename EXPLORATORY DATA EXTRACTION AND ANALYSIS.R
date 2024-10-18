library(R.matlab)

LOWWIND08_12_2013_52 <- readMat("HB141M-2013-12-08_09-52-02_LowFreq.mat")

LOWWIND08_12_2013_52

Hz2times <- LOWWIND08_12_2013_52$recording[18][[1]]
#extracts what I believe are time points of recordings
#transposed so I can produce a list of times

#RESULTS FROM A H1 Vest triaxial accelerometer
LOWWIND08_12_2013_52[["recording"]][[20]][[84]]

#VERIFY lengths of objects are the same
length(LOWWIND08_12_2013_52[["recording"]][[20]][[84]][,1])
length(Hz2times)

#plotting the accelerometer movements in each direction for sanity checking
plot(Hz2times,LOWWIND08_12_2013_52[["recording"]][[20]][[12]][,1], type="l", col="blue", ylab="(m)", ylim = c(0.005,0.025))
lines(Hz2times,LOWWIND08_12_2013_52[["recording"]][[20]][[12]][,2], type="l", col="magenta", ylab = "(m)")
lines(Hz2times,LOWWIND08_12_2013_52[["recording"]][[20]][[12]][,3], type="l", col="darkviolet", ylab = "(m)")
legend(x=0,y=0.013,legend=c("x","y","z"),fill=c("blue","magenta", "darkviolet"))
title(main="H1E Accelerometer")

plot(Hz2times, LOWWIND08_12_2013[["recording"]][[20]][[24]][,1], type="l", col="blue", ylab="(m)", ylim=c(-0.001,0.016))
lines(Hz2times,LOWWIND08_12_2013_52[["recording"]][[20]][[24]][,2], type="l", col="magenta", ylab = "(m)")
lines(Hz2times,LOWWIND08_12_2013_52[["recording"]][[20]][[24]][,3], type="l", col="darkviolet", ylab = "(m)")
legend(x=0,y=0.005,legend=c("x","y","z"),fill=c("blue","magenta", "darkviolet"))
title(main="H1W Accelerometer")

plot(Hz2times, LOWWIND08_12_2013[["recording"]][[20]][[36]][,1], type="l", col="blue", ylab="(m)", ylim=c(-0.001,0.016))
lines(Hz2times,LOWWIND08_12_2013_52[["recording"]][[20]][[36]][,2], type="l", col="magenta", ylab = "(m)")
lines(Hz2times,LOWWIND08_12_2013_52[["recording"]][[20]][[36]][,3], type="l", col="darkviolet", ylab = "(m)")
legend(x=0,y=0.005,legend=c("x","y","z"),fill=c("blue","magenta", "darkviolet"))
title(main="H2W Accelerometer")

plot(Hz2times, LOWWIND08_12_2013[["recording"]][[20]][[48]][,1], type="l", col="blue", ylab="(m)", ylim=c(0.009,0.0115))
lines(Hz2times,LOWWIND08_12_2013_52[["recording"]][[20]][[48]][,2], type="l", col="magenta", ylab = "(m)")
lines(Hz2times,LOWWIND08_12_2013_52[["recording"]][[20]][[48]][,3], type="l", col="darkviolet", ylab = "(m)")
legend(x=0,y=0.01075,legend=c("x","y","z"),fill=c("blue","magenta", "darkviolet"))
title(main="H3E Accelerometer")

### could go on forever this is another file we could look at:

STORMROAR2015_12_08_1 <- readMat("StormRoar_lowFreq/HB141M-2015-10-02_07-50-17_LowFreq.mat")


#It is probably easier to write code that extract the data from the LOWWIND08_12_2013 file
#This will enable faster extraction of the data and easier use in R

#NOTE THIS FUNCTION WILL ONLY WORK ON PARTICULAR DATASETS AND MAY HAVE TO BE ALTERED FOR EACH DATASET
#however having this basis will probably simplify data extraction
extractMAT_HARDANGER_v1 <- function(DATASET){
  
  #time from data set, these measurements are in seconds
  time <- DATASET[["recording"]][[18]]
  
  #Relevant data from the main triaxial accelerometers placed INSIDE BRIDGE DECK
  #These measurements are all in g, where 1g ~= 9.81 m/s^2
  H1E_x <- DATASET[["recording"]][[20]][[12]][,1]
  H1E_y <- DATASET[["recording"]][[20]][[12]][,2]
  H1E_z <- DATASET[["recording"]][[20]][[12]][,3]
  
  H1W_x <- DATASET[["recording"]][[20]][[24]][,1]
  H1W_y <- DATASET[["recording"]][[20]][[24]][,2]
  H1W_z <- DATASET[["recording"]][[20]][[24]][,3]
  
  #H2E as a sensor does not exist
  
  H2W_x <- DATASET[["recording"]][[20]][[36]][,1]
  H2W_y <- DATASET[["recording"]][[20]][[36]][,2]
  H2W_z <- DATASET[["recording"]][[20]][[36]][,3]
  
  H3E_x <- DATASET[["recording"]][[20]][[48]][,1]
  H3E_y <- DATASET[["recording"]][[20]][[48]][,2]
  H3E_z <- DATASET[["recording"]][[20]][[48]][,3]
  
  H3W_x <- DATASET[["recording"]][[20]][[60]][,1]
  H3W_y <- DATASET[["recording"]][[20]][[60]][,2]
  H3W_z <- DATASET[["recording"]][[20]][[60]][,3]
  
  H4E_x <- DATASET[["recording"]][[20]][[72]][,1]
  H4E_y <- DATASET[["recording"]][[20]][[72]][,2]
  H4E_z <- DATASET[["recording"]][[20]][[72]][,3]
  
  H4W_x <- DATASET[["recording"]][[20]][[84]][,1]
  H4W_y <- DATASET[["recording"]][[20]][[84]][,2]
  H4W_z <- DATASET[["recording"]][[20]][[84]][,3]
  
  H5E_x <- DATASET[["recording"]][[20]][[96]][,1]
  H5E_y <- DATASET[["recording"]][[20]][[96]][,2]
  H5E_z <- DATASET[["recording"]][[20]][[96]][,3]
  
  H5W_x <- DATASET[["recording"]][[20]][[108]][,1]
  H5W_y <- DATASET[["recording"]][[20]][[108]][,2]
  H5W_z <- DATASET[["recording"]][[20]][[108]][,3]
  
  H6E_x <- DATASET[["recording"]][[20]][[120]][,1]
  H6E_y <- DATASET[["recording"]][[20]][[120]][,2]
  H6E_z <- DATASET[["recording"]][[20]][[120]][,3]
  
  H6W_x <- DATASET[["recording"]][[20]][[132]][,1]
  H6W_y <- DATASET[["recording"]][[20]][[132]][,2]
  H6W_z <- DATASET[["recording"]][[20]][[132]][,3]
 
  H7E_x <- DATASET[["recording"]][[20]][[144]][,1]
  H7E_y <- DATASET[["recording"]][[20]][[144]][,2]
  H7E_z <- DATASET[["recording"]][[20]][[144]][,3]
  
  H7W_x <- DATASET[["recording"]][[20]][[156]][,1]
  H7W_y <- DATASET[["recording"]][[20]][[156]][,2]
  H7W_z <- DATASET[["recording"]][[20]][[156]][,3]
  
  H8E_x <- DATASET[["recording"]][[20]][[168]][,1]
  H8E_y <- DATASET[["recording"]][[20]][[168]][,2]
  H8E_z <- DATASET[["recording"]][[20]][[168]][,3]
  
  #H8W as a sensor does not exist
  
  H9E_x <- DATASET[["recording"]][[20]][[180]][,1]
  H9E_y <- DATASET[["recording"]][[20]][[180]][,2]
  H9E_z <- DATASET[["recording"]][[20]][[180]][,3]
  
  H9W_x <- DATASET[["recording"]][[20]][[192]][,1]
  H9W_y <- DATASET[["recording"]][[20]][[192]][,2]
  H9W_z <- DATASET[["recording"]][[20]][[192]][,3]
 
  #Accelerometers INSIDE TOWER
  
  H10E_x <- DATASET[["recording"]][[20]][[204]][,1]
  H10E_y <- DATASET[["recording"]][[20]][[204]][,2]
  H10E_z <- DATASET[["recording"]][[20]][[204]][,3]
  
  H10W_x <- DATASET[["recording"]][[20]][[216]][,1]
  H10W_y <- DATASET[["recording"]][[20]][[216]][,2]
  H10W_z <- DATASET[["recording"]][[20]][[216]][,3]
  
  H11E_x <- DATASET[["recording"]][[20]][[228]][,1]
  H11E_y <- DATASET[["recording"]][[20]][[228]][,2]
  H11E_z <- DATASET[["recording"]][[20]][[228]][,3]
  
  H11W_x <- DATASET[["recording"]][[20]][[240]][,1]
  H11W_y <- DATASET[["recording"]][[20]][[240]][,2]
  H11W_z <- DATASET[["recording"]][[20]][[240]][,3]
 
  #Now for the Anemometers, they capture several different weather indicators
  #angle is in degrees, U (eastward wind-flow) and w (upward wind-flow) are measured in m/s, temp is measured in degrees Celsius
  
  #A1 is located near H1W/H1E
  A1_angle <- DATASET[["recording"]][[20]][[252]][,1]
  A1_U <- DATASET[["recording"]][[20]][[252]][,2]
  A1_w <- DATASET[["recording"]][[20]][[252]][,3]
  A1_temp <- DATASET[["recording"]][[20]][[252]][,4]
  
  A2_angle <- DATASET[["recording"]][[20]][[264]][,1]
  A2_U <- DATASET[["recording"]][[20]][[264]][,2]
  A2_w <- DATASET[["recording"]][[20]][[264]][,3]
  A2_temp <- DATASET[["recording"]][[20]][[264]][,4]
  
  A3_angle <- DATASET[["recording"]][[20]][[276]][,1]
  A3_U <- DATASET[["recording"]][[20]][[276]][,2]
  A3_w <- DATASET[["recording"]][[20]][[276]][,3]
  A3_temp <- DATASET[["recording"]][[20]][[276]][,4]
  
  A4_angle <- DATASET[["recording"]][[20]][[288]][,1]
  A4_U <- DATASET[["recording"]][[20]][[288]][,2]
  A4_w <- DATASET[["recording"]][[20]][[288]][,3]
  A4_temp <- DATASET[["recording"]][[20]][[288]][,4]
  
  A5_angle <- DATASET[["recording"]][[20]][[300]][,1]
  A5_U <- DATASET[["recording"]][[20]][[300]][,2]
  A5_w <- DATASET[["recording"]][[20]][[300]][,3]
  A5_temp <- DATASET[["recording"]][[20]][[300]][,4]
  
  A6_angle <- DATASET[["recording"]][[20]][[312]][,1]
  A6_U <- DATASET[["recording"]][[20]][[312]][,2]
  A6_w <- DATASET[["recording"]][[20]][[312]][,3]
  A6_temp <- DATASET[["recording"]][[20]][[312]][,4]
  
  A7_angle <- DATASET[["recording"]][[20]][[324]][,1]
  A7_U <- DATASET[["recording"]][[20]][[324]][,2]
  A7_w <- DATASET[["recording"]][[20]][[324]][,3]
  A7_temp <- DATASET[["recording"]][[20]][[324]][,4]
  
  A8_angle <- DATASET[["recording"]][[20]][[336]][,1]
  A8_U <- DATASET[["recording"]][[20]][[336]][,2]
  A8_w <- DATASET[["recording"]][[20]][[336]][,3]
  A8_temp <- DATASET[["recording"]][[20]][[336]][,4]
  
  #Rather than being along the bridge A9 is within the tower
  #Anemometer A9 was removed in September 2018 due to a lack of useful data measured
  A9_angle <- DATASET[["recording"]][[20]][[348]][,1]
  A9_U <- DATASET[["recording"]][[20]][[348]][,2]
  A9_w <- DATASET[["recording"]][[20]][[348]][,3]
  A9_temp <- DATASET[["recording"]][[20]][[348]][,4]
  #Return all the extracted data in one easier to use dataframe for R
  return(data.frame(time, H1E_x, H1E_y, H1E_z, H1W_x, H1W_y, H1W_z, 
                    H2W_x, H2W_y, H2W_z,
                    H3E_x, H3E_y, H3E_z, H3W_x, H3W_y, H3W_z,
                    H4E_x, H4E_y, H4E_z, H4W_x, H4W_y, H4W_z,
                    H5E_x, H5E_y, H5E_z, H5W_x, H5W_y, H5W_z,
                    H6E_x, H6E_y, H6E_z, H6W_x, H6W_y, H6W_z,
                    H7E_x, H7E_y, H7E_z, H7W_x, H7W_y, H7W_z,
                    H8E_x, H8E_y, H8E_z,
                    H9E_x, H9E_y, H9E_z, H9W_x, H9W_y, H9W_z,
                    H10E_x, H10E_y, H10E_z, H10W_x, H10W_y, H10W_z,
                    H11E_x, H11E_y, H11E_z, H11W_x, H11W_y, H11W_z,
                    A1_angle, A1_U, A1_w, A1_temp,
                    A2_angle, A2_U, A2_w, A2_temp,
                    A3_angle, A3_U, A3_w, A3_temp,
                    A4_angle, A4_U, A4_w, A4_temp,
                    A5_angle, A5_U, A5_w, A5_temp,
                    A6_angle, A6_U, A6_w, A6_temp,
                    A7_angle, A7_U, A7_w, A7_temp,
                    A8_angle, A8_U, A8_w, A8_temp,
                    A9_angle, A9_U, A9_w, A9_temp))
}

#SECOND VERSION THAT SHOULD WORK FOR DATA POST SEPTEMBER 2018

extractMAT_HARDANGER_v2 <- function(DATASET){
  
  #time from data set, these measurements are in seconds
  time <- DATASET[["recording"]][[18]]
  
  #Relevant data from the main triaxial accelerometers placed INSIDE BRIDGE DECK
  #These measurements are all in g, where 1g ~= 9.81 m/s^2
  H1E_x <- DATASET[["recording"]][[20]][[12]][,1]
  H1E_y <- DATASET[["recording"]][[20]][[12]][,2]
  H1E_z <- DATASET[["recording"]][[20]][[12]][,3]
  
  H1W_x <- DATASET[["recording"]][[20]][[24]][,1]
  H1W_y <- DATASET[["recording"]][[20]][[24]][,2]
  H1W_z <- DATASET[["recording"]][[20]][[24]][,3]
  
  #H2E as a sensor does not exist
  
  H2W_x <- DATASET[["recording"]][[20]][[36]][,1]
  H2W_y <- DATASET[["recording"]][[20]][[36]][,2]
  H2W_z <- DATASET[["recording"]][[20]][[36]][,3]
  
  H3E_x <- DATASET[["recording"]][[20]][[48]][,1]
  H3E_y <- DATASET[["recording"]][[20]][[48]][,2]
  H3E_z <- DATASET[["recording"]][[20]][[48]][,3]
  
  H3W_x <- DATASET[["recording"]][[20]][[60]][,1]
  H3W_y <- DATASET[["recording"]][[20]][[60]][,2]
  H3W_z <- DATASET[["recording"]][[20]][[60]][,3]
  
  H4E_x <- DATASET[["recording"]][[20]][[72]][,1]
  H4E_y <- DATASET[["recording"]][[20]][[72]][,2]
  H4E_z <- DATASET[["recording"]][[20]][[72]][,3]
  
  H4W_x <- DATASET[["recording"]][[20]][[84]][,1]
  H4W_y <- DATASET[["recording"]][[20]][[84]][,2]
  H4W_z <- DATASET[["recording"]][[20]][[84]][,3]
  
  H5E_x <- DATASET[["recording"]][[20]][[96]][,1]
  H5E_y <- DATASET[["recording"]][[20]][[96]][,2]
  H5E_z <- DATASET[["recording"]][[20]][[96]][,3]
  
  H5W_x <- DATASET[["recording"]][[20]][[108]][,1]
  H5W_y <- DATASET[["recording"]][[20]][[108]][,2]
  H5W_z <- DATASET[["recording"]][[20]][[108]][,3]
  
  H6E_x <- DATASET[["recording"]][[20]][[120]][,1]
  H6E_y <- DATASET[["recording"]][[20]][[120]][,2]
  H6E_z <- DATASET[["recording"]][[20]][[120]][,3]
  
  H6W_x <- DATASET[["recording"]][[20]][[132]][,1]
  H6W_y <- DATASET[["recording"]][[20]][[132]][,2]
  H6W_z <- DATASET[["recording"]][[20]][[132]][,3]
  
  H7E_x <- DATASET[["recording"]][[20]][[144]][,1]
  H7E_y <- DATASET[["recording"]][[20]][[144]][,2]
  H7E_z <- DATASET[["recording"]][[20]][[144]][,3]
  
  H7W_x <- DATASET[["recording"]][[20]][[156]][,1]
  H7W_y <- DATASET[["recording"]][[20]][[156]][,2]
  H7W_z <- DATASET[["recording"]][[20]][[156]][,3]
  
  H8E_x <- DATASET[["recording"]][[20]][[168]][,1]
  H8E_y <- DATASET[["recording"]][[20]][[168]][,2]
  H8E_z <- DATASET[["recording"]][[20]][[168]][,3]
  
  #H8W as a sensor does not exist
  
  H9E_x <- DATASET[["recording"]][[20]][[180]][,1]
  H9E_y <- DATASET[["recording"]][[20]][[180]][,2]
  H9E_z <- DATASET[["recording"]][[20]][[180]][,3]
  
  H9W_x <- DATASET[["recording"]][[20]][[192]][,1]
  H9W_y <- DATASET[["recording"]][[20]][[192]][,2]
  H9W_z <- DATASET[["recording"]][[20]][[192]][,3]
  
  #Accelerometers INSIDE TOWER
  
  H10E_x <- DATASET[["recording"]][[20]][[204]][,1]
  H10E_y <- DATASET[["recording"]][[20]][[204]][,2]
  H10E_z <- DATASET[["recording"]][[20]][[204]][,3]
  
  H10W_x <- DATASET[["recording"]][[20]][[216]][,1]
  H10W_y <- DATASET[["recording"]][[20]][[216]][,2]
  H10W_z <- DATASET[["recording"]][[20]][[216]][,3]
  
  H11E_x <- DATASET[["recording"]][[20]][[228]][,1]
  H11E_y <- DATASET[["recording"]][[20]][[228]][,2]
  H11E_z <- DATASET[["recording"]][[20]][[228]][,3]
  
  H11W_x <- DATASET[["recording"]][[20]][[240]][,1]
  H11W_y <- DATASET[["recording"]][[20]][[240]][,2]
  H11W_z <- DATASET[["recording"]][[20]][[240]][,3]
  
  #Now for the Anemometers, they capture several different weather indicators
  #angle is in degrees, U (eastward wind-flow) and w (upward wind-flow) are measured in m/s, temp is measured in degrees Celsius
  
  #A1 is located near H1W/H1E
  A1_angle <- DATASET[["recording"]][[20]][[252]][,1]
  A1_U <- DATASET[["recording"]][[20]][[252]][,2]
  A1_w <- DATASET[["recording"]][[20]][[252]][,3]
  A1_temp <- DATASET[["recording"]][[20]][[252]][,4]
  
  A2_angle <- DATASET[["recording"]][[20]][[264]][,1]
  A2_U <- DATASET[["recording"]][[20]][[264]][,2]
  A2_w <- DATASET[["recording"]][[20]][[264]][,3]
  A2_temp <- DATASET[["recording"]][[20]][[264]][,4]
  
  A3_angle <- DATASET[["recording"]][[20]][[276]][,1]
  A3_U <- DATASET[["recording"]][[20]][[276]][,2]
  A3_w <- DATASET[["recording"]][[20]][[276]][,3]
  A3_temp <- DATASET[["recording"]][[20]][[276]][,4]
  
  A4_angle <- DATASET[["recording"]][[20]][[288]][,1]
  A4_U <- DATASET[["recording"]][[20]][[288]][,2]
  A4_w <- DATASET[["recording"]][[20]][[288]][,3]
  A4_temp <- DATASET[["recording"]][[20]][[288]][,4]
  
  A5_angle <- DATASET[["recording"]][[20]][[300]][,1]
  A5_U <- DATASET[["recording"]][[20]][[300]][,2]
  A5_w <- DATASET[["recording"]][[20]][[300]][,3]
  A5_temp <- DATASET[["recording"]][[20]][[300]][,4]
  
  A6_angle <- DATASET[["recording"]][[20]][[312]][,1]
  A6_U <- DATASET[["recording"]][[20]][[312]][,2]
  A6_w <- DATASET[["recording"]][[20]][[312]][,3]
  A6_temp <- DATASET[["recording"]][[20]][[312]][,4]
  
  A7_angle <- DATASET[["recording"]][[20]][[324]][,1]
  A7_U <- DATASET[["recording"]][[20]][[324]][,2]
  A7_w <- DATASET[["recording"]][[20]][[324]][,3]
  A7_temp <- DATASET[["recording"]][[20]][[324]][,4]
  
  A8_angle <- DATASET[["recording"]][[20]][[336]][,1]
  A8_U <- DATASET[["recording"]][[20]][[336]][,2]
  A8_w <- DATASET[["recording"]][[20]][[336]][,3]
  A8_temp <- DATASET[["recording"]][[20]][[336]][,4]
  
  #Rather than being along the bridge A9 is within the tower
  #Anemometer A9 was removed in September 2018 due to a lack of useful data measured
  #Return all the extracted data in one easier to use dataframe for R
  return(data.frame(time, H1E_x, H1E_y, H1E_z, H1W_x, H1W_y, H1W_z, 
                    H2W_x, H2W_y, H2W_z,
                    H3E_x, H3E_y, H3E_z, H3W_x, H3W_y, H3W_z,
                    H4E_x, H4E_y, H4E_z, H4W_x, H4W_y, H4W_z,
                    H5E_x, H5E_y, H5E_z, H5W_x, H5W_y, H5W_z,
                    H6E_x, H6E_y, H6E_z, H6W_x, H6W_y, H6W_z,
                    H7E_x, H7E_y, H7E_z, H7W_x, H7W_y, H7W_z,
                    H8E_x, H8E_y, H8E_z,
                    H9E_x, H9E_y, H9E_z, H9W_x, H9W_y, H9W_z,
                    H10E_x, H10E_y, H10E_z, H10W_x, H10W_y, H10W_z,
                    H11E_x, H11E_y, H11E_z, H11W_x, H11W_y, H11W_z,
                    A1_angle, A1_U, A1_w, A1_temp,
                    A2_angle, A2_U, A2_w, A2_temp,
                    A3_angle, A3_U, A3_w, A3_temp,
                    A4_angle, A4_U, A4_w, A4_temp,
                    A5_angle, A5_U, A5_w, A5_temp,
                    A6_angle, A6_U, A6_w, A6_temp,
                    A7_angle, A7_U, A7_w, A7_temp,
                    A8_angle, A8_U, A8_w, A8_temp))
}


