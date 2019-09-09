#' @title stackImages
#' @description Function to stack given images
#' @details The input files should be in tif-format.
#' The output will be a tif-file as well.
#' @aliases stackimages
#' @author Kai Budde
#' @export stackImages
#' @param input_dir A string
#' @param input_files A list of strings
#' @param stackMethod A string

stackImages <- function(input_dir = NULL,
                        input_files = NULL,
                        stackMethod = "average") {

  if(missing(input_dir) && missing(input_files)){
    print(paste("Please call the function either with a directory ",
                "or with a list of files.", sep=""))
    return()
  }

  options(stringsAsFactors = FALSE, warn=-1)

  # ---------------------------------------------------------------------- #
  # ---------------------- Data acquisition ------------------------------ #
  # ---------------------------------------------------------------------- #

  # Save the file names (tifs) ---------------------------------------------
  if(!is.null(input_dir)){
    print("Reading files in directory")
    file_names <- list.files(path = input_dir)
    file_names <- file_names[grepl("tif", file_names)]
  }else{
    print("Reading given files.")
    file_names <- input_files
    input_dir <- gsub("[^/.]+\\.tif", "", input_files[1])
    input_dir <- gsub("/$", "", input_dir)
  }


  # ---------------------------------------------------------------------- #
  # ---------------------- Data manipulation ----------------------------- #
  # ---------------------------------------------------------------------- #

  # Go through every image and add it to the stack

  # i = 1 .. n(images) Go through all images (tifs) ------------------------
  print(paste("Going through every image and adding it to each other."))
  for(i in 1:length(file_names)){
    print(paste("Dealing with file ", file_names[i], ". (It is now ",
                Sys.time(), ".)", sep=""))


    # Save the row number of data.frame which contains the current
    # image.
    if(!is.null(input_files)){
      # Using directly the given files
      image_path <- file_names[i]
    }else{
      # Using files from directory
      image_path <- paste(input_dir, file_names[i], sep="/")
    }



    # Read, manipulate and save pillar/non-pillar parts --------------------

    # Average-mathod
    if(stackMethod == "average"){
      if(i == 1){
        image <- tiff::readTIFF(source = image_path, info = FALSE)
        image <- image / length(file_names)
      }else{
        new_image <- tiff::readTIFF(source = image_path, info = FALSE)
        new_image <- new_image / length(file_names)

        image <- image + new_image
      }
    }else if(stackMethod == "max"){
      if(i == 1){
        image <- tiff::readTIFF(source = image_path, info = FALSE)
      }else{
        new_image <- tiff::readTIFF(source = image_path, info = FALSE)
        dimensions <- dim(image)

        for(i in 1:dimensions[1]){
          for(j in 1:dimensions[2]){
            for(k in 1:dimensions[3]){
              if(image[i,j,k] < new_image[i,j,k]){
                image[i,j,k] <- new_image[i,j,k]
              }
            }
          }
        }

      }

    }else if(stackMethod == "addAndNormalize"){
      if(i == 1){
        image <- tiff::readTIFF(source = image_path, info = FALSE)
      }else{
        new_image <- tiff::readTIFF(source = image_path, info = FALSE)
        image <- image + new_image
      }


    }else{
      print("Please enter a correct type of stackMethod.")
      return(0)
    }


  }

  # Normalize
  if(stackMethod == "addAndNormalize"){
    maxValue <- max(image)
    image <- image / maxValue

  }

  # ---------------------------------------------------------------------- #
  # ---------------------- Data output------------------------------------ #
  # ---------------------------------------------------------------------- #

  dir.create(paste(input_dir, "/stack/", sep=""), showWarnings = FALSE)
  # Save the results
  tiff::writeTIFF(what = image,
                  where = paste(input_dir, "/stack/stackMethod_", stackMethod,
                                ".tif", sep = ""),
                  bits.per.sample = 8L, compression = "none",
                  reduce = TRUE)



  return(image)
}
