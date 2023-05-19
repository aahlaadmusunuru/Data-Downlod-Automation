library(httr)
library(XML)

Download_Data <- function(Maindir, URL, Filter) {
  setwd(Maindir)
  
  # Retrieve the HTML content
  resource <- GET(URL)
  parse <- htmlParse(resource)
  
  # Extract the file URLs
  links <- xpathSApply(parse, path = "//a", xmlGetAttr, "href")
  
  # Filter the file URLs based on the provided filter
  filtered_links <- grep(Filter, links, value = TRUE, ignore.case = TRUE)
  
  # Download the files
  for (link in filtered_links) {
    # Construct the full URL for downloading
    download_url <- ifelse(startsWith(link, "http"), link, paste0(URL, link))
    
    filename <- basename(link)
    
    # Check if the file exists
    if (!file.exists(filename)) {
      # If the file does not exist, download it
      download.file(download_url, destfile = filename, mode = 'wb', quiet = TRUE)
      message(paste("Downloaded", filename))
    } else {
      # If the file exists, skip the download
      message(paste("Skipping download of", filename, "- file already exists"))
    }
  }
}

Download_Data_SET <- Download_Data(Maindir = 'D:/Drought Data/ESA netcdf/New folder',
                                   URL = "https://hub.worldpop.org/geodata/summary?id=50353",
                                   Filter = ".tif")

Download_Data_SET <- Download_Data(Maindir = 'D:/Drought Data/ESA netcdf/New folder',
                                   URL = "https://www.star.nesdis.noaa.gov/pub/corp/scsb/wguo/data/Blended_VH_4km/geo_TIFF/",
                                   Filter = ".tif")

Download_Data_SET <- Download_Data(Maindir = 'D:/Drought Data/ESA netcdf/New folder',
                                   URL = "https://data.chc.ucsb.edu/products/CHIRP/2-monthly/",
                                   Filter = ".tif")
