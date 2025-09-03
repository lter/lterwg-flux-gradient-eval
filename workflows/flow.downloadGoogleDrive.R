# Download the data post filtering:

# -------------- Change this stuff -------------
#DirRepo <- 'C:/Users/csturtevant/Documents/Git/lterwg-flux-gradient' # Relative or absolute path to lterwg-flux-gradient git repo on your local machine. Make sure you've pulled the latest from main!
DirRepo <-"/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval"
setwd(DirRepo)
localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'

email <- 'sparklelmalone@gmail.com'
googledrive::drive_auth(email = TRUE) 
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3") # The Data 
data_folder <- googledrive::drive_ls(path = drive_url)


# Download necessary files from Google Drive ####
googledrive::drive_auth(email = email) # Likely will not work on RStudio Server. If you get an error, try email=TRUE to open an interactive auth session.

localdir.data <- paste("/data")

dnld_files <- c( "Ameriflux_NEON field-sites.csv","DIEL_SUMMARY.RDATA", "SITES_One2One.Rdata", "Sites_AOP_Summary.Rdata","Site_Attributes.csv", "CanopyInformation.Rdata","canopy_commbined.csv", "flow.CCC_VIZ.R" ) # List of files you want to download

# Download the files : 

for (focal_file in dnld_files){
  
  message('Downloading ',focal_file, ' to ',localdir.data)
  
  file_id <- subset(data_folder , name == focal_file)
  
  pathDnld <- fs::path(localdir.data,focal_file)
  googledrive::drive_download(file = file_id$id, 
                              path = fs::path(localdir.data,focal_file),
                              overwrite = T)
  zip.file <- fs::path(localdir.data,focal_file)
  unzip(zip.file, exdir=localdir.data)
  
}

##### Download filtered data for sites: ####
metadata <- read.csv('/Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv') # has a list of all the sites

# -------------------------------------------------------
site.list <- metadata$Site_Id.NEON %>% unique

# ---------------------------------------------

# Download necessary files from Google Drive
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3")
googledrive::drive_auth(email = email) # Likely will not work on RStudio Server. If you get an error, try email=TRUE to open an interactive auth session.
data_folder <- googledrive::drive_ls(path = drive_url)


if(DnldFromGoogleDrive == TRUE){
  for( site in site.list){
    print(site)
    
    site <- site
    
    paste(site, "_Evaluation.Rdata", sep = "")
    dnld_files <- c()
    
    localdir.site <- paste(localdir,"/", site, sep = "")
    
    for (focal_file in dnld_files){
      
      message('Downloading ',focal_file, ' to ',localdir)
      
      site_folder <- googledrive::drive_ls(path = data_folder$id[data_folder$name==site])
      file_id <- subset(site_folder, name == focal_file)
      
      pathDnld <- fs::path(localdir.site,focal_file)
      googledrive::drive_download(file = file_id$id, 
                                  path = fs::path(localdir.site,focal_file),
                                  overwrite = T)
      zip.file <- fs::path(localdir.site,focal_file)
      unzip(zip.file, exdir=localdir.site)
      
    }
  } }


 