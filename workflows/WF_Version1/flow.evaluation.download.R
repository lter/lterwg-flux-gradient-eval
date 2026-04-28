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