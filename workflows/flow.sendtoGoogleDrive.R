# Add files to google drive

metadata <- read.csv('/Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv') # has a list of all the sites
localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'
mail <- 'sparklelmalone@gmail.com'
googledrive::drive_auth(email = TRUE) 
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3") # The 

fileSave <- fs::path('/Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv')
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)


fileSave <- fs::path(file.path(paste(localdir, "canopy_commbined.csv", sep="/")))
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)


fileSave <- fs::path(DirRepo,'workflows/flow.evaluation_SITELIST.R')
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

