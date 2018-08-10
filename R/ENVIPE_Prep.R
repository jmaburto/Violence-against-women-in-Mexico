
# Author: tim
###############################################################################

me <- system("whoami",intern=TRUE)

# * make one of these to set your working directory automatically.
# it can go in most script headers
if (me == "tim"){
	setwd("/home/tim/git/Violence-against-women-in-Mexico")
}

if (me == "sun-sdu\\jmaburto"){
  setwd("C:/Users/jmaburto/Documents/GitHub/Violence-against-women-in-Mexico")
}

# this works on all operating systems that might use different file seperators
dbf.path <- file.path("Data", "ENVIPE")
# make download directory if doesn't exist
if (!dir.exists(dbf.path )){
	dir.create(dbf.path )
}
zip.path <- file.path("Data", "Zips")
# make download directory if doesn't exist
if (!dir.exists(zip.path )){
	dir.create(zip.path )
}


basenames <- c("base_de_datos_envipe_20", "base_de_datos_envipe_",
		"bd_envipe_13"
		)
# create download urls
years <- 2011:2017
# unfortunately naming not regular. Paths regular, but not names.
base.url <- "http://www.beta.inegi.org.mx/contenidos/proyectos/enchogares/regulares/envipe/"
file.names <- c(
		"base_de_datos_envipe_2011_dbf.zip",
		"base_de_datos_envipe_2012_dbf.zip",
		"bd_envipe13_dbf.zip",
		"bd_envipe2014_dbf.zip",
		"bd_envipe2015_dbf.zip",
		"bd_envipe2016_dbf.zip",
		"bd_envipe2017_dbf.zip")
urls <- paste0(base.url, years, "/microdatos/",file.names)

# all(sapply(urls, RCurl:::url.exists)) # TRUE

# now download in bulk

for (i in 1:length(years)){
	zipname   <- paste0("ENVIPE", years[i], ".zip")
	dest      <- file.path(zip.path, zipname)
	download.file(urls[i],destfile=dest)
	
	dbf.dir <- file.path(dbf.path,paste0("ENVIPE", years[i]))
	dir.create(dbf.dir)
	
	unzip(dest, exdir = dbf.dir)

	unlink(dest)
#	download.file(urls[i], destfile = full.path) 
	# take a breather
	Sys.sleep(10)	
}

# NOTE: Data/ENVIPE/ENVIPE2017 puts its stuff in subfolders. 
# (ENVIPE2014/bd_envipe2014/bdenvipe2014)
# Go ahead and manually move those contents to ENVIPE2014


# NOTE: Data/ENVIPE/ENVIPE2017 puts its stuff in a subfolder. 
# Go ahead and manually move it up a level and delete that subfolder.




