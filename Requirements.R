# Requirements for R
#Linux
#Gunzip #Caso não encontre o comando em R

# list of necessary packages
packagesList <- c("XML", "RCurl", "downloader", "R.utils")

# checks if the packages are installed if not put the non-istalled in a vector
toInstall <- packagesList[!(packagesList %in% installed.packages()[, "Package"])]

# installs non-istalled packages
if(length(toInstall)){
	install.packages(toInstall)
}

# finally loads packages
for(i in seq_along(packagesList)){
	library(packagesList[i], character.only=T)
}

# end of installation and loading process
