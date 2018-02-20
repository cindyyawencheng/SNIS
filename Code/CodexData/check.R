



 
setwd(paste0(pathData, '/Codex'))
metadata= read.csv('metadata.csv', stringsAsFactors = FALSE)
 
setwd(paste0(pathData, '/Codex'))
FOLDERS = list.files()[1:38]


filesToRemoveAll = c()


for (folder in 1:length(FOLDERS)){
 
	print(paste('Now starting in folder', FOLDERS[folder], sep = ' '))
 

	# set working directory to where txt files are
	setwd(paste0(pathData, '/Codex/', FOLDERS[folder], '/txt/Archive'))
	files = list.files()
    files = files[grep('txt', files)]
    files = gsub('txt', 'pdf', files)
 
 
    metafiles = unlist(metadata[which(metadata$Folder ==FOLDERS[folder]& metadata$reportEN == 1), c('reportID', 'agendaID')])
   
    filesToRemove = setdiff(files, metafiles)
 


    if (length(filesToRemove)>0){
    filesToRemoveDF = data.frame(filesToRemove = filesToRemove , Folder = FOLDERS[folder])
	filesToRemoveAll = rbind(filesToRemoveAll, filesToRemoveDF)
	}

}
