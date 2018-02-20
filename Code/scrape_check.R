# This R file double checks to make sure that all files are downloaded and in the proper folders

path = '/Users/cindycheng/Dropbox/Documents/SNIS/Data/Codex/'

if(Sys.info()['user'] == "tranganhdo"){'/Users/tranganhdo/Dropbox/snis/Code')
}

setwd(path)


folders = list.files()

yearProbs = c()
n_tableProb = c()
n_folderProb = c()
n_hrefProb = c()

for ( f in 1:length(folders)){

	setwd(paste(path, folders[f], sep = '/'))
	table = read.csv(paste(folders[f], 'csv', sep ='.'), stringsAsFactors = F)
	 
	# checks to see all files are downloaded
	n_table = sum(table$EN, table$reportEN)
	href = c(table$hrefEnAgenda[grep('pdf', table$hrefEnAgenda)] , table$hrefEnReport[grep('pdf', table$hrefEnReport)])
	n_href = length(href)
	n_folder = length(list.files(pattern = '.pdf'))

	if (n_href != n_folder){
		print (paste('Discrepancy in number of files for year ', folders[f], sep = ' '))
		yearProbs = c(folders[f], yearProbs)
		n_tableProb = c(n_table, n_tableProb)
		n_folderProb = c(n_folder, n_folderProb)
		n_hrefProb = c(n_href, n_hrefProb)
	} else{
		print(paste('All good! -- no discrepancy in number of files for year ', folders[f], sep = ' '))
	}

	# checks to see that all files are in proper folder
	files =  list.files(pattern = '.pdf')
	nchar_files = nchar(files[1]) -1
	

	hrefPDF = c()
	for (h in 1:length(href)){
	n = nchar(href[h])
	hrefPDF[h] = substr(href[h], n-nchar_files , n)
}

	if (length(setdiff(hrefPDF, files)) >0){
	print (paste('wrong files in ', folders[f], sep = ' '))

	}

	else{
		print (paste('no wrong files in ', folders[f], sep = ' '))
	}
}

cbind(yearProbs, n_tableProb, n_folderProb, n_hrefProb)

 
