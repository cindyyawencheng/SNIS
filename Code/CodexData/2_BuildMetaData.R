# compiles meta data for codex files

if(Sys.info()['user'] == "cindycheng"){
source('/Users/cindycheng/Dropbox/Documents/SNIS/Code/setup.R')}
 
if(Sys.info()['user'] == "tranganhdo"){
        source('/Users/tranganhdo/Dropbox/snis/Code')
}

###################################### 

###################################### 

### combine meta data for all years
setwd(paste0(pathData, '/Codex'))

FOLDERS = list.files()
idALL = c()

for ( folder in 1:38){
	print (FOLDERS[folder])
	setwd(paste(c(pathData, 'Codex',  FOLDERS[folder]), collapse = '/'))
	files = list.files()
	id = read.csv(files[grep('.csv', files)][1], stringsAsFactors = F)
	id$Folder = FOLDERS[folder]

	idALL = rbind( idALL, id) 

}
 

#check agenda and reports scraped properly
which(is.na(idALL$hrefEnAgenda))
which(is.na(idALL$hrefEnReport))

###################################### 

###################################### 
# clean place names

idALL$Place = gsub("([a-z])([A-Z])", "\\1 \\2", idALL$Place)

idALL$Place[which(idALL$Place %in% c('Bras\303\255lia', 'Bras\314_lia Brazil'))] = 'Brazilia Brazil'
idALL$Place[which(idALL$Place %in% c('Jeju (City)Republic of Korea'))] = 'Jeju Republic of Korea'
idALL$Place[which(idALL$Place %in% c('Rome, FAO Headquarters Italy'))] = 'Rome Italy'
idALL$Place[which(idALL$Place %in% c('San Jos\303\251Costa Rica', 'San Jos\325\251Costa Rica', 'San Jos\355\251Costa Rica'))] = 'San Jose Costa Rica'
idALL$Place[which(idALL$Place %in% c('Neuch\303\242tel Switzerland', 'Neuch\314\242tel Switzerland'))] = 'Neuchatel Switzerland'



###################################### 

###################################### 

###  create IDs for reports and agendas from urls
# for '1964', '1965', '1966', '1978' ,'1979', '1989', '2003', '2006', '2007', '2008', '2009' , '2011', '2012', '2013', '2014' used -19 characters because of redundancies in file names
# for 1974, used -23 characters because of redundancies in file names for shorter filenames
 
idALL$reportID = substr(idALL$hrefEnReport, nchar(idALL$hrefEnReport) - 12, nchar(idALL$hrefEnReport))
years_expand19 = c('1964', '1965', '1966', '1978' ,'1979', '1989', '2003', '2006', '2007', '2008', '2009' , '2011', '2012', '2013', '2014')
idALL$reportID[which(idALL$Folder %in% years_expand19 )] = 
substr(idALL$hrefEnReport[which(idALL$Folder %in% years_expand19 )], 
nchar(idALL$hrefEnReport[which(idALL$Folder %in% years_expand19 )]) - 18,
 nchar(idALL$hrefEnReport[which(idALL$Folder %in% years_expand19 )]))

idALL$reportID[which(idALL$Folder %in% c('1974'))] = substr(idALL$hrefEnReport[which(idALL$Folder %in% c('1974'))], nchar(idALL$hrefEnReport[which(idALL$Folder %in% c('1974'))]) - 22, nchar(idALL$hrefEnReport[which(idALL$Folder %in% c('1974'))]))
idALL$agendaID = substr(idALL$hrefEnAgenda, nchar(idALL$hrefEnAgenda) - 12, nchar(idALL$hrefEnAgenda))
idALL$agendaID[which(idALL$Folder %in% years_expand19 )] = 
substr(idALL$hrefEnAgenda[which(idALL$Folder %in% years_expand19 )], 
nchar(idALL$hrefEnAgenda[which(idALL$Folder %in% years_expand19 )]) - 18,
 nchar(idALL$hrefEnAgenda[which(idALL$Folder %in% years_expand19 )]))
idALL$agendaID[which(idALL$Folder %in% c('1974'))] = substr(idALL$hrefEnAgenda[which(idALL$Folder %in% c('1974'))], 
			nchar(idALL$hrefEnAgenda[which(idALL$Folder %in% c('1974'))]) - 22, nchar(idALL$hrefEnAgenda[which(idALL$Folder %in% c('1974'))]))

## note that grabbing only 13 characters is insufficient to differentiate between the urls for 'CCFAC28 Codex Committee on Food Additives and Contaminants' and 'CCFAC29 Codex Committee on Food Additives and Contaminants'
# rather than rename all of the 1997 files to be 19 characters long, I just made the url for 'CCFAC28 Codex Committee on Food Additives and Contaminants' to be 19 characters long 
idALL$reportID[which(idALL$Meeting ==  'CCFAC28 Codex Committee on Food Additives and Contaminants' )] = 
substr(idALL$hrefEnReport[which(idALL$Meeting ==  'CCFAC28 Codex Committee on Food Additives and Contaminants' )], 
nchar(idALL$hrefEnReport[which(idALL$Meeting ==  'CCFAC28 Codex Committee on Food Additives and Contaminants' )]) - 18,
nchar(idALL$hrefEnReport[which(idALL$Meeting ==  'CCFAC28 Codex Committee on Food Additives and Contaminants' )]))

###################################### 

###################################### 

### Codex duplicates/errors --- enquire with codex as to whether they have the right meeting reports to upload
 
# Codex enters duplicate entries ; that is it posts the same meeting in multiple year 'folders'
idALL$CodexError = 0
idALL$CodexError[which(idALL$reportID  == '01%252Fal65_15e.pdf' & idALL$Folder == 1966)]= 1 # duplicate entry for CCM1 Codex Committee on Meat
idALL$CodexError[which(idALL$reportID  == '-02%252Fal65_2e.pdf' & idALL$Folder == 1966)]= 1 # duplicate entry for CCEURO2 FAO/WHO Coordinating Committee for Europe 
idALL$CodexError[which(idALL$reportID  == '-01%252Fal65_9e.pdf' & idALL$Folder == 1966)]= 1 # duplicate entry for CCGP1 Codex Committee on General Principles
idALL$CodexError[which(idALL$reportID  == '-01%252Fal65_9e' & idALL$Folder == 1965)]= 1 # duplicate entry for CCGP1 Codex Committee on General Principles
idALL$CodexError[which(idALL$reportID  == '03%252Fal65_30e.pdf' & idALL$Folder == 1966)]= 1 # duplicate entry for CAC3 Codex Alimentarius Commission Principles

 




# Codex uploads wrong document (for another meeting) and uses unique url ending for these documents
	# *** FIXED *** : 
	# idALL$CodexError[which(idALL$reportID == '16%252Fal79_13e.pdf' )]= 1 # supposed to be the report for CCFH16 Codex Committee on Food Hygiene; however Codex uploaded the report for CCFH15 Codex Committee on Food Hygiene
		idALL[c(grep('CCFH16', idALL$Meeting), grep('16%252Fal79_13e.pdf', idALL$reportID)),] # Codex Secretariat fixed this error and I uploaded the correct pdf in its stead

	# idALL$CodexError[which(idALL$reportID ==  'al93_24Ae.pdf'  & idALL$Folder == 1991)]= 1 # supposed to be the report for  CCPR23 Codex Committee on Pesticide Residues; however Codex uploaded the report for CCPR25 Codex Committee on Pesticide Residues
		idALL[unique(c(grep('CCPR23', idALL$Meeting), grep( 'al91_24Ae.pdf', idALL$reportID))),]# Codex Secretariat fixed this error and I uploaded the correct pdf in its stead

	# idALL$CodexError[which(idALL$reportID == '714-08%252Fal72_22e.pdf')]= 1 # supposed to be the report for CCFL8 Codex Committee on Food Labelling; however Codex uploaded the report for CCFL7 Codex Committee on Food Labelling
		idALL[unique(c(grep('CCFL8', idALL$Meeting), grep('714-08%252Fal74_22e.pdf', idALL$reportID))),]# Codex Secretariat fixed this error and I uploaded the correct pdf in its stead


	# idALL$CodexError[which(idALL$reportID %in% c('Fal32_42e.pdf'))]= 1 # supposed to be the report for  TFAMR3 Ad hoc Codex Intergovernmental Task Force on Antimicrobial Resistance; however Codex uploaded report for  TFAMR2 Ad hoc Codex Intergovernmental Task Force on Antimicrobial Resistance
		idALL[c(grep('TFAMR3', idALL$Meeting), grep('Fal32_42e.pdf', idALL$reportID)),] # Codex Secretariat fixed this error and I uploaded the correct pdf in its stead

# still need to double check
	# idALL$CodexError[which(idALL$reportID %in% c('Fcx73_16e.pdf'))]= 1 # supposed to be the report for  CGECPMMP15 Joint FAO/WHO Committee of Government Experts on the Code of Principles Concerning Milk and Milk Products; however Codex uploaded report for  CGECPMMP16 Joint FAO/WHO Committee of Government Experts on the Code of Principles Concerning Milk and Milk Products
		idALL[c(grep('CGECPMMP15', idALL$Meeting), grep('Fcx73_16e.pdf', idALL$reportID)),] # Codex Secretariat fixed this error and I uploaded the correct pdf in its stead

	# idALL$CodexError[which(idALL$reportID %in% c('16%252Fal89_16e.pdf'))]= 1 # supposed to be the report for CCMAS16 Codex Committee on Methods of Analysis and Sampling; however Codex uploaded report for CCPMPP14 Codex Committee on Processed Meat and Poultry Products 
		idALL[c(grep('CCMAS16', idALL$Meeting), grep('16%252Fal89_23e.pdf', idALL$reportID)),] # Codex Secretariat fixed this error and I uploaded the correct pdf in its stead

	# idALL$CodexError[which(idALL$reportID %in% c('712-10%252Fal74_10e.pdf'))]= 1 # supposed to be the report for CCFH10 Codex Committee on Food Hygiene; however Codex uploaded report for CCCPC10 Codex Committee on Cocoa Products and Chocolate
		idALL[c(grep('CCFH10', idALL$Meeting), grep('712-10%252Fal74_13e.pdf', idALL$reportID)),] # Codex Secretariat fixed this error and I uploaded the correct pdf in its stead and changed the url accordingly

	# idALL$CodexError[which(idALL$reportID %in% c('Fal04_33e.pdf') & idALL$Folder == 2005)]= 1 # supposed to be the report for CCGP21 Codex Committee on General Principles; however Codex uploaded report for CCGP19 Codex Committee on General Principles
		idALL[c(grep('CCGP21', idALL$Meeting), grep('Fal04_33e.pdf', idALL$reportID)),] # Codex Secretariat fixed this error and I uploaded the correct pdf in its stead

# still need to doublecheck
	# idALL$CodexError[which(idALL$reportID %in% c('Fal89_30e.pdf'))]= 1 # supposed to be the report for CCVP1 Codex Committee on Vegetable Proteins; however Codex uploaded report for CCVP5 Codex Committee on Vegetable Proteins
		idALL[c(grep('CCVP1', idALL$Meeting), grep('Fal81_30e.pdf', idALL$reportID)),] # Codex Secretariat fixed this error and I uploaded the correct pdf in its stead and changed the url accordingly
# still need to doublecheck
	# idALL$CodexError[which(idALL$reportID %in% c('l89_35e_Part-II.pdf'))]= 1 # supposed to be the report for CCFFV1 Codex Committee on Fresh Fruits and Vegetables but is only part II of the report
		idALL[c(grep('CCFFV1 ', idALL$Meeting), grep('l89_35e_Part-II.pdf', idALL$reportID)),] # Codex Secretariat fixed this error and I uploaded the correct pdf in its stead


	# idALL$CodexError[which(idALL$reportID %in% c('03%252Fal65_30e.pdf'))]= 1 # supposed to be the report for CAC3 Codex Alimentarius Commission but is report for CCGP1 Codex Committee on General Principles
		idALL[c(grep('CAC3 Codex Alimentarius ', idALL$Meeting), grep('03%252Fal65_30e', idALL$reportID)),] # Codex Secretariat fixed this error and I uploaded the correct pdf in its stead

	# idALL$CodexError[which(idALL$reportID == "Fal04_33e.pdf" & idALL$Folder == 2005)] = 1 # supposed to be report for CCGP21 Codex Committee on General Principles, instead Codex uploaded the report for CCGP19 Codex Committee on General Principles
		idALL[c(grep('CCGP21', idALL$Meeting), grep('Fal28_33e.pdf', idALL$reportID)),] # Codex Secretariat fixed this error and I uploaded the correct pdf in its stead

		
	
	# idALL$CodexError[which(idALL$reportID %in% c('-01%252Fal64_2e.pdf' ))]= 1 # supposed to be the report for CCEXEC1 Executive Committee of the Codex Alimentarius Commission; however Codex uploaded report for CCEXEC2 Executive Committee of the Codex Alimentarius Commission; also this was also already uploaded in 1963
		idALL[which(idALL$reportID %in% c('%2BReport.pdf' )),] # Codex Secretariat fixed this error, I uploaded the correct pdf and adjusted the name of the file to reflect the new url name accordingly
		

# **** Not fixed ********
		idALL$CodexError[which(idALL$reportID %in% c('01%252Fal66_15e.pdf'))] = 1 # supposed to be the report for CCPMPP1 Codex Committee on Processed Meat and Poultry Products; however Codex uploaded report for CCM2 Codex Committee Pit Meat and Meat Products
		 
### need to check all below


## Agenda errors -- ignore 
#idALL$CodexError[which(idALL$agendaID %in% c('Fnf03_01e.pdf'))]= 1 # supposed to be the agenda for CCNFSDU25 Codex Committee on Nutrition and Foods for Special Dietary Uses; however Codex uploaded report for CCNFSDU24 Codex Committee on Nutrition and Foods for Special Dietary Uses

#idALL$CodexError[which(idALL$agendaID %in% c('04%252Fbt03_01e.pdf'))]= 1 # supposed to be the agenda for TFFBT4 Ad Hoc Intergovernmental Task Force on Food Derived from Biotechnology ; however Codex uploaded agenda for TFFBT4 Ad Hoc Intergovernmental Task Force on Food Derived from Biotechnology

# idALL$CodexError[which(idALL$agendaID %in% c('01%252Ffh45_01e.pdf'))]= 1 # supposed to be the agenda for CCSCH1 Codex Committee on Spices and Culinary Herbs ; however Codex uploaded agenda for CCFH45 Codex Committee on Food Hygiene 

# Codex uploads wrong document (appendix/other miscellanea) and uses unique url ending for these documents
# *** FIXED *** :
# idALL$CodexError[which(idALL$reportID == 'AnnexVIII.pdf')] = 1 # supposed to be report for CCFFP14 Codex Committee on Fish and Fishery Products, instead uploaded Appendix VIII PROPOSED DRAFT CODE
	idALL[c(grep('CCFFP14', idALL$Meeting), grep('AnnexVIII.pdf', idALL$reportID)),] # Codex Secretariat fixed this error and I uploaded the correct pdf in its stead

# idALL$CodexError[which(idALL$reportID == '_AnnexVII.pdf')] = 1 # supposed to be report for CCFFP15 Codex Committee on Fish and Fishery Products, instead uploaded Appendix VII APPENDIX VII ??? DRAFT CODE OF PRACTICE 
	idALL[c(grep('CCFFP15 ', idALL$Meeting), grep('_AnnexVII.pdf', idALL$reportID)),] # Codex Secretariat fixed this error and I uploaded the correct pdf in its stead

# idALL$CodexError[which(idALL$reportID == '4Be_Add.1.pdf')] = 1 # supposed to be report for CCPR17 Codex Committee on Pesticide Residues, instead uploaded addendum to report for CCPR17 Codex Committee on Pesticide Residues
	idALL[c(grep('CCPR17 ', idALL$Meeting), grep('4Be_Add.1.pdf', idALL$reportID)),] # Codex Secretariat fixed this error and I uploaded the correct pdf in its stead

# idALL$CodexError[which(idALL$reportID == '_Annex_II.pdf')] = 1 # supposed to be report for CCLAC5 FAO/WHO Coordinating Committee for Latin America and the Caribbean, instead uploaded Appendix II
	idALL[c(grep('CCLAC5 ', idALL$Meeting), grep('_Annex_II.pdf', idALL$reportID)),] # Codex Secretariat fixed this error and I uploaded the correct pdf in its stead

# idALL$CodexError[which(idALL$reportID == '30e_Add.1.pdf')] = 1 # supposed to be report for CCFICS1 Codex Committee on Food Import and Export Inspection and Certification Systems, uploaded revised draft terms of reference instead
	idALL[c(grep('CCFICS1 ', idALL$Meeting), grep('30e_Add.1.pdf', idALL$reportID)),] # Codex Secretariat fixed this error and I uploaded the correct pdf in its stead

# idALL$CodexError[which(idALL$reportID == '06%252Fbt06_01e.pdf')] = 1 # supposed to be report for TFFBT6 Ad Hoc Intergovernmental Task Force on Food Derived from Biotechnology, instead uploaded agenda for TFFBT6 Ad Hoc Intergovernmental Task Force on Food Derived from Biotechnology 
	idALL[c(grep('TFFBT6', idALL$Meeting), grep('06%252Fbt06_01e.pdf', idALL$reportID)),] # Codex Secretariat fixed this error and I uploaded the correct pdf in its stead

# idALL$CodexError[which(idALL$reportID == 'IV_Corr_e.pdf')] = 1 # supposed to be report for CCLAC7 FAO/WHO Coordinating Committee for Latin America and the Caribbean; instead uploaded code of hygiene
	idALL[c(grep('CCLAC7', idALL$Meeting), grep('IV_Corr_e.pdf', idALL$reportID)),] # Codex Secretariat fixed this error and I uploaded the correct pdf in its stead


# Codex uploads wrong reportIDument but uses same url ending for these reportIDuments

# idALL$CodexError[which(idALL$reportID == "2Fal69_3e.pdf" & idALL$Folder == 1968)] = 1 # supposed to be report for CCEURO5 FAO/WHO Coordinating Committee for Europe, instead Codex uploaded the report for CCEXEC12 Executive Committee of the Codex Alimentarius Commission
	idALL[c(grep('CCEURO5', idALL$Meeting), grep('2Fal69_3e.pdf', idALL$reportID)),] # Codex Secretariat fixed this error and I uploaded the correct pdf in its stead

# idALL$CodexError[which(idALL$reportID == "2Fal69_9e.pdf" & idALL$Folder == 1968)] = 1 # supposed to be report for CCGP2 Codex Committee on General , instead Codex uploaded the report for CCGP3 Codex Committee on General 
	idALL[c(grep('CCGP2 ', idALL$Meeting), grep('2Fal69_9e.pdf', idALL$reportID)),] # Codex Secretariat fixed this error and I uploaded the correct pdf in its stead

# idALL$CodexError[which(idALL$reportID == "2Fal95_4e.pdf" & idALL$Folder == 1991)] = 1 # supposed to be report for CCEXEC38 Executive Committee of the Codex Alimentarius Commission , instead Codex uploaded the report for CCEXEC41 Executive Committee of the Codex Alimentarius Commission
	idALL[c(grep('CCEXEC38', idALL$Meeting), grep('2Fal95_4e.pdf', idALL$reportID)),] # Codex Secretariat fixed this error and I uploaded the correct pdf in its stead


# idALL$CodexError[which(idALL$reportID == "al93_24Ae.pdf" & idALL$Folder == 1991)] = 1 # supposed to be report for CCPR23 Codex Committee on Pesticide Residues, instead Codex uploaded the report for Codex Committee on Pesticide Residues          
	idALL[c(grep('CCPR23', idALL$Meeting), grep('al91_24Ae.pdf', idALL$reportID)),] # Codex Secretariat fixed this error and I uploaded the correct pdf in its stead


# idALL$CodexError[which(idALL$reportID == "2Fal64_2e.pdf" & idALL$Folder == 1963)] = 1 # supposed to be report for CCEXEC1 Executive Committee of the Codex Alimentarius Commission, instead Codex uploaded the report for CCEXEC2 Executive Committee of the Codex Alimentarius Commission
idALL[which(idALL$reportID == "%2BReport" & idALL$Folder == 1963),] # Codex uploaded the correct meeting report

# check - should only have 5 'errors' because of a) Codex duplicates (4) and b) Codex hasn't fixed the report for CCMPP1 yet
table(idALL$CodexError)
###################################### 

###################################### 
 ## pdf files that could not be turned into txt correctly ####
# idALL$ConversionError = 0
# idALL$ConversionError[which(idALL$reportID == "01%252Fal64_22e.pdf")] = 1 # tesseract converted
# idALL$ConversionError[which(idALL$reportID == "Fal68_21e.pdf")] = 1 # tesseract converted
# idALL$ConversionError[which(idALL$reportID == "cx5_40_3e.pdf ")] = 1 # handcoded participant countries 
# idALL$ConversionError[which(idALL$reportID == "02%252FAL03_40e.pdf")] = 1 #  tesseract converted
# idALL$ConversionError[which(idALL$reportID == "03%252FAL03_38e.pdf ")] = 1 #  tesseract converted
# idALL$ConversionError[which(idALL$reportID == "05%252FAL03_11e.pdf")] = 1  #  tesseract converted, works for countries
# idALL$ConversionError[which(idALL$reportID == "08%252FAL03_16e.pdf")] = 1 # tesseract conversion, works for countries 
# idALL$ConversionError[which(idALL$reportID == "13%252Fal03_31e.pdf")] = 1 # tesseract conversion, handcoded some countries
# idALL$ConversionError[which(idALL$reportID == "2Fal68_5e.pdf")] = 1
# idALL$ConversionError[which(idALL$reportID == "-03%252Fal64_3e.pdf")] = 1 handcoded
# idALL$ConversionError[which(idALL$reportID == "Fal69_11e.pdf")] = 1 #  handcoded participants, ceylon
# idALL$ConversionError[which(idALL$reportID == "Fal68_16e.pdf")] = 1 # HANDCODED PARTICIPANTS countries

 
# 1. Year 1964  : 01%252Fal64_22e.pdf # empty
# 2. Year 1968  : Fal68_21e.pdf # emtpy
# 3. Year 1969  : cx5_40_3e.pdf # emtpy
# 4. Year 2003  : 02%252FAL03_40e.pdf # txt all garbled
# 5. Year 2003  : 03%252FAL03_38e.pdf # txt all garbled
# 6. Year 2003  : 05%252FAL03_11e.pdf # txt all garbled
# 7. Year 2003  : 08%252FAL03_16e.pdf # txt all garbled
# 8. Year 2003  : 13%252Fal03_31e.pdf # txt all garbled
# 9. Year 1968   : 2Fal68_5e.pdf #  some significant problems
# 10. Year 1964 : -03%252Fal64_3e.pdf #  some significant problems
# 11. Year 1969: Fal69_11e.pdf # some significant problems
# 12. Year 1968: Fal68_16e.pdf # some significant problems

# save csv
setwd(paste0(pathData, '/Codex'))
write.csv(idALL, file = 'metadata.csv', row.names = F)
 
# metadata= read.csv('metadata.csv')
#write.csv(metadata, file = 'metadataOld.csv', row.names = F)




## dupecheck
# setwd(paste0(pathData))
# meta = read.csv('metadata.csv', stringsAsFactors = F)

# dupe1 = '01%252Ffh45_01e'
# dupe2 = "45%252Ffh45_01e"
# meta[grep(paste(dupe1, dupe2, sep = '|'), meta$reportID), ]
# meta[grep(paste(dupe1, dupe2, sep = '|'), meta$agendaID), ]
 