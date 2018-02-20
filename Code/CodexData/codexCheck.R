
rm(list = ls())
if(Sys.info()['user'] == "cindycheng"){
source('/Users/cindycheng/Dropbox/Documents/SNIS/Code/setup.R')
}

# years = c(1963:1966, 1968:1972, 1974, 1976, 1978, 1979, 1981, 9183, 1985, 1987, 1989, 1991, 1993, 1995)

years = 1966

setwd(paste0(pathData, '/Codex/', years, '/txt'))
files = list.files()


length(years)
checkTextsAll = list()
checkTessTextsAll = list()
for ( year in 1:length(years)){
 
  setwd(paste0(pathData, '/Codex/', years[year], '/txt'))
  files = list.files()
  checkText = list()
  
  for ( file in 1:length(files)){
    print (file)
    checkText[as.character(files[file])] = paste(readLines(files[file]), collapse = '')
    
    # checkText[as.character(files[file])] = readChar(files[file], file.info(files[file])$size)
  }
  
  setwd(paste0(pathData, '/Codex/', years[year]))
  checkTesseract = grep( 'Tesseract', list.files(setwd(paste0(pathData, '/Codex/', years[year]))))
  if (length(checkTesseract) == 1 ){
    setwd(paste0(pathData, '/Codex/', years[year], '/TesseractTexts'))
    tfiles = list.files()
    tfiles = tfiles[grep('txt', tfiles)]
    
    checkTessText = list()
    for(tfile in 1:length(tfiles)){
      checkTessText[as.character(tfiles[tfile])] = paste(readLines(tfiles[tfile]), collapse = '')

        # readChar(tfiles[tfile], file.info(tfiles[tfile])$size)
    }
}

}


metadata = read.csv(paste0(pathData, '/Codex/metadata.csv'), stringsAsFactors = F)

metadata[grep( '%252Fal65_9e', metadata$reportID), c('reportID', 'Meeting', 'Folder')]
head(metadata$reportID)
names(metadata)
names(checkText)
years
names(checkTessText)
checkText[[12]]

names(checkText)[4]
names(checkTessText)[5]
checkText[names(checkTessText)[6]]

checkTessText[[8]]
names(checkTessText)

names(wiseScrape)
    


1964





remember to recombine texts that anh did as she probably used the incorrect sh before i corrected it


textcleaner -s 20 -o 30 -f 50 -p 1  1964/imagesDeskew/01%252Fal64_22e/jpeg/deskewed/scan_4_deskew.jpg 1964/imagesDeskew/01%252Fal64_22e/jpeg/deskewed/scan_4_deskew_2.jpg

-03%252Fal64_3e - HANCODED
-01%252Fal64_2e -- HandCoded --- note that you have 3 copies of this (02%252Fal64_2e) and Fal64_2e in 1963
01%252Fal64_22e  --- HANDCODED
02%252Fal64_30e


1965
-02%252Fal65_2e
-01%252Fal65_4e.txt - attended by handcoded touched up
"-01%252Fal65_9e.txt" - attended by handcoded until the appendix
"-02%252Fal65_2e.txt" - attended by -- handcoded


1966
-03%252Fal65_9e - attended by handcoded touched up
-02%252Fal65_2e x -- handcoded version in 1965
-03%252Fal66_4e  
01%252Fal66_4Ae	 
02%252Fal66_22e  
04%252Fal66_30e  X tesseract done - maybe handcode particiapnts **** HAS A BUNCH OF DIFF MEETINGS WITHIN, need to split up somehow
09%252Fal66_3Ae
02%252Fal65_2e duplicate
"01%252Fal65_23e.txt" handcoded
"01%252Fal66_4Be.txt" - tesserat and pdftext amalgamation/handcoded
"02%252Fal66_23e.txt" - handcoded
"03%252Fal66_11e.txt" - tesseract and handcoded

1968
Fal68_21e -- HANDCODED REPORT BUT NOT APPENDICES -- states that participant list is not attached
Fal68_24e -- hand touch up? needs special attn
Fal68_35e
Fal68_16e -- HANDCODED PARTICIPANTS

1969
cx5_40_3e --- still pretty bad but handcoded the participants
Fal69_67e
Fal69_11e - handcoded participants, ceylon
Fal69_16e - handcoded some participants
usa u.s.a.

1970
Fal70_43e


2Fal70_3e - no participant list, but participants in first para

1971
Fal71_20e
Fal71_31e
Fal72_20e


1972
2Fal72_3e
al72_20Ae
Fal72_18e  
Fal72_35e

1974
701-10%252Fal74_44e
709-07%252Fal74_19e
712-10%252Fal74_10e
713-10%252Fal74_20e
714-08%252Fal72_22e

1976

al76_13Ae
al76_18Ae
Fal76_24e
Fal76_44e

1978
4%252Fal78_13Ae
09%252Fal78_17e 
09%252Fal78_24e
10%252Fal78_23e
12%252Fal78_41e
13%252Fal78_13e  



1979

02%252Fal79_15e
10%252Fal79_17e # handcoded participants
10%252Fal79_24e
12%252Fal79_25e
13%252Fal79_38e
14%252Fal79_20e
15%252Fal79_13e
16%252Fal79_13e
19%252Fcx78_19e



1981

Fal81_24e
Fal81_39e
Fal81_13e

1983

al83_24Ae
Fcx82_20e
Fal83_26e
Fal83_43e
Fal8312ae	

1985 
2Fal85_4e 	X
al85_13Ae	X
al85_24Ae -- note that lists participants in the front
Fal85_16e
Fal85_18e
Fal85_26e
Fal85_47e

1987
al87_13Ae	X
Fal87_12e	X
Fal87_13e	X
Fal87_24e	X
Fal87_26e	X
Fal87_29e	X
Fal8712ae	X


1989
3%252Fal89_31Ae
09%252Fal89_33e
18%252Fal89_18e
18%252Fal89_40e  
23%252Fal89_13e


1991
Fal91_40e	X


1993
al93_13Ae	X
Fal93_13e	X
Fal9316ae	X

1995
Fal95_13e	X
Fal95_17e	X
Fal95_18e skewed -- DESKEWED
 

# http://unix.stackexchange.com/questions/113715/how-can-i-concatenate-all-files-in-a-directory-together-in-one-command-line-oper
sudo 1995/TesseractTexts/Fal95_29e/* > 1995/TesseractTexts/Fal95_29e/Fal95_29e.txt
ls -1 *.txt | sort -n | while read fn ; do cat "$fn" >> Fal95_29e.txt; done

cat $BASE_DIRECTORY/TesseractTexts/$filename/* > $BASE_DIRECTORY/TesseractTexts/$filename.txt
ls -tr 1995/TesseractTexts/Fal95_29e/*.txt | while read fn ; do cat "$fn" >> 1995/TesseractTexts/Fal95_29e.txt; done


