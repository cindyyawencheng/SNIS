#!/bin/bash

## move pdfs into same folder
# for f in /Users/cindycheng/Dropbox/Documents/SNIS/Data/Codex/*/;
# do
# 	echo $(basename $f)
# 	mkdir "$f/pdf"
# 	mv $(basename $f)/*.pdf $(basename $f)/pdf/
# done

# # move txt files into same folder
# for f in /Users/cindycheng/Downloads/Codex/*/;
# do
# 	echo $(basename $f)
# 	mkdir "$f/txt"
# 	echo $f/*.txt
# 	mv $f/*.txt $f/txt/
# done


# ## move txt folder and participant csvs to same folder as pdfs
# for f in /Users/cindycheng/Downloads/Codex/*/;
# do
# 	echo $(basename $f)
# 	mv $f/txt /Users/cindycheng/Dropbox/Documents/SNIS/Data/Codex/$(basename $f)/
# 	mv $f*.csv /Users/cindycheng/Dropbox/Documents/SNIS/Data/Codex/$(basename $f)/

# done




#### do not use - removes folders
# for f in /Users/cindycheng/Dropbox/Documents/SNIS/Data/Codex/*/;
# do
# 	echo $(basename $f)
# 	rm -r  $f$(basename $f)
# done


### do not use - removes csvs
# for f in /Users/cindycheng/Dropbox/Documents/SNIS/Data/Codex/*/;
# do
# 	echo $f$(basename $f).csv
# 	rm $f$(basename $f).csv
# done

### do not use - renames csvs
for f in /Users/cindycheng/Dropbox/Documents/SNIS/Data/Codex/*/;
do
	# echo $f$(basename $f) "(Cindy Cheng's conflicted copy 2016-02-13).csv"
	# find  $f$(basename $f)" (Cindy Cheng's conflicted copy 2016-02-13).csv"
	mv $f$(basename $f)" (Cindy Cheng's conflicted copy 2016-02-13)".csv $f$(basename $f).csv
done

 

