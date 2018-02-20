replfrom selenium import webdriver
from selenium.webdriver.support.ui import Select 
from selenium.webdriver.support.ui import WebDriverWait 
from selenium.webdriver.support import expected_conditions as EC 
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
from selenium.common.exceptions import TimeoutException
from selenium.common.exceptions import NoSuchElementException  


import urllib2
from bs4 import BeautifulSoup

import os
import shutil
import csv
import re
import math
import time

#####################################################################


#####################################################################
#### Setup

# set download folder
downloadFilePath = "/Users/cindycheng/Dropbox/Documents/SNIS/Data/Codex"

# driver options
chrome_options = webdriver.ChromeOptions()
prefs = {"download.default_directory": downloadFilePath, 
		"download.prompt_for_download":False, 
		"plugins.plugins_disabled": ["Chrome PDF Viewer"]}
chrome_options.add_experimental_option("prefs", prefs)

# initiate webdriver
driver = webdriver.Chrome(executable_path = "/Users/cindycheng/Dropbox/Documents/SNIS/Data/chromedriver" ,  chrome_options=chrome_options)

# Collect all the years that you are going to scrape
page_to_scrape = "http://www.codexalimentarius.org/meetings-reports/en/"
webpage = urllib2.urlopen(page_to_scrape)
soup = BeautifulSoup(webpage.read())

YEARS = soup.findAll("a", { 'class': 'external-link-new-window yearsLinks' })
nLinks = len(YEARS)
 
#####################################################################


#####################################################################
 
for year in xrange(0, len(YEARS)):
	# save and clean year name
	yearName = YEARS[year].getText()
	yearName = yearName.replace('/', "")

	print yearName

	## Change to new year
	print 'switching webpae to year ' + yearName
	page_to_scrape =  YEARS[year]['href']
	webpage = urllib2.urlopen(page_to_scrape)
	soup = BeautifulSoup(webpage.read())

	print soup

	print "Now extracting table information for year " + yearName

	####### Save Table Informtion  ####### 
	# extract table of information:
	table = soup.find('table', {'id': 'circular-table'})
	subTable = soup.find('table', {'id': 'circular-table'}).findAll('table')
	
	# extract and clean headers
	headers = [header.text for header in table.find_all('th')]
	headers = sum([x.split('\n') for x in headers], [])
	headers = filter(None, headers)
	headers = [x.encode('UTF8') for x in headers]
	headers.extend([ 'report'  + x  for x in headers[4:10]])
	headers.extend(['hrefEnAgenda', 'hrefEnReport'])

	# find all rows and columns in the table
	rows = []
	for row in table.find_all('tr'):
		rows.append([val.text.encode('utf8') for val in row.find_all('td')])

	# cleans rows
	rows.pop(0) # removes first item in the list
	rows = rows[0::3] # removes empty lists

	# extract information on 'checkmarks'
		#  -- whether a document is available in a particular language is saved in temporarily in 'subrows' and permanently in 'rows'
		#  -- what the href is for any document that is available in English is saved temporarly in 'docLInk' and permananently in 'docLinksSave' 
	docLinksSave = []
	for t in xrange(0, len(subTable)): 
		subrows = []
		docLinks = []
		for subrow in subTable[t].find_all('tr'):
			for td in subrow.find_all('td'):
				for img in td.find_all('img'):
					if img['src'] == '/fileadmin/user_upload/codexalimentarius/images/g_tick_sm12.png':
						subrows.append(1)
					else:
						subrows.append(0)		
			for td in subrow.find_all('td')[0]:
				try:
					docLinks.append(td['href'])	
					docLinksSave.append(td['href'])			
				except KeyError:
					pass
		subrows.extend(docLinks)
	 	rows[t][len(rows[t])-14:len(rows[t])] = subrows


	# make new folder with yearName and set directory to it
	
	print "Now saving table information for year " + yearName
	
	newPath = downloadFilePath + str('/') + yearName
	if not os.path.exists(newPath):
		os.makedirs(newPath)
	os.chdir(newPath)

	# write the table to a csv 
	with open(yearName + '.csv', 'wb') as f:
		writer = csv.writer(f)
		writer.writerow(headers)
		writer.writerows(row for row in rows if row)


	### Save all English language documents START ####

	print "Now downloading English langugage files for " + yearName


	# Go to page of Year T
	driver.get(YEARS[year].get('href'))

	print docLinksSave
	print 'the total number of aritlces is:' + str(len(docLinksSave))
	 
	for doc in xrange(0, len(docLinksSave) ):
		print doc
		print "Saving Doc " + str(docLinksSave[doc])

		wait = WebDriverWait(driver, 120)
		wait.until(EC.presence_of_element_located((By.ID,'circular-table' )))

		xp_str1 = str('//a[@href="')
		xp_str2 = str(docLinksSave[doc])
		xp_str3 = str('"]') 
		xp_str = xp_str1+ xp_str2 + xp_str3

		driver.find_element_by_xpath(xp_str).click()

		# set wd to where files are being downloaded
		os.chdir(downloadFilePath)


		while not os.path.isfile(downloadFilePath+'/download.pdf'):
			time.sleep(1)
		if os.path.isfile(downloadFilePath+'/download.pdf'):

			for filename in os.listdir(downloadFilePath):
				if filename == "download.pdf":
					shutil.move(filename, newPath)
					os.chdir(newPath)

					while not os.path.exists(newPath+'/download.pdf'):
						time.sleep(1)
					if os.path.isfile(newPath+'/download.pdf'):
						# rename downloaded files from 'download' to the last 13 characters of the href
						# for 1979, 1978, 1966, 1965, 1964 used -19 characters because of redundancies in file names
						# for 1974, used -23 characters because of redundancies in file names for shorter filenames
						os.rename(filename, docLinksSave[doc][-13:])
						print filename


	
 
 


