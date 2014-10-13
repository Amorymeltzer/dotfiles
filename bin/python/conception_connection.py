#!/usr/bin/python
# -*- coding: utf-8 -*-

##################################################
##                                              ##
## SMBC's 'Conception Connection'               ##
## (http://tinyurl.com/conceptionconncection)   ##
##                                              ##
## by Jonas Betzendahl, March 2013              ##
## (jbetzend[at]techfak.uni-bielefeld.de)       ##
##                                              ##
## Licence:                                     ##
## Creative Commons Zero (Public Domain)        ##
##                                              ##
##################################################

import sys
import datetime
import urllib2

def main():

	print "--- Welcome to the Conception Connection™."
	print "--- You can exit at any prompt by entering 'quit'.\n"

	while True:

		# Get birthday from stdin (a.k.a. 'user')
		userinput = raw_input("Please enter your Birthday (YYYY-MM-DD): ")

		# Let user go is she demands as such.
		quit_strings = ['quit', 'end', 'exit', ':q']
		if userinput in quit_strings:
			print "Bye Bye!"
			sys.exit()
	
		# Check for validity of input
		try:
    			birthday = datetime.date(*map(int, userinput.split('-')))
			conception = birthday - datetime.timedelta(weeks=39)
			
			event = get_arousal_event(conception)
			print "Most likely historical event that aroused your parents:"
			print ("--> " + event)
	
		except ValueError:
			print "That didn't work. Try again, fool!\n"

# Function names I never thought I'd ever use someday.
def get_arousal_event(prob_conception):
	
	workthesis_concept = prob_conception
	hasEvent = False
	iterfactor = 0

	months = ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December']
	l = "None, check back with implementer."
	
	while (hasEvent == False):

		opener = urllib2.build_opener()
		opener.addheaders = [('User-agent', 'SMBC Conception Connection')]
		wikipage = "http://en.wikipedia.org/wiki/" + str(workthesis_concept.year)
		infile = opener.open(wikipage)
		wikilines = infile.readlines()
		infile.close()

		monthnr = workthesis_concept.month - 1
		# compensating for Python's zero-indexing of lists.	

		searchstring = "title=\"" + months[monthnr] + " " + str(workthesis_concept.day)+ "\""
	
		discard = False
		dis_birth = title="Edit section: Births"
		dis_death = title="Edit section: Deaths"
		delim = ' – '
		takeNext = 0

		for line in wikilines:

			if (takeNext == 1):
				takeNext = 2
			elif (takeNext == 2):
				takeNext = 0
				l = line
				hasEvent = True

			# Discard all sections concerning births and deaths.
			if ((dis_birth in line) or (dis_death in line)):
				discard = True

			if ((searchstring in line) and (discard == False)):
			
				if delim in line:	# no multiple events, everything is fine 
					l = line
					hasEvent = True
				else:			# Take the first of all events on this date.
					line 
					takeNext = 1
		
		# In case we didn't find an event, search event closest to date.
		if (hasEvent == False):
			iterfactor += 1
			iterfactor *= (-1)
			workthesis_concept = workthesis_concept + datetime.timedelta(days=iterfactor)


        return wikiclean_line(l)

def wikiclean_line(line):

	# Basically, lose everything in pointy brackets. That's just metatext.

	takestring = ""
	take1 = True
	take2 = True

	for c in line:

		if (c == '<'):
			take1 = False
		elif (c == '>'):
			take1 = True
		elif (c == '['):
			take2 = False
		elif (c == ']'):
			take2= False
		else:
			if ((take1 == True) and (take2 == True)):
				takestring += c

	# Now lose everything up to after the first " - ", if there is one.

	finalstring = takestring
	delim = ' – '

	if delim in takestring:
		finalstring = takestring.split(delim, 1)[1]

	return finalstring

if __name__ == "__main__":
	main()	
