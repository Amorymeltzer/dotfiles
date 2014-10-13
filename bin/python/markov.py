#
# Markov chain code for constrained writing.
# This is a quick hack, put together in an hour or two.
# 
#
# First, you need a list of three-word sequences that fit your constraint.
# The way I generated that was by creating a giant file of lines of my IRC 
# chat logs and running a shell command like this one:
#
#   grep -Po "\b([yuiophjklbnm]+( |$)){3}" chatlines.txt > righthand.txt
#
# Then I launched python, imported markov.py ...
#
# > import markov
#
# ... and ran this command:
#
# > markov.process("righthand.txt")
#

import random
import re

def getTriples(filename):
	lines = []
	with open(filename, "rb") as inf:
		lines = [re.split(" +", i.strip()) for i in inf.readlines()]
	clean = [i for i in lines if len(i)==3]
	return clean

def makeIndex(triples):
	lookup = {}
	for trip in triples:
		pairKey = (trip[0], trip[1])
		if not lookup.has_key(pairKey):
			lookup[pairKey] = set()
		lookup[pairKey].add(trip[2].lower())
	for key in lookup.keys():
		lookup[key] = list(lookup[key])
	return lookup

def makeText(idx):
	start = list(random.choice(idx.keys()))
	options = idx[tuple(start)]
	while(len(options)>0):
		found = False
		options.sort(key = lambda x: len(x)+random.random()*5.0, reverse=True)
		for i in options:
			# print "Trying", i
			if idx.has_key((start[-1], i)) and i not in start and i not in list("bcdefghjklmnopqrstuvwxyz"):
				start.append(i)
				found = True
				break
		if not found:
			start.append(random.choice(options))
			return start
		options = idx[(start[-2], start[-1])]
	return start

def makeLongText(idx, required):
	found = []
	while(True):
		cand = makeText(idx)
		if len(cand) >= required:
			found.append(cand)
			return cand
	return

def process(filename):
	trips = getTriples(filename)
	idx = makeIndex(trips)
	sentences = []
	wantedlength = 0
	for i in range(50):
		sentence = makeLongText(idx, wantedlength)
		print " ".join(sentence)
		if wantedlength < 20 and i%3 == 0:
			wantedlength += 1
		sentences.append(sentence)
	return