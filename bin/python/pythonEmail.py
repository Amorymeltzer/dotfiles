#!/usr/bin/env python

# send a text email from the command line using python
#
# created 6/25/09 by Hank McShane
# version 1.0
#
# The code at the following URL was modified to create this script:
# http://www.cs.cmu.edu/~benhdj/Mac/unix.html#smtpScript
#
# NOTE: if smtp username is "" then code will not use the smtp authentication method
#
# input parameters
#	sys.argv[1] is the sender email address
#	sys.argv[2] is the reciever email address,
#		this can be a comma separated string for multiple recievers
#	sys.argv[3] is the subject text
#	sys.argv[4] is the body text
#	sys.argv[5] is the smtp host
#	sys.argv[6] is the smtp username
#	sys.argv[7] is the smtp password
#	sys.argv[8] is the smtp port
#

import smtplib, email, sys, time

# check to make sure the number of arguments is correct
if len(sys.argv) != 9:
  print 'Usage: pythonEmail.py <sender> <receiver> <subject> <bodyText> <smptHost> <username> <password> <port>'
  sys.exit(1)

# get the argv variables
sender = sys.argv[1]
receiver = sys.argv[2]
subj = sys.argv[3]
bodyText = sys.argv[4]
smtpHost = sys.argv[5]
username = sys.argv[6] # use "" if no SMTP authentication is required
passwd = sys.argv[7] # ignored if no SMTP authentication is required
port = sys.argv[8] # ignored if no SMTP authentication is required
  
# create a list from the receiver in case we have a comma separated string of multiple receivers
rList = []
rList = receiver.split(',');

# setup the message header
timegmt = time.gmtime(time.time( ))
fmt = '%a, %d %b %Y %H:%M:%S GMT'
datestr = time.strftime(fmt, timegmt)
msg = 'From: %s\nTo: %s\nDate: %s\nSubject: %s\n%s' \
       % (sender, receiver, datestr, subj, bodyText)

# determine if a passworded smpt host is being used and connect as necessary
if username == "":
	server = smtplib.SMTP(smtpHost) # smtp server is not password protected
else:
	server = smtplib.SMTP(smtpHost, port)
	server.login(username, passwd)

failed = server.sendmail(sender, rList, msg)
server.quit() 

# return the status
if failed:
  print 'pythonEmail.py: Failed:', failed
else:
  print 'pythonEmail.py: Finished with no errors.'