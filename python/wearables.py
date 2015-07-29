#!/usr/bin/env python
import argparse
import logging
import os
import sys
import time
import unittest

start_time = ""
end_time = ""
INFILE = ""
LOG = ""
MODE = ""
EMAIL = ""

"""
Function : 
Args     : 
Returns  : 
"""

def set_up_logging(hard_drive):
    log_path = os.path.join(LOG_DIR, hard_drive, "output.log")
    logging.basicConfig(filename=log_path,
                            format='%(asctime)s %(name)-12s %(levelname)-8s %(message)s', level=logging.INFO)

def parse_command_line():
    parser = argparse.ArgumentParser(
        description = 'This script transfers data from a hard drive to a local computing cluster.')

	parser.add_argument('--infile', '-i', 
		help="A tab delimited file that needs processing")
	parser.add_argument('--log', '-v', 
		help="An output file containing JSON objects to post")
	parser.add_argument('--mode', '-m',default="dev",
		help="Determines which Syapse LIMS host URL to connect to. Default is %(default)s.")
	parser.add_argument('--email', '-e',
		help="Determins whom to send email to.")

    options = parser.parse_args()

	INFILE = args.infile
	LOG = args.log
	MODE = args.mode
	EMAIL = args.email

    return options

class ETL:
	def __init__(self):
		print "ETL Init"
		return None

	def getFilesFromBox(self):
		pass

	def transformDataFiles(self):
		pass

	def getDataFromAppleHealthKit(self):
		pass

class Notification:
	def __init__(self, email):
		self.email = email
		print "Notification " + str(email)
		return None 

class OutcomesTest(unittest.TestCase):
    def testPass(self):
        return

    def testFail(self):
        self.failIf(True)

    def testError(self):
        raise RuntimeError('Test error!')

if __name__ == "__main__":
	start_time = time.time()

	options = parse_command_line()

	etl = ETL()
	notification = Notification(EMAIL)

	end_time = time.time()
	duration = end_time - start_time
	print("DONE: Completed in %.2f seconds" % duration)

'''
CRITICAL
ERROR
WARNING
INFO
DEBUG

logger = logging.getLogger()
logger.setLevel(logging.DEBUG)
formatter = logging.Formatter('%(asctime)s - %(levelname)s - %(message)s')
fh = logging.FileHandler('log_filename.log')
fh.setLevel(logging.DEBUG)
fh.setFormatter(formatter)
logger.addHandler(fh)

ch = logging.StreamHandler()
ch.setLevel(logging.DEBUG)
ch.setFormatter(formatter)
logger.addHandler(ch)
logger.debug('This is a test log message.')
'''
