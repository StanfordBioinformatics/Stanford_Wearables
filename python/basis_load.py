# Python example
# Loads the table from Google Cloud Storage and prints the table.

import sys
import json

from apiclient.discovery import build
from oauth2client.file import Storage
from oauth2client.client import AccessTokenRefreshError
from oauth2client.client import OAuth2WebServerFlow
from oauth2client.tools import run
from apiclient.errors import HttpError
import httplib2

def loadTable(service, projectId, datasetId, targetTableId, sourceCSV):
  try:
    jobCollection = service.jobs()
    jobData = {
      'projectId': projectId,
      'configuration': {
          'load': {
            'sourceUris': [sourceCSV],
            'schema': {
              'fields': [
                {
                  'name': 'time',
                  'type': 'TIMESTAMP'
                },
                {
                  'name': 'hr',
                  'type': 'FLOAT'
                },
                {
                  'name': 'accel_magnitude',
                  'type': 'INTEGER'
                },
                {
                  'name': 'gsr',
                  'type': 'FLOAT'
                },
                {
                  'name': 'skin_temp',
                  'type': 'FLOAT'
                },
                {
                  'name': 'activity_level',
                  'type': 'STRING'
                },
                {
                  'name': 'activity',
                  'type': 'STRING'
                },
                {
                  'name': 'steps',
                  'type': 'FLOAT'
                },
                {
                  'name': 'calories',
                  'type': 'FLOAT'
                }
              ]
            },
            'destinationTable': {
              'projectId': projectId,
              'datasetId': datasetId,
              'tableId': targetTableId
            },
          }
        }
      }

    insertResponse = jobCollection.insert(projectId=projectId,
                                         body=jobData).execute()

    # Ping for status until it is done, with a short pause between calls.
    import time
    while True:
      job = jobCollection.get(projectId=projectId,
                                 jobId=insertResponse['jobReference']['jobId']).execute()
      if 'DONE' == job['status']['state']:
          print 'Done Loading!'
          return

      print 'Waiting for loading to complete...'
      time.sleep(10)

    if 'errorResult' in job['status']:
      print 'Error loading table: ', pprint.pprint(job)
      return

  except HttpError as err:
    print 'Error in loadTable: ', pprint.pprint(err.resp)


def main(argv):
  # If the credentials don't exist or are invalid, run the native client
  # auth flow. The Storage object will ensure that if successful the good
  # credentials will get written back to a file.
  storage = Storage('bigquery2.dat') # Choose a file name to store the credentials.
  credentials = storage.get()
  if credentials is None or credentials.invalid:
    credentials = run(FLOW, storage)

  # Create an httplib2.Http object to handle our HTTP requests and authorize it
  # with our good credentials.
  http = httplib2.Http()
  http = credentials.authorize(http)

  projectId = "gbsc-gcp-project-wearables"
  datasetId = "gbsc-gcp-project-wearables:wearable"
  targetTableId = "gbsc-gcp-project-wearables:wearable.mps_basis"
  sourceCSV = "/Users/dsalins/Downloads/mpsnyder_9-1-12-3/mpsnyder_20140523.csv"

  service = build('bigquery','v2', http=http)

  loadTable(service, projectId, datasetId, targetTableId, sourceCSV)

if __name__ == '__main__':
  main(sys.argv)
