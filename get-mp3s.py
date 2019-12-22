# -*- coding: utf-8 -*-

# Sample Python code for youtube.channels.list
# See instructions for running these code samples locally:
# https://developers.google.com/explorer-help/guides/code_samples#python

import os

import google
import google_auth_oauthlib.flow
import googleapiclient.discovery
import googleapiclient.errors
import json
import sys

search_string, count, pageToken = sys.argv[1:]
count = int(count)

scopes = ["https://www.googleapis.com/auth/youtube.readonly"]

def main():
    # Disable OAuthlib's HTTPS verification when running locally.
    # *DO NOT* leave this option enabled in production.
    os.environ["OAUTHLIB_INSECURE_TRANSPORT"] = "1"

    api_service_name = "youtube"
    api_version = "v3"
    client_secrets_file = "client_secret.json"
    creds_file = "credentials.json"

    if not os.path.exists(creds_file):
        # Get credentials and create an API client
        flow = google_auth_oauthlib.flow.InstalledAppFlow.from_client_secrets_file(
            client_secrets_file, scopes)
        credentials = flow.run_console()

        creds_data = {
            'token': credentials.token,
            'refresh_token': credentials.refresh_token,
            'token_uri': credentials.token_uri,
            'client_id': credentials.client_id,
            'client_secret': credentials.client_secret,
            'scopes': credentials.scopes
        }
        #del creds_data['token']
        with open(creds_file, 'w') as outfile:
            json.dump(creds_data, outfile)

    credentials = google.oauth2.credentials.Credentials.from_authorized_user_file(creds_file)
    # print(credentials)

    youtube = googleapiclient.discovery.build(
        api_service_name, api_version, credentials=credentials)

    # request = youtube.channels().list(
    #     part="snippet,contentDetails,statistics",
    #     id="UC_x5XG1OV2P6uZZ5FSM9Ttw"
    # )
    request = youtube.search().list(
            part = "snippet",
            maxResults = count,
            q = search_string,
            pageToken = pageToken
    )
    searchResponse = request.execute()

    videoResponses = []
    for item in searchResponse['items']:
        id = item['id']['videoId']
        request = youtube.videos().list(
                part = "snippet,contentDetails",
                id = id
        )
        videoResponse = request.execute()
        videoResponses += [videoResponse]
        #print(json.dumps(response))

    response = { 'search': searchResponse, 'videos': videoResponses }

    print(json.dumps(response))

if __name__ == "__main__":
    main()
