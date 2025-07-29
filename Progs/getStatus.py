#api interface to get ZIMRA FDMS device status
#Design/Written:....................Simbarashe chisoro
#Date:............................5 December 2024

from flask import Flask, render_template, request, url_for, redirect, session, jsonify
import requests

#app = Flask(__name__)

def g_status(deviceid,model,version,client_cert,client_key,server_cert,link):
    url = link +"/Device/v1/"+ deviceid +"/GetStatus"  # API URL

    headers = {
        'accept': 'application/json',
        'DeviceModelName': model,
        'DeviceModelVersion': version
    }

    try:
        response = requests.get(
            url,
            headers=headers,
            cert=(server_cert, client_key),
            verify=False

        )

        # Check if the response is empty or not and return accordingly
        if response.status_code != 200:
            return jsonify([{'rsMessage': "We cannot reach the API."}]), response.status_code

        print(response.json())
        return jsonify(response.json())  # Return JSON response

    except Exception as e:
        return jsonify([{'rsMessage': str(e)}]), 500  # Return error as JSON
