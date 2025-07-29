#api interface to get ZIMRA FDMS device configuration
#Design/Written:....................Simbarashe chisoro
#Date:............................5 December 2024

from flask import Flask, render_template, request, url_for, redirect, session, jsonify
import requests

def d_config(deviceid,model,version,link,server_cert,client_key):
    url = f'{link}/Device/v1/{deviceid}/GetConfig'  # API URL
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
            #verify='C:\\Users\\ADMIN\\server_cert.pem'  # CA certificate for server verification
            #Note: Not sure if this is true mTLS as in the ZIMRA documentation because disabling verification mutual trust is not implemented.
            #To check production version in consultation with ZIMRA
        )
        # Check if the response is empty or not and return accordingly
        if response.status_code != 200:
            return jsonify([{'rsMessage': "We cannot reach the API."}]), response.status_code
        print(jsonify(response.json()))
        return jsonify(response.json())  # Return JSON response
    except Exception as e:
        return jsonify([{'rsMessage': str(e)}]), 500  # Return error as JSON
