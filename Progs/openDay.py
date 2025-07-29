#api interface to open ZIMRA FDMS device device
#Design/Written:....................Simbarashe chisoro
#Date:............................5 December 2024

from flask import Flask, render_template, request, url_for, redirect, session, jsonify
import requests

def d_openDay(deviceid,model,version,dnumber,fdOpen,client_key,server_cert,link):
    url = link + "/Device/v1/"+ deviceid +"/OpenDay"  # API URL
    headers = {
        'accept': 'application/json',
        'DeviceModelName': model,
        'DeviceModelVersion': version,
        "Content-Type": "application/json"
    }
    dnumber = int(dnumber)
    if dnumber == 0:
        #payload = '{"openDayRequest":[{"fiscalDayOpened": "' + fdOpen + '"}]}'
        #dnumber = 101
        payload = {"fiscalDayNo": dnumber, "fiscalDayOpened":  fdOpen }
    else:
        payload = {"fiscalDayNo": dnumber, "fiscalDayOpened":  fdOpen }
    try:
        response = requests.post(
            url,
            headers=headers,
            json=payload,
            cert=(server_cert, client_key),
            verify=False
        )
        # Check if the response is empty or not and return accordingly
        if response.status_code != 200:
            #return jsonify([{'rsMessage': "We cannot reach the API."}]), response.status_code
            print(response.json())
            return jsonify(response.json()), response.status_code
        print(response.json())
        return jsonify(response.json())  # Return JSON response

    except Exception as e:
        print(str(e))
        return jsonify([{'rsMessage': str(e)}]), 500  # Return error as JSON
