#api interface to register ZIMRA FDMS device
#Design/Written:....................Simbarashe chisoro
#Date:............................5 December 2024

from flask import Flask, render_template, request, url_for, redirect, session, jsonify
import requests
import csv

def d_reg(deviceid,ack,dvname,ver,link,server_cert_req):
    mycert = csv.reader(open(server_cert_req))
    for row in mycert:
        scr = row[0]

    url = link+"/Public/" + ver + "/" + deviceid +"/RegisterDevice"

    payload = json.dumps({
      "certificateRequest": scr,
      "activationKey": ack
    })
    headers = {
      'Accept': 'application/json',
      'DeviceModelName': dvname,
      'DeviceModelVersion': ver,
      'Content-Type': 'application/json'
    }

    r = requests.request("POST", url, headers=headers, data=payload)

    if r == "[]":
        return jsonify([{'rsMessage':"We cannot reach the api."}])
    else:
        #return r.text
        return r.text
