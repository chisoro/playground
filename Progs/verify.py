#api interface to verify ZIMRA FDMS device Tax Payer information
#Design/Written:....................Simbarashe chisoro
#Date:............................5 December 2024

from flask import Flask, render_template, request, url_for, redirect, session, jsonify
import requests

def d_veri(deviceid,ack,dvser,link):
    url = link+ "/Public/v1/" + deviceid +"/VerifyTaxpayerInformation"
    payload = json.dumps({
      "activationKey": ack,
      "deviceSerialNo": dvser
    })
    headers = {
      'Content-Type': 'application/json'
    }
    r = requests.request("POST", url, headers=headers, data=payload)
    if r == "[]":
        return jsonify([{'rsMessage':"We cannot reach the api."}])
    else:
        return jsonify(r.text)
