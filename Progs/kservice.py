#api interface to electronic services including ZIMRA FDMS
#Design/Written:....................Simbarashe chisoro
#Date:............................5 December 2024
from flask import Flask, render_template, request, url_for, redirect, session, jsonify
from flask_login import LoginManager, UserMixin, login_user, logout_user
from flask_bcrypt import bcrypt
import json
import getStatus
import getReg
import getConfig
import verify
import createReceipt
import printout

app = Flask(__name__)
file_path = f'C:\\Users\\ADMIN\\OneDrive\\Documents\\Projects\\Progress\\Simacc\\bin\\ksetting.json' #modify to pick current directory using OS import
with open(file_path, 'r') as file:
    data = json.load(file)

env = data["environment"]
client_cert = data["clientCertificate"]  # Self-signed client certificate not used in this situation
client_key =  data["clientKey"]    # Private key for the client certificate
server_cert = data["serverCertificate"]
ccode = data["companyyCode"]
host_ip = data["hostIP"]
production_link = data["productionURL"]
test_link = data["testURL"]
server_cert_req = data["serverCertReq"]
host_port = data["hostPort"]
receipt_path = data["receiptPath"]
if env == "D":
    link = test_link
else:
    link = production_link

@app.route('/getStatus/<deviceid>/<model>/<version>', methods=['GET'])
def get_status(deviceid,model,version):
    r = getStatus.g_status(deviceid,model,version,client_cert,client_key,server_cert,link)
    return r

@app.route('/register/<deviceid>/<ver>/<ack>/<dvname>', methods=['GET'])
def get_reg(deviceid,ack,dvname,ver):
    r = getReg.d_reg(deviceid,ack,dvname,ver,link,server_cert_req)
    return r

@app.route('/getConfig/<deviceid>/<model>/<version>', methods=['GET'])
def get_conf(deviceid,model,version):
    r = getConfig.d_config(deviceid,model,version,link,server_cert,client_key)
    return r

@app.route('/verify/<deviceid>/<ack>/<dvser>', methods=['GET'])
def get_ver(deviceid,ack,dvser):
    r = verify.d_veri(deviceid,ack,dvser,link)
    return r

@app.route('/openDay/<deviceid>/<model>/<version>/<dnumber>/<fdOpen>', methods=['GET'])
def get_open(deviceid,model,version,dnumber,fdOpen):
    r = openDay.d_openDay(deviceid,model,version,dnumber,fdOpen,client_key,server_cert,link)
    return r

@app.route('/onlinerec/<deviceid>/<version>/<modname>/<refile>', methods=['GET'])
def onlinereceipt(deviceid,version,modname,refile):
    r = createReceipt.on_receipt(deviceid,version,modname,refile,client_cert,client_key,server_cert,link,receipt_path)
    return r

@app.route('/offlinerec/<deviceid>/<version>/<modname>/<refile>', methods=['GET'])
def offlinereceipt(deviceid,version,modname,refile):
    r = createReceipt.of_receipt(deviceid,version,modname,refile,client_cert,client_key,server_cert,link,receipt_path)
    return r

@app.route('/closeday/<deviceid>/<version>/<modname>/<refile>', methods=['GET'])
def closeday(deviceid, version, modname, refile):
    r = createReceipt.zx_report(deviceid,version,modname,refile,client_cert,client_key,server_cert,link,receipt_path)
    return r


@app.route('/print/<filename>', methods=['GET'])
def f_printout(filename):
    r = printout.re_print(filename,receipt_path)
    return r


if __name__ == '__main__':
    #app.run(debug=True)
    app.run(host=host_ip, port=int(host_port), debug=True)
