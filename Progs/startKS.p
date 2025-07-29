/* Program.................startKS.p
   Notes:................. Starts Kservice API. The program creates a json file with settings. Passes the settings to the API and initialise the service
   Author:.................S. Chisoro
   Date:......................26/07/25
   
  */
  
session:DATA-ENTRY-RETURN = TRUE.

DEFINE VARIABLE cJson AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cFile AS CHARACTER NO-UNDO. 
FIND FIRST simctr NO-LOCK NO-ERROR.
FIND FIRST simctr2 NO-LOCK NO-ERROR.
IF NOT AVAILABLE simctr2 THEN DO:
    MESSAGE "KServices not enabled" VIEW-AS ALERT-BOX.
END.
ELSE IF AVAILABLE simctr2 THEN DO:
       cFile  = substring(simctr.repDir,1,(LENGTH(simctr.repDir) - 8)) + "bin\ksetting.json".
       cJson = '~{' + CHR(10).  
       cJson = cJson + '    "environment": "' + simctr2.api_env + '",' + CHR(10).
       cJson = cJson + '    "clientCertificate": "' + simctr2.client_cert_path + '",' + CHR(10).
       cJson = cJson + '    "clientKey": "' + simctr2.client_key_path + '",' + CHR(10).
       cJson = cJson + '    "serverCertificate": "' +  simctr2.server_cert_path + '",' + CHR(10).
       cJson = cJson + '    "companyyCode": "' +  simctr2.COCODE  + '",' + CHR(10).
       cJson = cJson + '    "hostIP": "' +  simctr2.host_ip + '",' + CHR(10).
       cJson = cJson + '    "productionURL": "' +  simctr2.p_url  + '",' + CHR(10).     
       cJson = cJson + '    "testURL": "' +  simctr2.t_url + '",' + CHR(10).
       cJson = cJson + '    "serverCertReq": "' +  simctr2.scr_path + '",' + CHR(10).
       cJson = cJson + '    "receiptPath": "' +  simctr2.rec_path + '",' + CHR(10).
       cJson = cJson + '    "hostPort": ' +  string(simctr2.host_port)  + CHR(10).
       cJson = cJson + '      }' + CHR(10).  
    OUTPUT TO VALUE(cFile).  
    PUT UNFORMATTED cJson.
    OUTPUT CLOSE. 
    /*call startK service*/
END.

