USING System.Net.Http.*. 
USING System.Environment.
/* Program.................fdmsDC.p
   Notes:................. FDMS Device Creation
   Author:.................S. Chisoro
  */

  
 session:DATA-ENTRY-RETURN = TRUE.
DEFINE STREAM certi.
OUTPUT STREAM certi TO C:\Users\ADMIN\ss_cert.pem.
 &SCOPED-DEFINE wsTitle           "FDMS Device File Maintenance"
 &SCOPED-DEFINE tmptable            fdmsDevice
&SCOPED-DEFINE skey                fdmsDevice.deviceID
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.TName ~
                                        COLUMN-LABEL ' Trade Name ':C
&SCOPED-DEFINE tmpFields      bfr{&tmptable}.TName ~
                                        COLUMN-LABEL ' Trade Name ':C  ~
                              bfr{&tmptable}.deviceID ~
                                        COLUMN-LABEL 'Device ID ':C~
                              bfr{&tmptable}.modelName ~
                                        COLUMN-LABEL ' Model Name ':C ~
                              bfr{&tmptable}.serialNumber ~
                                        COLUMN-LABEL 'Serial Number ':C ~
                              bfr{&tmptable}.activationKey ~
                                            COLUMN-LABEL 'Activation Key':C ~
                               bfr{&tmptable}.modelVersion~
                                        COLUMN-LABEL ' Version ':C~
                               bfr{&tmptable}.devBranchName ~
                                        COLUMN-LABEL  ' Branch ' FORMAT 'X(20)':C 


define rectangle rect-1
     edge-pixels 2 graphic-edge  no-fill
     size 145 by 2.3.

define rectangle rect-2
     edge-pixels 2 graphic-edge  NO-FILL
     size 145 by 24.4.

define rectangle rect-3
     edge-pixels 2 graphic-edge  NO-FILL
     size 55 by 9.5.
define rectangle rect-4
     edge-pixels 2 graphic-edge  NO-FILL
     size 84 by 9.5.
DEF BUTTON btn-devReg LABEL 'REGISTER DEVICE'.
DEF BUTTON btn-devVer LABEL 'VERIFY TAX PAYER'.
DEF BUTTON btn-devConfig LABEL 'GET CONFIGURATION'.
DEF BUTTON btn-devIssue LABEL 'RENEW CERTIFICATE'.
DEF STREAM a.

{varlib.i}

define frame frm-input
    SKIP(0.5)
    simctr.cocode   colon 30 label "Company Code" SKIP(0.5)
    simctr.CONAME   COLON 30 LABEL "Taxpayer Name" SKIP(0.5)
    SIMCTR.REGNO    colon 30 LABEL "VAT Registration Number" SKIP(0.5)
    SIMCTR.BPNO         colon 30     LABEL "TIN Number" SKIP(0.5)
    simctr.phone      COLON 30      LABEL "Phone" SKIP(0.5)
    
   
    rect-3 AT ROW 13.0 COL 3.5
    rect-4 AT ROW 13.0 COL 62
    bfr{&tmptable}.TName   AT ROW 13.2 COL 5 LABEL "Trade Name"  SKIP(0.2)
    bfr{&tmptable}.activationKey COLON 15 LABEL "Activation Key"  SKIP(0.2) 
    {&skey} COLON 15 LABEL "Device ID"  SKIP(0.2)
    bfr{&tmptable}.modelName COLON 15 LABEL "Model Name"  SKIP(0.2)
    bfr{&tmptable}.modelVersion COLON 15 LABEL "Version"  SKIP(0.2)
    bfr{&tmptable}.serialNumber COLON 15 LABEL "Serial Number"  SKIP(0.2)


    bfr{&tmptable}.certificateRe  AT ROW 13.5 COL 63 LABEL "PEM"  VIEW-AS EDITOR SIZE 76 BY 8.5 SKIP(0.2)
    
    rect-2 AT ROW 1 COL 2
    rect-1 AT ROW 25.5 COL 2
    btn-ok AT ROW 26.3 COL  20
    btn-close colon 80 SKIP(0.5)
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d scrollable
         TITLE "FDMS MASTER CONTROL FILE".
    
DEF NEW SHARED FRAME frm-main
     SKIP(0.5)
    simctr.cocode   colon 25 label "Company Code" SPACE(5)
    simctr.CONAME   LABEL "Taxpayer Name" SKIP(0.5)
    SIMCTR.REGNO    colon 25 LABEL "VAT Registration Number" SPACE(5)
    SIMCTR.BPNO          LABEL "TIN Number" SKIP(0.5)
    simctr.phone      COLON 25      LABEL "Phone" SKIP(1)
    brw-{&tmptable}  COLON 5
    btn-add AT ROW 26.7 COL 5
    Space(4) btn-edit
    space(4) btn-del
    SPACE(4) btn-devReg
     SPACE(4) btn-devVer
    SPACE(4) btn-devConfig
    SPACE(4) btn-devIssue
    space(4) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 26 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     /*BGCOLOR 8 FGCOLOR 1 */ SIZE 150 BY 30
    TITLE {&wsTitle} VIEW-AS DIALOG-BOX.

/*API intergration*/
DEFINE VARIABLE HttpClient AS CLASS System.Net.WebClient. 
DEFINE VARIABLE webResponse AS LONGCHAR NO-UNDO. 
DEF VAR txtWebResponse AS CHAR.
DEF VAR txtUnWantedR AS CHAR EXTENT 8.
DEF VAR txtUnWantedR1 AS CHAR EXTENT 3.
DEF VAR rspCode AS CHAR.
DEF VAR rspMessage AS CHAR.

def var operationID  as char.
def var taxPayerName as char.
def var taxPayerTIN as char.
def var vatNumber as char.
def var deviceBranchName as char.
def var city as char.
def var district as char.
def var houseNo as char.
def var province as char.
def var street as char.
def var phone as char.
DEF VAR email AS CHAR.
DEF VAR certif AS CHAR.
DEF VAR txtTaxes AS CHAR.
DEF VAR qrUrl AS CHAR.
DEF VAR dOMode AS CHAR.
DEF VAR txtValid AS CHAR. 
DEF VAR txtSerial AS CHAR.
DEF VAR vTemp AS CHAR.
DEF VAR i AS INTEGER.
DEF VAR taxID AS CHAR.
DEF VAR taxName AS CHAR.
DEF VAR taxPercent AS CHAR.

/* *** Triggers to input frame frm-input*** */
ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
   
    wsid = INT({&skey}:SCREEN-VALUE IN FRAME frm-input).
     IF (btn-ok:LABEL IN  FRAME frm-input) = "Save" THEN DO:
         RUN simip.p.
        wsBtn = "ADD".
        CREATE bfr{&tmptable}. 
          ASSIGN bfr{&tmptable}.deviceID = wsid
                 bfr{&tmptable}.TName = bfr{&tmptable}.TName:SCREEN-VALUE
                 bfr{&tmptable}.activationKey=  bfr{&tmptable}.activationKey:SCREEN-VALUE
                 bfr{&tmptable}.modelName = bfr{&tmptable}.modelName:SCREEN-VALUE
                 bfr{&tmptable}.modelVersion = bfr{&tmptable}.modelVersion:SCREEN-VALUE
                 bfr{&tmptable}.serialNumber = bfr{&tmptable}.serialNumber:SCREEN-VALUE
                 bfr{&tmptable}.certificateRe   =  bfr{&tmptable}.certificateRe:SCREEN-VALUE
                bfr{&tmptable}.DeviceName = lc-host
                 bfr{&tmptable}.UID = varUser
                 bfr{&tmptable}.creDate =NOW.
          RELEASE {&tmptable}.
          APPLY 'close' TO SELF.
         HIDE FRAME frm-input.
         brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
    END.
    ELSE DO:
        /*need to use Siliot's program*/
        wsBtn = "EDIT".
        ASSIGN bfr{&tmptable}.deviceID = wsid
                 bfr{&tmptable}.TName = bfr{&tmptable}.TName:SCREEN-VALUE
                 bfr{&tmptable}.activationKey=  bfr{&tmptable}.activationKey:SCREEN-VALUE
                 bfr{&tmptable}.modelName = bfr{&tmptable}.modelName:SCREEN-VALUE
                 bfr{&tmptable}.modelVersion = bfr{&tmptable}.modelVersion:SCREEN-VALUE
                 bfr{&tmptable}.serialNumber = bfr{&tmptable}.serialNumber:SCREEN-VALUE
                 bfr{&tmptable}.certificateRe   =  bfr{&tmptable}.certificateRe:SCREEN-VALUE.
        RELEASE {&tmptable}.
        APPLY 'close' TO SELF.
        HIDE FRAME frm-input.
        brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
    END.
   
END.

ON CHOOSE OF btn-edit
    OR 'enter':u OF brw-{&tmptable}
    OR 'mouse-select-dblclick' OF brw-{&tmptable}
DO:
    btn-ok:LABEL IN  FRAME frm-input = "Update".
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    DISABLE {&skey} WITH FRAME frm-input.
    run proc-edit.
    brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
    RETURN.
END.

ON CHOOSE OF btn-Del DO:
   GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.deviceID.
    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.deviceID = wsid EXCLUSIVE-LOCK NO-ERROR.
    wsBtn = "EDIT".
      ASSIGN bfr{&tmptable}.dStat = "D".
       RELEASE {&tmptable}.
       OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.dStat = "A"  NO-LOCK .
     APPLY 'entry' TO btn-exit.
END.


ON CHOOSE OF btn-Add IN FRAME frm-main DO:
     btn-ok:LABEL IN  FRAME frm-input = "Save".
    RUN proc-input.
    RETURN.
END.

ON CHOOSE OF btn-devVer IN FRAME frm-main DO:
    webResponse ="".
    FIX-CODEPAGE (webResponse) = "UTF-8".
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.deviceID.
    RUN proc-check.
    RETURN.
END.

ON CHOOSE OF btn-devReg IN FRAME frm-main DO:
    webResponse ="".
    FIX-CODEPAGE (webResponse) = "UTF-8".
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.deviceID.
    RUN proc-regDevice.
    RETURN.
END.

ON CHOOSE OF btn-devConfig IN FRAME frm-main DO:
    webResponse ="".
    FIX-CODEPAGE (webResponse) = "UTF-8".
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.deviceID.
    RUN proc-gConfig.
    RETURN.
END.

{audit.i}
/********** MAIN LOGIC **********/
FIND FIRST SIMCTR NO-LOCK NO-WAIT NO-ERROR.

IF AVAILABLE SIMCTR THEN
    DISPLAY  simctr.cocode simctr.CONAME  SIMCTR.REGNO  SIMCTR.BPNO simctr.phone WITH FRAME FRM-main.
IF NOT AVAILABLE SIMCTR THEN
DO:
    MESSAGE "SIMCTR is currently in use." SKIP
                "Please try again later."
            VIEW-AS ALERT-BOX.
    RETURN.
END.
OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.dStat = "A"  NO-LOCK .
ENABLE ALL EXCEPT simctr.cocode simctr.CONAME  SIMCTR.REGNO  SIMCTR.BPNO simctr.phone WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
CLOSE QUERY qry-{&tmptable}.
HIDE FRAME frm-main.



PROCEDURE proc-input:
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   CLEAR FRAME frm-input ALL.
   ENABLE ALL EXCEPT simctr.cocode simctr.CONAME  SIMCTR.REGNO  SIMCTR.BPNO simctr.phone WITH FRAME frm-input.
   DISPLAY  simctr.cocode simctr.CONAME  SIMCTR.REGNO  SIMCTR.BPNO simctr.phone WITH FRAME FRM-input.
  ASSIGN {&skey}:SCREEN-VALUE = "0".
  WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
         CLOSE QUERY qry-{&tmptable}.
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   HIDE FRAME frm-input.
END.


PROCEDURE proc-Edit:
   ASSIGN wsid = bfr{&tmptable}.deviceID.
   FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.deviceID = wsid EXCLUSIVE-LOCK NO-ERROR.
   VIEW FRAME frm-input.
   ENABLE ALL EXCEPT {&skey} simctr.cocode simctr.CONAME  SIMCTR.REGNO  SIMCTR.BPNO simctr.phone WITH FRAME frm-input.
   DISPLAY wsid @ {&skey} simctr.cocode simctr.CONAME  SIMCTR.REGNO  SIMCTR.BPNO simctr.phone
        bfr{&tmptable}.TName bfr{&tmptable}.activationKey bfr{&tmptable}.modelName bfr{&tmptable}.modelVersion bfr{&tmptable}.serialNumber bfr{&tmptable}.certificateRe
       WITH FRAME frm-input.
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input
          OR CHOOSE OF btn-ok IN FRAME frm-input.
   HIDE FRAME frm-input.
END.


PROCEDURE proc-check:
   FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.deviceID = wsid NO-LOCK NO-ERROR.

   HttpClient = NEW System.Net.WebClient(). 
   HttpClient:Proxy:Credentials = System.Net.CredentialCache:DefaultNetworkCredentials. 

   webResponse = httpClient:DownloadString("http://localhost:8090/verify/" + string(wsid) + "/" + bfr{&tmptable}.activationKey + "/" + bfr{&tmptable}.serialNumber). /*, "application/json" */

   HttpClient:Dispose(). 
   DELETE OBJECT HttpClient.

   txtWebResponse = STRING(webResponse).

   txtUnWantedR[1] = "~{".
   txtUnWantedR[2] = "~}".
   txtUnWantedR[3] = "~[".
   txtUnWantedR[4] = "~]".
   txtUnWantedR[5] = "~"".
   txtUnWantedR[6] = "~\".
   txtUnWantedR[7] = "deviceBranchAddress:".
   txtUnWantedR[8] = "deviceBranchContacts:".

   
    
    DO j = 1 TO EXTENT(txtUnWantedR):
        IF txtUnWantedR[j] <> "" THEN DO:
            txtWebResponse = REPLACE(txtWebResponse, txtUnWantedR[j], "").
        END.
    END.


    
    j = 0.
          IF j = 0 THEN DO:
              j = LOOKUP("status:422",txtWebResponse).
              IF j<>0 THEN DO:
                  rspCode = REPLACE(ENTRY(j,txtWebResponse),":",",").
                  rspMessage = REPLACE(ENTRY(4,txtWebResponse),":",",").
                  rspMessage = ENTRY(2,rspMessage).
                  rspCode= ENTRY(2,rspCode).
                  MESSAGE rspCode  rspMessage VIEW-AS ALERT-BOX.
              END.
          END.
          IF j = 0 THEN DO:
              j = LOOKUP("status:500",txtWebResponse).
              IF j<>0 THEN DO:
                  rspCode = REPLACE(ENTRY(j,txtWebResponse),":",",").
                  rspMessage = "Server Error".
                  rspCode= ENTRY(2,rspCode).
                  MESSAGE rspCode  rspMessage VIEW-AS ALERT-BOX.
              END.
          END.
        IF j = 0 THEN DO:
           
            DO i = 1 TO NUM-ENTRIES(txtWebResponse):
                   vTemp = ENTRY(i,txtWebResponse).
                   IF SUBSTR(trim(vTemp),1,11) = "operationID"  THEN DO:
                        operationID = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,12) = "taxPayerName"  THEN DO:
                        taxPayerName = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,11) = "taxPayerTIN"  THEN DO:
                        taxPayerTIN = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,9) = "vatNumber"  THEN DO:
                       vatNumber = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,16) = "deviceBranchName"  THEN DO:
                       deviceBranchName = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,4) = "city"  THEN DO:
                       city = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,8) = "district"  THEN DO:
                       district = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,7) = "houseNo"  THEN DO:
                       houseNo = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,8) = "province"  THEN DO:
                       province = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,6) = "street"  THEN DO:
                       street = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,7) = "phoneNo"  THEN DO:
                       phone = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,5) = "email"  THEN DO:
                       email = replace(vTemp,":",",").
                   END.
                    
            END.
            

            vTemp = ENTRY(2,operationID).

            

            IF NUM-ENTRIES(operationID) > 2 THEN DO:
                DO i = 3 TO NUM-ENTRIES(operationID):
                    vTemp = vTemp + ":" + ENTRY(i,operationID).
                END.
            END.
            
            
           operationID = vTemp.
            taxPayerName = ENTRY(2,taxPayerName).
            
            taxPayerTIN = ENTRY(2,taxPayerTIN).
           
           vatNumber = ENTRY(2,vatNumber).
           deviceBranchName = ENTRY(2,deviceBranchName).
          
          city = ENTRY(2,city).
          /*district = ENTRY(2,district).*/
          houseNo = ENTRY(2,houseNo).
         
          province = ENTRY(2,province).
           
          street = ENTRY(2,street).
          phone = ENTRY(2,phone).
          email = ENTRY(2,email).
           MESSAGE "Operation ID: " + operationID + "~n Tax Payer Name: " + taxPayerName + "~n Tax Payer TIN: " + taxPayerTIN  + "~n Vat Number: " 
               + vatNumber  + "~n Device Branch Name: " + deviceBranchName + "~n City: " + city 
               + "~n House Number: " + houseNo + "~n Province: " + province
                 + "~n Street: " + street + "~n Phone: " + phone + "~n Email: " + email
               VIEW-AS ALERT-BOX.


        END.
 END.

 PROCEDURE proc-regDevice:
     
     FIND FIRST simctr NO-LOCK NO-ERROR.

     FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.deviceID = wsid NO-LOCK NO-ERROR.
     wsFile  = substring(simctr.repDir,1,(LENGTH(simctr.repDir) - 8)) + "zimra\scr"  + string(wsid) + ".csv".
    
    OUTPUT STREAM a TO VALUE(wsFile).
    EXPORT STREAM a DELIMITER "," bfr{&tmptable}.certificateRe.
    OUTPUT STREAM a CLOSE.
   
   HttpClient = NEW System.Net.WebClient(). 
   HttpClient:Proxy:Credentials = System.Net.CredentialCache:DefaultNetworkCredentials. 

   webResponse = httpClient:DownloadString("http://localhost:8090/register/" + string(wsid) +  "/"  + bfr{&tmptable}.modelVersion +  "/"  + bfr{&tmptable}.activationKey + "/" + bfr{&tmptable}.modelName).

   HttpClient:Dispose(). 
   DELETE OBJECT HttpClient.

   txtWebResponse = STRING(webResponse).


   txtUnWantedR1[1] = "~{".
   txtUnWantedR1[2] = "~}".
   txtUnWantedR1[3] = "~"".
   
     DO j = 1 TO EXTENT(txtUnWantedR1):
        IF txtUnWantedR1[j] <> "" THEN DO:
            txtWebResponse = REPLACE(txtWebResponse, txtUnWantedR1[j], "").
        END.
    END.

     j = 0.
          IF j = 0 THEN DO:
              j = LOOKUP("status:422",txtWebResponse).
              IF j<>0 THEN DO:
                  rspCode = REPLACE(ENTRY(j,txtWebResponse),":",",").
                  rspMessage = REPLACE(ENTRY(4,txtWebResponse),":",",").
                  rspMessage = ENTRY(2,rspMessage).
                  rspCode= ENTRY(2,rspCode).
                  MESSAGE rspCode  rspMessage VIEW-AS ALERT-BOX.
              END.
          END.
          IF j = 0 THEN DO:
              j = LOOKUP("status:500",txtWebResponse).
              IF j<>0 THEN DO:
                  rspCode = REPLACE(ENTRY(j,txtWebResponse),":",",").
                  rspMessage = "Server Error".
                  rspCode= ENTRY(2,rspCode).
                  MESSAGE rspCode  rspMessage VIEW-AS ALERT-BOX.
              END.
          END.
         IF j = 0 THEN DO:
              DO i = 1 TO NUM-ENTRIES(txtWebResponse):
                    vTemp = ENTRY(i,txtWebResponse).
                    
                  IF SUBSTR(trim(vTemp),1,11) = "operationID"  THEN DO:
                         operationID = replace(vTemp,":",",").
                    END.
                   
                    IF SUBSTR(trim(vTemp),1,11) = "certificate"  THEN DO:
                         certif = replace(vTemp,":",",").
                    END.
                    
              END.
             vTemp = ENTRY(2,operationID).
             IF NUM-ENTRIES(operationID) > 2 THEN DO:
                 DO i = 3 TO NUM-ENTRIES(operationID):
                     vTemp = vTemp + ":" + ENTRY(i,operationID).
                 END.
             END.
      
           operationID = vTemp.
           certif = ENTRY(2,certif).
            
            /*save this to pem file as well as the database*/
           FOR EACH fdmsDevice WHERE fdmsDevice.deviceID = wsid:
               assign
                   fdmsDevice.certificate = certif
                   fdmsDevice.operationID = operationID.
           END.
            certif = REPLACE(certif,"\n",",").
            DO j = 1 TO NUM-ENTRIES(certif):
                    PUT STREAM certi TRIM(ENTRY(j,certif)) FORMAT "x(67)" SKIP.
                    
            END.
            MESSAGE "Device Succesfully Registered" VIEW-AS ALERT-BOX.
    
        END.

END.


 PROCEDURE proc-gConfig:
    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.deviceID = wsid NO-LOCK NO-ERROR.

    HttpClient = NEW System.Net.WebClient(). 
    HttpClient:Proxy:Credentials = System.Net.CredentialCache:DefaultNetworkCredentials. 

    webResponse = httpClient:DownloadString("http://localhost:8090/getConfig/" + string(wsid) + "/" + bfr{&tmptable}.modelName + "/" + bfr{&tmptable}.modelVersion).
                                                                                                
    
    HttpClient:Dispose(). 
    DELETE OBJECT HttpClient.

    txtWebResponse = STRING(webResponse).

    txtTaxes = SUBSTRING(txtWebResponse, INDEX(txtWebResponse, "~["),  INDEX(txtWebResponse, "~]") - INDEX(txtWebResponse, "~[")).
    
    txtTaxes = REPLACE(txtTaxes,"~}","&").
    txtTaxes = REPLACE(txtTaxes,",","#").
    txtTaxes = REPLACE(txtTaxes,"&",",").

    

    txtUnWantedR[1] = "~{".
    txtUnWantedR[2] = "~}".
    txtUnWantedR[3] = "~[".
    txtUnWantedR[4] = "~]".
    txtUnWantedR[5] = "~"".
    txtUnWantedR[6] = "~\".
    txtUnWantedR[7] = "deviceBranchAddress:".
    txtUnWantedR[8] = "deviceBranchContacts:".
    

     DO j = 1 TO EXTENT(txtUnWantedR):
         IF txtUnWantedR[j] <> "" THEN DO:
             txtWebResponse = REPLACE(txtWebResponse, txtUnWantedR[j], "").
         END.
     END.


     j = 0.
           IF j = 0 THEN DO:
               j = LOOKUP("status:422",txtWebResponse).
               IF j<>0 THEN DO:
                   rspCode = REPLACE(ENTRY(j,txtWebResponse),":",",").
                   rspMessage = REPLACE(ENTRY(4,txtWebResponse),":",",").
                   rspMessage = ENTRY(2,rspMessage).
                   rspCode= ENTRY(2,rspCode).
                   MESSAGE rspCode  rspMessage VIEW-AS ALERT-BOX.
               END.
           END.
           IF j = 0 THEN DO:
               j = LOOKUP("status:500",txtWebResponse).
               IF j<>0 THEN DO:
                   rspCode = REPLACE(ENTRY(j,txtWebResponse),":",",").
                   rspMessage = "Server Error".
                   rspCode= ENTRY(2,rspCode).
                   MESSAGE rspCode  rspMessage VIEW-AS ALERT-BOX.
               END.
           END.
         IF j = 0 THEN DO:
            txtTaxes = REPLACE(txtTaxes,"~[","").
            txtTaxes =REPLACE(txtTaxes,"~{","").
             DO i = 1 TO (NUM-ENTRIES(txtTaxes) - 1):
                    vTemp = ENTRY(i,txtTaxes).
                    IF SUBSTR(TRIM(vTemp),1,1) = "#" THEN DO:
                        vTemp = SUBSTR(TRIM(vTemp),2).
                    END.
                    vTemp = REPLACE(vTemp,"#",",").
                    vTemp = REPLACE(vTemp,"~"","").
                    
                    taxID = ENTRY(1,vTemp).
                    IF SUBSTR(trim(taxID),1,5) = "taxID"  THEN DO:
                         taxID = replace(taxID,":",",").
                    END.
                    
                    taxName = ENTRY(2,vTemp).
                   IF SUBSTR(trim(taxName),1,7) = "taxName"  THEN DO:
                         taxName = replace(taxName,":",",").
                    END.
                    IF NUM-ENTRIES(vTemp) > 3 THEN DO:
                        taxPercent = ENTRY(3,vTemp).
                        IF SUBSTR(trim(taxPercent),1,10) = "taxPercent"  THEN DO:
                            taxPercent = replace(taxPercent,":",",").
                       END.
                    END.
                    ELSE DO:
                        taxPercent = "".
                    END.
                    taxID = ENTRY(2,taxID).
                    taxName = ENTRY(2,taxName).
                    IF NUM-ENTRIES(taxPercent) > 1 THEN DO:
                         taxPercent = ENTRY(2,taxPercent).
                    END.
                    FIND FIRST fdmsTax WHERE fdmsTax.deviceID = wsid AND fdmsTax.taxID = INTEGER(taxID) EXCLUSIVE-LOCK NO-ERROR.
                    IF AVAILABLE fdmsTax THEN DO:
                        ASSIGN
                            fdmsTax.taxName = taxName
                            fdmsTax.taxPerc = decimal(taxPercent) WHEN taxPercent <> "".
                    END.
                    ELSE IF NOT available fdmsTax THEN DO:
                        CREATE fdmsTax.
                        ASSIGN
                            fdmsTax.deviceID = wsid
                            fdmsTax.taxID =INTEGER(taxID)
                            fdmsTax.taxName = taxName
                            fdmsTax.taxPerc = decimal(taxPercent) WHEN taxPercent <> "".
                    END.
                    
             END.

             DO i = 1 TO NUM-ENTRIES(txtWebResponse):
                    vTemp = ENTRY(i,txtWebResponse).
                    IF SUBSTR(trim(vTemp),1,11) = "operationID"  THEN DO:
                         operationID = replace(vTemp,":",",").
                    END.
                    IF SUBSTR(TRIM(vTemp),1,5) = "qrUrl" THEN DO:
                         qrUrl = REPLACE(vTemp,":",",").
                    END.
                    IF SUBSTR(trim(vTemp),1,12) = "taxPayerName"  THEN DO:
                         taxPayerName = replace(vTemp,":",",").
                    END.
                    IF SUBSTR(trim(vTemp),1,11) = "taxPayerTIN"  THEN DO:
                         taxPayerTIN = replace(vTemp,":",",").
                    END.
                    IF SUBSTR(trim(vTemp),1,9) = "vatNumber"  THEN DO:
                        vatNumber = replace(vTemp,":",",").
                    END.
                    IF SUBSTR(trim(vTemp),1,16) = "deviceBranchName"  THEN DO:
                        deviceBranchName = replace(vTemp,":",",").
                    END.
                    IF SUBSTR(trim(vTemp),1,20) = "certificateValidTill"  THEN DO:
                        txtValid = replace(vTemp,":",",").
                        txtValid = SUBSTRING(txtValid,1,LENGTH(txtValid) - 9).
                    END.
                    
                    IF SUBSTR(trim(vTemp),1,14) = "deviceSerialNo"  THEN DO:
                        txtSerial = replace(vTemp,":",",").
                    END.
                        
                    IF SUBSTR(trim(vTemp),1,4) = "city"  THEN DO:
                        city = replace(vTemp,":",",").
                    END.
                    IF SUBSTR(trim(vTemp),1,19) = "deviceOperatingMode"  THEN DO:
                        dOMode = replace(vTemp,":",",").
                    END.
                    IF SUBSTR(trim(vTemp),1,7) = "houseNo"  THEN DO:
                        houseNo = replace(vTemp,":",",").
                    END.
                    IF SUBSTR(trim(vTemp),1,8) = "province"  THEN DO:
                        province = replace(vTemp,":",",").
                    END.
                    IF SUBSTR(trim(vTemp),1,6) = "street"  THEN DO:
                        street = replace(vTemp,":",",").
                    END.
                    IF SUBSTR(trim(vTemp),1,7) = "phoneNo"  THEN DO:
                        phone = replace(vTemp,":",",").
                    END.
                    IF SUBSTR(trim(vTemp),1,5) = "email"  THEN DO:
                       email = replace(vTemp,":",",").
                   END.

             END.
             
             vTemp = ENTRY(2, qrUrl).
             IF NUM-ENTRIES(qrUrl) > 2 THEN DO:
                 DO i = 3 TO NUM-ENTRIES(qrUrl):
                     vTemp = vTemp + ":" + ENTRY(i,qrUrl).
                 END.
             END.
            
             qrUrl = vTemp.
             
             vTemp = ENTRY(2,operationID).
             IF NUM-ENTRIES(operationID) > 2 THEN DO:
                 DO i = 3 TO NUM-ENTRIES(operationID):
                     vTemp = vTemp + ":" + ENTRY(i,operationID).
                 END.
             END.

             
            operationID = vTemp.
            txtValid = ENTRY(2,txtValid).
            vTemp = trim(txtValid).
            txtValid = SUBSTRING(vTemp,9,2) + SUBSTRING(vTemp,5,4) + SUBSTRING(vTemp,1,4).
            txtSerial = ENTRY(2,txtSerial).
             taxPayerName = ENTRY(2,taxPayerName).
             taxPayerTIN = ENTRY(2,taxPayerTIN).
            vatNumber = ENTRY(2,vatNumber).
            deviceBranchName = ENTRY(2,deviceBranchName).
           city = ENTRY(2,city).
           dOMode = ENTRY(2,dOMode).
           houseNo = ENTRY(2,houseNo).
           province = ENTRY(2,province).
           street = ENTRY(2,street).
           phone = ENTRY(2,phone).
           email = ENTRY(2,email).
           
         /*save this to database*/
            /*extract applicatble taxes*/
      FIND FIRST fdmsDevice WHERE fdmsDevice.deviceID = wsid NO-ERROR.
      IF AVAILABLE fdmsDevice THEN DO:
            ASSIGN
                    fdmsDevice.devBranchName = deviceBranchName 
                    fdmsDevice.operationID = operationID
                    fdmsDevice.opMode = dOMode
                    fdmsDevice.TNAME = taxPayerName
                    fdmsDevice.contactemail = email
                    fdmsDevice.Phone = phone
                    fdmsDevice.serialNumber = txtSerial
                    fdmsDevice.TIN = taxPayerTIN
                    fdmsDevice.txtCity = city
                    fdmsDevice.txtHouse =HouseNo
                    fdmsDevice.txtProvince  = province
                    fdmsDevice.txtSerial = txtSerial
                    fdmsDevice.txtStreet = street 
                    fdmsDevice.validity = DATE(trim(txtValid))
                    fdmsDevice.VATNO = vatNumber
                    fdmsDevice.qrUrl = qrUrl.
            MESSAGE "Configuration Completed" VIEW-AS ALERT-BOX.
      END.
      ELSE IF NOT AVAILABLE fdmsDevice THEN DO:
                MESSAGE "Device not available." VIEW-AS ALERT-BOX.
      END.
            

         END.
  END. 
