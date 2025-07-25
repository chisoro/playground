USING System.Net.Http.*. 
USING System.Environment.
/* Program.................fdmsOpenDay.p
   Notes:.................FDMs Open Day
   Author:.................S. Chisoro
   1. Getstatus
   2. getConfig
   3. OpenDay
  */

  
 session:DATA-ENTRY-RETURN = TRUE.


    DEFINE VARIABLE HttpClient AS CLASS System.Net.WebClient. 
   DEFINE VARIABLE webResponse AS LONGCHAR NO-UNDO. 
   DEF VAR txtWebResponse AS CHAR.
   DEF VAR txtUnWantedR AS CHAR EXTENT 8.
   DEF VAR txtUnWantedR1 AS CHAR EXTENT 3.
   DEF VAR rspCode AS CHAR.
   DEF VAR rspMessage AS CHAR.
   DEF VAR fdNumber AS INTEGER.
   DEF VAR dT AS DATETIME-TZ.
   DEF VAR fdOpen AS CHAR FORM "x(19)".


   def var operationID  as char.
   DEF VAR lastReceiptGlobalNo AS CHAR.
   DEF VAR lastFiscalDayNo AS CHAR.
    DEF VAR fiscalDayClosed AS CHAR.
    DEF VAR fiscalDayOpened AS CHAR.
    DEF VAR fiscalDayNo AS CHAR.
    DEF VAR sign AS CHAR.
    DEF VAR hash AS CHAR.
    DEF VAR certificateThumbprint AS CHAR.
    DEF VAR fiscalDayReconciliationMode AS CHAR.
    DEF VAR fiscalDayStatus AS CHAR.
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
   DEF VAR j AS INTEGER.
   DEF VAR taxID AS CHAR.
   DEF VAR taxName AS CHAR.
   DEF VAR taxPercent AS CHAR.
   DEF VAR swID LIKE fdmsDevice.deviceID.
   DEF VAR wsModel LIKE  fdmsDevice.modelName.
   DEF VAR wsVersion LIKE  fdmsDevice.modelVersion.

/*-------Mian Program--------*/
 FIND FIRST fdmsDevice WHERE fdmsDevice.dStat = "A"  NO-LOCK .
 swID = fdmsDevice.deviceID.
 wsModel = fdmsDevice.modelName.
 wsVersion = fdmsDevice.modelVersion.
 RUN proc-gStatus.

 PROCEDURE proc-gStatus:
    HttpClient = NEW System.Net.WebClient(). 
    HttpClient:Proxy:Credentials = System.Net.CredentialCache:DefaultNetworkCredentials. 

    webResponse = httpClient:DownloadString("http://localhost:8090/getStatus/" + string(swID) + "/" + fdmsDevice.modelName + "/" + fdmsDevice.modelVersion).
                                                                                                
    
    HttpClient:Dispose(). 
    DELETE OBJECT HttpClient.

    txtWebResponse = STRING(webResponse).

    txtUnWantedR[1] = "~{".
    txtUnWantedR[2] = "~}".
    txtUnWantedR[3] = "~[".
    txtUnWantedR[4] = "~]".
    txtUnWantedR[5] = "~"".
    txtUnWantedR[6] = "fiscalDayServerSignature:".
    txtUnWantedR[7] = "deviceBranchAddress:".
    txtUnWantedR[8] = "deviceBranchContacts:".


     DO j = 1 TO EXTENT(txtUnWantedR):
         IF txtUnWantedR[j] <> "" THEN DO:
             txtWebResponse = REPLACE(txtWebResponse, txtUnWantedR[j], "").
         END.
     END.


     j = 0.
     IF j = 0 THEN DO:
               j = LOOKUP("status:400",txtWebResponse).
               IF j<>0 THEN DO:
                   rspCode = REPLACE(ENTRY(j,txtWebResponse),":",",").
                   rspMessage = REPLACE(ENTRY(4,txtWebResponse),":",",").
                   rspMessage = ENTRY(2,rspMessage).
                   rspCode= ENTRY(2,rspCode).
                   MESSAGE rspCode  rspMessage VIEW-AS ALERT-BOX.
               END.
           END.
      IF j = 0 THEN DO:
               j = LOOKUP("status:401",txtWebResponse).
               IF j<>0 THEN DO:
                   rspCode = REPLACE(ENTRY(j,txtWebResponse),":",",").
                   rspMessage = "Server Error".
                   rspCode= ENTRY(2,rspCode).
                   MESSAGE rspCode  rspMessage VIEW-AS ALERT-BOX.
               END.
           END.
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
                   IF SUBSTR(trim(vTemp),1,15) = "fiscalDayStatus"  THEN DO:
                        fiscalDayStatus = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,27) = "fiscalDayReconciliationMode"  THEN DO:
                        fiscalDayReconciliationMode = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,21) = "certificateThumbprint"  THEN DO:
                       certificateThumbprint = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,4) = "hash"  THEN DO:
                       hash = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,9) = "signature"  THEN DO:
                       sign = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,15) = "fiscalDayClosed"  THEN DO:
                       fiscalDayClosed = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,15) = "lastFiscalDayNo"  THEN DO:
                       lastFiscalDayNo = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,19) = "lastReceiptGlobalNo"  THEN DO:
                       lastReceiptGlobalNo = replace(vTemp,":",",").
                   END.
                   
            END.
            

            vTemp = ENTRY(2,operationID).

            

            IF NUM-ENTRIES(operationID) > 2 THEN DO:
                DO i = 3 TO NUM-ENTRIES(operationID):
                    vTemp = vTemp + ":" + ENTRY(i,operationID).
                END.
            END.
            
            
           operationID = vTemp.
           IF NUM-ENTRIES(lastReceiptGlobalNo )  > 1 THEN DO:
                     lastReceiptGlobalNo = ENTRY(2,lastReceiptGlobalNo).
           END.
            IF NUM-ENTRIES(lastFiscalDayNo) > 1 THEN DO:
                    lastFiscalDayNo = ENTRY(2,lastFiscalDayNo).
            END.
            IF NUM-ENTRIES(fiscalDayClosed ) > 1 THEN DO:
                    fiscalDayClosed = ENTRY(2,fiscalDayClosed).
            END.
            IF NUM-ENTRIES(sign) > 1 THEN DO:
                    sign = ENTRY(2,sign).
            END.
            IF NUM-ENTRIES(hash) > 1 THEN DO:
                    hash = ENTRY(2,hash).
            END.
            IF NUM-ENTRIES(certificateThumbprint) > 1 THEN DO:
                     certificateThumbprint = ENTRY(2,certificateThumbprint).
            END.
            IF NUM-ENTRIES(fiscalDayReconciliationMode) > 1 THEN DO:
                    fiscalDayReconciliationMode = ENTRY(2,fiscalDayReconciliationMode).
            END.
           IF NUM-ENTRIES(fiscalDayStatus) > 1  THEN DO:
                    fiscalDayStatus = ENTRY(2,fiscalDayStatus).
           END.
            
            
           MESSAGE "Operation ID: " + operationID + "~n Global Number: " +  lastReceiptGlobalNo + "~n Fisical Day Number: " + lastFiscalDayNo  + "~n last Closed: " 
               + fiscalDayClosed  + "~n Signature: " + sign + "~n Hash: " + hash 
               + "~n Thumprint: " + certificateThumbprint + "~n Mode: " + fiscalDayReconciliationMode
                 + "~n day Status: " + fiscalDayStatus 
               VIEW-AS ALERT-BOX.
            IF TRIM(fiscalDayStatus) = "FiscalDayClosed" THEN DO:
                    RUN proc-gConfig.
                    RUN proc-oDay.
                    
            END.
            ELSE DO:
                MESSAGE "Fiscal Day is Not Closed. You can not Open New Day" VIEW-AS ALERT-BOX.
            END.

       
       END.

 END PROCEDURE.

 PROCEDURE proc-gConfig:
    FIND FIRST fdmsDevice WHERE fdmsDevice.deviceID = swID NO-LOCK NO-ERROR.

    HttpClient = NEW System.Net.WebClient(). 
    HttpClient:Proxy:Credentials = System.Net.CredentialCache:DefaultNetworkCredentials. 

    webResponse = httpClient:DownloadString("http://localhost:8090/getConfig/" + string(swID) + "/" + fdmsDevice.modelName + "/" + fdmsDevice.modelVersion).
                                                                                                
    
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
                    FIND FIRST fdmsTax WHERE fdmsTax.deviceID = swID AND fdmsTax.taxID = INTEGER(taxID) EXCLUSIVE-LOCK NO-ERROR.
                    IF AVAILABLE fdmsTax THEN DO:
                        ASSIGN
                            fdmsTax.taxName = taxName
                            fdmsTax.taxPerc = decimal(taxPercent) WHEN taxPercent <> "".
                    END.
                    ELSE IF NOT available fdmsTax THEN DO:
                        CREATE fdmsTax.
                        ASSIGN
                            fdmsTax.deviceID = swID
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
      FIND FIRST fdmsDevice WHERE fdmsDevice.deviceID = swID NO-ERROR.
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
                   fdmsDevice.qrUrl = qrUrl
                   fdmsDevice.fdStat = fiscalDayStatus
                   fdmsDevice.hash = hash
                    fdmsDevice.fdClosed = fiscalDayClosed
                    fdmsDevice.cThprint = certificateThumbprint
                    fdmsDevice.recMode = fiscalDayReconciliationMode 
                    fdmsDevice.sign = sign.
                    MESSAGE "Configuration Completed" VIEW-AS ALERT-BOX.
      END.
      ELSE IF NOT AVAILABLE fdmsDevice THEN DO:
                MESSAGE "Device not available." VIEW-AS ALERT-BOX.
                QUIT.
      END.
            

         END.
  END PROCEDURE. 

 PROCEDURE proc-oDay:
     HttpClient = NEW System.Net.WebClient(). 
 HttpClient:Proxy:Credentials = System.Net.CredentialCache:DefaultNetworkCredentials. 
 IF INT(lastFiscalDayNo) = 0 THEN
     fdnumber = 1.
 ELSE 
     fdNumber = INT(lastFiscalDayNo) + 1.




dt = NOW.
fdOpen = SUBSTRING(string(dt),7,4)  + "-" + SUBSTRING(string(dt),4,2) + "-" + SUBSTRING(string(dt),1,2) + "T" +  SUBSTRING(STRING(dt),12,8).

 webResponse = httpClient:DownloadString("http://localhost:8090/openDay/" + string(swID) + "/" + fdmsDevice.modelName + "/" + fdmsDevice.modelVersion + "/" + string(fdNumber) + "/" + fdOpen).


 HttpClient:Dispose(). 
 DELETE OBJECT HttpClient.

 txtWebResponse = STRING(webResponse).

 txtUnWantedR[1] = "~{".
 txtUnWantedR[2] = "~}".
 txtUnWantedR[3] = "~[".
 txtUnWantedR[4] = "~]".
 txtUnWantedR[5] = "~"".
 txtUnWantedR[6] = "fiscalDayServerSignature:".
 txtUnWantedR[7] = "deviceBranchAddress:".
 txtUnWantedR[8] = "deviceBranchContacts:".


  DO j = 1 TO EXTENT(txtUnWantedR):
      IF txtUnWantedR[j] <> "" THEN DO:
          txtWebResponse = REPLACE(txtWebResponse, txtUnWantedR[j], "").
      END.
  END.


  j = 0.
  IF j = 0 THEN DO:
            j = LOOKUP("status:400",txtWebResponse).
            IF j<>0 THEN DO:
                rspCode = REPLACE(ENTRY(j,txtWebResponse),":",",").
                rspMessage = REPLACE(ENTRY(4,txtWebResponse),":",",").
                rspMessage = ENTRY(2,rspMessage).
                rspCode= ENTRY(2,rspCode).
                MESSAGE rspCode  rspMessage VIEW-AS ALERT-BOX.
            END.
        END.
   IF j = 0 THEN DO:
            j = LOOKUP("status:401",txtWebResponse).
            IF j<>0 THEN DO:
                rspCode = REPLACE(ENTRY(j,txtWebResponse),":",",").
                rspMessage = "Server Error".
                rspCode= ENTRY(2,rspCode).
                MESSAGE rspCode  rspMessage VIEW-AS ALERT-BOX.
            END.
        END.
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
                IF SUBSTR(trim(vTemp),1,11) = "fiscalDayOpened"  THEN DO:
                     fiscalDayOpened = replace(vTemp,":",",").
                END.
                IF SUBSTR(trim(vTemp),1,15) = "fiscalDayNo"  THEN DO:
                    fiscalDayNo = replace(vTemp,":",",").
                END.
         END.

        MESSAGE "F " + fiscalDayOpened VIEW-AS ALERT-BOX.
         vTemp = ENTRY(2,fiscalDayOpened).

        IF NUM-ENTRIES(fiscalDayOpened) > 2 THEN DO:
             DO i = 3 TO NUM-ENTRIES(fiscalDayOpened):
                 vTemp = vTemp + ":" + ENTRY(i,fiscalDayOpened ).
             END.
         END.


        fiscalDayOpened  = vTemp. /*for checking*/
        MESSAGE fiscalDayOpened VIEW-AS ALERT-BOX.
       
        IF NUM-ENTRIES(fiscalDayNo )  > 1 THEN DO:
                  fiscalDayNo = ENTRY(2,fiscalDayNo).
        END.
        /*Save fiscal day Number and fisical Day Opened*/
        
        FIND FIRST fdmsDevice WHERE fdmsDevice.deviceID = swID EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE fdmsDevice THEN
            ASSIGN 
                /*fdmsDevice.fDNumber  = integer(fiscalDayNo)*/
                fdOpen = fiscalDayOpened.
        
        CREATE fdmsRecCounter.
               ASSIGN
                    fdmsRecCounter.fDNumber = fdNumber
                   fdmsRecCounter.deviceID  = swID
                   fdmsRecCounter.glbNumber = int(lastReceiptGlobalNo)
                   fdmsRecCounter.recDate = TODAY.

        MESSAGE "Fiscal Day Opened" VIEW-AS ALERT-BOX.
         
    END.

 END PROCEDURE.
