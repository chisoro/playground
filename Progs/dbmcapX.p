/* Program.................dbmcapX.p
   Notes:................. Meter Capture
   Author:.................S. Mawire
   Modified:..............S. Chisoro
*/
session:DATA-ENTRY-RETURN = TRUE.
/*DEFINE VARIABLE oIPHostEntry AS System.Net.IPHostEntry NO-UNDO.
DEFINE VARIABLE oIPAddress   AS System.Net.IPAddress   NO-UNDO.
DEFINE VARIABLE cIPAddresses AS CHARACTER              NO-UNDO.
DEFINE VAR olV LIKE  db005.tblTranslogDb003.oldValue.
DEFINE SHARED VARIABLE varUser AS CHARACTER.*/

DEF  VAR wsStatus   AS CHAR FORM "X(20)".
DEF BUTTON btn-exit  LABEL "CLOSE".
DEF BUTTON btn-OK  LABEL "PROCESS".
DEF VAR wsFile as CHAR.
DEF VAR wsFile1 AS CHAR FORM "X(20)".
DEF VAR wsFile2 AS CHAR FORM "X(20)".
DEF VAR Acc LIKE dbmmf.dbacc.
DEF VAR Meter LIKE dbmmf.Meter.
 DEF VAR Lat LIKE dbmmf.lat.
 DEF VAR Lon LIKE dbmmf.lon.
 DEF VAR Read AS DECIMAL.
 DEF VAR rDate AS DATE.
 DEF VAR cn AS INTEGER.
 DEF VAR cn1 AS INTEGER.

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 80 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 80 by 8.5.


DEF FRAME frm-main
    SKIP(1)
    "The CSV file must have fields in the following order:"  COLON 10 skip(0.2) 
    "Account, Meter Number, Latitude, Longitude, Meter Reading" COLON 10 SKIP(0.5) 
   wsFile1 COLON 26 LABEL "Enter File Name" SKIP (1)
    rDate COLON 26 LABEL "Reading Date" SKIP (1)
    wsStatus       COLON 26  LABEL "Processing Account........." VIEW-AS TEXT
    btn-ok AT ROW 10.5 COL 10
    btn-exit AT ROW 10.5 COL 65 NO-TAB-STOP
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 10 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 85 BY 13.0
    TITLE "METER READING UPLOAD FROM FILE" VIEW-AS DIALOG-BOX.


ON 'enter':U OF wsFile1 IN FRAME frm-main 
    OR 'tab':U OF wsFile1 IN FRAME frm-main
DO:
    IF wsFile1:SCREEN-VALUE = "" THEN DO:
        MESSAGE "File name cannot be Blank" VIEW-AS ALERT-BOX.
             RETURN NO-APPLY.
    END.
      
    ELSE DO:
         APPLY 'ENTRY' TO rDate.
            RETURN.
    END.  

END.

ON 'enter':U OF rDate IN FRAME frm-main 
    OR 'tab':U OF rDate IN FRAME frm-main
DO:
    
    IF rDate:SCREEN-VALUE = "/  /" THEN DO:
        MESSAGE "Reading Date cannot be Blank." VIEW-AS ALERT-BOX.
             RETURN NO-APPLY.
    END.
      
    ELSE DO:
         
            RETURN.
    END.  

END.

 ON CHOOSE OF btn-ok IN FRAME frm-main 
     
 DO: 
     wsFile = wsFile + wsFile1:SCREEN-VALUE IN FRAME frm-main + ".csv".
    cn = 0.
    cn1 = 0.
    input FROM value(wsFile).
    OUTPUT TO VALUE(wsFile2).
    REPEAT:
         import delimiter ","  Acc Meter Lat Lon Read.
         wsStatus:SCREEN-VALUE = string(Acc).
           cn = cn + 1.  
         FIND FIRST dbmmf WHERE dbmmf.dbAcc = dec(Acc) AND dbmmf.meter = Meter  NO-ERROR.
         IF AVAILABLE dbmmf THEN DO:
          cn1 = cn1 + 1.
          /*olV = string(dbmmf.READ[13]) + "/" + string(dbmmf.rDate[13]).*/
         ASSIGN
                     dbmmf.READ[13] = READ
                     dbmmf.rDate[13] = date(rDate:SCREEN-VALUE IN FRAME frm-main)
                    /* dbmmf.lat = Lat
                     dbmmf.lon = Lon*/.
         assign
             dbmmf.consum[13] = dbmmf.READ[13] - dbmmf.READ[12].
          /*audit trail, name of file, user, machine and ip address, date and time, old value, account, meter
         CREATE tblTransLogDB003.
         ASSIGN
             db005.tblTranslogDb003.action =  "Upload from " + wsFile1
              db005.tblTranslogDb003.dateexecuted = NOW
             db005.tblTranslogDb003.hostname = System.Net.Dns:GetHostName()
             db005.tblTranslogDb003.ipaddress = cIPAddresses
             db005.tblTranslogDb003.NewValue = string(READ) + "/" +  rDate:SCREEN-VALUE IN FRAME frm-main
             db005.tblTranslogDb003.oldValue = olV
             db005.tblTranslogDb003.ttable  = "dbmmf"
             db005.tblTranslogDb003.usercode = varUser.*/
         END.
         IF NOT AVAILABLE dbmmf THEN DO:
             EXPORT DELIMITER "," Acc Meter.
         END.

    END.
    MESSAGE cn1 " Out of  " cn  "Accounts Upload Succesful." VIEW-AS ALERT-BOX.
    MESSAGE "Check errorReadings.csv under Reports for accounts that failed" VIEW-AS ALERT-BOX.
  END.

/********** MAIN LOGIC **********/
/*VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW. */
FIND FIRST simctr NO-LOCK.
wsFile = simctr.repDir.
wsFile2 = simctr.repDir + "errorReadings.csv".
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.
