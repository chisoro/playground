/* Program.................kservice.p
   Notes:................. Kservice API connection settings
   Author:.................S. Chisoro
   Date:......................26/07/25
  */

  
session:DATA-ENTRY-RETURN = TRUE.
DEF VAR wsid LIKE simctr2.cocode.
DEF VAR Current-Record AS ROWID.
DEF VAR method-status AS LOGICAL.
DEF BUTTON btn-close LABEL "CLOSE".
DEF BUTTON btn-ok    LABEL "OK".
DEF VAR device AS CHAR.
define rectangle rect-1
     edge-pixels 2 graphic-edge  no-fill
     size 170 by 2.3.

define rectangle rect-2
     edge-pixels 2 graphic-edge  NO-FILL
     size 170 by 24.4.


define frame frm-input
    SKIP(0.5)
    simctr2.cocode   colon 30 label "Company Code" SPACE(5)
    simCtr2.api_env             LABEL  "Environment" VIEW-AS COMBO-BOX LIST-ITEM-PAIRS
                     "Production","P", "Development", "D" AUTO-RETURN SKIP(0.5)
    simctr2.t_url     COLON 30 LABEL "Development URL"SKIP(0.5)
    simctr2.p_url    COLON 30 LABEL "Production URL"SKIP(0.5)
    simctr2.scr_path COLON 30 LABEL "Server Certificate Request" FORM "X(128)" SKIP(0.5)
    simctr2.server_cert_path COLON 30 LABEL "Server Certificate"  FORM "X(128)" SKIP(0.5)
    simctr2.client_cert_path COLON 30 LABEL "Client Certificate"  FORM "X(128)" SKIP(0.5)
    simctr2.client_key_path COLON 30 LABEL "Client Private Key"  FORM "X(128)" SKIP(0.5)
    simctr2.host_ip COLON 30 LABEL "Host IP Address"SKIP(0.5)
    simctr2.host_port COLON 30 LABEL "Host IP Port Number"SKIP(0.5)
    simctr2.rec_path COLON 30 LABEL "Receipts"SKIP(0.5)
    rect-2 AT ROW 1 COL 2
    rect-1 AT ROW 25.5 COL 2
    btn-ok AT ROW 26.3 COL  20 LABEL "UPDATE"
    btn-close colon 150 LABEL "CANCEL" SKIP(0.5) 
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d scrollable
         TITLE "KSERVICE SETTINGS".

ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
   DO TRANSACTION:
     ASSIGN simctr2. 
    END. 
    RELEASE simctr2.
     APPLY 'close' TO THIS-PROCEDURE IN FRAME frm-input.
     APPLY 'quit' TO FRAME frm-input.
END.

/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
FIND FIRST fdmsDevice NO-LOCK NO-ERROR.
IF AVAILABLE fdmsDevice THEN DO:
    device =  simctr.repDir.
END.
IF NOT AVAILABLE fdmsDevice THEN DO:
    device = "".
END.
FIND FIRST SIMCTR2 EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
IF AVAILABLE SIMCTR2 THEN
    ASSIGN
     simctr2.scr_path  = substring(simctr.repDir,1,(LENGTH(simctr.repDir) - 8)) + "zimra\scr"  + string(fdmsDevice.deviceID) + ".csv" WHEN device <> "".
    FIND FIRST SIMCTR2 EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
    DISPLAY SIMCTR2 WITH FRAME FRM-INPUT.
IF NOT AVAILABLE SIMCTR2 THEN
DO:
    CREATE simctr2.    ASSIGN
        simctr2.cocode = simctr.cocode.
        FIND FIRST SIMCTR2 EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
        DISPLAY SIMCTR2 WITH FRAME FRM-INPUT.
     
END.
VIEW FRAME frm-input.
ENABLE ALL WITH FRAME frm-input.
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
HIDE FRAME frm-input.
RETURN.
