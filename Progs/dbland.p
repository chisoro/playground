session:DATA-ENTRY-RETURN = TRUE.
/* Program.................dbland.p
   Notes:...... Land Sales/Lease file maintenance
   Author:.................S. Mawire
*/
DEF NEW SHARED VAR wsTransType LIKE landsale.ls.

DEF BUTTON btnOk LABEL "  OK  ".

DEF FRAME TransType
    SKIP(1)
    wsTransType  COLON 30 LABEL "Choose Transaction Type"
    btnOk SKIP(1)
    with TITLE "LAND SALE AND LEASE/RENTAL MAINTENANCE " view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED.


/* ***** Triggers  **** */

ON 'enter':U OF wsTransType 
DO:
    ASSIGN wsTransType.
    APPLY 'entry' TO btnOk.
    RETURN.
END.

ON 'choose':U OF BtnOk 
DO:
    ASSIGN wsTransType.
    IF wsTransType = 1 THEN
        RUN dbcmf02.p.
    ELSE 
        RUN dbcmf03.p.
    RETURN.
END.

/********** MAIN LOGIC **********/
ENABLE ALL WITH FRAME TransType.
wsTransType:SCREEN-VALUE = "1".
WAIT-FOR CHOOSE OF BtnOk OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.


