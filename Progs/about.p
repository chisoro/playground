DEF BUTTON btn-Ok LABEL "OK".

DEF FRAME frm-About
    "SimAcc Verison 1.0.0"  COLON 5 SKIP(0.2)
    "2020 JobSim Enterprises (Pvt.} Ltd."  COLON 5 SKIP(0.2)
    "263-772 751 490/774 085 922"  COLON 5 SKIP(0.2)
    "All Rights Reserved"  COLON 5  SKIP(1.0) 
    "This is a Public Sector Accounting System."  COLON 5  SKIP(0.2) 
    "The system is designed to do all accounting Aspects from Billing," COLON 5  SKIP(0.2)
    "  Receipting, expenditure and other auxilary modules up to Financials"  COLON 5 SKIP(0.5)
    "Product Licensed under JobSim License Terms"  COLON 5 SKIP(1.5)
     btn-Ok COLON 30 
    WITH SIDE-LABELS BGCOLOR 2 FGCOLOR 1 FONT 6 VIEW-AS DIALOG-BOX AT  ROW 3 COL 10 SIZE 55 BY 12
    TITLE "About SimAcc ".  

VIEW FRAME frm-About.
ENABLE ALL WITH FRAME frm-About.
WAIT-FOR CHOOSE OF btn-Ok OR CLOSE OF THIS-PROCEDURE.
HIDE FRAME frm-About.
RETURN.
