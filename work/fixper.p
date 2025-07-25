DEF VAR wsamt LIKE gltdf.amt.
DEF BUFFER bftdf FOR gltdf.
FOR EACH gltdf WHERE gltdf.period >= 201801 AND gltdf.period <= 201812 
    BREAK BY gltdf.transid:
    IF FIRST-OF(gltdf.transid) THEN
        wsamt = 0.
    wsamt = wsamt + gltdf.amt.
    IF LAST-OF(gltdf.transid) AND wsAmt <> 0 THEN DO:
        /*FOR EACH bftdf WHERE bftdf.transid = gltdf.transid:
            DISPLAY bftdf.transid bftdf.acct bftdf.per bftdf.trdate 
                bftdf.SOURCE bftdf.amt WITH WIDTH 120.
        END.*/
        DISPLAY gltdf.transid wsamt.
        CREATE bftdf.
        BUFFER-COPY gltdf TO bftdf.
        ASSIGN bftdf.amt = wsAmt * -1
               bftdf.ref = "Sim67".
    END.
   

END.
