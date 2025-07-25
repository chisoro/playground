DEF VAR wsamt LIKE dbhtf.amt.
FOR EACH dbcmf:
    DISPLAY dbacc.
    wsAmt = 0.
    FOR EACH dbhtf WHERE dbhtf.dbacc = dbcmf.dbacc AND trdate >= 01/31/19:
        wsAmt = wsAmt + amt.
    END.
    IF wsAmt > = THEN
        DISPLAY dbcmf.acc NAME wsAmt.
END.
