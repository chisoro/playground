FOR EACH igltdf.
   CREATE GLTDF.
   DISPLAY acct-no. PAUSE 0.
    ASSIGN  gltdf.acc = igltdf.acct-no
            gltdf.AMT  = igltdf.amount
            gltdf.CREDATE = igltdf.eff-date
            gltdf.DESCRIP = igltdf.description       
            gltdf.period = igltdf.per
            gltdf.REF    =  igltdf.reference
            gltdf.source = igltdf.source
            gltdf.TransID = igltdf.link-no
            gltdf.trDATE = igltdf.datex
            gltdf.UID    = "1"
            gltdf.UID2   = "1".
    IF LENGTH(STRING(gltdf.acc)) = 6 THEN
        gltdf.acc = dec("100" + STRING(gltdf.acc)).
    ELSE IF LENGTH(STRING(gltdf.acc)) = 8 THEN
        gltdf.acc = dec("1" + STRING(gltdf.acc)).

END.
