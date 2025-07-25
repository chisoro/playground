DEF VAR X AS INt.
/*FOR EACH farloc:
    X = X + 1.
    farloc.town = X.
    DISPLAY farloc.loc-name farloc.building farloc.loc-no farloc.town.
    PAUSE 0.
    CREATE aloc.
    ASSIGN aloc.LCode = farloc.loc-no
           aloc.descrip = farloc.loc-name
           aloc.loc = X 
           aloc.area = farloc.building.
END.
FOR EACH farbdg:
    DISPLAY farbdg. PAUSE 0.
    CREATE area.
    ASSIGN Area.area = farbdg.code
           Area.descrip =  farbdg.descrip.
    
END.
FOR EACH farcls:
    DISPLAY farcls.code farcls.descrip. PAUSE 0.
    CREATE asubcat.
    ASSIGN asubcat.Alife  = farcls.useful-life
        asubcat.cat       = farcls.cat
        asubcat.Descrip   = farcls.descrip
        asubcat.DMethod   = 2
        asubcat.DPer      = farcls.bk-deprec-pc
        asubcat.subcat    = farcls.code .
END.
FOR EACH farcat:
    CREATE acat.
    ASSIGN acat.cat = farcat.code
           acat.Descrip = farcat.descrip.
END. */
FOR EACH faramf.
    DISPLAY CODE location. PAUSE 0.
    CREATE assmf.
    ASSIGN Assmf.Alife    = faramf.useful-life
        Assmf.Cost    = faramf.orig-cost
        Assmf.DepPer  = DEC(string(YEAR(faramf.acq-date)) 
                           + STRING(MONTH(faramf.acq-date),"99"))
        Assmf.Dept    = faramf.dept
        assmf.serial  = faramf.ser-no
        assmf.lPer    = faramf.rem-periods
        Assmf.descrip = faramf.descrip
        Assmf.DPer    = faramf.bk-deprec-pc
        Assmf.FUND    = 0 /*faramf.fund */
        Assmf.PDate  = faramf.acq-date
        Assmf.subcat  = faramf.class
        Assmf.syscode = faramf.CODE
        Assmf.acode = STRING(faramf.CODE)
        assmf.rcost   =  faramf.residual-val.
    FIND FIRST farloc WHERE farloc.loc-no = faramf.location  NO-ERROR.
       ASSIGN Assmf.Lcode    = farloc.loc-no.  
              Assmf.area   = farloc.building.
    FIND FIRST asubcat WHERE Asubcat.subcat = faramf.CLASS NO-ERROR.
        Assmf.cat = asubcat.cat.
         
END.
