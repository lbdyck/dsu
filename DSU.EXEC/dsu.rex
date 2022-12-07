  /* ==============================* REXX *==============================*/
  /*     TYPE:  ISPF 3.4 COMMAND                                        */
  /*  PURPOSE:  ALLOCATE, COPY AND DELETE A PDS, PDSE OR SEQ DSN        */
  /*    OWNER:  MAINFRAME OPERATING SYSTEMS SERVICES (MOSS)             */
  /*--------------------------------------------------------------------*/
  /* FLG  YYMMDD  USERID   DESCRIPTION                                  */
  /* ---  ------  -------  -------------------------------------------- */
  /* xxx  221206  xxx      Error msg if PDSE Gen Mbr is Load Module     */
  /* lbd  221113  LBD      Clean up for public distribution             */
  /* lbd  221110  LBD      Add dsn prompt if not called from 3.4        */
  /* lbd  161026  LBD      Change to use normal ISPF/TSO dsname         */
  /*                       conventions (prefix, noprefix)               */
  /*                       Commented out IAM                            */
  /* lbd  160727  LBD      add system maxgen                            */
  /* lbd  160726  LBD      Commented out libdef/altlibs, ckdsn and      */
  /*                       changed msg stk001 to isrz001                */
  /* @D9  160722  DSU      Added capability to copy PDSE V2 members     */
  /* @D8  160323  DSU      Removed checking ZSCTPREF variable for WFS   */
  /* @D7  150817  DSU      Allow copying a PDS(MBR) to Sequential file  */
  /* @D6  131204  DSU      Reduce storage usage                         */
  /* @D5  130808  DSU      If DSUCPY=NO, do not copy file               */
  /* @D4  130808  DSU      Added code for EXTENDED sequential files     */
  /* @D3  130130  DSU      Fix copy PDSE allocated in BLKSIZE           */
  /* @D2  110619  DSU      Added completion message                     */
  /* @D1  110409  DSU      Keep entered blocksize when RECFM=U          */
  /* @C9  110220  DSU      Chged security chk to validate PDSE files    */
  /* @C8  110217  DSU      Prevent Dir Blocks=0 and DS Type=LIBRARY     */
  /* @C7  101029  DSU      Check security authorization                 */
  /* @C6  101018  DSU      Added support for IAM files                  */
  /* @C5  101013  DSU      Allow copying SAS datasets                   */
  /* @C4  100927  DSU      Do not allow a member name in dataset name   */
  /* @C3  100808  DSU      Fixed allocation error for RECFM=VBM files   */
  /* @C2  100706  DSU      Fixed abend when coping empty seq file       */
  /* @C1  100704  DSU      Save data entered, incase redisplay of screen*/
  /* @B9  100414  DSU      Added multi-volume seq. file support         */
  /* @B8  100225  DSU      Fixed RECFM(F B S), was RECFM(FBS)           */
  /* @B7  100223  DSU      Validate New DSN is less than 44 chars       */
  /* @B6  100223  DSU      Allow Muti-Volume files (SYSREASON=19)       */
  /* @B5  100210  DSU      Fix deleting old DSN, wrong variable names   */
  /* @B4  091101  DSU      Allow non-SMS datasets, SYSREASON=30         */
  /* @B3  091031  DSU      If DSN was deleted and DSLIST not Refreshed  */
  /* @B2  091031  DSU      Added LIBRARY and comments                   */
  /* @B1  091030  DSU      Chged msgs to be more meaningfull            */
  /* @A9  091030  DSU      Fixed copying Seq. files, removed FROMMEM(*) */
  /* @A8  091030  DSU      Added code for RECFM=FBA and RECFM=VBA       */
  /* @A7  091030  DSU      Fix SETMSG error... was not in ISPEXEC       */
  /* @A6  091025  DSU      Validate dsn is a PDS, PDSE, & Seq file      */
  /* @A5  091025  DSU      Chged msg to be more generic                 */
  /* @A4  091022  DSU      Validate new dsn doesn't already exist       */
  /* @A3  091001  DSU      FIXED DSUTYPE of 'PROGRAM_LIBRARY'           */
  /* @A2  090931  DSU      FIXED 'SEQ' FILE TYPE                        */
  /* @A1  090918  DSU      FIXED BLOCK ISSUE                            */
  /* @A0  090806  DSU      DEVELOPED REXX                               */
  /*====================================================================*/
  X        = MSG("OFF")
  PANEL01  = "DSU"
  PANEL02  = "DSU2"
  PGMCOPY  = "PDSEGENC"
  MULTIVOL = "N"
  DSUCPY   = "YES"
  DSUDEL   = "NO "
  DSUEXP   = 0
  DONE     = "N"
  PARSE VALUE MVSVAR("SYSPLEX") WITH 1 SYSPLEX 4 .

  NUMERIC DIGITS 10
  CVT      = C2D(Storage(10,4))
  CVTDFA   = C2D(Storage(D2X(CVT + 1216),4))   /* cvt + 4c0 */
  mgen     = C2D(Storage(D2X(cvtdfa + 76),4))  /* dfa + 4c */
  PANEL = PANEL02

  ADDRESS ISPEXEC
  ARG DSUDSNO
  IF (DSUDSNO = "?") THEN DO
    "DISPLAY PANEL(#DSU)"
    EXIT
  END

  IF (DSUDSNO = "") THEN DO
    'AddPop Row(5) Column(8)'
    'display panel(dsup)'
    xrc = rc
    'rempop'
    if xrc > 0 then
    EXIT
  END

  ADDRESS ISPEXEC
  CALL DSNATTR
  DO WHILE DONE = "N"
    "DISPLAY PANEL("PANEL")"
    IF (RC = 08) THEN LEAVE
    dsudsno = fix_dsn(dsudsno)
    dsudsnn = fix_dsn(dsudsnn)
    CALL ALLOCDSN
  END
  EXIT

  /* ----------------------------------------------------- *
  | Fix_DSN - fixup dsn to fully qualified without quotes |
  * ----------------------------------------------------- */
Fix_dsn: procedure
  arg dsn
  if left(dsn,1) = "'" then dsn = strip(dsn,,"'")
  else do
    if sysvar('syspref') = null
    then do
      dsn = dsn
    end
    else do
      hlq = sysvar('syspref')
      dsn = hlq'.'dsn
    end
  end
  return dsn
  /*--------------------------------------------------------------------*/
  /*   Obtain Input dataset's attributes                                */
  /*--------------------------------------------------------------------*/
DSNATTR:

  DSUDSNO = STRIP(DSUDSNO,,"'")
  DSUDSNO = STRIP(DSUDSNO,,'"')
  DSUDSNO = "'"DSUDSNO"'"

  DSUALLOC = ""
  IF (DSUEXP = 0) THEN
  DSUEXP  = ""

  X = LISTDSI(DSUDSNO" DIRECTORY NORECALL SMSINFO")
  TMPMSG1 = SUBSTR(SYSMSGLVL2,11,1)
  TMPMSG2 = SUBSTR(SYSMSGLVL2,12)
  TMPMSG2 = TRANSLATE(TMPMSG2,'abcdefghijklmnopqrstuvwxyz',,
    'ABCDEFGHIJKLMNOPQRSTUVWXYZ')
  ERRMSG  = TMPMSG1||TMPMSG2

  IF (SYSREASON = 5) THEN DO          /* Cataloged?        */
    zedsmsg = "DSN Doesn't Exist"
    zedlmsg = "Dataset does not exist. ",
      "Please enter a valid dataset name. ",
      ERRMSG
    "SETMSG MSG(isrz001)"
    ZDLMSG  = "Not Cataloged"
    "VPUT (ZDLMSG)"
    EXIT
  END

  IF (SYSREASON = 8) THEN DO          /* Tape?            */
    zedsmsg = "Tape - Not Supported"
    zedlmsg = "Tape datasets are not supported. ",
      "Dataset must be a PDS, PDS-E,",
      "or Sequential file. ",
      ERRMSG
    "SETMSG MSG(isrz001)"
    ZDLMSG  = "Error - Tape DSN"
    "VPUT (ZDLMSG)"
    EXIT
  END

  IF (SYSREASON = 9) THEN DO          /* Migrated?        */
    zedsmsg = "Dataset is Migrated"
    zedlmsg = "Please recall dataset,",
      "before using this utility. ",
      ERRMSG
    "SETMSG MSG(isrz001)"
    ZDLMSG  = "Migrated DSN"
    "VPUT (ZDLMSG)"
    EXIT
  END

  IF (SYSREASON = 11) THEN DO         /* Unauthorized?    */
    zedsmsg = "Unauthorized Access"
    zedlmsg = "Userid ("USERID()") is not",
      "authorized access to dataset",
      DSUDSNO
    "SETMSG MSG(isrz001)"
    ZDLMSG  = "Not Authorized"
    "VPUT (ZDLMSG)"
    EXIT
  END

  IF (SYSREASON = 12) THEN DO         /* VSAM?            */
    zedsmsg = "VSAM - Not Supported"
    zedlmsg = "VSAM datasets are not supported. ",
      "Dataset must be a PDS, PDS-E,",
      "or Sequential file. ",
      ERRMSG
    "SETMSG MSG(isrz001)"
    ZDLMSG  = "Not PDS/Seq File"
    "VPUT (ZDLMSG)"
    EXIT
  END

  IF (SYSREASON = 19) THEN DO         /* Mult-Volume?     */
    zedsmsg = "Multi-Volume File"
    zedlmsg = "Multi-Volume sequential",
      "datasets are supported."
    "SETMSG MSG(isrz001)"
    MULTIVOL = "Y"
  END

  IF (MULTIVOL <> "Y") &,             /* Multivolume?     */
    (SYSREASON > 0) &,               /* All other errors */
    (SYSREASON <> 30) THEN DO        /* SMS Managed?     */
    zedsmsg = "Not PDS/Seq File"
    zedlmsg = "Dataset does not exist. ",
      "Please enter a valid dataset name. ",
      ERRMSG
    "SETMSG MSG(isrz001)"
    ZDLMSG  = "Not PDS/Seq File"
    "VPUT (ZDLMSG)"
    EXIT
  END

  DSUDSNN = SYSDSNAME
  DSUVOL  = SYSVOLUME
  DSUUNIT = SYSUNIT
  DSUORG  = SYSDSORG
  DSURCF  = SYSRECFM
  DSURCL  = SYSLRECL
  DSUBLK  = SYSBLKSIZE
  DSUSPU  = SYSUNITS
  DSUSPP  = SYSPRIMARY
  DSUSPS  = SYSSECONDS
  DSUSMSD = SYSDATACLASS
  DSUSMSS = SYSSTORCLASS
  DSUSMSM = SYSMGMTCLASS
  DSUDIR  = SYSADIRBLK
  DSUTYPE = SYSDSSMS
  /*-------------------------------------------------------*/
  /*  Obtain PDSE version and member generations           */
  /*-------------------------------------------------------*/
  "DSINFO DATASET("DSUDSNO")"
  DSUVER  = ZDSDSNV
  DSUGEN  = ZDSNGEN
  dsuigen = dsugen

  MBRCOL = POS("(",DSUDSNO)
  IF (DSUTYPE = "LIBRARY") |,           /* Empty PDSE      */
    (DSUTYPE = "DATA_LIBRARY") |,      /* PDSE with mbrs  */
    (DSUTYPE = "PROGRAM_LIBRARY") |,   /* Executables     */
    (DSUTYPE = "SEQ") |,
    (DSUTYPE = "PDS") |,
    (DSUTYPE = "PDSE") THEN
  NOP
  ELSE DO
    zedsmsg = "Not PDS/SEQ File"
    zedlmsg = "Dataset must be a PDS, PDS-E,",
      "or Sequential file"
    "SETMSG MSG(isrz001)"
    ZDLMSG  = "Not PDS/Seq File"
    "VPUT (ZDLMSG)"
    EXIT
  END
  IF (DSUTYPE = "PROGRAM_LIBRARY") THEN  /* Executables */
  DSUTYPE  = "PDSE"
  IF (DSUTYPE = "DATA_LIBRARY") THEN     /* PDSE with mbrs */
  DSUTYPE  = "PDSE"
  IF (DSUTYPE = "LIBRARY") THEN          /* Empty PDSE  */
  DSUTYPE  = "PDSE"
  IF (DSUTYPE = "SEQ") THEN
  DSUTYPE  = ""
  DSUTYPEO    = DSUTYPE
  IF (DSUDIR  = "NO_LIM") THEN
  DSUDIR   = ""
  IF (DSUSMSS <> "") THEN DO
    DSUVOL   = ""
    DSUUNIT  = ""
  END
  IF (SYSSEQDSNTYPE <> "") THEN DO
    IF (SYSSEQDSNTYPE = "EXTENDED") THEN
    DSUTYPE = "EXTREQ"
    ELSE
    DSUTYPE = SYSSEQDSNTYPE
  END
  DSUDSNN = STRIP(DSUDSNO,,"'")
  DSUDSNN = STRIP(SUBSTR(DSUDSNN,1,37))
  DSUDSNN = STRIP(DSUDSNN,,'.')
  DSUDSNN = DSUDSNN".CLONE"
  dsudsnn = "'"dsudsnn"'"

  IF (MBRCOL > 0) THEN DO
    DSUDSNN  = STRIP(SUBSTR(SYSDSNAME,1,MBRCOL-1))".CLONE"
    DSUDIR   = ""
    DSUORG   = "PS"
    DSUTYPE  = ""
    MULTIVOL = "Y"
  END

  /*-----------------------------------------*/
  /*   Save input fields, incase redisplay   */
  /*   of screen due to error                */
  /*-----------------------------------------*/
  TMPDSNO = DSUDSNO
  TMPDSNN = DSUDSNN
  TMPSMSD = DSUSMSD
  TMPSMSS = DSUSMSS
  TMPSMSM = DSUSMSM
  TMPVOL  = DSUVOL
  TMPUNIT = DSUUNIT
  TMPSPU  = DSUSPU
  TMPSPP  = DSUSPP
  TMPSPS  = DSUSPS
  TMPORG  = DSUORG
  TMPRCF  = DSURCF
  TMPRCL  = DSURCL
  TMPBLK  = DSUBLK
  TMPDIR  = DSUDIR
  TMPTYPE = DSUTYPE
  TMPEXP  = DSUEXP
  TMPVER  = DSUVER
  TMPGEN  = DSUGEN
  RETURN
  /*--------------------------------------------------------------------*/
  /*   Save input fields, incase redisplay of screen due to error       */
  /*--------------------------------------------------------------------*/
RESTORE_INPUT:
  DSUDSNO = TMPDSNO
  DSUDSNN = TMPDSNN
  DSUSMSD = TMPSMSD
  DSUSMSS = TMPSMSS
  DSUSMSM = TMPSMSM
  DSUVOL  = TMPVOL
  DSUUNIT = TMPUNIT
  DSUSPU  = TMPSPU
  DSUSPP  = TMPSPP
  DSUSPS  = TMPSPS
  DSUORG  = TMPORG
  DSURCF  = TMPRCF
  DSURCL  = TMPRCL
  DSUBLK  = TMPBLK
  DSUDIR  = TMPDIR
  DSUTYPE = TMPTYPE
  DSUEXP  = TMPEXP
  RETURN
  /*--------------------------------------------------------------------*/
  /*   Allocate / Copy DSN                                              */
  /*--------------------------------------------------------------------*/
ALLOCDSN:

  ADDRESS ISPEXEC
  DSUALLOC = ""
  IF (POS("'",DSUDSNO) = 0) THEN
  DSUDSNO  = "'"DSUDSNO"'"
  IF (POS("'",DSUDSNN) = 0) THEN
  DSUDSNN  = "'"DSUDSNN"'"
  IF (DSUTYPE = "PDSE") THEN
  DSUTYPE  = "LIBRARY,2"
  IF (DSURCF  = "FB") THEN
  DSURCF   = "F B"
  IF (DSURCF  = "FS") THEN
  DSURCF   = "F S"
  IF (DSURCF  = "FBA") THEN
  DSURCF   = "F B A"
  IF (DSURCF  = "FBS") THEN
  DSURCF   = "F B S"
  IF (DSURCF  = "FBM") THEN
  DSURCF   = "F B M"
  IF (DSURCF  = "VB") THEN
  DSURCF   = "V B"
  IF (DSURCF  = "VBA") THEN
  DSURCF   = "V B A"
  IF (DSURCF  = "VBS") THEN
  DSURCF   = "V B S"
  IF (DSURCF  = "VBM") THEN
  DSURCF   = "V B M"
  IF (DSURCF  <> "") THEN
  DSUALLOC = DSUALLOC" RECFM("DSURCF")"
  IF (DSURCL  <> "") THEN
  DSUALLOC = DSUALLOC" LRECL("DSURCL")"
  /*--------------------------------------------------------------------*/
  /*   IF (DSUTYPE = "LIBRARY") & (DSURCF <> "U") THEN                  */
  /*      DSUBLK   = ""                                                 */
  /*--------------------------------------------------------------------*/
  IF (DSUBLK  <> "") THEN
  DSUALLOC = DSUALLOC" BLKSIZE("DSUBLK")"
  IF (DSUDIR  <> "") & (DSUDIR <> 0) THEN
  DSUALLOC = DSUALLOC" DIR("DSUDIR")"
  IF (DSUSPU  =  "BLOCK") THEN
  DSUALLOC = DSUALLOC" BLOCK("DSUBLK")"
  ELSE IF (DSUSPU  <> "") THEN
  DSUALLOC = DSUALLOC" "DSUSPU
  IF (DSUSPP  <> "") THEN
  DSUALLOC = DSUALLOC" SP("DSUSPP DSUSPS")"
  IF (DSUORG  <> "") THEN
  DSUALLOC = DSUALLOC" DSORG("DSUORG")"
  IF (DSUUNIT <> "") THEN
  DSUALLOC = DSUALLOC" UNIT("DSUUNIT")"
  IF (DSUVOL  <> "") THEN
  DSUALLOC = DSUALLOC" VOL("DSUVOL")"
  IF (DSUSMSD <> "") THEN
  DSUALLOC = DSUALLOC" DATACLAS("DSUSMSD")"
  IF (DSUSMSM <> "") THEN
  DSUALLOC = DSUALLOC" MGMTCLAS("DSUSMSM")"
  IF (DSUSMSS <> "") THEN
  DSUALLOC = DSUALLOC" STORCLAS("DSUSMSS")"
  IF (DSUEXP  <> "") & (DSUEXP <> "0") THEN
  DSUALLOC = DSUALLOC" EXPDT("DSUEXP")"
  IF (DSUTYPE <> "") THEN
  DSUALLOC = DSUALLOC" DSNTYPE("DSUTYPE")"
  IF (DSUGEN  <> "") THEN
  DSUALLOC = DSUALLOC" MAXGENS("DSUGEN")"

  IF (SYSDSN(DSUDSNN) = "OK") THEN DO
    zedsmsg = "DSN already exists"
    zedlmsg = "This dataset already exists: "DSUDSNN
    CALL RESTORE_INPUT
    "SETMSG MSG(isrz001)"
    RETURN
  END
  IF (DSUGEN > 0) & (DSURCF = "U") THEN DO
     STKSMSG = "Mbr Gen can't be loadlib"
     STKLMSG = "PDSE member generation datasets,",
               "cannot be a loadlib/executables."
     ZDLMSG  = "Alloc/Copy/Del"
     ADDRESS ISPEXEC "SETMSG MSG(STK001)"
     CALL RESTORE_INPUT
     RETURN
  END

  ADDRESS TSO
  "FREE  FI(DSUTEMP)"
  "ALLOC FI(DSUTEMP) DA("DSUDSNN") NEW CATALOG "DSUALLOC
  IF (RC > 0) THEN DO
    ADDRESS ISPEXEC
    zedsmsg = "Allocation Error"
    zedlmsg = "Unable to allocate dataset: "DSUDSNN
    "SETMSG MSG(isrz001)"
    ADDRESS TSO
    "FREE  FI(DSUTEMP)"
    CALL RESTORE_INPUT
    RETURN
  END
  "FREE  FI(DSUTEMP)"

  IF (DSUCPY = "YES") THEN DO
    SELECT
      WHEN (MULTIVOL = "Y") & (DSUORG = "PS") THEN
      CALL MULTIVOL
      WHEN (DSURCF = "F S") THEN
      CALL CPYSAS
      WHEN (dsuigen = 0) THEN
      CALL NONMULTI
      OTHERWISE
      CALL NONMULTIGEN
    END
  END

  ADDRESS ISPEXEC
  IF (DSUCPY = "YES") & (DSUDEL = "NO") THEN DO
    zedsmsg = "Alloc/Copy Successful"
    zedlmsg = "Dataset was successfully allocated,",
      "copied, but not deleted."
    ZDLMSG  = "Alloc/Copy"
  END
  IF (DSUCPY = "YES") & (DSUDEL = "YES") THEN DO
    zedsmsg = "Alloc/Cpy/Del Successful"
    zedlmsg = "Dataset was successfully allocated,",
      "copied and deleted."
    ZDLMSG  = "Alloc/Copy/Del"
  END
  IF (DSUCPY = "NO") & (DSUDEL = "YES") THEN DO
    zedsmsg = "Alloc/Del Successful"
    zedlmsg = "Dataset was successfully allocated",
      "and deleted, but not copied."
    ZDLMSG  = "Alloc/Del"
  END
  IF (DSUCPY = "NO") & (DSUDEL = "NO") THEN DO
    zedsmsg = "Alloc Successful"
    zedlmsg = "Dataset was successfully allocated,",
      "but not copied and deleted."
    ZDLMSG  = "Alloc"
  END
  "SETMSG MSG(isrz001)"
  "VPUT (ZDLMSG)"

  ADDRESS TSO
  IF (DSUDEL = "YES") THEN
  "DELETE "DSUDSNO

  DONE = "Y"
  RETURN
  /*--------------------------------------------------------------------*/
  /*    Non-Multi-Volume                                                */
  /*--------------------------------------------------------------------*/
NONMULTI:

  ADDRESS ISPEXEC
  "LMINIT DATAID(DSUIDO) DATASET("DSUDSNO") ENQ(SHR)"
  "LMINIT DATAID(DSUIDN) DATASET("DSUDSNN") ENQ(SHR)"
  IF ((DSUTYPE  = "") | (DSUTYPE  = "BASIC"),
    |  (DSUTYPE  = "EXTREQ")) &,
    ((DSUTYPEO = "") | (DSUTYPEO = "BASIC"),
    |  (DSUTYPEO = "EXTREQ")) THEN
  "LMCOPY FROMID("DSUIDO") TODATAID("DSUIDN")",
    "REPLACE TRUNC"
  ELSE                               /* PDS File */
  "LMCOPY FROMID("DSUIDO") TODATAID("DSUIDN")",
    "REPLACE TRUNC FROMMEM(*)"
  "LMFREE DATAID("DSUIDN")"
  "LMFREE DATAID("DSUIDO")"

  RETURN
  /*--------------------------------------------------------------------*/
  /*    Non-Multi-Volume PDSE V2 with Member Generations           @D9  */
  /*--------------------------------------------------------------------*/
NONMULTIGEN:
  Address ISPExec ,
    "SELECT CMD(%"Pdsegenc DSUDSNO DSUDSNN" *)"

  RETURN
  /*--------------------------------------------------------------------*/
  /*    Multi-Volume - Copy Dataset                                     */
  /*--------------------------------------------------------------------*/
MULTIVOL:

  ADDRESS TSO
  "ALLOC FI(DSUDSNO) DA("DSUDSNO") SHR REUSE"
  "ALLOC FI(DSUDSNN) DA("DSUDSNN") SHR REUSE"
  "ALLOC  FI(SYSPRINT) NEW SP(1 1) CYL REUSE",
    "BLKSIZE(6254) LRECL(125) DSORG(PS)",
    "RECFM(V B A) UNIT(VIO)"
  "ALLOC  FI(SYSIN) NEW SP(1) TRACKS UNIT(VIO) LRECL(80)",
    "RECFM(F) REUSE"

  QUEUE "  REPRO INFILE(DSUDSNO) OUTFILE(DSUDSNN)"
  QUEUE
  "EXECIO * DISKW SYSIN (FINIS"

  ADDRESS ISPEXEC
  "SELECT PGM(IDCAMS)"

  ADDRESS TSO
  "FREE FI(DSUDSNO DSUDSNN SYSIN SYSPRINT)"

  RETURN
  /*--------------------------------------------------------------------*/
  /*  Copy SAS datasets                                            @C5  */
  /*--------------------------------------------------------------------*/
CPYSAS:
  DSNSASI  = "DSUSAS.SYSIN"RANDOM()
  DSNSASL  = "DSUSAS.LOG"RANDOM()

  ADDRESS TSO
  "FREE FI(DSUSASI DSUSASL)"
  "ALLOC FI(DSUSASI) NEW SP(5) TRACKS UNIT(SYSDA)",
    "LRECL(80) RECFM(F) REUSE DA("DSNSASI") CATALOG"
  "ALLOC FI(DSUSASL) NEW SP(1 1) CYLINDERS UNIT(SYSDA)",
    "LRECL(132) RECFM(V B A) DA("DSNSASL") CATALOG",
    "REUSE"

  QUEUE "  libname SASIN "DSUDSNO";"
  QUEUE "  libname SASOUT "DSUDSNN";"
  QUEUE "  proc copy in=sasin out=sasout;"
  QUEUE "  run;"
  QUEUE
  "EXECIO * DISKW DSUSASI (FINIS"
  "FREE FI(DSUSASI DSUSASL)"

  ADDRESS ISPEXEC
  "SELECT CMD(%PLPSAS INPUT("DSNSASI") LOG("DSNSASL")"

  RETURN
