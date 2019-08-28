//CSU0115A JOB (ASM),'C3121',CLASS=A,MSGCLASS=A,NOTIFY=&SYSUID,         00010099
//   MSGLEVEL=(0,0),TIME=(,2),COND=(4,LT)                               00020099
//****************************************************************      00030099
//*   NEXT THREE PARAMETERS MUST BE SET BEFORE COMPILES!         *      00040099
//****************************************************************      00050099
//*      SET   CICS=0    ** NO, NOT IN CICS CLASS **                    00060099
//       SET   CICS=1    ** YES, ALSO IN CICS CLASS **                  00070099
//       SET   ID=15     ** SET ONE TIME TO LAST TWO DIGITS OF ID       00080099
//       SET   PROG=6    ** CHANGE TO MATCH EACH PROGRAM NUMBER         00090099
//****************************************************************      00100099
//ASM    EXEC   PROC=HLASMCLG                                           00110099
//SYSIN  DD     *                                                       00120099
         TITLE  'SKELETON ASSEMBLER PROGRAM'                            00130099
         PRINT  ON,NODATA,NOGEN                                         00140099
******************************************************************      00150099
*                                                                *      00160099
*   PROGRAMMER:  ADAM DAVIES                                     *      00170099
*   COMMENTS  :  ASM PROGRAM 6                                   *      00180099
*                                                                *      00190099
******************************************************************      00200099
*   STANDARD BEGINNING FOR ASSEMBLER PROGRAMS                    *      00210099
******************************************************************      00220099
PROG6    CSECT                                                          00230099
         STM   R14,R12,12(R13)         STORE EXISTING REGISTERS         00240099
         LR    R12,R15                 ESTABLISH 1ST BASE REG           00250099
         USING PROG6,R12,R11,R10       DEFINING THREE BASE REGS         00260099
         LAY   R11,PROG6+4096          SECOND BASE REG + 4K             00270099
         LAY   R10,PROG6+8192          THIRD BASE REG + 8K              00280099
         ST    R13,SAVEAREA+4          BACKWARD CHAIN CALLER'S          00290099
         LA    R13,SAVEAREA            ADDRESS OF MY SAVE AREA          00300099
******************************************************************      00310099
* BEGIN THE PROGRAM LOGIC. FIRST OPEN THE INPUT AND OUTPUT FILES *      00320099
******************************************************************      00330099
         OPEN  (FILEIN1,(INPUT))       OPEN INPUT FILE                  00340099
         OPEN  (FILEOUT1,(OUTPUT))     OPEN OUTPUT FILE                 00350099
*        PUT   FILEOUT1,PRHEAD         OUTPUT THE HEADER                00360099
         GET   FILEIN1,RECIN1          GET THE FIRST RECORD, IF ONE     00370099
******************************************************************      00380099
*        READ AND PRINT MAIN PROGRAM LOOP                        *      00390099
******************************************************************      00400099
LOOP     EQU   *                                                        00410099
         BAS   R8,DATAVAL              THIS WILL TEST DATA IN           00420099
         CLC   FLAGYP,=CL1'F'          TESTING IF DATA IS GOOD          00430099
         BE    FLAGYF                                                   00440099
         BAS   R8,SETUP                USE SUBROUTINE FOR LINE SETUP    00450099
         BAS   R8,INHEAD                                                00460099
ILOOP    EQU   *                                                        00470099
         BAS   R8,CALPT1               THIS WILL FIND COST&SUM #        00480099
         BAS   R8,PRINTSET             THIS IS TO FIND THE AVG          00490099
         AP    PKN,=PL1'1'                                              00500099
         PUT   FILEOUT1,RECOUT1        OUTPUT THE DETAIL RECORD!        00510099
         CP    PKTIME,PKN              IF MONTHS R !DONE B 2 ILOOP      00520099
         BNL   ILOOP                                                    00530099
         BAS   R9,LASTPR                                                00540099
         PUT   FILEOUT1,RECOUT1        OUTPUT THE DETAIL RECORD!        00550099
FLAGYF   EQU   *                       BAD DATA INPUT                   00560099
         GET   FILEIN1,RECIN1          GET THE NEXT RECORD              00570099
         B     LOOP                    GO BACK AND PROCESS              00580099
******************************************************************      00590099
*        END OF INPUT PROCESSING                                 *      00600099
******************************************************************      00610099
EOF1     EQU   *                                                        00620099
         CLOSE (FILEIN1)               CLOSE INPUT FILE                 00630099
         CLOSE (FILEOUT1)              CLOSE OUTPUT FILE                00640099
         L     R13,SAVEAREA+4          POINT AT OLD SAVE AREA           00650099
         LM    R14,R12,12(R13)         RESTORE THE REGISTERS            00660099
         LA    R15,0                   RETURN CODE = 0                  00670099
         BR    R14                     RETURN TO OPERATING SYSTEM       00680099
******************************************************************      00690099
*       DATAVAL VALADATION OF THE DATA IN                        *      00700099
******************************************************************      00710099
DATAVAL  EQU   *                       DATA VALADATION SUBROUTINE!      00720099
         MVC   FLAGYP,=CL1'T'          SETS FLAG TO TRUE                00730099
         PACK  PKPV,PV                 PACK IN THE DATA                 00740099
         TP    PKPV                    TESTED PRICE                     00750099
         BNZ   FAILED                  FAILED TEST                      00760099
         PACK  PKRATE,RATE             PACK IN THE DATA                 00770099
         TP    PKRATE                  TESTED PRICE                     00780099
         BNZ   FAILED                  FAILED TEST                      00790099
         PACK  PKTIME,TIME             PACK IN THE DATA                 00800099
         TP    PKTIME                  TESTED PRICE                     00810099
         BNZ   FAILED                  FAILED TEST                      00820099
         PACK  PKPMT,PMT                                                00830099
         TP    PKPMT                   TESTED UNITS                     00840099
         BNZ   FAILED                  FAILED TEST                      00850099
         AP    PKCOUNT,=PL2'1'         ADDS ONE TO PKCOUNT IF GOOD      00860099
         BR    R8                                                       00870099
FAILED   EQU   *                                                        00880099
         MVC   FLAGYP,=CL1'F'          FAILED TO PACK OR B>A            00890099
         BR    R8                      RETURN TO INST AFTER BAS!        00900099
******************************************************************      00910099
*        ADDED (SUB)ROUTINES FOLLOW THE SETUP ROUTINE!           *      00920099
******************************************************************      00930099
SETUP    EQU   *                       NAME OF FIRST SUBROUTINE!        00940099
*        MVC   PKTIME,=CL2'  '         *** WHAT??? ***                  00950099
*  *** ABOVE LINE CAUSES NONNUMERIC DATA IN PACKED FIELD!! S0C7!!       00960099
*        SP    PKTIME,PKTIME           ZEROS PKTIME TOO SOON??          00970099
         MVC   PRTIME(6),=CL6' '       REMOVE TOTALS FROM PRINTLINE     00980099
         ZAP   PKN,=PL1'1'                                              00990099
         ZAP   PKCOUNT,=P'0'           RESETS COUNTER                   01000099
         ZAP   PKSUMPV,=P'0'           RESETS THE SUMS                  01010099
         ZAP   PKSUMR,=P'0'                                             01020099
         ZAP   PKSUMPMT,=P'0'                                           01030099
         BR    R8                      RETURN TO INST AFTER BAS!        01040099
******************************************************************      01050099
*     INHEAD  (SUB)ROUTINES  THIS IS TO SET UP INNER HEADER     *       01060099
******************************************************************      01070099
INHEAD   EQU   *                       NAME OF 3RD SUBROUTINE!          01080099
         PUT   FILEOUT1,PRHEAD         OUTPUT THE first HEADER          01090099
         MVC   H1PV,EDMASK                                              01100099
         ED    H1PV,PKPV                                                01110099
*        SRP   PKPMT,64-2,0,         **WHY SCALE THE INPUT?             01120099
         MVC   H1PMT,EDMASKPM         I notice the dec was not in       01130099
         ED    H1PMT,PKPMT            it's home was trying to push      01140099
         MVC   H1R,EDMASKR            it in                             01150099
         ED    H1R,PKRATE                                               01160099
         MVC   H1N,EDMASKN                                              01170099
         ED    H1N,PKTIME                                               01180099
         PUT   FILEOUT1,HEAD2          OUTPUT THE SECOND HEADER         01190099
         BR    R8                                                       01200099
******************************************************************      01210099
*     CALPT1  (SUB)ROUTINES  THIS IS TO CALCULATE INTREST        *      01220099
******************************************************************      01230099
CALPT1   EQU   *                       NAME OF 2ND SUBROUTINE!          01240099
         ZAP   PKRAMTR,PKPV                                             01250099
         MP    PKRAMTR,PKRATE                                           01260099
         DP    PKRAMTR,N12             SET INT PER MONTH                01270099
         ZAP   PKPMT2,PKPMT                                             01280099
         SP    PKPMT2,PKINT                                             01290099
         CLC   PKPV,PKPMT                                               01300099
         BNH   ZEROME                                                   01310099
         ZAP   PKPMT,PKPV                                               01320099
         ZAP   PKPV,=PL1'0'                                             01330099
         AP    PKSUMPMT,PKPMT          SUM OF PMT                       01340099
         AP    PKSUMR,PKRAMT           SUM OF I                         01350099
         BR    R8                                                       01360099
ZEROME   EQU   *                                                        01370099
         SP    PKPV,PKPMT                                               01380099
         AP    PKSUMPMT,PKPMT          SUM OF PMT                       01390099
         AP    PKSUMR,PKRAMT           SUM OF I                         01400099
         BR    R8                                                       01410099
******************************************************************      01420099
*     JUNKME  (SUB)ROUTINES  THIS IS TO FIND THE SUM VALUES      *      01430099
******************************************************************      01440099
*        SRP   PKPRICE,4,0                                              01450099
*        SRP   PKUNITS,2,0                                              01460099
*        ZAP   PKCOSTSW,PKCOSTS                                         01470099
*        AP    PKSUMC,PKCOSTSW         THIS ADDS TO THE SUME EACH       01480099
*        MVC   UNITS1,EDMASKU          MOVE EDITMASK IN FOR OUTPUT      01490099
*        MVC   PRICE1,EDMASKP                                           01500099
*        MVC   COST1,EDMASKC                                            01510099
*        ED    UNITS1,PKUNITS                                           01520099
*        ED    PRICE1,PKPRICE                                           01530099
*        ED    COST1,PKCOSTSW                                           01540099
*        BR    R8                                                       01550099
******************************************************************      01560099
*     PRINTFUN(SUB)ROUTINES  THIS IS TO SET UP EDIT MASK @HERE   *      01570099
******************************************************************      01580099
PRINTSET EQU   *                       NAME OF 3RD SUBROUTINE!          01590099
         MVC   PRTIME,EDMASKN                                           01600099
         ED    PRTIME,PKN                                               01610099
         MVC   PRPMT,EDMASK                                             01620099
         ED    PRPMT,PKPMT                                              01630099
         MVC   PRINST,EDMASK                                            01640099
         ED    PRINST,PKINTS                                            01650099
         MVC   PRPINC,EDMASK                                            01660099
         ED    PRPINC,PKPRINC                                           01670099
         MVC   PRPV,EDMASK                                              01680099
         ED    PRPV,PKPV                                                01690099
         BR    R8                                                       01700099
******************************************************************      01710099
*       PRINT LAST PRINT FOR DATA OUTPUT                 @HERE   *      01720099
******************************************************************      01730099
LASTPR   EQU   *                       NAME OF 4TH SUBROUTINE!          01740099
         MVC   PRTIME(6),=CL6'TOTALS'                                   01750099
         MVC   PRPMT,EDMASK                                             01760099
         ED    PRPMT,PKSUMPMT                                           01770099
         MVC   PRINST,EDMASK                                            01780099
         ED    PRINST,PKSUMR                                            01790099
         PACK  PKPV,PV                                                  01800099
         MVC   PRPINC,EDMASK                                            01810099
         ED    PRPINC,PKPV                                              01820099
         BR    R9                                                       01830099
******************************************************************      01840099
*    WORK AREA DATA DEFINITIONS                                  *      01850099
******************************************************************      01860099
********CORE VALUES********                                             01870099
PKPV     DC    PL06'0'        PV VALE IN PV,PMT,FV,N,R,T                01880099
PKRAMTR  DC   0PL10'0'        PV*R RAW                                  01890099
PKINT    DC    PL08'0'                                                  01900099
JUNK     DC    PL02'0'                                                  01910099
PKRAMT   DC    PL06'0'        PV*R FINISHED                             01920099
PKRATE   DC    PL03'0'        RATE                                      01930099
N12      DC    PL02'12'       RATE TO / BY                              01940099
*KNRATE  DC    PL05'0'        THIS MIGHT NOT BE NEEDED                  01950099
PKPMT    DC    PL06'0'        PMT                                       01960099
PKPMT2   DC    PL06'0'        PMT - I                                   01970099
PKTIME   DC    PL02'0'        TIME                                      01980099
PKCOUNT  DC    PL02'0'        counter                                   01990099
PKINTS   DC    PL06'0'        INTREST CALULATED                         02000099
PKN      DC    PL02'0'        COUNTS UP                                 02010099
PKPRINC  DC    PL06'0'        THE PRICEABLE POST INTREST                02020099
PKSUMPV  DC    PL07'0'        SUMS PV                                   02030099
PKSUMR   DC    PL07'0'        SUM RATE                                  02040099
PKSUMPMT DC    PL07'0'        SUM PMT                                   02050099
********FLAGES*************                                             02060099
FLAGYP   DC    CL1'F'             DATA IS GOOD                          02070099
********EDITMASKS**********                             @HERE           02080099
* A RULER FOR 20'S:  1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0            02090099
EDMASK   DC    XL16'402020206B2020206B2021204B202060'         6P        02100099
EDMASKPM DC    XL19'4020206B2020206B2020206B2021204B202060'   7P        02110099
* change 40-60                                                          02111099
EDMASKR  DC    XL08'4020204B20202060'                         3P        02120099
EDMASKN  DC    XL05'4020212040'                               2P        02130099
******************************************************************      02140099
*     INPUT FILE - DATA CONTROL BLOCK                            *      02150099
******************************************************************      02160099
FILEIN1  DCB   DSORG=PS,                                               X02170099
               MACRF=(GM),                                             X02180099
               DEVD=DA,                                                X02190099
               DDNAME=FILEIN1,                                         X02200099
               EODAD=EOF1,                                             X02210099
               RECFM=FB,                                               X02220099
               LRECL=80                                                 02230099
******************************************************************      02240099
*    INPUT RECORD AREA                                           *      02250099
******************************************************************      02260099
RECIN1   DS   0CL80           FULL RECORD DEFINITION (80 BYTES)         02270099
PV       DS    CL09           THIS THE AMOUNT THAT ENTERS               02280099
         DS    CL01                                                     02290099
RATE     DS    CL05           THIS IS THE RATE OF LOAN                  02300099
         DS    CL05                                                     02310099
PMT      DS    CL07           THIS IS THE PAYMENT                       02320099
         DS    CL03                                                     02330099
TIME     DS    CL03           THIS IS THE TIME IN MONTHS                02340099
         DS    CL47                                                     02350099
******************************************************************      02360099
*     OUTPUT FILE - DATA CONTROL BLOCK                           *      02370099
******************************************************************      02380099
FILEOUT1 DCB   DSORG=PS,                                               X02390099
               MACRF=(PM),                                             X02400099
               DEVD=DA,                                                X02410099
               DDNAME=FILEOUT1,                                        X02420099
               RECFM=FM,                                               X02430099
               LRECL=80                                                 02440099
******************************************************************      02450099
*    OUTPUT RECORD AREAS                                         *      02460099
******************************************************************      02470099
*      HERE IS THE HEADER FOR ** C S U **                               02480099
******************************************************************      02490099
PRHEAD   DS   0CL80                                                     02500099
PRC1     DC    CL01' '           PRINT CONTROL - SINGLE SPACE           02510099
         DC    CL05' '           SPACING OF 05 CHARACTERS               02520099
         DC    CL40'CSU Amortization Schedule - Spring 2019'            02530099
         DC    CL34' - 0115 Adam Davies'                                02540099
******************************************************************      02550099
HEAD2    DC   0CL80                                                     02560099
PRC4     DC    CL01' '                                                  02570099
         DC    CL07'Amount:'                                            02580099
H1PV     DC    CL16' '                                                  02590099
         DC    CL09' Payment:'                                          02600099
H1PMT    DC    CL18' '                                                  02610099
         DC    CL06' Rate:'                                             02620099
H1R      DC    CL08' '                                                  02630099
         DC    CL06' Term:'                                             02640099
H1N      DC    CL04' '                                                  02650099
         DC    CL09' '                                                  02660099
******************************************************************      02670099
* HERE IS THE DETAIL OUTPUT LINE                                 *      02680099
******************************************************************      02690099
RECOUT1  DS   0CL80              PRINT AREA                             02700099
PRC2     DC    CL01' '           PRINT CONTROL CHARACTER                02710099
PRTIME   DC    CL05' '                                                  02720099
         DC    CL01' '                                                  02730099
PRPMT    DC    CL19' '                                                  02740099
PRINST   DC    CL19' '                                                  02750099
PRPINC   DC    CL19' '                                                  02760099
PRPV     DC    CL16' '                                                  02770099
         DC    CL05' '           SPACING TO MAKE 80 CHAR RECORD         02780099
******************************************************************      02790099
*    REGISTER SAVE AREA                                          *      02800099
******************************************************************      02810099
SAVEAREA DS  18F                 ROOM FOR STORAGE OF REGISTERS          02820099
******************************************************************      02830099
*     REGISTER EQUATES                                           *      02840099
******************************************************************      02850099
R0       EQU   0                                                        02860099
R1       EQU   1                                                        02870099
R2       EQU   2                                                        02880099
R3       EQU   3                                                        02890099
R4       EQU   4                                                        02900099
R5       EQU   5                                                        02910099
R6       EQU   6                                                        02920099
R7       EQU   7                                                        02930099
R8       EQU   8                                                        02940099
R9       EQU   9                                                        02950099
R10      EQU   10                                                       02960099
R11      EQU   11                                                       02970099
R12      EQU   12                                                       02980099
R13      EQU   13                                                       02990099
R14      EQU   14                                                       03000099
R15      EQU   15                                                       03010099
******************************************************************      03020099
*    LITERAL POOL - THIS PROGRAM MAY USE LITERALS.                      03030099
******************************************************************      03040099
         LTORG *                                                        03050099
         END   PROG6                                                    03060099
/*                                                                      03070099
//G.SYSABOUT DD SYSOUT=*                                                03080099
//G.SYSUDUMP DD SYSOUT=*                                                03090099
//G.FILEOUT1 DD SYSOUT=*,OUTLIM=2500                                    03100099
//*.FILEIN1  DD DSN=CSU0&CICS&ID..C3121.ASM(DATAPRG&PROG),DISP=SHR      03110099
//G.FILEIN1  DD DSN=CSU.PUBLIC.DATA(DATAPRG&PROG),DISP=SHR              03120099
//                                                                      03130099
