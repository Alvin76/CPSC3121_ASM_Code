//CSU0115A JOB (ASM),'C3121',CLASS=A,MSGCLASS=A,NOTIFY=&SYSUID,         00010049
//   MSGLEVEL=(0,0),TIME=(,2),COND=(4,LT)                               00020028
//****************************************************************      00030037
//*   NEXT THREE PARAMETERS MUST BE SET BEFORE COMPILES!         *      00040041
//****************************************************************      00050037
//*      SET   CICS=0    ** NO, NOT IN CICS CLASS **                    00060049
//       SET   CICS=1    ** YES, ALSO IN CICS CLASS **                  00070049
//       SET   ID=15     ** SET ONE TIME TO LAST TWO DIGITS OF ID       00080049
//       SET   PROG=5    ** CHANGE TO MATCH EACH PROGRAM NUMBER         00090099
//****************************************************************      00100037
//ASM    EXEC   PROC=HLASMCLG                                           00110033
//SYSIN  DD     *                                                       00120042
         TITLE  'SKELETON ASSEMBLER PROGRAM'                            00130000
         PRINT  ON,NODATA,NOGEN                                         00140000
******************************************************************      00150000
*                                                                *      00160000
*   PROGRAMMER:  ADAM DAVIES                                     *      00170050
*   COMMENTS  :  ASM PROGRAM 5                                   *      00180099
*                                                                *      00190000
******************************************************************      00200000
*   STANDARD BEGINNING FOR ASSEMBLER PROGRAMS                    *      00210006
******************************************************************      00220000
PROG5    CSECT                                                          00230099
         STM   R14,R12,12(R13)         STORE EXISTING REGISTERS         00240024
         LR    R12,R15                 ESTABLISH 1ST BASE REG           00250024
         USING PROG5,R12,R11,R10       DEFINING THREE BASE REGS         00260099
         LAY   R11,PROG5+4096          SECOND BASE REG + 4K             00270099
         LAY   R10,PROG5+8192          THIRD BASE REG + 8K              00280099
         ST    R13,SAVEAREA+4          BACKWARD CHAIN CALLER'S          00290000
         LA    R13,SAVEAREA            ADDRESS OF MY SAVE AREA          00300000
******************************************************************      00310003
* BEGIN THE PROGRAM LOGIC. FIRST OPEN THE INPUT AND OUTPUT FILES *      00320006
******************************************************************      00330003
         OPEN  (FILEIN1,(INPUT))       OPEN INPUT FILE                  00340006
         OPEN  (FILEOUT1,(OUTPUT))     OPEN OUTPUT FILE                 00350006
         PUT   FILEOUT1,PRHEAD         OUTPUT THE HEADER                00360006
         PUT   FILEOUT1,HEAD2                                           00370099
         GET   FILEIN1,RECIN1          GET THE FIRST RECORD, IF ONE     00380006
******************************************************************      00390006
*        READ AND PRINT MAIN PROGRAM LOOP                        *      00400041
******************************************************************      00410006
LOOP     EQU   *                                                        00420000
         BAS   R8,DATAVAL              THIS WILL TEST DATA IN           00430099
         BAS   R8,SETUP                USE SUBROUTINE FOR LINE SETUP    00440099
         CLC   FLAGYP,FALSEY           TESTING IF DATA IS GOOD          00450099
         BE    FLAGYF                                                   00460099
         BAS   R8,COSTFUN              THIS WILL FIND COST&SUM #        00470099
         BAS   R8,AVGFUN               THIS IS TO FIND THE AVG          00480099
         PUT   FILEOUT1,RECOUT1        OUTPUT THE DETAIL RECORD!        00490099
FLAGYF   EQU   *                       BAD DATA INPUT                   00500099
         GET   FILEIN1,RECIN1          GET THE NEXT RECORD              00510098
         B     LOOP                    GO BACK AND PROCESS              00520070
******************************************************************      00530006
*        END OF INPUT PROCESSING                                 *      00540006
******************************************************************      00550006
EOF1     EQU   *                                                        00560006
         BAS   R9,LASTPR                                                00570099
         CLOSE (FILEIN1)               CLOSE INPUT FILE                 00580006
         CLOSE (FILEOUT1)              CLOSE OUTPUT FILE                00590006
         L     R13,SAVEAREA+4          POINT AT OLD SAVE AREA           00600000
         LM    R14,R12,12(R13)         RESTORE THE REGISTERS            00610000
         LA    R15,0                   RETURN CODE = 0                  00620000
         BR    R14                     RETURN TO OPERATING SYSTEM       00630000
******************************************************************      00640045
*        ADDED (SUB)ROUTINES FOLLOW THE SETUP ROUTINE!           *      00650045
******************************************************************      00660045
SETUP    EQU   *                       NAME OF FIRST SUBROUTINE!        00670045
*        MVC   TITLE,TITLE1            MOVES THE TITLE IN               00680099
         MVC   TITLE1,TITLE            FOR TESTING WHAT DATA IS AND     00690099
         MVC   PRICE1,PRICE              FORMATING HEADER               00700099
         MVC   UNITS1,UNITS                                             00710099
         BR    R8                      RETURN TO INST AFTER BAS!        00720099
******************************************************************      00730056
*       DATAVAL VALADATION OF THE DATA IN KEEP THIS!!!!! @A      *      00740099
******************************************************************      00750056
DATAVAL  EQU   *                       DATA VALADATION SUBROUTINE!      00760099
         MVC   FLAGYP,TRUEY            SETS FLAG TO TRUE                00770099
         PACK  PKPRICE,PRICE           PACK IN THE DATA                 00780099
         TP    PKPRICE                 TESTED PRICE                     00790099
         BNZ   FAILED                  FAILED TEST                      00800099
         PACK  PKUNITS,UNITS                                            00810099
         TP    PKUNITS                 TESTED UNITS                     00820099
         BNZ   FAILED                  FAILED TEST                      00830099
         AP    PKCOUNT,=PL1'1'         ADDS ONE TO PKCOUNT IF GOOD      00840099
         BR    R8                                                       00850099
FAILED   EQU   *                                                        00860099
         MVC   FLAGYP,FALSEY           FAILED TO PACK OR B>A            00870099
         BR    R8                      RETURN TO INST AFTER BAS!        00880099
******************************************************************      00890083
*     COSTFUN (SUB)ROUTINES  THIS IS TO SUM # AND COST #         *      00900099
******************************************************************      00910083
COSTFUN  EQU   *                       NAME OF 2ND SUBROUTINE!          00920099
         ZAP   PKCOSTS,PKPRICE                                          00930099
         MP    PKCOSTS,PKUNITS                                          00940099
         AP    PKSUMU,PKUNITS          TIME A NEW RECORD COMES IN       00950099
         SRP   PKPRICE,4,0                                              00960099
         SRP   PKUNITS,2,0                                              00970099
         ZAP   PKCOSTSW,PKCOSTS                                         00980099
         AP    PKSUMC,PKCOSTSW         THIS ADDS TO THE SUME EACH       00990099
         MVC   UNITS1,EDMASKU          MOVE EDITMASK IN FOR OUTPUT      01000099
         MVC   PRICE1,EDMASKP                                           01010099
         MVC   COST1,EDMASKC                                            01020099
         ED    UNITS1,PKUNITS                                           01030099
         ED    PRICE1,PKPRICE                                           01040099
         ED    COST1,PKCOSTSW                                           01050099
         BR    R8                                                       01060085
******************************************************************      01070099
*       AVGFUN(SUB)ROUTINES  THIS IS TO GET AVG VALUE            *      01080099
******************************************************************      01090099
AVGFUN   EQU   *                       NAME OF 3RD SUBROUTINE!          01100099
         ZAP   PKAVGU,PKSUMU                                            01110099
         ZAP   PKAVGC,PKSUMC                                            01120099
         DP    PKAVGC,PKCOUNT          CALCULATES AVG                   01130099
         DP    PKAVGU,PKCOUNT                                           01140099
         ZAP   PKSUMCP,PKSUMC                                           01150099
         BR    R8                                                       01160099
******************************************************************      01170099
*       PRINT AVG AND SUM                                        *      01180099
******************************************************************      01190099
LASTPR   EQU   *                       NAME OF 3RD SUBROUTINE!          01200099
         SRP   PKSUMU,2,0                                               01210099
         SRP   PKSUMC,4,0                                               01220099
         MVC   TITLE2,=cl6'Totals'                                      01230099
         MVC   UNITS2,EDMASKU           MOVE EDITMASK IN FOR OUTPUT     01240099
         ED    UNITS2,PKSUMU                                            01250099
         MVC   COST2,EDMASKCD                                           01260099
         ED    COST2,PKSUMC                                             01270099
         PUT   FILEOUT1,RECOUT2        OUTPUT THE DETAIL RECORD!        01280099
         SRP   PKAVGUP,2,0                                              01290099
*        SRP   PKAVGCP,4,0                                              01300099
         MVC   TITLE2,=cl8'Averages'                                    01310099
         MVC   UNITS2,EDMASKU           MOVE EDITMASK IN FOR OUTPUT     01320099
         ED    UNITS2,PKAVGUP                                           01330099
         MVC   COST2,EDMASKC                                            01340099
         ED    COST2,PKAVGCP                                            01350099
         PUT   FILEOUT1,RECOUT2        OUTPUT THE DETAIL RECORD!        01360099
         BR    R9                                                       01370099
******************************************************************      01380061
*    WORK AREA DATA DEFINITIONS                                  *      01390061
******************************************************************      01400061
PKPRICE  DC    PL05'0'            All data must be packed!              01410099
PKUNITS  DC    PL05'0'                                                  01420099
PKCOSTS  DC    PL11'0'                                                  01430099
PKCOSTSW DC    PL06'0'                                                  01440099
PKSUMU   DC    PL05'0'                                                  01450099
PKSUMC   DC    PL08'0'                                                  01460099
PKSUMCP  DC    PL05'0'                                                  01470099
PKCOUNT  DC    PL03'0'            FIND OUT HOW MANY RECORDS             01480099
PKAVGU   DC   0PL08'0'                                                  01490099
PKAVGUP  DS    PL05                                                     01500099
         DS    PL03                                                     01510099
PKAVGC   DC   0PL09'0'                                                  01520099
PKAVGCP  DS    PL06                                                     01530099
         DS    PL03                                                     01540099
FLAGYP   DC    CL1'F'             DATA IS GOOD                          01550089
TRUEY    DC    CL1'T'             EH MOVING FLAGS                       01560089
FALSEY   DC    CL1'F'                        fix the editmask           01570099
*                   1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4     01580099
EDMASKC  DC    XL16'402020206B2020206B2021204B202060'                   01590099
EDMASKCD DC    XL16'402020206B2020206B2021204B202040'                   01600099
EDMASKU  DC    XL13'40202020206B2021204B202060'                         01610099
EDMASKP  DC    XL07'402021204B2020'                                     01620099
******************************************************************      01630000
*     INPUT FILE - DATA CONTROL BLOCK                            *      01640041
******************************************************************      01650000
FILEIN1  DCB   DSORG=PS,                                               X01660006
               MACRF=(GM),                                             X01670000
               DEVD=DA,                                                X01680000
               DDNAME=FILEIN1,                                         X01690006
               EODAD=EOF1,                                             X01700006
               RECFM=FB,                                               X01710000
               LRECL=80                                                 01720000
******************************************************************      01730000
*    INPUT RECORD AREA                                           *      01740041
******************************************************************      01750000
RECIN1   DS   0CL80           FULL RECORD DEFINITION (80 BYTES)         01760041
         DS    CL10                                                     01770099
TITLE    DS    CL20           PULL IN THE NAME                          01780099
*        DS    CL00           FILLER TO PUT B IN CORRECT POSITION!      01790099
PRICE    DS    CL05           THIS IS THE INOUT FOR DATA B 2BYTES       01800099
         DS    CL05                                                     01810099
UNITS    DS    CL05                                                     01820099
         DS    CL35                                                     01830099
******************************************************************      01840006
*     OUTPUT FILE - DATA CONTROL BLOCK                           *      01850006
******************************************************************      01860006
FILEOUT1 DCB   DSORG=PS,                                               X01870006
               MACRF=(PM),                                             X01880006
               DEVD=DA,                                                X01890006
               DDNAME=FILEOUT1,                                        X01900006
               RECFM=FM,                                               X01910006
               LRECL=80                                                 01920006
******************************************************************      01930006
*    OUTPUT RECORD AREAS                                         *      01940041
******************************************************************      01950000
*      HERE IS THE HEADER FOR ** C S U **                               01960006
******************************************************************      01970006
PRHEAD   DS   0CL80                                                     01980006
PRC1     DC    CL1' '            PRINT CONTROL - SINGLE SPACE           01990006
         DC    CL05' '           SPACING OF 05 CHARACTERS               02000041
         DC    CL74'Sales Report Summary  - 15 Adam Davies'             02010099
HEAD2    DC   0CL80                                                     02020099
PRC4     DC    CL1' '                                                   02030099
         DC    CL20'Name'                                               02040099
         DC    CL09' '                                                  02050099
         DC    CL10'Quantity'                                           02060099
         DC    CL03' '                                                  02070099
         DC    CL08'Price'                                              02080099
         DC    CL11' '                                                  02090099
         DC    CL18'Cost'                                               02100099
******************************************************************      02110006
* HERE IS THE DETAIL OUTPUT LINE                                 *      02120041
******************************************************************      02130006
RECOUT1  DS   0CL80              PRINT AREA                             02140006
PRC2     DC    CL1' '            PRINT CONTROL CHARACTER                02150006
TITLE1   DC    CL20' '                                                  02160099
         DC    CL05' '                                                  02170081
UNITS1   DC    CL13' '                                                  02180099
         DC    CL02' '                                                  02190099
PRICE1   DC    CL07' '                                                  02200099
         DC    CL03' '                                                  02210099
COST1    DC    CL16' '                                                  02220099
         DC    CL12' '           SPACING TO MAKE 80 CHAR RECORD         02230099
******************************************************************      02240099
* HERE IS THE DETAIL OUTPUT LINE 4 SUM & AVG **WHY SEPARATE??**  *      02250099
******************************************************************      02260099
RECOUT2  DS   0CL80              PRINT AREA                             02270099
PRC3     DC    CL1' '            PRINT CONTROL CHARACTER                02280099
         DC    CL2' '                                                   02290099
TITLE2   DC    CL08' '                                                  02300099
         DC    CL15' '                                                  02310099
UNITS2   DC    CL13' '                                                  02320099
         DC    CL12' '                                                  02330099
COST2    DC    CL16' '                                                  02340099
         DC    CL13' '           SPACING TO MAKE 80 CHAR RECORD         02350099
******************************************************************      02360041
*    REGISTER SAVE AREA                                          *      02370041
******************************************************************      02380041
SAVEAREA DS  18F                 ROOM FOR STORAGE OF REGISTERS          02390041
******************************************************************      02400009
*     REGISTER EQUATES                                           *      02410009
******************************************************************      02420009
R0       EQU   0                                                        02430009
R1       EQU   1                                                        02440009
R2       EQU   2                                                        02450009
R3       EQU   3                                                        02460009
R4       EQU   4                                                        02470009
R5       EQU   5                                                        02480009
R6       EQU   6                                                        02490009
R7       EQU   7                                                        02500009
R8       EQU   8                                                        02510009
R9       EQU   9                                                        02520009
R10      EQU   10                                                       02530009
R11      EQU   11                                                       02540009
R12      EQU   12                                                       02550009
R13      EQU   13                                                       02560009
R14      EQU   14                                                       02570009
R15      EQU   15                                                       02580009
******************************************************************      02590000
*    LITERAL POOL - THIS PROGRAM MAY USE LITERALS.                      02600029
******************************************************************      02610000
         LTORG *                                                        02620014
         END   PROG5                                                    02630099
/*                                                                      02640000
//G.SYSABOUT DD SYSOUT=*                                                02650000
//G.SYSUDUMP DD SYSOUT=*                                                02660000
//G.FILEOUT1 DD SYSOUT=*,OUTLIM=2500                                    02670006
//*.FILEIN1  DD DSN=CSU0&CICS&ID..C3121.ASM(DATAPRG&PROG),DISP=SHR      02680040
//G.FILEIN1  DD DSN=CSU.PUBLIC.DATA(DATAPRG&PROG),DISP=SHR              02690039
//                                                                      02700000
