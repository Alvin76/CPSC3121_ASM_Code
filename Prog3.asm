//CSU0115A JOB (ASM),'C3121',CLASS=A,MSGCLASS=A,NOTIFY=&SYSUID,         00010049
//   MSGLEVEL=(0,0),TIME=(,2),COND=(4,LT)                               00020028
//****************************************************************      00030037
//*   NEXT THREE PARAMETERS MUST BE SET BEFORE COMPILES!         *      00040041
//****************************************************************      00050037
//*      SET   CICS=0    ** NO, NOT IN CICS CLASS **                    00060049
//       SET   CICS=1    ** YES, ALSO IN CICS CLASS **                  00070049
//       SET   ID=15     ** SET ONE TIME TO LAST TWO DIGITS OF ID       00080049
//       SET   PROG=3    ** CHANGE TO MATCH EACH PROGRAM NUMBER         00090080
//****************************************************************      00100037
//ASM    EXEC   PROC=HLASMCLG                                           00110033
//SYSIN  DD     *                                                       00120042
         TITLE  'SKELETON ASSEMBLER PROGRAM'                            00130000
         PRINT  ON,NODATA,NOGEN                                         00140000
******************************************************************      00150000
*                                                                *      00160000
*   PROGRAMMER:  ADAM DAVIES                                     *      00170050
*   COMMENTS  :  ASM PROGRAM 3                                   *      00180082
*                                                                *      00190000
******************************************************************      00200000
*   STANDARD BEGINNING FOR ASSEMBLER PROGRAMS                    *      00210006
******************************************************************      00220000
PROG3    CSECT                                                          00230080
         STM   R14,R12,12(R13)         STORE EXISTING REGISTERS         00240024
         LR    R12,R15                 ESTABLISH 1ST BASE REG           00250024
         USING PROG3,R12,R11,R10       DEFINING THREE BASE REGS         00260080
         LAY   R11,PROG3+4096          SECOND BASE REG + 4K             00270080
         LAY   R10,PROG3+8192          THIRD BASE REG + 8K              00280080
         ST    R13,SAVEAREA+4          BACKWARD CHAIN CALLER'S          00290000
         LA    R13,SAVEAREA            ADDRESS OF MY SAVE AREA          00300000
******************************************************************      00310003
* BEGIN THE PROGRAM LOGIC. FIRST OPEN THE INPUT AND OUTPUT FILES *      00320006
******************************************************************      00330003
         OPEN  (FILEIN1,(INPUT))       OPEN INPUT FILE                  00340006
         OPEN  (FILEOUT1,(OUTPUT))     OPEN OUTPUT FILE                 00350006
         PUT   FILEOUT1,PRHEAD         OUTPUT THE HEADER                00360006
         GET   FILEIN1,RECIN1          GET THE FIRST RECORD, IF ONE     00370006
******************************************************************      00380006
*        READ AND PRINT MAIN PROGRAM LOOP                        *      00390041
******************************************************************      00400006
LOOP     EQU   *                                                        00410000
         BAS   R8,SETUP                USE SUBROUTINE FOR LINE SETUP    00420045
         BAS   R8,DATAVAL              @A  THIS WILL TEST DATA IN       00430083
         CLC   FLAGYP,FALSEY           TESTING IF DATA IS GOOD          00440088
         BE    FLAGYT                                                   00450099
         BAS   R8,SNVAL                WILL GET THE VALUES FOR S&N      00460091
         BAS   R8,NCON                 CONVETS N TO A,B,C,D,X,OR T      00470091
         MVC   NUMBS,EDITMASK          MOVE IN MASK                     00480099
         ED    NUMBS,PKS               MAKES THE PKS LOOK NICE          00490099
FLAGYT   EQU   *                       IF DATA IS GOOD                  00500099
         PUT   FILEOUT1,RECOUT1        OUTPUT THE DETAIL RECORD!        00510099
         GET   FILEIN1,RECIN1          GET THE NEXT RECORD              00520098
         B     LOOP                    GO BACK AND PROCESS              00530070
******************************************************************      00540006
*        END OF INPUT PROCESSING                                 *      00550006
******************************************************************      00560006
EOF1     EQU   *                                                        00570006
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
         MVC   NUMBA,DATAINA        ** REVERSED ORDER                   00680099
         MVC   NUMBB,DATAINB             THANKS                         00690099
         MVC   CSIZE,=C' '                                              00700099
         MVC   NUMBS,=CL5' '                                            00710099
         BR    R8                      RETURN TO INST AFTER BAS!        00720048
******************************************************************      00730056
*       DATAVAL VALADATION OF THE DATA IN                        *      00740086
******************************************************************      00750056
DATAVAL  EQU   *                       NAME OF FIRST SUBROUTINE!        00760083
         MVC   FLAGYP,TRUEY                                             00770088
         PACK  PKA,DATAINA             PACK IN THE DATA                 00780086
         TP    PKA                     TESTED PACK A                    00790086
         BNZ   FAILED                  FAILED TEST                      00800086
         PACK  PKB,DATAINB                                              00810099
         TP    PKB                     TESTED PACK B                    00820086
         BNZ   FAILED                  FAILED TEST                      00830086
         CLC   PKA,PKB                                                  00840086
         BNH   FAILED                                                   00850086
         BR    R8                                                       00860099
FAILED   EQU   *                                                        00870099
         MVC   FLAGYP,FALSEY           FAILED TO PACK OR B>A            00880088
         BR    R8                      RETURN TO INST AFTER BAS!        00890056
******************************************************************      00900083
*       SNVAL (SUB)ROUTINES  THIS IS TO GET N VALUE              *      00910085
******************************************************************      00920083
SNVAL    EQU   *                       NAME OF 2ND SUBROUTINE!          00930083
         ZAP   PKS,PKB                                                  00940085
         AP    PKS,=PL1'4'                                              00950085
         ZAP   PKTEMP,PKS                                               00960085
         DP    PKTEMP,=PL1'2'          ADD IN THE REMANDER PART         00970091
         AP    PKS,PKR                 ADDS THE REMANADER PART          00980091
         ZAP   PKN,PKA                                                  00990099
         SP    PKN,PKS                                                  01000099
         BR    R8                                                       01010085
******************************************************************      01020083
*       THIS COVERTES THE N TO T,A,B,C,D,X SIZES                 *      01030085
******************************************************************      01040083
NCON     EQU   *                       NAME OF 3RD SUBROUTINE!          01050091
         CP    PKN,=PL1'1'                                              01060092
         BNL   JUMPA                   THIS JUMPS TO A                  01070099
         MVC   CSIZE,=CL1'T'                                            01080093
JUMPA    EQU   *                                                        01090085
         CP    PKN,=PL1'1'                                              01100092
         BNE   JUMPB                   THIS JUMPS TO B                  01110085
         MVC   CSIZE,=CL1'A'                                            01120093
JUMPB    EQU   *                                                        01130085
         CP    PKN,=PL1'2'                                              01140092
         BNE   JUMPC                   THIS JUMPS TO C                  01150085
         MVC   CSIZE,=CL1'B'                                            01160093
JUMPC    EQU   *                                                        01170085
         CP    PKN,=PL1'3'                                              01180092
         BNE   JUMPD                   THIS JUMPS TO D                  01190085
         MVC   CSIZE,=CL1'C'                                            01200093
JUMPD    EQU   *                                                        01210085
         CP    PKN,=PL1'4'                                              01220092
         BNE   JUMPX                   THIS JUMPS TO X                  01230085
         MVC   CSIZE,=CL1'D'                                            01240093
JUMPX    EQU   *                                                        01250085
         CP    PKN,=PL1'4'                                              01260092
         BNH   JUMPO                   THIS JUMPS OUT  @B >4 HERE       01270099
         MVC   CSIZE,=CL1'X'                                            01280093
JUMPO    EQU   *                                                        01290085
         BR    R8                                                       01300085
******************************************************************      01310061
*    WORK AREA DATA DEFINITIONS                                  *      01320061
******************************************************************      01330061
PKA      DC    PL2'0'                                                   01340099
PKB      DC    PL2'0'             THIS IS THE B FELIED                  01350099
PKS      DC    PL2'0'                                                   01360099
PKN      DC    PL2'0'             MOVES INPUT TO PACKED                 01370099
PKTEMP   DC   0PL3'0'                                                   01380099
PKJ      DS    PL02                                                     01390099
PKR      DS    PL01               THIS IS THE REMANDER                  01400099
FLAGYP   DC    CL1'F'             DATA IS GOOD                          01410089
TRUEY    DC    CL1'T'             EH MOVING FLAGS                       01420089
FALSEY   DC    CL1'F'                                                   01430089
EDITMASK DC    XL5'4021202060'                                          01440099
******************************************************************      01450000
*     INPUT FILE - DATA CONTROL BLOCK                            *      01460041
******************************************************************      01470000
FILEIN1  DCB   DSORG=PS,                                               X01480006
               MACRF=(GM),                                             X01490000
               DEVD=DA,                                                X01500000
               DDNAME=FILEIN1,                                         X01510006
               EODAD=EOF1,                                             X01520006
               RECFM=FB,                                               X01530000
               LRECL=80                                                 01540000
******************************************************************      01550000
*    INPUT RECORD AREA                                           *      01560041
******************************************************************      01570000
RECIN1   DS   0CL80           FULL RECORD DEFINITION (80 BYTES)         01580041
DATAINA  DS    CL02           THIS IS THE INPUT FOR DATA A 2BYETS       01590083
         DS    CL03           FILLER TO PUT B IN CORRECT POSITION!      01600099
DATAINB  DS    CL02           THIS IS THE INOUT FOR DATA B 2BYTES       01610083
         DS    CL73                                                     01620099
******************************************************************      01630006
*     OUTPUT FILE - DATA CONTROL BLOCK                           *      01640006
******************************************************************      01650006
FILEOUT1 DCB   DSORG=PS,                                               X01660006
               MACRF=(PM),                                             X01670006
               DEVD=DA,                                                X01680006
               DDNAME=FILEOUT1,                                        X01690006
               RECFM=FM,                                               X01700006
               LRECL=80                                                 01710006
******************************************************************      01720006
*    OUTPUT RECORD AREAS                                         *      01730041
******************************************************************      01740000
*      HERE IS THE HEADER FOR ** C S U **                               01750006
******************************************************************      01760006
PRHEAD   DS   0CL80                                                     01770006
PRC1     DC    CL1' '            PRINT CONTROL - SINGLE SPACE           01780006
         DC    CL05' '           SPACING OF 05 CHARACTERS               01790041
         DC    CL74'***CSU SPRING 2019***'                              01800047
******************************************************************      01810006
* HERE IS THE DETAIL OUTPUT LINE                                 *      01820041
******************************************************************      01830006
RECOUT1  DS   0CL80              PRINT AREA                             01840006
PRC2     DC    CL01' '           PRINT CONTROL CHARACTER                01850099
NUMBA    DC    CL02' '                                                  01860083
         DC    CL03' '                                                  01870099
NUMBB    DC    CL02' '                                                  01880083
         DC    CL03' '                                                  01890099
CSIZE    DC    CL01' '                                                  01900081
         DC    CL02' '                                                  01910099
NUMBS    DC    CL05' '                                                  01920099
         DC    CL61' '           SPACING TO MAKE 80 CHAR RECORD         01930099
******************************************************************      01940041
*    REGISTER SAVE AREA                                          *      01950041
******************************************************************      01960041
SAVEAREA DS  18F                 ROOM FOR STORAGE OF REGISTERS          01970041
******************************************************************      01980009
*     REGISTER EQUATES                                           *      01990009
******************************************************************      02000009
R0       EQU   0                                                        02010009
R1       EQU   1                                                        02020009
R2       EQU   2                                                        02030009
R3       EQU   3                                                        02040009
R4       EQU   4                                                        02050009
R5       EQU   5                                                        02060009
R6       EQU   6                                                        02070009
R7       EQU   7                                                        02080009
R8       EQU   8                                                        02090009
R9       EQU   9                                                        02100009
R10      EQU   10                                                       02110009
R11      EQU   11                                                       02120009
R12      EQU   12                                                       02130009
R13      EQU   13                                                       02140009
R14      EQU   14                                                       02150009
R15      EQU   15                                                       02160009
******************************************************************      02170000
*    LITERAL POOL - THIS PROGRAM MAY USE LITERALS.                      02180029
******************************************************************      02190000
         LTORG *                                                        02200014
         END   PROG3                                                    02210080
/*                                                                      02220000
//G.SYSABOUT DD SYSOUT=*                                                02230000
//G.SYSUDUMP DD SYSOUT=*                                                02240000
//G.FILEOUT1 DD SYSOUT=*,OUTLIM=2500                                    02250006
//*.FILEIN1  DD DSN=CSU0&CICS&ID..C3121.ASM(DATAPRG&PROG),DISP=SHR      02260040
//G.FILEIN1  DD DSN=CSU.PUBLIC.DATA(DATAPRG&PROG),DISP=SHR              02270039
//                                                                      02280000
