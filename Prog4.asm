//CSU0115A JOB (ASM),'C3121',CLASS=A,MSGCLASS=A,NOTIFY=&SYSUID,         00010049
//   MSGLEVEL=(0,0),TIME=(,2),COND=(4,LT)                               00020028
//****************************************************************      00030037
//*   NEXT THREE PARAMETERS MUST BE SET BEFORE COMPILES!         *      00040041
//****************************************************************      00050037
//*      SET   CICS=0    ** NO, NOT IN CICS CLASS **                    00060049
//       SET   CICS=1    ** YES, ALSO IN CICS CLASS **                  00070049
//       SET   ID=15     ** SET ONE TIME TO LAST TWO DIGITS OF ID       00080049
//       SET   PROG=4    ** CHANGE TO MATCH EACH PROGRAM NUMBER         00090099
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
PROG4    CSECT                                                          00230099
         STM   R14,R12,12(R13)         STORE EXISTING REGISTERS         00240024
         LR    R12,R15                 ESTABLISH 1ST BASE REG           00250024
         USING PROG4,R12,R11,R10       DEFINING THREE BASE REGS         00260099
         LAY   R11,PROG4+4096          SECOND BASE REG + 4K             00270099
         LAY   R10,PROG4+8192          THIRD BASE REG + 8K              00280099
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
         BAS   R8,ADDER                @A  THIS WILL TEST DATA IN       00430099
         BAS   R8,BAS10                CONVETS N TO A,B,C,D,X,OR T      00470099
         PUT   FILEOUT1,RECOUT1        OUTPUT THE DETAIL RECORD!        00500099
         GET   FILEIN1,RECIN1          GET THE NEXT RECORD              00510098
         B     LOOP                    GO BACK AND PROCESS              00520070
******************************************************************      00530006
*        END OF INPUT PROCESSING                                 *      00540006
******************************************************************      00550006
EOF1     EQU   *                                                        00560006
         CLOSE (FILEIN1)               CLOSE INPUT FILE                 00570006
         CLOSE (FILEOUT1)              CLOSE OUTPUT FILE                00580006
         L     R13,SAVEAREA+4          POINT AT OLD SAVE AREA           00590000
         LM    R14,R12,12(R13)         RESTORE THE REGISTERS            00600000
         LA    R15,0                   RETURN CODE = 0                  00610000
         BR    R14                     RETURN TO OPERATING SYSTEM       00620000
******************************************************************      00630045
*        ADDED (SUB)ROUTINES FOLLOW THE SETUP ROUTINE!           *      00640045
******************************************************************      00650045
SETUP    EQU   *                       NAME OF FIRST SUBROUTINE!        00660045
         MVC   VALUE,MAINS             MOVE DATA INTO PRINT LINE        00661099
         MVC   CHECK,=CL5'FAILD'                                        00662099
         PACK  PK1,VAL1                PACKING DATA IN                  00670099
         PACK  PK2,VAL2                                                 00680099
         PACK  PK3,VAL3                                                 00681099
         PACK  PK4,VAL4                PACKING DATA IN                  00682099
         PACK  PK5,VAL5                                                 00683099
         PACK  PK6,VAL6                PACKING DATA IN                  00684099
         PACK  PK7,VAL7                                                 00685099
         PACK  PK8,CHECKV                                               00686099
         BR    R8                      RETURN TO INST AFTER BAS!        00690048
******************************************************************      00870083
*      ADDER (SUB)ROUTINES  THIS ODD*3 SUM EVEN SUM ODD+EVEN     *      00880099
******************************************************************      00890083
ADDER    EQU   *                       NAME OF 1ST SUBROUTINE!          00900099
         AP    PKODD,PK1                                                00910099
         AP    PKODD,PK3                                                00920099
         AP    PKODD,PK5                                                00930099
         AP    PKODD,PK7               SUMS THE ODDS                    00940099
         MP    PKODD,=P'3'             TIMES 3 HERE! @TODO              00950099
         AP    PKSUM,PKODD             MOVES ODD INTO TOTAL             00951099
         AP    PKEVEN,PK2                                               00960099
         AP    PKEVEN,PK4                                               00961099
         AP    PKEVEN,PK6                                               00962099
         AP    PKSUM,PKEVEN            MOVES EVEN INYO TOTAL            00970099
         BR    R8                                                       00980085
******************************************************************      00990099
*      ADDER (SUB)ROUTINES  THIS ODD*3 SUM EVEN SUM ODD+EVEN     *      01000099
******************************************************************      01010099
BAS10    EQU   *                       NAME OF 3ST SUBROUTINE!          01020099
         AP    PKTEN,=PL2'10'          WILL ADD 10 UNTILL GRATER        01030099
         CLC   PKTEN,PKSUM             THEN THE SUM VALUE THEN WIL      01040099
         BNH   BAS10                   L RETURN BACK TO R8              01050099
         AP    PKSUM,PK8                                                01060099
         CLC   PKTEN,PKSUM                                              01070099
         BNE   FAILED                                                   01080099
         MVC   CHECK,=CL5'PASS '                                        01090099
FAILED   EQU   *                                                        01100099
         BR    R8                                                       01130099
******************************************************************      01280061
*    WORK AREA DATA DEFINITIONS                                  *      01290061
******************************************************************      01300061
PK1      DS    PL01               MOVE THE IMPUT INTO PACKED            01310099
PK2      DS    PL01                                                     01320099
PK3      DS    PL01                                                     01330099
PK4      DS    PL01                                                     01340099
PK5      DS    PL01                                                     01350099
PK6      DS    PL01                                                     01360099
PK7      DS    PL01                                                     01370099
PK8      DS    PL01               CHECK VALUE!                          01370199
PKODD    DS    PL05               SUM OF ODD VALUES TIMES BY 3          01371099
PKEVEN   DS    PL02               SUM OF THE EVEN VALUES                01372099
PKSUM    DS    PL10               ADDED TOGETHER                        01373099
PKTEN    DS    PL10               CHECK BY BASE 10                      01374099
FLAGYP   DC    CL1'F'             DATA IS GOOD(OLD REMOVE ME IF @O)     01380099
TRUEY    DC    CL1'T'             EH MOVING FLAGS  (UNEEDED)    @O      01390099
FALSEY   DC    CL1'F'                                           @O      01400099
******************************************************************      01420000
*     INPUT FILE - DATA CONTROL BLOCK                            *      01430041
******************************************************************      01440000
FILEIN1  DCB   DSORG=PS,                                               X01450006
               MACRF=(GM),                                             X01460000
               DEVD=DA,                                                X01470000
               DDNAME=FILEIN1,                                         X01480006
               EODAD=EOF1,                                             X01490006
               RECFM=FB,                                               X01500000
               LRECL=80                                                 01510000
******************************************************************      01520000
*    INPUT RECORD AREA                                           *      01530041
******************************************************************      01540000
RECIN1   DS   0CL80           FULL RECORD DEFINITION (80 BYTES)         01550041
MAINS    DS   0CL08           MAKE IT EASY TO MOVE TO PRINT LINE        01551099
VAL1     DS    CL01           WE TAKE THE INPUT AND BREAK IT UP         01560099
VAL2     DS    CL01           INTO 8 PARTS WITH THE LAST NUMBER         01570099
VAL3     DS    CL01           BEING A CHECK VALUE                       01580099
VAL4     DS    CL01                                                     01581099
VAL5     DS    CL01                                                     01582099
VAL6     DS    CL01                                                     01583099
VAL7     DS    CL01                                                     01584099
CHECKV   DS    CL01           CHECK VALUE                               01585099
         DS    CL72                                                     01590099
******************************************************************      01600006
*     OUTPUT FILE - DATA CONTROL BLOCK                           *      01610006
******************************************************************      01620006
FILEOUT1 DCB   DSORG=PS,                                               X01630006
               MACRF=(PM),                                             X01640006
               DEVD=DA,                                                X01650006
               DDNAME=FILEOUT1,                                        X01660006
               RECFM=FM,                                               X01670006
               LRECL=80                                                 01680006
******************************************************************      01690006
*    OUTPUT RECORD AREAS                                         *      01700041
******************************************************************      01710000
*      HERE IS THE HEADER FOR ** C S U **                               01720006
******************************************************************      01730006
PRHEAD   DS   0CL80                                                     01740006
PRC1     DC    CL1' '            PRINT CONTROL - SINGLE SPACE           01750006
         DC    CL05' '           SPACING OF 05 CHARACTERS               01760041
         DC    CL74'***CSU SPRING 2019***'                              01770047
******************************************************************      01780006
* HERE IS THE DETAIL OUTPUT LINE                                 *      01790041
******************************************************************      01800006
RECOUT1  DS   0CL80              PRINT AREA                             01810006
PRC2     DC    CL1' '            PRINT CONTROL CHARACTER                01820006
VALUE    DC    CL08' '                                                  01830099
         DC    CL06' '                                                  01840099
CHECK    DC    CL05' '                                                  01850099
         DC    CL60' '           SPACING TO MAKE 80 CHAR RECORD         01900099
******************************************************************      01910041
*    REGISTER SAVE AREA                                          *      01920041
******************************************************************      01930041
SAVEAREA DS  18F                 ROOM FOR STORAGE OF REGISTERS          01940041
******************************************************************      01950009
*     REGISTER EQUATES                                           *      01960009
******************************************************************      01970009
R0       EQU   0                                                        01980009
R1       EQU   1                                                        01990009
R2       EQU   2                                                        02000009
R3       EQU   3                                                        02010009
R4       EQU   4                                                        02020009
R5       EQU   5                                                        02030009
R6       EQU   6                                                        02040009
R7       EQU   7                                                        02050009
R8       EQU   8                                                        02060009
R9       EQU   9                                                        02070009
R10      EQU   10                                                       02080009
R11      EQU   11                                                       02090009
R12      EQU   12                                                       02100009
R13      EQU   13                                                       02110009
R14      EQU   14                                                       02120009
R15      EQU   15                                                       02130009
******************************************************************      02140000
*    LITERAL POOL - THIS PROGRAM MAY USE LITERALS.                      02150029
******************************************************************      02160000
         LTORG *                                                        02170014
         END   PROG4                                                    02180099
/*                                                                      02190000
//G.SYSABOUT DD SYSOUT=*                                                02200000
//G.SYSUDUMP DD SYSOUT=*                                                02210000
//G.FILEOUT1 DD SYSOUT=*,OUTLIM=2500                                    02220006
//*.FILEIN1  DD DSN=CSU0&CICS&ID..C3121.ASM(DATAPRG&PROG),DISP=SHR      02230040
//G.FILEIN1  DD DSN=CSU.PUBLIC.DATA(DATAPRG&PROG),DISP=SHR              02240039
//                                                                      02250000
