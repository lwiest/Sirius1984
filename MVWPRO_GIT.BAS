D1000 ' RELEASE 01***** Neubearbeitung durch 04**********
1010 '
1020 ' LW 31-JAN-2026: Load MSBASIC.COM with /S:560 /F:5
1050 '
1060 ' *** Initialisierung, Dimensionierung ***********************************
1070 '
1080 DEFINT A-Z                                              'Integerdefinition
1090 DIM FELD$(27),SUCH$(27),DAT$(27)                    'Arrays dimensionieren
1091 RUB$    =CHR$(8)                                                'Backspace
1100 ESC$    =CHR$(27)                                'Einleitung Escapesequenz
1160 CLS$    =ESC$+"E"                                      'Bildschirm löschen
1170 INVON$  =ESC$+"p"                                              'Invers ein
1180 INVOFF$ =ESC$+"q"                                              'Invers aus
1190 CURSON$ =ESC$+"y5"                                             'Cursor ein
1200 CURSOFF$=ESC$+"x5"                                             'Cursor aus
1210 HIGHON$ =ESC$+"("                                           'Helle Schrift
1220 HIGHOFF$=ESC$+")"                                          'Dunkle Schrift
1230 DELLINE$=ESC$+"K"                                           'Zeile löschen
1240 DELHIGH$=ESC$+"+"                                   'Helle Schrift löschen
1250 STRICH$ =STRING$(79,"-")                                           'Strich
1251 DEF FNCUR$(Z,S)=ESC$+"Y"+CHR$(31+Z)+CHR$(31+S)       'Cursor positionieren
1252 DEF FNROW(F$)=VAL(MID$(F$,17,2))                     'Zeile des Feldnamens
1253 DEF FNCOL(F$)=VAL(MID$(F$,15,2))                    'Spalte des Feldnamens
1254 DEF FNLNG(F$)=VAL(MID$(F$,19,2))                         'Länge des Feldes
1255 DEF FNTYP(F$)=VAL(RIGHT$(F$,1))                     'Gültigkeits-Test Flag
1256 HOME$=FNCUR$(3,1)                                    'Cursor Home Position
1260 ' 
1270 ON ERROR GOTO 13240
1280 '
1290 RESTORE 1330                                           'Tastatur umbelegen
1291 KEY=0
1292 WHILE KEY>=0
1300   PRINT ESC$;"41";CHR$(KEY);CHR$(0);                    'Umbelegen UNSHIFT
1310   PRINT ESC$;"42";CHR$(KEY);CHR$(0);                      'Umbelegen SHIFT
1320   READ KEY
1321 WEND
1330 DATA 11,26,27,32:    'ESC, CLR, DEL CHR, INT                  'Tastencodes
1350 DATA 33,47,48,53:                                  'TAB, INS, DEL/EOL, RVS
1370 DATA 67,68,73,94:                                  'SCROL, LTRL, UNDL, RPT
1390 DATA 97,-1:                                                 'CONT, Endflag
1400 '
1410 PRINT ESC$;"41";CHR$(88);CHR$(200);                           'Cursor rauf
1420 PRINT ESC$;"41";CHR$(89);CHR$(201);                         'Cursor runter
1430 PRINT ESC$;"41";CHR$(98);CHR$(202);                          'Cursor links
1440 PRINT ESC$;"41";CHR$(99);CHR$(203);                         'Cursor rechts
1450 '
1460 ' *** Beginn des Mitgliederverwaltungsprogramms **************************
1470 '
1480 MENUT$="MITGLIEDERVERWALTUNG":MENUID$="G-01000":GOSUB 12980   'Basis Maske
1490 Q$="Befindet sich die Daten-Diskette im Laufwerk B?":GOSUB 12520 'Ja/Nein-Frage
1491 IF JA=1 THEN 1540
1500 Q$="Bitte legen Sie sie ein. Fertig?":GOSUB 12520           'Ja/Nein-Frage
1501 IF JA=0 THEN RUN "DISKMENU.EXE"
1510 '
1520 ' *** Mitgliederverwaltungsmenü (MVW-Menü) *******************************
1530 '
1540 GOSUB 12300                                         'Fielden der STS-Datei
1541 GOSUB 13550                                        'Fielden der VN-Dateien
1542 OPEN "I",#1,"B:FELD.DAT"
1543 INPUT #1,FELDMAX
1544 FOR I=1 TO FELDMAX
1545   INPUT #1,FELD$(I)
1546 NEXT
1547 CLOSE #1
1560 MENUT$="MITGLIEDERVERWALTUNG":MENUID$="G-01000":GOSUB 12980   'Basis Maske
1561 KOM$="":GOSUB 13030                                        'F-Tastenleiste
1570 VNFLAG=0
1580 PRINT TAB(26);"1 - Stammsatzpflege"                              'Menütext
1581 PRINT
1590 PRINT TAB(26);"2 - Verwendungsnachweis"
1591 PRINT
1600 PRINT TAB(26);"3 - Mitgliedsliste drucken"
1610 FKEYMAX=3:GOSUB 13100                                         'Menüeingabe
1620 DRUCKFLAG=0                                                     '(???????)
1630 IF NKEY THEN ON NKEY GOTO 1700,4240,5010
1640 ON FKEY GOTO 1650,1660,1560,1660
1650 GOSUB 10520:GOTO 1560                                           'Help TEXT
1660 GOSUB 13160:GOTO 1560                                  'Zurück & Hauptmenü
1669 '
1670 ' *** Stammsatzpflege vorbereiten ****************************************
1680 '
1690 DEL$=RUB$+"."+RUB$                                  'Zeichen links löschen
1700 OPEN "I",#1,"B:MTGLZAHL.DAT":INPUT #1,MTGL$
1710 MTGLMAX=VAL(MTGL$):CLOSE #1
1720 IF DRUCKFLAG THEN RETURN                                 '       (???????)
1730 '
1740 ' *** Stammsatzpflegemenü (STS-Menü) *************************************
1750 '
1760 MENUT$="STAMMSATZPFLEGE":MENUID$="G-01100":GOSUB 12980        'Basis Maske
1761 KOM$="":GOSUB 13030                                        'F-Tastenleiste
1770 PRINT TAB(29);"1 - Stammsatz anlegen"                            'Menütext
1771 PRINT
1780 PRINT TAB(29);"2 - Stammsatz ändern"
1781 PRINT
1790 PRINT TAB(29);"3 - Stammsatz löschen"
1791 PRINT
1800 PRINT TAB(29);"4 - Stammsatz drucken"
1801 PRINT
1810 PRINT TAB(29);"5 - Stammsatz suchen"
1820 FKEYMAX=5:GOSUB 13100                                         'Menüeingabe
1830 IF NKEY THEN GOSUB 10200:ON NKEY GOTO 1910,2160,2630,4010,2880
1840 ON FKEY GOTO 1850,1860,1860,1870
1850 GOSUB 10720:GOTO 1760                                           'Help TEXT
1860 GOSUB 13220:GOTO 1560
1870 GOSUB 13160:GOTO 1760                                           'Hauptmenü
1880 '
1890 ' *** Stammsatz erfassen von 02************ ******************************
1900 '
1910 MENUT$="STAMMSATZ ANLEGEN":MENUID$="G-01101":PRINT CURSON$:DATENAUSG=1:F=1
1920 MTGLZAHL=VAL(MTGL$)
1930 MTGLZAHL=MTGLZAHL+1:MTGL$=STR$(MTGLZAHL)
1940 GOSUB 12980                                                   'Basis Maske
1941 KOM$="<F7>=Abspeichern":GOSUB 13030                        'F-Tastenleiste
1942 PRINT FNCUR$(3,30);"Mitgliedsnummer";FNCUR$(5,41);"Beruf:":GOSUB 10250  'Feldmaske
1950 GOSUB 12890                                                      'Defaults
1960 IF DATENAUSG=1 THEN GOSUB 10420:DATENAUSG=0             'wenn vorher HILFE
1970 PRINT FNCUR$(21,1);DELLINE$                               'Cursorparameter
1980 PRINT HIGHON$;CURSON$;FNCUR$(3,46);MTGL$
1990 Z=FNROW(FELD$(F)):S=FNCOL(FELD$(F))+14
2000 LANG=FNLNG(FELD$(F)):BEREICH=FNTYP(FELD$(F))
2010 PRINT FNCUR$(21,1);DELLINE$
2020 GOSUB 10020                                                         'INPUT
2030 IF AASC=13 THEN IF MUSS$="=" THEN IF LEN(ANT$)<1 THEN N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 12950:GOTO 2020 'Nachricht
2040 IF AASC=13 THEN DAT$(F)=ANT$                                       'Return
2050 IF AASC=13 THEN IF F<FELDMAX THEN F=F+1:GOTO 1990 ELSE N$="Letztes Feld":GOSUB 12950:GOTO 1990          'ELSE zu IF F< 'Nachricht
2060 IF HOCH<>0 THEN PRINT FNCUR$(Z,S);DAT$(F)+STRING$(LANG-LEN(DAT$(F)),"."):F=F+(HOCH=-1)*(-(F>1)+(F=1)*25)+(HOCH=1)*((F<27)-(F=27)*26):GOTO 1990
2070 IF ZURUCK=1 THEN GOSUB 10930:GOTO 1940                             'F-Keys
2080 IF ZURUCK=3 THEN GOSUB 12800:PRINT HIGHOFF$:GOSUB 13220:GOTO 1560
2090 IF ZURUCK=2 THEN GOSUB 12800:PRINT HIGHOFF$:GOTO 1760
2100 IF ZURUCK=4 THEN GOSUB 12800:PRINT HIGHOFF$:GOSUB 13160:GOTO 2020
2110 IF ZURUCK=7 THEN GOSUB 12420:GOSUB 10340:F=1:MTGLZAHL=MTGLZAHL+1:MTGL$=STR$(MTGLZAHL):GOSUB 13220:GOSUB 12890:GOSUB 10420:GOTO 1980
2120 N$="Ungültige Eingabe - Bitte wiederholen":GOSUB 12950:GOSUB 10040:GOTO 2030 'Nachricht
2130 ' 
2140 ' *** Stammsatz ändern von 03************ ********************************
2150 '
2160 MENUT$="STAMMSATZ ÄNDERN":MENUID$="G-01102":GOSUB 12980       'Basis Maske
2161 F=1:PRINT CURSON$
2170 KOM$="":GOSUB 13030                                        'F-Tastenleiste
2180 PRINT HIGHOFF$
2190 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer"
2200 PRINT FNCUR$(5,23)"des zu verändernden Stammsatzes ein,"
2210 PRINT FNCUR$(6,22)"und beenden Sie die Eingabe mit RETURN"
2220 PRINT FNCUR$(8,37)"=>..."
2230 LANG=3:BEREICH=2:Z=8:S=39
2240 GOSUB 10020                                                         'INPUT
2250 IF ZURUCK=1 THEN GOSUB 11120:GOTO 2160                             'F-Keys
2260 IF ZURUCK=2 THEN 1760
2270 IF ZURUCK=3 THEN GOSUB 13220:GOTO 1560
2280 IF ZURUCK=4 THEN GOSUB 13160
2290 IF HOCH<>0 OR ZURUCK<>0 THEN GOTO 2230                             'Errors
2300 IF LEN(ANT$)<1 THEN N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 12950:GOTO 2230 ELSE MITGLIED=VAL(ANT$) 'Nachricht
2310 IF MITGLIED=0 THEN MITGLIED=MTGLMAX+2
2320 IF MITGLIED>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12950:PRINT HIGHOFF$;FNCUR$(8,39);"...";DELHIGH$:ANT$="":GOTO 2240 'Nachricht
2330 MTGLZAHL=MITGLIED:MTGL$=STR$(MITGLIED)
2340 FOR K=4 TO 8:PRINT FNCUR$(K,1);DELLINE$:NEXT:PRINT FNCUR$(21,1);DELLINE$
2350 GOSUB 12340                                                   'Daten lesen
2360 PRINT HIGHOFF$;FNCUR$(3,30);"Mitgliedsnummer";FNCUR$(5,41);"Beruf:"
2370 GOSUB 10250
2380 GOSUB 10420
2390 PRINT FNCUR$(3,46);MTGLZAHL
2400 KOM$="<F7>=Abspeichern":GOSUB 13030                        'F-Tastenleiste
2420 S=FNCOL(FELD$(F))+14:Z=FNROW(FELD$(F)):N$="":GOSUB 12950      'Nachricht
2430 LANG=FNLNG(FELD$(F)):BEREICH=FNTYP(FELD$(F))
2440 GOSUB 10020                                                         'INPUT
2450 IF ZURUCK=1 THEN GOSUB 11220:GOTO 2360                 'Hilfe       F-Keys
2460 IF ZURUCK=2 THEN GOSUB 13490: GOTO 2160                   'Eing.-Mtgl.-Nr.
2470 IF ZURUCK=3 THEN GOSUB 13490:GOSUB 13220: GOTO 1560              'Mvw-Menü
2480 IF ZURUCK=4 THEN GOSUB 13160:PRINT HIGHOFF$;FNCUR$(24,1);"<F7>=Abspeichern";FNCUR$(1,1);HIGHON$:GOTO 2440
2490 IF ZURUCK=7 THEN 2560                                     'Daten Speichern
2500 IF HOCH=-1 THEN PRINT FNCUR$(Z,S);DAT$(F);STRING$(LANG-LEN(DAT$(F)),"."):IF F>1  THEN F=F-1:GOTO 2420  ELSE F=26:GOTO 2420 'Cursor hoch
2510 IF HOCH=1  THEN PRINT FNCUR$(Z,S);DAT$(F);STRING$(LANG-LEN(DAT$(F)),"."):IF F<27 THEN F=F+1:GOTO 2420  ELSE F= 1:GOTO 2420 'Cursor runter
2520 IF AASC=13 AND LEN(ANT$)<1 AND MUSS$="=" THEN N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 12950:GOTO 2440 'Nachricht
2530 IF AASC=13 THEN DAT$(F)=ANT$:PRINT HIGHON$;FNCUR$(Z,S);DAT$(F);STRING$(LANG-LEN(DAT$(F)),".")                    'Return
2540 IF AASC=13 THEN IF F<FELDMAX THEN F=F+1:GOTO 2420 ELSE F=1:GOTO 2420
2550 N$="Ungültige Eingabe - Bitte wiederholen":GOSUB 12950:FOR KK=1 TO 25000:NEXT:GOTO 2420 'Nachricht
2560 GOSUB 12430                                               'Daten speichern
2570 Q$="Wollen Sie noch einen weiteren Datensatz ändern?":GOSUB 12520 'Ja/Nein-Frage
2580 IF JA=1 THEN 2160
2590 GOTO 1760                                                    'Zum STS-Menü
2600 '
2610 ' *** Stammsatz löschen von 04********** *********************************
2620 '
2630 MENUT$="STAMMSATZ LÖSCHEN":MENUID$="G-01103":GOSUB 12980      'Basis Maske
2631 KOM$="":GOSUB 13030                                        'F-Tastenleiste
2640 PRINT FNCUR$(4,23);"Bitte geben Sie die Mitgliedsnummer"         'Menütext
2650 PRINT FNCUR$(5,25);"des zu löschenden Mitglieds ein."
2660 PRINT FNCUR$(6,15);"(Dabei wird nur der Status des Mitglieds verändert.)"
2670 PRINT FNCUR$(7,23);"Beenden Sie die Eingabe mit RETURN"
2680 PRINT FNCUR$(9,37);"=>..."
2690 LANG=3:BEREICH=2:Z=9:S=39
2700 PRINT HIGHON$:GOSUB 10030:PRINT HIGHOFF$                            'INPUT
2710 IF ZURUCK=2 THEN 1760                                              'F-Keys
2720 IF ZURUCK=3 THEN GOSUB 13220: GOTO 1560
2730 IF ZURUCK=1 THEN GOSUB 11400:GOTO 2630
2740 IF ZURUCK=4 THEN GOSUB 13160:GOTO 2640
2750 IF ZURUCK>4 THEN HOCH=1
2760 IF HOCH<>0 THEN N$="Ungültige Eingabe - Bitte wiederholen":GOSUB 12950:GOTO 2680 'Nachricht
2770 MTGLZAHL=VAL(ANT$):IF MTGLZAHL=0 THEN MTGLZAHL=MTGLMAX+1           'Errors
2780 IF MTGLZAHL>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12950:GOTO 2680 'Nachricht
2790 GOSUB 12340                                                'Daten einlesen
2800 GOSUB 12980                                                   'Basis Maske
2801 PRINT FNCUR$(3,30);"Mitgliedsnummer";FNCUR$(3,46);MTGLZAHL;FNCUR$(5,41);"Beruf:":GOSUB 10250:GOSUB 10420 'Feldmaske
2810 Q$="Endgültig löschen?":GOSUB 12520                         'Ja/Nein-Frage 
2820 IF JA=0 THEN 2850                                               'Löschen ?
2830 DAT$(5)="ausgetreten     ":PRINT FNCUR$(9,16);HIGHON$;DAT$(5);HIGHOFF$
2840 GOSUB 12430                                             'Daten abspeichern
2850 Q$="Soll noch jemand gelöscht werden?":GOSUB 12520          'Ja/Nein-Frage
2860 IF JA=1 THEN 2630 ELSE GOTO 1760
2869 '
2870 ' *** Stammsatz suchen von 05*************** *****************************
2871 '
2880 MENUID$="G-01105"
2890 NUMSUCH1=0:NUMSUCH2=0:MTGL$=""
2900 PRINT HIGHOFF$
2910 IF DRUCKFLAG THEN MENUT$="SUCHEN & DRUCKEN" ELSE MENUT$="STAMMSATZ SUCHEN"
2920 GOSUB 12980                                                   'Basis Maske
2921 KOM$="":GOSUB 13030                                        'F-Tastenleiste
2930 PRINT CURSOFF$                        
2940 IF DRUCKFLAG THEN A$=" Drucken " ELSE A$=" Suchen "
2950 PRINT FNCUR$(6,30)"1 -";A$;"nach Nummern"
2960 PRINT FNCUR$(8,30)"2 -";A$;"nach Maske  ":GOSUB 13220
2970 FKEYMAX=2:GOSUB 13100                                             'Menüeingabe
2980 IF FKEY=1 THEN GOSUB 11530:GOTO 2900                                  'F-Keys
2990 IF FKEY=2 THEN IF DRUCKFLAG THEN GOSUB 13220:GOTO 5010 ELSE 1760
3000 IF FKEY=3 THEN GOSUB 13220: GOTO 1560
3010 IF FKEY=4 THEN GOSUB 13160:GOTO 2970
3020 IF NKEY=1 THEN 3220
3030 F=1                                                     'Suchen nach Maske
3040 FOR I=1 TO 27:SUCH$(I)="":DAT$(I)="":NEXT
3050 KOM$="<F5>=Suchen ":GOSUB 3920
3060 IF DRUCKFLAG THEN KOM$="<F5>=Suchen & Drucken"
3070 GOSUB 13030                                                'F-Tastenleiste
3080 Z=FNROW(FELD$(F)):S=FNCOL(FELD$(F))+14                    'Cursorparameter
3090 LANG=FNLNG(FELD$(F)):BEREICH=3
3100 PRINT FNCUR$(Z,S)
3110 ANT$="":GOSUB 10020                                                 'INPUT
3120 IF ZURUCK=5 THEN MTGLZAHL=1:GOTO 3510                              'F-Keys
3130 IF ZURUCK=1 THEN GOSUB 11530:GOSUB 3920:GOSUB 10420:GOTO 3060
3140 IF ZURUCK=4 THEN GOSUB 13160:GOTO 3080
3150 IF ZURUCK=2 THEN 2880
3160 IF ZURUCK=3 THEN GOSUB 13220: GOTO 1560
3170 IF ANT$<>"" OR AASC=13 THEN SUCH$(F)=ANT$:DAT$(F)=ANT$
3180 IF HOCH=-1 THEN IF F>1 THEN F=F-1 ELSE F=26                   'Cursor hoch
3190 IF HOCH=1 OR AASC=13 THEN IF F<27 THEN F=F+1 ELSE F=1       'Cursor runter
3200 N$="":GOSUB 12950                                             'Nachricht
3210 GOTO 3080
3220 PRINT CLS$:PRINT:PRINT                                'Suchen nach Nummern
3230 Z=7:S=1:IF DRUCKFLAG THEN DRUCK$="gedruckt" ELSE DRUCK$="gesucht "
3240 BEREICH=2:LANG=3:PRINT CURSON$;                                  'Menütext
3250 GOSUB 12980                                                   'Basis Maske
3251 PRINT FNCUR$(4,5);"Maximale Mitgliedsnummer:";MTGLMAX
3252 KOM$="":GOSUB 13030                                        'F-Tastenleiste
3260 PRINT HOME$;:PRINT FNCUR$(Z,S);                    'Beginn bei Nr...
3270 PRINT HIGHOFF$;"Ab welcher Mitgliedsnummer soll ";DRUCK$;" werden      =>...":S=56
3280 GOSUB 10030                                                         'INPUT
3290 IF ZURUCK=1 THEN GOSUB 11530:GOTO 3220                             'F-Keys
3300 IF ZURUCK=2 THEN 2880
3310 IF ZURUCK=3 THEN GOSUB 13220: GOTO 1560
3320 IF ZURUCK=4 THEN GOSUB 13160:S=1:GOTO 3260
3330 IF ANT$="" THEN S=1:GOTO 3260
3340 NUMSUCH1=VAL(ANT$)
3350 IF NUMSUCH1>MTGLMAX OR NUMSUCH1<1 THEN 3480
3360 Z=9:S=1
3370 N$="":GOSUB 12950                                             'Nachricht
3380 PRINT HOME$:PRINT FNCUR$(Z,S);                       'Ende bei Nr...
3390 PRINT HIGHOFF$;"Bis zu welcher Mitgliedsnummer soll ";DRUCK$;" werden  =>...":S=56
3400 GOSUB 10030                                                         'INPUT
3410 IF ZURUCK=1 THEN GOSUB 11530:GOTO 3220                             'F-Keys
3420 IF ZURUCK=2 THEN 2880
3430 IF ZURUCK=3 THEN GOSUB 13220: GOTO 1560
3440 IF ZURUCK=4 THEN GOSUB 13160:S=1:GOTO 3380
3450 IF ANT$="" THEN S=1:GOTO 3380
3460 NUMSUCH2=VAL(ANT$)
3470 IF NUMSUCH2<=MTGLMAX AND NUMSUCH1<=NUMSUCH2 THEN 3500
3480 N$="Unlogische Eingabe - Bitte neu eingeben":GOSUB 12950         'Errors 'Nachricht
3490 Z=6:S=1:PRINT FNCUR$(9,1);DELLINE$;FNCUR$(Z,S);:Z=7:GOTO 3260
3500 MTGLZAHL=NUMSUCH1                                               'Feldmaske
3510 PRINT CURSOFF$
3520 IF DRUCKFLAG THEN GOSUB 15100
3530 CC$="  <F7>=Adressaufkleber"
3540 IF NUMSUCH1<>0 THEN KOM$="<F5>=Suchen rückwärts  <F6>=vorwärts"+CC$ ELSE KOM$="<F6>=Suchen vorwärts"+CC$
3550 GOSUB 3920
3560 PRINT FNCUR$(3,30);"Mitgliedsnummer"
3570 IF NUMSUCH1=0  AND MTGLZAHL<=MTGLMAX  THEN 3640
3580 IF NUMSUCH1<>0 AND MTGLZAHL<=NUMSUCH2 THEN 3640
3590 IF DRUCKFLAG THEN GOSUB 13220:GOTO 2880
3600 PRINT HIGHON$;CURSOFF$;:MTGLZAHL=MTGLZAHL-1
3610 N$="Daten nicht vorhanden. Mit <F2> neue Suchmöglichkeit.":GOSUB 12950 'Nachricht
3620 PRINT HIGHOFF$;
3630 GOTO 3790
3640 GOSUB 12340                                                   'Daten lesen
3650 IF NUMSUCH1<>0 THEN 3750
3660 FOR K=1 TO FELDMAX                                      'Daten durchsuchen
3670   IF LEFT$(SUCH$(K),1)<>"*" THEN 3700
3680   IF INSTR(DAT$(K),RIGHT$(SUCH$(K),LEN(SUCH$(K))-1))=0 THEN MTGLZAHL=MTGLZAHL+1:GOTO 3570
3690   GOTO 3740
3700   I=1
3710   IF MID$(SUCH$(K),I,1)="?" THEN I=I+1:GOTO 3710
3720   I=I-1
3730   IF INSTR(DAT$(K),RIGHT$(SUCH$(K),LEN(SUCH$(K))-I))<>I+1 THEN MTGLZAHL=MTGLZAHL+1:GOTO 3570
3740 NEXT
3750 GOSUB 10420                                                     'Feldmaske
3760 MTGL$=STR$(MTGLZAHL) 
3770 PRINT CURSOFF$;FNCUR$(3,46);MTGL$
3780 IF DRUCKFLAG THEN GOSUB 14920:IF BRKFLAG THEN BRKFLAG=0:N$="":GOSUB 12950:GOTO 2880 ELSE N$="":GOSUB 12950:PRINT CURSOFF$:MTGLZAHL=MTGLZAHL+1:GOTO 3570 'Nachricht 'Nachricht
3790 ZURUCK=ASC(INPUT$(1))-240
3800 N$="":GOSUB 12950                                             'Nachricht
3810 IF ZURUCK=1 THEN GOSUB 11530:GOSUB 3920:PRINT FNCUR$(3,30);"Mitgliedsnummer":GOTO 3750
3820 IF ZURUCK=2 THEN IF NUMSUCH1<>0 THEN 3220 ELSE 3030
3830 IF ZURUCK=3 THEN GOSUB 13220:GOTO 1560
3840 IF ZURUCK=7 THEN GOSUB 15170:GOSUB 13030:N$="":GOSUB 12950:GOTO 3750 'F-Tastenleiste 'Nachricht
3850 IF ZURUCK=4 THEN GOSUB 13160:GOTO 3750
3860 IF ZURUCK=5 AND NUMSUCH1<>0 THEN IF MTGLZAHL>NUMSUCH1 THEN MTGLZAHL=MTGLZAHL-1:GOTO 3640
3870 IF ZURUCK=6 THEN 3900
3880 N$="Falsche Eingabe":GOSUB 12950                            'Nachricht
3890 GOTO 3790
3900 MTGLZAHL=MTGLZAHL+1:PRINT FNCUR$(21,1);DELLINE$;HOME$;
3910 GOTO 3570
3920 GOSUB 12980                                                   'Basis Maske
3930 IF DRUCKFLAG THEN MENUT$="SUCHEN & DRUCKEN" ELSE MENUT$="STAMMSATZ SUCHEN"
3940 GOSUB 10250
3950 IF NOT DRUCKFLAG THEN GOSUB 13030                          'F-Tastenleiste
3960 PRINT FNCUR$(5,41);"BERUF:"
3970 RETURN
3980 '
3990 ' *** Stammsatz ausdrucken von 02************ & 04********** *************
4000 '
4010 MENUT$="STAMMSATZ AUSDRUCKEN":MENUID$="G-01104":GOSUB 12980   'Basis Maske
4011 KOM$="":GOSUB 13030                                        'F-Tastenleiste
4020 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer"
4030 PRINT FNCUR$(5,21)"des auszudruckenden Stammsatzes ein."
4040 PRINT FNCUR$(6,23)"Beenden Sie die Eingabe mit RETURN"
4050 PRINT FNCUR$(8,37);"=>..."
4060 LANG=3:BEREICH=2:Z=8:S=39
4070 PRINT HIGHON$:GOSUB 10030:PRINT HIGHOFF$                            'INPUT
4080 IF ZURUCK=1 THEN GOSUB 11750:GOTO 4010                             'F-Keys
4090 IF ZURUCK=2 THEN 1760
4100 IF ZURUCK=3 THEN PRINT HIGHOFF$:GOSUB 13220: GOTO 1560
4110 IF ZURUCK=4 THEN GOSUB 13160:GOTO 4050
4120 IF ZURUCK>4 THEN HOCH=1
4130 IF HOCH<>0 THEN 4050
4140 MTGLZAHL=VAL(ANT$):IF MTGLZAHL=0 THEN MTGLZAHL=MTGLMAX+1           'Errors
4150 IF MTGLZAHL>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12950:GOTO 4050 'Nachricht
4160 GOSUB 12340                                                   'Daten lesen
4170 GOSUB 12980                                                   'Basis Maske
4171 PRINT FNCUR$(3,30);"Mitgliedsnummer";HIGHON$;FNCUR$(3,46);MTGLZAHL;HIGHOFF$;FNCUR$(5,41);"Beruf:":GOSUB 10250:GOSUB 10420
4180 Q$="Wollen Sie diese Daten ausdrucken lassen?":GOSUB 12520  'Ja/Nein-Frage
4181 IF JA=0 THEN 4010                                            'Ausdrucken ?
4190 Q$="Ist der Drucker bereit zur Ausgabe?":GOSUB 12520        'Ja/Nein-Frage
4191 IF JA=0 THEN N$="Bitte schalten Sie den Drucker ein.":GOSUB 12950:FOR DEL=0 TO 1000:NEXT:GOTO 4180     'Drucker bereit ? 'Nachricht
4200 GOSUB 15310:GOTO 4010                                             'Drucken
4210 '
4220 ' Verwendungsnachweis vorbereiten ****************************************
4230 '
4240 OPEN "I",#1,"B:MTGLZAHL.DAT":INPUT #1,MTGL$:MTGLMAX=VAL(MTGL$):CLOSE #1
4250 DEL$=RUB$+" "+RUB$:VNFLAG=-1
4260 '
4270 ' Verwendungsnachweis Menü ***********************************************
4280 '
4290 MENUT$="VERWENDUNGSNACHWEIS ":MENUID$="G-01200":GOSUB 12980   'Basis Maske
4300 F=4:PRINT CURSON$:KOM$="":GOSUB 13030                      'F-Tastenleiste
4301 PRINT HIGHOFF$
4310 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer"
4320 PRINT FNCUR$(5,20)"des zu ändernden Verwendungsnachweises ein"
4330 PRINT FNCUR$(6,22)"und beenden Sie die Eingabe mit RETURN"
4340 PRINT FNCUR$(8,37)"=>..."
4350 LANG=3:BEREICH=2:Z=8:S=39
4360 GOSUB 10020
4370 IF ZURUCK=1 THEN GOSUB 11850:GOTO 4290
4380 IF ZURUCK=2 OR ZURUCK=3 THEN GOSUB 13220:VNFLAG=0:GOTO 1560
4390 IF ZURUCK=4 THEN GOSUB 13160
4400 IF ZURUCK>4 THEN 4350
4410 IF HOCH<>0 THEN GOTO 4350
4420 IF LEN(ANT$)=0 THEN N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 12950:GOTO 4340 ELSE MITGLIED=VAL(ANT$):IF MITGLIED=0 THEN 4360 'Nachricht
4430 IF MITGLIED>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12950:PRINT HOME$;HIGHOFF$:GOTO 4340 'Nachricht
4440 MTGLZAHL=MITGLIED:MTGL$=STR$(MITGLIED)
4449 '
4450 ' *** Einlesen des Mitgliedsnamens ***************************************
4451 '
4460 PRINT FNCUR$(21,1);DELLINE$;
4470 GET #2,MITGLIED                                        'EINLESEN DATEN STS
4480 NNAM$=LEFT$(DD1$,25)                                 'NACHNAME RAUSFISCHEN
4490 VNAM$=MID$(DD1$,26,25)                               'VORNAME  RAUSFISCHEN
4499 '
4500 ' ** LEERZEICHEN HINTER NACHNAME ABSCHNEIDEN *****************************
4501 '
4510 FOR I=LEN(NNAM$) TO 1 STEP -1
4520   IF MID$(NNAM$,I,1)<>" " THEN NNAM$=LEFT$(NNAM$,I):I=1
4530 NEXT
4540 NAMM$=NNAM$+" "+VNAM$
4550 GOSUB 13700                                          'EINLESEN 1. VN-BLATT
4560 BLATT=1
4570 VP=1:HP=1                                                 'INITIALISIERUNG
4580 F=8
4590 GOSUB 12980                                                   'Basis Maske
4600 KOM$="<F7>=Abspeichern":GOSUB 13030                        'F-Tastenleiste
4620 PRINT HIGHOFF$
4630 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=Vorwärts";TAB(54);"<F9>=Blatt drucken";
4640 PRINT HOME$
4650 PRINT HOME$;FNCUR$(5,2);"Mitglieds-Nr.:"
4660 PRINT FNCUR$(3,2); "Blatt:"
4670 PRINT FNCUR$(5,24);"Name:"
4680 MSK$="Von:  .  .   Bis:  .  .   Amt:"
4690 FOR KK=7 TO 19 
4700   PRINT FNCUR$(KK,2)MSK$;TAB(43)MSK$
4710 NEXT
4720 GOSUB 14500                                                 'Ausgabe Daten
4730 LANG=2-(HP=7)*6                                   'BEGIN OF THE INPUT-LOOP
4740 BEREICH=2-(HP=7)
4750 Z=VP+13*(VP>13)+6
4760 S=6-39*(VP>13)-(HP<7)*(HP-1)*3+(HP>3)*(HP<7)*4-(HP=7)*26
4770 N$="":GOSUB 12950                         'ERASE MESSAGE-LINE 'Nachricht
4780 GOSUB 10030                                                         'INPUT
4790 IF HP<7 THEN IF AASC=13 THEN ANT$=STRING$(2-LEN(ANT$),"0")+ANT$   '+NULLEN
4800 IF HOCH<>0 OR AASC=13 THEN PRINT FNCUR$(Z,S);ANT$
4810 IF AASC=13 THEN IF HP<7 THEN MID$(DAT$(VP),-1+2*HP,2)=ANT$ ELSE DAT$(VP)=LEFT$(DAT$(VP),12)+ANT$+SPACE$(8-LEN(ANT$))
4820 IF AASC=13 AND HP<7 THEN JA=1:TEST$=ANT$:ON HP GOSUB 12730,12750,12770,12730,12750,12770:IF JA=0 THEN N$="Ungültiges Datum":GOSUB 12950:GOTO 4780 'Datums-Check 'Nachricht
4830 IF AASC=13 THEN IF HP=7 THEN HP=1:VP=VP-(VP<26)-(VP=26)*(-25):GOTO 4730 ELSE HP=HP+1:GOTO 4730     'NEUE SCREEN-POSITION
4840 IF HOCH<>0 THEN TEST$=MID$(DAT$(VP),-(HP<7)*2*HP-1-(HP=7)*14,-(HP<7)*2-(HP=7)*8):PRINT FNCUR$(Z,S);TEST$
4850 IF HOCH<>0 THEN IF HP<7 AND NOT TEST$="  " THEN JA=1:ON HP GOSUB 12730,12750,12770,12730,12750,12770:IF JA=0 THEN N$="Ungültiges Datum":GOSUB 12950:GOTO 4780 'Nachricht
4860 IF HOCH=1  THEN VP=VP-(VP<26)-(VP=26)*(-25):GOTO 4730
4870 IF HOCH=-1 THEN VP=VP+(VP>1)-(VP=1)*25     :GOTO 4730
4880 IF HOCH=2  THEN HP=HP-(HP<7)-(HP=7)*(-6)   :GOTO 4730
4890 IF HOCH=-2 THEN HP=HP+(HP>1)+(HP=1)*(-6)   :GOTO 4730
4900 IF ZURUCK=1 THEN GOSUB 11950 :GOTO 4570         'HILFE - ZU INIT UND MASKE
4910 IF ZURUCK=2 THEN GOTO 4290                                'ZU MTGL-EINGABE
4920 IF ZURUCK=3 THEN VNFLAG=0:GOSUB 13220:GOTO 1560
4930 IF ZURUCK=4 THEN GOSUB 13160:GOSUB 13070:GOTO 4780 
4940 IF ZURUCK=5 THEN GOSUB 13750:GOTO 4730
4950 IF ZURUCK=6 THEN GOSUB 13830:GOTO 4730
4960 IF ZURUCK=7 THEN GOSUB 14230:GOTO 4780             'ABSPEICHERN DANN INPUT
4970 IF ZURUCK=9 THEN GOSUB 14690:GOSUB 13880:GOTO 4770
4980 '
4990 ' *** Mitgliedsliste drucken von 05*************** ***********************
5000 '
5010 DRUCKFLAG=-1:MTGL$="":DATUM$="  .  .  ":GOSUB 1680:GOSUB 10200
5020 PRINT CLS$;
5021 PRINT HIGHOFF$;CURSOFF$;
5022 MENUT$="MITGLIEDSLISTE DRUCKEN":MENUID$="G-01300":GOSUB 12980 'Basis Maske
5030 KOM$="":GOSUB 13030                                        'F-Tastenleiste
5040 PRINT FNCUR$( 6,30);"1 - Suchen & Ausdrucken
5050 PRINT FNCUR$( 8,30);"2 - Komplett ausdrucken"
5060 PRINT FNCUR$(10,30);"3 - Datenblatt drucken"
5070 FKEYMAX=3:GOSUB 13100                                             'Menüeingabe
5080 IF FKEY=1 THEN GOSUB 12120:GOTO 5020
5090 IF FKEY=2 OR FKEY=3 THEN GOSUB 13220:DRUCKFLAG=0:GOTO 1560
5100 IF FKEY=4 THEN GOSUB 13160:GOTO 5070
5110 IF NKEY=1 THEN 5140
5120 IF NKEY=2 THEN DRUCKFLAG=0:GOTO 5140
5130 IF NKEY=3 THEN 5440
5140 GOSUB 12980                                                   'Basis Maske
5141 GOSUB 13030                                                'F-Tastenleiste
5150 PRINT FNCUR$(7,13);"Geben Sie bitte das heutige Datum ein :   .  .  ":Z=7:LANG=2:BEREICH=2
5160 PRINT HIGHON$;CURSON$
5170 FOR K=0 TO 2
5180   S=53+K*3
5190   VNFLAG=-1:GOSUB 10030:VNFLAG=0    'in order to activate right-left shift
5200 IF HOCH<>0 THEN JA=1:TEST$=MID$(DATUM$,K*3+1,2):ON K+1 GOSUB 12730,12750,12770:IF JA=0 THEN N$="Ungültiges Datum":GOSUB 12950:GOTO 5180 'Nachricht
5210   IF ZURUCK=1 THEN GOSUB 12120:GOTO 5140
5220   IF ZURUCK=2 THEN 5010
5230   IF ZURUCK=3 THEN GOSUB 13220:DRUCKFLAG=0:GOTO 1560
5240   IF ZURUCK=4 THEN GOSUB 13160:GOTO 5180
5250   IF HOCH=-2 THEN IF K=0 THEN 5180 ELSE K=K-1:GOTO 5180
5260   IF HOCH=+2 THEN IF K=2 THEN K=0:GOTO 5180 ELSE K=K+1:GOTO 5180
5270   IF AASC<>13 THEN 5180
5280   ANT$=STRING$(2-LEN(ANT$),"0")+ANT$:PRINT FNCUR$(Z,S);ANT$
5290   TEST$=ANT$:JA=1:ON K+1 GOSUB 12730,12750,12770:IF JA=0 THEN N$="Ungültige Datumseingabe":GOSUB 12950:GOTO 5180 'Nachricht
5300   MID$(DATUM$,K*3+1,2)=ANT$
5310 NEXT
5320 PRINT HIGHOFF$;CURSOFF$
5330 Q$="Bitte den Drucker bereit machen. Fertig?":GOSUB 12520   'Ja/Nein-Frage
5331 IF JA=0 THEN N$="":GOSUB 12950:GOTO 5140                      'Nachricht
5339 '
5340 ' *** Ausdrucken Anfang **************************************************
5341 '
5350 IF NOT DRUCKFLAG THEN PRINT FNCUR$(7,1);DELLINE$CURSOFF$:N$="Mitgliedsliste wird gedruckt. Abbrechen mit Taste.":GOSUB 12950 'Nachricht
5360 IF DRUCKFLAG THEN 2890
5370 PRINT FNCUR$(23,1);DELLINE$;:GOSUB 15100
5380 FOR MTGLZAHL=1 TO MTGLMAX
5390   GOSUB 12340:IF INSTR(DAT$(5),"ausgetreten")>0 THEN 5410
5400   GOSUB 14920:IF BRKFLAG THEN BRKFLAG=0:N$="Drucken abgebrochen":GOSUB 12950:PRINT CURSOFF$:GOTO 5420 ELSE GOSUB 12950:PRINT CURSOFF$ 'Nachricht
5410 NEXT
5420 GOSUB 13220:DRUCKFLAG=0:GOTO 5010
5429 '
5430 ' *** Datenblatt drucken *************************************************
5431 '
5440 MENUT$="DATENBLATT DRUCKEN":MENUID$="G-01303":GOSUB 12980     'Basis Maske
5441 KOM$="":GOSUB 13030                                        'F-Tastenleiste
5450 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer"
5460 PRINT FNCUR$(5,24)"des zu druckenden Datenblattes ein."
5470 PRINT FNCUR$(7,23)"Beenden Sie die Eingabe mit RETURN"
5480 PRINT FNCUR$(9,37)"=>..."
5490 LANG=3:BEREICH=2:Z=9:S=39
5500 PRINT HIGHON$:GOSUB 10030:PRINT HIGHOFF$                            'INPUT
5510 IF ZURUCK=2 THEN 5010                                              'F-Keys
5520 IF ZURUCK=3 THEN GOSUB 13220: GOTO 1560
5530 IF ZURUCK=1 THEN GOSUB 12120:PRINT CURSON$:MENUT$="DATENBLATT DRUCKEN":GOTO 5440
5540 IF ZURUCK=4 THEN GOSUB 13160:GOTO 5440
5550 IF ZURUCK>4 THEN HOCH=1 
5560 IF HOCH<>0 THEN N$="Ungültige Eingabe - Bitte wiederholen":GOSUB 12950:GOTO 5480 'Nachricht
5570 MTGLZAHL=VAL(ANT$):IF MTGLZAHL=0 THEN MTGLZAHL=MTGLMAX+1           'Errors
5580 IF MTGLZAHL>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12950:GOTO 5480 'Nachricht
5590 Q$="Ist der Drucker bereit zur Ausgabe?":GOSUB 12520        'Ja/Nein-Frage
5591 IF JA=0 THEN N$="Bitte schalten Sie den Drucker ein.":GOSUB 12950:FOR DEL=0 TO 1000:NEXT:GOTO 5440     'Drucker bereit ? 'Nachricht
5600 GOSUB 12340                                            'S.S.Daten einlesen
5610 NNAM$ = LEFT$(DD1$,25) : NNAM$ = LEFT$(NNAM$,INSTR(NNAM$," "))
5620 NAMM$ = NNAM$ + MID$(DD1$,26,25)
5630 GOSUB 15310                                                   'S.S.drucken
5640 GOSUB 13700                                            'V.N Daten einlesen
5650 GOSUB 14710                                             'V.N Daten drucken
5660 GOSUB 15490                                             'Mitgliedsvermerke
5670 GOTO 5440
10000 '
10001 ' ***********************************************************************
10002 ' *                                                                     *
10003 ' *        Unterprogramme von 06*****, 07****, 08**** und 09****        *
10004 ' *                                                                     *
10005 ' ***********************************************************************
10006 '
10010 ' *** Eingaberoutine (INPUT) ********************************************
10011 '
10020 MUSS$=MID$(FELD$(F),13,1)
10030 PRINT HIGHON$:ANT$="":I=1
10040 HOCH=0:ZURUCK=0:PRINT HOME$;" ":PRINT FNCUR$(Z,S+I-1);
10050 A$=INPUT$(1):AASC=ASC(A$)
10060 ON BEREICH GOSUB 12590,12640,12670,12700                  'Bereichs check
10070 IF DRIN=1 THEN IF I<=LANG THEN PRINT A$;:ANT$=LEFT$(ANT$,I-1)+A$:I=I+1:GOTO 10050 ELSE GOTO 10050
10080 IF AASC=8 THEN IF I>1 THEN PRINT DEL$;:I=I-1:ANT$=LEFT$(ANT$,I-1):GOTO 10040 ELSE GOTO 10050                'Delete
10090 IF VNFLAG THEN IF AASC=13 THEN PRINT HIGHOFF$;STRING$(LANG-I+1," ");HIGHON$:RETURN 'BEI VN RETURN OHNE PUNKTE
10100 IF AASC=13  THEN PRINT HIGHOFF$;STRING$(LANG-I+1,"."):RETURN      'Return
10110 IF AASC=200 THEN HOCH=-1:GOSUB 10180:RETURN                  'Cursor hoch
10120 IF AASC=201 THEN HOCH=+1:GOSUB 10180:RETURN                'Cursor runter
10130 IF VNFLAG AND AASC=202 THEN HOCH=-2:GOSUB 10180:RETURN
10140 IF VNFLAG AND AASC=203 THEN HOCH=2 :GOSUB 10180:RETURN
10150 IF AASC>240 AND AASC<250 THEN ZURUCK=AASC-240:RETURN              'F-Keys
10160 N$="Ungültige Eingabe - Bitte wiederholen":GOSUB 12950:GOTO 10040 'Nachricht
10169 '
10170 ' *** Cursor hoch/runter Repeat Blockierung *****************************
10171 '
10180 A$=INKEY$:IF A$>CHR$(199) THEN 10180 ELSE RETURN
10189 '
10190 ' *** Disk I/O Variablen Initalisierung *********************************
10191 '
10200 OPEN "I",#1,"B:MTGLZAHL.DAT"
10210 INPUT #1,MTGL$:MTGLZAHL=VAL(MTGL$):CLOSE #1
10220 FOR K=1 TO FELDMAX:DAT$(K)="":SUCH$(K)="":NEXT
10230 RETURN
10239 '
10240 ' *** Feldnamen Maske ***************************************************
10241 '
10250 PRINT HIGHOFF$CURSOFF$;
10260 FOR K=1 TO FELDMAX
10270   S=FNCOL(FELD$(K))
10280   Z=FNROW(FELD$(K))
10290   PRINT FNCUR$(Z,S);MID$(FELD$(K),1,14);STRING$(FNLNG(FELD$(K)),46)
10300 NEXT
10310 MTGL$=STR$(MTGLZAHL):PRINT CURSON$;
10320 RETURN
10329 '
10330 ' *** Pünktchen der Felder **********************************************
10331 '
10340 PRINT HOME$;" "
10350 FOR K=1 TO FELDMAX
10360   S=FNCOL(FELD$(K))+14
10370   Z=FNROW(FELD$(K))
10380   PRINT FNCUR$(Z,S);STRING$(FNLNG(FELD$(K)),46)
10390 NEXT
10400 RETURN
10409 '
10410 ' *** Felddaten ausgeben ************************************************
10411 '
10420 PRINT HIGHON$
10430 FOR K=1 TO FELDMAX
10440   LANG=FNLNG(FELD$(K))
10450   E1=FNCOL(FELD$(K))+14
10460   E2=FNROW(FELD$(K))
10470   PRINT FNCUR$(E2,E1);DAT$(K)
10480 NEXT
10490 RETURN
10491 '
10500 ' *** Help-Text "Mitgliederverwaltung" ********************************** 
10510 '
10520 MENUT$="HILFE":GOSUB 12980                                   'Basis Maske
10525 PRINT HOME$
10530 PRINT "       Die MITGLIEDERVERWALTUNG bietet folgende Funktionen an :"
10540 PRINT 
10550 PRINT "        1 - STAMMSATZPFLEGE"
10560 PRINT "            Bearbeiten von Mitgliedsdaten gemäß dem Personal-"
10570 PRINT "            stammblatt." 
10580 PRINT
10590 PRINT "        2 - VERWENDUNGSNACHWEIS"
10600 PRINT "            Bearbeiten von Mitgliedsdaten, die Ämter umd Amts-"
10610 PRINT "            perioden eines Mitglieds enthalten."
10620 PRINT
10630 PRINT "        3 - MITGLIEDSLISTE DRUCKEN"
10640 PRINT "            Ausdruck von Mitgliedslisten nach verschiedenen"
10650 PRINT
10660 PRINT "            Gesichtspunkten."
10670 PRINT "    Wählen Sie aus, indem Sie die zugehörige Zahlentaste drücken."
10680 GOSUB 13200
10685 RETURN
10690 '
10700 ' *** Help-Text "Stammsatzpflege" *************************************** 
10710 '
10720 MENUT$="HILFE":GOSUB 12980                                   'Basis Maske
10725 PRINT HOME$
10730 PRINT "     Die STAMMSATZPFLEGE bietet folgende Funktionen an:"
10740 PRINT
10750 PRINT "      1 - STAMMSATZ ANLEGEN"
10760 PRINT "          Erfassen der Daten eines neuen Mitglieds."
10770 PRINT
10780 PRINT "      2 - STAMMSATZ ÄNDERN"
10790 PRINT "          Verändern eines bereits bestehenden Stammsatzes."
10800 PRINT
10810 PRINT "      3 - STAMMSATZ LÖSCHEN"
10820 PRINT "          Verändern des Mitgliedsstatus auf `ausgetreten`."
10830 PRINT
10840 PRINT "      4 - STAMMSATZ DRUCKEN"
10850 PRINT "          Ausdrucken eines kompletten Stammsatzes."
10860 PRINT
10870 PRINT "      5 - STAMMSATZ SUCHEN"
10880 PRINT "          Suchen vom Stammsätzen nach bestimmten Gesichtspunkten."
10890 GOSUB 13200
10895 RETURN
10900 '
10910 ' *** Help-Text "Stammsatz anlegen" *************************************
10920 '
10930 MENUT$="HILFE":GOSUB 12980                                   'Basis Maske
10935 PRINT HOME$
10940 PRINT "   Sie können jetzt Daten eines neuen Mitglieds eingeben,"
10950 PRINT "   jedoch nicht die eines bestehenden Mitglieds ändern."
10960 PRINT
10970 PRINT "   Die Mitgliedsnummer wird laufend vergeben."
10980 PRINT
10990 PRINT "   Während der Dateneingebe können Sie mit der DELETE-Taste "
11000 PRINT "   korrigieren. Ist ein Feld ausgefüllt worden, muß RETURN"
11010 PRINT "   gedrückt werden, da sonst die Eingabe nicht gespeichert"
11020 PRINT "   wird. Mit den Cursortasten kann das Feld gewechselt werden."
11030 PRINT
11040 PRINT "   `=>`-Felder sollten ausgefüllt werden."
11050 PRINT
11060 PRINT "   Weitere Funktionstasten :"
11070 PRINT "   <F7>  Diesen Stammsatz abspeichern."
11080 GOSUB 13200
11081 MENUT$="STAMMSATZ ANLEGEN"
11082 DATENAUSG=1
11083 RETURN
11090 '
11100 ' *** Help-Text "Stammsatz ändern (I)" **********************************
11110 '
11120 MENUT$="HILFE":GOSUB 12980                                   'Basis Maske
11121 PRINT HOME$
11130 PRINT "      Sie können jetzt einen bestehenden Stammsatz ändern."
11140 PRINT
11150 PRINT "      Geben Sie die Mitgliedsnummer des betreffenen Stammsatzes"
11160 PRINT "      ein. Bei Tippfehlern können Sie mit DELETE-Taste korri-"
11170 PRINT "      gieren."
11180 GOSUB 13200   
11181 RETURN
11190 '
11200 ' *** Help-Text "Stammsatz ändern (II)" *********************************
11210 '
11220 MENUT$="HILFE":GOSUB 12980                                   'Basis Maske
11221 PRINT HOME$
11230 PRINT "    Sie können jetzt einen bestehenden Stammsatz ändern."
11240 PRINT
11250 PRINT "    Während der Dateneingabe können Sie mit der DELETE-Taste"
11260 PRINT "    korrigieren. Ist ein Feld ausgefüllt, muß RETURN gedrückt"
11270 PRINT "    werden, da sonst die Änderung nicht gespeichert wird."
11280 PRINT "      Mit den Cursortasten kann das Feld zum nächsten oberen,"
11290 PRINT "    bzw. unteren Feld gewechselt werden."
11300 PRINT
11310 PRINT "    `=>`-Felder sollten ausgefüllt werden."
11320 PRINT
11330 PRINT "    Weitere Funktionstasten :"
11340 PRINT
11350 PRINT "    <F7>  Diesen Stammsatz speichern.
11360 GOSUB 13200
11361 MENUT$="STAMMDATEN ÄNDERN":GOSUB 12980                       'Basis Maske
11362 GOSUB 13030                                               'F-Tastenleiste
11363 RETURN
11370 '
11380 ' *** Help-Text "Stammsatz löschen" *************************************
11390 '
11400 MENUT$="HILFE":GOSUB 12980                                   'Basis Maske
11401 PRINT HOME$
11410 PRINT "    Sie können jetzt den Status eines Mitglieds auf `ausgetreten'"
11420 PRINT "    ändern. Damit wird der Stammsatz beim Ausdruck von Mitglieds-"
11430 PRINT "    listen nicht berüchsichtigt."
11440 PRINT "    Auf keinen Fall werden die Daten WIRKLICH gelöscht."
11450 PRINT
11460 PRINT "    Geben Sie die Mitgliedsnummer des betreffenden Stammsatzes "
11470 PRINT "    ein. Bei Tippfehlern können Sie mit der DELETE-Taste"
11480 PRINT "    korrigieren."
11490 GOSUB 13200
11491 RETURN
11500 '
11510 ' *** Help-Text "Stammsatz suchen" **************************************
11520 '
11530 IF DRUCKFLAG THEN 12120
11540 MENUT$="HILFE":GOSUB 12980                                   'Basis Maske
11541 PRINT HOME$
11550 PRINT "    Sie können jetzt Stammsätze suchen."
11560 PRINT
11570 PRINT "    1 - Suchen nach Nummern"
11580 PRINT "        Geben Sie ein Intervall von Mitgliedsnummern ein."
11590 PRINT "        Es werden alle Stammsätze ausgegeben, deren Nummern"
11600 PRINT "        im Intervall liegt."
11610 PRINT
11620 PRINT "    2 - Suchen nach Maske"
11630 PRINT "        Sie füllen ein Formular aus. Diejenigen Stammsätze, die"
11640 PRINT "        mit dem Formular übereinstimmen, werden ausgegeben."
11650 PRINT "        Leere Felder bezeichnen beliebige Stammsatzdaten."
11660 PRINT "        Ein * zu Beginn des Feldes bedeutet : Das folgende Wort"
11670 PRINT "        darf sich an beliebiger Stelle des Feldes befinden."
11680 PRINT "        Ein oder mehrere ? zu Beginn des Feldes stellen Platz-"
11690 PRINT "        halter für beliebige Zeichen dar."
11700 PRINT
11710 GOSUB 13200
11711 MENUT$="STAMMSATZ SUCHEN"
11712 RETURN
11720 ' 
11730 ' *** Help-Text für "Stammsatz drucken" *********************************
11740 '
11750 MENUT$="HILFE":GOSUB 12980                                   'Basis Maske
11751 PRINT FNCUR$(5,1)
11760 PRINT "   Sie können jetzt einen kompletten Stammsatz, wie er auf"
11770 PRINT "   dem Bildschirm steht, ausdrucken."
11780 PRINT
11790 PRINT "   Geben Sie die Mitgliedsnummer ein. Bei Tippfehlern können"
11800 PRINT "   Sie mit der DELETE-Taste korrigieren."
11810 MENUT$="STAMMSATZ DRUCKEN":GOSUB 13200
11811 RETURN
11820 '
11830 ' *** Help-Text "Verwendungsnachweis (I)" *******************************
11840 '
11850 MENUT$="HILFE":GOSUB 12980                                   'Basis Maske
11851 PRINT FNCUR$(6,1)
11860 PRINT "    Sie können jetzt einen Verwendungsnachweis ändern."
11870 PRINT
11880 PRINT "    Geben Sie die Mitgliedsnummer des betreffenden "
11890 PRINT "    Verwendungsnachweises ein. Bei Tippfehlern können"
11900 PRINT "    Sie mit der DELETE-Taste korrigieren."
11910 GOSUB 13200
11911 RETURN
11920 '
11930 ' *** Help-Text "Verwendungsnachweis (II)" ******************************
11940 '
11950 MENUT$="HILFE":GOSUB 12980                                   'Basis Maske
11951 PRINT HOME$
11960 PRINT "    Sie können jetzt einen bestehenden Verwendungsnachweis ändern."
11970 PRINT
11980 PRINT "    Während der Dateneingabe können Sie mit der DELETE-Taste"
11990 PRINT "    korrigieren. Ist ein Feld ausgefüllt worden, muß RETURN"
12000 PRINT "    gedrückt werden, da sonst die Änderung nicht gespeichert"
12010 PRINT "    wird. Mit den Cursortasten kann das Feld gewechselt werden."
12020 PRINT
12030 PRINT "    Weitere Funktionstasten :"
12040 PRINT "    <F5>  Zurückblättern (ohne die jetzige Seite zu speichern)"
12050 PRINT "    <F6>  Vorblättern    (ohne die jetzige Seite zu speichern)"
12060 PRINT "    <F7>  Die aktuelle Seite speichern"
12070 PRINT "    <F9>  Die aktuelle Seite ausdrucken"
12080 GOSUB 13200
12081 MENUT$="VERWENDUNGSNACHWEIS"
12082 RETURN
12090 '
12100 ' *** Help-Text "Mitgliedsliste drucken" ********************************
12110 '
12120 MENUT$="HILFE":GOSUB 12980                                   'Basis Maske
12121 PRINT FNCUR$(4,1)
12130 PRINT "    Sie können jetzt eine Mitgliedsliste drucken lassen." 
12140 PRINT
12150 PRINT "    1 - Suchen & Drucken"
12160 PRINT "        Sie können Stammsätze nach verschiedenen Gesichtspunkten"
12170 PRINT "        suchen und drucken lassen."
12180 PRINT
12190 PRINT "    2 - Komplett drucken"
12200 PRINT "        Es wird eine komplette Mitgliedsliste ausgedruckt."
12210 PRINT "        Ausgetretene Mitglieder werden nicht berücksichtigt."
12220 PRINT
12230 PRINT "    3 - Datenblatt drucken"
12240 PRINT "        Der Stammsatz und der Verwendungsnachweis werden im"
12250 PRINT "        Bildschirmformat mit einer Liste von Mitgliedsver-"
12260 PRINT "        merken ausgedruckt."
12270 GOSUB 13200
12271 MENUT$="MITGLIEDSLISTE AUSDRUCKEN"
12272 RETURN
12280 '
12290 ' *** Fielden der Stammsatz Random X.S.-Datei ************************OK*
12291 '
12300 OPEN "R",#2,"B:MV.DAT",462
12310 FIELD #2,236 AS DD1$,225 AS DD2$
12320 RETURN
12329 '
12330 ' *** Einlesen der Felddaten von Disk ***********************************
12331 '
12340 GET #2,MTGLZAHL
12350 FOR LL=1 TO FELDMAX:DAT$(LL)="":NEXT
12360 HLANG=1
12370 FOR K=1 TO 14:LANG=FNLNG(FELD$(K)):DAT$(K)=MID$(DD1$,HLANG,LANG):HLANG=LANG+HLANG:NEXT
12380 HLANG=1
12390 FOR K=15 TO 27:LANG=FNLNG(FELD$(K)):DAT$(K)=MID$(DD2$,HLANG,LANG):HLANG=LANG+HLANG:NEXT
12400 RETURN
12409 '
12410 ' *** Abspeichern der Felddaten auf Disk ********************************
12411 '
12420 GOSUB 13630       'NEUES VN-BLATT ANLEGEN   DARF NUR STS-ERF. ANSPRINGEN!
12430 FOR K=1 TO 27:LANG=FNLNG(FELD$(K)):DAT$(K)=DAT$(K)+STRING$(LANG-LEN(DAT$(K))," "):NEXT
12440 HH$=""
12450 FOR K=1 TO 14:HH$=HH$+DAT$(K):NEXT:LSET DD1$=HH$
12460 HH$="":FOR K=15 TO 27:HH$=HH$+DAT$(K):NEXT:LSET DD2$=HH$:HH$=""
12470 PRINT FNCUR$(21,1);
12480 PUT #2,MTGLZAHL
12490 N$="Die Daten sind auf Diskette abgespeichert.":GOSUB 12950 'Nachricht
12500 RETURN
12509 '
12510 ' *** Ja/Nein - Frage ***************************************************
12511 '
12512 ' IN:  Q$ - Frage
12513 ' OUT: JA 1 = true, 0 = false
12520 N$=Q$+" (J=Ja/N=Nein)":GOSUB 12950                      'Nachricht
12530 PRINT HOME$;" ";
12531 PRINT FNCUR$(23,1);DELLINE$;FNCUR$(24,1);DELLINE$;HOME$
12540 PRINT CURSOFF$;
12541 ANSWER$=INPUT$(1)
12542 PRINT CURSON$;RUB$;
12550 IF ANSWER$="J" OR ANSWER$="j" THEN JA=1:GOSUB 13030:RETURN 'F-Tastenleiste
12560 IF ANSWER$="N" OR ANSWER$="n" THEN JA=0:RETURN
12570 GOTO 12540
12579 '
12580 ' *** Testbereich 1 (Alpha) *********************************************
12581 '
12590 DRIN=0
12600 IF AASC>57 THEN IF AASC<127        THEN DRIN=1:RETURN
12610 IF AASC>31 THEN IF AASC<48         THEN DRIN=1
12620 RETURN
12629 '
12630 ' *** Testbereich 2 (Numerisch) *****************************************
12631 '
12640 DRIN=0:IF AASC>47 AND AASC<58      THEN DRIN=1
12650 RETURN
12659 '
12660 ' *** Testbereich 3 (Alphanumerisch) ************************************
12661 '
12670 DRIN=0:IF AASC>31 THEN IF AASC<127 THEN DRIN=1
12680 RETURN
12689 '
12690 ' *** Testbereich 4 (Numerisch + "/" und ".") ***************************
12691 '
12700 DRIN=0:IF AASC>45 THEN IF AASC<58  THEN DRIN=1
12710 RETURN
12719 '
12720 ' *** DATUMS-TEST (bei VN und M-Liste drucken) **************************
12721 '
12730 IF VAL(TEST$)=0 OR VAL(TEST$)>31 THEN JA=0                           'TAG
12740 RETURN
12750 IF VAL(TEST$)=0 OR VAL(TEST$)>12 THEN JA=0                         'MONAT
12760 RETURN
12770 IF VAL(TEST$)=0 THEN JA=0                                           'JAHR
12780 RETURN
12789 '
12790 ' *** EXIT für ERFASSEN *************************************************
12791 '
12800 K=1:FOR II=1 TO 30:A$=INKEY$:NEXT:A$=""
12810 PRINT FNCUR$(21,1);DELLINE$
12820 IF INSTR(SPACE$(25),DAT$(K))<>0 THEN K=K+1:IF K<28 THEN 12820 ELSE MTGLZAHL=MTGLZAHL-1:MTGL$=STR$(MTGLZAHL):GOTO 12850
12830 Q$="Wollen Sie diese Daten abspeichern?":GOSUB 12520       'Ja/Nein-Frage
12840 IF JA=1 THEN GOSUB 12420:N$="Daten werden abgespeichert.":GOSUB 12950 ELSE N$="Daten werden nicht abgespeichert.":GOSUB 12950:M=MTGLZAHL-1:MTGL$=STR$(M) 'Nachricht 'Nachricht
12850 OPEN "O",#1,"B:MTGLZAHL.DAT":PRINT #1,MTGL$:CLOSE #1
12860 MTGLMAX=MTGLZAHL:MTGL$=STR$(MTGLZAHL)
12870 RETURN
12879 '
12880 ' *** Voreinstellungen im ERFASSEN  (Defaults) **************************
12881 '
12890 DAT$(5)="10***"
12901 DAT$(7)=""11**""
12910 DAT$(11)="12**"
12911 DAT$(12)="13******"
12920 DAT$(21)="3"
12921 DAT$(22)="Nein"
12930 DAT$(23)="Nein"
12931 DAT$(25)="Nein"
12932 RETURN
12939 '
12940 ' *** Nachricht anzeigen *********************************************OK*
12941 '
12942 ' IN: N$ - Nachricht
12950 PRINT HOME$;" "                                'TODO: is this neccessary?
12951 PRINT FNCUR$(21,2);DELLINE$;INVON$;" ";N$;" ";INVOFF$
12960 RETURN
12969 '
12970 ' *** Bildschirmmaske - Basis ****************************************OK*
12971 '
12972 ' IN: MENUT$  - Menu title
12973 ' IN: MENUID$ - Menu ID
12980 PRINT CLS$;
12981 PRINT HIGHOFF$;
12982 PRINT "00**********************",
12990 PRINT FNCUR$(1,(80-LEN(MENUT$))/2);MENUT$;FNCUR$(1,72);MENUID$
13000 PRINT STRICH$;
13001 PRINT FNCUR$(21,1);
13002 PRINT STRICH$
13010 RETURN
13019 '
13020 ' *** Funktionstasten Leiste (KEY LABELS) *******************************
13021 '
13030 PRINT HIGHOFF$;FNCUR$(23,1);"<F1>=Hilfe <F2>=Zurück <F3>=Anfang <F4>=Hauptmenü ";
13040 PRINT KOM$;: PRINT HOME$;" "
13050 RETURN
13051 '
13060 ' *** restliche funktionstasten bei Verw.-nachweis ausgeben
13061 '
13070 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=Vorwärts             <F8>=Drucken";
13071 PRINT HOME$;" "
13080 RETURN
13081 '
13090 ' *** Eingabe bei Menübildern *******************************************
13091 '
13092 ' IN: FKEYMAX - max valid function key
13093 ' OUT: FKEY - number of pressed function key or 0
13094 ' OUT: NKEY - number of pressed numeric key or 0
13100 PRINT CURSOFF$
13101 NKEY=0:FKEY=0
13102 FOR I=1 TO 30:A$=INKEY$:NEXT                              'Key debouncing
13110 A$=INPUT$(1)
13120 IF A$>"0" AND ASC(A$)<48+FKEYMAX+1 THEN NKEY=VAL(A$):PRINT CURSON$:RETURN
13121 'IF A$>="q" AND A$<"u" THEN FKEY=ASC(A$)-240:PRINT CURSON$:RETURN
13130 IF A$>CHR$(240) AND A$<CHR$(245) THEN FKEY=ASC(A$)-240:PRINT CURSON$:RETURN
13140 N$="Sie haben eine falsche Taste gedrückt - Bitte neue Auswahl treffen.":GOSUB 12950 'Nachricht
13141 GOTO 13110
13149 '
13150 ' *** Rückkehr ins Hauptmenü ? ******************************************
13151 '
13160 Q$="Sie wollen wirklich ins Hauptmenü?":GOSUB 12520        'Ja/Nein-Frage
13170 IF JA AND MENUT$="STAMMSATZ ÄNDERN" THEN GOSUB 13490
13180 IF JA=1 THEN CLOSE:RUN "DISKMENU.EXE" ELSE PRINT HOME$:PRINT FNCUR$(21,1);DELLINE$:GOSUB 13030:RETURN
13189 '
13190 ' *** Rückkehr aus HELP-Text ? ******************************************
13191 '
13200 N$="Zurück ? (J=Ja)":GOSUB 12950:GOSUB 12530:IF JA=1 THEN RETURN ELSE 13200 'Nachricht 'TODO why not 12520?
13209 '
13210 ' *** Leerlesen von DAT$() (Ersetzt ERASE-Befehl) ***********************
13211 '
13220 FOR II=1 TO 27
13221   DAT$(II)=""
13222 NEXT
13223 RETURN
13229 '
13230 ' *** Disk I/O Error Trap Nachricht *************************************
13231 '
13240 PRINT CLS$
13241 PRINT FNCUR$(13,1);STRICH$
13250 PRINT
13260 PRINT
13270 GOSUB 12980                                                  'Basis Maske
13271 IF ERR<50 THEN PRINT FNCUR$(5,1);"Programmfehler."
13280 PRINT "   14*********************"
13290 PRINT
13300 PRINT "   15************************************************************"
13310 PRINT "   16*************************************************"
13320 PRINT "   
13330 PRINT "   17**********************"
13340 PRINT
13350 PRINT "         18********************************************"
13360 PRINT
13370 PRINT "         19**************************"
13380 PRINT
13390 PRINT "                                      20***************"
13400 PRINT
13410 PRINT STRICH$;
13411 PRINT CURSON$
13420 PRINT "ERL (Error Line) :";ERL                                'LW - Added
13430 PRINT "ERR (Error Code) :";ERR                                'LW - Added
13440 RESET
13450 'LW GOTO 13450                                       'LW - Endlosschleife
13460 END
13471 '
13480 ' *** UPRO FÜR ÄNDERN-EXIT **********************************************
13481 '
13490 Q$="Wollen Sie die Daten abspeichern?":GOSUB 12520         'Ja/Nein-Frage
13500 IF JA THEN GOSUB 12430
13510 RETURN
13511 '
13520 ' **** VN - Subroutines, permitting one page handling *******************
13530 ' ****         by 04********** 21*******              *******************
13531 '
13532 '
13540 ' **** OPEN & FIELDING VERWENDUNGSNACHWEIS ******************************
13541 '
13550 OPEN "R",#3,"B:VN.NDX",4
13560 FIELD #3,4 AS POINT0$
13570 OPEN "R",#4,"B:VN.POI",8
13580 FIELD #4,4 AS POINT1$, 4 AS POINT2$
13590 OPEN "R",#5,"B:VN.DAT",560
13600 FIELD #5,240 AS DDD1$,240 AS DDD2$,80 AS DDD3$
13610 RETURN
13611 '
13620 ' *** VERWENDUNGSNACHWEIS FÜR NEUES MITGLIED ANLEGEN ********************
13621 '
13630 GET #3,1                            'letzte Seitennummer einlesen dann +1
13631 TMPOINT=VAL(POINT0$)+1
13640 LSET DDD1$=""                                    'leeren Record speichern
13641 LSET DDD2$=""
13642 LSET DDD3$=""
13643 PUT #5,TMPOINT
13650 LSET POINT0$=STR$(TMPOINT)             'letzte Seitennummer neu speichern
13651 PUT #3,1
13660 PUT #3,MTGLZAHL+1
13670 LSET POINT1$="0000"
13671 LSET POINT2$="0000"
13672 PUT #4,TMPOINT
13680 RETURN
13681 '
13690 ' **** ERSTE SEITE FINDEN & LESEN ***************************************
13691 '
13692 ' IN: MTGLZAHL
13700 GET #3,MTGLZAHL+1                                            'Index_Datei
13701 CPOINT=VAL(POINT0$)
13710 GET #5,CPOINT:GOSUB 14390                                       'Einlesen
13711 BLATT=1
13720 GET #4,CPOINT                                              'Pointer lesen
13730 RETURN
13731 '
13740 ' **** SEITE RÜCKWÄRTS **************************************************
13741 '
13750 TMPOINT=VAL(POINT1$)
13760 IF TMPOINT<>0 THEN 13770
13761 N$="Erste Seite":GOSUB 12950                               'Nachricht
13762 FOR KK=1 TO 9999:NEXT                                              'Delay
13763 RETURN
13770 CPOINT=TMPOINT
13771 BLATT=BLATT-1
13780 GET #5,CPOINT:GOSUB 14390   
13790 GET #4,CPOINT:GOSUB 14500                             'Neue Pointer lesen
13800 HP=1:VP=1
13801 GOSUB 13880
13802 FOR KK=1 TO 100:NEXT                                               'Delay
13810 RETURN
13811 '
13820 ' **** SEITE VORWÄRTS  **************************************************
13821 '
13830 TMPOINT=VAL(POINT2$)
13840 IF TMPOINT=0 THEN 13850
13841 CPOINT=TMPOINT
13842 BLATT=BLATT+1
13843 GET #5,CPOINT:GOSUB 14390
13844 GET #4,CPOINT:GOSUB 14500
13846 HP=1:VP=1
13847 RETURN
13850 N$="Letzte Seite"
13860 IF INSTR(SPACE$(20),DAT$(26))=0 THEN 13868 
13862 GOSUB 12950                                                    'Nachricht
13864 FOR KK=1 TO 20000:NEXT                                             'Delay
13866 RETURN
13868 Q$="Letzte Seite - Neue Seite anlegen?":GOSUB 12520        'Ja/Nein-Frage
13869 N$="":GOSUB 12950                                            'Nachricht
13870 IF JA=0 THEN 13880
13872 GOSUB 14060
13873 BLATT=BLATT+1
13874 GOSUB 14500
13875 HP=1:VP=1
13880 GOSUB 13030
13882 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=vorwärts";
13890 PRINT TAB(54);"<F9>=Blatt drucken";
13891 PRINT HOME$;" "
13900 RETURN
13901 '
13910 ' **** ERSTE SEITE EINES MITGLIEDS ANLEGEN ******************************
13911 '
13920 GET #3,1
13930 LSET POINT0$=STR$(VAL(POINT0$)+1)                                  'NDX+1
13940 PUT #3,1
13950 LSET POINT1$="0000"                                        'Page pointers
13960 LSET POINT2$="0000"
13970 PUT #4,VAL(POINT0$)
13980 LSET POINT0$=POINT0$                                          'First page
13990 PUT #3,MTGLZAHL+1
14000 LSET DDD1$=""                                                 'Datenblatt
14010 LSET DDD2$=""
14020 LSET DDD3$=""
14030 PUT #5,VAL(POINT0$)
14040 RETURN
14041 '
14050 ' **** ALLGEMEIN EINE NEUE SEITE ANLEGEN ********************************
14051 '
14060 GET #3,1                                                           'NDX+1
14070 LSET POINT0$=STR$(VAL(POINT0$)+1)
14080 PUT #3,1:TMPOINT$=POINT0$    'Vektoren der bisher letzten Seite verbiegen
14100 LSET POINT2$=POINT0$                                'Verbiege Vektoren...
14110 PUT #4,CPOINT                                         '...der alten Seite
14120 LSET POINT1$=STR$(CPOINT)                             '...der neuen Seite
14130 LSET POINT2$="0000"                                                '.. ""
14140 CPOINT=VAL(TMPOINT$)                                       'Nr neue Seite
14150 PUT #4,CPOINT
14160 LSET DDD1$=""                                           'Write empty page
14170 LSET DDD2$=""
14180 LSET DDD3$="" 
14190 PUT #5,CPOINT
14200 DATEN$=SPACE$(20)
14201 FOR II=1 TO 26
14202   DAT$(II)=DATEN$
14203 NEXT
14210 RETURN
14211 '
14220 ' **** SEITE ABSPEICHERN ************************************************
14221 '
14230 DATEN$=""
14240 FOR II=1 TO 12
14250   DATEN$=DATEN$+DAT$(II)
14260 NEXT
14270 LSET DDD1$=DATEN$:DATEN$=""
14280 FOR II=13 TO 24
14290   DATEN$=DATEN$+DAT$(II)
14300 NEXT
14310 LSET DDD2$=DATEN$:DATEN$=""
14320 FOR II=25 TO 26
14330   DATEN$=DATEN$+DAT$(II)
14340 NEXT 
14350 LSET DDD3$=DATEN$:DATEN$=""
14360 PUT #5,CPOINT
14370 RETURN
14371 '
14380 ' **** HACK ! (ZERSTÜCKELN DER DATEN-STRINGS) ***************************
14381 '
14390 FOR II=1 TO 12
14400   DAT$(II)=MID$(DDD1$,(II-1)*20+1,20)
14410 NEXT
14420 FOR II=13 TO 24
14430   DAT$(II)=MID$(DDD2$,(II-13)*20+1,20)
14440 NEXT 
14450 FOR II=25 TO 26
14460   DAT$(II)=MID$(DDD3$,(II-25)*20+1,20)
14470 NEXT 
14480 RETURN
14481 '
14490 ' **** UPRO DATENAUSGABE VN  ********************************************
14491 '
14492 ' IN: MTGLZAHL
14493 ' IN: NAMM$
14494 ' IN: BLATT
14500 PRINT DELHIGH$;HIGHON$
14520 PRINT FNCUR$(5,16);MTGLZAHL
14530 PRINT FNCUR$(5,29);NAMM$
14540 PRINT FNCUR$(3,8);BLATT
14550 FOR K=1 TO 26
14560   HOR=6-39*(K>13)
14570   VER=6+K+13*(K>13)
14580   FOR J=1 TO 3
14590     PRINT FNCUR$(VER,HOR+(J-1)*3);MID$(DAT$(K),-1+J*2,2)
14600   NEXT
14610   FOR J=1 TO 3
14620     PRINT FNCUR$(VER,HOR+13+(J-1)*3);MID$(DAT$(K),5+J*2,2)
14630   NEXT
14640   PRINT FNCUR$(VER,HOR+26);MID$(DAT$(K),13,8)
14650 NEXT
14660 PRINT HIGHOFF$
14670 RETURN
14671 '
14680 ' *** vn drucken ********************************************************
14681 '
14682 ' IN: BLATT
14683 ' IN: MITGLZAHL
14684 ' IN: NAMM$
14690 Q$="Ist der Drucker an?":GOSUB 12520                       'Ja/Nein-Frage
14700 IF JA=0 THEN 14890
14710 LPRINT STRICH$
14720 LPRINT
14730 LPRINT TAB(2);"Blatt:";BLATT
14740 LPRINT
14750 LPRINT TAB(2);"Mitglieds-Nr.:";MTGLZAHL;TAB(24);"Name:";NAMM$
14760 LPRINT
14770 FOR II=1 TO 13
14780   LOZ=II:GOSUB 14860
14790   LPRINT TAB(2);"Von:";LODAT1$;" Bis:";LODAT2$;" Amt:";LODAT3$;
14800   LOZ=II+13:GOSUB 14860
14810   LPRINT " Von:";LODAT1$;" Bis:";LODAT2$;" Amt:";LODAT3$
14820 NEXT 
14830 LPRINT
14840 LPRINT STRICH$
14850 RETURN
14851 '
14860 LODAT1$=LEFT$(DAT$(LOZ),2)+"."+MID$(DAT$(LOZ),3,2)+"."+MID$(DAT$(LOZ),5,2)
14870 LODAT2$=MID$(DAT$(LOZ),7,2)+"."+MID$(DAT$(LOZ),9,2)+"."+MID$(DAT$(LOZ),11,2)
14880 LODAT3$=RIGHT$(DAT$(LOZ),8)
14890 RETURN
14891 '
14900 ' *** upros MITGLIEDSLISTE DRUCKEN **************************************
14905 '
14910 ' *** MITGLIED DRUCKEN **************************************************
14915 '
14916 ' IN: MTGLZAHL
14920 LPRINT
14930 LPRINT TAB(5);MTGLZAHL;
14940 LPRINT TAB(10);DAT$(1);                                        'Nachnamen
14950 LPRINT TAB(36);DAT$( 2);                                        'Vornamen
14960 LPRINT TAB(65);"(";LEFT$(DAT$( 5),1);")";                         'Status
14970 AMT$=""
14980 IN=INSTR(DAT$(6),"22**"):IF IN<>0 THEN AMT$=MID$(DAT$(6),IN,6):GOTO 15010
14990 IN=INSTR(DAT$(6),"23**"):IF IN<>0 THEN AMT$=MID$(DAT$(6),IN,5):GOTO 15010
15000 IN=INSTR(DAT$(6),"24**"):IF IN<>0 THEN AMT$=MID$(DAT$(6),IN,3)
15010 LPRINT TAB(71);AMT$
15020 LPRINT TAB(36);DAT$(10)                                           'Straße
15030 LPRINT TAB(36);DAT$(11);                                    'Postleitzahl
15040 LPRINT TAB(41);DAT$(12)                                              'Ort
15050 LPRINT TAB(36);"Tel.:";DAT$(13)                                  'Telefon
15060 LPRINT
15070 IF INKEY$="" THEN 15080
15072 Q$="Drucken abbrechen ?":GOSUB 12520                       'Ja/Nein-Frage
15074 IF JA=1 THEN BRKFLAG=-1 ELSE BRKFLAG=0
15080 RETURN
15081 '
15090 ' *** KOPF DER MITGLIEDSLISTE *******************************************
15091 '
15092 ' IN: DATUM$
15100 LPRINT TAB(16);"MITGLIEDSLISTE 25*******************************"
15110 LPRINT 
15120 LPRINT TAB(32);"Stand:";DATUM$
15130 LPRINT
15140 LPRINT TAB(3);"Mitgl."; 
15141 LPRINT TAB(10);"Name";
15142 LPRINT TAB(36);"Vorname, Anschrift";
15143 LPRINT TAB(63);"Status";
15144 LPRINT TAB(71);"Amt"
15150 RETURN
15151 '
15160 ' *** UPRO ADRESSAUFKLEBER DRUCKEN **************************************
15161 '
15170 N$="Ist der Drucker bereit und positioniert? (J/N)":GOSUB 12950 'Nachricht
15180 GOSUB 12530                                           'TODO Why not 12520
15190 IF JA=0 THEN RETURN
15200 LPRINT "   ";DAT$(7)
15210 SPAC1=INSTR(DAT$(2)," ")
15220 LPRINT "   ";LEFT$(DAT$(2),SPAC1);DAT$(1)
15230 LPRINT
15240 LPRINT "   ";DAT$(10)
15250 LPRINT
15260 LPRINT "   ";DAT$(11);" "DAT$(12)
15270 LPRINT
15271 LPRINT
15272 LPRINT
15280 LPRINT
15281 LPRINT
15282 LPRINT
15290 RETURN
15291 '
15300 ' *** Stammsatz ausdrucken **********************************************
15301 '
15310 RESTORE 15420
15320 LPRINT STRICH$
15330 LPRINT
15340 LPRINT TAB(30);"Mitgliedsnummer  ";MTGLZAHL
15350 LPRINT
15360 LPRINT TAB(2);LEFT$(FELD$(1),14);DAT$(1);TAB(41);"Beruf:"
15370 FOR I=1 TO 6:READ A,B
15380   LPRINT TAB(2);LEFT$(FELD$(A),14);DAT$(A);
15390   IF B=0 THEN LPRINT:GOTO 15410
15400   LPRINT TAB(41);LEFT$(FELD$(B),14);DAT$(B);
15410 NEXT
15420 DATA 2,15,3,16,4,0,5,17,6,18,0,19
15430 FOR I=7 TO 14
15440   LPRINT TAB(2);LEFT$(FELD$(I),14);DAT$(I);
15441   LPRINT TAB(41);LEFT$(FELD$(I+13),14);DAT$(I+13);
15450 NEXT
15460 LPRINT
15461 LPRINT
15462 LPRINT
15463 LPRINT STRICH$
15470 RETURN
15471 '
15480 ' *** Mitgliedsvermerke *************************************************
15481 '
15482 ' IN: NAMM$ =
15484 SPC$=STRING$(9," "):LIN$=" ___ / ___ / ___" 
15490 LPRINT TAB(10);"Mitgliedsnr.:";TAB(28);"Name:";NAMM$
15500 LPRINT TAB(65);"Datum:"
15505 LPRINT
15520 LPRINT SPC$;"26***************************";LIN$
15525 LRPINT
15530 LPRINT SPC$;"27***************************";LIN$
15535 LRPINT
15540 LPRINT SPC$;"28***************************";LIN$
15545 LRPINT
15550 LPRINT SPC$;"29***************************";LIN$
15555 LRPINT
15560 LPRINT SPC$;"30***************************";LEFT$(LIN$,10)
15565 LRPINT
15570 LPRINT SPC$;"31***************************";LIN$
15575 LRPINT
15580 LPRINT SPC$;"32***************************";LIN$
15585 LRPINT
15590 LPRINT SPC$;"33***************************";LEFT$(LIN$,4)
15595 LRPINT
15600 LPRINT SPC$;"34***************************";LIN$
15605 LRPINT
15610 LPRINT SPC$;"35***************************";LIN$
15615 LRPINT
15620 LPRINT SPC$;"36***************************";LIN$
15625 LRPINT
15630 LPRINT SPC$;"37***************************";LIN$
15635 LRPINT
15640 LPRINT SPC$;"38***************************";LIN$
15645 RETURN
