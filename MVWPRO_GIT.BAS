1000 ' RELEASE 01***** Neubearbeitung durch 04**********
1010 '
1020 ' LW 31-JAN-2026: Load MSBASIC.COM with /S:560 /F:5
1030 '
1040 ' *** Initialisierung, Dimensionierung ***********************************
1050 '
1060 DEFINT A-Z                                              'Integerdefinition
1070 DIM FELD$(27),SUCH$(27),DAT$(27)                    'Arrays dimensionieren
1080 BSP$    =CHR$(8)                                                'Backspace
1090 ESC$    =CHR$(27)                                'Einleitung Escapesequenz
1100 CLS$    =ESC$+"E"                                      'Bildschirm löschen
1110 INVON$  =ESC$+"p"                                              'Invers ein
1120 INVOFF$ =ESC$+"q"                                              'Invers aus
1130 CURSON$ =ESC$+"y5"                                             'Cursor ein
1140 CURSOFF$=ESC$+"x5"                                             'Cursor aus
1150 HIGHON$ =ESC$+"("                                           'Helle Schrift
1160 HIGHOFF$=ESC$+")"                                          'Dunkle Schrift
1170 DELLINE$=ESC$+"K"                                           'Zeile löschen
1180 DELHIGH$=ESC$+"+"                                   'Helle Schrift löschen
1190 STRICH$ =STRING$(79,"-")                                           'Strich
1200 DEF FNCUR$(Z,S)=ESC$+"Y"+CHR$(31+Z)+CHR$(31+S)       'Cursor positionieren
1210 DEF FNROW(F$)=VAL(MID$(F$,17,2))                     'Zeile des Feldnamens
1220 DEF FNCOL(F$)=VAL(MID$(F$,15,2))                    'Spalte des Feldnamens
1230 DEF FNLNG(F$)=VAL(MID$(F$,19,2))                         'Länge des Feldes
1240 DEF FNTYP(F$)=VAL(RIGHT$(F$,1))                     'Gültigkeits-Test Flag
1250 HOME$=FNCUR$(3,1)                                    'Cursor Home Position
1260 ' 
1270 ON ERROR GOTO 14550
1280 '
1290 ' Tasten deaktivieren
1295 RESTORE 1370
1300 READ KEY
1310 IF KEY<0 THEN 1400
1320 ' Map unshifted logical key code KEY to ASCII code 0
1330   PRINT ESC$;"41";CHR$(KEY);CHR$(0);
1340 ' Map shifted logical key code KEY to ASCII code 0
1350   PRINT ESC$;"42";CHR$(KEY);CHR$(0);
1360 GOTO 1300
1370 DATA 11,26,27,32,33,47,48:      'ESC, CLR, DEL CHR, INT, TAB, INS, DEL/EOL
1380 DATA 53,67,68,73,94,97,-1:      'RVS,SCROL, LTRL, UNDL, RPT, CONT, Endflag
1390 ' Map unshifted logical cursor key codes to ASCII codes
1400 PRINT ESC$;"41";CHR$(88);CHR$(200);    'Key 88 "Cursor up"    -> ASCII 200
1410 PRINT ESC$;"41";CHR$(89);CHR$(201);    'Key 89 "Cursor down"  -> ASCII 201
1420 PRINT ESC$;"41";CHR$(98);CHR$(202);    'Key 98 "Cursor right" -> ASCII 202
1430 PRINT ESC$;"41";CHR$(99);CHR$(203);    'Key 99 "Cursor left"  -> ASCII 203
1440 '
1450 ' *** Beginn des Mitgliederverwaltungsprogramms **************************
1460 '
1470 MT$="MITGLIEDERVERWALTUNG":MI$="G-01000":GOSUB 13940          'Basis Maske
1480 Q$="Befindet sich die Daten-Diskette im Laufwerk B?":GOSUB 13190 'Ja/Nein-Frage
1490 IF JA=1 THEN 1590
1500 Q$="Bitte legen Sie sie ein. Fertig?":GOSUB 13190           'Ja/Nein-Frage
1510 IF JA=0 THEN RUN "DISKMENU.EXE"
1520 '
1530 ' ************************************************************************
1540 ' *                                                                      *
1550 ' *                 Mitgliederverwaltungsmenü (MVW-Menü)                 *
1560 ' *                                                                      *
1570 ' ************************************************************************
1580 '
1590 GOSUB 12890                                   'Fielden der Stammsatz-Datei
1600 GOSUB 14940                       'Fielden der Verwendungsnachweis-Dateien
1610 '
1620 ' Einlesen Stammdatenfelder
1630 OPEN "I",#1,"B:FELD.DAT"                        
1640 INPUT #1,FELDMAX
1650 FOR I=1 TO FELDMAX
1660   INPUT #1,FELD$(I)
1670 NEXT
1680 CLOSE #1
1690 '
1700 MT$="MITGLIEDERVERWALTUNG":MI$="G-01000":GOSUB 13940          'Basis Maske
1710 FKEYS$="":GOSUB 14060                                      'F-Tastenleiste
1720 VNFLAG=0                                                              '???
1730 PRINT FNCUR$(4,26);"1 - Stammsatzpflege"                         'Menütext
1740 'TODO XOX Obsolete PRINT
1750 PRINT FNCUR$(6,26);"2 - Verwendungsnachweis"
1760 'TODO XOX Obsolete PRINT
1770 PRINT FNCUR$(8,26);"3 - Mitgliedsliste drucken"
1780 NKEYMAX=3:GOSUB 14230                                         'Menüeingabe
1790 DRUCKFLAG=0                                                           '???
1800 IF NKEY THEN ON NKEY GOTO 1930,4680,5560
1810 ON FKEY GOTO 1820,1830,1700,1830         'Hilfe, Zurück, Anfang, Hauptmenü
1820 GOSUB 10800:GOTO 1700                                       'F1->Help Text
1830 GOSUB 14350:GOTO 1700                           'F2->Zurück, F4->Hauptmenü
1840 '
1850 DEL$=BSP$+"."+BSP$                 'Zeichen links löschen 'TODO ever used?
1860 '
1870 ' ************************************************************************
1880 ' *                                                                      *
1890 ' *                           Stammsatzpflege                            *
1900 ' *                                                                      *
1910 ' ************************************************************************
1920 '
1930 GOSUB 10400                                              'Einlesen MTGLMAX
1940 IF DRUCKFLAG THEN RETURN                                              '???
1950 '
1960 ' *** Stammsatzpflege-Menü ***********************************************
1970 '
1980 MT$="STAMMSATZPFLEGE":MI$="G-01100":GOSUB 13940               'Basis Maske
1990 FKEYS$="":GOSUB 14060                                      'F-Tastenleiste
2000 PRINT FNCUR$(4,29);"1 - Stammsatz anlegen"            'TODO LINE 4    'Menütext
2010 ' PRINT
2020 PRINT FNCUR$(6,29);"2 - Stammsatz ändern"
2030 ' PRINT
2040 PRINT FNCUR$(8,29);"3 - Stammsatz löschen"
2050 ' PRINT
2060 PRINT FNCUR$(10,29);"4 - Stammsatz drucken"
2070 ' PRINT
2080 PRINT FNCUR$(12,29);"5 - Stammsatz suchen"
2090 NKEYMAX=5:GOSUB 14230                                         'Menüeingabe
2100 IF NKEY THEN GOSUB 10310:ON NKEY GOTO 2180,2450,2920,4370,3210 'Variablen Init: Read MTGLZAHL, clear DAT$(), SUCH$()
2110 ON FKEY GOTO 2120,2130,2130,2140         'Hilfe, Zurück, Anfang, Hauptmenü
2120 GOSUB 11020:GOTO 1980                                       'F1->Help-Text
2130 GOSUB 14480:GOTO 1700                      'F2,F3->Clear DAT$(),->MVW-Menü  
2140 GOSUB 14350:GOTO 1980                                       'F4->Hauptmenü
2150 '
2160 ' *** Stammsatz erfassen von 02************ ******************************
2170 '
2180 MT$="STAMMSATZ ANLEGEN":MI$="G-01101":PRINT CURSON$:DATENAUSG=1:F=1
2190 MTGLZAHL=MTGLMAX
2200 MTGLZAHL=MTGLZAHL+1:MTGL$=STR$(MTGLZAHL)
2210 GOSUB 13940                                                   'Basis Maske
2220 FKEYS$="<F7>=Abspeichern":GOSUB 14060                        'F-Tastenleiste
2230 PRINT FNCUR$(3,30);"Mitgliedsnummer";FNCUR$(5,41);"Beruf:":GOSUB 10480  'Feldmaske
2240 GOSUB 13730                                                      'Defaults
2250 IF DATENAUSG=1 THEN GOSUB 10690:DATENAUSG=0             'wenn vorher HILFE
2260 PRINT FNCUR$(21,1);DELLINE$                               'Cursorparameter
2270 PRINT HIGHON$;CURSON$;FNCUR$(3,46);MTGL$
2280 Z=FNROW(FELD$(F)):S=FNCOL(FELD$(F))+14
2290 LNG=FNLNG(FELD$(F)):TYP=FNTYP(FELD$(F))
2300 PRINT FNCUR$(21,1);DELLINE$
2310 GOSUB 10090                                                         'INPUT
2320 IF AASC=13 THEN IF MUSS$="=" THEN IF LEN(ANT$)<1 THEN N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 13860:GOTO 2310 'Nachricht
2330 IF AASC=13 THEN DAT$(F)=ANT$                                       'Return
2340 IF AASC=13 THEN IF F<FELDMAX THEN F=F+1:GOTO 2280 ELSE N$="Letztes Feld":GOSUB 13860:GOTO 2280          'ELSE zu IF F< 'Nachricht
2350 IF HOCH<>0 THEN PRINT FNCUR$(Z,S);DAT$(F)+STRING$(LNG-LEN(DAT$(F)),"."):F=F+(HOCH=-1)*(-(F>1)+(F=1)*25)+(HOCH=1)*((F<27)-(F=27)*26):GOTO 2280
2360 IF ZURUCK=1 THEN GOSUB 11250:GOTO 2210                             'F-Keys
2370 IF ZURUCK=3 THEN GOSUB 13620:PRINT HIGHOFF$:GOSUB 14480:GOTO 1700
2380 IF ZURUCK=2 THEN GOSUB 13620:PRINT HIGHOFF$:GOTO 1980
2390 IF ZURUCK=4 THEN GOSUB 13620:PRINT HIGHOFF$:GOSUB 14350:GOTO 2310
2400 IF ZURUCK=7 THEN GOSUB 13050:GOSUB 10590:F=1:MTGLZAHL=MTGLZAHL+1:MTGL$=STR$(MTGLZAHL):GOSUB 14480:GOSUB 13730:GOSUB 10690:GOTO 2270
2410 N$="Ungültige Eingabe - Bitte wiederholen":GOSUB 13860          'Nachricht
2411 GOSUB 10110                                       'INPUT (2 Zeilen später)
2412 GOTO 2320                                                      'Auswertung 
2420 ' 
2430 ' *** Stammsatz ändern von 03************ ********************************
2440 '
2450 MT$="STAMMSATZ ÄNDERN":MI$="G-01102":GOSUB 13940              'Basis Maske
2460 F=1:PRINT CURSON$
2470 FKEYS$="":GOSUB 14060                                      'F-Tastenleiste
2480 PRINT HIGHOFF$
2490 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer"
2500 PRINT FNCUR$(5,23)"des zu verändernden Stammsatzes ein,"
2510 PRINT FNCUR$(6,22)"und beenden Sie die Eingabe mit RETURN"
2520 PRINT FNCUR$(8,37)"=>..."
2530 LNG=3:TYP=2:Z=8:S=39
2540 GOSUB 10090                                                         'INPUT
2550 IF ZURUCK=1 THEN GOSUB 11480:GOTO 2450                             'F-Keys
2560 IF ZURUCK=2 THEN 1980
2570 IF ZURUCK=3 THEN GOSUB 14480:GOTO 1700
2580 IF ZURUCK=4 THEN GOSUB 14350
2590 IF HOCH<>0 OR ZURUCK<>0 THEN GOTO 2530                             'Errors
2600 IF LEN(ANT$)<1 THEN N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 13860:GOTO 2530 ELSE MITGLIED=VAL(ANT$) 'Nachricht
2610 IF MITGLIED=0 THEN MITGLIED=MTGLMAX+2
2620 IF MITGLIED>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 13860:PRINT HIGHOFF$;FNCUR$(8,39);"...";DELHIGH$:ANT$="":GOTO 2540 'Nachricht
2630 MTGLZAHL=MITGLIED:MTGL$=STR$(MITGLIED)
2640 FOR K=4 TO 8:PRINT FNCUR$(K,1);DELLINE$:NEXT:PRINT FNCUR$(21,1);DELLINE$
2650 GOSUB 12950                                                   'Daten lesen
2660 PRINT HIGHOFF$;FNCUR$(3,30);"Mitgliedsnummer";FNCUR$(5,41);"Beruf:"
2670 GOSUB 10480
2680 GOSUB 10690
2690 PRINT FNCUR$(3,46);MTGLZAHL
2700 FKEYS$="<F7>=Abspeichern":GOSUB 14060                      'F-Tastenleiste
2710 S=FNCOL(FELD$(F))+14:Z=FNROW(FELD$(F)):N$="":GOSUB 13860        'Nachricht
2720 LNG=FNLNG(FELD$(F)):TYP=FNTYP(FELD$(F))
2730 GOSUB 10090                                                         'INPUT
2740 IF ZURUCK=1 THEN GOSUB 11600:GOTO 2660                 'Hilfe       F-Keys
2750 IF ZURUCK=2 THEN GOSUB 14840: GOTO 2450                   'Eing.-Mtgl.-Nr.
2760 IF ZURUCK=3 THEN GOSUB 14840:GOSUB 14480: GOTO 1700              'Mvw-Menü
2770 IF ZURUCK=4 THEN GOSUB 14350:PRINT HIGHOFF$;FNCUR$(24,1);"<F7>=Abspeichern";FNCUR$(1,1);HIGHON$:GOTO 2730
2780 IF ZURUCK=7 THEN 2850                                     'Daten Speichern
2790 IF HOCH=-1 THEN PRINT FNCUR$(Z,S);DAT$(F);STRING$(LNG-LEN(DAT$(F)),"."):IF F>1  THEN F=F-1:GOTO 2710  ELSE F=26:GOTO 2710 'Cursor hoch
2800 IF HOCH=1  THEN PRINT FNCUR$(Z,S);DAT$(F);STRING$(LNG-LEN(DAT$(F)),"."):IF F<27 THEN F=F+1:GOTO 2710  ELSE F= 1:GOTO 2710 'Cursor runter
2810 IF AASC=13 AND LEN(ANT$)<1 AND MUSS$="=" THEN N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 13860:GOTO 2730 'Nachricht
2820 IF AASC=13 THEN DAT$(F)=ANT$:PRINT HIGHON$;FNCUR$(Z,S);DAT$(F);STRING$(LNG-LEN(DAT$(F)),".")                    'Return
2830 IF AASC=13 THEN IF F<FELDMAX THEN F=F+1:GOTO 2710 ELSE F=1:GOTO 2710
2840 N$="Ungültige Eingabe - Bitte wiederholen":GOSUB 13860:FOR KK=1 TO 25000:NEXT:GOTO 2710 'Nachricht
2850 GOSUB 13060                                               'Daten speichern
2860 Q$="Wollen Sie noch einen weiteren Datensatz ändern?":GOSUB 13190 'Ja/Nein-Frage
2870 IF JA=1 THEN 2450
2880 GOTO 1980                                                    'Zum STS-Menü
2890 '
2900 ' *** Stammsatz löschen von 04********** *********************************
2910 '
2920 MT$="STAMMSATZ LÖSCHEN":MI$="G-01103":GOSUB 13940             'Basis Maske
2930 FKEYS$="":GOSUB 14060                                      'F-Tastenleiste
2940 PRINT FNCUR$(4,23);"Bitte geben Sie die Mitgliedsnummer"         'Menütext
2950 PRINT FNCUR$(5,25);"des zu löschenden Mitglieds ein."
2960 PRINT FNCUR$(6,15);"(Dabei wird nur der Status des Mitglieds verändert.)"
2970 PRINT FNCUR$(7,23);"Beenden Sie die Eingabe mit RETURN"
2980 PRINT FNCUR$(9,37);"=>..."
2990 LNG=3:TYP=2:Z=9:S=39
3000 PRINT HIGHON$:GOSUB 10100:PRINT HIGHOFF$                            'INPUT
3010 IF ZURUCK=2 THEN 1980                                              'F-Keys
3020 IF ZURUCK=3 THEN GOSUB 14480: GOTO 1700
3030 IF ZURUCK=1 THEN GOSUB 11820:GOTO 2920
3040 IF ZURUCK=4 THEN GOSUB 14350:GOTO 2940
3050 IF ZURUCK>4 THEN HOCH=1
3060 IF HOCH<>0 THEN N$="Ungültige Eingabe - Bitte wiederholen":GOSUB 13860:GOTO 2980 'Nachricht
3070 MTGLZAHL=VAL(ANT$):IF MTGLZAHL=0 THEN MTGLZAHL=MTGLMAX+1           'Errors
3080 IF MTGLZAHL>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 13860:GOTO 2980 'Nachricht
3090 GOSUB 12950                                                'Daten einlesen
3100 GOSUB 13940                                                   'Basis Maske
3110 PRINT FNCUR$(3,30);"Mitgliedsnummer";FNCUR$(3,46);MTGLZAHL;FNCUR$(5,41);"Beruf:":GOSUB 10480:GOSUB 10690 'Feldmaske
3120 Q$="Endgültig löschen?":GOSUB 13190                         'Ja/Nein-Frage 
3130 IF JA=0 THEN 3160                                               'Löschen ?
3140 DAT$(5)="ausgetreten     ":PRINT FNCUR$(9,16);HIGHON$;DAT$(5);HIGHOFF$
3150 GOSUB 13060                                             'Daten abspeichern
3160 Q$="Soll noch jemand gelöscht werden?":GOSUB 13190          'Ja/Nein-Frage
3170 IF JA=1 THEN 2920 ELSE 1980
3180 '
3190 ' *** Stammsatz suchen von 05*************** *****************************
3200 '
3210 MI$="G-01105"
3220 NUMSUCH1=0:NUMSUCH2=0:MTGL$=""
3230 PRINT HIGHOFF$
3240 IF DRUCKFLAG THEN MT$="SUCHEN & DRUCKEN" ELSE MT$="STAMMSATZ SUCHEN"
3250 GOSUB 13940                                                   'Basis Maske
3260 FKEYS$="":GOSUB 14060                                      'F-Tastenleiste
3270 PRINT CURSOFF$                        
3280 IF DRUCKFLAG THEN A$=" Drucken " ELSE A$=" Suchen "
3290 PRINT FNCUR$(6,30)"1 -";A$;"nach Nummern"
3300 PRINT FNCUR$(8,30)"2 -";A$;"nach Maske  ":GOSUB 14480
3310 NKEYMAX=2:GOSUB 14230                                         'Menüeingabe
3320 IF FKEY=1 THEN GOSUB 11970:GOTO 3230                               'F-Keys
3330 IF FKEY=2 THEN IF DRUCKFLAG THEN GOSUB 14480:GOTO 5560 ELSE 1980
3340 IF FKEY=3 THEN GOSUB 14480: GOTO 1700
3350 IF FKEY=4 THEN GOSUB 14350:GOTO 3310
3360 IF NKEY=1 THEN 3560
3370 F=1                                                     'Suchen nach Maske
3380 FOR I=1 TO 27:SUCH$(I)="":DAT$(I)="":NEXT
3390 FKEYS$="<F5>=Suchen ":GOSUB 4280
3400 IF DRUCKFLAG THEN FKEYS$="<F5>=Suchen & Drucken"
3410 GOSUB 14060                                                'F-Tastenleiste
3420 Z=FNROW(FELD$(F)):S=FNCOL(FELD$(F))+14                    'Cursorparameter
3430 LNG=FNLNG(FELD$(F)):TYP=3
3440 PRINT FNCUR$(Z,S)
3450 ANT$="":GOSUB 10090                                                 'INPUT
3460 IF ZURUCK=5 THEN MTGLZAHL=1:GOTO 3870                              'F-Keys
3470 IF ZURUCK=1 THEN GOSUB 11970:GOSUB 4280:GOSUB 10690:GOTO 3400
3480 IF ZURUCK=4 THEN GOSUB 14350:GOTO 3420
3490 IF ZURUCK=2 THEN 3210
3500 IF ZURUCK=3 THEN GOSUB 14480: GOTO 1700
3510 IF ANT$<>"" OR AASC=13 THEN SUCH$(F)=ANT$:DAT$(F)=ANT$
3520 IF HOCH=-1 THEN IF F>1 THEN F=F-1 ELSE F=26                   'Cursor hoch
3530 IF HOCH=1 OR AASC=13 THEN IF F<27 THEN F=F+1 ELSE F=1       'Cursor runter
3540 N$="":GOSUB 13860                                               'Nachricht
3550 GOTO 3420
3560 PRINT CLS$:PRINT:PRINT                                'Suchen nach Nummern
3570 Z=7:S=1:IF DRUCKFLAG THEN DRUCK$="gedruckt" ELSE DRUCK$="gesucht "
3580 TYP=2:LNG=3:PRINT CURSON$;                                       'Menütext
3590 GOSUB 13940                                                   'Basis Maske
3600 PRINT FNCUR$(4,5);"Maximale Mitgliedsnummer:";MTGLMAX
3610 FKEYS$="":GOSUB 14060                                        'F-Tastenleiste
3620 PRINT HOME$;:PRINT FNCUR$(Z,S);                    'Beginn bei Nr...
3630 PRINT HIGHOFF$;"Ab welcher Mitgliedsnummer soll ";DRUCK$;" werden      =>...":S=56
3640 GOSUB 10100                                                         'INPUT
3650 IF ZURUCK=1 THEN GOSUB 11970:GOTO 3560                             'F-Keys
3660 IF ZURUCK=2 THEN 3210
3670 IF ZURUCK=3 THEN GOSUB 14480: GOTO 1700
3680 IF ZURUCK=4 THEN GOSUB 14350:S=1:GOTO 3620
3690 IF ANT$="" THEN S=1:GOTO 3620
3700 NUMSUCH1=VAL(ANT$)
3710 IF NUMSUCH1>MTGLMAX OR NUMSUCH1<1 THEN 3840
3720 Z=9:S=1
3730 N$="":GOSUB 13860                                               'Nachricht
3740 PRINT HOME$:PRINT FNCUR$(Z,S);                       'Ende bei Nr...
3750 PRINT HIGHOFF$;"Bis zu welcher Mitgliedsnummer soll ";DRUCK$;" werden  =>...":S=56
3760 GOSUB 10100                                                         'INPUT
3770 IF ZURUCK=1 THEN GOSUB 11970:GOTO 3560                             'F-Keys
3780 IF ZURUCK=2 THEN 3210
3790 IF ZURUCK=3 THEN GOSUB 14480: GOTO 1700
3800 IF ZURUCK=4 THEN GOSUB 14350:S=1:GOTO 3740
3810 IF ANT$="" THEN S=1:GOTO 3740
3820 NUMSUCH2=VAL(ANT$)
3830 IF NUMSUCH2<=MTGLMAX AND NUMSUCH1<=NUMSUCH2 THEN 3860
3840 N$="Unlogische Eingabe - Bitte neu eingeben":GOSUB 13860 'Errors 'Nachricht
3850 Z=6:S=1:PRINT FNCUR$(9,1);DELLINE$;FNCUR$(Z,S);:Z=7:GOTO 3620
3860 MTGLZAHL=NUMSUCH1                                               'Feldmaske
3870 PRINT CURSOFF$
3880 IF DRUCKFLAG THEN GOSUB 17190
3890 CC$="  <F7>=Adressaufkleber"
3900 IF NUMSUCH1<>0 THEN FKEYS$="<F5>=Suchen rückwärts  <F6>=vorwärts"+CC$ ELSE FKEYS$="<F6>=Suchen vorwärts"+CC$
3910 GOSUB 4280
3920 PRINT FNCUR$(3,30);"Mitgliedsnummer"
3930 IF NUMSUCH1=0  AND MTGLZAHL<=MTGLMAX  THEN 4000
3940 IF NUMSUCH1<>0 AND MTGLZAHL<=NUMSUCH2 THEN 4000
3950 IF DRUCKFLAG THEN GOSUB 14480:GOTO 3210
3960 PRINT HIGHON$;CURSOFF$;:MTGLZAHL=MTGLZAHL-1
3970 N$="Daten nicht vorhanden. Mit <F2> neue Suchmöglichkeit.":GOSUB 13860 'Nachricht
3980 PRINT HIGHOFF$;
3990 GOTO 4150
4000 GOSUB 12950                                                   'Daten lesen
4010 IF NUMSUCH1<>0 THEN 4110
4020 FOR K=1 TO FELDMAX                                      'Daten durchsuchen
4030   IF LEFT$(SUCH$(K),1)<>"*" THEN 4060
4040   IF INSTR(DAT$(K),RIGHT$(SUCH$(K),LEN(SUCH$(K))-1))=0 THEN MTGLZAHL=MTGLZAHL+1:GOTO 3930
4050   GOTO 4100
4060   I=1
4070   IF MID$(SUCH$(K),I,1)="?" THEN I=I+1:GOTO 4070
4080   I=I-1
4090   IF INSTR(DAT$(K),RIGHT$(SUCH$(K),LEN(SUCH$(K))-I))<>I+1 THEN MTGLZAHL=MTGLZAHL+1:GOTO 3930
4100 NEXT
4110 GOSUB 10690                                                     'Feldmaske
4120 MTGL$=STR$(MTGLZAHL) 
4130 PRINT CURSOFF$;FNCUR$(3,46);MTGL$
4140 IF DRUCKFLAG THEN GOSUB 16960:IF BRKFLAG THEN BRKFLAG=0:N$="":GOSUB 13860:GOTO 3210 ELSE N$="":GOSUB 13860:PRINT CURSOFF$:MTGLZAHL=MTGLZAHL+1:GOTO 3930 'Nachricht 'Nachricht
4150 ZURUCK=ASC(INPUT$(1))-240
4160 N$="":GOSUB 13860                                               'Nachricht
4170 IF ZURUCK=1 THEN GOSUB 11970:GOSUB 4280:PRINT FNCUR$(3,30);"Mitgliedsnummer":GOTO 4110
4180 IF ZURUCK=2 THEN IF NUMSUCH1<>0 THEN 3560 ELSE 3370
4190 IF ZURUCK=3 THEN GOSUB 14480:GOTO 1700
4200 IF ZURUCK=7 THEN GOSUB 17320:GOSUB 14060:N$="":GOSUB 13860:GOTO 4110 'F-Tastenleiste 'Nachricht
4210 IF ZURUCK=4 THEN GOSUB 14350:GOTO 4110
4220 IF ZURUCK=5 AND NUMSUCH1<>0 THEN IF MTGLZAHL>NUMSUCH1 THEN MTGLZAHL=MTGLZAHL-1:GOTO 4000
4230 IF ZURUCK=6 THEN 4260
4240 N$="Falsche Eingabe":GOSUB 13860                                'Nachricht
4250 GOTO 4150
4260 MTGLZAHL=MTGLZAHL+1:PRINT FNCUR$(21,1);DELLINE$;HOME$;
4270 GOTO 3930
4280 GOSUB 13940                                                   'Basis Maske
4290 IF DRUCKFLAG THEN MT$="SUCHEN & DRUCKEN" ELSE MT$="STAMMSATZ SUCHEN"
4300 GOSUB 10480
4310 IF NOT DRUCKFLAG THEN GOSUB 14060                          'F-Tastenleiste
4320 PRINT FNCUR$(5,41);"BERUF:"
4330 RETURN
4340 '
4350 ' *** Stammsatz ausdrucken von 02************ & 04********** *************
4360 '
4370 MT$="STAMMSATZ AUSDRUCKEN":MI$="G-01104":GOSUB 13940          'Basis Maske
4380 FKEYS$="":GOSUB 14060                                      'F-Tastenleiste
4390 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer"
4400 PRINT FNCUR$(5,21)"des auszudruckenden Stammsatzes ein."
4410 PRINT FNCUR$(6,23)"Beenden Sie die Eingabe mit RETURN"
4420 PRINT FNCUR$(8,37);"=>..."
4430 LNG=3:TYP=2:Z=8:S=39
4440 PRINT HIGHON$:GOSUB 10100:PRINT HIGHOFF$                            'INPUT
4450 IF ZURUCK=1 THEN GOSUB 12220:GOTO 4370                             'F-Keys
4460 IF ZURUCK=2 THEN 1980
4470 IF ZURUCK=3 THEN PRINT HIGHOFF$:GOSUB 14480: GOTO 1700
4480 IF ZURUCK=4 THEN GOSUB 14350:GOTO 4420
4490 IF ZURUCK>4 THEN HOCH=1
4500 IF HOCH<>0 THEN 4420
4510 MTGLZAHL=VAL(ANT$):IF MTGLZAHL=0 THEN MTGLZAHL=MTGLMAX+1           'Errors
4520 IF MTGLZAHL>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 13860:GOTO 4420 'Nachricht
4530 GOSUB 12950                                                   'Daten lesen
4540 GOSUB 13940                                                   'Basis Maske
4550 PRINT FNCUR$(3,30);"Mitgliedsnummer";HIGHON$;FNCUR$(3,46);MTGLZAHL;HIGHOFF$;FNCUR$(5,41);"Beruf:":GOSUB 10480:GOSUB 10690
4560 Q$="Wollen Sie diese Daten ausdrucken lassen?":GOSUB 13190  'Ja/Nein-Frage
4570 IF JA=0 THEN 4370                                            'Ausdrucken ?
4580 Q$="Ist der Drucker bereit zur Ausgabe?":GOSUB 13190        'Ja/Nein-Frage
4590 IF JA=0 THEN N$="Bitte schalten Sie den Drucker ein.":GOSUB 13860:FOR DEL=0 TO 1000:NEXT:GOTO 4560     'Drucker bereit ? 'Nachricht
4600 GOSUB 17520:GOTO 4370                                             'Drucken
4610 '
4620 ' ************************************************************************
4630 ' *                                                                      *
4640 ' *                         Verwendungsnachweis                          *
4650 ' *                                                                      *
4660 ' ************************************************************************
4670 '
4680 GOSUB 10400                                              'Einlesen MTGLMAX
4690 DEL$=BSP$+" "+BSP$                                        'TODO ever used?
4700 VNFLAG=-1
4710 '
4720 ' Verwendungsnachweis-Menü ***********************************************
4730 '
4740 MT$="VERWENDUNGSNACHWEIS":MI$="G-01200":GOSUB 13940           'Basis Maske
4750 F=4                                                                'TODO ?
4760 FKEYS$="":GOSUB 14060                                      'F-Tastenleiste
4770 PRINT CURSON$
4780 PRINT HIGHOFF$
4790 PRINT FNCUR$(4,20)"   Bitte geben Sie die Mitgliedsnummer"
4800 PRINT FNCUR$(5,20)"des zu ändernden Verwendungsnachweises ein"
4810 PRINT FNCUR$(6,20)" und beenden Sie die Eingabe mit RETURN."
4820 PRINT FNCUR$(8,20)"                 =>..."
4830 Z=8:S=39:LNG=3:TYP=2
4840 GOSUB 10090                                                         'INPUT
4850 IF ZURUCK=1 THEN GOSUB 12350:GOTO 4740
4860 IF ZURUCK=2 OR ZURUCK=3 THEN GOSUB 14480:VNFLAG=0:GOTO 1700
4870 IF ZURUCK=4 THEN GOSUB 14350
4880 IF ZURUCK>4 THEN 4830
4890 IF HOCH<>0 THEN GOTO 4830
4900 IF LEN(ANT$)=0 THEN N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 13860:GOTO 4820 ELSE MITGLIED=VAL(ANT$):IF MITGLIED=0 THEN 4840 'Nachricht
4910 IF MITGLIED>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 13860:PRINT HOME$;HIGHOFF$:GOTO 4820 'Nachricht
4920 MTGLZAHL=MITGLIED:MTGL$=STR$(MITGLIED)
4930 '
4940 ' *** Einlesen des Mitgliedsnamens ***************************************
4950 '
4960 PRINT FNCUR$(21,1);DELLINE$;
4970 GET #2,MITGLIED                                        'EINLESEN DATEN STS
4980 NNAM$=LEFT$(DD1$,25)                                 'NACHNAME RAUSFISCHEN
4990 VNAM$=MID$(DD1$,26,25)                               'VORNAME  RAUSFISCHEN
5000 '
5010 ' ** LEERZEICHEN HINTER NACHNAME ABSCHNEIDEN *****************************
5020 '
5030 FOR I=LEN(NNAM$) TO 1 STEP -1
5040   IF MID$(NNAM$,I,1)<>" " THEN NNAM$=LEFT$(NNAM$,I):I=1
5050 NEXT
5060 NAMM$=NNAM$+" "+VNAM$
5070 GOSUB 15210                                          'EINLESEN 1. VN-BLATT
5080 BLATT=1
5090 VP=1:HP=1                                                 'INITIALISIERUNG
5100 F=8
5110 GOSUB 13940                                                   'Basis Maske
5120 FKEYS$="<F7>=Abspeichern":GOSUB 14060                      'F-Tastenleiste
5130 PRINT HIGHOFF$
5140 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=Vorwärts";TAB(54);"<F9>=Blatt drucken";
5150 PRINT HOME$
5160 PRINT HOME$;FNCUR$(5,2);"Mitglieds-Nr.:"
5170 PRINT FNCUR$(3,2); "Blatt:"
5180 PRINT FNCUR$(5,24);"Name:"
5190 MSK$="Von:  .  .   Bis:  .  .   Amt:"
5200 FOR KK=7 TO 19 
5210   PRINT FNCUR$(KK,2)MSK$;TAB(43)MSK$
5220 NEXT
5230 GOSUB 16450                                                 'Ausgabe Daten
5240 LNG=2-(HP=7)*6                                    'BEGIN OF THE INPUT-LOOP
5250 TYP=2-(HP=7)
5260 Z=VP+13*(VP>13)+6
5270 S=6-39*(VP>13)-(HP<7)*(HP-1)*3+(HP>3)*(HP<7)*4-(HP=7)*26
5280 N$="":GOSUB 13860                         'ERASE MESSAGE-LINE   'Nachricht
5290 GOSUB 10100                                                         'INPUT
5300 IF HP<7 THEN IF AASC=13 THEN ANT$=STRING$(2-LEN(ANT$),"0")+ANT$   '+NULLEN
5310 IF HOCH<>0 OR AASC=13 THEN PRINT FNCUR$(Z,S);ANT$
5320 IF AASC=13 THEN IF HP<7 THEN MID$(DAT$(VP),-1+2*HP,2)=ANT$ ELSE DAT$(VP)=LEFT$(DAT$(VP),12)+ANT$+SPACE$(8-LEN(ANT$))
5330 IF AASC=13 AND HP<7 THEN JA=1:TEST$=ANT$:ON HP GOSUB 13530,13550,13570,13530,13550,13570:IF JA=0 THEN N$="Ungültiges Datum":GOSUB 13860:GOTO 5290 'Datums-Check 'Nachricht
5340 IF AASC=13 THEN IF HP=7 THEN HP=1:VP=VP-(VP<26)-(VP=26)*(-25):GOTO 5240 ELSE HP=HP+1:GOTO 5240     'NEUE SCREEN-POSITION
5350 IF HOCH<>0 THEN TEST$=MID$(DAT$(VP),-(HP<7)*2*HP-1-(HP=7)*14,-(HP<7)*2-(HP=7)*8):PRINT FNCUR$(Z,S);TEST$
5360 IF HOCH<>0 THEN IF HP<7 AND NOT TEST$="  " THEN JA=1:ON HP GOSUB 13530,13550,13570,13530,13550,13570:IF JA=0 THEN N$="Ungültiges Datum":GOSUB 13860:GOTO 5290 'Nachricht
5370 IF HOCH=1  THEN VP=VP-(VP<26)-(VP=26)*(-25):GOTO 5240
5380 IF HOCH=-1 THEN VP=VP+(VP>1)-(VP=1)*25     :GOTO 5240
5390 IF HOCH=2  THEN HP=HP-(HP<7)-(HP=7)*(-6)   :GOTO 5240
5400 IF HOCH=-2 THEN HP=HP+(HP>1)+(HP=1)*(-6)   :GOTO 5240
5410 IF ZURUCK=1 THEN GOSUB 12470 :GOTO 5090         'HILFE - ZU INIT UND MASKE
5420 IF ZURUCK=2 THEN GOTO 4740                                'ZU MTGL-EINGABE
5430 IF ZURUCK=3 THEN VNFLAG=0:GOSUB 14480:GOTO 1700
5440 IF ZURUCK=4 THEN GOSUB 14350:GOSUB 14140:GOTO 5290 
5450 IF ZURUCK=5 THEN GOSUB 15300:GOTO 5240
5460 IF ZURUCK=6 THEN GOSUB 15460:GOTO 5240
5470 IF ZURUCK=7 THEN GOSUB 16110:GOTO 5290             'ABSPEICHERN DANN INPUT
5480 IF ZURUCK=9 THEN GOSUB 16680:GOSUB 15660:GOTO 5280
5490 '
5500 ' ************************************************************************
5510 ' *                                                                      *
5520 ' *             Mitgliedsliste drucken von 05***************             *
5530 ' *                                                                      *
5540 ' ************************************************************************
5550 '
5560 DRUCKFLAG=-1
5570 MTGL$=""
5580 DATUM$="  .  .  "
5590 ' GOSUB 1680                                               'TODO Sad hack!
5600 DEL$=BSP$+"."+BSP$
5610 GOSUB 10400                                              'Einlesen MTGLMAX
5620 GOSUB 10310          'Variablen Init: Read MTGLZAHL, clear DAT$(), SUCH$()
5630 '
5640 PRINT CLS$;
5650 PRINT HIGHOFF$;
5660 PRINT CURSOFF$;
5670 MT$="MITGLIEDSLISTE DRUCKEN":MI$="G-01300":GOSUB 13940        'Basis Maske
5680 FKEYS$="":GOSUB 14060                                      'F-Tastenleiste
5690 PRINT FNCUR$( 6,28);"1 - Suchen & Ausdrucken"
5700 PRINT FNCUR$( 8,28);"2 - Komplett ausdrucken"
5710 PRINT FNCUR$(10,28);"3 - Datenblatt drucken"
5720 NKEYMAX=3:GOSUB 14230                                         'Menüeingabe
5730 IF FKEY=1 THEN GOSUB 12670:GOTO 5640
5740 IF FKEY=2 OR FKEY=3 THEN GOSUB 14480:DRUCKFLAG=0:GOTO 1700
5750 IF FKEY=4 THEN GOSUB 14350:GOTO 5720
5760 IF NKEY=1 THEN 5820
5770 IF NKEY=2 THEN DRUCKFLAG=0:GOTO 5820
5780 IF NKEY=3 THEN 6230
5790 '
5800 ' *** Suchen & Ausdrucken, Komplett ausdrucken ***************************
5810 '
5820 GOSUB 13940                                                   'Basis Maske
5830 GOSUB 14060                                                'F-Tastenleiste
5840 PRINT FNCUR$(7,13);"Geben Sie bitte das heutige Datum ein :   .  .  ":Z=7:LNG=2:TYP=2
5850 PRINT HIGHON$;CURSON$
5860 FOR K=0 TO 2
5870   S=53+K*3
5880   VNFLAG=-1:GOSUB 10100:VNFLAG=0    'in order to activate right-left shift
5890   IF HOCH<>0 THEN JA=1:TEST$=MID$(DATUM$,K*3+1,2):ON K+1 GOSUB 13530,13550,13570:IF JA=0 THEN N$="Ungültiges Datum":GOSUB 13860:GOTO 5870 'Nachricht
5900   IF ZURUCK=1 THEN GOSUB 12670:GOTO 5820
5910   IF ZURUCK=2 THEN 5560
5920   IF ZURUCK=3 THEN GOSUB 14480:DRUCKFLAG=0:GOTO 1700
5930   IF ZURUCK=4 THEN GOSUB 14350:GOTO 5870
5940   IF HOCH=-2 THEN IF K=0 THEN 5870 ELSE K=K-1:GOTO 5870
5950   IF HOCH=+2 THEN IF K=2 THEN K=0:GOTO 5870 ELSE K=K+1:GOTO 5870
5960   IF AASC<>13 THEN 5870
5970   ANT$=STRING$(2-LEN(ANT$),"0")+ANT$:PRINT FNCUR$(Z,S);ANT$
5980   TEST$=ANT$:JA=1:ON K+1 GOSUB 13530,13550,13570:IF JA=0 THEN N$="Ungültige Datumseingabe":GOSUB 13860:GOTO 5870 'Nachricht
5990   MID$(DATUM$,K*3+1,2)=ANT$
6000 NEXT
6010 PRINT HIGHOFF$;
6020 PRINT CURSOFF$
6030 Q$="Bitte den Drucker bereit machen. Fertig?":GOSUB 13190   'Ja/Nein-Frage
6040 IF JA=0 THEN N$="":GOSUB 13860:GOTO 5820                        'Nachricht
6050 '
6060 ' *** Ausdrucken Anfang **************************************************
6070 '
6080 IF DRUCKFLAG THEN 6120
6090 PRINT FNCUR$(7,1);DELLINE$;
6100 PRINT CURSOFF$
6110 N$="Mitgliedsliste wird gedruckt. Abbrechen mit Taste.":GOSUB 13860 'Nachricht
6120 IF DRUCKFLAG THEN 3220
6130 PRINT FNCUR$(23,1);DELLINE$;
6140 GOSUB 17190
6150 FOR MTGLZAHL=1 TO MTGLMAX
6160   GOSUB 12950:IF INSTR(DAT$(5),"ausgetreten")>0 THEN 6180
6170   GOSUB 16960:IF BRKFLAG THEN BRKFLAG=0:N$="Drucken abgebrochen":GOSUB 13860:PRINT CURSOFF$:GOTO 6190 ELSE GOSUB 13860:PRINT CURSOFF$ 'Nachricht
6180 NEXT MTGLZAHL
6190 GOSUB 14480:DRUCKFLAG=0:GOTO 5560
6200 '
6210 ' *** Datenblatt drucken *************************************************
6220 '
6230 MT$="DATENBLATT DRUCKEN":MI$="G-01303":GOSUB 13940            'Basis Maske
6240 FKEYS$="":GOSUB 14060                                      'F-Tastenleiste
6250 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer"
6260 PRINT FNCUR$(5,24)"des zu druckenden Datenblattes ein."
6270 PRINT FNCUR$(7,23)"Beenden Sie die Eingabe mit RETURN"
6280 PRINT FNCUR$(9,37)"=>..."
6290 LNG=3:TYP=2:Z=9:S=39
6300 PRINT HIGHON$:GOSUB 10100:PRINT HIGHOFF$                            'INPUT
6310 IF ZURUCK=2 THEN 5560                                              'F-Keys
6320 IF ZURUCK=3 THEN GOSUB 14480: GOTO 1700
6330 IF ZURUCK=1 THEN GOSUB 12670:PRINT CURSON$:MT$="DATENBLATT DRUCKEN":GOTO 6230
6340 IF ZURUCK=4 THEN GOSUB 14350:GOTO 6230
6350 IF ZURUCK>4 THEN HOCH=1 
6360 IF HOCH<>0 THEN N$="Ungültige Eingabe - Bitte wiederholen":GOSUB 13860:GOTO 6280 'Nachricht
6370 MTGLZAHL=VAL(ANT$):IF MTGLZAHL=0 THEN MTGLZAHL=MTGLMAX+1           'Errors
6380 IF MTGLZAHL>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 13860:GOTO 6280 'Nachricht
6390 Q$="Ist der Drucker bereit zur Ausgabe?":GOSUB 13190        'Ja/Nein-Frage
6400 IF JA=0 THEN N$="Bitte schalten Sie den Drucker ein.":GOSUB 13860:FOR DEL=0 TO 1000:NEXT:GOTO 6230     'Drucker bereit ? 'Nachricht
6410 GOSUB 12950                                            'S.S.Daten einlesen
6420 NNAM$ = LEFT$(DD1$,25) : NNAM$ = LEFT$(NNAM$,INSTR(NNAM$," "))
6430 NAMM$ = NNAM$ + MID$(DD1$,26,25)
6440 GOSUB 17520                                                   'S.S.drucken
6450 GOSUB 15210                                            'V.N Daten einlesen
6460 GOSUB 16700                                             'V.N Daten drucken
6470 GOSUB 17780                                             'Mitgliedsvermerke
6480 GOTO 6230
10000 '
10010 ' ***********************************************************************
10020 ' *                                                                     *
10030 ' *        Unterprogramme von 06*****, 07****, 08**** und 09****        *
10040 ' *                                                                     *
10050 ' ***********************************************************************
10060 '
10070 ' *** Eingaberoutine (INPUT) ********************************************
10080 '
10090 MUSS$=MID$(FELD$(F),13,1)
10100 PRINT HIGHON$:ANT$="":I=1
10110 HOCH=0:ZURUCK=0:PRINT HOME$;" ":PRINT FNCUR$(Z,S+I-1);
10120 A$=INPUT$(1):AASC=ASC(A$)
10130 ON TYP GOSUB 13310,13380,13430,13480                      'Bereichs check
10140 IF DRIN=1 THEN IF I<=LNG THEN PRINT A$;:ANT$=LEFT$(ANT$,I-1)+A$:I=I+1:GOTO 10120 ELSE GOTO 10120
10150 IF AASC=8 THEN IF I>1 THEN PRINT DEL$;:I=I-1:ANT$=LEFT$(ANT$,I-1):GOTO 10110 ELSE GOTO 10120                'Delete
10160 IF VNFLAG THEN IF AASC=13 THEN PRINT HIGHOFF$;STRING$(LNG-I+1," ");HIGHON$:RETURN 'BEI VN RETURN OHNE PUNKTE
10170 IF AASC=13  THEN PRINT HIGHOFF$;STRING$(LNG-I+1,"."):RETURN       'Return
10180 IF AASC=200 THEN HOCH=-1:GOSUB 10270:RETURN                  'Cursor hoch
10190 IF AASC=201 THEN HOCH=+1:GOSUB 10270:RETURN                'Cursor runter
10200 IF VNFLAG AND AASC=202 THEN HOCH=-2:GOSUB 10270:RETURN
10210 IF VNFLAG AND AASC=203 THEN HOCH=2 :GOSUB 10270:RETURN
10220 IF AASC>240 AND AASC<250 THEN ZURUCK=AASC-240:RETURN              'F-Keys
10230 N$="Ungültige Eingabe - Bitte wiederholen":GOSUB 13860:GOTO 10110 'Nachricht
10240 '
10250 ' *** Cursor hoch/runter Repeat Blockierung *****************************
10260 '
10270 A$=INKEY$
10271 IF A$>CHR$(199) THEN 10270                    'Blocks any cursor movement
10272 RETURN
10280 '
10290 ' *** Disk I/O Variablen Initalisierung *********************************
10300 '
10310 GOSUB 10400:MTGLZAHL=MTGLMAX                            'Einlesen MTGLMAX 
10320 FOR K=1 TO FELDMAX                                 'Clear DAT$(), SUCH$()
10330   DAT$(K)=""
10340   SUCH$(K)=""
10350 NEXT K
10360 RETURN
10370 '
10380 ' *** Einlesen MTGLMAX **************************************************
10390 '
10400 OPEN "I",#1,"B:MTGLZAHL.DAT"
10410   INPUT #1,MTGLMAX$
10420   MTGLMAX=VAL(MTGLMAX$)
10430 CLOSE #1
10440 RETURN 
10450 '
10460 ' *** Maske Feldnamen und Feldwerte-Platzhalter (Pünktchen) *************
10470 '
10471 ' IN: FELDMAX
10472 ' IN: MTGLZAHL
10480 PRINT HIGHOFF$;
10481 PRINT CURSOFF$;
10490 FOR K=1 TO FELDMAX
10500   S=FNCOL(FELD$(K))
10510   Z=FNROW(FELD$(K))
10520   PRINT FNCUR$(Z,S);MID$(FELD$(K),1,14);STRING$(FNLNG(FELD$(K)),".")
10530 NEXT
10540 PRINT CURSON$;
10541 MTGL$=STR$(MTGLZAHL)                                              'TODO ?
10550 RETURN
10560 '
10570 ' *** Maske Feldwerte-Platzhalter (Pünktchen) ***************************
10580 '
10581 ' IN: FELDMAX
10590 PRINT HOME$;" "                                         'TODO Not needed?
10600 FOR K=1 TO FELDMAX
10610   S=FNCOL(FELD$(K))+14
10620   Z=FNROW(FELD$(K))
10630   PRINT FNCUR$(Z,S);STRING$(FNLNG(FELD$(K)),".")
10640 NEXT
10650 RETURN
10660 '
10670 ' *** Maske Feldwerte ***************************************************
10680 '
10681 ' IN: FELDMAX
10690 PRINT HIGHON$
10700 FOR K=1 TO FELDMAX
10710   LNG=FNLNG(FELD$(K))                                   'TODO Not needed?
10720   S=FNCOL(FELD$(K))+14
10730   Z=FNROW(FELD$(K))
10740   PRINT FNCUR$(Z,S);DAT$(K)
10750 NEXT
10760 RETURN
10770 '
10780 ' *** Help-Text "Mitgliederverwaltung" ********************************** 
10790 '
10800 MT$="HILFE":GOSUB 13940                                      'Basis Maske
10810 PRINT HOME$
10820 PRINT "       Die MITGLIEDERVERWALTUNG bietet folgende Funktionen an :"
10830 PRINT 
10840 PRINT "        1 - STAMMSATZPFLEGE"
10850 PRINT "            Bearbeiten von Mitgliedsdaten gemäß dem Personal-"
10860 PRINT "            stammblatt." 
10870 PRINT
10880 PRINT "        2 - VERWENDUNGSNACHWEIS"
10890 PRINT "            Bearbeiten von Mitgliedsdaten, die Ämter umd Amts-"
10900 PRINT "            perioden eines Mitglieds enthalten."
10910 PRINT
10920 PRINT "        3 - MITGLIEDSLISTE DRUCKEN"
10930 PRINT "            Ausdruck von Mitgliedslisten nach verschiedenen"
10940 PRINT "            Gesichtspunkten."
10950 PRINT
10960 PRINT "    Wählen Sie aus, indem Sie die zugehörige Zahlentaste drücken."
10970 GOSUB 14410                                       'Rückkehr aus Help-Text
10980 RETURN
10990 '
11000 ' *** Help-Text "Stammsatzpflege" *************************************** 
11010 '
11020 MT$="HILFE":GOSUB 13940                                      'Basis Maske
11030 PRINT HOME$
11040 PRINT "     Die STAMMSATZPFLEGE bietet folgende Funktionen an:"
11050 PRINT
11060 PRINT "      1 - STAMMSATZ ANLEGEN"
11070 PRINT "          Erfassen der Daten eines neuen Mitglieds."
11080 PRINT
11090 PRINT "      2 - STAMMSATZ ÄNDERN"
11100 PRINT "          Verändern eines bereits bestehenden Stammsatzes."
11110 PRINT
11120 PRINT "      3 - STAMMSATZ LÖSCHEN"
11130 PRINT "          Verändern des Mitgliedsstatus auf `ausgetreten`."
11140 PRINT
11150 PRINT "      4 - STAMMSATZ DRUCKEN"
11160 PRINT "          Ausdrucken eines kompletten Stammsatzes."
11170 PRINT
11180 PRINT "      5 - STAMMSATZ SUCHEN"
11190 PRINT "          Suchen vom Stammsätzen nach bestimmten Gesichtspunkten."
11200 GOSUB 14410                                       'Rückkehr aus Help-Text
11210 RETURN
11220 '
11230 ' *** Help-Text "Stammsatz anlegen" *************************************
11240 '
11250 MT$="HILFE":GOSUB 13940                                      'Basis Maske
11260 PRINT HOME$
11270 PRINT "   Sie können jetzt Daten eines neuen Mitglieds eingeben,"
11280 PRINT "   jedoch nicht die eines bestehenden Mitglieds ändern."
11290 PRINT
11300 PRINT "   Die Mitgliedsnummer wird laufend vergeben."
11310 PRINT
11320 PRINT "   Während der Dateneingebe können Sie mit der DELETE-Taste "
11330 PRINT "   korrigieren. Ist ein Feld ausgefüllt worden, muß RETURN"
11340 PRINT "   gedrückt werden, da sonst die Eingabe nicht gespeichert"
11350 PRINT "   wird. Mit den Cursortasten kann das Feld gewechselt werden."
11360 PRINT
11370 PRINT "   `=>`-Felder sollten ausgefüllt werden."
11380 PRINT
11390 PRINT "   Weitere Funktionstasten :"
11400 PRINT "   <F7>  Diesen Stammsatz abspeichern."
11410 GOSUB 14410                                       'Rückkehr aus Help-Text
11420 MT$="STAMMSATZ ANLEGEN"
11430 DATENAUSG=1
11440 RETURN
11450 '
11460 ' *** Help-Text "Stammsatz ändern (I)" **********************************
11470 '
11480 MT$="HILFE":GOSUB 13940                                      'Basis Maske
11490 PRINT HOME$
11500 PRINT "      Sie können jetzt einen bestehenden Stammsatz ändern."
11510 PRINT
11520 PRINT "      Geben Sie die Mitgliedsnummer des betreffenen Stammsatzes"
11530 PRINT "      ein. Bei Tippfehlern können Sie mit DELETE-Taste korri-"
11540 PRINT "      gieren."
11550 GOSUB 14410                                       'Rückkehr aus Help-Text
11560 RETURN
11570 '
11580 ' *** Help-Text "Stammsatz ändern (II)" *********************************
11590 '
11600 MT$="HILFE":GOSUB 13940                                      'Basis Maske
11610 PRINT HOME$
11620 PRINT "    Sie können jetzt einen bestehenden Stammsatz ändern."
11630 PRINT
11640 PRINT "    Während der Dateneingabe können Sie mit der DELETE-Taste"
11650 PRINT "    korrigieren. Ist ein Feld ausgefüllt, muß RETURN gedrückt"
11660 PRINT "    werden, da sonst die Änderung nicht gespeichert wird."
11670 PRINT "      Mit den Cursortasten kann das Feld zum nächsten oberen,"
11680 PRINT "    bzw. unteren Feld gewechselt werden."
11690 PRINT
11700 PRINT "    `=>`-Felder sollten ausgefüllt werden."
11710 PRINT
11720 PRINT "    Weitere Funktionstasten :"
11730 PRINT
11740 PRINT "    <F7>  Diesen Stammsatz speichern.
11750 GOSUB 14410                                       'Rückkehr aus Help-Text
11760 MT$="STAMMDATEN ÄNDERN":GOSUB 13940                          'Basis Maske
11770 GOSUB 14060                                               'F-Tastenleiste
11780 RETURN
11790 '
11800 ' *** Help-Text "Stammsatz löschen" *************************************
11810 '
11820 MT$="HILFE":GOSUB 13940                                      'Basis Maske
11830 PRINT HOME$
11840 PRINT "    Sie können jetzt den Status eines Mitglieds auf `ausgetreten'"
11850 PRINT "    ändern. Damit wird der Stammsatz beim Ausdruck von Mitglieds-"
11860 PRINT "    listen nicht berüchsichtigt."
11870 PRINT "    Auf keinen Fall werden die Daten WIRKLICH gelöscht."
11880 PRINT
11890 PRINT "    Geben Sie die Mitgliedsnummer des betreffenden Stammsatzes "
11900 PRINT "    ein. Bei Tippfehlern können Sie mit der DELETE-Taste"
11910 PRINT "    korrigieren."
11920 GOSUB 14410                                       'Rückkehr aus Help-Text
11930 RETURN
11940 '
11950 ' *** Help-Text "Stammsatz suchen" **************************************
11960 '
11970 IF DRUCKFLAG THEN 12670
11980 MT$="HILFE":GOSUB 13940                                      'Basis Maske
11990 PRINT HOME$
12000 PRINT "    Sie können jetzt Stammsätze suchen."
12010 PRINT
12020 PRINT "    1 - Suchen nach Nummern"
12030 PRINT "        Geben Sie ein Intervall von Mitgliedsnummern ein."
12040 PRINT "        Es werden alle Stammsätze ausgegeben, deren Nummern"
12050 PRINT "        im Intervall liegt."
12060 PRINT
12070 PRINT "    2 - Suchen nach Maske"
12080 PRINT "        Sie füllen ein Formular aus. Diejenigen Stammsätze, die"
12090 PRINT "        mit dem Formular übereinstimmen, werden ausgegeben."
12100 PRINT "        Leere Felder bezeichnen beliebige Stammsatzdaten."
12110 PRINT "        Ein * zu Beginn des Feldes bedeutet : Das folgende Wort"
12120 PRINT "        darf sich an beliebiger Stelle des Feldes befinden."
12130 PRINT "        Ein oder mehrere ? zu Beginn des Feldes stellen Platz-"
12140 PRINT "        halter für beliebige Zeichen dar."
12150 PRINT
12160 GOSUB 14410                                       'Rückkehr aus Help-Text
12170 MT$="STAMMSATZ SUCHEN"
12180 RETURN
12190 ' 
12200 ' *** Help-Text für "Stammsatz drucken" *********************************
12210 '
12220 MT$="HILFE":GOSUB 13940                                      'Basis Maske
12230 PRINT FNCUR$(5,1)
12240 PRINT "   Sie können jetzt einen kompletten Stammsatz, wie er auf"
12250 PRINT "   dem Bildschirm steht, ausdrucken."
12260 PRINT
12270 PRINT "   Geben Sie die Mitgliedsnummer ein. Bei Tippfehlern können"
12280 PRINT "   Sie mit der DELETE-Taste korrigieren."
12290 GOSUB 14410                                       'Rückkehr aus Help-Text
12300 MT$="STAMMSATZ DRUCKEN"
12310 RETURN
12320 '
12330 ' *** Help-Text "Verwendungsnachweis (I)" *******************************
12340 '
12350 MT$="HILFE":GOSUB 13940                                      'Basis Maske
12360 PRINT FNCUR$(6,1)
12370 PRINT "    Sie können jetzt einen Verwendungsnachweis ändern."
12380 PRINT
12390 PRINT "    Geben Sie die Mitgliedsnummer des betreffenden "
12400 PRINT "    Verwendungsnachweises ein. Bei Tippfehlern können"
12410 PRINT "    Sie mit der DELETE-Taste korrigieren."
12420 GOSUB 14410                                       'Rückkehr aus Help-Text
12430 RETURN
12440 '
12450 ' *** Help-Text "Verwendungsnachweis (II)" ******************************
12460 '
12470 MT$="HILFE":GOSUB 13940                                      'Basis Maske
12480 PRINT HOME$
12490 PRINT "    Sie können jetzt einen bestehenden Verwendungsnachweis ändern."
12500 PRINT
12510 PRINT "    Während der Dateneingabe können Sie mit der DELETE-Taste"
12520 PRINT "    korrigieren. Ist ein Feld ausgefüllt worden, muß RETURN"
12530 PRINT "    gedrückt werden, da sonst die Änderung nicht gespeichert"
12540 PRINT "    wird. Mit den Cursortasten kann das Feld gewechselt werden."
12550 PRINT
12560 PRINT "    Weitere Funktionstasten :"
12570 PRINT "    <F5>  Zurückblättern (ohne die jetzige Seite zu speichern)"
12580 PRINT "    <F6>  Vorblättern    (ohne die jetzige Seite zu speichern)"
12590 PRINT "    <F7>  Die aktuelle Seite speichern"
12600 PRINT "    <F9>  Die aktuelle Seite ausdrucken"
12610 GOSUB 14410                                       'Rückkehr aus Help-Text
12620 MT$="VERWENDUNGSNACHWEIS"
12630 RETURN
12640 '
12650 ' *** Help-Text "Mitgliedsliste drucken" ********************************
12660 '
12670 MT$="HILFE":GOSUB 13940                                      'Basis Maske
12680 PRINT FNCUR$(4,1)
12690 PRINT "    Sie können jetzt eine Mitgliedsliste drucken lassen." 
12700 PRINT
12710 PRINT "    1 - Suchen & Drucken"
12720 PRINT "        Sie können Stammsätze nach verschiedenen Gesichtspunkten"
12730 PRINT "        suchen und drucken lassen."
12740 PRINT
12750 PRINT "    2 - Komplett drucken"
12760 PRINT "        Es wird eine komplette Mitgliedsliste ausgedruckt."
12770 PRINT "        Ausgetretene Mitglieder werden nicht berücksichtigt."
12780 PRINT
12790 PRINT "    3 - Datenblatt drucken"
12800 PRINT "        Der Stammsatz und der Verwendungsnachweis werden im"
12810 PRINT "        Bildschirmformat mit einer Liste von Mitgliedsver-"
12820 PRINT "        merken ausgedruckt."
12830 GOSUB 14410                                       'Rückkehr aus Help-Text
12840 MT$="MITGLIEDSLISTE AUSDRUCKEN"
12850 RETURN
12860 '
12870 ' *** Fielden der Stammsatz Random X.S.-Datei ************************OK*
12880 '
12890 OPEN "R",#2,"B:MV.DAT",462
12900 FIELD #2,236 AS DD1$,225 AS DD2$
12910 RETURN
12920 '
12930 ' *** Einlesen der Felddaten von Disk ***********************************
12940 '
12950 GET #2,MTGLZAHL
12960 FOR LL=1 TO FELDMAX:DAT$(LL)="":NEXT
12970 HLANG=1
12980 FOR K=1 TO 14:LNG=FNLNG(FELD$(K)):DAT$(K)=MID$(DD1$,HLANG,LNG):HLANG=LNG+HLANG:NEXT
12990 HLANG=1
13000 FOR K=15 TO 27:LNG=FNLNG(FELD$(K)):DAT$(K)=MID$(DD2$,HLANG,LNG):HLANG=LNG+HLANG:NEXT
13010 RETURN
13020 '
13030 ' *** Abspeichern der Felddaten auf Disk ********************************
13040 '
13050 GOSUB 15040       'NEUES VN-BLATT ANLEGEN   DARF NUR STS-ERF. ANSPRINGEN!
13060 FOR K=1 TO 27:LNG=FNLNG(FELD$(K)):DAT$(K)=DAT$(K)+STRING$(LNG-LEN(DAT$(K))," "):NEXT
13070 HH$=""
13080 FOR K=1 TO 14:HH$=HH$+DAT$(K):NEXT:LSET DD1$=HH$
13090 HH$="":FOR K=15 TO 27:HH$=HH$+DAT$(K):NEXT:LSET DD2$=HH$:HH$=""
13100 PRINT FNCUR$(21,1);
13110 PUT #2,MTGLZAHL
13120 N$="Die Daten sind auf Diskette abgespeichert.":GOSUB 13860 'Nachricht
13130 RETURN
13140 '
13150 ' *** Ja/Nein - Frage ***************************************************
13160 '
13170 ' IN:  Q$ - Frage
13180 ' OUT: JA 1 = true, 0 = false
13190 N$=Q$+" (J=Ja/N=Nein)":GOSUB 13860                             'Nachricht
13200 PRINT HOME$;" ";
13210 PRINT FNCUR$(23,1);DELLINE$;FNCUR$(24,1);DELLINE$;HOME$
13220 PRINT CURSOFF$;
13230 ANSWER$=INPUT$(1)
13240 PRINT CURSON$;BSP$;
13249 'IF ANSWER$="J" OR ANSWER$="j" THEN JA=1:GOSUB 14060:RETURN 'F-Tastenleiste
13250 IF ANSWER$="J" OR ANSWER$="j" THEN JA=1:RETURN            'F-Tastenleiste
13260 IF ANSWER$="N" OR ANSWER$="n" THEN JA=0:RETURN
13270 GOTO 13220
13280 '
13290 ' *** Testbereich 1 (Alpha) *********************************************
13300 '
13310 DRIN=0
13320 IF AASC>57 THEN IF AASC<127        THEN DRIN=1:RETURN
13330 IF AASC>31 THEN IF AASC<48         THEN DRIN=1
13340 RETURN
13350 '
13360 ' *** Testbereich 2 (Numerisch) *****************************************
13370 '
13380 DRIN=0:IF AASC>47 AND AASC<58      THEN DRIN=1
13390 RETURN
13400 '
13410 ' *** Testbereich 3 (Alphanumerisch) ************************************
13420 '
13430 DRIN=0:IF AASC>31 THEN IF AASC<127 THEN DRIN=1
13440 RETURN
13450 '
13460 ' *** Testbereich 4 (Numerisch + "/" und ".") ***************************
13470 '
13480 DRIN=0:IF AASC>45 THEN IF AASC<58  THEN DRIN=1
13490 RETURN
13500 '
13510 ' *** DATUMS-TEST (bei VN und M-Liste drucken) **************************
13520 '
13530 IF VAL(TEST$)=0 OR VAL(TEST$)>31 THEN JA=0                           'TAG
13540 RETURN
13550 IF VAL(TEST$)=0 OR VAL(TEST$)>12 THEN JA=0                         'MONAT
13560 RETURN
13570 IF VAL(TEST$)=0 THEN JA=0                                           'JAHR
13580 RETURN
13590 '
13600 ' *** EXIT für ERFASSEN *************************************************
13610 '
13620 K=1:FOR II=1 TO 30:A$=INKEY$:NEXT:A$=""
13630 PRINT FNCUR$(21,1);DELLINE$
13640 IF INSTR(SPACE$(25),DAT$(K))<>0 THEN K=K+1:IF K<28 THEN 13640 ELSE MTGLZAHL=MTGLZAHL-1:MTGL$=STR$(MTGLZAHL):GOTO 13670
13650 Q$="Wollen Sie diese Daten abspeichern?":GOSUB 13190       'Ja/Nein-Frage
13660 IF JA=1 THEN GOSUB 13050:N$="Daten werden abgespeichert.":GOSUB 13860 ELSE N$="Daten werden nicht abgespeichert.":GOSUB 13860:M=MTGLZAHL-1:MTGL$=STR$(M) 'Nachricht 'Nachricht
13670 OPEN "O",#1,"B:MTGLZAHL.DAT":PRINT #1,MTGL$:CLOSE #1
13680 MTGLMAX=MTGLZAHL:MTGL$=STR$(MTGLZAHL)
13690 RETURN
13700 '
13710 ' *** Voreinstellungen im ERFASSEN  (Defaults) **************************
13720 '
13730 DAT$(5)="10***"
13740 DAT$(7)=""11**""
13750 DAT$(11)="12**"
13760 DAT$(12)="13******"
13770 DAT$(21)="3"
13780 DAT$(22)="Nein"
13790 DAT$(23)="Nein"
13800 DAT$(25)="Nein"
13810 RETURN
13820 '
13830 ' *** Nachricht anzeigen *********************************************OK*
13840 '
13850 ' IN: N$ - Nachricht
13860 PRINT HOME$;" "                                        'TODO: Not needed?
13870 PRINT FNCUR$(21,2);DELLINE$;INVON$;" ";N$;" ";INVOFF$
13880 RETURN
13890 '
13900 ' *** Bildschirmmaske - Basis ****************************************OK*
13910 '
13920 ' IN: MT$ - Menu title
13930 ' IN: MI$ - Menu ID
13940 PRINT CLS$;
13950 PRINT HIGHOFF$;
13960 PRINT "00**********************",
13970 PRINT FNCUR$(1,(80-LEN(MT$))/2);MT$;FNCUR$(1,72);MI$
13980 PRINT STRICH$;
13990 PRINT FNCUR$(21,1);
14000 PRINT STRICH$
14010 RETURN
14020 '
14030 ' *** Funktionstasten Leiste (KEY LABELS) *******************************
14040 '
14050 ' IN: FKEYS$ - Additional funtion key labels 
14060 PRINT HIGHOFF$;
14070 PRINT FNCUR$(23,1);"<F1>=Hilfe <F2>=Zurück <F3>=Anfang <F4>=Hauptmenü ";
14080 PRINT FKEYS$ 'TODO Obsolete;
14090 'TODO LW Obsolete PRINT HOME$;" "
14100 RETURN
14110 '
14120 ' *** restliche funktionstasten bei Verw.-nachweis ausgeben
14130 '
14140 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=Vorwärts             <F8>=Drucken";
14150 PRINT HOME$;" "
14160 RETURN
14170 '
14180 ' *** Eingabe bei Menübildern *******************************************
14190 '
14200 ' IN:  NKEYMAX - max valid numeric key
14210 ' OUT: FKEY - number of pressed function key or 0
14220 ' OUT: NKEY - number of pressed numeric key or 0
14230 PRINT CURSOFF$
14240 NKEY=0:FKEY=0
14250 FOR I=1 TO 30:A$=INKEY$:NEXT                              'Key debouncing
14260 A$=INPUT$(1)
14270 IF A$>"0" AND ASC(A$)<48+NKEYMAX+1 THEN NKEY=VAL(A$):PRINT CURSON$:RETURN
14290 IF A$>CHR$(240) AND A$<CHR$(245) THEN FKEY=ASC(A$)-240:PRINT CURSON$:RETURN
14300 N$="Sie haben eine falsche Taste gedrückt - Bitte neue Auswahl treffen.":GOSUB 13860 'Nachricht
14310 GOTO 14260
14320 '
14330 ' *** Rückkehr ins Hauptmenü ? ******************************************
14340 '
14350 Q$="Sie wollen wirklich ins Hauptmenü?":GOSUB 13190        'Ja/Nein-Frage
14360 IF JA AND MT$="STAMMSATZ ÄNDERN" THEN GOSUB 14840
14370 IF JA=1 THEN CLOSE:RUN "DISKMENU.EXE" ELSE PRINT HOME$:PRINT FNCUR$(21,1);DELLINE$:GOSUB 14060:RETURN
14380 '
14390 ' *** Rückkehr aus HELP-Text? ****************************************OK*
14400 '
14410 N$="Zurück? (J=Ja)":GOSUB 13860                                'Nachricht
14420 GOSUB 13200                                          'TODO why not *****?
14430 IF JA=0 THEN 14410
14440 RETURN
14450 '
14460 ' *** Leerlesen von DAT$() (Ersetzt ERASE-Befehl) ***********************
14470 '
14480 FOR II=1 TO 27
14490   DAT$(II)=""
14500 NEXT
14510 RETURN
14520 '
14530 ' *** Disk I/O Error Trap Nachricht *************************************
14540 '
14550 PRINT CLS$
14560 PRINT FNCUR$(13,1);STRICH$
14570 PRINT
14580 PRINT
14590 GOSUB 13940                                                  'Basis Maske
14600 IF ERR<50 THEN PRINT FNCUR$(5,1);"Programmfehler."
14610 PRINT "   14*********************"
14620 PRINT
14630 PRINT "   15************************************************************"
14640 PRINT "   16*************************************************"
14650 PRINT "   
14660 PRINT "   17**********************"
14670 PRINT
14680 PRINT "         18********************************************"
14690 PRINT
14700 PRINT "         19**************************"
14710 PRINT
14720 PRINT "                                      20***************"
14730 PRINT
14740 PRINT STRICH$;
14750 PRINT CURSON$
14760 PRINT "ERL (Error Line) :";ERL                                'LW - Added
14770 PRINT "ERR (Error Code) :";ERR                                'LW - Added
14780 RESET
14790 'LW GOTO 13450                                       'LW - Endlosschleife
14800 END
14810 '
14820 ' *** UPRO FÜR ÄNDERN-EXIT **********************************************
14830 '
14840 Q$="Wollen Sie die Daten abspeichern?":GOSUB 13190         'Ja/Nein-Frage
14850 IF JA THEN GOSUB 13060
14860 RETURN
14870 '
14880 ' **** VN - Subroutines, permitting one page handling *******************
14890 ' ****         by 04********** 21*******              *******************
14900 '
14910 '
14920 ' **** OPEN & FIELDING VERWENDUNGSNACHWEIS ******************************
14930 '
14940 OPEN "R",#3,"B:VN.NDX",4
14950 FIELD #3,4 AS POINT0$
14960 OPEN "R",#4,"B:VN.POI",8
14970 FIELD #4,4 AS POINT1$, 4 AS POINT2$
14980 OPEN "R",#5,"B:VN.DAT",560
14990 FIELD #5,240 AS DDD1$,240 AS DDD2$,80 AS DDD3$
15000 RETURN
15010 '
15020 ' *** VERWENDUNGSNACHWEIS FÜR NEUES MITGLIED ANLEGEN ********************
15030 '
15040 GET #3,1                            'letzte Seitennummer einlesen dann +1
15050 TMPOINT=VAL(POINT0$)+1
15060 LSET DDD1$=""                                    'leeren Record speichern
15070 LSET DDD2$=""
15080 LSET DDD3$=""
15090 PUT #5,TMPOINT
15100 LSET POINT0$=STR$(TMPOINT)             'letzte Seitennummer neu speichern
15110 PUT #3,1
15120 PUT #3,MTGLZAHL+1
15130 LSET POINT1$="0000"
15140 LSET POINT2$="0000"
15150 PUT #4,TMPOINT
15160 RETURN
15170 '
15180 ' **** ERSTE SEITE FINDEN & LESEN ***************************************
15190 '
15200 ' IN: MTGLZAHL
15210 GET #3,MTGLZAHL+1                                            'Index_Datei
15220 CPOINT=VAL(POINT0$)
15230 GET #5,CPOINT:GOSUB 16290                                       'Einlesen
15240 BLATT=1
15250 GET #4,CPOINT                                              'Pointer lesen
15260 RETURN
15270 '
15280 ' **** SEITE RÜCKWÄRTS **************************************************
15290 '
15300 TMPOINT=VAL(POINT1$)
15310 IF TMPOINT<>0 THEN 15350
15320 N$="Erste Seite":GOSUB 13860                                   'Nachricht
15330 FOR KK=1 TO 9999:NEXT                                              'Delay
15340 RETURN
15350 CPOINT=TMPOINT
15360 BLATT=BLATT-1
15370 GET #5,CPOINT:GOSUB 16290   
15380 GET #4,CPOINT:GOSUB 16450                             'Neue Pointer lesen
15390 HP=1:VP=1
15400 GOSUB 15660
15410 FOR KK=1 TO 100:NEXT                                               'Delay
15420 RETURN
15430 '
15440 ' **** SEITE VORWÄRTS  **************************************************
15450 '
15460 TMPOINT=VAL(POINT2$)
15470 IF TMPOINT=0 THEN 15540
15480 CPOINT=TMPOINT
15490 BLATT=BLATT+1
15500 GET #5,CPOINT:GOSUB 16290
15510 GET #4,CPOINT:GOSUB 16450
15520 HP=1:VP=1
15530 RETURN
15540 N$="Letzte Seite"
15550 IF INSTR(SPACE$(20),DAT$(26))=0 THEN 15590 
15560 GOSUB 13860                                                    'Nachricht
15570 FOR KK=1 TO 20000:NEXT                                             'Delay
15580 RETURN
15590 Q$="Letzte Seite - Neue Seite anlegen?":GOSUB 13190        'Ja/Nein-Frage
15600 N$="":GOSUB 13860                                              'Nachricht
15610 IF JA=0 THEN 15660
15620 GOSUB 15900
15630 BLATT=BLATT+1
15640 GOSUB 16450
15650 HP=1:VP=1
15660 GOSUB 14060
15670 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=vorwärts";
15680 PRINT TAB(54);"<F9>=Blatt drucken";
15690 PRINT HOME$;" "
15700 RETURN
15710 '
15720 ' **** ERSTE SEITE EINES MITGLIEDS ANLEGEN ******************************
15730 '
15740 GET #3,1
15750 LSET POINT0$=STR$(VAL(POINT0$)+1)                                  'NDX+1
15760 PUT #3,1
15770 LSET POINT1$="0000"                                        'Page pointers
15780 LSET POINT2$="0000"
15790 PUT #4,VAL(POINT0$)
15800 LSET POINT0$=POINT0$                                          'First page
15810 PUT #3,MTGLZAHL+1
15820 LSET DDD1$=""                                                 'Datenblatt
15830 LSET DDD2$=""
15840 LSET DDD3$=""
15850 PUT #5,VAL(POINT0$)
15860 RETURN
15870 '
15880 ' **** ALLGEMEIN EINE NEUE SEITE ANLEGEN ********************************
15890 '
15900 GET #3,1                                                           'NDX+1
15910 LSET POINT0$=STR$(VAL(POINT0$)+1)
15920 PUT #3,1:TMPOINT$=POINT0$    'Vektoren der bisher letzten Seite verbiegen
15930 LSET POINT2$=POINT0$                                'Verbiege Vektoren...
15940 PUT #4,CPOINT                                         '...der alten Seite
15950 LSET POINT1$=STR$(CPOINT)                             '...der neuen Seite
15960 LSET POINT2$="0000"                                                '.. ""
15970 CPOINT=VAL(TMPOINT$)                                       'Nr neue Seite
15980 PUT #4,CPOINT
15990 LSET DDD1$=""                                           'Write empty page
16000 LSET DDD2$=""
16010 LSET DDD3$="" 
16020 PUT #5,CPOINT
16030 DATEN$=SPACE$(20)
16040 FOR II=1 TO 26
16050   DAT$(II)=DATEN$
16060 NEXT
16070 RETURN
16080 '
16090 ' **** SEITE ABSPEICHERN ************************************************
16100 '
16110 DATEN$=""
16120 FOR II=1 TO 12
16130   DATEN$=DATEN$+DAT$(II)
16140 NEXT
16150 LSET DDD1$=DATEN$:DATEN$=""
16160 FOR II=13 TO 24
16170   DATEN$=DATEN$+DAT$(II)
16180 NEXT
16190 LSET DDD2$=DATEN$:DATEN$=""
16200 FOR II=25 TO 26
16210   DATEN$=DATEN$+DAT$(II)
16220 NEXT 
16230 LSET DDD3$=DATEN$:DATEN$=""
16240 PUT #5,CPOINT
16250 RETURN
16260 '
16270 ' **** HACK ! (ZERSTÜCKELN DER DATEN-STRINGS) ***************************
16280 '
16290 FOR II=1 TO 12
16300   DAT$(II)=MID$(DDD1$,(II-1)*20+1,20)
16310 NEXT
16320 FOR II=13 TO 24
16330   DAT$(II)=MID$(DDD2$,(II-13)*20+1,20)
16340 NEXT 
16350 FOR II=25 TO 26
16360   DAT$(II)=MID$(DDD3$,(II-25)*20+1,20)
16370 NEXT 
16380 RETURN
16390 '
16400 ' **** UPRO DATENAUSGABE VN  ********************************************
16410 '
16420 ' IN: MTGLZAHL
16430 ' IN: NAMM$
16440 ' IN: BLATT
16450 PRINT DELHIGH$;HIGHON$
16460 PRINT FNCUR$(5,16);MTGLZAHL
16470 PRINT FNCUR$(5,29);NAMM$
16480 PRINT FNCUR$(3,8);BLATT
16490 FOR K=1 TO 26
16500   HOR=6-39*(K>13)
16510   VER=6+K+13*(K>13)
16520   FOR J=1 TO 3
16530     PRINT FNCUR$(VER,HOR+(J-1)*3);MID$(DAT$(K),-1+J*2,2)
16540   NEXT
16550   FOR J=1 TO 3
16560     PRINT FNCUR$(VER,HOR+13+(J-1)*3);MID$(DAT$(K),5+J*2,2)
16570   NEXT
16580   PRINT FNCUR$(VER,HOR+26);MID$(DAT$(K),13,8)
16590 NEXT
16600 PRINT HIGHOFF$
16610 RETURN
16620 '
16630 ' *** vn drucken ********************************************************
16640 '
16650 ' IN: BLATT
16660 ' IN: MITGLZAHL
16670 ' IN: NAMM$
16680 Q$="Ist der Drucker an?":GOSUB 13190                       'Ja/Nein-Frage
16690 IF JA=0 THEN 16890
16700 LPRINT STRICH$
16710 LPRINT
16720 LPRINT TAB(2);"Blatt:";BLATT
16730 LPRINT
16740 LPRINT TAB(2);"Mitglieds-Nr.:";MTGLZAHL;TAB(24);"Name:";NAMM$
16750 LPRINT
16760 FOR II=1 TO 13
16770   LOZ=II:GOSUB 16860
16780   LPRINT TAB(2);"Von:";LODAT1$;" Bis:";LODAT2$;" Amt:";LODAT3$;
16790   LOZ=II+13:GOSUB 16860
16800   LPRINT " Von:";LODAT1$;" Bis:";LODAT2$;" Amt:";LODAT3$
16810 NEXT 
16820 LPRINT
16830 LPRINT STRICH$
16840 RETURN
16850 '
16860 LODAT1$=LEFT$(DAT$(LOZ),2)+"."+MID$(DAT$(LOZ),3,2)+"."+MID$(DAT$(LOZ),5,2)
16870 LODAT2$=MID$(DAT$(LOZ),7,2)+"."+MID$(DAT$(LOZ),9,2)+"."+MID$(DAT$(LOZ),11,2)
16880 LODAT3$=RIGHT$(DAT$(LOZ),8)
16890 RETURN
16900 '
16910 ' *** upros MITGLIEDSLISTE DRUCKEN **************************************
16920 '
16930 ' *** MITGLIED DRUCKEN **************************************************
16940 '
16950 ' IN: MTGLZAHL
16960 LPRINT
16970 LPRINT TAB(5);MTGLZAHL;
16980 LPRINT TAB(10);DAT$(1);                                        'Nachnamen
16990 LPRINT TAB(36);DAT$( 2);                                        'Vornamen
17000 LPRINT TAB(65);"(";LEFT$(DAT$( 5),1);")";                         'Status
17010 AMT$=""
17020 IN=INSTR(DAT$(6),"22**"):IF IN<>0 THEN AMT$=MID$(DAT$(6),IN,6):GOTO 17050
17030 IN=INSTR(DAT$(6),"23**"):IF IN<>0 THEN AMT$=MID$(DAT$(6),IN,5):GOTO 17050
17040 IN=INSTR(DAT$(6),"24**"):IF IN<>0 THEN AMT$=MID$(DAT$(6),IN,3)
17050 LPRINT TAB(71);AMT$
17060 LPRINT TAB(36);DAT$(10)                                           'Straße
17070 LPRINT TAB(36);DAT$(11);                                    'Postleitzahl
17080 LPRINT TAB(41);DAT$(12)                                              'Ort
17090 LPRINT TAB(36);"Tel.:";DAT$(13)                                  'Telefon
17100 LPRINT
17110 IF INKEY$="" THEN 17140
17120 Q$="Drucken abbrechen ?":GOSUB 13190                       'Ja/Nein-Frage
17130 IF JA=1 THEN BRKFLAG=-1 ELSE BRKFLAG=0
17140 RETURN
17150 '
17160 ' *** KOPF DER MITGLIEDSLISTE *******************************************
17170 '
17180 ' IN: DATUM$
17190 LPRINT TAB(16);"MITGLIEDSLISTE 25*******************************"
17200 LPRINT 
17210 LPRINT TAB(32);"Stand:";DATUM$
17220 LPRINT
17230 LPRINT TAB(3);"Mitgl."; 
17240 LPRINT TAB(10);"Name";
17250 LPRINT TAB(36);"Vorname, Anschrift";
17260 LPRINT TAB(63);"Status";
17270 LPRINT TAB(71);"Amt"
17280 RETURN
17290 '
17300 ' *** UPRO ADRESSAUFKLEBER DRUCKEN **************************************
17310 '
17320 N$="Ist der Drucker bereit und positioniert? (J/N)":GOSUB 13860 'Nachricht
17330 GOSUB 13200                                           'TODO Why not 12520
17340 IF JA=0 THEN RETURN
17350 LPRINT "   ";DAT$(7)
17360 SPAC1=INSTR(DAT$(2)," ")
17370 LPRINT "   ";LEFT$(DAT$(2),SPAC1);DAT$(1)
17380 LPRINT
17390 LPRINT "   ";DAT$(10)
17400 LPRINT
17410 LPRINT "   ";DAT$(11);" "DAT$(12)
17420 LPRINT
17430 LPRINT
17440 LPRINT
17450 LPRINT
17460 LPRINT
17470 LPRINT
17480 RETURN
17490 '
17500 ' *** Stammsatz ausdrucken **********************************************
17510 '
17520 RESTORE 17630
17530 LPRINT STRICH$
17540 LPRINT
17550 LPRINT TAB(30);"Mitgliedsnummer  ";MTGLZAHL
17560 LPRINT
17570 LPRINT TAB(2);LEFT$(FELD$(1),14);DAT$(1);TAB(41);"Beruf:"
17580 FOR I=1 TO 6:READ A,B
17590   LPRINT TAB(2);LEFT$(FELD$(A),14);DAT$(A);
17600   IF B=0 THEN LPRINT:GOTO 17620
17610   LPRINT TAB(41);LEFT$(FELD$(B),14);DAT$(B);
17620 NEXT
17630 DATA 2,15,3,16,4,0,5,17,6,18,0,19
17640 FOR I=7 TO 14
17650   LPRINT TAB(2);LEFT$(FELD$(I),14);DAT$(I);
17660   LPRINT TAB(41);LEFT$(FELD$(I+13),14);DAT$(I+13);
17670 NEXT
17680 LPRINT
17690 LPRINT
17700 LPRINT
17710 LPRINT STRICH$
17720 RETURN
17730 '
17740 ' *** Mitgliedsvermerke *************************************************
17750 '
17760 ' IN: NAMM$ =
17770 LIN$=" ___ / ___ / ___" 
17780 LPRINT TAB(10);"Mitgliedsnr.:";TAB(28);"Name:";NAMM$
17790 LPRINT TAB(65);"Datum:"
17800 LPRINT
17810 LPRINT TAB(9);"26***************************";LIN$
17820 LRPINT
17830 LPRINT TAB(9);"27***************************";LIN$
17840 LRPINT
17850 LPRINT TAB(9);"28***************************";LIN$
17860 LRPINT
17870 LPRINT TAB(9);"29***************************";LIN$
17880 LRPINT
17890 LPRINT TAB(9);"30***************************";LEFT$(LIN$,10)
17900 LRPINT
17910 LPRINT TAB(9);"31***************************";LIN$
17920 LRPINT
17930 LPRINT TAB(9);"32***************************";LIN$
17940 LRPINT
17950 LPRINT TAB(9);"33***************************";LEFT$(LIN$,4)
17960 LRPINT
17970 LPRINT TAB(9);"34***************************";LIN$
17980 LRPINT
17990 LPRINT TAB(9);"35***************************";LIN$
18000 LRPINT
18010 LPRINT TAB(9);"36***************************";LIN$
18020 LRPINT
18030 LPRINT TAB(9);"37***************************";LIN$
18040 LRPINT
18050 LPRINT TAB(9);"38***************************";LIN$
18060 RETURN
