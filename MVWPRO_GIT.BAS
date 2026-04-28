1000 ' RELEASE 01***** Neubearbeitung durch 04**********
1010 '
1020 ' LW 31-JAN-2026: Load MSBASIC.COM with /S:560 /F:5
1030 '
1040 ' *** Initialisierung, Dimensionierung ***********************************
1050 '
1060 DEFINT A-Z                                              'Integerdefinition
1070 '
1080 FELDMAX = 27                                                'Anzahl Felder
1090 DIM FENAM$(FELDMAX)                                       'Name des Feldes
1100 DIM FEMUSS(FELDMAX)                              'Muss-Feld (1=Ja, 0=Nein)
1110 DIM FEROW(FELDMAX)                           'Zeile des Feldnamens Ä1..25Ü
1120 DIM FECOL(FELDMAX)                          'Spalte des Feldnamens Ä1..80Ü
1130 DIM FELNG(FELDMAX)                                       'Länge des Feldes
1140 DIM FETYP(FELDMAX)                                         'Typ des Feldes
1150 '
1160 DIM DAT$(FELDMAX)                                              'Datenarray
1170 DIM SUCH$(FELDMAX)                                              'Sucharray
1180 '
1190 BSP$    =CHR$(8)                                                'Backspace
1200 DEL$    =BSP$+"."+BSP$                                             'Delete
1210 ESC$    =CHR$(27)                                'Einleitung Escapesequenz
1220 CLS$    =ESC$+"E"                                      'Bildschirm löschen
1230 INVON$  =ESC$+"p"                                              'Invers ein
1240 INVOFF$ =ESC$+"q"                                              'Invers aus
1250 CURON$  =ESC$+"y5"                                             'Cursor ein
1260 CUROFF$ =ESC$+"x5"                                             'Cursor aus
1270 HIGHON$ =ESC$+"("                                           'Helle Schrift
1280 HIGHOFF$=ESC$+")"                                          'Dunkle Schrift
1290 DELLINE$=ESC$+"K"                                           'Zeile löschen
1300 DELHIGH$=ESC$+"+"                                   'Helle Schrift löschen
1310 STRICH$ =STRING$(79,"-")                                           'Strich
1320 IN$=SPACE$(10)                                     'Indent für Hilfe-Texte
1330 '
1340 DEF FNCUR$(Z,S)=ESC$+"Y"+CHR$(31+Z)+CHR$(31+S)       'Cursor positionieren
1350 HOME$=FNCUR$(3,1)                                    'Cursor Home Position
1360 ' 
1370 CKEYUP=1
1380 CKEYDN=2
1390 CKEYLT=3
1400 CKEYRT=4
1410 '
1420 ASCBSP=8
1430 ASCRET=13
1440 ASCESC=27
1445 ASC1=49
1450 ASCCUP=200
1460 ASCCDN=201
1470 ASCCLT=202
1480 ASCCRT=203
1490 ASCF1=241
1491 ASCF4=244
1500 ASCF9=249
1510 '
1520 ' Other global variables:
1521 ' -----------------------
1530 ' LIND$   - Local text indent
1540 ' F       - Feldindex 1..27 (FELDMAX)
1541 ' MTGLMAX - Höchste Mitgliedsnummer
1542 '
1550 XOFF=14             'Offset Colum Feldname to Column Wertefeld (Pünktchen)
1551 '
1552 ' TIP: Do not let the cursor slip into line 25 -> display will scroll up
1560 '
1570 ON ERROR GOTO 17340                                 'Enable error trapping
1580 '
1590 GOSUB 10050    'Tasten deaktivieren, ASCII Codes für Cursortasten vergeben
1600 PRINT CUROFF$;
1610 '
1620 ' *** Beginn des Mitgliederverwaltungsprogramms **************************
1630 '
1640 MT$="MITGLIEDERVERWALTUNG":MI$="G-01000":GOSUB 12580          'Basis Maske
1650 Q$="Bitte Daten-Diskette in Laufwerk B einlegen. Bereit?":GOSUB 11900 'Ja/Nein-Frage
1660 IF JA=0 THEN 17480                                           'Programmende
1690 '
1700 N$="Einen Moment...":GOSUB 12510                                'Nachricht
1710 GOSUB 10830                          'Einlesen Stammdatenfelder-Definition
1720 GOSUB 11600                                   'Fielden der Stammsatz-Datei
1730 GOSUB 13230                       'Fielden der Verwendungsnachweis-Dateien
1735 GOSUB 11090:MTGLZAHL=MTGLMAX                            'Einlesen MTGLZAHL
1740 N$="":GOSUB 12510                                               'Nachricht
1750 '
1760 ' ################# Mitgliederverwaltungsmenü (MVW-Menü) #################
1770 '
1780 MT$="MITGLIEDERVERWALTUNG":MI$="G-01000":GOSUB 12580          'Basis Maske
1790 FKEYS$="":GOSUB 12690                                      'F-Tastenleiste
1800 VNFLAG=0                                                              '???
1810 PRINT FNCUR$(4,26);"1 - Stammsatzpflege"                         'Menütext
1820 PRINT FNCUR$(6,26);"2 - Verwendungsnachweis"
1830 PRINT FNCUR$(8,26);"3 - Mitgliedsliste drucken"
1840 NKEYMAX=3:GOSUB 12840                                         'Menüeingabe
1850 DRUCKFLAG=0                                                           '???
1860 IF NKEY THEN ON NKEY GOTO 1940,5050,5880
1870 ON FKEY GOTO 1880,1890,1780,1890         'Hilfe, Zurück, Anfang, Hauptmenü
1880 GOSUB 16530:GOTO 1780                                      'F1->Hilfe-Text
1890 GOSUB 12940:GOTO 1780                           'F2->Zurück, F4->Hauptmenü
1900 '
1910 ' ########################### Stammsatzpflege ############################
1920 '
1940 IF DRUCKFLAG THEN RETURN                                              '???
1941 GOSUB 11044                                 'Clear DAT$() TODO: necessary?
1942 GOSUB 11049                                'Clear SUCH$() TODO: necessary?
1950 '
1960 ' *** Stammsatzpflege-Menü ***********************************************
1970 '
1980 MT$="STAMMSATZPFLEGE":MI$="G-01100":GOSUB 12580               'Basis Maske
1990 FKEYS$="":GOSUB 12690                                      'F-Tastenleiste
2000 PRINT FNCUR$(4,29);"1 - Stammsatz anlegen"
2010 PRINT FNCUR$(6,29);"2 - Stammsatz ändern"
2020 PRINT FNCUR$(8,29);"3 - Stammsatz löschen"
2030 PRINT FNCUR$(10,29);"4 - Stammsatz drucken"
2040 PRINT FNCUR$(12,29);"5 - Stammsatz suchen"
2050 NKEYMAX=5:GOSUB 12840                                         'Menüeingabe
2060 IF NKEY THEN ON NKEY GOTO 2140,2820,3290,4770,3580
2070 ON FKEY GOTO 2080,1780,1780,2100         'Hilfe, Zurück, Anfang, Hauptmenü
2080 GOSUB 16600:GOTO 1980                                      'F1->Hilfe-Text
2100 GOSUB 12940:GOTO 1980                                       'F4->Hauptmenü
2110 '
2120 ' *** Stammsatz erfassen von 02************ ******************************
2130 '
2140 MTGLZAHL=MTGLMAX+1                                       'TODO Entry point
2150 F=1
2160 DATENAUSG=1                                                           '???
2170 MT$="STAMMSATZ ANLEGEN":MI$="G-01101":GOSUB 12580             'Basis Maske
2180 FKEYS$="<F7>=Speichern":GOSUB 12690                        'F-Tastenleiste
2190 PRINT FNCUR$(3,31);"Mitgliedsnummer"
2195 GOSUB 11044                                                  'Clear DAT$()
2210 GOSUB 12380                                           'Defaultwerte setzen
2220 IF DATENAUSG=1 THEN GOSUB 11556:DATENAUSG=0                           '???
2230 PRINT FNCUR$(3,46);HIGHON$;MTGLZAHL;HIGHOFF$
2240 Z=FEROW(F):S=FECOL(F)+XOFF:LNG=FELNG(F):TYP=FETYP(F):FKEYMASK$="12347"
2250 INTEXT$=DAT$(F)                                          'TODO Entry Point
2260 GOSUB 10410                                                         'Input
2270 IF ASCC<>ASCRET THEN 2320                                 'Carriage return
2280   IF FEMUSS(F) AND LEN(OUTTEXT$)<1 THEN N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 12510:GOTO 2250 'Nachricht
2290   DAT$(F)=OUTTEXT$
2300   F=F+1:IF F>FELDMAX THEN F=1
2310   GOTO 2240
2320 IF CKEY=0 THEN 2380                                           'Cursor keys
2330   PRINT FNCUR$(Z,S);HIGHON$;DAT$(F);HIGHOFF$;STRING$(LNG-LEN(DAT$(F)),".")
2340 'LW *** Turing Award: F=F+(CKEY=CKEYUP)*(-(F>1)+(F=1)*25)+(CKEY=CKEYDN)*((F<27)-(F=27)*26)
2350   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FELDMAX
2360   IF CKEY=CKEYDN THEN F=F+1:IF F>FELDMAX THEN F=1
2370   GOTO 2240
2380 IF FKEY=1 THEN GOSUB 16670:GOTO 2170                       'F1->Hilfe-Text
2390 IF FKEY=2 THEN GOSUB 2590:GOTO 1980                  '2590 Exit "Erfassen"
2400 IF FKEY=3 THEN GOSUB 2590:GOTO 1780                  '2590 Exit "Erfassen"
2410 IF FKEY=4 THEN GOSUB 2590:GOSUB 12940:GOTO 2240  '2590 Exit "Erfassen" 12980 Implicit No
2420 IF FKEY=7 THEN GOSUB 2720:GOTO 2140
2540 N$="DEBUG: Should never happen!":GOSUB 12510    'TODO: Should never happen 'Nachricht
2560 GOTO 2260                                       'TODO: Should never happen
2570 '
2590 TMPF=1                                                                '???
2620 IF INSTR(SPACE$(25),DAT$(TMPF))=0 THEN 2670
2630   N$="DEBUG: DAT$() with 25 spaces!":GOSUB 12510                'Nachricht
2640   TMPF=TMPF+1:IF TMPF<(FELDMAX+1) THEN 2620
2650   MTGLZAHL=MTGLZAHL-1
2660   GOTO 2750
2661 '
2670 GOSUB 10794                                             'Debounce keyboard
2671 Q$="Wollen Sie vorher diesen Stammsatz speichern?":GOSUB 11900 'Ja/Nein-Frage
2680 IF JA=1 THEN 2720
2690   N$="Stammsatz wird nicht abgespeichert.":GOSUB 12510          'Nachricht
2700   MTGLZAHL=MTGLZAHL-1
2710   GOTO 2770
2711 '
2720 N$="Stammsatz wird abgespeichert...":GOSUB 12510                'Nachricht
2730 GOSUB 13340                                        'Erste VN-Seite anlegen
2740 GOSUB 11770                                       'Speichern der Felddaten
2750 MTGLMAX=MTGLZAHL:GOSUB 11180                            'Schreiben MTGLMAX
2770 N$="":GOSUB 12510                                       'Nachricht löschen
2780 RETURN
2790 ' 
2800 ' *** Stammsatz ändern von 03************ ********************************
2810 '
2820 MT$="STAMMSATZ ÄNDERN":MI$="G-01102":GOSUB 12580              'Basis Maske
2830 F=1:PRINT CURON$
2840 FKEYS$="":GOSUB 12690                                      'F-Tastenleiste
2850 'LW *** PRINT HIGHOFF$;
2860 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer"
2870 PRINT FNCUR$(5,23)"des zu verändernden Stammsatzes ein,"
2880 PRINT FNCUR$(6,22)"und beenden Sie die Eingabe mit RETURN"
2890 PRINT FNCUR$(8,37)"=>..."
2900 LNG=3:TYP=2:Z=8:S=39
2910 GOSUB 10410                                                         'INPUT
2920 IF FKEY=1 THEN GOSUB 16750:GOTO 2820                               'F-Keys
2930 IF FKEY=2 THEN 1980
2940 IF FKEY=3 THEN GOSUB 11044:GOTO 1780                         'Clear DAT$()
2950 IF FKEY=4 THEN GOSUB 12940
2960 IF CKEY<>0 OR FKEY<>0 THEN GOTO 2900                               'Errors
2970 IF LEN(OUTTEXT$)<1 THEN N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 12510:GOTO 2900 ELSE MITGLIED=VAL(OUTTEXT$) 'Nachricht
2980 IF MITGLIED=0 THEN MITGLIED=MTGLMAX+2
2990 IF MITGLIED>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12510:PRINT HIGHOFF$;FNCUR$(8,39);"...";DELHIGH$:OUTTEXT$="":GOTO 2910 'Nachricht
3000 MTGLZAHL=MITGLIED:MTGL$=STR$(MITGLIED)
3010 FOR K=4 TO 8:PRINT FNCUR$(K,1);DELLINE$:NEXT:PRINT FNCUR$(21,1);DELLINE$
3020 GOSUB 11660                                                   'Daten lesen
3030 PRINT HIGHOFF$;FNCUR$(3,30);"Mitgliedsnummer";FNCUR$(5,41);"Beruf:"
3040 GOSUB 11270                                         'LW replace with 11556
3050 GOSUB 11490
3060 PRINT FNCUR$(3,46);MTGLZAHL
3070 FKEYS$="<F7>=Speichern":GOSUB 12690                        'F-Tastenleiste
3080 S=FECOL(F)+XOFF:Z=FEROW(F):N$="":GOSUB 12510                    'Nachricht
3090 LNG=FELNG(F):TYP=FETYP(F)
3100 GOSUB 10410                                                         'INPUT
3110 IF FKEY=1 THEN GOSUB 16820:GOTO 3030                   'Hilfe-Text  F-Keys
3120 IF FKEY=2 THEN GOSUB 13140: GOTO 2820                     'Eing.-Mtgl.-Nr.
3130 IF FKEY=3 THEN GOSUB 13140:GOSUB 11044: GOTO 1780              'Mvw-Menü 'Clear DAT$() 12610
3140 IF FKEY=4 THEN GOSUB 12940:PRINT HIGHOFF$;FNCUR$(24,1);"<F7>=Speichern";FNCUR$(1,1);HIGHON$:GOTO 3100
3150 IF FKEY=7 THEN 3220                                       'Daten Speichern
3160 IF CKEY=CKEYUP THEN PRINT FNCUR$(Z,S);DAT$(F);STRING$(LNG-LEN(DAT$(F)),"."):IF F>1  THEN F=F-1:GOTO 3080  ELSE F=26:GOTO 3080 'Cursor CKEY
3170 IF CKEY=CKEYDN THEN PRINT FNCUR$(Z,S);DAT$(F);STRING$(LNG-LEN(DAT$(F)),"."):IF F<27 THEN F=F+1:GOTO 3080  ELSE F= 1:GOTO 3080 'Cursor runter
3180 IF ASCC=13 AND LEN(OUTTEXT$)<1 AND FEMUSS(F) THEN N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 12510:GOTO 3100 'Nachricht
3190 IF ASCC=13 THEN DAT$(F)=OUTTEXT$:PRINT HIGHON$;FNCUR$(Z,S);DAT$(F);STRING$(LNG-LEN(DAT$(F)),".")                    'Return
3200 IF ASCC=13 THEN IF F<FELDMAX THEN F=F+1:GOTO 3080 ELSE F=1:GOTO 3080
3210 N$="Ungültige Eingabe - Bitte wiederholen":GOSUB 12510:FOR KK=1 TO 25000:NEXT:GOTO 3080 'Nachricht
3220 GOSUB 11770                                               'Daten speichern
3230 Q$="Wollen Sie noch einen weiteren Datensatz ändern?":GOSUB 11900 'Ja/Nein-Frage
3240 IF JA=1 THEN 2820
3250 GOTO 1980                                                    'Zum STS-Menü
3260 '
3270 ' *** Stammsatz löschen von 04********** *********************************
3280 '
3290 MT$="STAMMSATZ LÖSCHEN":MI$="G-01103":GOSUB 12580             'Basis Maske
3300 FKEYS$="":GOSUB 12690                                      'F-Tastenleiste
3310 PRINT FNCUR$(4,23);"Bitte geben Sie die Mitgliedsnummer"         'Menütext
3320 PRINT FNCUR$(5,25);"des zu löschenden Mitglieds ein."
3330 PRINT FNCUR$(6,15);"(Dabei wird nur der Status des Mitglieds verändert.)"
3340 PRINT FNCUR$(7,23);"Beenden Sie die Eingabe mit RETURN"
3350 PRINT FNCUR$(9,37);"=>..."
3360 LNG=3:TYP=2:Z=9:S=39
3370 PRINT HIGHON$:GOSUB 10410:PRINT HIGHOFF$                          'INPUT+1
3380 IF FKEY=2 THEN 1980                                                'F-Keys
3390 IF FKEY=3 THEN GOSUB 11044: GOTO 1780                        'Clear DAT$()
3400 IF FKEY=1 THEN GOSUB 16910:GOTO 3290
3410 IF FKEY=4 THEN GOSUB 12940:GOTO 3310
3420 IF FKEY>4 THEN CKEY=CKEYDN
3430 IF CKEY<>0 THEN N$="Ungültige Eingabe - Bitte wiederholen":GOSUB 12510:GOTO 3350 'Nachricht
3440 MTGLZAHL=VAL(OUTTEXT$):IF MTGLZAHL=0 THEN MTGLZAHL=MTGLMAX+1       'Errors
3450 IF MTGLZAHL>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12510:GOTO 3350 'Nachricht
3460 GOSUB 11660                                                'Daten einlesen
3470 GOSUB 12580                                                   'Basis Maske
3480 PRINT FNCUR$(3,30);"Mitgliedsnummer";FNCUR$(3,46);MTGLZAHL;FNCUR$(5,41);"Beruf:"
3481 GOSUB 11270:GOSUB 11490 'Feldmaske                 'LW replace with 11556?
3490 Q$="Endgültig löschen?":GOSUB 11900                         'Ja/Nein-Frage
3500 IF JA=0 THEN 3530                                               'Löschen ?
3510 DAT$(5)="ausgetreten     ":PRINT FNCUR$(9,16);HIGHON$;DAT$(5);HIGHOFF$
3520 GOSUB 11770                                               'Daten speichern
3530 Q$="Soll noch jemand gelöscht werden?":GOSUB 11900          'Ja/Nein-Frage
3540 IF JA=1 THEN 3290 ELSE 1980
3550 '
3560 ' *** Stammsatz suchen von 05*************** *****************************
3570 '
3580 MI$="G-01105"
3590 NUMSUCH1=0:NUMSUCH2=0:MTGL$=""
3600 PRINT HIGHOFF$
3610 IF DRUCKFLAG THEN MT$="SUCHEN & DRUCKEN" ELSE MT$="STAMMSATZ SUCHEN"
3620 GOSUB 12580                                                   'Basis Maske
3630 FKEYS$="":GOSUB 12690                                      'F-Tastenleiste
3640 PRINT CUROFF$;
3650 IF DRUCKFLAG THEN A$=" Drucken " ELSE A$=" Suchen "
3660 PRINT FNCUR$(6,30)"1 -";A$;"nach Nummern"
3670 PRINT FNCUR$(8,30)"2 -";A$;"nach Maske"
3680 GOSUB 11044                                                  'Clear DAT$()
3690 NKEYMAX=2:GOSUB 12840                                         'Menüeingabe
3700 IF FKEY=1 THEN GOSUB 16980:GOTO 3600                               'F-Keys
3710 IF FKEY=2 THEN IF DRUCKFLAG THEN GOSUB 11044:GOTO 5880 ELSE 1980 'Clear DAT$() 12610
3720 IF FKEY=3 THEN GOSUB 11044: GOTO 1780                        'Clear DAT$()
3730 IF FKEY=4 THEN GOSUB 12940:GOTO 3690
3740 IF NKEY=1 THEN 3940
3750 F=1                                                     'Suchen nach Maske
3760 FOR I=1 TO 27:SUCH$(I)="":DAT$(I)="":NEXT
3770 FKEYS$="<F5>=Suchen ":GOSUB 4680
3780 IF DRUCKFLAG THEN FKEYS$="<F5>=Suchen & Drucken"
3790 GOSUB 12690                                                'F-Tastenleiste
3800 Z=FEROW(F):S=FECOL(F)+XOFF                                'Cursorparameter
3810 LNG=FELNG(F):TYP=3
3820 PRINT FNCUR$(Z,S)
3830 OUTTEXT$="":GOSUB 10410                                             'INPUT
3840 IF FKEY=5 THEN MTGLZAHL=1:GOTO 4250                                'F-Keys
3850 IF FKEY=1 THEN GOSUB 16980:GOSUB 4680:GOSUB 11490:GOTO 3780
3860 IF FKEY=4 THEN GOSUB 12940:GOTO 3800
3870 IF FKEY=2 THEN 3580
3880 IF FKEY=3 THEN GOSUB 11044: GOTO 1780                        'Clear DAT$()
3890 IF OUTTEXT$<>"" OR ASCC=13 THEN SUCH$(F)=OUTTEXT$:DAT$(F)=OUTTEXT$
3900 IF CKEY=CKEYUP THEN IF F>1 THEN F=F-1 ELSE F=26               'Cursor CKEY
3910 IF CKEY=CKEYDN OR ASCC=ASCRET THEN IF F<27 THEN F=F+1 ELSE F=1       'Cursor runter
3920 N$="":GOSUB 12510                                               'Nachricht
3930 GOTO 3800
3940 PRINT CLS$:PRINT:PRINT                                'Suchen nach Nummern
3950 Z=7:S=1:IF DRUCKFLAG THEN DRUCK$="gedruckt" ELSE DRUCK$="gesucht "
3960 TYP=2:LNG=3:PRINT CURON$;                                        'Menütext
3970 GOSUB 12580                                                   'Basis Maske
3980 PRINT FNCUR$(4,5);"Maximale Mitgliedsnummer:";MTGLMAX
3990 FKEYS$="":GOSUB 12690                                      'F-Tastenleiste
4000 PRINT HOME$;:PRINT FNCUR$(Z,S);                          'Beginn bei Nr...
4010 PRINT HIGHOFF$;"Ab welcher Mitgliedsnummer soll ";DRUCK$;" werden      =>...":S=56
4020 GOSUB 10410                                                       'INPUT+1
4030 IF FKEY=1 THEN GOSUB 16980:GOTO 3940                               'F-Keys
4040 IF FKEY=2 THEN 3580
4050 IF FKEY=3 THEN GOSUB 11044: GOTO 1780                        'Clear DAT$()
4060 IF FKEY=4 THEN GOSUB 12940:S=1:GOTO 4000
4070 IF OUTTEXT$="" THEN S=1:GOTO 4000
4080 NUMSUCH1=VAL(OUTTEXT$)
4090 IF NUMSUCH1>MTGLMAX OR NUMSUCH1<1 THEN 4220
4100 Z=9:S=1
4110 N$="":GOSUB 12510                                               'Nachricht
4120 PRINT HOME$:PRINT FNCUR$(Z,S);                             'Ende bei Nr...
4130 PRINT HIGHOFF$;"Bis zu welcher Mitgliedsnummer soll ";DRUCK$;" werden  =>...":S=56
4140 GOSUB 10410                                                       'INPUT+1
4150 IF FKEY=1 THEN GOSUB 16980:GOTO 3940                               'F-Keys
4160 IF FKEY=2 THEN 3580
4170 IF FKEY=3 THEN GOSUB 11044: GOTO 1780                        'Clear DAT$()
4180 IF FKEY=4 THEN GOSUB 12940:S=1:GOTO 4120
4190 IF OUTTEXT$="" THEN S=1:GOTO 4120
4200 NUMSUCH2=VAL(OUTTEXT$)
4210 IF NUMSUCH2<=MTGLMAX AND NUMSUCH1<=NUMSUCH2 THEN 4240
4220 N$="Unlogische Eingabe - Bitte neu eingeben":GOSUB 12510 'Errors 'Nachricht
4230 Z=6:S=1:PRINT FNCUR$(9,1);DELLINE$;FNCUR$(Z,S);:Z=7:GOTO 4000
4240 MTGLZAHL=NUMSUCH1                                               'Feldmaske
4250 PRINT CUROFF$
4260 IF DRUCKFLAG THEN GOSUB 15290               'Mitgliedsliste drucken (Kopf)
4270 CC$="  <F7>=Adressaufkleber"
4280 IF NUMSUCH1<>0 THEN FKEYS$="<F5>=Suchen rückwärts  <F6>=vorwärts"+CC$ ELSE FKEYS$="<F6>=Suchen vorwärts"+CC$
4290 GOSUB 4680
4300 PRINT FNCUR$(3,30);"Mitgliedsnummer"
4310 IF NUMSUCH1=0  AND MTGLZAHL<=MTGLMAX  THEN 4380
4320 IF NUMSUCH1<>0 AND MTGLZAHL<=NUMSUCH2 THEN 4380
4330 IF DRUCKFLAG THEN GOSUB 11044:GOTO 3580                      'Clear DAT$()
4340 PRINT HIGHON$;CUROFF$;:MTGLZAHL=MTGLZAHL-1
4350 N$="Daten nicht vorhanden. Mit <F2> neue Suchmöglichkeit.":GOSUB 12510 'Nachricht
4360 PRINT HIGHOFF$;
4370 GOTO 4550
4380 GOSUB 11660                                                   'Daten lesen
4390 IF NUMSUCH1<>0 THEN 4490
4400 FOR K=1 TO FELDMAX                                      'Daten durchsuchen
4410   IF LEFT$(SUCH$(K),1)<>"*" THEN 4440
4420   IF INSTR(DAT$(K),RIGHT$(SUCH$(K),LEN(SUCH$(K))-1))=0 THEN MTGLZAHL=MTGLZAHL+1:GOTO 4310
4430   GOTO 4480
4440   I=1
4450   IF MID$(SUCH$(K),I,1)="?" THEN I=I+1:GOTO 4450
4460   I=I-1
4470   IF INSTR(DAT$(K),RIGHT$(SUCH$(K),LEN(SUCH$(K))-I))<>I+1 THEN MTGLZAHL=MTGLZAHL+1:GOTO 4310
4480 NEXT
4490 GOSUB 11490                                                     'Feldmaske
4500 MTGL$=STR$(MTGLZAHL) 
4510 PRINT CUROFF$;FNCUR$(3,46);MTGL$
4520 IF DRUCKFLAG=0 THEN 4550
4530 GOSUB 15440                             'Mitgliedsliste drucken (Mitglied)
4540 IF ISBRKPRNT THEN ISBRKPRNT=0:N$="":GOSUB 12510:GOTO 3580 ELSE N$="":GOSUB 12510:PRINT CUROFF$:MTGLZAHL=MTGLZAHL+1:GOTO 4310 'Nachricht 'Nachricht
4550 FKEY=ASC(INPUT$(1))-240
4560 N$="":GOSUB 12510                                               'Nachricht
4570 IF FKEY=1 THEN GOSUB 16980:GOSUB 4680:PRINT FNCUR$(3,30);"Mitgliedsnummer":GOTO 4490
4580 IF FKEY=2 THEN IF NUMSUCH1<>0 THEN 3940 ELSE 3750
4590 IF FKEY=3 THEN GOSUB 11044:GOTO 1780                         'Clear DAT$()
4600 IF FKEY=7 THEN GOSUB 15680:GOSUB 12690:N$="":GOSUB 12510:GOTO 4490 'Adressaufkleber drucken 'F-Tastenleiste 'Nachricht
4610 IF FKEY=4 THEN GOSUB 12940:GOTO 4490
4620 IF FKEY=5 AND NUMSUCH1<>0 THEN IF MTGLZAHL>NUMSUCH1 THEN MTGLZAHL=MTGLZAHL-1:GOTO 4380
4630 IF FKEY=6 THEN 4660
4640 N$="Falsche Eingabe":GOSUB 12510                                'Nachricht
4650 GOTO 4550
4660 MTGLZAHL=MTGLZAHL+1:PRINT FNCUR$(21,1);DELLINE$;HOME$;
4670 GOTO 4310
4680 GOSUB 12580                                                   'Basis Maske
4690 IF DRUCKFLAG THEN MT$="SUCHEN & DRUCKEN" ELSE MT$="STAMMSATZ SUCHEN"
4700 GOSUB 11270
4710 IF NOT DRUCKFLAG THEN GOSUB 12690                          'F-Tastenleiste
4720 PRINT FNCUR$(5,41);"BERUF:"
4730 RETURN
4740 '
4750 ' *** Stammsatz drucken von 02************ & 04********** ****************
4760 '
4770 MT$="STAMMSATZ DRUCKEN":MI$="G-01104":GOSUB 12580             'Basis Maske
4780 FKEYS$="":GOSUB 12690                                      'F-Tastenleiste
4790 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer"
4800 PRINT FNCUR$(5,21)"des auszudruckenden Stammsatzes ein."
4810 PRINT FNCUR$(6,23)"Beenden Sie die Eingabe mit RETURN"
4820 PRINT FNCUR$(8,37);"=>..."
4830 LNG=3:TYP=2:Z=8:S=39
4840 PRINT HIGHON$:GOSUB 10410:PRINT HIGHOFF$                          'INPUT+1
4850 IF FKEY=1 THEN GOSUB 17060:GOTO 4770                               'F-Keys
4860 IF FKEY=2 THEN 1980
4870 IF FKEY=3 THEN PRINT HIGHOFF$:GOSUB 11044: GOTO 1780         'Clear DAT$()
4880 IF FKEY=4 THEN GOSUB 12940:GOTO 4820
4890 IF FKEY>4 THEN CKEY=CKEYDN
4900 IF CKEY<>0 THEN 4820
4910 MTGLZAHL=VAL(OUTTEXT$):IF MTGLZAHL=0 THEN MTGLZAHL=MTGLMAX+1       'Errors
4920 IF MTGLZAHL>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12510:GOTO 4820 'Nachricht
4930 GOSUB 11660                                                   'Daten lesen
4940 GOSUB 12580                                                   'Basis Maske
4950 PRINT FNCUR$(3,30);"Mitgliedsnummer";HIGHON$;FNCUR$(3,46);MTGLZAHL;HIGHOFF$;FNCUR$(5,41);"Beruf:"
4951 GOSUB 11270:GOSUB 11490                            'LW replace with 11556?
4960 Q$="Wollen Sie diese Daten drucken lassen?":GOSUB 11900     'Ja/Nein-Frage
4970 IF JA=0 THEN 4770                                               'Drucken ?
4980 Q$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 11900 'Ja/Nein-Frage
4990 IF JA=0 THEN N$="Bitte schalten Sie den Drucker ein.":GOSUB 12510:DELAY=1000:GOSUB 10280:GOTO 4960 'Drucker bereit ? 'Nachricht 'Delay
5000 GOSUB 15880                          'Stammsatz drucken (Bildschirmformat)
5010 GOTO 4770
5020 '
5030 ' ######################### Verwendungsnachweis ##########################
5040 '
5050 GOSUB 11090                                              'Einlesen MTGLMAX
5070 VNFLAG=-1
5080 '
5090 ' Verwendungsnachweis-Menü ***********************************************
5100 '
5110 MT$="VERWENDUNGSNACHWEIS":MI$="G-01200":GOSUB 12580           'Basis Maske
5120 F=4                                                                'TODO ?
5130 FKEYS$="":GOSUB 12690                                      'F-Tastenleiste
5140 PRINT CURON$;
5150 PRINT HIGHOFF$;
5160 PRINT FNCUR$(4,20)"   Bitte geben Sie die Mitgliedsnummer"
5170 PRINT FNCUR$(5,20)"des zu ändernden Verwendungsnachweises ein"
5180 PRINT FNCUR$(6,20)" und beenden Sie die Eingabe mit RETURN."
5190 PRINT FNCUR$(8,20)"                 =>..."
5200 Z=8:S=39:LNG=3:TYP=2
5210 GOSUB 10410                                                         'INPUT
5220 IF FKEY=1 THEN GOSUB 17130:GOTO 5110
5230 IF FKEY=2 OR FKEY=3 THEN GOSUB 11044:VNFLAG=0:GOTO 1780      'Clear DAT$()
5240 IF FKEY=4 THEN GOSUB 12940
5250 IF FKEY>4 THEN 5200
5260 IF CKEY<>0 THEN GOTO 5200
5270 IF LEN(OUTTEXT$)=0 THEN N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 12510:GOTO 5190 ELSE MITGLIED=VAL(OUTTEXT$):IF MITGLIED=0 THEN 5210 'Nachricht
5280 IF MITGLIED>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12510:PRINT HOME$;HIGHOFF$:GOTO 5190 'Nachricht
5290 MTGLZAHL=MITGLIED:MTGL$=STR$(MITGLIED)
5300 '
5310 ' *** Einlesen des Mitgliedsnamens ***************************************
5320 '
5330 PRINT FNCUR$(21,1);DELLINE$;
5340 GET #2,MITGLIED                                        'EINLESEN DATEN STS
5350 NNAM$=LEFT$(DD1$,25)                                 'NACHNAME RAUSFISCHEN
5360 VNAM$=MID$(DD1$,26,25)                               'VORNAME  RAUSFISCHEN
5370 '
5380 ' ** LEERZEICHEN HINTER NACHNAME ABSCHNEIDEN *****************************
5390 '
5400 FOR I=LEN(NNAM$) TO 1 STEP -1
5410   IF MID$(NNAM$,I,1)<>" " THEN NNAM$=LEFT$(NNAM$,I):I=1
5420 NEXT
5430 NAMM$=NNAM$+" "+VNAM$
5440 GOSUB 13510                                          'EINLESEN 1. VN-BLATT
5450 BLATT=1
5460 VP=1:HP=1                                                 'INITIALISIERUNG
5470 F=8
5480 GOSUB 12580                                                   'Basis Maske
5490 FKEYS$="<F7>=Speichern":GOSUB 12690                        'F-Tastenleiste
5500 PRINT HIGHOFF$;
5510 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=Vorwärts";TAB(53);"<F9>=Blatt drucken";
5520 PRINT FNCUR$(3,2);"Blatt:"
5530 PRINT FNCUR$(5,2);"Mitglieds-Nr.:"
5540 PRINT FNCUR$(5,24);"Name:"
5550 MSK$="Von:  .  .   Bis:  .  .   Amt:"
5560 FOR KK=7 TO 19 
5570   PRINT FNCUR$(KK,2);MSK$;TAB(43);MSK$       'TODO No semicolon at the end
5580 NEXT
5590 GOSUB 14750                                                 'Ausgabe Daten
5600 LNG=2-(HP=7)*6                                    'BEGIN OF THE INPUT-LOOP
5610 TYP=2-(HP=7)
5620 Z=VP+13*(VP>13)+6
5630 S=6-39*(VP>13)-(HP<7)*(HP-1)*3+(HP>3)*(HP<7)*4-(HP=7)*26
5640 N$="":GOSUB 12510                         'ERASE MESSAGE-LINE   'Nachricht
5650 GOSUB 10410                                                       'INPUT+1
5660 IF HP<7 THEN IF ASCC=ASCRET THEN OUTTEXT$=STRING$(2-LEN(OUTTEXT$),"0")+OUTTEXT$   '+NULLEN
5670 IF CKEY<>0 OR ASCC=ASCRET THEN PRINT FNCUR$(Z,S);OUTTEXT$
5680 IF ASCC=ASCRET THEN IF HP<7 THEN MID$(DAT$(VP),-1+2*HP,2)=OUTTEXT$ ELSE DAT$(VP)=LEFT$(DAT$(VP),12)+OUTTEXT$+SPACE$(8-LEN(OUTTEXT$))
5690 IF ASCC=ASCRET AND HP<7 THEN JA=1:TEST$=OUTTEXT$:ON HP GOSUB 12270,12300,12330,12270,12300,12330:IF JA=0 THEN N$="Ungültiges Datum":GOSUB 12510:GOTO 5650 'Datums-Check 'Nachricht
5700 IF ASCC=ASCRET THEN IF HP=7 THEN HP=1:VP=VP-(VP<26)-(VP=26)*(-25):GOTO 5600 ELSE HP=HP+1:GOTO 5600     'NEUE SCREEN-POSITION
5710 IF CKEY<>0 THEN TEST$=MID$(DAT$(VP),-(HP<7)*2*HP-1-(HP=7)*14,-(HP<7)*2-(HP=7)*8):PRINT FNCUR$(Z,S);TEST$
5720 IF CKEY<>0 THEN IF HP<7 AND NOT TEST$="  " THEN JA=1:ON HP GOSUB 12270,12300,12330,12270,12300,12330:IF JA=0 THEN N$="Ungültiges Datum":GOSUB 12510:GOTO 5650 'Nachricht
5730 IF CKEY=CKEYDN THEN VP=VP-(VP<26)-(VP=26)*(-25):GOTO 5600
5740 IF CKEY=CKEYUP THEN VP=VP+(VP>1)-(VP=1)*25     :GOTO 5600
5750 IF CKEY=CKEYRT THEN HP=HP-(HP<7)-(HP=7)*(-6)   :GOTO 5600
5760 IF CKEY=CKEYLT THEN HP=HP+(HP>1)+(HP=1)*(-6)   :GOTO 5600
5770 IF FKEY=1 THEN GOSUB 17200 :GOTO 5460           'HILFE - ZU INIT UND MASKE
5780 IF FKEY=2 THEN GOTO 5110                                  'ZU MTGL-EINGABE
5790 IF FKEY=3 THEN VNFLAG=0:GOSUB 11044:GOTO 1780                'Clear DAT$()
5800 IF FKEY=4 THEN GOSUB 12940:GOSUB 12750:GOTO 5650 
5810 IF FKEY=5 THEN GOSUB 13600:GOTO 5600
5820 IF FKEY=6 THEN GOSUB 13760:GOTO 5600
5830 IF FKEY=7 THEN GOSUB 14410:GOTO 5650                 'SPEICHERN DANN INPUT
5840 IF FKEY=9 THEN GOSUB 14990:GOSUB 13960:GOTO 5640
5850 '
5860 ' ############# Mitgliedsliste drucken von 05*************** #############
5870 '
5880 DRUCKFLAG=-1
5890 MTGL$=""
5900 DATUM$="  .  .  "
5910 ' GOSUB 1680                                               'TODO Sad hack!
5930 GOSUB 11090:MTGLZAHL=MTGLMAX           'Einlesen MTGLZAHL TODO: necessary?
5935 GOSUB 11044                                 'Clear DAT$() TODO: Necessary?
5940 GOSUB 11090                                'Clear SUCH$() TODO: Necessary?
5950 '
5960 PRINT CLS$;
5970 PRINT HIGHOFF$;
5980 PRINT CUROFF$;
5990 MT$="MITGLIEDSLISTE DRUCKEN":MI$="G-01300":GOSUB 12580        'Basis Maske
6000 FKEYS$="":GOSUB 12690                                      'F-Tastenleiste
6010 PRINT FNCUR$(4,28);"1 - Suchen & Drucken"
6020 PRINT FNCUR$(6,28);"2 - Komplett drucken"
6030 PRINT FNCUR$(8,28);"3 - Datenblatt drucken"
6040 NKEYMAX=3:GOSUB 12840                                         'Menüeingabe
6050 IF FKEY=1 THEN GOSUB 17270:GOTO 5960
6060 IF FKEY=2 OR FKEY=3 THEN GOSUB 11044:DRUCKFLAG=0:GOTO 1780   'Clear DAT$()
6070 IF FKEY=4 THEN GOSUB 12940:GOTO 6040
6080 IF NKEY=1 THEN 6140
6090 IF NKEY=2 THEN DRUCKFLAG=0:GOTO 6140
6100 IF NKEY=3 THEN 6570
6110 '
6120 ' *** Suchen & Drucken, Komplett drucken *********************************
6130 '
6140 GOSUB 12580                                                   'Basis Maske
6150 GOSUB 12690                                                'F-Tastenleiste
6160 PRINT FNCUR$(7,13);"Geben Sie bitte das heutige Datum ein :   .  .  ":Z=7:LNG=2:TYP=2
6170 PRINT HIGHON$;CURON$
6180 FOR K=0 TO 2
6190   S=53+K*3
6200   VNFLAG=-1:GOSUB 10410:VNFLAG=0    'in order to activate right-left shift 'INPUT+1
6210   IF CKEY<>0 THEN JA=1:TEST$=MID$(DATUM$,K*3+1,2):ON K+1 GOSUB 12270,12300,12330:IF JA=0 THEN N$="Ungültiges Datum":GOSUB 12510:GOTO 6190 'Nachricht
6220   IF FKEY=1 THEN GOSUB 17270:GOTO 6140
6230   IF FKEY=2 THEN 5880
6240   IF FKEY=3 THEN GOSUB 11044:DRUCKFLAG=0:GOTO 1780           'Clear DAT$()
6250   IF FKEY=4 THEN GOSUB 12940:GOTO 6190
6260   IF CKEY=CKEYLT THEN IF K=0 THEN 6190 ELSE K=K-1:GOTO 6190
6270   IF CKEY=CKEYRT THEN IF K=2 THEN K=0:GOTO 6190 ELSE K=K+1:GOTO 6190
6280   IF ASCC<>13 THEN 6190
6290   OUTTEXT$=STRING$(2-LEN(OUTTEXT$),"0")+OUTTEXT$:PRINT FNCUR$(Z,S);OUTTEXT$
6300   TEST$=OUTTEXT$:JA=1:ON K+1 GOSUB 12270,12300,12330:IF JA=0 THEN N$="Ungültige Datumseingabe":GOSUB 12510:GOTO 6190 'Nachricht
6310   MID$(DATUM$,K*3+1,2)=OUTTEXT$
6320 NEXT
6330 PRINT HIGHOFF$;
6340 PRINT CUROFF$
6350 Q$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 11900   'Ja/Nein-Frage
6360 IF JA=0 THEN N$="":GOSUB 12510:GOTO 6140                        'Nachricht
6370 '
6380 ' *** Drucken Anfang *****************************************************
6390 '
6400 IF DRUCKFLAG THEN 6440
6410 PRINT FNCUR$(7,1);DELLINE$;
6420 PRINT CUROFF$
6430 N$="Mitgliedsliste wird gedruckt. Abbrechen mit Taste.":GOSUB 12510 'Nachricht
6440 IF DRUCKFLAG THEN 3590
6450 PRINT FNCUR$(23,1);DELLINE$;
6460 GOSUB 15290                                 'Mitgliedsliste drucken (Kopf)
6470 FOR MTGLZAHL=1 TO MTGLMAX
6480   GOSUB 11660:IF INSTR(DAT$(5),"ausgetreten")>0 THEN 6510
6490   GOSUB 15440                           'Mitgliedsliste drucken (Mitglied)
6500   IF ISBRKPRNT THEN ISBRKPRNT=0:N$="Drucken abgebrochen":GOSUB 12510:PRINT CUROFF$:GOTO 6520 ELSE GOSUB 12510:PRINT CUROFF$ 'Nachricht
6510 NEXT MTGLZAHL
6520 GOSUB 11044                                                  'Clear DAT$()
6530 DRUCKFLAG=0:GOTO 5880
6540 '
6550 ' *** Datenblatt drucken *************************************************
6560 '
6570 MT$="DATENBLATT DRUCKEN":MI$="G-01303":GOSUB 12580            'Basis Maske
6580 FKEYS$="":GOSUB 12690                                      'F-Tastenleiste
6590 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer"
6600 PRINT FNCUR$(5,24)"des zu druckenden Datenblattes ein."
6610 PRINT FNCUR$(7,23)"Beenden Sie die Eingabe mit RETURN"
6620 PRINT FNCUR$(9,37)"=>..."
6630 LNG=3:TYP=2:Z=9:S=39
6640 PRINT HIGHON$:GOSUB 10410:PRINT HIGHOFF$                          'INPUT+1
6650 IF FKEY=2 THEN 5880                                                'F-Keys
6660 IF FKEY=3 THEN GOSUB 11044: GOTO 1780                        'Clear DAT$()
6670 IF FKEY=1 THEN GOSUB 17270:PRINT CURON$:MT$="DATENBLATT DRUCKEN":GOTO 6570
6680 IF FKEY=4 THEN GOSUB 12940:GOTO 6570
6690 IF FKEY>4 THEN CKEY=CKEYDN 
6700 IF CKEY<>0 THEN N$="Ungültige Eingabe - Bitte wiederholen":GOSUB 12510:GOTO 6620 'Nachricht
6710 MTGLZAHL=VAL(OUTTEXT$):IF MTGLZAHL=0 THEN MTGLZAHL=MTGLMAX+1       'Errors
6720 IF MTGLZAHL>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12510:GOTO 6620 'Nachricht
6730 Q$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 11900        'Ja/Nein-Frage
6740 IF JA=0 THEN N$="Bitte schalten Sie den Drucker ein.":GOSUB 12510:DELAY=1000:GOSUB 10280:GOTO 6570     'Drucker bereit ? 'Nachricht 'Delay
6750 GOSUB 11660                                            'S.S.Daten einlesen
6760 NNAM$ = LEFT$(DD1$,25) : NNAM$ = LEFT$(NNAM$,INSTR(NNAM$," "))
6770 NAMM$ = NNAM$ + MID$(DD1$,26,25)
6780 GOSUB 15880                          'Stammsatz drucken (Bildschirmformat)
6790 GOSUB 13510                                            'V.N Daten einlesen
6800 GOSUB 15030                                             'V.N Daten drucken
6810 GOSUB 16130                                             'Mitgliedsvermerke
6820 GOTO 6570
10000 '
10010 ' ### Unterprogramme von 06*****, 07****, 08**** und 09**** #############
10020 '
10030 ' *** Tasten deaktivieren, ASCII Codes für Cursortasten vergeben
10040 '
10050 RESTORE 10130
10060 READ KEY
10070 IF KEY<0 THEN 10160
10080 ' Map unshifted logical key code KEY to ASCII code 0
10090   PRINT ESC$;"41";CHR$(KEY);CHR$(0);
10100 ' Map shifted logical key code KEY to ASCII code 0
10110   PRINT ESC$;"42";CHR$(KEY);CHR$(0);
10120 GOTO 10060
10130 DATA 11,26,27,32,33,47,48:     'ESC, CLR, DEL CHR, INT, TAB, INS, DEL/EOL
10140 DATA 53,67,68,73,94,97,-1:     'RVS,SCROL, LTRL, UNDL, RPT, CONT, Endflag
10150 ' Map unshifted logical cursor key codes to ASCII codes
10160 PRINT ESC$;"41";CHR$(88);CHR$(ASCCUP);            'Key 88 "Cursor up"   .
10170 PRINT ESC$;"41";CHR$(89);CHR$(ASCCDN);            'Key 89 "Cursor down" .
10180 PRINT ESC$;"41";CHR$(98);CHR$(ASCCLT);            'Key 98 "Cursor left" .
10190 PRINT ESC$;"41";CHR$(99);CHR$(ASCCRT);            'Key 99 "Cursor right".
10200 ' Map unshifted and shifted logical tab key codes to ASCII code 'Escape '
10210 PRINT ESC$;"41";CHR$(33);CHR$(ASCESC);                  'Key 33 "Escape".
10220 PRINT ESC$;"42";CHR$(33);CHR$(ASCESC);                  'Key 33 "Escape".
10230 RETURN
10240 '
10250 ' *** Delay *************************************************************
10260 '
10270 ' IN: DELAY
10280 FOR DD=0 TO DELAY:NEXT DD
10290 RETURN
10300 '
10310 ' *** Eingaberoutine (INPUT) ********************************************
10320 '
10330 ' IN: Z=Zeile     OUT: MUSS$    TMP: P
10340 ' IN: S=Spalte    OUT: OUTTEXT$ TMP: VALID (1=true 0=false)
10350 ' IN: LNG= Länge  OUT: CKEY
10360 ' IN: TYP=Typ     OUT: FKEY
10370 ' IN: VNFLAG      OUT: ASCC
10380 ' IN: INTEXT$
10390 ' IN: FKEYMASK$
10400 '
10410 P=LEN(INTEXT$)
10420 PRINT FNCUR$(Z,S);HIGHON$;INTEXT$;HIGHOFF$;STRING$(LNG-P,".");
10440 OUTTEXT$=INTEXT$
10450 PRINT CURON$;
10451 FEHLER=0
10460 CKEY=0:FKEY=0                    'TODO ??? Entry point from EXIT Erfassen
10470 PRINT FNCUR$(Z,S+P);
10480 A$=INPUT$(1):ASCC=ASC(A$)
10481 IF FEHLER THEN N$="":GOSUB 12510:PRINT FNCUR$(Z,S+P);:FEHLER=0
10490 ON TYP GOSUB 12020,12090,12150,12210                          'Type check
10500 IF VALID=0 THEN 10560
10510   IF P>=LNG THEN 10480                                   'Valid character
10520   PRINT HIGHON$;A$;HIGHOFF$;
10530   OUTTEXT$=OUTTEXT$+A$
10540   P=P+1
10550   GOTO 10480
10560 IF ASCC<>ASCBSP THEN 10620
10570   IF P<1 THEN 10480                                            'Backspace
10580   PRINT DEL$;
10590   P=P-1
10600   OUTTEXT$=LEFT$(OUTTEXT$,P)
10610   GOTO 10480
10620 IF ASCC=ASCESC THEN 10410                                         'Escape
10630 IF ASCC=ASCRET THEN 10730
10640 IF ASCC=ASCCUP THEN CKEY=CKEYUP:GOTO 10725
10650 IF ASCC=ASCCDN THEN CKEY=CKEYDN:GOTO 10725
10660 IF VNFLAG=0 THEN 10690
10670   IF ASCC=ASCCLT THEN CKEY=CKEYLT:GOTO 10725 
10680   IF ASCC=ASCCRT THEN CKEY=CKEYRT:GOTO 10725 
10690 IF ASCC<ASCF1 OR ASCC>ASCF9 THEN 10700                            'F-Keys
10691   FKEY=ASCC-ASCF1+1
10692   IF INSTR(FKEYMASK$,CHR$(FKEY+ASC1-1))>0 THEN 10730
10700 N$="Ungültige Eingabe - Bitte wiederholen":GOSUB 12510         'Nachricht
10701 FEHLER=1
10710 GOTO 10470
10720 '
10725 GOSUB 10780                                     'Block cursor auto-repeat
10730 PRINT CUROFF$                 'LW NO semicolon else the screen garbles up
10740 RETURN 
10750 '
10760 ' *** Block cursor auto-repeat ******************************************
10770 '
10780 A$=INKEY$:IF A$>=CHR$(ASCCUP) THEN 10780
10790 RETURN
10791 '
10792 ' *** Debounce keyboard
10793 '
10794 FOR K=1 TO 30:A$=INKEY$:NEXT K
10795 RETURN
10800 '
10810 ' *** Einlesen Stammdatenfelder-Definition ******************************
10820 '
10830 OPEN "I",#1,"B:FELD.DAT"                        
10840   INPUT #1,TMPFELDMAX                                             'ignore
10850   FOR I=1 TO FELDMAX
10860     INPUT #1,F$
10870     FENAM$(I)=LEFT$(F$,14)
10880     FEMUSS(I)=-(MID$(F$,13,1)="=")
10890     FECOL(I)=VAL(MID$(F$,15,2))
10900     FEROW(I)=VAL(MID$(F$,17,2))
10910     FELNG(I)=VAL(MID$(F$,19,2))
10920     FETYP(I)=VAL(RIGHT$(F$,1)) 
10930   NEXT
10940 CLOSE #1
10950 RETURN
11041 '
11042 ' *** Clear DAT$()
11043 '
11044 FOR K=1 TO FELDMAX:DAT$(K)="":NEXT K
11045 RETURN
11046 '
11047 ' *** Clear SUCH$()
11048 '
11049 FOR K=1 TO FELDMAX:SUCH$(K)="":NEXT K
11050 RETURN
11059 '
11060 ' *** Lesen MTGLMAX
11070 '
11080 ' OUT: MTGLMAX
11090 OPEN "I",#1,"B:MTGLZAHL.DAT"
11100   INPUT #1,MTGLMAX$
11110   MTGLMAX=VAL(MTGLMAX$)
11120 CLOSE #1
11130 RETURN
11140 '
11150 ' *** Schreiben MTGLMAX
11160 '
11170 ' IN: MTGLMAX
11180 OPEN "O",#1,"B:MTGLZAHL.DAT"
11190   PRINT #1,STR$(MTGLMAX)
11200 CLOSE #1
11210 RETURN
11220 '
11230 ' *** Maske Feldnamen und Eingabefelder (Pünktchen)
11240 '
11250 ' IN: FELDMAX
11260 ' IN: MTGLZAHL
11270 FOR K=1 TO FELDMAX
11280   Z=FEROW(K)
11290   S=FECOL(K)
11300   PRINT FNCUR$(Z,S);FENAM$(K)
11310   S=S+XOFF
11320   PRINT FNCUR$(Z,S);STRING$(FELNG(K),".")
11330 NEXT
11340 RETURN
11450 '
11460 ' *** Maske Feldwerte
11470 '
11480 ' IN: FELDMAX
11490 PRINT HIGHON$
11500 FOR K=1 TO FELDMAX
11510   Z=FEROW(K)
11520   S=FECOL(K)+XOFF
11530   PRINT FNCUR$(Z,S);DAT$(K)
11540 NEXT
11550 PRINT HIGHOFF$
11551 RETURN
11552 '
11553 ' *** Maske Feldnamen, Eingabefelder und Filler
11554 '
11555 ' IN: FELDMAX
11556 FOR K=1 TO FELDMAX
11557   Z=FEROW(K)
11558   S=FECOL(K)
11559   PRINT FNCUR$(Z,S);FENAM$(K)
11560   S=S+XOFF
11561   PRINT FNCUR$(Z,S);HIGHON$;DAT$(K);HIGHOFF$;
11562   PRINT STRING$(FELNG(K)-LEN(DAT$(K)),".");
11565 NEXT
11566 RETURN
11570 '
11580 ' *** FIELDen der Stammsatz-Datei
11590 '
11600 OPEN "R",#2,"B:MV.DAT",462
11610 FIELD #2,236 AS DD1$,225 AS DD2$
11620 RETURN
11630 '
11640 ' *** Einlesen der Felddaten von Disk ***********************************
11650 '
11660 GET #2,MTGLZAHL
11670 FOR LL=1 TO FELDMAX:DAT$(LL)="":NEXT
11680 HLANG=1
11690 FOR K=1 TO 14:LNG=FELNG(K):DAT$(K)=MID$(DD1$,HLANG,LNG):HLANG=LNG+HLANG:NEXT
11700 HLANG=1
11710 FOR K=15 TO 27:LNG=FELNG(K):DAT$(K)=MID$(DD2$,HLANG,LNG):HLANG=LNG+HLANG:NEXT
11720 RETURN
11730 '
11740 ' *** Speichern der Felddaten auf Disk **********************************
11750 '
11760 ' IN: MTGLZAHL
11770 FOR K=1 TO 27:LNG=FELNG(K):DAT$(K)=DAT$(K)+SPACE$(LNG-LEN(DAT$(K))):NEXT
11780 HH$=""
11790 FOR K=1 TO 14:HH$=HH$+DAT$(K):NEXT:LSET DD1$=HH$
11800 HH$="":FOR K=15 TO 27:HH$=HH$+DAT$(K):NEXT:LSET DD2$=HH$:HH$=""
11810 PRINT FNCUR$(21,1);
11820 PUT #2,MTGLZAHL
11830 N$="Die Daten sind auf Diskette abgespeichert.":GOSUB 12510    'Nachricht 'TODO: Too may messages queued?
11840 RETURN
11850 '
11860 ' *** Ja/Nein - Frage ***************************************************
11870 '
11880 ' IN:  Q$ - Frage
11890 ' OUT: JA - (1=yes, 0=no)
11900 N$=Q$+" (J=Ja/N=Nein)":GOSUB 12510                             'Nachricht
11910 PRINT FNCUR$(23,1);DELLINE$             'Delete F-keys 'TODO: Entry-Point
11920 PRINT FNCUR$(24,1);DELLINE$; 'LW DON'T remove semicolon -> screen scrolls up 
11930 PRINT HOME$                                             'TODO: Necessary?
11940 ANSWER$=INPUT$(1)
11950 PRINT BSP$;                                             'TODO: Necessary?
11960 IF ANSWER$="J" OR ANSWER$="j" THEN JA=1:RETURN
11970 IF ANSWER$="N" OR ANSWER$="n" THEN JA=0:RETURN
11980 GOTO 11940
11990 '
12000 ' *** Testbereich 1 (Alpha) *********************************************
12010 '
12020 VALID=0
12030 IF ASCC>57 AND ASCC<127 THEN VALID=1:GOTO 12050
12040 IF ASCC>31 AND ASCC<48 THEN VALID=1
12050 RETURN
12060 '
12070 ' *** Testbereich 2 (Numerisch) *****************************************
12080 '
12090 VALID=0
12100 IF ASCC>47 AND ASCC<58 THEN VALID=1
12110 RETURN
12120 '
12130 ' *** Testbereich 3 (Alphanumerisch) ************************************
12140 '
12150 VALID=0
12160 IF ASCC>31 AND ASCC<127 THEN VALID=1
12170 RETURN
12180 '
12190 ' *** Testbereich 4 (Numerisch + "/" und ".") ***************************
12200 '
12210 VALID=0
12220 IF ASCC>45 AND ASCC<58 THEN VALID=1
12230 RETURN
12240 '
12250 ' *** DATUMS-TEST (bei VN und M-Liste drucken) **************************
12260 '
12270 IF VAL(TEST$)=0 OR VAL(TEST$)>31 THEN JA=0                           'Tag
12280 RETURN
12290 '
12300 IF VAL(TEST$)=0 OR VAL(TEST$)>12 THEN JA=0                         'Monat
12310 RETURN
12320 '
12330 IF VAL(TEST$)=0 THEN JA=0                                           'Jahr
12340 RETURN
12350 '
12360 ' *** Voreinstellungen im ERFASSEN  (Defaults) **************************
12370 '
12380 DAT$(5)="10***"
12390 DAT$(7)=""11**""
12400 DAT$(11)="12**"
12410 DAT$(12)="13******"
12420 DAT$(21)="3"
12430 DAT$(22)="Nein"
12440 DAT$(23)="Nein"
12450 DAT$(25)="Nein"
12460 RETURN
12470 '
12480 ' *** Nachricht anzeigen *********************************************OK*
12490 '
12500 ' IN: N$ - Nachricht
12510 PRINT FNCUR$(21,2);DELLINE$;
12511 IF N$<>"" THEN PRINT INVON$;" ";N$;" ";INVOFF$;
12512 PRINT                              'LW workaround quirk in display driver 
12520 RETURN
12530 '
12540 ' *** Bildschirmmaske - Basis ****************************************OK*
12550 '
12560 ' IN: MT$ - Menu title
12570 ' IN: MI$ - Menu ID
12580 PRINT CLS$;
12590 PRINT "00**********************";
12600 PRINT FNCUR$(1,(80-LEN(MT$))/2);MT$;FNCUR$(1,73);MI$
12610 PRINT STRICH$
12620 PRINT FNCUR$(21,1);
12630 PRINT STRICH$             'quirk in display driver places this in line 22
12640 RETURN
12650 '
12660 ' *** Funktionstasten Leiste (KEY LABELS) *******************************
12670 '
12680 ' IN: FKEYS$ - Additional function key labels 
12690 PRINT FNCUR$(23,1);"<F1>=Hilfe <F2>=Zurück <F3>=Anfang <F4>=Hauptmenü ";
12700 PRINT FKEYS$         'LW NO semicolon after FKEY$ else display is garbled
12710 RETURN
12720 '
12730 ' *** restliche funktionstasten bei Verw.-nachweis ausgeben
12740 '
12750 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=Vorwärts             <F8>=Drucken";
12760 PRINT HOME$;" "
12770 RETURN
12780 '
12790 ' *** Eingabe bei Menübildern *******************************************
12800 '
12810 ' IN:  NKEYMAX - max valid numeric key
12820 ' OUT: FKEY - number of pressed function key or 0
12830 ' OUT: NKEY - number of pressed numeric key or 0
12840 NKEY=0:FKEY=0
12850 GOSUB 10794                                            'Debounce keyboard
12860 A$=INPUT$(1):ASCC=ASC(A$)
12870 IF ASCC>=ASC1 AND ASCC<=ASC1+NKEYMAX-1 THEN NKEY=ASCC-ASC1+1:RETURN
12880 IF ASCC>=ASCF1 AND ASCC<=ASCF4 THEN FKEY=ASCC-ASCF1+1:RETURN
12890 N$="Sie haben eine falsche Taste gedrückt - Bitte neue Auswahl treffen.":GOSUB 12510 'Nachricht
12900 GOTO 12860
12910 '
12920 ' *** Rückkehr ins Hauptmenü ? ******************************************
12930 '
12940 Q$="Sie wollen wirklich ins Hauptmenü?":GOSUB 11900        'Ja/Nein-Frage
12950 IF JA AND MT$="STAMMSATZ ÄNDERN" THEN GOSUB 13140                    '???
12960 IF JA=1 THEN 17480                                          'Programmende
12961 N$="":GOSUB 12510                                      'Nachricht löschen 
12962 GOSUB 12690                                  'F-Keys TODO: is FKEYS$ set?
12963 RETURN
12970 '
12980 ' *** Rückkehr aus Hilfe-Text? ***************************************OK*
12990 '
13000 N$="Zurück? (J=Ja)":GOSUB 12510                                'Nachricht
13010 GOSUB 11910                                          'TODO why not *****?
13020 IF JA=0 THEN 13000
13030 RETURN
13110 '
13120 ' *** UPRO FÜR ÄNDERN-EXIT **********************************************
13130 '
13140 Q$="Wollen Sie die Daten speichern?":GOSUB 11900           'Ja/Nein-Frage
13150 IF JA THEN GOSUB 11770
13160 RETURN
13170 '
13180 ' **** VN - Subroutines, permitting one page handling *******************
13190 ' ****         by 04********** 21*******              *******************
13200 '
13210 ' **** OPEN & FIELDING VERWENDUNGSNACHWEIS ******************************
13220 '
13230 OPEN "R",#3,"B:VN.NDX",4
13240 FIELD #3,4 AS POINT0$
13250 OPEN "R",#4,"B:VN.POI",8
13260 FIELD #4,4 AS POINT1$, 4 AS POINT2$
13270 OPEN "R",#5,"B:VN.DAT",560
13280 FIELD #5,240 AS DDD1$,240 AS DDD2$,80 AS DDD3$
13290 RETURN
13300 '
13310 ' *** VERWENDUNGSNACHWEIS FÜR NEUES MITGLIED ANLEGEN ********************
13320 '
13330 ' IN: MTGLZAHL
13340 GET #3,1                            'letzte Seitennummer einlesen dann +1
13350 TMPOINT=VAL(POINT0$)+1
13360 LSET DDD1$=""                                    'leeren Record speichern
13370 LSET DDD2$=""
13380 LSET DDD3$=""
13390 PUT #5,TMPOINT
13400 LSET POINT0$=STR$(TMPOINT)             'letzte Seitennummer neu speichern
13410 PUT #3,1
13420 PUT #3,MTGLZAHL+1
13430 LSET POINT1$="0000"
13440 LSET POINT2$="0000"
13450 PUT #4,TMPOINT
13460 RETURN
13470 '
13480 ' **** ERSTE SEITE FINDEN & LESEN ***************************************
13490 '
13500 ' IN: MTGLZAHL
13510 GET #3,MTGLZAHL+1                                            'Index_Datei
13520 CPOINT=VAL(POINT0$)
13530 GET #5,CPOINT:GOSUB 14590                                       'Einlesen
13540 BLATT=1
13550 GET #4,CPOINT                                              'Pointer lesen
13560 RETURN
13570 '
13580 ' **** SEITE RÜCKWÄRTS **************************************************
13590 '
13600 TMPOINT=VAL(POINT1$)
13610 IF TMPOINT<>0 THEN 13650
13620 N$="Erste Seite":GOSUB 12510                                   'Nachricht
13630 DELAY=9999:GOSUB 10280                                             'Delay
13640 RETURN
13650 CPOINT=TMPOINT
13660 BLATT=BLATT-1
13670 GET #5,CPOINT:GOSUB 14590   
13680 GET #4,CPOINT:GOSUB 14750                             'Neue Pointer lesen
13690 HP=1:VP=1
13700 GOSUB 13960
13710 DELAY=100:GOSUB 10280                                              'Delay
13720 RETURN
13730 '
13740 ' **** SEITE VORWÄRTS  **************************************************
13750 '
13760 TMPOINT=VAL(POINT2$)
13770 IF TMPOINT=0 THEN 13840
13780 CPOINT=TMPOINT
13790 BLATT=BLATT+1
13800 GET #5,CPOINT:GOSUB 14590
13810 GET #4,CPOINT:GOSUB 14750
13820 HP=1:VP=1
13830 RETURN
13840 N$="Letzte Seite"
13850 IF INSTR(SPACE$(20),DAT$(26))=0 THEN 13890 
13860 GOSUB 12510                                                    'Nachricht
13870 DELAY=20000:GOSUB 10280                                            'Delay
13880 RETURN
13890 Q$="Letzte Seite - Neue Seite anlegen?":GOSUB 11900        'Ja/Nein-Frage
13900 N$="":GOSUB 12510                                              'Nachricht
13910 IF JA=0 THEN 13960
13920 GOSUB 14200
13930 BLATT=BLATT+1
13940 GOSUB 14750
13950 HP=1:VP=1
13960 GOSUB 12690
13970 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=vorwärts";
13980 PRINT TAB(54);"<F9>=Blatt drucken";
13990 PRINT HOME$;" "
14000 RETURN
14010 '
14020 ' **** ERSTE SEITE EINES MITGLIEDS ANLEGEN ******************************
14030 '
14040 GET #3,1
14050 LSET POINT0$=STR$(VAL(POINT0$)+1)                                  'NDX+1
14060 PUT #3,1
14070 LSET POINT1$="0000"                                        'Page pointers
14080 LSET POINT2$="0000"
14090 PUT #4,VAL(POINT0$)
14100 LSET POINT0$=POINT0$                                          'First page
14110 PUT #3,MTGLZAHL+1
14120 LSET DDD1$=""                                                 'Datenblatt
14130 LSET DDD2$=""
14140 LSET DDD3$=""
14150 PUT #5,VAL(POINT0$)
14160 RETURN
14170 '
14180 ' **** ALLGEMEIN EINE NEUE SEITE ANLEGEN ********************************
14190 '
14200 GET #3,1                                                           'NDX+1
14210 LSET POINT0$=STR$(VAL(POINT0$)+1)
14220 PUT #3,1:TMPOINT$=POINT0$    'Vektoren der bisher letzten Seite verbiegen
14230 LSET POINT2$=POINT0$                                'Verbiege Vektoren...
14240 PUT #4,CPOINT                                         '...der alten Seite
14250 LSET POINT1$=STR$(CPOINT)                             '...der neuen Seite
14260 LSET POINT2$="0000"                                                '.. ""
14270 CPOINT=VAL(TMPOINT$)                                       'Nr neue Seite
14280 PUT #4,CPOINT
14290 LSET DDD1$=""                                           'Write empty page
14300 LSET DDD2$=""
14310 LSET DDD3$="" 
14320 PUT #5,CPOINT
14330 DATEN$=SPACE$(20)
14340 FOR II=1 TO 26
14350   DAT$(II)=DATEN$
14360 NEXT
14370 RETURN
14380 '
14390 ' **** SEITE SPEICHERN **************************************************
14400 '
14410 DATEN$=""
14420 FOR II=1 TO 12
14430   DATEN$=DATEN$+DAT$(II)
14440 NEXT
14450 LSET DDD1$=DATEN$:DATEN$=""
14460 FOR II=13 TO 24
14470   DATEN$=DATEN$+DAT$(II)
14480 NEXT
14490 LSET DDD2$=DATEN$:DATEN$=""
14500 FOR II=25 TO 26
14510   DATEN$=DATEN$+DAT$(II)
14520 NEXT 
14530 LSET DDD3$=DATEN$:DATEN$=""
14540 PUT #5,CPOINT
14550 RETURN
14560 '
14570 ' **** HACK ! (ZERSTÜCKELN DER DATEN-STRINGS) ***************************
14580 '
14590 FOR II=1 TO 12
14600   DAT$(II)=MID$(DDD1$,(II-1)*20+1,20)
14610 NEXT
14620 FOR II=13 TO 24
14630   DAT$(II)=MID$(DDD2$,(II-13)*20+1,20)
14640 NEXT 
14650 FOR II=25 TO 26
14660   DAT$(II)=MID$(DDD3$,(II-25)*20+1,20)
14670 NEXT 
14680 RETURN
14690 '
14700 ' **** UPRO DATENAUSGABE VN  ********************************************
14710 '
14720 ' IN: MTGLZAHL
14730 ' IN: NAMM$
14740 ' IN: BLATT
14750 PRINT DELHIGH$;                                          'TODO Necessary?
14760 PRINT HIGHON$
14770 PRINT FNCUR$(5,16);MTGLZAHL
14780 PRINT FNCUR$(5,29);NAMM$
14790 PRINT FNCUR$(3,8);BLATT
14800 FOR K=1 TO 26
14810   HOR=6-39*(K>13)
14820   VER=6+K+13*(K>13)
14830   FOR J=1 TO 3                                                 'Von-Datum
14840     PRINT FNCUR$(VER,HOR+(J-1)*3);MID$(DAT$(K),-1+J*2,2)
14850   NEXT
14860   FOR J=1 TO 3                                                 'Bis-Datum
14870     PRINT FNCUR$(VER,HOR+13+(J-1)*3);MID$(DAT$(K),5+J*2,2)
14880   NEXT
14890   PRINT FNCUR$(VER,HOR+26);MID$(DAT$(K),13,8)                        'Amt
14900 NEXT
14910 PRINT HIGHOFF$
14920 RETURN
14930 '
14940 ' *** Verwendungsnachweis drucken ***************************************
14950 '
14960 ' IN: BLATT
14970 ' IN: MITGLZAHL
14980 ' IN: NAMM$
14990 Q$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 11900 'Ja/Nein-Frage TODO Back to main code?
15000 IF JA=0 THEN 15220
15010 LIND$=SPACE$(2)
15020 '
15030 LPRINT STRICH$
15040 LPRINT
15050 LPRINT LIND$;"Blatt:";BLATT
15060 LPRINT
15070 LPRINT LIND$;"Mitglieds-Nr.:";MTGLZAHL;TAB(24);"Name:";NAMM$
15080 LPRINT
15090 FOR K=1 TO 13
15100   KK=K:GOSUB 15190
15110   LPRINT LIND$;"Von:";VON$;" Bis:";BIS$;" Amt:";AMT$;
15120   KK=K+13:GOSUB 15190
15130   LPRINT " Von:";VON$;" Bis:";BIS$;" Amt:";AMT$
15140 NEXT 
15150 LPRINT
15160 LPRINT STRICH$
15170 RETURN
15180                                           'Format: VVVVVV BBBBBB AAAAAAAA
15190 VON$=LEFT$(DAT$(KK),2)+"."+MID$(DAT$(KK),3,2)+"."+MID$(DAT$(KK),5,2)
15200 BIS$=MID$(DAT$(KK),7,2)+"."+MID$(DAT$(KK),9,2)+"."+MID$(DAT$(KK),11,2)
15210 AMT$=RIGHT$(DAT$(KK),8)
15220 RETURN
15230 '
15240 ' ### Unterprogramme "Mitgliedsliste drucken" ###########################
15250 '
15260 ' *** Mitgliedsliste drucken (Kopf)
15270 '
15280 ' IN: DATUM$
15290 LPRINT TAB(16);"MITGLIEDSLISTE 25*******************************"
15300 LPRINT 
15310 LPRINT TAB(32);"Stand:";DATUM$
15320 LPRINT
15330 LPRINT TAB(3);"Mitgl."; 
15340 LPRINT TAB(10);"Name";
15350 LPRINT TAB(36);"Vorname, Anschrift";
15360 LPRINT TAB(63);"Status";
15370 LPRINT TAB(71);"Amt"
15380 RETURN
15390 '
15400 ' *** Mitgliedsliste drucken (Mitglied)
15410 '
15420 ' IN:  MTGLZAHL
15430 ' OUT: ISBRKPRNT (1=stop printing, 0=continue printing)
15440 AMT$=""
15450 IN=INSTR(DAT$(6),"22**"):IF IN<>0 THEN AMT$=MID$(DAT$(6),IN,6):GOTO 15490
15460 IN=INSTR(DAT$(6),"23**"):IF IN<>0 THEN AMT$=MID$(DAT$(6),IN,5):GOTO 15490
15470 IN=INSTR(DAT$(6),"24**"):IF IN<>0 THEN AMT$=MID$(DAT$(6),IN,3)
15480 '
15490 LPRINT
15500 LPRINT TAB(5);MTGLZAHL;
15510 LPRINT TAB(10);DAT$(1);                                         'Nachname
15520 LPRINT TAB(36);DAT$(2);                                          'Vorname
15530 LPRINT TAB(65);"(";LEFT$(DAT$(5),1);")";                          'Status
15540 LPRINT TAB(71);AMT$                                                  'Amt
15550 LPRINT TAB(36);DAT$(10)                                           'Straße
15560 LPRINT TAB(36);DAT$(11);                                    'Postleitzahl
15570 LPRINT TAB(41);DAT$(12)                                              'Ort
15580 LPRINT TAB(36);"Tel.:";DAT$(13)                                  'Telefon
15590 LPRINT
15600 '
15610 IF INKEY$="" THEN 15640          'TODO Move back to main code? Used twice
15620 Q$="Drucken abbrechen?":GOSUB 11900                        'Ja/Nein-Frage
15630 IF JA THEN ISBRKPRNT=1 ELSE ISBRKPRNT=0
15640 RETURN
15650 '
15660 ' *** Adressaufkleber drucken
15670 '
15680 N$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 11900 'Ja/Nein-Frage
15690 IF JA=0 THEN RETURN
15700 LIND$=SPACE$(3)
15710 '
15720 LPRINT LIND$;DAT$(7)                                              'Anrede
15730 LPRINT LIND$;LEFT$(DAT$(2),INSTR(DAT$(2)," "));DAT$(1)  'Vorname Nachname
15740 LPRINT
15750 LPRINT LIND$;DAT$(10)                                  'Straße Hausnummer
15760 LPRINT
15770 LPRINT LIND$;DAT$(11);" ";DAT$(12)                      'Postleitzahl Ort
15780 LPRINT
15790 LPRINT
15800 LPRINT
15810 LPRINT
15820 LPRINT
15830 LPRINT
15840 RETURN
15850 '
15860 ' *** Stammsatz drucken (Bildschirmformat)
15870 '
15880 RESTORE 15990
15890 LPRINT STRICH$
15900 LPRINT
15910 LPRINT TAB(30);"Mitgliedsnummer  ";MTGLZAHL
15920 LPRINT
15930 LPRINT TAB(2);FENAM$(1);DAT$(1);TAB(41);"Beruf:"
15940 FOR I=1 TO 6:READ A,B
15950   LPRINT TAB(2);FENAM$(A);DAT$(A);
15960   IF B=0 THEN LPRINT:GOTO 15980
15970   LPRINT TAB(41);FENAM$(B);DAT$(B);
15980 NEXT
15990 DATA 2,15,3,16,4,0,5,17,6,18,0,19
16000 FOR I=7 TO 14
16010   LPRINT TAB(2);FENAM$(I);DAT$(I);
16020   LPRINT TAB(41);FENAM$(I+13);DAT$(I+13);
16030 NEXT
16040 LPRINT
16050 LPRINT
16060 LPRINT
16070 LPRINT STRICH$
16080 RETURN
16090 '
16100 ' *** Mitgliedsvermerke drucken
16110 '
16120 ' IN: NAMM$ =
16130 LIND$=SPACE$(9)
16140 SLOT1$="___"
16150 SLOT2$="___ / ___"
16160 SLOT3$="___ / ___ / ___"
16170 '
16180 LPRINT TAB(10);"Mitgliedsnr.: ";SLOT1$;
16190 LPRINT TAB(28);"Name: ";NAMM$;
16200 LPRINT TAB(65);"Datum: ";SLOT3$
16210 LPRINT
16220 LPRINT LIND$;"26*************************** ";SLOT3$
16230 LRPINT
16240 LPRINT LIND$;"27*************************** ";SLOT3$
16250 LRPINT
16260 LPRINT LIND$;"28*************************** ";SLOT3$
16270 LRPINT
16280 LPRINT LIND$;"29*************************** ";SLOT3$
16290 LRPINT
16300 LPRINT LIND$;"30*************************** ";SLOT2$
16310 LRPINT
16320 LPRINT LIND$;"31*************************** ";SLOT3$
16330 LRPINT
16340 LPRINT LIND$;"32*************************** ";SLOT3$
16350 LRPINT
16360 LPRINT LIND$;"33*************************** ";SLOT1$
16370 LRPINT
16380 LPRINT LIND$;"34*************************** ";SLOT3$
16390 LRPINT
16400 LPRINT LIND$;"35*************************** ";SLOT3$
16410 LRPINT
16420 LPRINT LIND$;"36*************************** ";SLOT3$
16430 LRPINT
16440 LPRINT LIND$;"37*************************** ";SLOT3$
16450 LRPINT
16460 LPRINT LIND$;"38*************************** ";SLOT3$
16470 RETURN
16480 '
16490 ' ### Hilfe-Texte #######################################################
16500 '
16510 ' *** Hilfe-Text "Mitgliederverwaltung" 
16520 '
16530 MT$="HILFE":GOSUB 12580                                      'Basis Maske
16540 PRINT HOME$
16550 GOSUB 13000                                      'Rückkehr aus Hilfe-Text
16560 RETURN
16570 '
16580 ' *** Hilfe-Text "Stammsatzpflege" 
16590 '
16600 MT$="HILFE":GOSUB 12580                                      'Basis Maske
16610 PRINT HOME$
16620 GOSUB 13000                                      'Rückkehr aus Hilfe-Text
16630 RETURN
16640 '
16650 ' *** Hilfe-Text "Stammsatz anlegen"
16660 '
16670 SAVEMT$=MT$:MT$="HILFE":GOSUB 12580:MT$=SAVEMT$              'Basis Maske
16680 PRINT HOME$
16690 GOSUB 13000                                      'Rückkehr aus Hilfe-Text
16700 DATENAUSG=1                                                          '???
16710 RETURN
16720 '
16730 ' *** Hilfe-Text "Stammsatz ändern (I)"
16740 '
16750 MT$="HILFE":GOSUB 12580                                      'Basis Maske
16760 PRINT HOME$
16770 GOSUB 13000                                      'Rückkehr aus Hilfe-Text
16780 RETURN
16790 '
16800 ' *** Hilfe-Text "Stammsatz ändern (II)"
16810 '
16820 SAVEMT$=MT$:MT$="HILFE":GOSUB 12580:MT$=SAVEMT$              'Basis Maske
16830 PRINT HOME$
16840 GOSUB 13000                                      'Rückkehr aus Hilfe-Text
16850 GOSUB 12580                                                  'Basis Maske
16860 GOSUB 12690                                               'F-Tastenleiste
16870 RETURN
16880 '
16890 ' *** Hilfe-Text "Stammsatz löschen"
16900 '
16910 MT$="HILFE":GOSUB 12580                                      'Basis Maske
16920 PRINT HOME$
16930 GOSUB 13000                                      'Rückkehr aus Hilfe-Text
16940 RETURN
16950 '
16960 ' *** Hilfe-Text "Stammsatz suchen"
16970 '
16980 IF DRUCKFLAG THEN 17270                                              '???
16990 SAVEMT$=MT$:MT$="HILFE":GOSUB 12580:MT$=SAVEMT$              'Basis Maske
17000 PRINT HOME$
17010 GOSUB 13000                                      'Rückkehr aus Hilfe-Text
17020 RETURN
17030 ' 
17040 ' *** Hilfe-Text für "Stammsatz drucken"
17050 '
17060 SAVEMT$=MT$:MT$="HILFE":GOSUB 12580:MT$=SAVEMT$              'Basis Maske
17070 PRINT HOME$
17080 GOSUB 13000                                      'Rückkehr aus Hilfe-Text
17090 RETURN
17100 '
17110 ' *** Hilfe-Text "Verwendungsnachweis (I)"
17120 '
17130 MT$="HILFE":GOSUB 12580                                      'Basis Maske
17140 PRINT HOME$
17150 GOSUB 13000                                      'Rückkehr aus Hilfe-Text
17160 RETURN
17170 '
17180 ' *** Hilfe-Text "Verwendungsnachweis (II)"
17190 '
17200 SAVEMT$=MT$:MT$="HILFE":GOSUB 12580:MT$=SAVEMT$              'Basis Maske
17210 PRINT HOME$
17220 GOSUB 13000                                      'Rückkehr aus Hilfe-Text
17230 RETURN
17240 '
17250 ' *** Hilfe-Text "Mitgliedsliste drucken"
17260 '
17270 SAVEMT$=MT$:MT$="HILFE":GOSUB 12580:MT$=SAVEMT$              'Basis Maske
17280 PRINT HOME$
17290 GOSUB 13000                                      'Rückkehr aus Hilfe-Text
17300 RETURN
17310 '
17320 ' *** Programmfehler
17330 '
17340 LIND$=SPACE$(15)
17350 GOSUB 12580                                                  'Basis Maske
17360 PRINT HOME$
17370 PRINT LIND$;"Ein Programmfehler ist aufgetreten."
17380 PRINT   
17390 PRINT LIND$;"- Notieren Sie folgende Fehlerinformation:"
17400 PRINT LIND$;"    ";HIGHON$;"ERL";HIGHOFF$;" (Error Line)";HIGHON$;ERL;HIGHOFF$
17410 PRINT LIND$;"    ";HIGHON$;"ERR";HIGHOFF$;" (Error Code)";HIGHON$;ERR;HIGHOFF$
17420 PRINT LIND$;"- Drücken Sie am Rechner den Knopf hinten rechts."
17430 PRINT LIND$;"19**************************"
17440 PRINT
17450 '
17460 ' *** Programmende
17470 '
17480 ON ERROR GOTO 0                                   'Disable error trapping
17490 PRINT HIGHOFF$;CURON$;
17500 PRINT FNCUR$(22,1)
17510 RESET
17520 'LW RUN "DISKMENU.EXE"
17530 END                                                           '1340 lines
