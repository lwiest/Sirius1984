1000 ' RELEASE 01***** Neubearbeitung durch 04**********
1005 '
1010 ' LW 31-JAN-2026: Load MSBASIC.COM with /S:560 /F:5
1020 '
1030 ' *** Initialisierung, Dimensionierung ***********************************
1040 '
1050 DEFINT A-Z                                              'Integerdefinition
1060 '
1070 FELDMAX = 27                                                'Anzahl Felder
1080 DIM FENAM$(FELDMAX)                                       'Name des Feldes
1090 DIM FEMUSS(FELDMAX)                              'Muss-Feld (1=Ja, 0=Nein)
1100 DIM FEROW(FELDMAX)                           'Zeile des Feldnamens Ä1..25Ü
1110 DIM FECOL(FELDMAX)                          'Spalte des Feldnamens Ä1..80Ü
1120 DIM FELNG(FELDMAX)                                       'Länge des Feldes
1130 DIM FETYP(FELDMAX)                                         'Typ des Feldes
1140 '
1150 DIM DAT$(FELDMAX)                                              'Datenarray
1160 DIM SUCH$(FELDMAX)                                              'Sucharray
1170 '
1180 BSP$    =CHR$(8)                                                'Backspace
1190 DEL$    =BSP$+"."+BSP$                                             'Delete
1200 ESC$    =CHR$(27)                                'Einleitung Escapesequenz
1210 CLS$    =ESC$+"E"                                      'Bildschirm löschen
1220 INVON$  =ESC$+"p"                                              'Invers ein
1230 INVOFF$ =ESC$+"q"                                              'Invers aus
1240 CURON$  =ESC$+"y5"                                             'Cursor ein
1250 CUROFF$ =ESC$+"x5"                                             'Cursor aus
1260 HIGHON$ =ESC$+"("                                           'Helle Schrift
1270 HIGHOFF$=ESC$+")"                                          'Dunkle Schrift
1280 DELLINE$=ESC$+"K"                                           'Zeile löschen
1290 DELHIGH$=ESC$+"+"                                   'Helle Schrift löschen
1300 STRICH$ =STRING$(79,"-")                                           'Strich
1310 IN$=STRING$(10," ")                                'Indent für Hilfe-Texte
1320 '
1330 DEF FNCUR$(Z,S)=ESC$+"Y"+CHR$(31+Z)+CHR$(31+S)       'Cursor positionieren
1340 HOME$=FNCUR$(3,1)                                    'Cursor Home Position
1350 ' 
1360 CKEYUP=1
1370 CKEYDN=2
1380 CKEYLT=3
1390 CKEYRT=4
1400 '
1410 ASCBSP=8
1420 ASCRET=13
1430 ASCESC=27
1440 ASCCUP=200
1450 ASCCDN=201
1460 ASCCLT=202
1470 ASCCRT=203
1480 ASCF1=241
1490 ASCF9=249
1500 '
1510 ' Other global variables:
1520 ' LIND$ - Local text indent
1521 ' F     - Feldindex 1..27
1522 XOFF=14             'Offset Colum Feldname to Column Wertefeld (Pünktchen)
1530 '
1540 ON ERROR GOTO 17380                                 'Enable error trapping
1550 '
1560 GOSUB 10050    'Tasten deaktivieren, ASCII Codes für Cursortasten vergeben
1570 PRINT CUROFF$
1580 '
1590 ' *** Beginn des Mitgliederverwaltungsprogramms **************************
1600 '
1610 MT$="MITGLIEDERVERWALTUNG":MI$="G-01000":GOSUB 12610          'Basis Maske
1620 Q$="Befindet sich die Daten-Diskette im Laufwerk B?":GOSUB 11820 'Ja/Nein-Frage
1630 IF JA=1 THEN 1670
1640 Q$="Bitte legen Sie sie ein. Fertig?":GOSUB 11820           'Ja/Nein-Frage
1650 IF JA=0 THEN 17520                                           'Programmende
1660 '
1670 N$="Einen Moment...":GOSUB 12540                                'Nachricht
1680 GOSUB 10830                          'Einlesen Stammdatenfelder-Definition
1690 GOSUB 11520                                   'Fielden der Stammsatz-Datei
1700 GOSUB 13270                       'Fielden der Verwendungsnachweis-Dateien
1710 N$="":GOSUB 12540                                               'Nachricht
1720 '
1730 ' ################# Mitgliederverwaltungsmenü (MVW-Menü) #################
1740 '
1750 MT$="MITGLIEDERVERWALTUNG":MI$="G-01000":GOSUB 12610          'Basis Maske
1760 FKEYS$="":GOSUB 12730                                      'F-Tastenleiste
1770 VNFLAG=0                                                              '???
1780 PRINT FNCUR$(4,26);"1 - Stammsatzpflege"                         'Menütext
1790 PRINT FNCUR$(6,26);"2 - Verwendungsnachweis"
1800 PRINT FNCUR$(8,26);"3 - Mitgliedsliste drucken"
1810 NKEYMAX=3:GOSUB 12880                                         'Menüeingabe
1820 DRUCKFLAG=0                                                           '???
1830 IF NKEY THEN ON NKEY GOTO 1920,4660,5490
1840 ON FKEY GOTO 1850,1860,1750,1860         'Hilfe, Zurück, Anfang, Hauptmenü
1850 GOSUB 16570:GOTO 1750                                      'F1->Hilfe-Text
1860 GOSUB 12980:GOTO 1750                           'F2->Zurück, F4->Hauptmenü
1870 '
1900 ' ########################### Stammsatzpflege ############################
1910 '
1920 GOSUB 11080                                              'Einlesen MTGLMAX
1930 IF DRUCKFLAG THEN RETURN                                              '???
1940 '
1950 ' *** Stammsatzpflege-Menü ***********************************************
1960 '
1970 MT$="STAMMSATZPFLEGE":MI$="G-01100":GOSUB 12610               'Basis Maske
1980 FKEYS$="":GOSUB 12730                                      'F-Tastenleiste
1990 PRINT FNCUR$(4,29);"1 - Stammsatz anlegen"
2000 PRINT FNCUR$(6,29);"2 - Stammsatz ändern"
2010 PRINT FNCUR$(8,29);"3 - Stammsatz löschen"
2020 PRINT FNCUR$(10,29);"4 - Stammsatz drucken"
2030 PRINT FNCUR$(12,29);"5 - Stammsatz suchen"
2040 NKEYMAX=5:GOSUB 12880                                         'Menüeingabe
2050 IF NKEY THEN GOSUB 10990:ON NKEY GOTO 2130,2430,2900,4380,3190 'Variablen Init: Read MTGLZAHL, clear DAT$(), SUCH$()
2060 ON FKEY GOTO 2070,2080,2080,2090         'Hilfe, Zurück, Anfang, Hauptmenü
2070 GOSUB 16640:GOTO 1970                                      'F1->Hilfe-Text
2080 GOSUB 13110:GOTO 1750                      'F2,F3->Clear DAT$(),->MVW-Menü
2090 GOSUB 12980:GOTO 1970                                       'F4->Hauptmenü
2100 '
2110 ' *** Stammsatz erfassen von 02************ ******************************
2120 '
2130 MTGLZAHL=MTGLMAX+1                                       'TODO Entry point
2134 F=1
2135 DATENAUSG=1                                                           '???
2160 MT$="STAMMSATZ ANLEGEN":MI$="G-01101":GOSUB 12610             'Basis Maske
2170 FKEYS$="<F7>=Speichern":GOSUB 12730                        'F-Tastenleiste
2180 PRINT FNCUR$(3,31);"Mitgliedsnummer"
2181 GOSUB 11200                                      'Leere Feldmaske ausgeben
2190 GOSUB 12410                                           'Defaultwerte setzen
2200 IF DATENAUSG=1 THEN GOSUB 11420:DATENAUSG=0 '??? wenn vorher HILFE 'Feldwerte ausgeben
2220 PRINT FNCUR$(3,46);HIGHON$;MTGLZAHL;HIGHOFF$
2240 Z=FEROW(F):S=FECOL(F)+XOFF:LNG=FELNG(F):TYP=FETYP(F)
2260 INTEXT$=DAT$(F)                                          'TODO Entry Point
2270 GOSUB 10410                                                         'Input
2272 IF ASCC<>ASCRET THEN 2319                                 'Carriage return
2280   IF FEMUSS(F) AND LEN(OUTTEXT$)<1 THEN N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 12540:GOTO 2260 'Nachricht
2290   DAT$(F)=OUTTEXT$
2300   F=F+1:IF F>FELDMAX THEN F=1
2301   GOTO 2240
2319 IF CKEY=0 THEN 2339                                           'Cursor keys
2320   PRINT FNCUR$(Z,S);HIGHON$;DAT$(F);HIGHOFF$;STRING$(LNG-LEN(DAT$(F)),".")
2329 'LW *** Turing Award:  F=F+(CKEY=CKEYUP)*(-(F>1)+(F=1)*25)+(CKEY=CKEYDN)*((F<27)-(F=27)*26)
2330   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FELDMAX
2331   IF CKEY=CKEYDN THEN F=F+1:IF F>FELDMAX THEN F=1
2335   GOTO 2240
2339 IF FKEY=1 THEN GOSUB 16710:GOTO 2160                               'F-Keys
2340 IF FKEY=2 THEN GOSUB 2386:GOTO 1970                '12300 Exit "Erfassen"
2345 IF FKEY=3 THEN GOSUB 2386:GOSUB 13110:GOTO 1750  '12300 Exit "Erfassen", 13110 Clear DAT$()
2350 IF FKEY=4 THEN GOSUB 2386:GOSUB 12980:GOTO 2240  '12300 Exit "Erfassen" 12980 Implicit No
2360 IF FKEY<>7 THEN 2379
2361   N$="Stammdatensatz wird abgespeichert...":GOSUB 12540         'Nachricht
2362   GOSUB 13370                                      'Erste Seite VN anlegen
2363   GOSUB 11690                            'Speichern der Felddaten auf Disk
2364   GOSUB 11320                              'Leere Feldplatzhalter ausgeben
2365   N$="":GOSUB 12540                                             'Nachricht
2366   MTGLZAHL=MTGLZAHL+1
2367   F=1
2368   GOSUB 13110                                                'Clear DAT$()
2369   GOSUB 12410                                         'Defaultwerte setzen
2370   GOSUB 11420                                 'Aktuelle Feldwerte ausgeben
2371   GOTO 2220
2379 N$="Sad hack! Ungültige Eingabe - Bitte wiederholen":GOSUB 12540 'Nachricht
2380 GOSUB 10460                                   'INPUT+2 TODO: ??? Sad hack!
2381 GOTO 2280                                              'Erneute Auswertung
2382 '
2385 ' IN: MTGLZAHL
2386 FOR K=1 TO 30:A$=INKEY$:NEXT K                      '??? Debounce keyboard
2387 PRINT FNCUR$(21,1);DELLINE$                            '??? Delete message
2388 TMPF=1
2389 IF INSTR(SPACE$(25),DAT$(TMPF))=0 THEN 2394                           '???
2390   N$="DEBUG: DAT$() with 25 spaces!":GOSUB 12540             'LW Nachricht
2391   TMPF=TMPF+1:IF TMPF<28 THEN 2389
2392   MTGLZAHL=MTGLZAHL-1
2393   GOTO 2402
2394 Q$="Wollen Sie diese Daten speichern?":GOSUB 11820          'Ja/Nein-Frage
2395 IF JA=1 THEN 2399
2396   N$="Daten werden nicht abgespeichert.":GOSUB 12540            'Nachricht
2397   MTGLZAHL=MTGLZAHL-1 
2398   GOTO 2404
2399 N$="Daten werden abgespeichert...":GOSUB 12540                  'Nachricht
2400 GOSUB 13370                                        'Erste Seite VN anlegen
2401 GOSUB 11690                              'Speichern der Felddaten auf Disk
2402 GOSUB 11125                                            'Schreiben MTGLZAHL
2403 MTGLMAX=MTGLZAHL
2404 PRINT FNCUR$(21,1);DELLINE$                                'Delete message
2405 RETURN
2427 ' 
2428 ' *** Stammsatz ändern von 03************ ********************************
2429 '
2430 MT$="STAMMSATZ ÄNDERN":MI$="G-01102":GOSUB 12610              'Basis Maske
2440 F=1:PRINT CURON$
2450 FKEYS$="":GOSUB 12730                                      'F-Tastenleiste
2460 'LW *** PRINT HIGHOFF$;
2470 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer"
2480 PRINT FNCUR$(5,23)"des zu verändernden Stammsatzes ein,"
2490 PRINT FNCUR$(6,22)"und beenden Sie die Eingabe mit RETURN"
2500 PRINT FNCUR$(8,37)"=>..."
2510 LNG=3:TYP=2:Z=8:S=39
2520 GOSUB 10410                                                         'INPUT
2530 IF FKEY=1 THEN GOSUB 16790:GOTO 2430                               'F-Keys
2540 IF FKEY=2 THEN 1970
2550 IF FKEY=3 THEN GOSUB 13110:GOTO 1750                         'Clear DAT$()
2560 IF FKEY=4 THEN GOSUB 12980
2570 IF CKEY<>0 OR FKEY<>0 THEN GOTO 2510                               'Errors
2580 IF LEN(OUTTEXT$)<1 THEN N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 12540:GOTO 2510 ELSE MITGLIED=VAL(OUTTEXT$) 'Nachricht
2590 IF MITGLIED=0 THEN MITGLIED=MTGLMAX+2
2600 IF MITGLIED>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12540:PRINT HIGHOFF$;FNCUR$(8,39);"...";DELHIGH$:OUTTEXT$="":GOTO 2520 'Nachricht
2610 MTGLZAHL=MITGLIED:MTGL$=STR$(MITGLIED)
2620 FOR K=4 TO 8:PRINT FNCUR$(K,1);DELLINE$:NEXT:PRINT FNCUR$(21,1);DELLINE$
2630 GOSUB 11580                                                   'Daten lesen
2640 PRINT HIGHOFF$;FNCUR$(3,30);"Mitgliedsnummer";FNCUR$(5,41);"Beruf:"
2650 GOSUB 11200
2660 GOSUB 11420
2670 PRINT FNCUR$(3,46);MTGLZAHL
2680 FKEYS$="<F7>=Speichern":GOSUB 12730                        'F-Tastenleiste
2690 S=FECOL(F)+14:Z=FEROW(F):N$="":GOSUB 12540                      'Nachricht
2700 LNG=FELNG(F):TYP=FETYP(F)
2710 GOSUB 10410                                                         'INPUT
2720 IF FKEY=1 THEN GOSUB 16860:GOTO 2640                   'Hilfe-Text  F-Keys
2730 IF FKEY=2 THEN GOSUB 13180: GOTO 2430                     'Eing.-Mtgl.-Nr.
2740 IF FKEY=3 THEN GOSUB 13180:GOSUB 13110: GOTO 1750              'Mvw-Menü 'Clear DAT$() 12610
2750 IF FKEY=4 THEN GOSUB 12980:PRINT HIGHOFF$;FNCUR$(24,1);"<F7>=Speichern";FNCUR$(1,1);HIGHON$:GOTO 2710
2760 IF FKEY=7 THEN 2830                                       'Daten Speichern
2770 IF CKEY=CKEYUP THEN PRINT FNCUR$(Z,S);DAT$(F);STRING$(LNG-LEN(DAT$(F)),"."):IF F>1  THEN F=F-1:GOTO 2690  ELSE F=26:GOTO 2690 'Cursor CKEY
2780 IF CKEY=CKEYDN THEN PRINT FNCUR$(Z,S);DAT$(F);STRING$(LNG-LEN(DAT$(F)),"."):IF F<27 THEN F=F+1:GOTO 2690  ELSE F= 1:GOTO 2690 'Cursor runter
2790 IF ASCC=13 AND LEN(OUTTEXT$)<1 AND FEMUSS(F) THEN N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 12540:GOTO 2710 'Nachricht
2800 IF ASCC=13 THEN DAT$(F)=OUTTEXT$:PRINT HIGHON$;FNCUR$(Z,S);DAT$(F);STRING$(LNG-LEN(DAT$(F)),".")                    'Return
2810 IF ASCC=13 THEN IF F<FELDMAX THEN F=F+1:GOTO 2690 ELSE F=1:GOTO 2690
2820 N$="Ungültige Eingabe - Bitte wiederholen":GOSUB 12540:FOR KK=1 TO 25000:NEXT:GOTO 2690 'Nachricht
2830 GOSUB 11690                                               'Daten speichern
2840 Q$="Wollen Sie noch einen weiteren Datensatz ändern?":GOSUB 11820 'Ja/Nein-Frage
2850 IF JA=1 THEN 2430
2860 GOTO 1970                                                    'Zum STS-Menü
2870 '
2880 ' *** Stammsatz löschen von 04********** *********************************
2890 '
2900 MT$="STAMMSATZ LÖSCHEN":MI$="G-01103":GOSUB 12610             'Basis Maske
2910 FKEYS$="":GOSUB 12730                                      'F-Tastenleiste
2920 PRINT FNCUR$(4,23);"Bitte geben Sie die Mitgliedsnummer"         'Menütext
2930 PRINT FNCUR$(5,25);"des zu löschenden Mitglieds ein."
2940 PRINT FNCUR$(6,15);"(Dabei wird nur der Status des Mitglieds verändert.)"
2950 PRINT FNCUR$(7,23);"Beenden Sie die Eingabe mit RETURN"
2960 PRINT FNCUR$(9,37);"=>..."
2970 LNG=3:TYP=2:Z=9:S=39
2980 PRINT HIGHON$:GOSUB 10410:PRINT HIGHOFF$                          'INPUT+1
2990 IF FKEY=2 THEN 1970                                                'F-Keys
3000 IF FKEY=3 THEN GOSUB 13110: GOTO 1750                        'Clear DAT$()
3010 IF FKEY=1 THEN GOSUB 16950:GOTO 2900
3020 IF FKEY=4 THEN GOSUB 12980:GOTO 2920
3030 IF FKEY>4 THEN CKEY=CKEYDN
3040 IF CKEY<>0 THEN N$="Ungültige Eingabe - Bitte wiederholen":GOSUB 12540:GOTO 2960 'Nachricht
3050 MTGLZAHL=VAL(OUTTEXT$):IF MTGLZAHL=0 THEN MTGLZAHL=MTGLMAX+1       'Errors
3060 IF MTGLZAHL>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12540:GOTO 2960 'Nachricht
3070 GOSUB 11580                                                'Daten einlesen
3080 GOSUB 12610                                                   'Basis Maske
3090 PRINT FNCUR$(3,30);"Mitgliedsnummer";FNCUR$(3,46);MTGLZAHL;FNCUR$(5,41);"Beruf:":GOSUB 11200:GOSUB 11420 'Feldmaske
3100 Q$="Endgültig löschen?":GOSUB 11820                         'Ja/Nein-Frage
3110 IF JA=0 THEN 3140                                               'Löschen ?
3120 DAT$(5)="ausgetreten     ":PRINT FNCUR$(9,16);HIGHON$;DAT$(5);HIGHOFF$
3130 GOSUB 11690                                               'Daten speichern
3140 Q$="Soll noch jemand gelöscht werden?":GOSUB 11820          'Ja/Nein-Frage
3150 IF JA=1 THEN 2900 ELSE 1970
3160 '
3170 ' *** Stammsatz suchen von 05*************** *****************************
3180 '
3190 MI$="G-01105"
3200 NUMSUCH1=0:NUMSUCH2=0:MTGL$=""
3210 PRINT HIGHOFF$
3220 IF DRUCKFLAG THEN MT$="SUCHEN & DRUCKEN" ELSE MT$="STAMMSATZ SUCHEN"
3230 GOSUB 12610                                                   'Basis Maske
3240 FKEYS$="":GOSUB 12730                                      'F-Tastenleiste
3250 PRINT CUROFF$;
3260 IF DRUCKFLAG THEN A$=" Drucken " ELSE A$=" Suchen "
3270 PRINT FNCUR$(6,30)"1 -";A$;"nach Nummern"
3280 PRINT FNCUR$(8,30)"2 -";A$;"nach Maske"
3290 GOSUB 13110                                                  'Clear DAT$()
3300 NKEYMAX=2:GOSUB 12880                                         'Menüeingabe
3310 IF FKEY=1 THEN GOSUB 17020:GOTO 3210                               'F-Keys
3320 IF FKEY=2 THEN IF DRUCKFLAG THEN GOSUB 13110:GOTO 5490 ELSE 1970 'Clear DAT$() 12610
3330 IF FKEY=3 THEN GOSUB 13110: GOTO 1750                        'Clear DAT$()
3340 IF FKEY=4 THEN GOSUB 12980:GOTO 3300
3350 IF NKEY=1 THEN 3550
3360 F=1                                                     'Suchen nach Maske
3370 FOR I=1 TO 27:SUCH$(I)="":DAT$(I)="":NEXT
3380 FKEYS$="<F5>=Suchen ":GOSUB 4290
3390 IF DRUCKFLAG THEN FKEYS$="<F5>=Suchen & Drucken"
3400 GOSUB 12730                                                'F-Tastenleiste
3410 Z=FEROW(F):S=FECOL(F)+14                                  'Cursorparameter
3420 LNG=FELNG(F):TYP=3
3430 PRINT FNCUR$(Z,S)
3440 OUTTEXT$="":GOSUB 10410                                             'INPUT
3450 IF FKEY=5 THEN MTGLZAHL=1:GOTO 3860                                'F-Keys
3460 IF FKEY=1 THEN GOSUB 17020:GOSUB 4290:GOSUB 11420:GOTO 3390
3470 IF FKEY=4 THEN GOSUB 12980:GOTO 3410
3480 IF FKEY=2 THEN 3190
3490 IF FKEY=3 THEN GOSUB 13110: GOTO 1750                        'Clear DAT$()
3500 IF OUTTEXT$<>"" OR ASCC=13 THEN SUCH$(F)=OUTTEXT$:DAT$(F)=OUTTEXT$
3510 IF CKEY=CKEYUP THEN IF F>1 THEN F=F-1 ELSE F=26               'Cursor CKEY
3520 IF CKEY=CKEYDN OR ASCC=ASCRET THEN IF F<27 THEN F=F+1 ELSE F=1       'Cursor runter
3530 N$="":GOSUB 12540                                               'Nachricht
3540 GOTO 3410
3550 PRINT CLS$:PRINT:PRINT                                'Suchen nach Nummern
3560 Z=7:S=1:IF DRUCKFLAG THEN DRUCK$="gedruckt" ELSE DRUCK$="gesucht "
3570 TYP=2:LNG=3:PRINT CURON$;                                        'Menütext
3580 GOSUB 12610                                                   'Basis Maske
3590 PRINT FNCUR$(4,5);"Maximale Mitgliedsnummer:";MTGLMAX
3600 FKEYS$="":GOSUB 12730                                      'F-Tastenleiste
3610 PRINT HOME$;:PRINT FNCUR$(Z,S);                          'Beginn bei Nr...
3620 PRINT HIGHOFF$;"Ab welcher Mitgliedsnummer soll ";DRUCK$;" werden      =>...":S=56
3630 GOSUB 10410                                                       'INPUT+1
3640 IF FKEY=1 THEN GOSUB 17020:GOTO 3550                               'F-Keys
3650 IF FKEY=2 THEN 3190
3660 IF FKEY=3 THEN GOSUB 13110: GOTO 1750                        'Clear DAT$()
3670 IF FKEY=4 THEN GOSUB 12980:S=1:GOTO 3610
3680 IF OUTTEXT$="" THEN S=1:GOTO 3610
3690 NUMSUCH1=VAL(OUTTEXT$)
3700 IF NUMSUCH1>MTGLMAX OR NUMSUCH1<1 THEN 3830
3710 Z=9:S=1
3720 N$="":GOSUB 12540                                               'Nachricht
3730 PRINT HOME$:PRINT FNCUR$(Z,S);                             'Ende bei Nr...
3740 PRINT HIGHOFF$;"Bis zu welcher Mitgliedsnummer soll ";DRUCK$;" werden  =>...":S=56
3750 GOSUB 10410                                                       'INPUT+1
3760 IF FKEY=1 THEN GOSUB 17020:GOTO 3550                               'F-Keys
3770 IF FKEY=2 THEN 3190
3780 IF FKEY=3 THEN GOSUB 13110: GOTO 1750                        'Clear DAT$()
3790 IF FKEY=4 THEN GOSUB 12980:S=1:GOTO 3730
3800 IF OUTTEXT$="" THEN S=1:GOTO 3730
3810 NUMSUCH2=VAL(OUTTEXT$)
3820 IF NUMSUCH2<=MTGLMAX AND NUMSUCH1<=NUMSUCH2 THEN 3850
3830 N$="Unlogische Eingabe - Bitte neu eingeben":GOSUB 12540 'Errors 'Nachricht
3840 Z=6:S=1:PRINT FNCUR$(9,1);DELLINE$;FNCUR$(Z,S);:Z=7:GOTO 3610
3850 MTGLZAHL=NUMSUCH1                                               'Feldmaske
3860 PRINT CUROFF$
3870 IF DRUCKFLAG THEN GOSUB 15330               'Mitgliedsliste drucken (Kopf)
3880 CC$="  <F7>=Adressaufkleber"
3890 IF NUMSUCH1<>0 THEN FKEYS$="<F5>=Suchen rückwärts  <F6>=vorwärts"+CC$ ELSE FKEYS$="<F6>=Suchen vorwärts"+CC$
3900 GOSUB 4290
3910 PRINT FNCUR$(3,30);"Mitgliedsnummer"
3920 IF NUMSUCH1=0  AND MTGLZAHL<=MTGLMAX  THEN 3990
3930 IF NUMSUCH1<>0 AND MTGLZAHL<=NUMSUCH2 THEN 3990
3940 IF DRUCKFLAG THEN GOSUB 13110:GOTO 3190                      'Clear DAT$()
3950 PRINT HIGHON$;CUROFF$;:MTGLZAHL=MTGLZAHL-1
3960 N$="Daten nicht vorhanden. Mit <F2> neue Suchmöglichkeit.":GOSUB 12540 'Nachricht
3970 PRINT HIGHOFF$;
3980 GOTO 4160
3990 GOSUB 11580                                                   'Daten lesen
4000 IF NUMSUCH1<>0 THEN 4100
4010 FOR K=1 TO FELDMAX                                      'Daten durchsuchen
4020   IF LEFT$(SUCH$(K),1)<>"*" THEN 4050
4030   IF INSTR(DAT$(K),RIGHT$(SUCH$(K),LEN(SUCH$(K))-1))=0 THEN MTGLZAHL=MTGLZAHL+1:GOTO 3920
4040   GOTO 4090
4050   I=1
4060   IF MID$(SUCH$(K),I,1)="?" THEN I=I+1:GOTO 4060
4070   I=I-1
4080   IF INSTR(DAT$(K),RIGHT$(SUCH$(K),LEN(SUCH$(K))-I))<>I+1 THEN MTGLZAHL=MTGLZAHL+1:GOTO 3920
4090 NEXT
4100 GOSUB 11420                                                     'Feldmaske
4110 MTGL$=STR$(MTGLZAHL) 
4120 PRINT CUROFF$;FNCUR$(3,46);MTGL$
4130 IF DRUCKFLAG=0 THEN 4160
4140 GOSUB 15480                             'Mitgliedsliste drucken (Mitglied)
4150 IF ISBRKPRNT THEN ISBRKPRNT=0:N$="":GOSUB 12540:GOTO 3190 ELSE N$="":GOSUB 12540:PRINT CUROFF$:MTGLZAHL=MTGLZAHL+1:GOTO 3920 'Nachricht 'Nachricht
4160 FKEY=ASC(INPUT$(1))-240
4170 N$="":GOSUB 12540                                               'Nachricht
4180 IF FKEY=1 THEN GOSUB 17020:GOSUB 4290:PRINT FNCUR$(3,30);"Mitgliedsnummer":GOTO 4100
4190 IF FKEY=2 THEN IF NUMSUCH1<>0 THEN 3550 ELSE 3360
4200 IF FKEY=3 THEN GOSUB 13110:GOTO 1750                         'Clear DAT$()
4210 IF FKEY=7 THEN GOSUB 15720:GOSUB 12730:N$="":GOSUB 12540:GOTO 4100 'Adressaufkleber drucken 'F-Tastenleiste 'Nachricht
4220 IF FKEY=4 THEN GOSUB 12980:GOTO 4100
4230 IF FKEY=5 AND NUMSUCH1<>0 THEN IF MTGLZAHL>NUMSUCH1 THEN MTGLZAHL=MTGLZAHL-1:GOTO 3990
4240 IF FKEY=6 THEN 4270
4250 N$="Falsche Eingabe":GOSUB 12540                                'Nachricht
4260 GOTO 4160
4270 MTGLZAHL=MTGLZAHL+1:PRINT FNCUR$(21,1);DELLINE$;HOME$;
4280 GOTO 3920
4290 GOSUB 12610                                                   'Basis Maske
4300 IF DRUCKFLAG THEN MT$="SUCHEN & DRUCKEN" ELSE MT$="STAMMSATZ SUCHEN"
4310 GOSUB 11200
4320 IF NOT DRUCKFLAG THEN GOSUB 12730                          'F-Tastenleiste
4330 PRINT FNCUR$(5,41);"BERUF:"
4340 RETURN
4350 '
4360 ' *** Stammsatz drucken von 02************ & 04********** ****************
4370 '
4380 MT$="STAMMSATZ DRUCKEN":MI$="G-01104":GOSUB 12610             'Basis Maske
4390 FKEYS$="":GOSUB 12730                                      'F-Tastenleiste
4400 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer"
4410 PRINT FNCUR$(5,21)"des auszudruckenden Stammsatzes ein."
4420 PRINT FNCUR$(6,23)"Beenden Sie die Eingabe mit RETURN"
4430 PRINT FNCUR$(8,37);"=>..."
4440 LNG=3:TYP=2:Z=8:S=39
4450 PRINT HIGHON$:GOSUB 10410:PRINT HIGHOFF$                          'INPUT+1
4460 IF FKEY=1 THEN GOSUB 17100:GOTO 4380                               'F-Keys
4470 IF FKEY=2 THEN 1970
4480 IF FKEY=3 THEN PRINT HIGHOFF$:GOSUB 13110: GOTO 1750         'Clear DAT$()
4490 IF FKEY=4 THEN GOSUB 12980:GOTO 4430
4500 IF FKEY>4 THEN CKEY=CKEYDN
4510 IF CKEY<>0 THEN 4430
4520 MTGLZAHL=VAL(OUTTEXT$):IF MTGLZAHL=0 THEN MTGLZAHL=MTGLMAX+1       'Errors
4530 IF MTGLZAHL>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12540:GOTO 4430 'Nachricht
4540 GOSUB 11580                                                   'Daten lesen
4550 GOSUB 12610                                                   'Basis Maske
4560 PRINT FNCUR$(3,30);"Mitgliedsnummer";HIGHON$;FNCUR$(3,46);MTGLZAHL;HIGHOFF$;FNCUR$(5,41);"Beruf:":GOSUB 11200:GOSUB 11420
4570 Q$="Wollen Sie diese Daten drucken lassen?":GOSUB 11820     'Ja/Nein-Frage
4580 IF JA=0 THEN 4380                                               'Drucken ?
4590 Q$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 11820 'Ja/Nein-Frage
4600 IF JA=0 THEN N$="Bitte schalten Sie den Drucker ein.":GOSUB 12540:DELAY=1000:GOSUB 10280:GOTO 4570 'Drucker bereit ? 'Nachricht 'Delay
4610 GOSUB 15920                          'Stammsatz drucken (Bildschirmformat)
4620 GOTO 4380
4630 '
4640 ' ######################### Verwendungsnachweis ##########################
4650 '
4660 GOSUB 11080                                              'Einlesen MTGLMAX
4670 DEL$=BSP$+" "+BSP$                                        'TODO ever used?
4680 VNFLAG=-1
4690 '
4700 ' Verwendungsnachweis-Menü ***********************************************
4710 '
4720 MT$="VERWENDUNGSNACHWEIS":MI$="G-01200":GOSUB 12610           'Basis Maske
4730 F=4                                                                'TODO ?
4740 FKEYS$="":GOSUB 12730                                      'F-Tastenleiste
4750 PRINT CURON$;
4760 PRINT HIGHOFF$;
4770 PRINT FNCUR$(4,20)"   Bitte geben Sie die Mitgliedsnummer"
4780 PRINT FNCUR$(5,20)"des zu ändernden Verwendungsnachweises ein"
4790 PRINT FNCUR$(6,20)" und beenden Sie die Eingabe mit RETURN."
4800 PRINT FNCUR$(8,20)"                 =>..."
4810 Z=8:S=39:LNG=3:TYP=2
4820 GOSUB 10410                                                         'INPUT
4830 IF FKEY=1 THEN GOSUB 17170:GOTO 4720
4840 IF FKEY=2 OR FKEY=3 THEN GOSUB 13110:VNFLAG=0:GOTO 1750      'Clear DAT$()
4850 IF FKEY=4 THEN GOSUB 12980
4860 IF FKEY>4 THEN 4810
4870 IF CKEY<>0 THEN GOTO 4810
4880 IF LEN(OUTTEXT$)=0 THEN N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 12540:GOTO 4800 ELSE MITGLIED=VAL(OUTTEXT$):IF MITGLIED=0 THEN 4820 'Nachricht
4890 IF MITGLIED>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12540:PRINT HOME$;HIGHOFF$:GOTO 4800 'Nachricht
4900 MTGLZAHL=MITGLIED:MTGL$=STR$(MITGLIED)
4910 '
4920 ' *** Einlesen des Mitgliedsnamens ***************************************
4930 '
4940 PRINT FNCUR$(21,1);DELLINE$;
4950 GET #2,MITGLIED                                        'EINLESEN DATEN STS
4960 NNAM$=LEFT$(DD1$,25)                                 'NACHNAME RAUSFISCHEN
4970 VNAM$=MID$(DD1$,26,25)                               'VORNAME  RAUSFISCHEN
4980 '
4990 ' ** LEERZEICHEN HINTER NACHNAME ABSCHNEIDEN *****************************
5000 '
5010 FOR I=LEN(NNAM$) TO 1 STEP -1
5020   IF MID$(NNAM$,I,1)<>" " THEN NNAM$=LEFT$(NNAM$,I):I=1
5030 NEXT
5040 NAMM$=NNAM$+" "+VNAM$
5050 GOSUB 13540                                          'EINLESEN 1. VN-BLATT
5060 BLATT=1
5070 VP=1:HP=1                                                 'INITIALISIERUNG
5080 F=8
5090 GOSUB 12610                                                   'Basis Maske
5100 FKEYS$="<F7>=Speichern":GOSUB 12730                        'F-Tastenleiste
5110 PRINT HIGHOFF$;
5120 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=Vorwärts";TAB(53);"<F9>=Blatt drucken";
5130 PRINT FNCUR$(3,2);"Blatt:"
5140 PRINT FNCUR$(5,2);"Mitglieds-Nr.:"
5150 PRINT FNCUR$(5,24);"Name:"
5160 MSK$="Von:  .  .   Bis:  .  .   Amt:"
5170 FOR KK=7 TO 19 
5180   PRINT FNCUR$(KK,2);MSK$;TAB(43);MSK$       'TODO No semicolon at the end
5190 NEXT
5200 GOSUB 14780                                                 'Ausgabe Daten
5210 LNG=2-(HP=7)*6                                    'BEGIN OF THE INPUT-LOOP
5220 TYP=2-(HP=7)
5230 Z=VP+13*(VP>13)+6
5240 S=6-39*(VP>13)-(HP<7)*(HP-1)*3+(HP>3)*(HP<7)*4-(HP=7)*26
5250 N$="":GOSUB 12540                         'ERASE MESSAGE-LINE   'Nachricht
5260 GOSUB 10410                                                       'INPUT+1
5270 IF HP<7 THEN IF ASCC=ASCRET THEN OUTTEXT$=STRING$(2-LEN(OUTTEXT$),"0")+OUTTEXT$   '+NULLEN
5280 IF CKEY<>0 OR ASCC=ASCRET THEN PRINT FNCUR$(Z,S);OUTTEXT$
5290 IF ASCC=ASCRET THEN IF HP<7 THEN MID$(DAT$(VP),-1+2*HP,2)=OUTTEXT$ ELSE DAT$(VP)=LEFT$(DAT$(VP),12)+OUTTEXT$+SPACE$(8-LEN(OUTTEXT$))
5300 IF ASCC=ASCRET AND HP<7 THEN JA=1:TEST$=OUTTEXT$:ON HP GOSUB 12190,12220,12250,12190,12220,12250:IF JA=0 THEN N$="Ungültiges Datum":GOSUB 12540:GOTO 5260 'Datums-Check 'Nachricht
5310 IF ASCC=ASCRET THEN IF HP=7 THEN HP=1:VP=VP-(VP<26)-(VP=26)*(-25):GOTO 5210 ELSE HP=HP+1:GOTO 5210     'NEUE SCREEN-POSITION
5320 IF CKEY<>0 THEN TEST$=MID$(DAT$(VP),-(HP<7)*2*HP-1-(HP=7)*14,-(HP<7)*2-(HP=7)*8):PRINT FNCUR$(Z,S);TEST$
5330 IF CKEY<>0 THEN IF HP<7 AND NOT TEST$="  " THEN JA=1:ON HP GOSUB 12190,12220,12250,12190,12220,12250:IF JA=0 THEN N$="Ungültiges Datum":GOSUB 12540:GOTO 5260 'Nachricht
5340 IF CKEY=CKEYDN THEN VP=VP-(VP<26)-(VP=26)*(-25):GOTO 5210
5350 IF CKEY=CKEYUP THEN VP=VP+(VP>1)-(VP=1)*25     :GOTO 5210
5360 IF CKEY=CKEYRT THEN HP=HP-(HP<7)-(HP=7)*(-6)   :GOTO 5210
5370 IF CKEY=CKEYLT THEN HP=HP+(HP>1)+(HP=1)*(-6)   :GOTO 5210
5380 IF FKEY=1 THEN GOSUB 17240 :GOTO 5070           'HILFE - ZU INIT UND MASKE
5390 IF FKEY=2 THEN GOTO 4720                                  'ZU MTGL-EINGABE
5400 IF FKEY=3 THEN VNFLAG=0:GOSUB 13110:GOTO 1750                'Clear DAT$()
5410 IF FKEY=4 THEN GOSUB 12980:GOSUB 12790:GOTO 5260 
5420 IF FKEY=5 THEN GOSUB 13630:GOTO 5210
5430 IF FKEY=6 THEN GOSUB 13790:GOTO 5210
5440 IF FKEY=7 THEN GOSUB 14440:GOTO 5260                 'SPEICHERN DANN INPUT
5450 IF FKEY=9 THEN GOSUB 15020:GOSUB 13990:GOTO 5250
5460 '
5470 ' ############# Mitgliedsliste drucken von 05*************** #############
5480 '
5490 DRUCKFLAG=-1
5500 MTGL$=""
5510 DATUM$="  .  .  "
5520 ' GOSUB 1680                                               'TODO Sad hack!
5530 DEL$=BSP$+"."+BSP$
5540 GOSUB 11080                                              'Einlesen MTGLMAX
5550 GOSUB 10990          'Variablen Init: Read MTGLZAHL, clear DAT$(), SUCH$()
5560 '
5570 PRINT CLS$;
5580 PRINT HIGHOFF$;
5590 PRINT CUROFF$;
5600 MT$="MITGLIEDSLISTE DRUCKEN":MI$="G-01300":GOSUB 12610        'Basis Maske
5610 FKEYS$="":GOSUB 12730                                      'F-Tastenleiste
5620 PRINT FNCUR$(4,28);"1 - Suchen & Drucken"
5630 PRINT FNCUR$(6,28);"2 - Komplett drucken"
5640 PRINT FNCUR$(8,28);"3 - Datenblatt drucken"
5650 NKEYMAX=3:GOSUB 12880                                         'Menüeingabe
5660 IF FKEY=1 THEN GOSUB 17310:GOTO 5570
5670 IF FKEY=2 OR FKEY=3 THEN GOSUB 13110:DRUCKFLAG=0:GOTO 1750   'Clear DAT$()
5680 IF FKEY=4 THEN GOSUB 12980:GOTO 5650
5690 IF NKEY=1 THEN 5750
5700 IF NKEY=2 THEN DRUCKFLAG=0:GOTO 5750
5710 IF NKEY=3 THEN 6180
5720 '
5730 ' *** Suchen & Drucken, Komplett drucken *********************************
5740 '
5750 GOSUB 12610                                                   'Basis Maske
5760 GOSUB 12730                                                'F-Tastenleiste
5770 PRINT FNCUR$(7,13);"Geben Sie bitte das heutige Datum ein :   .  .  ":Z=7:LNG=2:TYP=2
5780 PRINT HIGHON$;CURON$
5790 FOR K=0 TO 2
5800   S=53+K*3
5810   VNFLAG=-1:GOSUB 10410:VNFLAG=0    'in order to activate right-left shift 'INPUT+1
5820   IF CKEY<>0 THEN JA=1:TEST$=MID$(DATUM$,K*3+1,2):ON K+1 GOSUB 12190,12220,12250:IF JA=0 THEN N$="Ungültiges Datum":GOSUB 12540:GOTO 5800 'Nachricht
5830   IF FKEY=1 THEN GOSUB 17310:GOTO 5750
5840   IF FKEY=2 THEN 5490
5850   IF FKEY=3 THEN GOSUB 13110:DRUCKFLAG=0:GOTO 1750           'Clear DAT$()
5860   IF FKEY=4 THEN GOSUB 12980:GOTO 5800
5870   IF CKEY=CKEYLT THEN IF K=0 THEN 5800 ELSE K=K-1:GOTO 5800
5880   IF CKEY=CKEYRT THEN IF K=2 THEN K=0:GOTO 5800 ELSE K=K+1:GOTO 5800
5890   IF ASCC<>13 THEN 5800
5900   OUTTEXT$=STRING$(2-LEN(OUTTEXT$),"0")+OUTTEXT$:PRINT FNCUR$(Z,S);OUTTEXT$
5910   TEST$=OUTTEXT$:JA=1:ON K+1 GOSUB 12190,12220,12250:IF JA=0 THEN N$="Ungültige Datumseingabe":GOSUB 12540:GOTO 5800 'Nachricht
5920   MID$(DATUM$,K*3+1,2)=OUTTEXT$
5930 NEXT
5940 PRINT HIGHOFF$;
5950 PRINT CUROFF$
5960 Q$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 11820   'Ja/Nein-Frage
5970 IF JA=0 THEN N$="":GOSUB 12540:GOTO 5750                        'Nachricht
5980 '
5990 ' *** Drucken Anfang *****************************************************
6000 '
6010 IF DRUCKFLAG THEN 6050
6020 PRINT FNCUR$(7,1);DELLINE$;
6030 PRINT CUROFF$
6040 N$="Mitgliedsliste wird gedruckt. Abbrechen mit Taste.":GOSUB 12540 'Nachricht
6050 IF DRUCKFLAG THEN 3200
6060 PRINT FNCUR$(23,1);DELLINE$;
6070 GOSUB 15330                                 'Mitgliedsliste drucken (Kopf)
6080 FOR MTGLZAHL=1 TO MTGLMAX
6090   GOSUB 11580:IF INSTR(DAT$(5),"ausgetreten")>0 THEN 6120
6100   GOSUB 15480                           'Mitgliedsliste drucken (Mitglied)
6110   IF ISBRKPRNT THEN ISBRKPRNT=0:N$="Drucken abgebrochen":GOSUB 12540:PRINT CUROFF$:GOTO 6130 ELSE GOSUB 12540:PRINT CUROFF$ 'Nachricht
6120 NEXT MTGLZAHL
6130 GOSUB 13110                                                  'Clear DAT$()
6140 DRUCKFLAG=0:GOTO 5490
6150 '
6160 ' *** Datenblatt drucken *************************************************
6170 '
6180 MT$="DATENBLATT DRUCKEN":MI$="G-01303":GOSUB 12610            'Basis Maske
6190 FKEYS$="":GOSUB 12730                                      'F-Tastenleiste
6200 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer"
6210 PRINT FNCUR$(5,24)"des zu druckenden Datenblattes ein."
6220 PRINT FNCUR$(7,23)"Beenden Sie die Eingabe mit RETURN"
6230 PRINT FNCUR$(9,37)"=>..."
6240 LNG=3:TYP=2:Z=9:S=39
6250 PRINT HIGHON$:GOSUB 10410:PRINT HIGHOFF$                          'INPUT+1
6260 IF FKEY=2 THEN 5490                                                'F-Keys
6270 IF FKEY=3 THEN GOSUB 13110: GOTO 1750                        'Clear DAT$()
6280 IF FKEY=1 THEN GOSUB 17310:PRINT CURON$:MT$="DATENBLATT DRUCKEN":GOTO 6180
6290 IF FKEY=4 THEN GOSUB 12980:GOTO 6180
6300 IF FKEY>4 THEN CKEY=CKEYDN 
6310 IF CKEY<>0 THEN N$="Ungültige Eingabe - Bitte wiederholen":GOSUB 12540:GOTO 6230 'Nachricht
6320 MTGLZAHL=VAL(OUTTEXT$):IF MTGLZAHL=0 THEN MTGLZAHL=MTGLMAX+1       'Errors
6330 IF MTGLZAHL>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12540:GOTO 6230 'Nachricht
6340 Q$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 11820        'Ja/Nein-Frage
6350 IF JA=0 THEN N$="Bitte schalten Sie den Drucker ein.":GOSUB 12540:DELAY=1000:GOSUB 10280:GOTO 6180     'Drucker bereit ? 'Nachricht 'Delay
6360 GOSUB 11580                                            'S.S.Daten einlesen
6370 NNAM$ = LEFT$(DD1$,25) : NNAM$ = LEFT$(NNAM$,INSTR(NNAM$," "))
6380 NAMM$ = NNAM$ + MID$(DD1$,26,25)
6390 GOSUB 15920                          'Stammsatz drucken (Bildschirmformat)
6400 GOSUB 13540                                            'V.N Daten einlesen
6410 GOSUB 15060                                             'V.N Daten drucken
6420 GOSUB 16170                                             'Mitgliedsvermerke
6430 GOTO 6180
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
10370 ' IN: DEL$        OUT: ASCC
10380 ' IN: VNFLAG      OUT: (A$)
10390 ' IN: INTEXT$
10400 '
10410 PRINT FNCUR$(Z,S);HIGHON$;INTEXT$;HIGHOFF$;             'TODO Entry point
10420 P=LEN(INTEXT$)
10430 IF P<LNG THEN PRINT STRING$(LNG-P,".");
10440 OUTTEXT$=INTEXT$
10450 PRINT CURON$;
10460 CKEY=0:FKEY=0                                           'TODO Entry point
10470 PRINT FNCUR$(Z,S+P);
10480 A$=INPUT$(1):ASCC=ASC(A$)                                      'Press key
10490 ON TYP GOSUB 11940,12010,12070,12130                      'Bereichs-Check
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
10640 IF ASCC=ASCCUP THEN CKEY=CKEYUP:GOSUB 10780:GOTO 10730  'Block cursor auto-repeat
10650 IF ASCC=ASCCDN THEN CKEY=CKEYDN:GOSUB 10780:GOTO 10730  'Block cursor auto-repeat
10660 IF VNFLAG=0 THEN 10690
10670   IF ASCC=ASCCLT THEN CKEY=CKEYLT:GOSUB 10780:GOTO 10730  'Block cursor auto-repeat 
10680   IF ASCC=ASCCRT THEN CKEY=CKEYRT:GOSUB 10780:GOTO 10730  'Block cursor auto-repeat 
10690 IF ASCC>=ASCF1 AND ASCC<=ASCF9 THEN FKEY=ASCC-ASCF1+1:GOTO 10730  'F-Keys
10700 N$="Ungültige Eingabe - Bitte wiederholen":GOSUB 12540         'Nachricht
10710 GOTO 10460
10720 '
10730 PRINT CUROFF$;
10740 RETURN 
10750 '
10760 ' *** Block cursor auto-repeat ******************************************
10770 '
10780 A$=INKEY$:IF A$>=CHR$(ASCCUP) THEN 10780
10790 RETURN
10800 '
10810 ' *** Einlesen Stammdatenfelder-Definition ******************************
10820 '
10830 OPEN "I",#1,"B:FELD.DAT"                        
10840   INPUT #1,TMPFELDMAX                                             'Ignore
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
10960 '
10970 ' *** Disk I/O Variablen Initalisierung *********************************
10980 '
10990 GOSUB 11080:MTGLZAHL=MTGLMAX                            'Einlesen MTGLMAX
11000 FOR K=1 TO FELDMAX                                 'Clear DAT$(), SUCH$()
11010   DAT$(K)=""
11020   SUCH$(K)=""
11030 NEXT K
11040 RETURN
11050 '
11060 ' *** Lesen MTGLMAX
11070 '
11071 ' OUT: MTGLMAX
11080 OPEN "I",#1,"B:MTGLZAHL.DAT"
11090   INPUT #1,MTGLMAX$
11100   MTGLMAX=VAL(MTGLMAX$)
11110 CLOSE #1
11120 RETURN
11121 '
11122 ' *** Schreiben MTGLZAHL
11123 '
11124 ' IN: MTGLZAHL
11125 OPEN "O",#1,"B:MTGLZAHL.DAT"
11126   PRINT #1,STR$(MTGLZAHL)
11127 CLOSE #1
11128 RETURN
11130 '
11140 ' *** Maske Feldnamen und Eingabefelder (Pünktchen)
11150 '
11160 ' IN: FELDMAX
11170 ' IN: MTGLZAHL
11200 FOR K=1 TO FELDMAX
11210   Z=FEROW(K)
11220   S=FECOL(K)
11230   PRINT FNCUR$(Z,S);FENAM$(K)
11231   S=S+XOFF
11232   PRINT FNCUR$(Z,S);STRING$(FELNG(K),".")
11240 NEXT
11270 RETURN
11280 '
11290 ' *** Maske Eingabefelder (Pünktchen)
11300 '
11310 ' IN: FELDMAX
11320 FOR K=1 TO FELDMAX
11330   Z=FEROW(K)
11340   S=FECOL(K)+XOFF
11350   PRINT FNCUR$(Z,S);STRING$(FELNG(K),".")
11360 NEXT
11370 RETURN
11380 '
11390 ' *** Maske Feldwerte
11400 '
11410 ' IN: FELDMAX
11420 PRINT HIGHON$
11430 FOR K=1 TO FELDMAX
11440   Z=FEROW(K)
11450   S=FECOL(K)+XOFF
11460   PRINT FNCUR$(Z,S);DAT$(K)
11470 NEXT
11471 PRINT HIGHOFF$
11480 RETURN
11490 '
11500 ' *** FIELDen der Stammsatz-Datei
11510 '
11520 OPEN "R",#2,"B:MV.DAT",462
11530 FIELD #2,236 AS DD1$,225 AS DD2$
11540 RETURN
11550 '
11560 ' *** Einlesen der Felddaten von Disk ***********************************
11570 '
11580 GET #2,MTGLZAHL
11590 FOR LL=1 TO FELDMAX:DAT$(LL)="":NEXT
11600 HLANG=1
11610 FOR K=1 TO 14:LNG=FELNG(K):DAT$(K)=MID$(DD1$,HLANG,LNG):HLANG=LNG+HLANG:NEXT
11620 HLANG=1
11630 FOR K=15 TO 27:LNG=FELNG(K):DAT$(K)=MID$(DD2$,HLANG,LNG):HLANG=LNG+HLANG:NEXT
11640 RETURN
11650 '
11660 ' *** Speichern der Felddaten auf Disk **********************************
11670 '
11671 ' IN: MTGLZAHL
11690 FOR K=1 TO 27:LNG=FELNG(K):DAT$(K)=DAT$(K)+STRING$(LNG-LEN(DAT$(K))," "):NEXT
11700 HH$=""
11710 FOR K=1 TO 14:HH$=HH$+DAT$(K):NEXT:LSET DD1$=HH$
11720 HH$="":FOR K=15 TO 27:HH$=HH$+DAT$(K):NEXT:LSET DD2$=HH$:HH$=""
11730 PRINT FNCUR$(21,1);
11740 PUT #2,MTGLZAHL
11750 N$="Die Daten sind auf Diskette abgespeichert.":GOSUB 12540    'Nachricht 'TODO: Too may messages queued?
11760 RETURN
11770 '
11780 ' *** Ja/Nein - Frage ***************************************************
11790 '
11800 ' IN:  Q$ - Frage
11810 ' OUT: JA 1 = true, 0 = false
11820 N$=Q$+" (J=Ja/N=Nein)":GOSUB 12540                             'Nachricht
11830 PRINT FNCUR$(23,1);DELLINE$             'Delete F-keys 'TODO: Entry-Point
11840 PRINT FNCUR$(24,1);DELLINE$ 
11850 PRINT HOME$                                             'TODO: Necessary?
11860 ANSWER$=INPUT$(1)
11870 PRINT BSP$;                                             'TODO: Necessary?
11880 IF ANSWER$="J" OR ANSWER$="j" THEN JA=1:RETURN
11890 IF ANSWER$="N" OR ANSWER$="n" THEN JA=0:RETURN
11900 GOTO 11860
11910 '
11920 ' *** Testbereich 1 (Alpha) *********************************************
11930 '
11940 VALID=0
11950 IF ASCC>57 AND ASCC<127 THEN VALID=1:GOTO 11970
11960 IF ASCC>31 AND ASCC<48 THEN VALID=1
11970 RETURN
11980 '
11990 ' *** Testbereich 2 (Numerisch) *****************************************
12000 '
12010 VALID=0
12020 IF ASCC>47 AND ASCC<58 THEN VALID=1
12030 RETURN
12040 '
12050 ' *** Testbereich 3 (Alphanumerisch) ************************************
12060 '
12070 VALID=0
12080 IF ASCC>31 AND ASCC<127 THEN VALID=1
12090 RETURN
12100 '
12110 ' *** Testbereich 4 (Numerisch + "/" und ".") ***************************
12120 '
12130 VALID=0
12140 IF ASCC>45 AND ASCC<58 THEN VALID=1
12150 RETURN
12160 '
12170 ' *** DATUMS-TEST (bei VN und M-Liste drucken) **************************
12180 '
12190 IF VAL(TEST$)=0 OR VAL(TEST$)>31 THEN JA=0                           'Tag
12200 RETURN
12210 '
12220 IF VAL(TEST$)=0 OR VAL(TEST$)>12 THEN JA=0                         'Monat
12230 RETURN
12240 '
12250 IF VAL(TEST$)=0 THEN JA=0                                           'Jahr
12260 RETURN
12380 '
12390 ' *** Voreinstellungen im ERFASSEN  (Defaults) **************************
12400 '
12410 DAT$(5)="10***"
12420 DAT$(7)=""11**""
12430 DAT$(11)="12**"
12440 DAT$(12)="13******"
12450 DAT$(21)="3"
12460 DAT$(22)="Nein"
12470 DAT$(23)="Nein"
12480 DAT$(25)="Nein"
12490 RETURN
12500 '
12510 ' *** Nachricht anzeigen *********************************************OK*
12520 '
12530 ' IN: N$ - Nachricht
12540 PRINT FNCUR$(21,2);DELLINE$;INVON$;" ";N$;" ";INVOFF$
12550 RETURN
12560 '
12570 ' *** Bildschirmmaske - Basis ****************************************OK*
12580 '
12590 ' IN: MT$ - Menu title
12600 ' IN: MI$ - Menu ID
12610 PRINT CLS$;
12630 PRINT "00**********************";
12640 PRINT FNCUR$(1,(80-LEN(MT$))/2);MT$;FNCUR$(1,72);MI$
12650 PRINT STRICH$
12660 PRINT FNCUR$(21,1);
12670 PRINT STRICH$
12680 RETURN
12690 '
12700 ' *** Funktionstasten Leiste (KEY LABELS) *******************************
12710 '
12720 ' IN: FKEYS$ - Additional function key labels 
12730 PRINT FNCUR$(23,1);"<F1>=Hilfe <F2>=Zurück <F3>=Anfang <F4>=Hauptmenü ";
12740 PRINT FKEYS$         'LW NO semicolon after FKEY$ else display is garbled
12750 RETURN
12760 '
12770 ' *** restliche funktionstasten bei Verw.-nachweis ausgeben
12780 '
12790 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=Vorwärts             <F8>=Drucken";
12800 PRINT HOME$;" "
12810 RETURN
12820 '
12830 ' *** Eingabe bei Menübildern *******************************************
12840 '
12850 ' IN:  NKEYMAX - max valid numeric key
12860 ' OUT: FKEY - number of pressed function key or 0
12870 ' OUT: NKEY - number of pressed numeric key or 0
12880 NKEY=0:FKEY=0
12890 FOR I=1 TO 30:A$=INKEY$:NEXT                           'Debounce keyboard
12900 A$=INPUT$(1)
12910 IF A$>"0" AND ASC(A$)<48+NKEYMAX+1 THEN NKEY=VAL(A$):RETURN
12920 IF A$>CHR$(240) AND A$<CHR$(245) THEN FKEY=ASC(A$)-240:RETURN
12930 N$="Sie haben eine falsche Taste gedrückt - Bitte neue Auswahl treffen.":GOSUB 12540 'Nachricht
12940 GOTO 12900
12950 '
12960 ' *** Rückkehr ins Hauptmenü ? ******************************************
12970 '
12980 Q$="Sie wollen wirklich ins Hauptmenü?":GOSUB 11820        'Ja/Nein-Frage
12990 IF JA AND MT$="STAMMSATZ ÄNDERN" THEN GOSUB 13180
13000 IF JA=1 THEN 17520 ELSE PRINT HOME$:PRINT FNCUR$(21,1);DELLINE$:GOSUB 12730:RETURN 'Programmende 18250
13010 '
13020 ' *** Rückkehr aus Hilfe-Text? ***************************************OK*
13030 '
13040 N$="Zurück? (J=Ja)":GOSUB 12540                                'Nachricht
13050 GOSUB 11830                                          'TODO why not *****?
13060 IF JA=0 THEN 13040
13070 RETURN
13080 '
13090 ' *** Clear DAT$() ******************************************************
13100 '
13110 FOR II=1 TO FELDMAX
13120   DAT$(II)=""
13130 NEXT
13140 RETURN
13150 '
13160 ' *** UPRO FÜR ÄNDERN-EXIT **********************************************
13170 '
13180 Q$="Wollen Sie die Daten speichern?":GOSUB 11820           'Ja/Nein-Frage
13190 IF JA THEN GOSUB 11690
13200 RETURN
13210 '
13220 ' **** VN - Subroutines, permitting one page handling *******************
13230 ' ****         by 04********** 21*******              *******************
13240 '
13250 ' **** OPEN & FIELDING VERWENDUNGSNACHWEIS ******************************
13260 '
13270 OPEN "R",#3,"B:VN.NDX",4
13280 FIELD #3,4 AS POINT0$
13290 OPEN "R",#4,"B:VN.POI",8
13300 FIELD #4,4 AS POINT1$, 4 AS POINT2$
13310 OPEN "R",#5,"B:VN.DAT",560
13320 FIELD #5,240 AS DDD1$,240 AS DDD2$,80 AS DDD3$
13330 RETURN
13340 '
13350 ' *** VERWENDUNGSNACHWEIS FÜR NEUES MITGLIED ANLEGEN ********************
13360 '
13361 ' IN: MTGLZAHL
13370 GET #3,1                            'letzte Seitennummer einlesen dann +1
13380 TMPOINT=VAL(POINT0$)+1
13390 LSET DDD1$=""                                    'leeren Record speichern
13400 LSET DDD2$=""
13410 LSET DDD3$=""
13420 PUT #5,TMPOINT
13430 LSET POINT0$=STR$(TMPOINT)             'letzte Seitennummer neu speichern
13440 PUT #3,1
13450 PUT #3,MTGLZAHL+1
13460 LSET POINT1$="0000"
13470 LSET POINT2$="0000"
13480 PUT #4,TMPOINT
13490 RETURN
13500 '
13510 ' **** ERSTE SEITE FINDEN & LESEN ***************************************
13520 '
13530 ' IN: MTGLZAHL
13540 GET #3,MTGLZAHL+1                                            'Index_Datei
13550 CPOINT=VAL(POINT0$)
13560 GET #5,CPOINT:GOSUB 14620                                       'Einlesen
13570 BLATT=1
13580 GET #4,CPOINT                                              'Pointer lesen
13590 RETURN
13600 '
13610 ' **** SEITE RÜCKWÄRTS **************************************************
13620 '
13630 TMPOINT=VAL(POINT1$)
13640 IF TMPOINT<>0 THEN 13680
13650 N$="Erste Seite":GOSUB 12540                                   'Nachricht
13660 DELAY=9999:GOSUB 10280                                             'Delay
13670 RETURN
13680 CPOINT=TMPOINT
13690 BLATT=BLATT-1
13700 GET #5,CPOINT:GOSUB 14620   
13710 GET #4,CPOINT:GOSUB 14780                             'Neue Pointer lesen
13720 HP=1:VP=1
13730 GOSUB 13990
13740 DELAY=100:GOSUB 10280                                              'Delay
13750 RETURN
13760 '
13770 ' **** SEITE VORWÄRTS  **************************************************
13780 '
13790 TMPOINT=VAL(POINT2$)
13800 IF TMPOINT=0 THEN 13870
13810 CPOINT=TMPOINT
13820 BLATT=BLATT+1
13830 GET #5,CPOINT:GOSUB 14620
13840 GET #4,CPOINT:GOSUB 14780
13850 HP=1:VP=1
13860 RETURN
13870 N$="Letzte Seite"
13880 IF INSTR(SPACE$(20),DAT$(26))=0 THEN 13920 
13890 GOSUB 12540                                                    'Nachricht
13900 DELAY=20000:GOSUB 10280                                            'Delay
13910 RETURN
13920 Q$="Letzte Seite - Neue Seite anlegen?":GOSUB 11820        'Ja/Nein-Frage
13930 N$="":GOSUB 12540                                              'Nachricht
13940 IF JA=0 THEN 13990
13950 GOSUB 14230
13960 BLATT=BLATT+1
13970 GOSUB 14780
13980 HP=1:VP=1
13990 GOSUB 12730
14000 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=vorwärts";
14010 PRINT TAB(54);"<F9>=Blatt drucken";
14020 PRINT HOME$;" "
14030 RETURN
14040 '
14050 ' **** ERSTE SEITE EINES MITGLIEDS ANLEGEN ******************************
14060 '
14070 GET #3,1
14080 LSET POINT0$=STR$(VAL(POINT0$)+1)                                  'NDX+1
14090 PUT #3,1
14100 LSET POINT1$="0000"                                        'Page pointers
14110 LSET POINT2$="0000"
14120 PUT #4,VAL(POINT0$)
14130 LSET POINT0$=POINT0$                                          'First page
14140 PUT #3,MTGLZAHL+1
14150 LSET DDD1$=""                                                 'Datenblatt
14160 LSET DDD2$=""
14170 LSET DDD3$=""
14180 PUT #5,VAL(POINT0$)
14190 RETURN
14200 '
14210 ' **** ALLGEMEIN EINE NEUE SEITE ANLEGEN ********************************
14220 '
14230 GET #3,1                                                           'NDX+1
14240 LSET POINT0$=STR$(VAL(POINT0$)+1)
14250 PUT #3,1:TMPOINT$=POINT0$    'Vektoren der bisher letzten Seite verbiegen
14260 LSET POINT2$=POINT0$                                'Verbiege Vektoren...
14270 PUT #4,CPOINT                                         '...der alten Seite
14280 LSET POINT1$=STR$(CPOINT)                             '...der neuen Seite
14290 LSET POINT2$="0000"                                                '.. ""
14300 CPOINT=VAL(TMPOINT$)                                       'Nr neue Seite
14310 PUT #4,CPOINT
14320 LSET DDD1$=""                                           'Write empty page
14330 LSET DDD2$=""
14340 LSET DDD3$="" 
14350 PUT #5,CPOINT
14360 DATEN$=SPACE$(20)
14370 FOR II=1 TO 26
14380   DAT$(II)=DATEN$
14390 NEXT
14400 RETURN
14410 '
14420 ' **** SEITE SPEICHERN **************************************************
14430 '
14440 DATEN$=""
14450 FOR II=1 TO 12
14460   DATEN$=DATEN$+DAT$(II)
14470 NEXT
14480 LSET DDD1$=DATEN$:DATEN$=""
14490 FOR II=13 TO 24
14500   DATEN$=DATEN$+DAT$(II)
14510 NEXT
14520 LSET DDD2$=DATEN$:DATEN$=""
14530 FOR II=25 TO 26
14540   DATEN$=DATEN$+DAT$(II)
14550 NEXT 
14560 LSET DDD3$=DATEN$:DATEN$=""
14570 PUT #5,CPOINT
14580 RETURN
14590 '
14600 ' **** HACK ! (ZERSTÜCKELN DER DATEN-STRINGS) ***************************
14610 '
14620 FOR II=1 TO 12
14630   DAT$(II)=MID$(DDD1$,(II-1)*20+1,20)
14640 NEXT
14650 FOR II=13 TO 24
14660   DAT$(II)=MID$(DDD2$,(II-13)*20+1,20)
14670 NEXT 
14680 FOR II=25 TO 26
14690   DAT$(II)=MID$(DDD3$,(II-25)*20+1,20)
14700 NEXT 
14710 RETURN
14720 '
14730 ' **** UPRO DATENAUSGABE VN  ********************************************
14740 '
14750 ' IN: MTGLZAHL
14760 ' IN: NAMM$
14770 ' IN: BLATT
14780 PRINT DELHIGH$;                                          'TODO Necessary?
14790 PRINT HIGHON$
14800 PRINT FNCUR$(5,16);MTGLZAHL
14810 PRINT FNCUR$(5,29);NAMM$
14820 PRINT FNCUR$(3,8);BLATT
14830 FOR K=1 TO 26
14840   HOR=6-39*(K>13)
14850   VER=6+K+13*(K>13)
14860   FOR J=1 TO 3                                                 'Von-Datum
14870     PRINT FNCUR$(VER,HOR+(J-1)*3);MID$(DAT$(K),-1+J*2,2)
14880   NEXT
14890   FOR J=1 TO 3                                                 'Bis-Datum
14900     PRINT FNCUR$(VER,HOR+13+(J-1)*3);MID$(DAT$(K),5+J*2,2)
14910   NEXT
14920   PRINT FNCUR$(VER,HOR+26);MID$(DAT$(K),13,8)                        'Amt
14930 NEXT
14940 PRINT HIGHOFF$
14950 RETURN
14960 '
14970 ' *** Verwendungsnachweis drucken ***************************************
14980 '
14990 ' IN: BLATT
15000 ' IN: MITGLZAHL
15010 ' IN: NAMM$
15020 Q$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 11820 'Ja/Nein-Frage TODO Back to main code?
15030 IF JA=0 THEN 15250
15040 LIND$=SPACE(2," ")
15050 '
15060 LPRINT STRICH$
15070 LPRINT
15080 LPRINT LIND$;"Blatt:";BLATT
15090 LPRINT
15100 LPRINT LIND$;"Mitglieds-Nr.:";MTGLZAHL;TAB(24);"Name:";NAMM$
15110 LPRINT
15120 FOR K=1 TO 13
15130   KK=K:GOSUB 15220
15140   LPRINT LIND$;"Von:";VON$;" Bis:";BIS$;" Amt:";AMT$;
15150   KK=K+13:GOSUB 15220
15160   LPRINT " Von:";VON$;" Bis:";BIS$;" Amt:";AMT$
15170 NEXT 
15180 LPRINT
15190 LPRINT STRICH$
15200 RETURN
15210                                           'Format: VVVVVV BBBBBB AAAAAAAA
15220 VON$=LEFT$(DAT$(KK),2)+"."+MID$(DAT$(KK),3,2)+"."+MID$(DAT$(KK),5,2)
15230 BIS$=MID$(DAT$(KK),7,2)+"."+MID$(DAT$(KK),9,2)+"."+MID$(DAT$(KK),11,2)
15240 AMT$=RIGHT$(DAT$(KK),8)
15250 RETURN
15260 '
15270 ' ### Unterprogramme "Mitgliedsliste drucken" ###########################
15280 '
15300 ' *** Mitgliedsliste drucken (Kopf)
15310 '
15320 ' IN: DATUM$
15330 LPRINT TAB(16);"MITGLIEDSLISTE 25*******************************"
15340 LPRINT 
15350 LPRINT TAB(32);"Stand:";DATUM$
15360 LPRINT
15370 LPRINT TAB(3);"Mitgl."; 
15380 LPRINT TAB(10);"Name";
15390 LPRINT TAB(36);"Vorname, Anschrift";
15400 LPRINT TAB(63);"Status";
15410 LPRINT TAB(71);"Amt"
15420 RETURN
15430 '
15440 ' *** Mitgliedsliste drucken (Mitglied)
15450 '
15460 ' IN:  MTGLZAHL
15470 ' OUT: ISBRKPRNT (1=stop printing, 0=continue printing)
15480 AMT$=""
15490 IN=INSTR(DAT$(6),"22**"):IF IN<>0 THEN AMT$=MID$(DAT$(6),IN,6):GOTO 15530
15500 IN=INSTR(DAT$(6),"23**"):IF IN<>0 THEN AMT$=MID$(DAT$(6),IN,5):GOTO 15530
15510 IN=INSTR(DAT$(6),"24**"):IF IN<>0 THEN AMT$=MID$(DAT$(6),IN,3)
15520 '
15530 LPRINT
15540 LPRINT TAB(5);MTGLZAHL;
15550 LPRINT TAB(10);DAT$(1);                                         'Nachname
15560 LPRINT TAB(36);DAT$(2);                                          'Vorname
15570 LPRINT TAB(65);"(";LEFT$(DAT$(5),1);")";                          'Status
15580 LPRINT TAB(71);AMT$                                                  'Amt
15590 LPRINT TAB(36);DAT$(10)                                           'Straße
15600 LPRINT TAB(36);DAT$(11);                                    'Postleitzahl
15610 LPRINT TAB(41);DAT$(12)                                              'Ort
15620 LPRINT TAB(36);"Tel.:";DAT$(13)                                  'Telefon
15630 LPRINT
15640 '
15650 IF INKEY$="" THEN 15680          'TODO Move back to main code? Used twice
15660 Q$="Drucken abbrechen?":GOSUB 11820                        'Ja/Nein-Frage
15670 IF JA THEN ISBRKPRNT=1 ELSE ISBRKPRNT=0
15680 RETURN
15690 '
15700 ' *** Adressaufkleber drucken
15710 '
15720 N$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 11820 'Ja/Nein-Frage
15730 IF JA=0 THEN RETURN
15740 LIND$=SPACE(3," ")
15750 '
15760 LPRINT LIND$;DAT$(7)                                              'Anrede
15770 LPRINT LIND$;LEFT$(DAT$(2),INSTR(DAT$(2)," "));DAT$(1)  'Vorname Nachname
15780 LPRINT
15790 LPRINT LIND$;DAT$(10)                                  'Straße Hausnummer
15800 LPRINT
15810 LPRINT LIND$;DAT$(11);" ";DAT$(12)                      'Postleitzahl Ort
15820 LPRINT
15830 LPRINT
15840 LPRINT
15850 LPRINT
15860 LPRINT
15870 LPRINT
15880 RETURN
15890 '
15900 ' *** Stammsatz drucken (Bildschirmformat)
15910 '
15920 RESTORE 16030
15930 LPRINT STRICH$
15940 LPRINT
15950 LPRINT TAB(30);"Mitgliedsnummer  ";MTGLZAHL
15960 LPRINT
15970 LPRINT TAB(2);FENAM$(1);DAT$(1);TAB(41);"Beruf:"
15980 FOR I=1 TO 6:READ A,B
15990   LPRINT TAB(2);FENAM$(A);DAT$(A);
16000   IF B=0 THEN LPRINT:GOTO 16020
16010   LPRINT TAB(41);FENAM$(B);DAT$(B);
16020 NEXT
16030 DATA 2,15,3,16,4,0,5,17,6,18,0,19
16040 FOR I=7 TO 14
16050   LPRINT TAB(2);FENAM$(I);DAT$(I);
16060   LPRINT TAB(41);FENAM$(I+13);DAT$(I+13);
16070 NEXT
16080 LPRINT
16090 LPRINT
16100 LPRINT
16110 LPRINT STRICH$
16120 RETURN
16130 '
16140 ' *** Mitgliedsvermerke drucken
16150 '
16160 ' IN: NAMM$ =
16170 LIND$=SPACE(9," ")
16180 SLOT1$="___"
16190 SLOT2$="___ / ___"
16200 SLOT3$="___ / ___ / ___"
16210 '
16220 LPRINT TAB(10);"Mitgliedsnr.: ";SLOT1$;
16230 LPRINT TAB(28);"Name: ";NAMM$;
16240 LPRINT TAB(65);"Datum: ";SLOT3$
16250 LPRINT
16260 LPRINT LIND$;"26*************************** ";SLOT3$
16270 LRPINT
16280 LPRINT LIND$;"27*************************** ";SLOT3$
16290 LRPINT
16300 LPRINT LIND$;"28*************************** ";SLOT3$
16310 LRPINT
16320 LPRINT LIND$;"29*************************** ";SLOT3$
16330 LRPINT
16340 LPRINT LIND$;"30*************************** ";SLOT2$
16350 LRPINT
16360 LPRINT LIND$;"31*************************** ";SLOT3$
16370 LRPINT
16380 LPRINT LIND$;"32*************************** ";SLOT3$
16390 LRPINT
16400 LPRINT LIND$;"33*************************** ";SLOT1$
16410 LRPINT
16420 LPRINT LIND$;"34*************************** ";SLOT3$
16430 LRPINT
16440 LPRINT LIND$;"35*************************** ";SLOT3$
16450 LRPINT
16460 LPRINT LIND$;"36*************************** ";SLOT3$
16470 LRPINT
16480 LPRINT LIND$;"37*************************** ";SLOT3$
16490 LRPINT
16500 LPRINT LIND$;"38*************************** ";SLOT3$
16510 RETURN
16520 '
16530 ' ### Hilfe-Texte #######################################################
16540 '
16550 ' *** Hilfe-Text "Mitgliederverwaltung" 
16560 '
16570 MT$="HILFE":GOSUB 12610                                      'Basis Maske
16580 PRINT HOME$
16590 GOSUB 13040                                      'Rückkehr aus Hilfe-Text
16600 RETURN
16610 '
16620 ' *** Hilfe-Text "Stammsatzpflege" 
16630 '
16640 MT$="HILFE":GOSUB 12610                                      'Basis Maske
16650 PRINT HOME$
16660 GOSUB 13040                                      'Rückkehr aus Hilfe-Text
16670 RETURN
16680 '
16690 ' *** Hilfe-Text "Stammsatz anlegen"
16700 '
16710 SAVEMT$=MT$:MT$="HILFE":GOSUB 12610:MT$=SAVEMT$              'Basis Maske
16720 PRINT HOME$
16730 GOSUB 13040                                      'Rückkehr aus Hilfe-Text
16740 DATENAUSG=1                                                          '???
16750 RETURN
16760 '
16770 ' *** Hilfe-Text "Stammsatz ändern (I)"
16780 '
16790 MT$="HILFE":GOSUB 12610                                      'Basis Maske
16800 PRINT HOME$
16810 GOSUB 13040                                      'Rückkehr aus Hilfe-Text
16820 RETURN
16830 '
16840 ' *** Hilfe-Text "Stammsatz ändern (II)"
16850 '
16860 SAVEMT$=MT$:MT$="HILFE":GOSUB 12610:MT$=SAVEMT$              'Basis Maske
16870 PRINT HOME$
16880 GOSUB 13040                                      'Rückkehr aus Hilfe-Text
16890 GOSUB 12610                                                  'Basis Maske
16900 GOSUB 12730                                               'F-Tastenleiste
16910 RETURN
16920 '
16930 ' *** Hilfe-Text "Stammsatz löschen"
16940 '
16950 MT$="HILFE":GOSUB 12610                                      'Basis Maske
16960 PRINT HOME$
16970 GOSUB 13040                                      'Rückkehr aus Hilfe-Text
16980 RETURN
16990 '
17000 ' *** Hilfe-Text "Stammsatz suchen"
17010 '
17020 IF DRUCKFLAG THEN 17310                                              '???
17030 SAVEMT$=MT$:MT$="HILFE":GOSUB 12610:MT$=SAVEMT$              'Basis Maske
17040 PRINT HOME$
17050 GOSUB 13040                                      'Rückkehr aus Hilfe-Text
17060 RETURN
17070 ' 
17080 ' *** Hilfe-Text für "Stammsatz drucken"
17090 '
17100 SAVEMT$=MT$:MT$="HILFE":GOSUB 12610:MT$=SAVEMT$              'Basis Maske
17110 PRINT HOME$
17120 GOSUB 13040                                      'Rückkehr aus Hilfe-Text
17130 RETURN
17140 '
17150 ' *** Hilfe-Text "Verwendungsnachweis (I)"
17160 '
17170 MT$="HILFE":GOSUB 12610                                      'Basis Maske
17180 PRINT HOME$
17190 GOSUB 13040                                      'Rückkehr aus Hilfe-Text
17200 RETURN
17210 '
17220 ' *** Hilfe-Text "Verwendungsnachweis (II)"
17230 '
17240 SAVEMT$=MT$:MT$="HILFE":GOSUB 12610:MT$=SAVEMT$              'Basis Maske
17250 PRINT HOME$
17260 GOSUB 13040                                      'Rückkehr aus Hilfe-Text
17270 RETURN
17280 '
17290 ' *** Hilfe-Text "Mitgliedsliste drucken"
17300 '
17310 SAVEMT$=MT$:MT$="HILFE":GOSUB 12610:MT$=SAVEMT$              'Basis Maske
17320 PRINT HOME$
17330 GOSUB 13040                                      'Rückkehr aus Hilfe-Text
17340 RETURN
17350 '
17360 ' *** Programfehler
17370 '
17380 LIND$=STRING$(15," ")
17390 GOSUB 12610                                                  'Basis Maske
17400 PRINT HOME$
17410 PRINT LIND$;"Ein Programmfehler ist aufgetreten."
17420 PRINT   
17430 PRINT LIND$;"- Notieren Sie folgende Fehlerinformation:"
17440 PRINT LIND$;"    ";HIGHON$;"ERL";HIGHOFF$;" (Error Line)";HIGHON$;ERL;HIGHOFF$
17450 PRINT LIND$;"    ";HIGHON$;"ERR";HIGHOFF$;" (Error Code)";HIGHON$;ERR;HIGHOFF$
17460 PRINT LIND$;"- Drücken Sie am Rechner den Knopf hinten rechts."
17470 PRINT LIND$;"19**************************"
17480 PRINT
17490 '
17500 ' *** Programmende
17510 '
17520 ON ERROR GOTO 0                                   'Disable error trapping
17530 PRINT HIGHOFF$;CURON$;
17540 PRINT FNCUR$(22,1)
17550 RESET
17560 'LW RUN "DISKMENU.EXE"
17570 END
