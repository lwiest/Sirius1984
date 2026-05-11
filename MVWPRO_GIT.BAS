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
1110 DIM FEROW(FELDMAX)                           'Zeile des Feldnamens (1..25)
1120 DIM FECOL(FELDMAX)                          'Spalte des Feldnamens (1..80)
1130 DIM FELNG(FELDMAX)                                       'Länge des Feldes
1140 DIM FETYP(FELDMAX)                                         'Typ des Feldes
1150 '
1160 DIM DAT$(FELDMAX)                                              'Datenarray
1170 DIM SUCH$(FELDMAX)                                              'Sucharray
1180 '
1190 NUMSUCHMAX=2
1200 DIM NUMSUCH$(NUMSUCHMAX)
1210 DIM NUMSUCHROW(NUMSUCHMAX)
1220 NUMSUCHROW(1)=6
1230 NUMSUCHROW(2)=8
1240 '
1250 BYNUMS=1                                                  'Value of SEARCH
1260 BYMASK=2
1270 '
1280 BSP$    =CHR$(8)                                                'Backspace
1290 DEL$    =BSP$+"."+BSP$                                             'Delete
1300 ESC$    =CHR$(27)                                'Einleitung Escapesequenz
1310 CLS$    =ESC$+"E"                                      'Bildschirm löschen
1320 INVON$  =ESC$+"p"                                              'Invers ein
1330 INVOFF$ =ESC$+"q"                                              'Invers aus
1340 CURON$  =ESC$+"y5"                                             'Cursor ein
1350 CUROFF$ =ESC$+"x5"                                             'Cursor aus
1360 HIGHON$ =ESC$+"("                                           'Helle Schrift
1370 HIGHOFF$=ESC$+")"                                          'Dunkle Schrift
1380 DELLINE$=ESC$+"K"                                           'Zeile löschen
1390 DELHIGH$=ESC$+"+"                                   'Helle Schrift löschen
1400 STRICH$ =STRING$(79,"-")                                           'Strich
1410 IN$=SPACE$(10)                                     'Indent für Hilfe-Texte
1420 '
1430 DEF FNCUR$(Z,S)=ESC$+"Y"+CHR$(31+Z)+CHR$(31+S)       'Cursor positionieren
1440 HOME$=FNCUR$(3,1)                                    'Cursor Home Position
1450 ' 
1460 CKEYUP=1
1470 CKEYDN=2
1480 CKEYLT=3
1490 CKEYRT=4
1500 '
1510 ASCBSP=8
1520 ASCRET=13
1530 ASCESC=27
1540 ASC0=48
1550 ASC1=49
1560 ASCCUP=200
1570 ASCCDN=201
1580 ASCCLT=202
1590 ASCCRT=203
1600 ASCF1=241
1610 ASCF4=244
1620 ASCF9=249
1630 '
1640 ' Other global variables:
1650 ' -----------------------
1660 ' LIND$    - Local text indent
1670 ' F        - Feldindex 1..27 (FELDMAX)
1680 ' MTGLMAX  - Höchste auf Disk gespeicherte Mitgliedsnummer
1690 ' MTGLZAHL - Momentane Mitgliedsnummer
1700 '
1710 XOFF=14             'Offset Colum Feldname to Column Wertefeld (Pünktchen)
1720 '
1730 ' TIP: Do not let the cursor slip into line 25 -> display will scroll up
1740 ' TIP: Use semicolons with PRINT carefully -> display driver bug
1750 '
1760 ON ERROR GOTO 19120                                 'Enable error trapping
1770 '
1780 GOSUB 10050    'Tasten deaktivieren, ASCII Codes für Cursortasten vergeben
1790 PRINT CUROFF$;
1800 '
1810 ' *** Beginn des Mitgliederverwaltungsprogramms **************************
1820 '
1830 MT$="MITGLIEDERVERWALTUNG":MI$="G-01000":GOSUB 13100          'Basis Maske
1840 Q$="Bitte Daten-Diskette in Laufwerk B einlegen. Bereit?":GOSUB 12310
1850 IF JA=0 THEN 19250                                           'Programmende
1860 '
1870 N$="Einen Moment bitte...":GOSUB 12970    'Nachricht und F-keys ausblenden
1880 GOSUB 10990                          'Einlesen Stammdatenfelder-Definition
1890 GOSUB 11800                                   'Fielden der Stammsatz-Datei
1900 GOSUB 13770                       'Fielden der Verwendungsnachweis-Dateien
1910 GOSUB 11260                                              'Einlesen MTGLMAX
1920 GOSUB 13030                                             'Nachricht löschen
1930 '
1940 ' ################# Mitgliederverwaltungsmenü (MVW-Menü) #################
1950 '
1960 MT$="MITGLIEDERVERWALTUNG":MI$="G-01000":GOSUB 13100          'Basis Maske
1970 FKEYS$="":GOSUB 13210                           'F-Tastenleiste einblenden
1980 VNFLAG=0                                                              '???
1990 PRINT FNCUR$(4,26);"1 - Stammsatzpflege"                         'Menütext
2000 PRINT FNCUR$(6,26);"2 - Verwendungsnachweis"
2010 PRINT FNCUR$(8,26);"3 - Mitgliedsliste drucken"
2020 FKEYMAX=4:NKEYMAX=3:GOSUB 13370                              'Menü-Eingabe
2030 DRUCKFLAG=0                                                           '???
2040 IF NKEY THEN ON NKEY GOTO 2150,5900,6720
2050 ON FKEY GOTO 2060,2070,1960,2070         'Hilfe, Zurück, Anfang, Hauptmenü
2060 GOSUB 17080:GOTO 1960                                      'F1->Hilfe-Text
2070 GOSUB 13530:GOTO 2020                           'F2->Zurück, F4->Hauptmenü
2080 '
2090 ' ########################### Stammsatzpflege ############################
2100 '
2130 ' *** Stammsatzpflege-Menü ***********************************************
2140 '
2150 MT$="STAMMSATZPFLEGE":MI$="G-01100":GOSUB 13100               'Basis Maske
2160 FKEYS$="":GOSUB 13210                           'F-Tastenleiste einblenden
2170 PRINT FNCUR$(4,29);"1 - Stammsatz anlegen"
2180 PRINT FNCUR$(6,29);"2 - Stammsatz ändern"
2190 PRINT FNCUR$(8,29);"3 - Stammsatz löschen"
2200 PRINT FNCUR$(10,29);"4 - Stammsatz drucken"
2210 PRINT FNCUR$(12,29);"5 - Stammsatz suchen"
2220 FKEYMAX=4:NKEYMAX=5:GOSUB 13370                              'Menü-Eingabe
2230 IF NKEY THEN ON NKEY GOTO 2300,2840,3460,5530,3890
2240 ON FKEY GOTO 2250,1960,1960,2260         'Hilfe, Zurück, Anfang, Hauptmenü
2250 GOSUB 17300:GOTO 2150                                      'F1->Hilfe-Text
2260 GOSUB 13530:GOTO 2220                                       'F4->Hauptmenü
2270 '
2280 ' *** Stammsatz erfassen von 02************ ******************************
2290 '
2300 MTGLZAHL=MTGLMAX+1
2310 F=1
2320 DATENAUSG=1                                                           '???
2330 MT$="STAMMSATZ ANLEGEN":MI$="G-01101":GOSUB 13100             'Basis Maske
2340 FKEYS$="<F7>=Speichern":GOSUB 13210             'F-Tastenleiste einblenden
2350 GOSUB 11150                                                  'Clear DAT$()
2360 GOSUB 12760                                           'Defaultwerte setzen
2370 PRINT FNCUR$(3,31);"Mitgliedsnummer";HIGHON$;MTGLZAHL;HIGHOFF$
2380 IF DATENAUSG=1 THEN GOSUB 11680:DATENAUSG=0     'Felder und Feldwerte '???
2390 Z=FEROW(F):S=FECOL(F)+XOFF:LNG=FELNG(F):TYP=FETYP(F):MUSS=FEMUSS(F)
2400 FKEYMASK$="12347":CKEYMASK$="12":INTEXT$=DAT$(F)
2410 GOSUB 10430                                                         'Input
2420 IF ASCC<>ASCRET THEN 2460                                 'Carriage return
2430   DAT$(F)=OUTTEXT$
2440   F=F+1:IF F>FELDMAX THEN F=1
2450   GOTO 2390
2460 IF CKEY=0 THEN 2510                                           'Cursor keys
2470   PRINT FNCUR$(Z,S);HIGHON$;DAT$(F);HIGHOFF$;STRING$(LNG-LEN(DAT$(F)),".")
2480   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FELDMAX
2490   IF CKEY=CKEYDN THEN F=F+1:IF F>FELDMAX THEN F=1
2500   GOTO 2390
2510 IF FKEY=1 THEN GOSUB 17550:GOTO 2330                       'F1->Hilfe-Text
2520 IF FKEY=2 THEN GOSUB 2610:GOTO 2150                       'Exit "Erfassen"
2530 IF FKEY=3 THEN GOSUB 2610:GOTO 1960                       'Exit "Erfassen"
2540 IF FKEY<>4 THEN 2590
2550   GOSUB 13600                                'Rückkehr ins Hauptmenü Frage
2560   IF JA=0 THEN GOSUB 13210:GOTO 2410                    'F-Keys einblenden
2570   GOSUB 2610                                              'Exit "Erfassen"
2580   GOTO 19250                                                 'Programmende
2590 IF FKEY=7 THEN GOSUB 2750:GOTO 2300
2600 '
2610 ' TMPF=1                                                              '???
2620 ' IF INSTR(SPACE$(25),DAT$(TMPF))=0 THEN 2670
2630 '   N$="DEBUG: DAT$() with 25 spaces!":GOSUB 12510              'Nachricht
2640 '   TMPF=TMPF+1:IF TMPF<(FELDMAX+1) THEN 2620
2650 '   MTGLZAHL=MTGLZAHL-1
2660 '   GOTO 2750
2670 '
2680 GOSUB 10940                                             'Debounce keyboard
2690 Q$="Wollen Sie vorher den Stammsatz speichern?":GOSUB 12310 'Ja/Nein-Frage
2700 IF JA=1 THEN 2750                                        'Nachricht und...
2710   N$="Stammsatz wird nicht gespeichert.":GOSUB 12970    '...F-Keys löschen
2720   MTGLZAHL=MTGLZAHL-1
2730   GOTO 2790
2740 '                                                            'Nachricht...
2750 N$="Stammsatz wird gespeichert...":GOSUB 12970      '...und F-Keys löschen
2760 GOSUB 12110                                       'Speichern der Felddaten
2770 GOSUB 13880                                        'Erste VN-Seite anlegen
2780 MTGLMAX=MTGLZAHL:GOSUB 11350                            'Schreiben MTGLMAX
2790 GOSUB 13030                                             'Nachricht löschen
2800 RETURN
2810 ' 
2820 ' *** Stammsatz ändern von 03************ ********************************
2830 '
2840 MT$="STAMMSATZ ÄNDERN":MI$="G-01102":GOSUB 13100              'Basis Maske
2850 FKEYS$="":GOSUB 13210                           'F-Tastenleiste einblenden
2860 PRINT FNCUR$(4,24)"Bitte geben Sie die Mitgliedsnummer" 
2870 PRINT FNCUR$(5,25)"des zu ändernden Stammsatzes ein."
2880 PRINT FNCUR$(6,23);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
2890 PRINT FNCUR$(7,24)"Beenden Sie die Eingabe mit RETURN."
2900 PRINT FNCUR$(9,39)"=>..."
2910 BADINPUT=0
2920 Z=9:S=41:LNG=3:TYP=2:MUSS=1
2930 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
2940 GOSUB 10440                                                       'Input++
2950 IF FKEY=1 THEN GOSUB 17770:GOTO 2840                               'F-Keys
2960 IF FKEY=2 THEN 2150
2970 IF FKEY=3 THEN 1960
2980 IF FKEY=4 THEN GOSUB 13530:GOTO 2940         'Rückkehr ins Hauptmenü-Frage
2990 '
3000 M=VAL(OUTTEXT$)
3010 IF M>=1 AND M<=MTGLMAX THEN 3060
3020   N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12890       'Nachricht
3030   BADINPUT=1
3040   INTEXT$=OUTTEXT$                                                    '???
3050   GOTO 2920
3060 MTGLZAHL=M
3070 N$="Stammsatz wird gelesen...":GOSUB 12970   'Nachricht und F-Keys löschen
3080 GOSUB 11860                                                   'Daten lesen
3090 GOSUB 13030                                             'Nachricht löschen
3100 '
3110 GOSUB 13100                                                   'Basis Maske
3120 FKEYS$="<F7>=Speichern":GOSUB 13210             'F-Tastenleiste einblenden
3130 PRINT FNCUR$(3,31);"Mitgliedsnummer";HIGHON$;MTGLZAHL;HIGHOFF$
3140 GOSUB 11680                                 'Felder und Feldwerte ausgeben
3150 F=1
3160 Z=FEROW(F):S=FECOL(F)+XOFF:LNG=FELNG(F):TYP=FETYP(F):MUSS=FEMUSS(F)
3170 FKEYMASK$="12347":CKEYMASK$="12":INTEXT$=DAT$(F)
3180 GOSUB 10430                                                         'Input
3190 IF ASCC<>ASCRET THEN 3230
3200   DAT$(F)=OUTTEXT$
3210   F=F+1:IF F>FELDMAX THEN F=1
3220   GOTO 3160
3230 IF CKEY=0 THEN 3280
3240   PRINT FNCUR$(Z,S);HIGHON$;DAT$(F);HIGHOFF$;STRING$(LNG-LEN(DAT$(F)),".")
3250   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FELDMAX
3260   IF CKEY=CKEYDN THEN F=F+1:IF F>FELDMAX THEN F=1
3270   GOTO 3160
3280 IF FKEY=1 THEN GOSUB 17880:GOTO 3110
3290 IF FKEY=2 THEN GOSUB 3370:GOTO 2840               'Eingabe Mitgliedsnummer
3300 IF FKEY=3 THEN GOSUB 3370:GOTO 1960                              'MVW-Menü
3310 IF FKEY=7 THEN GOSUB 3390:GOTO 2840
3320 GOSUB 13600                             'F4-> Rückkehr ins Hauptmenü-Frage
3330 IF JA=0 THEN GOSUB 13210:GOTO 3180                      'F-Keys einblenden
3340 GOSUB 3370                                                  'Exit "Ändern'
3350 GOTO 19250                                                   'Programmende
3360 '
3370 Q$="Wollen Sie den Stammsatz speichern?":GOSUB 12310        'Ja/Nein-Frage
3380 IF JA=0 THEN RETURN
3390 N$="Stammsatz wird gespeichert...":GOSUB 12970 'Nachricht und F-Keys löschen
3400 GOSUB 12110                                               'Daten speichern
3410 GOSUB 13030                                             'Nachricht löschen
3420 RETURN
3430 '
3440 ' *** Stammsatz löschen von 04********** *********************************
3450 '
3460 MT$="STAMMSATZ LÖSCHEN":MI$="G-01103":GOSUB 13100             'Basis Maske
3470 FKEYS$="":GOSUB 13210                           'F-Tastenleiste einblenden
3480 PRINT FNCUR$(4,24);"Bitte geben Sie die Mitgliedsnummer"         'Menütext
3490 PRINT FNCUR$(5,25);"des zu löschenden Mitglieds ein."
3500 PRINT FNCUR$(6,23);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
3510 PRINT FNCUR$(7,15);"(Dabei wird nur der Status des Mitglieds verändert.)"
3520 PRINT FNCUR$(8,24);"Beenden Sie die Eingabe mit RETURN."
3530 PRINT FNCUR$(10,39);"=>..."
3540 BADINPUT=0
3550 Z=10:S=41:LNG=3:TYP=2:MUSS=1
3560 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
3570 GOSUB 10440                                                       'Input++
3580 IF FKEY=1 THEN GOSUB 18080:GOTO 3460
3590 IF FKEY=2 THEN 2150                                                'F-Keys
3600 IF FKEY=3 THEN 1960
3610 IF FKEY=4 THEN GOSUB 13530:GOTO 3570 'Rückkehr ins Hauptmenü-Frage 'F-keys einblenden
3620 '
3630 M=VAL(OUTTEXT$)
3640 IF M>=1 AND M<=MTGLMAX THEN 3690
3650   N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12890       'Nachricht
3660   BADINPUT=1
3670   INTEXT$=OUTTEXT$                                                    '???
3680   GOTO 3550
3690 MTGLZAHL=M
3700 N$="Stammsatz wird gelesen...":GOSUB 12970   'Nachricht und F-Keys löschen
3710 GOSUB 11860                                                   'Daten lesen
3720 GOSUB 13030                                             'Nachricht löschen
3730 '
3740 GOSUB 13100                                                   'Basis Maske
3750 PRINT FNCUR$(3,31);"Mitgliedsnummer";HIGHON$;MTGLZAHL;HIGHOFF$
3760 GOSUB 11680                                 'Felder und Feldwerte ausgeben
3770 Q$="Mitglied endgültig löschen?":GOSUB 12310                'Ja/Nein-Frage
3780 IF JA=0 THEN 3850
3790   DAT$(5)="ausgetreten"
3800   PRINT FNCUR$(FEROW(5),FECOL(5)+XOFF);HIGHON$;DAT$(5);HIGHOFF$;
3810   PRINT STRING$(FELNG(5)-LEN(DAT$(5)),".")                   'NO semicolon
3820   N$="Stammsatz wird gespeichert...":GOSUB 12970 'Nachricht und F-Keys löschen
3830   GOSUB 12110                                             'Daten speichern
3840   GOSUB 13030                                           'Nachricht löschen
3850 GOTO 3460
3860 '
3870 ' *** Stammsatz suchen (und drucken) von 05*************** ***************
3880 '
3890 DRUCKFLAG=0
3891 IF DRUCKFLAG=0 THEN MT$="STAMMSATZ SUCHEN":MI$="G-01105"
3892 IF DRUCKFLAG=1 THEN MT$="SUCHEN & DRUCKEN":MI$="TODO***"
3900 GOSUB 13100                                                   'Basis Maske
3910 FKEYS$="":GOSUB 13210                           'F-Tastenleiste einblenden
3920 IF DRUCKFLAG THEN TMP$="Drucken" ELSE TMP$="Suchen"
3930 PRINT FNCUR$(4,30)"1 - ";TMP$;" nach Nummern"
3940 PRINT FNCUR$(6,30)"2 - ";TMP$;" nach Maske"
3950 FKEYMAX=4:NKEYMAX=2:GOSUB 13370                              'Menü-Eingabe
3960 IF FKEY=1 THEN GOSUB 18220:GOTO 3891   'TODO IF DRUCKFLAG THEN ... 'F-Keys
3970 IF FKEY=2 THEN IF DRUCKFLAG THEN 6720 ELSE 2150
3980 IF FKEY=3 THEN 1960
3990 IF FKEY=4 THEN GOSUB 13530:GOTO 3950         'Rückkehr ins Hauptmenü-Frage
4000 ON NKEY GOTO 4020,4540 
4010 ' *** Suchen nach Nummern ************************************************
4020 NUMSUCH1=0
4030 NUMSUCH2=0
4040 TMP$=STR$(1):NUMSUCH$(1)=RIGHT$(TMP$,LEN(TMP$)-1)
4050 TMP$=STR$(MTGLMAX):NUMSUCH$(2)=RIGHT$(TMP$,LEN(TMP$)-1)
4060 GOSUB 13100                                                   'Basis Maske
4070 FKEYS$="<F5>=Suchen":IF DRUCKFLAG THEN FKEYS$="<F5>=Suchen & Drucken"
4080 GOSUB 13210                                     'F-Tastenleiste einblenden
4090 TMP$="gesucht":IF DRUCKFLAG THEN TMP$="gesucht und gedruckt"     
4100 PRINT FNCUR$(4,25);"Gültige Mitgliedsnummern: 1 -";STR$(MTGLMAX)
4110 PRINT FNCUR$(6,13);"Ab welcher Mitgliedsnummer soll ";TMP$;" werden?     =>..."
4120 PRINT FNCUR$(8,13);"Bis zu welcher Mitgliedsnummer soll ";TMP$;" werden? =>..."
4130 PRINT FNCUR$(6,67);HIGHON$;NUMSUCH$(1);HIGHOFF$
4140 PRINT FNCUR$(8,67);HIGHON$;NUMSUCH$(2);HIGHOFF$
4150 F=1
4160 BADINPUT=0
4170 Z=NUMSUCHROW(F):S=67:LNG=3:TYP=2:MUSS=1
4180 FKEYMASK$="12345":CKEYMASK$="":INTEXT$=NUMSUCH$(F)
4190 GOSUB 10440                                                       'Input++
4200 IF FKEY=1 THEN GOSUB 18220:GOTO 4060
4210 IF FKEY=2 THEN 3891
4220 IF FKEY=3 THEN 1960
4230 IF FKEY=4 THEN GOSUB 13530:GOTO 4190         'Rückkehr ins Hauptmenü-Frage
4240 IF FKEY<>5 THEN 4290
4250   SEARCH=BYNUMS
4260   NUMSUCH1=VAL(NUMSUCH$(1))
4270   NUMSUCH2=VAL(NUMSUCH$(2))
4280   IF DRUCKFLAG THEN 7220 ELSE 4780                         'Reentry, Suche
4290 M=VAL(OUTTEXT$)                                             'Handle RETURN
4300 IF M>=1 AND M<=MTGLMAX THEN 4340
4310   N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12890       'Nachricht
4320   GOSUB 4470
4330   GOTO 4190
4340 NUMSUCH$(F)=OUTTEXT$
4350 NUMSUCH1=VAL(NUMSUCH$(1))
4360 NUMSUCH2=VAL(NUMSUCH$(2))
4370 IF (NUMSUCH1<=NUMSUCH2) THEN 4410
4380   N$="Ungültiger Bereich":GOSUB 12890                           'Nachricht
4390   GOSUB 4470
4400   GOTO 4190
4410 F=F+1:IF F>NUMSUCHMAX THEN F=1
4420 FKEYMASK$="12345"
4430 FKEYS$="<F5>=Suchen":IF DRUCKFLAG THEN FKEYS$="<F5>=Suchen & Drucken" 'TODO Required?
4440 GOSUB 13470:GOSUB 13210             'F-Tastenleiste löschen und einblenden
4450 GOTO 4170
4460 '
4470 BADINPUT=1
4480 INTEXT$=OUTTEXT$
4490 FKEYMASK$="1234"
4500 FKEYS$=""
4510 GOSUB 13470:GOSUB 13210             'F-Tastenleiste löschen und einblenden
4520 RETURN
4530 ' *** Suchen nach Maske **************************************************
4540 GOSUB 11200                                                 'Clear SUCH$()
4550 GOSUB 13100                                                   'Basis Maske
4560 FKEYS$="<F5>=Suchen":IF DRUCKFLAG THEN FKEYS$="<F5>=Suchen & Drucken" 
4570 GOSUB 13210                                     'F-Tastenleiste einblenden
4580 GOSUB 11440                       'Feldnamen und Werteplatzhalter ausgeben
4590 F=1
4600 Z=FEROW(F):S=FECOL(F)+XOFF:LNG=FELNG(F):TYP=3
4610 FKEYMASK$="12345":CKEYMASK$="12":INTEXT$=""
4620 GOSUB 10430                                                         'Input
4630 IF FKEY=0 THEN 4690
4640   IF FKEY=1 THEN GOSUB 18220:GOTO 4550         'TODO IF DRUCKFLAG THEN ...
4650   IF FKEY=2 THEN 3891
4660   IF FKEY=3 THEN 1960
4670   IF FKEY=4 THEN GOSUB 13530:GOTO 4620       'Rückkehr ins Hauptmenü-Frage
4680   SEARCH=BYMASK                                                      '<F5>
4681   IF DRUCKFLAG THEN 7220 ELSE 4780                         'Reentry, Suche
4690 IF CKEY=0 THEN 4740                                           'Cursor keys
4700   PRINT FNCUR$(Z,S);HIGHON$;SUCH$(F);HIGHOFF$;STRING$(LNG-LEN(SUCH$(F)),".")
4710   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FELDMAX
4720   IF CKEY=CKEYDN THEN F=F+1:IF F>FELDMAX THEN F=1
4730   GOTO 4600
4740 SUCH$(F)=OUTTEXT$                                           'Handle RETURN
4750 F=F+1:IF F>FELDMAX THEN F=1
4760 GOTO 4600
4770 ' *** Suche **************************************************************
4780 IF SEARCH=BYMASK THEN MTGLZAHL=0 ELSE MTGLZAHL=NUMSUCH1      'Initialwerte
4781 '
4800 GOSUB 13100                                                   'Basis Maske
4802 FKEYS$="<F5>=Suchen rückwärts <F6>=Suchen vorwärts <F7>=Adressaufkleber" 
4804 GOSUB 13210                                     'F-Tastenleiste einblenden
4810 PRINT FNCUR$(3,31);"Mitgliedsnummer"
4820 GOSUB 11440                       'Feldnamen und Werteplatzhalter ausgeben
4821 IF SEARCH=BYMASK THEN 4824
4822   GOSUB 11860                                     'Stammsatzdaten einlesen
4823   GOTO 4829
4824 MTGLZAHL=0
4825 GOSUB 7265                 'Suche: liest daten in DAT$() und setzt MTGLZAHL
4826 IF MTGLZAHL<>-1 THEN 4829
4827   N$="Keine weiteren Daten vorhanden. Mit <F2> neue Suche.":GOSUB 12890 'Nachricht
4828   GOTO 5223                                                '->Menü-Eingabe     
4829 GOSUB 11560                                            'Feldwerte ausgeben
4831 PRINT FNCUR$(3,46);HIGHON$;STR$(MTGLZAHL);HIGHOFF$
4832 '
5220 FKEMAX=7:NKEYMAX=0:GOSUB 13370      'Menü-Eingabe 'TODO Nachricht löschen?
5221 ON FKEY GOTO 5223,5337,1960,5339,5342,5421,5460
5222 ' *** <F1> Hilfe
5223 GOSUB 18220
5224 GOTO 5220 
5336 ' *** <F2> Zurück
5337 IF SEARCH=BYNUMS THEN 4020 ELSE 4550            'TODO BYNUMS:Initialwerte?
5338 ' *** <F4> Rückkehr ins Hauptmenü
5339 GOSUB 13530                                  'Rückkehr ins Hauptmenü-Frage 
5340 GOTO 5220
5341 ' *** <F5> Suchen rückwärts
5342 IF SEARCH=BYMASK THEN 5347
5343   MTGLZAHL=MTGLZAHL-1
5344   IF MTGLZAHL<NUMSUCH1 THEN 5352
5345   GOTO 5435
5346 'Suche rückwärts nach Maske       
5347 TMP=MTGLZAHL
5349 GOSUB 7302                                     'prev match IN/OUT:MTGLZAHL
5350 IF MTGLZAHL<>-1 THEN 5435
5351   MTGLZAHL=TMP
5352 N$="Erster Suchtreffer":GOSUB 12890                             'Nachricht       
5353 GOTO 5220
5420 ' *** <F6> Suchen vorwärts
5421 IF SEARCH=BYMASK THEN 5427
5422   MTGLZAHL=MTGLZAHL+1
5423   IF MTGLZAHL>NUMSUCH2 THEN 5432
5425   GOTO 5435
5426 'Suche vorwärts nach Maske    
5427 TMP=MTGLZAHL
5429 GOSUB 7310                                     'next match IN/OUT:MTGLZAHL
5430 IF MTGLZAHL<>-1 THEN 5435
5431   MTGLZAHL=TMP
5432 N$="Letzter Suchtreffer":GOSUB 12890                            'Nachricht       
5433 GOTO 5220
5434 '
5435 GOSUB 13030                            'Nachricht löschen 'TODO notwendig?
5436 GOSUB 11860                                       'Stammsatzdaten einlesen
5437 GOSUB 11560                                            'Feldwerte ausgeben
5438 PRINT FNCUR$(3,46);HIGHON$;STR$(MTGLZAHL);HIGHOFF$
5439 GOTO 5220	
5450 ' *** <F7> Adressaufkleber drucken
5460 GOSUB 15786                          'Ist der Drucker eingeschaltet? Frage
5461 IF JA=0 THEN 5490                              'LW TODO F-Keys einblenden?
5462   N$="Adressaufkleber wird gedruckt...":GOSUB 12890             'Nachricht
5463   GOSUB 16220                                     'Adressaufkleber drucken
5464   GOSUB 13030                                           'Nachricht löschen
5470   GOSUB 13210                                           'F-keys einblenden
5490 GOTO 5220                                                    'Neue Eingabe
5500 '
5510 ' *** Stammsatz drucken von 02************ & 04********** ****************
5520 '
5530 MT$="STAMMSATZ DRUCKEN":MI$="G-01104":GOSUB 13100             'Basis Maske
5540 FKEYS$="":GOSUB 13210                           'F-Tastenleiste einblenden
5550 PRINT FNCUR$(4,24)"Bitte geben Sie die Mitgliedsnummer"
5560 PRINT FNCUR$(5,23)"des auszudruckenden Stammsatzes ein."
5570 PRINT FNCUR$(6,23);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
5580 PRINT FNCUR$(7,24)"Beenden Sie die Eingabe mit RETURN."
5590 PRINT FNCUR$(9,39);"=>..."
5600 BADINPUT=0
5610 Z=9:S=41:LNG=3:TYP=2:MUSS=1
5620 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
5630 GOSUB 10440                                                       'Input++
5640 IF FKEY=1 THEN GOSUB 18470:GOTO 5530                               'F-Keys
5650 IF FKEY=2 THEN 2150
5660 IF FKEY=3 THEN 1960
5670 IF FKEY=4 THEN GOSUB 13530:GOTO 5630         'Rückkehr ins Hauptmenü-Frage
5680 '
5690 M=VAL(OUTTEXT$)
5700   IF M>=1 AND M<=MTGLMAX THEN 5750
5710   N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12890       'Nachricht
5720   BADINPUT=1
5730   INTEXT$=OUTTEXT$                                                    '???
5740   GOTO 3550
5750 MTGLZAHL=M
5760 N$="Stammsatz wird gelesen...":GOSUB 12970   'Nachricht und F-Keys löschen
5770 GOSUB 11860                                                   'Daten lesen
5780 GOSUB 13030                                             'Nachricht löschen
5790 '
5800 GOSUB 13100                                                   'Basis Maske
5810 PRINT FNCUR$(3,31);"Mitgliedsnummer";HIGHON$;MTGLZAHL;HIGHOFF$
5820 GOSUB 11680                                 'Felder und Feldwerte ausgeben
5830 GOSUB 15786                                  'Drucker eingeschaltet? Frage
5840 IF JA=0 THEN 5860
5850 GOSUB 16420                          'Stammsatz drucken (Bildschirmformat)
5860 GOTO 5530
5870 '
5880 ' ######################### Verwendungsnachweis ##########################
5890 '
5900 GOSUB 11260                                              'Einlesen MTGLMAX
5910 VNFLAG=-1
5920 '
5930 ' Verwendungsnachweis-Menü ***********************************************
5940 '
5950 MT$="VERWENDUNGSNACHWEIS":MI$="G-01200":GOSUB 13100           'Basis Maske
5960 F=4                                                                'TODO ?
5970 FKEYS$="":GOSUB 13210                           'F-Tastenleiste einblenden
5980 PRINT CURON$;
5990 PRINT HIGHOFF$;
6000 PRINT FNCUR$(4,20)"   Bitte geben Sie die Mitgliedsnummer"
6010 PRINT FNCUR$(5,20)"des zu ändernden Verwendungsnachweises ein"
6020 PRINT FNCUR$(6,20)" und beenden Sie die Eingabe mit RETURN."
6030 PRINT FNCUR$(8,20)"                 =>..."
6040 Z=8:S=39:LNG=3:TYP=2
6050 GOSUB 10430                                                         'INPUT
6060 IF FKEY=1 THEN GOSUB 18590:GOTO 5950
6070 IF FKEY=2 OR FKEY=3 THEN GOSUB 11150:VNFLAG=0:GOTO 1960      'Clear DAT$()
6080 IF FKEY=4 THEN GOSUB 13530
6090 IF FKEY>4 THEN 6040
6100 IF CKEY<>0 THEN 6040
6110 IF LEN(OUTTEXT$)=0 THEN N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 12890:GOTO 6030 ELSE MITGLIED=VAL(OUTTEXT$):IF MITGLIED=0 THEN 6050 'Nachricht
6120 IF MITGLIED>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12890:PRINT HOME$;HIGHOFF$:GOTO 6030 'Nachricht
6130 MTGLZAHL=MITGLIED:MTGL$=STR$(MITGLIED)
6140 '
6150 ' *** Einlesen des Mitgliedsnamens ***************************************
6160 '
6170 PRINT FNCUR$(21,1);DELLINE$;
6180 GET #2,MITGLIED                                        'EINLESEN DATEN STS
6190 NNAM$=LEFT$(DD1$,25)                                 'NACHNAME RAUSFISCHEN
6200 VNAM$=MID$(DD1$,26,25)                               'VORNAME  RAUSFISCHEN
6210 '
6220 ' ** LEERZEICHEN HINTER NACHNAME ABSCHNEIDEN *****************************
6230 '
6240 FOR I=LEN(NNAM$) TO 1 STEP -1
6250   IF MID$(NNAM$,I,1)<>" " THEN NNAM$=LEFT$(NNAM$,I):I=1
6260 NEXT
6270 NAMM$=NNAM$+" "+VNAM$
6280 GOSUB 14050                                          'EINLESEN 1. VN-BLATT
6290 BLATT=1
6300 VP=1:HP=1                                                 'INITIALISIERUNG
6310 F=8
6320 GOSUB 13100                                                   'Basis Maske
6330 FKEYS$="<F7>=Speichern":GOSUB 13210             'F-Tastenleiste einblenden
6340 PRINT HIGHOFF$;
6350 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=Vorwärts";TAB(53);"<F9>=Blatt drucken";
6360 PRINT FNCUR$(3,2);"Blatt:"
6370 PRINT FNCUR$(5,2);"Mitglieds-Nr.:"
6380 PRINT FNCUR$(5,24);"Name:"
6390 MSK$="Von:  .  .   Bis:  .  .   Amt:"
6400 FOR KK=7 TO 19 
6410   PRINT FNCUR$(KK,2);MSK$;TAB(43);MSK$                    'LW NO semicolon
6420 NEXT
6430 GOSUB 15290                                                 'Ausgabe Daten
6440 LNG=2-(HP=7)*6                                    'BEGIN OF THE INPUT-LOOP
6450 TYP=2-(HP=7)
6460 Z=VP+13*(VP>13)+6
6470 S=6-39*(VP>13)-(HP<7)*(HP-1)*3+(HP>3)*(HP<7)*4-(HP=7)*26
6480 N$="":GOSUB 12890                         'ERASE MESSAGE-LINE   'Nachricht
6490 GOSUB 10430                                                       'INPUT+1
6500 IF HP<7 THEN IF ASCC=ASCRET THEN OUTTEXT$=STRING$(2-LEN(OUTTEXT$),"0")+OUTTEXT$   '+NULLEN
6510 IF CKEY<>0 OR ASCC=ASCRET THEN PRINT FNCUR$(Z,S);OUTTEXT$
6520 IF ASCC=ASCRET THEN IF HP<7 THEN MID$(DAT$(VP),-1+2*HP,2)=OUTTEXT$ ELSE DAT$(VP)=LEFT$(DAT$(VP),12)+OUTTEXT$+SPACE$(8-LEN(OUTTEXT$))
6530 IF ASCC=ASCRET AND HP<7 THEN JA=1:TEST$=OUTTEXT$:ON HP GOSUB 12650,12680,12710,12650,12680,12710:IF JA=0 THEN N$="Ungültiges Datum":GOSUB 12890:GOTO 6490 'Datums-Check 'Nachricht
6540 IF ASCC=ASCRET THEN IF HP=7 THEN HP=1:VP=VP-(VP<26)-(VP=26)*(-25):GOTO 6440 ELSE HP=HP+1:GOTO 6440     'NEUE SCREEN-POSITION
6550 IF CKEY<>0 THEN TEST$=MID$(DAT$(VP),-(HP<7)*2*HP-1-(HP=7)*14,-(HP<7)*2-(HP=7)*8):PRINT FNCUR$(Z,S);TEST$
6560 IF CKEY<>0 THEN IF HP<7 AND NOT TEST$="  " THEN JA=1:ON HP GOSUB 12650,12680,12710,12650,12680,12710:IF JA=0 THEN N$="Ungültiges Datum":GOSUB 12890:GOTO 6490 'Nachricht
6570 IF CKEY=CKEYDN THEN VP=VP-(VP<26)-(VP=26)*(-25):GOTO 6440
6580 IF CKEY=CKEYUP THEN VP=VP+(VP>1)-(VP=1)*25     :GOTO 6440
6590 IF CKEY=CKEYRT THEN HP=HP-(HP<7)-(HP=7)*(-6)   :GOTO 6440
6600 IF CKEY=CKEYLT THEN HP=HP+(HP>1)+(HP=1)*(-6)   :GOTO 6440
6610 IF FKEY=1 THEN GOSUB 18700 :GOTO 6300           'HILFE - ZU INIT UND MASKE
6620 IF FKEY=2 THEN 5950                                       'ZU MTGL-EINGABE
6630 IF FKEY=3 THEN VNFLAG=0:GOSUB 11150:GOTO 1960                'Clear DAT$()
6640 IF FKEY=4 THEN GOSUB 13530:GOSUB 13270:GOTO 6490 
6650 IF FKEY=5 THEN GOSUB 14140:GOTO 6440
6660 IF FKEY=6 THEN GOSUB 14300:GOTO 6440
6670 IF FKEY=7 THEN GOSUB 14950:GOTO 6490                 'SPEICHERN DANN INPUT
6680 IF FKEY=9 THEN GOSUB 15530:GOSUB 14500:GOTO 6480
6690 '
6700 ' ############# Mitgliedsliste drucken von 05*************** #############
6710 '
6720 MT$="MITGLIEDSLISTE DRUCKEN":MI$="G-01300":GOSUB 13100        'Basis Maske
6840 FKEYS$="":GOSUB 13210                           'F-Tastenleiste einblenden
6850 PRINT FNCUR$(4,28);"1 - Suchen & Drucken"
6860 PRINT FNCUR$(6,28);"2 - Komplett drucken"
6870 PRINT FNCUR$(8,28);"3 - Datenblatt drucken"
6880 FKEYMAX=4:NKEYMAX=3:GOSUB 13370                              'Menü-Eingabe
6890 IF FKEY=1 THEN GOSUB 18890:GOTO 6720
6900 IF FKEY=2 THEN 1960
6901 IF FKEY=3 THEN 1960
6910 IF FKEY=4 THEN GOSUB 13530:GOTO 6880
6920 ON NKEY GOTO 6980,6980,7410
6950 '
6960 ' *** Suchen & Drucken, Komplett drucken *********************************
6970 '
6980 IF NKEY=1 THEN MT$="SUCHEN & DRUCKEN":MI$="TODOAAAA"
6981 IF NKEY=2 THEN MT$="KOMPLETT DRUCKEN":MI$="TODOBBBB"
6982 GOSUB 13100                                                   'Basis Maske
6990 GOSUB 13210                                     'F-Tastenleiste einblenden
7000 PRINT FNCUR$(4,21);"Bitte geben Sie das heutige Datum ein."
7001 PRINT FNCUR$(5,22);"Beenden Sie die Eingabe mit RETURN."
7002 PRINT FNCUR$(7,35);"=>........"
7010 BADINPUT=0
7011 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
7012 Z=7:S=37:LNG=8:TYP=5:MUSS=1
7013 GOSUB 10440                                                       'Input++
7020 IF FKEY=1 THEN GOSUB 18890:GOTO 6980             'TODO Hilfe-Text anpassen
7021 IF FKEY=2 THEN 6720                                                'F-Keys
7022 IF FKEY=3 THEN 1960
7023 IF FKEY=4 THEN GOSUB 13530:GOTO 7013         'Rückkehr ins Hauptmenü-Frage
7030 '
7031 POS1=INSTR(OUTTEXT$,".")
7032 POS2=INSTR(POS1+1,OUTTEXT$,".")
7033 IF POS2=0 THEN 7200
7034 POS3=INSTR(POS2+1,OUTTEXT$,".")
7035 IF POS3<>0 THEN 7200
7036 D1$=LEFT$(OUTTEXT$,POS1-1)
7037 IF LEN(D1$)=0 OR LEN(D1$)>2 THEN 7200
7038 D2$=MID$(OUTTEXT$,POS1+1,POS2-POS1-1)
7039 IF LEN(D2$)=0 OR LEN(D2$)>2 THEN 7200
7040 D3$=RIGHT$(OUTTEXT$,LEN(OUTTEXT$)-POS2)
7041 IF LEN(D3$)=0 OR LEN(D3$)>2 THEN 7200
7042 '
7050 D1=VAL(D1$):IF D1<1 OR D1>31 THEN 7202
7051 D2=VAL(D2$):IF D2<1 OR D2>12 THEN 7202
7052 D3=VAL(D3$) 
7053 ' TODO: Deeper validation
7054 D1$=STR$(D1):D1$=MID$(D1$,2,99):IF D1<10 THEN D1$="0"+D1$
7055 D2$=STR$(D2):D2$=MID$(D2$,2,99):IF D2<10 THEN D2$="0"+D2$
7056 D3$=STR$(D3):D3$=MID$(D3$,2,99):IF D3<10 THEN D3$="0"+D3$
7057 DATUM$=D1$+"."+D2$+"."+D3$
7058 '
7070 PRINT FNCUR$(7,37);HIGHON$;DATUM$;HIGHOFF$
7071 GOTO 7205
7072 '
7200 N$="Datum muss Format TT.MM.JJ haben. Bitte korrigieren.":GOSUB 12890 'Nachricht
7201 GOTO 7203
7202 N$="Ungültiges Datum. Bitte korrigieren.":GOSUB 12890           'Nachricht
7203 BADINPUT=1
7204 INTEXT$=OUTTEXT$                                                      '???
7205 GOTO 7012
7206 '
7207 IF NKEY=1 THEN 7220             'Suchen und Drucken nach Nummern und Maske
7208   SEARCH=BYNUMS                          'Drucken komplette Mitgliedsliste
7209   NUMSUCH1=1
7210   NUMSUCH2=MTGLMAX
7211   GOSUB 15786                              '"Drucker eingeschaltet?"-Frage
7212   IF JA=0 THEN 6980
7213   GOTO 7222
7214 DRUCKFLAG=1
7215 GOTO 3891                                  'Stammsatz suchen (und drucken)
7216 ' ---> Reentry <----------------------------------------------------------
7217 ' IN: SEARCH (BYNUMS, BYMASK)
7218 ' IN: NUMSUCH1, NUMSUCH2
7219 ' IN: SUCH$()
7220 GOSUB 15786                                '"Drucker eingeschaltet?"-Frage
7221 IF JA=0 THEN 7252
7222 N$="Mitgliedsliste wird gedruckt... Abbrechen mit Taste.":GOSUB 12890 'Nachricht
7223 GOSUB 15830                                 'Mitgliedsliste drucken (Kopf)
7224 IF SEARCH=BYMASK THEN 7240
7225 FOR MTGLZAHL=NUMSUCH1 TO NUMSUCH2
7226   GOSUB 11860                                     'Stammsatzdaten einlesen
7227   IF INSTR(DAT$(5),"ausgetreten")>0 THEN 7229
7228     GOSUB 15980                         'Mitgliedsliste drucken (Mitglied)
7229   GOSUB 16185                                      'Druck abbrechen? Frage
7230   IF JA=1 THEN 7251
7231 NEXT MTGLZAHL
7232 GOTO 7251
7233 ' 
7240 MTGLZAHL=0
7241 GOSUB 7265                                                 'Search by mask
7242 IF MTGLZAHL=-1 THEN 7251
7244   GOSUB 11860            'Stammsatzdaten einlesen, IN:MTGLZAHL, OUT DAT$()
7245   IF INSTR(DAT$(5),"ausgetreten")>0 THEN 7247
7246     GOSUB 15980                         'Mitgliedsliste drucken (Mitglied)
7247   GOSUB 16185                                      'Druck abbrechen? Frage
7248   IF JA=1 THEN 7251
7249 GOTO 7241
7250 '
7251 GOSUB 13030                                             'Nachricht löschen
7252 IF SEARCH=BYNUMS THEN 4020 ELSE 4540
7260 '
7261 ' *** Suche nach Maske ***
7262 '
7263 ' IN: SUCH$()
7264 ' IN: MTGLZAHL (0=initial) OUT:MTGLZAHL (-1=not found)
7265 IF MTGLZAHL>MTGLMAX THEN 7285
7266 MTGLZAHL=MTGLZAHL+1 
7267 '
7270 GOSUB 11860                                                   'Daten lesen
7271 FOR K=1 TO FELDMAX                                      'Daten durchsuchen
7272   IF LEFT$(SUCH$(K),1)<>"*" THEN 7276
7273   IF INSTR(DAT$(K),RIGHT$(SUCH$(K),LEN(SUCH$(K))-1))>0 THEN 7282
7274     MTGLZAHL=MTGLZAHL+1
7275     GOTO 7284
7276   I=1
7277   IF MID$(SUCH$(K),I,1)="?" THEN I=I+1:GOTO 7277
7278   I=I-1
7279   IF INSTR(DAT$(K),RIGHT$(SUCH$(K),LEN(SUCH$(K))-I))=I+1 THEN 7282
7280     MTGLZAHL=MTGLZAHL+1
7281     GOTO 7284
7282 NEXT K
7283 GOTO 7286                                     'Match found, index MTGLZAHL 
7284 IF MTGLZAHL<=MTGLMAX THEN 7270
7285 MTGLZAHL=-1                                         'No more matches found
7286 RETURN
7299 '
7300 ' *** Prev match 
7301 ' IN: MTGLZAHL OUT: MTGLZAHL or -1 if not found
7302 MTGLZAHL=MTGLZAHL-1
7303 IF MTGLZAHL<1 THEN 7316
7304 GOSUB 7320
7305 IF NOT ISMATCH THEN 7302 
7306 RETURN
7307 '
7308 ' *** Next match 
7309 ' IN: MTGLZAHL OUT: MTGLZAHL or -1 if not found
7310 MTGLZAHL=MTGLZAHL+1
7311 IF MTGLZAHL>MTGLMAX THEN 7316
7312 GOSUB 7320
7313 IF NOT ISMATCH THEN 7310 
7314 RETURN
7315 '
7316 MTGLZAHL=-1
7317 RETURN
7318 '
7320 GOSUB 11860                                                   'Daten lesen
7321 FOR K=1 TO FELDMAX                                      'Daten durchsuchen
7322   IF LEFT$(SUCH$(K),1)<>"*" THEN 7325
7323     IF INSTR(DAT$(K),RIGHT$(SUCH$(K),LEN(SUCH$(K))-1))>0 THEN 7330
7324     GOTO 7334
7325   I=1
7326   IF MID$(SUCH$(K),I,1)="?" THEN I=I+1:GOTO 7326
7327   I=I-1
7328   IF INSTR(DAT$(K),RIGHT$(SUCH$(K),LEN(SUCH$(K))-I))=I+1 THEN 7330
7329     GOTO 7334
7330 NEXT K
7332 ISMATCH=1
7333 RETURN
7334 ISMATCH=0
7335 RETURN
7380 '
7390 ' *** Datenblatt drucken *************************************************
7400 '
7410 MT$="DATENBLATT DRUCKEN":MI$="G-01303":GOSUB 13100            'Basis Maske
7420 FKEYS$="":GOSUB 13210                           'F-Tastenleiste einblenden
7430 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer"
7440 PRINT FNCUR$(5,23)"des zu druckenden Datenblattes ein."
7450 PRINT FNCUR$(7,23)"Beenden Sie die Eingabe mit RETURN."
7460 PRINT FNCUR$(9,39)"=>..."
7461 BADINPUT=0
7470 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
7471 Z=9:S=41:LNG=3:TYP=2:MUSS=1
7480 GOSUB 10440                                                       'Input++
7490 IF FKEY=1 THEN GOSUB 18890:GOTO 7410
7500 IF FKEY=2 THEN 6720                                                'F-Keys
7510 IF FKEY=3 THEN 1960
7519 IF FKEY=4 THEN GOSUB 13530:GOTO 7480         'Rückkehr ins Hauptmenü-Frage
7520 '
7521 M=VAL(OUTTEXT$)
7522 IF M>=1 AND M<=MTGLMAX THEN 7550
7523   N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12890       'Nachricht
7524   BADINPUT=1
7525   INTEXT$=OUTTEXT$                                                    '???
7526   GOTO 7471
7550 MTGLZAHL=M
7551 GOSUB 15786                                '"Drucker eingeschaltet?"-Frage
7580 IF JA=1 THEN 7584
7581   GOSUB 13210                                   'F-Tastenleiste einblenden
7582   INTEXT$=OUTTEXT$
7583   GOTO 7471
7584 N$="Datenblatt drucken...":GOSUB 12970    'Nachricht und F-keys ausblenden
7590 GOSUB 11860                                          'Stammsatzdaten lesen
7610 NAMM$ = DAT$(1)+" "+DAT$(2)
7620 GOSUB 16420                          'Stammsatz drucken (Bildschirmformat)
7630 GOSUB 14050                             'Verwendungsnachweisdaten einlesen
7640 GOSUB 15550                              'Verwendungsnachweisdaten drucken
7650 GOSUB 16680                                     'Mitgliedsvermerke drucken
7651 GOSUB 13030                                             'Nachricht löschen
7660 GOTO 7410
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
10300 '
10310 ' *** Eingaberoutine (INPUT) ********************************************
10320 '
10330 ' IN: Z=Zeile     OUT:          TMP: P
10340 ' IN: S=Spalte    OUT: OUTTEXT$ TMP: VALID (1=true 0=false)
10350 ' IN: LNG= Länge  OUT: CKEY     TMP: BADINPUT
10360 ' IN: TYP=Typ     OUT: FKEY
10370 ' IN: VNFLAG      OUT: ASCC
10380 ' IN: INTEXT$
10390 ' IN: FKEYMASK$
10400 ' IN: MUSS
10410 ' IN: CKEYMASK$
10420 '
10430 BADINPUT=0
10440 P=LEN(INTEXT$)
10450 PRINT FNCUR$(Z,S);HIGHON$;INTEXT$;HIGHOFF$;STRING$(LNG-P,".")
10460 OUTTEXT$=INTEXT$
10470 PRINT CURON$;
10480 CKEY=0:FKEY=0
10490 PRINT FNCUR$(Z,S+P);
10500 A$=INPUT$(1):ASCC=ASC(A$)
10510 IF BADINPUT THEN N$="":GOSUB 12890:PRINT FNCUR$(Z,S+P);:BADINPUT=0
10520 ON TYP GOSUB 12400,12470,12530,12590,12614                    'Type check
10530 IF VALID=0 THEN 10590
10540   IF P>=LNG THEN 10500                                   'Valid character
10550   PRINT HIGHON$;A$;HIGHOFF$                              'LW NO semicolon
10560   OUTTEXT$=OUTTEXT$+A$
10570   P=P+1
10580   GOTO 10490
10590 IF ASCC<>ASCBSP THEN 10650
10600   IF P<1 THEN 10500                                            'Backspace
10610   PRINT DEL$                                             'LW NO semicolon
10620   P=P-1
10630   OUTTEXT$=LEFT$(OUTTEXT$,P)
10640   GOTO 10490
10650 IF ASCC=ASCESC THEN 10430                                         'Escape
10660 IF ASCC=ASCRET THEN 10790
10670 IF ASCC=ASCCUP THEN CKEY=CKEYUP:IF INSTR(CKEYMASK$,CHR$(CKEY+ASC0))>0 THEN 10790
10680 IF ASCC=ASCCDN THEN CKEY=CKEYDN:IF INSTR(CKEYMASK$,CHR$(CKEY+ASC0))>0 THEN 10790
10690 IF VNFLAG=0 THEN 10720
10700   IF ASCC=ASCCLT THEN CKEY=CKEYLT:IF INSTR(CKEYMASK$,STR$(CKEY+ASC0))>0 THEN 10790 
10710   IF ASCC=ASCCRT THEN CKEY=CKEYRT:IF INSTR(CKEYMASK$,CHR$(CKEY+ASC0))>0 THEN 10790 
10720 IF ASCC<ASCF1 OR ASCC>ASCF9 THEN 10750                            'F-Keys
10730   FKEY=ASCC-ASCF1+1
10740   IF INSTR(FKEYMASK$,CHR$(FKEY+ASC1-1))>0 THEN 10840
10750 N$="Ungültige Eingabe - Bitte wiederholen":GOSUB 12890         'Nachricht
10760 BADINPUT=1
10770 GOTO 10490
10780 '
10790 IF MUSS=0 OR LEN(OUTTEXT$)>0 THEN 10830
10800   N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 12890
10810   BADINPUT=1 
10820   GOTO 10490
10830 IF CKEY<>0 THEN GOSUB 10890                     'Block cursor auto-repeat
10840 PRINT CUROFF$                                            'LW NO semicolon
10850 RETURN 
10860 '
10870 ' *** Block cursor auto-repeat ********************************* REVIEWED
10880 '
10890 A$=INKEY$:IF A$>=CHR$(ASCCUP) AND A$<=CHR$(ASCCRT) THEN 10890
10900 RETURN
10910 '
10920 ' *** Debounce keyboard **************************************** REVIEWED
10930 '
10940 FOR K=1 TO 30:A$=INKEY$:NEXT K
10950 RETURN
10960 '
10970 ' *** Einlesen Stammdatenfelder-Definition ********************* REVIEWED
10980 '
10990 OPEN "I",#1,"B:FELD.DAT"                        
11000   INPUT #1,TMPFELDMAX                                             'ignore
11010   FOR I=1 TO FELDMAX
11020     INPUT #1,F$
11030     FENAM$(I)=LEFT$(F$,14)
11040     FEMUSS(I)=-(MID$(F$,13,1)="=")                      'TODO Verify this
11050     FECOL(I)=VAL(MID$(F$,15,2))
11060     FEROW(I)=VAL(MID$(F$,17,2))
11070     FELNG(I)=VAL(MID$(F$,19,2))
11080     FETYP(I)=VAL(RIGHT$(F$,1)) 
11090   NEXT
11100 CLOSE #1
11110 RETURN
11120 '
11130 ' *** Clear DAT$() ********************************************* REVIEWED
11140 '
11150 FOR K=1 TO FELDMAX:DAT$(K)="":NEXT K
11160 RETURN
11170 '
11180 ' *** Clear SUCH$()
11190 '
11200 FOR K=1 TO FELDMAX:SUCH$(K)="":NEXT K
11210 RETURN
11220 '
11230 ' *** Lesen MTGLMAX ******************************************** REVIEWED
11240 '
11250 ' OUT: MTGLMAX
11260 OPEN "I",#1,"B:MTGLZAHL.DAT"
11270   INPUT #1,MTGLMAX$
11280   MTGLMAX=VAL(MTGLMAX$)
11290 CLOSE #1
11300 RETURN
11310 '
11320 ' *** Schreiben MTGLMAX **************************************** REVIEWED
11330 '
11340 ' IN: MTGLMAX
11350 OPEN "O",#1,"B:MTGLZAHL.DAT"
11360   PRINT #1,STR$(MTGLMAX)
11370 CLOSE #1
11380 RETURN
11390 '
11400 ' *** Maske Feldnamen und Feldplatzhalter ausgeben
11410 '
11420 ' IN: FELDMAX
11430 ' IN: MTGLZAHL
11440 FOR K=1 TO FELDMAX
11450   Z=FEROW(K)
11460   S=FECOL(K)
11470   PRINT FNCUR$(Z,S);FENAM$(K)
11480   S=S+XOFF
11490   PRINT FNCUR$(Z,S);STRING$(FELNG(K),".")
11500 NEXT K
11510 RETURN
11520 '
11530 ' *** Maske Feldwerte ausgeben
11540 '
11550 ' IN: FELDMAX
11560 PRINT HIGHON$
11570 FOR K=1 TO FELDMAX
11580   Z=FEROW(K)
11590   S=FECOL(K)+XOFF
11600   PRINT FNCUR$(Z,S);DAT$(K)
11610 NEXT K
11620 PRINT HIGHOFF$
11630 RETURN
11640 '
11650 ' *** Maske Feldnamen, Eingabefelder und Filler ausgeben
11660 '
11670 ' IN: FELDMAX
11680 FOR K=1 TO FELDMAX
11690   Z=FEROW(K)
11700   S=FECOL(K)
11710   PRINT FNCUR$(Z,S);FENAM$(K)
11720   S=S+XOFF
11730   PRINT FNCUR$(Z,S);HIGHON$;DAT$(K);HIGHOFF$;
11740   PRINT STRING$(FELNG(K)-LEN(DAT$(K)),".")               'LW NO semicolon
11750 NEXT K
11760 RETURN
11770 '
11780 ' *** FIELDen der Stammsatz-Datei ****************************** REVIEWED
11790 '
11800 OPEN "R",#2,"B:MV.DAT",462
11810 FIELD #2,236 AS DD1$,225 AS DD2$
11820 RETURN
11830 '
11840 ' *** Einlesen der Felddaten von Disk ***********************************
11850 '
11860 GET #2,MTGLZAHL
11870 GOSUB 11150                                                 'Clear DAT$()
11880 I=1
11890 FOR K=1 TO 14
11900   LNG=FELNG(K)
11910   TMP$=MID$(DD1$,I,LNG):GOSUB 12020:DAT$(K)=TMP$
11920   I=I+LNG
11930 NEXT
11940 I=1
11950 FOR K=15 TO FELDMAX
11960   LNG=FELNG(K)
11970   TMP$=MID$(DD2$,I,LNG):GOSUB 12020:DAT$(K)=TMP$
11980   I=I+LNG
11990 NEXT
12000 RETURN
12010 '
12020 FOR KK=LEN(TMP$) TO 1 STEP -1
12030   IF MID$(TMP$,KK,1)<>" " THEN 12050
12040 NEXT KK
12050 TMP$=LEFT$(TMP$,KK)
12060 RETURN
12070 '
12080 ' *** Speichern der Felddaten auf Disk **********************************
12090 '
12100 ' IN: MTGLZAHL
12110 TMP$=""
12120 FOR K=1 TO 14
12130   LNG=FELNG(K)
12140   TMP$=TMP$+DAT$(K)+SPACE$(LNG-LEN(DAT$(K)))
12150 NEXT K
12160 LSET DD1$=TMP$
12170 TMP$=""
12180 FOR K=15 TO FELDMAX
12190   LNG=FELNG(K)
12200   TMP$=TMP$+DAT$(K)+SPACE$(LNG-LEN(DAT$(K)))
12210 NEXT K
12220 LSET DD2$=TMP$
12230 TMP$=""
12240 PUT #2,MTGLZAHL
12250 RETURN
12260 '
12270 ' *** Ja/Nein-Frage und F-Tastenleiste löschen ***************** REVIEWED
12280 '
12290 ' IN:  Q$ - Frage
12300 ' OUT: JA - (1=yes, 0=no)
12310 N$=Q$+" (J=Ja/N=Nein)":GOSUB 12890                             'Nachricht
12320 GOSUB 13470                                                'Delete F-keys
12330 ANSWER$=INPUT$(1)
12340 IF ANSWER$="J" OR ANSWER$="j" THEN JA=1:RETURN
12350 IF ANSWER$="N" OR ANSWER$="n" THEN JA=0:RETURN
12360 GOTO 12330
12370 '
12380 ' *** Testbereich 1 (Alpha) *********************************************
12390 '
12400 VALID=0
12410 IF ASCC>57 AND ASCC<127 THEN VALID=1:GOTO 12430
12420 IF ASCC>31 AND ASCC<48 THEN VALID=1
12430 RETURN
12440 '
12450 ' *** Testbereich 2 (Numerisch) *****************************************
12460 '
12470 VALID=0
12480 IF ASCC>47 AND ASCC<58 THEN VALID=1
12490 RETURN
12500 '
12510 ' *** Testbereich 3 (Alphanumerisch) ************************************
12520 '
12530 VALID=0
12540 IF ASCC>31 AND ASCC<127 THEN VALID=1
12550 RETURN
12560 '
12570 ' *** Testbereich 4 (Numerisch + "/" und ".") ***************************
12580 '
12590 VALID=0
12600 IF ASCC>45 AND ASCC<58 THEN VALID=1
12610 RETURN
12611 '
12612 ' *** Testbereich 5 (Numerisch + und ".") *******************************
12613 '
12614 VALID=0
12615 IF (ASCC>=48 AND ASCC<=57) OR ASCC=46 THEN VALID=1
12616 RETURN
12620 '
12630 ' *** DATUMS-TEST (bei VN und M-Liste drucken) **************************
12640 '
12650 IF VAL(TEST$)=0 OR VAL(TEST$)>31 THEN JA=0                           'Tag
12660 RETURN
12670 '
12680 IF VAL(TEST$)=0 OR VAL(TEST$)>12 THEN JA=0                         'Monat
12690 RETURN
12700 '
12710 IF VAL(TEST$)=0 THEN JA=0                                           'Jahr
12720 RETURN
12730 '
12740 ' *** Voreinstellungen im ERFASSEN (Defaults) ******************* REVIWED
12750 '
12760 DAT$(5)="10***"
12770 DAT$(7)=""11**""
12780 DAT$(11)="12**"
12790 DAT$(12)="13******"
12800 DAT$(21)="3"
12810 DAT$(22)="Nein"
12820 DAT$(23)="Nein"
12830 DAT$(25)="Nein"
12840 RETURN
12850 '
12860 ' *** Nachricht anzeigen *************************************** REVIEWED
12870 '
12880 ' IN: N$ - Nachricht
12890 PRINT FNCUR$(21,1);DELLINE$;
12900 IF N$<>"" THEN PRINT INVON$;" ";N$;" ";INVOFF$;
12910 PRINT                              'LW workaround quirk in display driver
12920 RETURN
12930 '
12940 ' *** Nachricht anzeigen und F-Tastenleiste ausblenden ********* REVIEWED
12950 '
12960 ' IN: N$ - Nachricht
12970 GOSUB 12890                                                    'Nachricht
12980 GOSUB 13470                                                'Delete F-Keys
12990 RETURN
13000 '
13010 ' *** Nachricht löschen **************************************** REVIEWED
13020 '
13030 N$="":GOSUB 12890                                      'Nachricht löschen
13040 RETURN
13050 '
13060 ' *** Bildschirmmaske - Basis ********************************** REVIEWED
13070 '
13080 ' IN: MT$ - Menu title
13090 ' IN: MI$ - Menu ID
13100 PRINT CLS$;
13110 PRINT "00**********************";
13120 PRINT FNCUR$(1,(80-LEN(MT$))/2+1);MT$;FNCUR$(1,73);MI$
13130 PRINT STRICH$
13140 PRINT FNCUR$(21,1);
13150 PRINT STRICH$             'quirk in display driver places this in line 22
13160 RETURN
13170 '
13180 ' *** F-Tastenleiste einblenden ******************************** REVIEWED
13190 '
13200 ' IN: FKEYS$ - Additional function key labels 
13210 PRINT FNCUR$(23,1);"<F1>=Hilfe <F2>=Zurück <F3>=Anfang <F4>=Hauptmenü";
13220 IF FKEYS$<>"" THEN PRINT " ";FKEYS$ ELSE PRINT           'LW NO semicolon
13230 RETURN
13240 '
13250 ' *** restliche funktionstasten bei Verw.-nachweis ausgeben
13260 '
13270 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=Vorwärts             <F8>=Drucken";
13280 PRINT HOME$
13290 RETURN
13300 '
13310 ' *** Eingabe bei Menübildern ********************************** REVIEWED
13320 '
13330 ' IN:  NKEYMAX - max valid numeric key (1,2,3,...)
13340 ' IN:  FKEYMAX - max valid function key (1,2,3,...)
13350 ' OUT: FKEY - number of pressed function key or 0
13360 ' OUT: NKEY - number of pressed numeric key or 0
13370 NKEY=0:FKEY=0
13380 GOSUB 10940                                            'Debounce keyboard
13390 A$=INPUT$(1):ASCC=ASC(A$)
13400 IF ASCC>=ASC1 AND ASCC<=ASC1+NKEYMAX-1 THEN NKEY=ASCC-ASC1+1:RETURN
13410 IF ASCC>=ASCF1 AND ASCC<=ASCF1+FKEYMAX-1 THEN FKEY=ASCC-ASCF1+1:RETURN
13420 N$="Sie haben eine falsche Taste gedrückt - Bitte neue Auswahl treffen.":GOSUB 12890 'Nachricht
13430 GOTO 13390
13440 '
13450 ' *** F-Tastenleiste ausblenden ******************************** REVIEWED
13460 '
13470 PRINT FNCUR$(23,1);DELLINE$
13480 PRINT FNCUR$(24,1);DELLINE$;
13490 RETURN
13500 '
13510 ' *** Rückkehr ins Hauptmenü? (Original) *********************** REVIEWED
13520 '
13530 GOSUB 13600                                 'Rückkehr ins Hauptmenü Frage
13540 IF JA=1 THEN 19250                                          'Programmende
13550 GOSUB 13210                                    'F-Tastenleiste einblenden
13560 RETURN
13570 '
13580 ' *** Rückkehr ins Hauptmenü Frage (einfach) ******************* REVIEWED
13590 '
13600 ' OUT: JA (1=true, 0=false)
13610 Q$="Wollen Sie wirklich ins Hauptmenü?":GOSUB 12310        'Ja/Nein-Frage
13620 N$="":GOSUB 12890                                      'Nachricht löschen
13630 RETURN
13640 '
13650 ' *** Rückkehr aus Hilfe-Text? ********************************* REVIEWED
13660 '
13670 N$="Zurück? (J=Ja)":GOSUB 12890                                'Nachricht
13680 GOSUB 12320                                           'skip string concat
13690 IF JA=0 THEN 13670
13700 RETURN
13710 '
13720 ' **** VN - Subroutines, permitting one page handling *******************
13730 ' ****         by 04********** 21*******              *******************
13740 '
13750 ' **** OPEN & FIELDING VERWENDUNGSNACHWEIS ******************************
13760 '
13770 OPEN "R",#3,"B:VN.NDX",4
13780 FIELD #3,4 AS POINT0$
13790 OPEN "R",#4,"B:VN.POI",8
13800 FIELD #4,4 AS POINT1$, 4 AS POINT2$
13810 OPEN "R",#5,"B:VN.DAT",560
13820 FIELD #5,240 AS DDD1$,240 AS DDD2$,80 AS DDD3$
13830 RETURN
13840 '
13850 ' *** VERWENDUNGSNACHWEIS FÜR NEUES MITGLIED ANLEGEN ********************
13860 '
13870 ' IN: MTGLZAHL
13880 GET #3,1                            'letzte Seitennummer einlesen dann +1
13890 TMPOINT=VAL(POINT0$)+1
13900 LSET DDD1$=""                                    'leeren Record speichern
13910 LSET DDD2$=""
13920 LSET DDD3$=""
13930 PUT #5,TMPOINT
13940 LSET POINT0$=STR$(TMPOINT)             'letzte Seitennummer neu speichern
13950 PUT #3,1
13960 PUT #3,MTGLZAHL+1
13970 LSET POINT1$="0000"
13980 LSET POINT2$="0000"
13990 PUT #4,TMPOINT
14000 RETURN
14010 '
14020 ' **** ERSTE SEITE FINDEN & LESEN ***************************************
14030 '
14040 ' IN: MTGLZAHL
14050 GET #3,MTGLZAHL+1                                            'Index_Datei
14060 CPOINT=VAL(POINT0$)
14070 GET #5,CPOINT:GOSUB 15130                                       'Einlesen
14080 BLATT=1
14090 GET #4,CPOINT                                              'Pointer lesen
14100 RETURN
14110 '
14120 ' **** SEITE RÜCKWÄRTS **************************************************
14130 '
14140 TMPOINT=VAL(POINT1$)
14150 IF TMPOINT<>0 THEN 14190
14160 N$="Erste Seite":GOSUB 12890                                   'Nachricht
14180 RETURN
14190 CPOINT=TMPOINT
14200 BLATT=BLATT-1
14210 GET #5,CPOINT:GOSUB 15130   
14220 GET #4,CPOINT:GOSUB 15290                             'Neue Pointer lesen
14230 HP=1:VP=1
14240 GOSUB 14500
14260 RETURN
14270 '
14280 ' **** SEITE VORWÄRTS  **************************************************
14290 '
14300 TMPOINT=VAL(POINT2$)
14310 IF TMPOINT=0 THEN 14380
14320 CPOINT=TMPOINT
14330 BLATT=BLATT+1
14340 GET #5,CPOINT:GOSUB 15130
14350 GET #4,CPOINT:GOSUB 15290
14360 HP=1:VP=1
14370 RETURN
14380 N$="Letzte Seite"
14390 IF INSTR(SPACE$(20),DAT$(26))=0 THEN 14430 
14400 GOSUB 12890                                                    'Nachricht
14420 RETURN
14430 Q$="Letzte Seite - Neue Seite anlegen?":GOSUB 12310        'Ja/Nein-Frage
14440 N$="":GOSUB 12890                                              'Nachricht
14450 IF JA=0 THEN 14500
14460 GOSUB 14740
14470 BLATT=BLATT+1
14480 GOSUB 15290
14490 HP=1:VP=1
14500 GOSUB 13210                                    'F-Tastenleiste einblenden
14510 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=vorwärts";
14520 PRINT TAB(54);"<F9>=Blatt drucken";
14530 PRINT HOME$;" "
14540 RETURN
14550 '
14560 ' **** ERSTE SEITE EINES MITGLIEDS ANLEGEN ******************************
14570 '
14580 GET #3,1
14590 LSET POINT0$=STR$(VAL(POINT0$)+1)                                  'NDX+1
14600 PUT #3,1
14610 LSET POINT1$="0000"                                        'Page pointers
14620 LSET POINT2$="0000"
14630 PUT #4,VAL(POINT0$)
14640 LSET POINT0$=POINT0$                                          'First page
14650 PUT #3,MTGLZAHL+1
14660 LSET DDD1$=""                                                 'Datenblatt
14670 LSET DDD2$=""
14680 LSET DDD3$=""
14690 PUT #5,VAL(POINT0$)
14700 RETURN
14710 '
14720 ' **** ALLGEMEIN EINE NEUE SEITE ANLEGEN ********************************
14730 '
14740 GET #3,1                                                           'NDX+1
14750 LSET POINT0$=STR$(VAL(POINT0$)+1)
14760 PUT #3,1:TMPOINT$=POINT0$    'Vektoren der bisher letzten Seite verbiegen
14770 LSET POINT2$=POINT0$                                'Verbiege Vektoren...
14780 PUT #4,CPOINT                                         '...der alten Seite
14790 LSET POINT1$=STR$(CPOINT)                             '...der neuen Seite
14800 LSET POINT2$="0000"                                                '.. ""
14810 CPOINT=VAL(TMPOINT$)                                       'Nr neue Seite
14820 PUT #4,CPOINT
14830 LSET DDD1$=""                                           'Write empty page
14840 LSET DDD2$=""
14850 LSET DDD3$="" 
14860 PUT #5,CPOINT
14870 DATEN$=SPACE$(20)
14880 FOR II=1 TO 26
14890   DAT$(II)=DATEN$
14900 NEXT
14910 RETURN
14920 '
14930 ' **** SEITE SPEICHERN **************************************************
14940 '
14950 DATEN$=""
14960 FOR II=1 TO 12
14970   DATEN$=DATEN$+DAT$(II)
14980 NEXT
14990 LSET DDD1$=DATEN$:DATEN$=""
15000 FOR II=13 TO 24
15010   DATEN$=DATEN$+DAT$(II)
15020 NEXT
15030 LSET DDD2$=DATEN$:DATEN$=""
15040 FOR II=25 TO 26
15050   DATEN$=DATEN$+DAT$(II)
15060 NEXT 
15070 LSET DDD3$=DATEN$:DATEN$=""
15080 PUT #5,CPOINT
15090 RETURN
15100 '
15110 ' **** HACK ! (ZERSTÜCKELN DER DATEN-STRINGS) ***************************
15120 '
15130 FOR II=1 TO 12
15140   DAT$(II)=MID$(DDD1$,(II-1)*20+1,20)
15150 NEXT
15160 FOR II=13 TO 24
15170   DAT$(II)=MID$(DDD2$,(II-13)*20+1,20)
15180 NEXT 
15190 FOR II=25 TO 26
15200   DAT$(II)=MID$(DDD3$,(II-25)*20+1,20)
15210 NEXT 
15220 RETURN
15230 '
15240 ' **** UPRO DATENAUSGABE VN  ********************************************
15250 '
15260 ' IN: MTGLZAHL
15270 ' IN: NAMM$
15280 ' IN: BLATT
15290 PRINT DELHIGH$;                                          'TODO Necessary?
15300 PRINT HIGHON$
15310 PRINT FNCUR$(5,16);MTGLZAHL
15320 PRINT FNCUR$(5,29);NAMM$
15330 PRINT FNCUR$(3,8);BLATT
15340 FOR K=1 TO 26
15350   HOR=6-39*(K>13)
15360   VER=6+K+13*(K>13)
15370   FOR J=1 TO 3                                                 'Von-Datum
15380     PRINT FNCUR$(VER,HOR+(J-1)*3);MID$(DAT$(K),-1+J*2,2)
15390   NEXT
15400   FOR J=1 TO 3                                                 'Bis-Datum
15410     PRINT FNCUR$(VER,HOR+13+(J-1)*3);MID$(DAT$(K),5+J*2,2)
15420   NEXT
15430   PRINT FNCUR$(VER,HOR+26);MID$(DAT$(K),13,8)                        'Amt
15440 NEXT
15450 PRINT HIGHOFF$
15460 RETURN
15470 '
15480 ' *** Verwendungsnachweis drucken ***************************************
15490 '
15500 ' IN: BLATT
15510 ' IN: MITGLZAHL
15520 ' IN: NAMM$
15530 Q$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 12310 'Ja/Nein-Frage TODO Back to main code?
15540 IF JA=0 THEN 15760                                  'LW TODO Remove this!
15541 '
15550 RETURN                                         '*** TMP Print Disable ***
15551 LIND$=SPACE$(2)
15560 '
15570 LPRINT STRICH$
15580 LPRINT
15590 LPRINT LIND$;"Blatt:";BLATT
15600 LPRINT
15610 LPRINT LIND$;"Mitglieds-Nr.:";MTGLZAHL;TAB(24);"Name:";NAMM$
15620 LPRINT
15630 FOR K=1 TO 13
15640   KK=K:GOSUB 15730
15650   LPRINT LIND$;"Von:";VON$;" Bis:";BIS$;" Amt:";AMT$;
15660   KK=K+13:GOSUB 15730
15670   LPRINT " Von:";VON$;" Bis:";BIS$;" Amt:";AMT$
15680 NEXT 
15690 LPRINT
15700 LPRINT STRICH$
15710 RETURN
15720 'Format: VVVVVV BBBBBB AAAAAAAA
15730 VON$=LEFT$(DAT$(KK),2)+"."+MID$(DAT$(KK),3,2)+"."+MID$(DAT$(KK),5,2)
15740 BIS$=MID$(DAT$(KK),7,2)+"."+MID$(DAT$(KK),9,2)+"."+MID$(DAT$(KK),11,2)
15750 AMT$=RIGHT$(DAT$(KK),8)
15760 RETURN
15770 '
15780 ' ### Unterprogramme "Mitgliedsliste drucken" ###########################
15781 '
15782 '
15783 ' *** Ist der Drucker eingeschaltet? Frage *******************************
15784 '
15785 ' OUT: JA (1=Ja, 0=Nein)
15786 Q$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 12310 'Ja/Nein-Frage
15787 GOSUB 13030                                            'Nachricht löschen
15788 RETURN
15790 '
15800 ' *** Mitgliedsliste drucken (Kopf)
15810 '
15820 ' IN: DATUM$
15830 LPRINT TAB(16);"MITGLIEDSLISTE 25*******************************"
15840 LPRINT 
15850 LPRINT TAB(32);"Stand:";DATUM$
15860 LPRINT
15870 LPRINT TAB(3);"Mitgl."; 
15880 LPRINT TAB(10);"Name";
15890 LPRINT TAB(36);"Vorname, Anschrift";
15900 LPRINT TAB(63);"Status";
15910 LPRINT TAB(71);"Amt"
15920 RETURN
15930 '
15940 ' *** Mitgliedsliste drucken (Mitglied)
15950 '
15960 ' IN:  MTGLZAHL
15980 AMT$=""
15990 IN=INSTR(DAT$(6),"22**"):IF IN<>0 THEN AMT$=MID$(DAT$(6),IN,6):GOTO 16030
16000 IN=INSTR(DAT$(6),"23**"):IF IN<>0 THEN AMT$=MID$(DAT$(6),IN,5):GOTO 16030
16010 IN=INSTR(DAT$(6),"24**"):IF IN<>0 THEN AMT$=MID$(DAT$(6),IN,3)
16020 '
16030 LPRINT
16040 LPRINT TAB(5);MTGLZAHL;
16050 LPRINT TAB(10);DAT$(1);                                         'Nachname
16060 LPRINT TAB(36);DAT$(2);                                          'Vorname
16070 LPRINT TAB(65);"(";LEFT$(DAT$(5),1);")";                          'Status
16080 LPRINT TAB(71);AMT$                                                  'Amt
16090 LPRINT TAB(36);DAT$(10)                                           'Straße
16100 LPRINT TAB(36);DAT$(11);                                    'Postleitzahl
16110 LPRINT TAB(41);DAT$(12)                                              'Ort
16120 LPRINT TAB(36);"Tel.:";DAT$(13)                                  'Telefon
16130 LPRINT
16180 RETURN
16181 '
16182 ' *** Druck abbrechen? Frage
16183 '
16184 ' OUT: JA (1=true,0=false)
16185 JA=0 
16186 IF INKEY$="" THEN 16190
16187 Q$="Drucken abbrechen?":GOSUB 12310                        'Ja/Nein-Frage
16188 IF JA=1 THEN 16190
16189 N$="Mitgliedsliste wird gedruckt... Abbrechen mit Taste.":GOSUB 12890 'Nachricht
16190 RETURN
16191 '
16200 ' *** Adressaufkleber drucken
16210 '                                                           Ja/Nein-Frage
16220 LIND$=SPACE$(3)
16250 '
16260 LPRINT LIND$;DAT$(7)                                              'Anrede
16270 LPRINT LIND$;LEFT$(DAT$(2),INSTR(DAT$(2)," "));DAT$(1)  'Vorname Nachname
16280 LPRINT
16290 LPRINT LIND$;DAT$(10)                                  'Straße Hausnummer
16300 LPRINT
16310 LPRINT LIND$;DAT$(11);" ";DAT$(12)                      'Postleitzahl Ort
16320 LPRINT
16330 LPRINT
16340 LPRINT
16350 LPRINT
16360 LPRINT
16370 LPRINT
16380 RETURN
16390 '
16400 ' *** Stammsatz drucken (Bildschirmformat)
16410 '
16420 RETURN                                         '*** TMP Print Disable *** 
16421 XCOL1=8
16430 XCOL2=43
16440 PRINT STRICH$
16450 PRINT
16460 PRINT TAB(31);"Mitgliedsnummer";MTGLZAHL
16470 PRINT
16480 RESTORE 16540
16490 FOR K=1 TO 7:READ A,B
16500   IF A<>0 THEN PRINT TAB(XCOL1);FENAM$(A);DAT$(A);
16510   IF B<>0 THEN PRINT TAB(XCOL2);FENAM$(B);DAT$(B);
16520   PRINT
16530 NEXT K
16540 DATA 1,0,2,15,3,16,4,0,5,17,6,18,0,19
16550 FOR K=7 TO 14
16560   PRINT TAB(XCOL1);FENAM$(K);DAT$(K);
16570   PRINT TAB(XCOL2);FENAM$(K+13);DAT$(K+13);
16580 NEXT K
16590 PRINT
16600 PRINT
16610 PRINT
16620 PRINT STRICH$
16630 RETURN
16640 '
16650 ' *** Mitgliedsvermerke drucken
16660 '
16670 ' IN: NAMM$ =
16680 RETURN                                         '*** TMP Print Disable ***
16681 LIND$=SPACE$(9)
16690 SLOT1$="___"
16700 SLOT2$="___ / ___"
16710 SLOT3$="___ / ___ / ___"
16720 '
16730 LPRINT TAB(10);"Mitgliedsnr.: ";SLOT1$;
16740 LPRINT TAB(28);"Name: ";NAMM$;
16750 LPRINT TAB(65);"Datum: ";SLOT3$
16760 LPRINT
16770 LPRINT LIND$;"26*************************** ";SLOT3$
16780 LRPINT
16790 LPRINT LIND$;"27*************************** ";SLOT3$
16800 LRPINT
16810 LPRINT LIND$;"28*************************** ";SLOT3$
16820 LRPINT
16830 LPRINT LIND$;"29*************************** ";SLOT3$
16840 LRPINT
16850 LPRINT LIND$;"30*************************** ";SLOT2$
16860 LRPINT
16870 LPRINT LIND$;"31*************************** ";SLOT3$
16880 LRPINT
16890 LPRINT LIND$;"32*************************** ";SLOT3$
16900 LRPINT
16910 LPRINT LIND$;"33*************************** ";SLOT1$
16920 LRPINT
16930 LPRINT LIND$;"34*************************** ";SLOT3$
16940 LRPINT
16950 LPRINT LIND$;"35*************************** ";SLOT3$
16960 LRPINT
16970 LPRINT LIND$;"36*************************** ";SLOT3$
16980 LRPINT
16990 LPRINT LIND$;"37*************************** ";SLOT3$
17000 LRPINT
17010 LPRINT LIND$;"38*************************** ";SLOT3$
17020 RETURN
17030 '
17040 ' ### Hilfe-Texte #######################################################
17050 '
17060 ' *** Hilfe-Text "Mitgliederverwaltung" 
17070 '
17080 MT$="HILFE":GOSUB 13100                                      'Basis Maske
17090 PRINT HOME$
17100 PRINT IN$;"Die MITGLIEDERVERWALTUNG bietet folgende Funktionen an :"
17110 PRINT 
17120 PRINT IN$;"1 - STAMMSATZPFLEGE"
17130 PRINT IN$;"    Bearbeiten von Mitgliedsdaten gemäß dem Personalstamm-"
17140 PRINT IN$;"    blatt" 
17150 PRINT
17160 PRINT IN$;"2 - VERWENDUNGSNACHWEIS"
17170 PRINT IN$;"    Bearbeiten von Mitgliedsdaten, die Ämter umd Amtsperioden"
17180 PRINT IN$;"    eines Mitglieds enthalten"
17190 PRINT
17200 PRINT IN$;"3 - MITGLIEDSLISTE DRUCKEN"
17210 PRINT IN$;"    Drucken von Mitgliedslisten nach verschiedenen Gesichts-"
17220 PRINT IN$;"    punkten"
17230 PRINT
17240 PRINT IN$;"Wählen Sie aus, indem Sie die zugehörige Zahlentaste drücken."
17250 GOSUB 13670                                      'Rückkehr aus Hilfe-Text
17260 RETURN
17270 '
17280 ' *** Hilfe-Text "Stammsatzpflege" 
17290 '
17300 MT$="HILFE":GOSUB 13100                                      'Basis Maske
17310 PRINT HOME$
17320 PRINT IN$;"Die STAMMSATZPFLEGE bietet folgende Funktionen an:"
17330 PRINT
17340 PRINT IN$;"1 - STAMMSATZ ANLEGEN"
17350 PRINT IN$;"    Erfassen der Daten eines neuen Mitglieds"
17360 PRINT
17370 PRINT IN$;"2 - STAMMSATZ ÄNDERN"
17380 PRINT IN$;"    Verändern eines bereits bestehenden Stammsatzes"
17390 PRINT
17400 PRINT IN$;"3 - STAMMSATZ LÖSCHEN"
17410 PRINT IN$;"    Verändern des Mitgliedsstatus auf `ausgetreten`"
17420 PRINT
17430 PRINT IN$;"4 - STAMMSATZ DRUCKEN"
17440 PRINT IN$;"    Drucken eines kompletten Stammsatzes"
17450 PRINT
17460 PRINT IN$;"5 - STAMMSATZ SUCHEN"
17470 PRINT IN$;"    Suchen vom Stammsätzen nach bestimmten Gesichtspunkten"
17480 PRINT
17490 PRINT IN$;"Wählen Sie aus, indem Sie die zugehörige Zahlentaste drücken."
17500 GOSUB 13670                                      'Rückkehr aus Hilfe-Text
17510 RETURN
17520 '
17530 ' *** Hilfe-Text "Stammsatz anlegen"
17540 '
17550 SAVEMT$=MT$:MT$="HILFE":GOSUB 13100:MT$=SAVEMT$              'Basis Maske
17560 PRINT HOME$
17570 PRINT IN$;"Sie können jetzt Daten eines neuen Mitglieds eingeben, jedoch"
17580 PRINT IN$;"nicht die eines bestehenden Mitglieds ändern."
17590 PRINT
17600 PRINT IN$;"Die Mitgliedsnummer wird laufend vergeben."
17610 PRINT
17620 PRINT IN$;"Während der Dateneingebe können Sie mit der DELETE-Taste"
17630 PRINT IN$;"korrigieren. Ist ein Feld ausgefüllt worden, muß RETURN"
17640 PRINT IN$;"gedrückt werden, da sonst die Eingabe nicht gespeichert wird."
17650 PRINT IN$;"Mit den Cursortasten kann das Feld gewechselt werden."
17660 PRINT
17670 PRINT IN$;"`=>`-Felder müssen ausgefüllt werden."
17680 PRINT
17690 PRINT IN$;"Weitere Funktionstasten:"
17700 PRINT IN$;"<F7>  Diesen Stammsatz speichern"
17710 GOSUB 13670                                      'Rückkehr aus Hilfe-Text
17720 DATENAUSG=1                                                          '???
17730 RETURN
17740 '
17750 ' *** Hilfe-Text "Stammsatz ändern (I)"
17760 '
17770 MT$="HILFE":GOSUB 13100                                      'Basis Maske
17780 PRINT HOME$
17790 PRINT IN$;"Sie können jetzt einen bestehenden Stammsatz ändern."
17800 PRINT
17810 PRINT IN$;"Geben Sie die Mitgliedsnummer des Stammsatzes ein."
17820 PRINT IN$;"Bei Tippfehlern können Sie mit der DELETE-Taste korrigieren."
17830 GOSUB 13670                                      'Rückkehr aus Hilfe-Text
17840 RETURN
17850 '
17860 ' *** Hilfe-Text "Stammsatz ändern (II)"
17870 '
17880 SAVEMT$=MT$:MT$="HILFE":GOSUB 13100:MT$=SAVEMT$              'Basis Maske
17890 PRINT HOME$
17900 PRINT IN$;"Sie können jetzt einen bestehenden Stammsatz ändern."
17910 PRINT
17920 PRINT IN$;"Während der Dateneingabe können Sie mit der DELETE-Taste"
17930 PRINT IN$;"korrigieren. Ist ein Feld geändert worden, muß RETURN"
17940 PRINT IN$;"gedrückt werden, da sonst die Änderung nicht gespeichert wird."
17950 PRINT IN$;"Mit den Cursortasten kann das Feld gewechselt werden."
17960 PRINT
17970 PRINT IN$;"`=>`-Felder müssen ausgefüllt werden."
17980 PRINT
17990 PRINT IN$;"Weitere Funktionstasten:"
18000 PRINT IN$;"<F7>  Diesen Stammsatz speichern"
18010 GOSUB 13670                                      'Rückkehr aus Hilfe-Text
18020 GOSUB 13100                                                  'Basis Maske
18030 GOSUB 13210                                    'F-Tastenleiste einblenden
18040 RETURN
18050 '
18060 ' *** Hilfe-Text "Stammsatz löschen"
18070 '
18080 MT$="HILFE":GOSUB 13100                                      'Basis Maske
18090 PRINT HOME$
18100 PRINT IN$;"Sie können jetzt den Status eines Mitglieds auf `ausgetreten`"
18110 PRINT IN$;"ändern. Damit wird der Stammsatz beim Druck von Mitglieds-"
18120 PRINT IN$;"listen nicht berüchsichtigt."
18130 PRINT IN$;"Auf keinen Fall werden die Daten WIRKLICH gelöscht."
18140 PRINT
18150 PRINT IN$;"Geben Sie die Mitgliedsnummer des Stammsatzes ein."
18160 PRINT IN$;"Bei Tippfehlern können Sie mit der DELETE-Taste korrigieren."
18170 GOSUB 13670                                      'Rückkehr aus Hilfe-Text
18180 RETURN
18190 '
18200 ' *** Hilfe-Text "Stammsatz suchen"
18210 '
18220 IF DRUCKFLAG THEN 18890                                              '???
18230 SAVEMT$=MT$:MT$="HILFE":GOSUB 13100:MT$=SAVEMT$              'Basis Maske
18240 PRINT HOME$
18250 PRINT IN$;"Sie können jetzt Stammsätze suchen."
18260 PRINT
18270 PRINT IN$;"1 - Suchen nach Nummern"
18280 PRINT IN$;"    Geben Sie ein Intervall von Mitgliedsnummern ein."
18290 PRINT IN$;"    Es werden alle Stammsätze ausgegeben, deren Nummern im"
18300 PRINT IN$;"    Intervall liegen."
18310 PRINT
18320 PRINT IN$;"2 - Suchen nach Maske"
18330 PRINT IN$;"    Sie füllen ein Formular aus. Diejenigen Stammsätze, die"
18340 PRINT IN$;"    mit dem Formular übereinstimmen, werden ausgegeben."
18350 PRINT IN$;"    Leere Felder bezeichnen beliebige Stammsatzdaten."
18360 PRINT IN$;"    Ein * zu Beginn des Feldes bedeutet: Das folgende Wort"
18370 PRINT IN$;"    darf sich an beliebiger Stelle des Feldes befinden."
18380 PRINT IN$;"    Ein oder mehrere ? zu Beginn des Feldes stellen Platz-"
18390 PRINT IN$;"    halter für beliebige Zeichen dar."
18400 PRINT
18410 PRINT IN$;"Wählen Sie aus, indem Sie die zugehörige Zahlentaste drücken."
18420 GOSUB 13670                                      'Rückkehr aus Hilfe-Text
18430 RETURN
18440 ' 
18450 ' *** Hilfe-Text für "Stammsatz drucken"
18460 '
18470 SAVEMT$=MT$:MT$="HILFE":GOSUB 13100:MT$=SAVEMT$              'Basis Maske
18480 PRINT HOME$
18490 PRINT IN$;"Sie können jetzt einen kompletten Stammsatz drucken, so, wie"
18500 PRINT IN$;"er auf dem Bildschirm steht."
18510 PRINT
18520 PRINT IN$;"Geben Sie die Mitgliedsnummer des Stammsatzes ein."
18530 PRINT IN$;"Bei Tippfehlern können Sie mit der DELETE-Taste korrigieren."
18540 GOSUB 13670                                      'Rückkehr aus Hilfe-Text
18550 RETURN
18560 '
18570 ' *** Hilfe-Text "Verwendungsnachweis (I)"
18580 '
18590 MT$="HILFE":GOSUB 13100                                      'Basis Maske
18600 PRINT HOME$
18610 PRINT IN$;"Sie können jetzt einen Verwendungsnachweis ändern."
18620 PRINT
18630 PRINT IN$;"Geben Sie die Mitgliedsnummer des Verwendungsnachweises ein."
18640 PRINT IN$;"Bei Tippfehlern können Sie mit der DELETE-Taste korrigieren."
18650 GOSUB 13670                                      'Rückkehr aus Hilfe-Text
18660 RETURN
18670 '
18680 ' *** Hilfe-Text "Verwendungsnachweis (II)"
18690 '
18700 SAVEMT$=MT$:MT$="HILFE":GOSUB 13100:MT$=SAVEMT$              'Basis Maske
18710 PRINT HOME$
18720 PRINT IN$;"Sie können jetzt einen bestehenden Verwendungsnachweis ändern."
18730 PRINT
18740 PRINT IN$;"Während der Dateneingabe können Sie mit der DELETE-Taste"
18750 PRINT IN$;"korrigieren. Ist ein Feld ausgefüllt worden, muß RETURN"
18760 PRINT IN$;"gedrückt werden, da sonst die Änderung nicht gespeichert"
18770 PRINT IN$;"wird. Mit den Cursortasten kann das Feld gewechselt werden."
18780 PRINT
18790 PRINT IN$;"Weitere Funktionstasten:"
18800 PRINT IN$;"<F5>  Zurückblättern (ohne die jetzige Seite zu speichern)"
18810 PRINT IN$;"<F6>  Vorblättern (ohne die jetzige Seite zu speichern)"
18820 PRINT IN$;"<F7>  Aktuelle Seite speichern"
18830 PRINT IN$;"<F9>  Aktuelle Seite drucken"
18840 GOSUB 13670                                      'Rückkehr aus Hilfe-Text
18850 RETURN
18860 '
18870 ' *** Hilfe-Text "Mitgliedsliste drucken"
18880 '
18890 SAVEMT$=MT$:MT$="HILFE":GOSUB 13100:MT$=SAVEMT$              'Basis Maske
18900 PRINT HOME$
18910 PRINT IN$;"Sie können jetzt eine Mitgliedsliste drucken." 
18920 PRINT
18930 PRINT IN$;"1 - Suchen & Drucken"
18940 PRINT IN$;"    Sie können Stammsätze nach verschiedenen Gesichtspunkten"
18950 PRINT IN$;"    suchen und drucken."
18960 PRINT
18970 PRINT IN$;"2 - Komplett drucken"
18980 PRINT IN$;"    Es wird eine komplette Mitgliedsliste ausgedruckt."
18990 PRINT IN$;"    Ausgetretene Mitglieder werden nicht berücksichtigt."
19000 PRINT
19010 PRINT IN$;"3 - Datenblatt drucken"
19020 PRINT IN$;"    Der Stammsatz und der Verwendungsnachweis werden im"
19030 PRINT IN$;"    Bildschirmformat mit einer Liste von Mitgliedsvermerken"
19040 PRINT IN$;"    ausgedruckt."
19050 PRINT
19060 PRINT IN$;"Wählen Sie aus, indem Sie die zugehörige Zahlentaste drücken."
19070 GOSUB 13670                                      'Rückkehr aus Hilfe-Text
19080 RETURN
19090 '
19100 ' *** Programmfehler ******************************************* REVIEWED
19110 '
19120 LIND$=SPACE$(15)
19130 GOSUB 13100                                                  'Basis Maske
19140 PRINT HOME$
19150 PRINT LIND$;"Ein Programmfehler ist aufgetreten."
19160 PRINT   
19170 PRINT LIND$;"- Notieren Sie folgende Fehlerinformation:"
19180 PRINT LIND$;"    ";HIGHON$;"ERL";HIGHOFF$;" (Error Line)";HIGHON$;ERL;HIGHOFF$
19190 PRINT LIND$;"    ";HIGHON$;"ERR";HIGHOFF$;" (Error Code)";HIGHON$;ERR;HIGHOFF$
19200 PRINT LIND$;"- Drücken Sie am Rechner den Knopf hinten rechts."
19210 PRINT LIND$;"19**************************"
19220 '
19230 ' *** Programmende ********************************************* REVIEWED
19240 '
19250 PRINT FNCUR$(22,1);HIGHOFF$;CURON$;
19260 RESET
19270 'LW RUN "DISKMENU.EXE"
19280 END                                                           '1596 lines
