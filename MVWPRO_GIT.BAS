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
1760 ON ERROR GOTO 19240                                 'Enable error trapping
1770 '
1780 GOSUB 10050    'Tasten deaktivieren, ASCII Codes für Cursortasten vergeben
1790 PRINT CUROFF$;
1800 '
1810 ' *** Beginn des Mitgliederverwaltungsprogramms **************************
1820 '
1830 MT$="MITGLIEDERVERWALTUNG":MI$="G-01000":GOSUB 13100          'Basis Maske
1840 Q$="Bitte Daten-Diskette in Laufwerk B einlegen. Bereit?":GOSUB 12250
1850 IF JA=0 THEN 19370                                           'Programmende
1860 '
1870 N$="Einen Moment bitte...":GOSUB 12970    'Nachricht und F-keys ausblenden
1880 GOSUB 10930                          'Einlesen Stammdatenfelder-Definition
1890 GOSUB 11740                                   'Fielden der Stammsatz-Datei
1900 GOSUB 13770                       'Fielden der Verwendungsnachweis-Dateien
1910 GOSUB 11200                                              'Einlesen MTGLMAX
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
2040 IF NKEY THEN ON NKEY GOTO 2130,5850,6670
2050 ON FKEY GOTO 2060,2070,1960,2070         'Hilfe, Zurück, Anfang, Hauptmenü
2060 GOSUB 17200:GOTO 1960                                      'F1->Hilfe-Text
2070 GOSUB 13530:GOTO 2020                           'F2->Zurück, F4->Hauptmenü
2080 '
2090 ' ########################### Stammsatzpflege ############################
2100 '
2110 ' *** Stammsatzpflege-Menü ***********************************************
2120 '
2130 MT$="STAMMSATZPFLEGE":MI$="G-01100":GOSUB 13100               'Basis Maske
2140 FKEYS$="":GOSUB 13210                           'F-Tastenleiste einblenden
2150 PRINT FNCUR$(4,29);"1 - Stammsatz anlegen"
2160 PRINT FNCUR$(6,29);"2 - Stammsatz ändern"
2170 PRINT FNCUR$(8,29);"3 - Stammsatz löschen"
2180 PRINT FNCUR$(10,29);"4 - Stammsatz drucken"
2190 PRINT FNCUR$(12,29);"5 - Stammsatz suchen"
2200 FKEYMAX=4:NKEYMAX=5:GOSUB 13370                              'Menü-Eingabe
2210 IF NKEY THEN ON NKEY GOTO 2280,2820,3440,5480,3870
2220 ON FKEY GOTO 2230,1960,1960,2240         'Hilfe, Zurück, Anfang, Hauptmenü
2230 GOSUB 17420:GOTO 2130                                      'F1->Hilfe-Text
2240 GOSUB 13530:GOTO 2200                                       'F4->Hauptmenü
2250 '
2260 ' *** Stammsatz erfassen von 02************ ******************************
2270 '
2280 MTGLZAHL=MTGLMAX+1
2290 F=1
2300 DATENAUSG=1                                                           '???
2310 MT$="STAMMSATZ ANLEGEN":MI$="G-01101":GOSUB 13100             'Basis Maske
2320 FKEYS$="<F7>=Speichern":GOSUB 13210             'F-Tastenleiste einblenden
2330 GOSUB 11090                                                  'Clear DAT$()
2340 GOSUB 12760                                           'Defaultwerte setzen
2350 PRINT FNCUR$(3,31);"Mitgliedsnummer";HIGHON$;MTGLZAHL;HIGHOFF$
2360 IF DATENAUSG=1 THEN GOSUB 11620:DATENAUSG=0     'Felder und Feldwerte '???
2370 Z=FEROW(F):S=FECOL(F)+XOFF:LNG=FELNG(F):TYP=FETYP(F):MUSS=FEMUSS(F)
2380 FKEYMASK$="12347":CKEYMASK$="12":INTEXT$=DAT$(F)
2390 GOSUB 10370                                                         'Input
2400 IF ASCC<>ASCRET THEN 2440                                 'Carriage return
2410   DAT$(F)=OUTTEXT$
2420   F=F+1:IF F>FELDMAX THEN F=1
2430   GOTO 2370
2440 IF CKEY=0 THEN 2490                                           'Cursor keys
2450   PRINT FNCUR$(Z,S);HIGHON$;DAT$(F);HIGHOFF$;STRING$(LNG-LEN(DAT$(F)),".")
2460   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FELDMAX
2470   IF CKEY=CKEYDN THEN F=F+1:IF F>FELDMAX THEN F=1
2480   GOTO 2370
2490 IF FKEY=1 THEN GOSUB 17670:GOTO 2310                       'F1->Hilfe-Text
2500 IF FKEY=2 THEN GOSUB 2590:GOTO 2130                       'Exit "Erfassen"
2510 IF FKEY=3 THEN GOSUB 2590:GOTO 1960                       'Exit "Erfassen"
2520 IF FKEY<>4 THEN 2570
2530   GOSUB 13600                                'Rückkehr ins Hauptmenü Frage
2540   IF JA=0 THEN GOSUB 13210:GOTO 2390                    'F-Keys einblenden
2550   GOSUB 2590                                              'Exit "Erfassen"
2560   GOTO 19370                                                 'Programmende
2570 IF FKEY=7 THEN GOSUB 2730:GOTO 2280
2580 '
2590 ' TMPF=1                                                              '???
2600 ' IF INSTR(SPACE$(25),DAT$(TMPF))=0 THEN 2670
2610 '   N$="DEBUG: DAT$() with 25 spaces!":GOSUB 12510              'Nachricht
2620 '   TMPF=TMPF+1:IF TMPF<(FELDMAX+1) THEN 2620
2630 '   MTGLZAHL=MTGLZAHL-1
2640 '   GOTO 2750
2650 '
2660 GOSUB 10880                                             'Debounce keyboard
2670 Q$="Wollen Sie vorher den Stammsatz speichern?":GOSUB 12250 'Ja/Nein-Frage
2680 IF JA=1 THEN 2730                                        'Nachricht und...
2690   N$="Stammsatz wird nicht gespeichert.":GOSUB 12970    '...F-Keys löschen
2700   MTGLZAHL=MTGLZAHL-1
2710   GOTO 2770
2720 '                                                            'Nachricht...
2730 N$="Stammsatz wird gespeichert...":GOSUB 12970      '...und F-Keys löschen
2740 GOSUB 12050                                       'Speichern der Felddaten
2750 GOSUB 13880                                        'Erste VN-Seite anlegen
2760 MTGLMAX=MTGLZAHL:GOSUB 11290                            'Schreiben MTGLMAX
2770 GOSUB 13030                                             'Nachricht löschen
2780 RETURN
2790 ' 
2800 ' *** Stammsatz ändern von 03************ ********************************
2810 '
2820 MT$="STAMMSATZ ÄNDERN":MI$="G-01102":GOSUB 13100              'Basis Maske
2830 FKEYS$="":GOSUB 13210                           'F-Tastenleiste einblenden
2840 PRINT FNCUR$(4,24)"Bitte geben Sie die Mitgliedsnummer" 
2850 PRINT FNCUR$(5,25)"des zu ändernden Stammsatzes ein."
2860 PRINT FNCUR$(6,23);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
2870 PRINT FNCUR$(7,24)"Beenden Sie die Eingabe mit RETURN."
2880 PRINT FNCUR$(9,39)"=>..."
2890 BADINPUT=0
2900 Z=9:S=41:LNG=3:TYP=2:MUSS=1
2910 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
2920 GOSUB 10380                                                       'Input++
2930 IF FKEY=1 THEN GOSUB 17890:GOTO 2820                               'F-Keys
2940 IF FKEY=2 THEN 2130
2950 IF FKEY=3 THEN 1960
2960 IF FKEY=4 THEN GOSUB 13530:GOTO 2920         'Rückkehr ins Hauptmenü-Frage
2970 '
2980 M=VAL(OUTTEXT$)
2990 IF M>=1 AND M<=MTGLMAX THEN 3040
3000   N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12890       'Nachricht
3010   BADINPUT=1
3020   INTEXT$=OUTTEXT$                                                    '???
3030   GOTO 2900
3040 MTGLZAHL=M
3050 N$="Stammsatz wird gelesen...":GOSUB 12970   'Nachricht und F-Keys löschen
3060 GOSUB 11800                                                   'Daten lesen
3070 GOSUB 13030                                             'Nachricht löschen
3080 '
3090 GOSUB 13100                                                   'Basis Maske
3100 FKEYS$="<F7>=Speichern":GOSUB 13210             'F-Tastenleiste einblenden
3110 PRINT FNCUR$(3,31);"Mitgliedsnummer";HIGHON$;MTGLZAHL;HIGHOFF$
3120 GOSUB 11620                                 'Felder und Feldwerte ausgeben
3130 F=1
3140 Z=FEROW(F):S=FECOL(F)+XOFF:LNG=FELNG(F):TYP=FETYP(F):MUSS=FEMUSS(F)
3150 FKEYMASK$="12347":CKEYMASK$="12":INTEXT$=DAT$(F)
3160 GOSUB 10370                                                         'Input
3170 IF ASCC<>ASCRET THEN 3210
3180   DAT$(F)=OUTTEXT$
3190   F=F+1:IF F>FELDMAX THEN F=1
3200   GOTO 3140
3210 IF CKEY=0 THEN 3260
3220   PRINT FNCUR$(Z,S);HIGHON$;DAT$(F);HIGHOFF$;STRING$(LNG-LEN(DAT$(F)),".")
3230   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FELDMAX
3240   IF CKEY=CKEYDN THEN F=F+1:IF F>FELDMAX THEN F=1
3250   GOTO 3140
3260 IF FKEY=1 THEN GOSUB 18000:GOTO 3090
3270 IF FKEY=2 THEN GOSUB 3350:GOTO 2820               'Eingabe Mitgliedsnummer
3280 IF FKEY=3 THEN GOSUB 3350:GOTO 1960                              'MVW-Menü
3290 IF FKEY=7 THEN GOSUB 3370:GOTO 2820
3300 GOSUB 13600                             'F4-> Rückkehr ins Hauptmenü-Frage
3310 IF JA=0 THEN GOSUB 13210:GOTO 3160                      'F-Keys einblenden
3320 GOSUB 3350                                                  'Exit "Ändern'
3330 GOTO 19370                                                   'Programmende
3340 '
3350 Q$="Wollen Sie den Stammsatz speichern?":GOSUB 12250        'Ja/Nein-Frage
3360 IF JA=0 THEN RETURN
3370 N$="Stammsatz wird gespeichert...":GOSUB 12970 'Nachricht und F-Keys löschen
3380 GOSUB 12050                                               'Daten speichern
3390 GOSUB 13030                                             'Nachricht löschen
3400 RETURN
3410 '
3420 ' *** Stammsatz löschen von 04********** *********************************
3430 '
3440 MT$="STAMMSATZ LÖSCHEN":MI$="G-01103":GOSUB 13100             'Basis Maske
3450 FKEYS$="":GOSUB 13210                           'F-Tastenleiste einblenden
3460 PRINT FNCUR$(4,24);"Bitte geben Sie die Mitgliedsnummer"         'Menütext
3470 PRINT FNCUR$(5,25);"des zu löschenden Mitglieds ein."
3480 PRINT FNCUR$(6,23);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
3490 PRINT FNCUR$(7,15);"(Dabei wird nur der Status des Mitglieds verändert.)"
3500 PRINT FNCUR$(8,24);"Beenden Sie die Eingabe mit RETURN."
3510 PRINT FNCUR$(10,39);"=>..."
3520 BADINPUT=0
3530 Z=10:S=41:LNG=3:TYP=2:MUSS=1
3540 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
3550 GOSUB 10380                                                       'Input++
3560 IF FKEY=1 THEN GOSUB 18200:GOTO 3440
3570 IF FKEY=2 THEN 2130                                                'F-Keys
3580 IF FKEY=3 THEN 1960
3590 IF FKEY=4 THEN GOSUB 13530:GOTO 3550 'Rückkehr ins Hauptmenü-Frage 'F-keys einblenden
3600 '
3610 M=VAL(OUTTEXT$)
3620 IF M>=1 AND M<=MTGLMAX THEN 3670
3630   N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12890       'Nachricht
3640   BADINPUT=1
3650   INTEXT$=OUTTEXT$                                                    '???
3660   GOTO 3530
3670 MTGLZAHL=M
3680 N$="Stammsatz wird gelesen...":GOSUB 12970   'Nachricht und F-Keys löschen
3690 GOSUB 11800                                                   'Daten lesen
3700 GOSUB 13030                                             'Nachricht löschen
3710 '
3720 GOSUB 13100                                                   'Basis Maske
3730 PRINT FNCUR$(3,31);"Mitgliedsnummer";HIGHON$;MTGLZAHL;HIGHOFF$
3740 GOSUB 11620                                 'Felder und Feldwerte ausgeben
3750 Q$="Mitglied endgültig löschen?":GOSUB 12250                'Ja/Nein-Frage
3760 IF JA=0 THEN 3830
3770   DAT$(5)="ausgetreten"
3780   PRINT FNCUR$(FEROW(5),FECOL(5)+XOFF);HIGHON$;DAT$(5);HIGHOFF$;
3790   PRINT STRING$(FELNG(5)-LEN(DAT$(5)),".")                'LW NO semicolon
3800   N$="Stammsatz wird gespeichert...":GOSUB 12970 'Nachricht und F-Keys löschen
3810   GOSUB 12050                                             'Daten speichern
3820   GOSUB 13030                                           'Nachricht löschen
3830 GOTO 3440
3840 '
3850 ' *** Stammsatz suchen (und drucken) von 05*************** ***************
3860 '
3870 DRUCKFLAG=0
3880 MT$="STAMMSATZ SUCHEN":IF DRUCKFLAG THEN MT$="SUCHEN & DRUCKEN"
3890 MI$="G-01105":IF DRUCKFLAG THEN MI$="TODO***"
3900 GOSUB 13100                                                   'Basis Maske
3910 FKEYS$="":GOSUB 13210                           'F-Tastenleiste einblenden
3920 TMP$="Suchen":IF DRUCKFLAG THEN TMP$="Drucken" 
3930 PRINT FNCUR$(4,30)"1 - ";TMP$;" nach Nummern"
3940 PRINT FNCUR$(6,30)"2 - ";TMP$;" nach Maske"
3950 FKEYMAX=4:NKEYMAX=2:GOSUB 13370                              'Menü-Eingabe
3960 IF FKEY=1 THEN GOSUB 18340:GOTO 3880   'TODO IF DRUCKFLAG THEN ... 'F-Keys
3970 IF FKEY=2 THEN IF DRUCKFLAG THEN 6670 ELSE 2130
3980 IF FKEY=3 THEN 1960
3990 IF FKEY=4 THEN GOSUB 13530:GOTO 3950         'Rückkehr ins Hauptmenü-Frage
4000 ON NKEY GOTO 4020,4540 
4009 '
4010 ' *** Suchen nach Nummern
4011 '
4020 TMP$=STR$(1):NUMSUCH$(1)=RIGHT$(TMP$,LEN(TMP$)-1)
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
4190 GOSUB 10380                                                       'Input++
4200 IF FKEY=1 THEN GOSUB 18340:GOTO 4060                       'TODO DRUCKFLAG
4210 IF FKEY=2 THEN 3880
4220 IF FKEY=3 THEN 1960
4230 IF FKEY=4 THEN GOSUB 13530:GOTO 4190         'Rückkehr ins Hauptmenü-Frage
4240 IF FKEY<>5 THEN 4290
4250   SEARCH=BYNUMS
4260   NUMSUCH1=VAL(NUMSUCH$(1))
4270   NUMSUCH2=VAL(NUMSUCH$(2))
4280   IF DRUCKFLAG THEN 7410 ELSE 4790                         'Reentry, Suche
4290 M=VAL(OUTTEXT$)                                             'Handle RETURN
4300 IF M>=1 AND M<=MTGLMAX THEN 4340
4310   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 12890         'Nachricht
4320   GOSUB 4470
4330   GOTO 4190
4340 NUMSUCH$(F)=OUTTEXT$
4350 NUMSUCH1=VAL(NUMSUCH$(1))
4360 NUMSUCH2=VAL(NUMSUCH$(2))
4370 IF NUMSUCH1<=NUMSUCH2 THEN 4410
4380   N$="Ungültiger Bereich":GOSUB 12890                           'Nachricht
4390   GOSUB 4470
4400   GOTO 4190
4410 F=F+1:IF F>NUMSUCHMAX THEN F=1
4420 FKEYMASK$="12345"
4430 FKEYS$="<F5>=Suchen":IF DRUCKFLAG THEN FKEYS$="<F5>=Suchen & Drucken" 'TODO Required?
4440 GOSUB 13470                                        'F-Tastenleiste löschen
4441 GOSUB 13210                                     'F-Tastenleiste einblenden
4450 GOTO 4170
4460 '
4470 BADINPUT=1                                  'Finish handling invalid input
4480 INTEXT$=OUTTEXT$
4490 FKEYMASK$="1234"
4510 GOSUB 13470                                        'F-Tastenleiste löschen
4511 FKEYS$="":GOSUB 13210                           'F-Tastenleiste einblenden
4520 RETURN
4529 '
4530 ' *** Suchen nach Maske
4531 '
4540 GOSUB 11140                                                 'Clear SUCH$()
4550 GOSUB 13100                                                   'Basis Maske
4560 FKEYS$="<F5>=Suchen":IF DRUCKFLAG THEN FKEYS$="<F5>=Suchen & Drucken" 
4570 GOSUB 13210                                     'F-Tastenleiste einblenden
4580 GOSUB 11380                       'Feldnamen und Werteplatzhalter ausgeben
4590 F=1
4600 Z=FEROW(F):S=FECOL(F)+XOFF:LNG=FELNG(F):TYP=3
4610 FKEYMASK$="12345":CKEYMASK$="12":INTEXT$=""
4620 GOSUB 10370                                                         'Input
4640 IF FKEY=1 THEN GOSUB 18340:GOTO 4550           'TODO IF DRUCKFLAG THEN ...
4650 IF FKEY=2 THEN 3880
4660 IF FKEY=3 THEN 1960
4670 IF FKEY=4 THEN GOSUB 13530:GOTO 4620         'Rückkehr ins Hauptmenü-Frage
4671 IF FKEY<>5 THEN 4700 
4680   SEARCH=BYMASK                                                      '<F5>
4690   IF DRUCKFLAG THEN 7410 ELSE 4790                         'Reentry, Suche
4700 IF CKEY=0 THEN 4750                                    'Handle cursor keys
4710   PRINT FNCUR$(Z,S);HIGHON$;SUCH$(F);HIGHOFF$;STRING$(LNG-LEN(SUCH$(F)),".")
4720   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FELDMAX
4730   IF CKEY=CKEYDN THEN F=F+1:IF F>FELDMAX THEN F=1
4740   GOTO 4600
4750 SUCH$(F)=OUTTEXT$                                           'Handle RETURN
4760 F=F+1:IF F>FELDMAX THEN F=1
4770 GOTO 4600
4779 '
4780 ' *** Suche
4781 '
4790 GOSUB 13100                                                   'Basis Maske
4820 FKEYS$="<F5>=Suchen rückwärts <F6>=Suchen vorwärts <F7>=Adressaufkleber" 
4830 GOSUB 13210                                     'F-Tastenleiste einblenden
4840 PRINT FNCUR$(3,31);"Mitgliedsnummer"
4850 GOSUB 11380                       'Feldnamen und Werteplatzhalter ausgeben
4851 '
4852 IF SEARCH=BYMASK THEN 4857
4853 '
4854 MTGLZAHL=NUMSUCH1                      'Initialisierung Suche nach Nummern
4855 GOTO 4868
4856 '
4857 MTGLZAHL=0                               'Initialisierung Suche nach Maske
4858 GOSUB 8030                                     'next match IN/OUT:MTGLZAHL
4859 IF MTGLZAHL<>-1 THEN 4868
4860   N$="Keine Suchtreffer. Mit <F2> neue Suche.":GOSUB 12890      'Nachricht
4861   FKEYS$="":GOSUB 13210                         'F-Tastenleiste einblenden
4862   FKEYMASK$="1234":NKEYMASK="":GOSUB 13370
4863   IF FKEY=1 THEN GOSUB 18340:GOTO 4550 'TODO IF DRUCKFLAG THEN ... 'F-Keys
4864   IF FKEY=2 THEN 4550
4865   IF FKEY=3 THEN 1960
4866   IF FKEY=4 THEN GOSUB 13530:GOTO 4862       'Rückkehr ins Hauptmenü-Frage
4868 GOSUB 5320                              'Suchtreffer einlesen und anzeigen
4869 '
4970 FKEMAX=7:NKEYMAX=0:GOSUB 13370      'Menü-Eingabe 'TODO Nachricht löschen?
4980 ON FKEY GOTO 5000,5030,1960,5050,5080,5200,5380
4990 ' *** <F1> Hilfe
5000 GOSUB 18340                                                'TODO DRUCKFLAG
5010 GOTO 4970 
5020 ' *** <F2> Zurück
5030 IF SEARCH=BYNUMS THEN 4020 ELSE 4550            'TODO BYNUMS:Initialwerte?
5040 ' *** <F4> Rückkehr ins Hauptmenü
5050 GOSUB 13530                                  'Rückkehr ins Hauptmenü-Frage
5060 GOTO 4970
5070 ' *** <F5> Suchen rückwärts
5080 IF SEARCH=BYMASK THEN 5130
5081 '
5090 MTGLZAHL=MTGLZAHL-1                          'Suchen rückwärts nach Nummer
5100 IF MTGLZAHL<NUMSUCH1 THEN 5170
5110   GOSUB 5320:GOTO 4970                  'Suchtreffer einlesen und anzeigen
5120 '       
5130 TMP=MTGLZAHL                                  'Suchen rückwärts nach Maske
5140 GOSUB 7950                                     'prev match IN/OUT:MTGLZAHL
5150 IF MTGLZAHL=-1 THEN 5160
5151   GOSUB 5320:GOTO 4970                  'Suchtreffer einlesen und anzeigen
5152 '
5160 MTGLZAHL=TMP                                               'No match found
5170 N$="Erster Suchtreffer":GOSUB 12890                             'Nachricht
5180 GOTO 4970
5190 ' *** <F6> Suchen vorwärts
5200 IF SEARCH=BYMASK THEN 5250
5201 '
5210 MTGLZAHL=MTGLZAHL+1                           'Suchen vorwärts nach Nummer
5220 IF MTGLZAHL>NUMSUCH2 THEN 5290
5230   GOSUB 5320:GOTO 4970                  'Suchtreffer einlesen und anzeigen
5231 '
5250 TMP=MTGLZAHL                                    'Suche vorwärts nach Maske
5260 GOSUB 8030                                     'next match IN/OUT:MTGLZAHL
5270 IF MTGLZAHL=-1 THEN 5280
5271   GOSUB 5320:GOTO 4970                  'Suchtreffer einlesen und anzeigen
5272 '
5280 MTGLZAHL=TMP                                               'No match found
5290 N$="Letzter Suchtreffer":GOSUB 12890                            'Nachricht
5300 GOTO 4970
5309 '
5310 ' *** Suchtreffer einlesen und anzeigen
5311 '
5320 GOSUB 13030                            'Nachricht löschen 'TODO notwendig?
5330 GOSUB 11800                                       'Stammsatzdaten einlesen
5340 GOSUB 11500                                            'Feldwerte ausgeben
5350 PRINT FNCUR$(3,46);HIGHON$;STR$(MTGLZAHL);HIGHOFF$
5360 RETURN
5370 ' *** <F7> Adressaufkleber drucken
5380 GOSUB 15830                          'Ist der Drucker eingeschaltet? Frage
5390 IF JA=0 THEN 5440                              'LW TODO F-Keys einblenden?
5400   N$="Adressaufkleber wird gedruckt...":GOSUB 12890             'Nachricht
5410   GOSUB 16340                                     'Adressaufkleber drucken
5420   GOSUB 13030                                           'Nachricht löschen
5430   GOSUB 13210                                           'F-keys einblenden
5440 GOTO 4970                                                    'Neue Eingabe
5450 '
5460 ' *** Stammsatz drucken von 02************ & 04********** ****************
5470 '
5480 MT$="STAMMSATZ DRUCKEN":MI$="G-01104":GOSUB 13100             'Basis Maske
5490 FKEYS$="":GOSUB 13210                           'F-Tastenleiste einblenden
5500 PRINT FNCUR$(4,24)"Bitte geben Sie die Mitgliedsnummer"
5510 PRINT FNCUR$(5,23)"des auszudruckenden Stammsatzes ein."
5520 PRINT FNCUR$(6,23);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
5530 PRINT FNCUR$(7,24)"Beenden Sie die Eingabe mit RETURN."
5540 PRINT FNCUR$(9,39);"=>..."
5550 BADINPUT=0
5560 Z=9:S=41:LNG=3:TYP=2:MUSS=1
5570 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
5580 GOSUB 10380                                                       'Input++
5590 IF FKEY=1 THEN GOSUB 18590:GOTO 5480                               'F-Keys
5600 IF FKEY=2 THEN 2130
5610 IF FKEY=3 THEN 1960
5620 IF FKEY=4 THEN GOSUB 13530:GOTO 5580         'Rückkehr ins Hauptmenü-Frage
5630 '
5640 M=VAL(OUTTEXT$)                                             'Handle RETURN
5650   IF M>=1 AND M<=MTGLMAX THEN 5700
5660   N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12890       'Nachricht
5670   BADINPUT=1
5680   INTEXT$=OUTTEXT$                                                    '???
5690   GOTO 3530
5700 MTGLZAHL=M
5710 N$="Stammsatz wird gelesen...":GOSUB 12970   'Nachricht und F-Keys löschen
5720 GOSUB 11800                                                   'Daten lesen
5730 GOSUB 13030                                             'Nachricht löschen
5740 '
5750 GOSUB 13100                                                   'Basis Maske
5760 PRINT FNCUR$(3,31);"Mitgliedsnummer";HIGHON$;MTGLZAHL;HIGHOFF$
5770 GOSUB 11620                                 'Felder und Feldwerte ausgeben
5780 GOSUB 15830                                  'Drucker eingeschaltet? Frage
5790 IF JA=0 THEN 5810
5800 GOSUB 16520                          'Stammsatz drucken (Bildschirmformat)
5810 GOTO 5480
5820 '
5830 ' ######################### Verwendungsnachweis ##########################
5840 '
5850 GOSUB 11200                                              'Einlesen MTGLMAX
5860 VNFLAG=-1
5870 '
5880 ' Verwendungsnachweis-Menü ***********************************************
5890 '
5900 MT$="VERWENDUNGSNACHWEIS":MI$="G-01200":GOSUB 13100           'Basis Maske
5910 F=4                                                                'TODO ?
5920 FKEYS$="":GOSUB 13210                           'F-Tastenleiste einblenden
5930 PRINT CURON$;
5940 PRINT HIGHOFF$;
5950 PRINT FNCUR$(4,20)"   Bitte geben Sie die Mitgliedsnummer"
5960 PRINT FNCUR$(5,20)"des zu ändernden Verwendungsnachweises ein"
5970 PRINT FNCUR$(6,20)" und beenden Sie die Eingabe mit RETURN."
5980 PRINT FNCUR$(8,20)"                 =>..."
5990 Z=8:S=39:LNG=3:TYP=2
6000 GOSUB 10370                                                         'INPUT
6010 IF FKEY=1 THEN GOSUB 18710:GOTO 5900
6020 IF FKEY=2 OR FKEY=3 THEN GOSUB 11090:VNFLAG=0:GOTO 1960      'Clear DAT$()
6030 IF FKEY=4 THEN GOSUB 13530
6040 IF FKEY>4 THEN 5990
6050 IF CKEY<>0 THEN 5990
6060 IF LEN(OUTTEXT$)=0 THEN N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 12890:GOTO 5980 ELSE MITGLIED=VAL(OUTTEXT$):IF MITGLIED=0 THEN 6000 'Nachricht
6070 IF MITGLIED>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12890:PRINT HOME$;HIGHOFF$:GOTO 5980 'Nachricht
6080 MTGLZAHL=MITGLIED:MTGL$=STR$(MITGLIED)
6090 '
6100 ' *** Einlesen des Mitgliedsnamens ***************************************
6110 '
6120 PRINT FNCUR$(21,1);DELLINE$;
6130 GET #2,MITGLIED                                        'EINLESEN DATEN STS
6140 NNAM$=LEFT$(DD1$,25)                                 'NACHNAME RAUSFISCHEN
6150 VNAM$=MID$(DD1$,26,25)                               'VORNAME  RAUSFISCHEN
6160 '
6170 ' ** LEERZEICHEN HINTER NACHNAME ABSCHNEIDEN *****************************
6180 '
6190 FOR I=LEN(NNAM$) TO 1 STEP -1
6200   IF MID$(NNAM$,I,1)<>" " THEN NNAM$=LEFT$(NNAM$,I):I=1
6210 NEXT
6220 NAMM$=NNAM$+" "+VNAM$
6230 GOSUB 14050                                          'EINLESEN 1. VN-BLATT
6240 BLATT=1
6250 VP=1:HP=1                                                 'INITIALISIERUNG
6260 F=8
6270 GOSUB 13100                                                   'Basis Maske
6280 FKEYS$="<F7>=Speichern":GOSUB 13210             'F-Tastenleiste einblenden
6290 PRINT HIGHOFF$;
6300 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=Vorwärts";TAB(53);"<F9>=Blatt drucken";
6310 PRINT FNCUR$(3,2);"Blatt:"
6320 PRINT FNCUR$(5,2);"Mitglieds-Nr.:"
6330 PRINT FNCUR$(5,24);"Name:"
6340 MSK$="Von:  .  .   Bis:  .  .   Amt:"
6350 FOR KK=7 TO 19 
6360   PRINT FNCUR$(KK,2);MSK$;TAB(43);MSK$                    'LW NO semicolon
6370 NEXT
6380 GOSUB 15260                                                 'Ausgabe Daten
6390 LNG=2-(HP=7)*6                                    'BEGIN OF THE INPUT-LOOP
6400 TYP=2-(HP=7)
6410 Z=VP+13*(VP>13)+6
6420 S=6-39*(VP>13)-(HP<7)*(HP-1)*3+(HP>3)*(HP<7)*4-(HP=7)*26
6430 GOSUB 13030                                             'Nachricht löschen
6440 GOSUB 10370                                                       'INPUT+1
6450 IF HP<7 THEN IF ASCC=ASCRET THEN OUTTEXT$=STRING$(2-LEN(OUTTEXT$),"0")+OUTTEXT$   '+NULLEN
6460 IF CKEY<>0 OR ASCC=ASCRET THEN PRINT FNCUR$(Z,S);OUTTEXT$
6470 IF ASCC=ASCRET THEN IF HP<7 THEN MID$(DAT$(VP),-1+2*HP,2)=OUTTEXT$ ELSE DAT$(VP)=LEFT$(DAT$(VP),12)+OUTTEXT$+SPACE$(8-LEN(OUTTEXT$))
6480 IF ASCC=ASCRET AND HP<7 THEN JA=1:TEST$=OUTTEXT$:ON HP GOSUB 12650,12680,12710,12650,12680,12710:IF JA=0 THEN N$="Ungültiges Datum":GOSUB 12890:GOTO 6440 'Datums-Check 'Nachricht
6490 IF ASCC=ASCRET THEN IF HP=7 THEN HP=1:VP=VP-(VP<26)-(VP=26)*(-25):GOTO 6390 ELSE HP=HP+1:GOTO 6390     'NEUE SCREEN-POSITION
6500 IF CKEY<>0 THEN TEST$=MID$(DAT$(VP),-(HP<7)*2*HP-1-(HP=7)*14,-(HP<7)*2-(HP=7)*8):PRINT FNCUR$(Z,S);TEST$
6510 IF CKEY<>0 THEN IF HP<7 AND NOT TEST$="  " THEN JA=1:ON HP GOSUB 12650,12680,12710,12650,12680,12710:IF JA=0 THEN N$="Ungültiges Datum":GOSUB 12890:GOTO 6440 'Nachricht
6520 IF CKEY=CKEYDN THEN VP=VP-(VP<26)-(VP=26)*(-25):GOTO 6390
6530 IF CKEY=CKEYUP THEN VP=VP+(VP>1)-(VP=1)*25     :GOTO 6390
6540 IF CKEY=CKEYRT THEN HP=HP-(HP<7)-(HP=7)*(-6)   :GOTO 6390
6550 IF CKEY=CKEYLT THEN HP=HP+(HP>1)+(HP=1)*(-6)   :GOTO 6390
6560 IF FKEY=1 THEN GOSUB 18820 :GOTO 6250           'HILFE - ZU INIT UND MASKE
6570 IF FKEY=2 THEN 5900                                       'ZU MTGL-EINGABE
6580 IF FKEY=3 THEN VNFLAG=0:GOSUB 11090:GOTO 1960                'Clear DAT$()
6590 IF FKEY=4 THEN GOSUB 13530:GOSUB 13270:GOTO 6440 
6600 IF FKEY=5 THEN GOSUB 14140:GOTO 6390
6610 IF FKEY=6 THEN GOSUB 14280:GOTO 6390
6620 IF FKEY=7 THEN GOSUB 14920:GOTO 6440                 'SPEICHERN DANN INPUT
6630 IF FKEY=9 THEN GOSUB 15500:GOSUB 14470:GOTO 6430
6640 '
6650 ' ############# Mitgliedsliste drucken von 05*************** #############
6660 '
6670 MT$="MITGLIEDSLISTE DRUCKEN":MI$="G-01300":GOSUB 13100        'Basis Maske
6680 FKEYS$="":GOSUB 13210                           'F-Tastenleiste einblenden
6690 PRINT FNCUR$(4,28);"1 - Suchen & Drucken"
6700 PRINT FNCUR$(6,28);"2 - Komplett drucken"
6710 PRINT FNCUR$(8,28);"3 - Datenblatt drucken"
6720 FKEYMAX=4:NKEYMAX=3:GOSUB 13370                              'Menü-Eingabe
6730 IF FKEY=1 THEN GOSUB 19010:GOTO 6670
6740 IF FKEY=2 THEN 1960
6750 IF FKEY=3 THEN 1960
6760 IF FKEY=4 THEN GOSUB 13530:GOTO 6720
6770 ON NKEY GOTO 6810,6810,8300
6780 '
6790 ' *** Suchen & Drucken, Komplett drucken *********************************
6800 '
6810 IF NKEY=1 THEN MT$="SUCHEN & DRUCKEN":MI$="TODOAAAA"
6820 IF NKEY=2 THEN MT$="KOMPLETT DRUCKEN":MI$="TODOBBBB"
6830 GOSUB 13100                                                   'Basis Maske
6840 GOSUB 13210                                     'F-Tastenleiste einblenden
6850 PRINT FNCUR$(4,21);"Bitte geben Sie das heutige Datum ein."
6860 PRINT FNCUR$(5,22);"Beenden Sie die Eingabe mit RETURN."
6870 PRINT FNCUR$(7,35);"=>........"
6880 BADINPUT=0
6890 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
6900 Z=7:S=37:LNG=8:TYP=5:MUSS=1
6910 GOSUB 10380                                                       'Input++
6920 IF FKEY=1 THEN GOSUB 19010:GOTO 6810             'TODO Hilfe-Text anpassen
6930 IF FKEY=2 THEN 6670                                                'F-Keys
6940 IF FKEY=3 THEN 1960
6950 IF FKEY=4 THEN GOSUB 13530:GOTO 6910         'Rückkehr ins Hauptmenü-Frage
6960 '
6970 POS1=INSTR(OUTTEXT$,".")                                    'Handle RETURN
6980 POS2=INSTR(POS1+1,OUTTEXT$,".")
6990 IF POS2=0 THEN 7210
7000 POS3=INSTR(POS2+1,OUTTEXT$,".")
7010 IF POS3<>0 THEN 7210
7020 D1$=LEFT$(OUTTEXT$,POS1-1)
7030 IF LEN(D1$)=0 OR LEN(D1$)>2 THEN 7210
7040 D2$=MID$(OUTTEXT$,POS1+1,POS2-POS1-1)
7050 IF LEN(D2$)=0 OR LEN(D2$)>2 THEN 7210
7060 D3$=RIGHT$(OUTTEXT$,LEN(OUTTEXT$)-POS2)
7070 IF LEN(D3$)=0 OR LEN(D3$)>2 THEN 7210
7080 '
7090 D1=VAL(D1$):IF D1<1 OR D1>31 THEN 7230
7100 D2=VAL(D2$):IF D2<1 OR D2>12 THEN 7230
7110 D3=VAL(D3$) 
7120 ' TODO: Deeper validation
7130 D1$=STR$(D1):D1$=MID$(D1$,2,99):IF D1<10 THEN D1$="0"+D1$
7140 D2$=STR$(D2):D2$=MID$(D2$,2,99):IF D2<10 THEN D2$="0"+D2$
7150 D3$=STR$(D3):D3$=MID$(D3$,2,99):IF D3<10 THEN D3$="0"+D3$
7160 DATUM$=D1$+"."+D2$+"."+D3$
7170 '
7180 PRINT FNCUR$(7,37);HIGHON$;DATUM$;HIGHOFF$
7190 GOTO 7280
7191 '
7200 ' *** Fehlerbehandlung Datum
7201 '
7210 N$="Datum muss Format TT.MM.JJ haben. Bitte korrigieren.":GOSUB 12890 'Nachricht
7220 GOTO 7240
7230 N$="Ungültiges Datum. Bitte korrigieren.":GOSUB 12890           'Nachricht
7240 BADINPUT=1
7250 INTEXT$=OUTTEXT$                                                      '???
7260 GOTO 6900
7270 '
7280 IF NKEY=1 THEN 7350
7281 '
7282 ' *** Komplette Mitgliedliste drucken
7290   SEARCH=BYNUMS                          'Drucken komplette Mitgliedsliste
7300   NUMSUCH1=1:NUMSUCH2=MTGLMAX
7320   GOSUB 15830                              '"Drucker eingeschaltet?"-Frage
7330   IF JA=0 THEN 6810
7340   GOTO 7430                                           'LW OK-ish target...
7341 '
7342 ' *** Suchen nach Nummern und Maske
7350 DRUCKFLAG=1
7360 GOTO 3880                                  'Stammsatz suchen (und drucken)
7370 ' ---> Reentry <----------------------------------------------------------
7380 ' IN: SEARCH (BYNUMS, BYMASK)
7390 ' IN: NUMSUCH1, NUMSUCH2
7400 ' IN: SUCH$()
7410 GOSUB 15830                                '"Drucker eingeschaltet?"-Frage
7420 IF JA=0 THEN 7660
7430 N$="Mitgliedsliste wird gedruckt... Abbrechen mit Taste.":GOSUB 12890 'Nachricht
7440 GOSUB 15900                                 'Mitgliedsliste drucken (Kopf)
7450 IF SEARCH=BYMASK THEN 7550
7451 '
7452 ' *** Suchen nach Nummern
7460 FOR MTGLZAHL=NUMSUCH1 TO NUMSUCH2
7470   GOSUB 11800                                     'Stammsatzdaten einlesen
7480   IF INSTR(DAT$(5),"ausgetreten")>0 THEN 7500
7490     GOSUB 16040                         'Mitgliedsliste drucken (Mitglied)
7500   GOSUB 16250                                      'Druck abbrechen? Frage
7510   IF JA=1 THEN 7650
7520 NEXT MTGLZAHL
7530 GOTO 7650
7540 ' 
7541 ' *** Suchen nach Maske
7550 MTGLZAHL=0
7560 GOSUB 8030                                     'next match IN/OUT:MTGLZAHL
7570 IF MTGLZAHL=-1 THEN 7650
7580   GOSUB 11800            'Stammsatzdaten einlesen, IN:MTGLZAHL, OUT DAT$()
7590   IF INSTR(DAT$(5),"ausgetreten")>0 THEN 7610
7600     GOSUB 16040                         'Mitgliedsliste drucken (Mitglied)
7610   GOSUB 16250                                      'Druck abbrechen? Frage
7620   IF JA=1 THEN 7650
7630 GOTO 7570
7640 '
7641 ' *** Suchen zuende
7650 GOSUB 13030                                             'Nachricht löschen
7660 IF SEARCH=BYNUMS THEN 4020 ELSE 4540                  'LW Feels hackish...
7920 '
7930 ' *** Subroutine Prev match 
7940 ' IN: MTGLZAHL OUT: MTGLZAHL or -1 if not found
7950 MTGLZAHL=MTGLZAHL-1
7960 IF MTGLZAHL<1 THEN 8090
7970 GOSUB 8120
7980 IF NOT ISMATCH THEN 7950 
7990 RETURN
8000 '
8010 ' *** Subroutine Next match 
8020 ' IN: MTGLZAHL OUT: MTGLZAHL or -1 if not found
8030 MTGLZAHL=MTGLZAHL+1
8040 IF MTGLZAHL>MTGLMAX THEN 8090
8050 GOSUB 8120
8060 IF NOT ISMATCH THEN 8030 
8070 RETURN
8080 '
8090 MTGLZAHL=-1
8100 RETURN
8110 '
8111 ' *** Subroutine Is match? 
8120 GOSUB 11800                                                   'Daten lesen
8130 FOR K=1 TO FELDMAX
8140   IF LEFT$(SUCH$(K),1)<>"*" THEN 8170
8150     IF INSTR(DAT$(K),RIGHT$(SUCH$(K),LEN(SUCH$(K))-1))>0 THEN 8220
8160     GOTO 8250
8170   I=1
8180   IF MID$(SUCH$(K),I,1)="?" THEN I=I+1:GOTO 8180
8190   I=I-1
8200   IF INSTR(DAT$(K),RIGHT$(SUCH$(K),LEN(SUCH$(K))-I))=I+1 THEN 8220
8210   GOTO 8250
8220 NEXT K
8230 ISMATCH=1
8240 RETURN
8241 '
8250 ISMATCH=0
8260 RETURN
8270 '
8280 ' *** Datenblatt drucken *************************************************
8290 '
8300 MT$="DATENBLATT DRUCKEN":MI$="G-01303":GOSUB 13100            'Basis Maske
8310 FKEYS$="":GOSUB 13210                           'F-Tastenleiste einblenden
8320 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer"
8330 PRINT FNCUR$(5,23)"des zu druckenden Datenblattes ein."
8340 PRINT FNCUR$(7,23)"Beenden Sie die Eingabe mit RETURN."
8350 PRINT FNCUR$(9,39)"=>..."
8360 BADINPUT=0
8370 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
8380 Z=9:S=41:LNG=3:TYP=2:MUSS=1
8390 GOSUB 10380                                                       'Input++
8400 IF FKEY=1 THEN GOSUB 19010:GOTO 8300
8410 IF FKEY=2 THEN 6670                                                'F-Keys
8420 IF FKEY=3 THEN 1960
8430 IF FKEY=4 THEN GOSUB 13530:GOTO 8390         'Rückkehr ins Hauptmenü-Frage
8440 '
8450 M=VAL(OUTTEXT$)                                             'Handle RETURN
8460 IF M>=1 AND M<=MTGLMAX THEN 8510
8470   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 12890         'Nachricht
8480   BADINPUT=1
8490   INTEXT$=OUTTEXT$                                                    '???
8500   GOTO 8380
8510 MTGLZAHL=M
8520 GOSUB 15830                                '"Drucker eingeschaltet?"-Frage
8530 IF JA=1 THEN 8570
8540   GOSUB 13210                                   'F-Tastenleiste einblenden
8550   INTEXT$=OUTTEXT$
8560   GOTO 8380
8570 N$="Datenblatt drucken...":GOSUB 12970    'Nachricht und F-keys ausblenden
8580 GOSUB 11800                                          'Stammsatzdaten lesen
8590 NAMM$ = DAT$(1)+" "+DAT$(2)
8600 GOSUB 16520                          'Stammsatz drucken (Bildschirmformat)
8610 GOSUB 14050                             'Verwendungsnachweisdaten einlesen
8620 GOSUB 15530                              'Verwendungsnachweisdaten drucken
8630 GOSUB 16790                                     'Mitgliedsvermerke drucken
8640 GOSUB 13030                                             'Nachricht löschen
8650 GOTO 8300
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
10250 ' *** Eingaberoutine (INPUT) ********************************************
10260 '
10270 ' IN: Z=Zeile     OUT:          TMP: P
10280 ' IN: S=Spalte    OUT: OUTTEXT$ TMP: VALID (1=true 0=false)
10290 ' IN: LNG= Länge  OUT: CKEY     TMP: BADINPUT
10300 ' IN: TYP=Typ     OUT: FKEY
10310 ' IN: VNFLAG      OUT: ASCC
10320 ' IN: INTEXT$
10330 ' IN: FKEYMASK$
10340 ' IN: MUSS
10350 ' IN: CKEYMASK$
10360 '
10370 BADINPUT=0
10380 P=LEN(INTEXT$)
10390 PRINT FNCUR$(Z,S);HIGHON$;INTEXT$;HIGHOFF$;STRING$(LNG-P,".")
10400 OUTTEXT$=INTEXT$
10410 PRINT CURON$;
10420 CKEY=0:FKEY=0
10430 PRINT FNCUR$(Z,S+P);
10440 A$=INPUT$(1):ASCC=ASC(A$)
10450 IF NOT BADINPUT THEN 10460
10451   GOSUB 13030                                          'Nachricht löschen
10452   PRINT FNCUR$(Z,S+P);
10453   BADINPUT=0
10460 ON TYP GOSUB 12340,12410,12470,12530,12590                    'Type check
10470 IF VALID=0 THEN 10530
10480   IF P>=LNG THEN 10440                                   'Valid character
10490   PRINT HIGHON$;A$;HIGHOFF$                              'LW NO semicolon
10500   OUTTEXT$=OUTTEXT$+A$
10510   P=P+1
10520   GOTO 10430
10530 IF ASCC<>ASCBSP THEN 10590
10540   IF P<1 THEN 10440                                            'Backspace
10550   PRINT DEL$                                             'LW NO semicolon
10560   P=P-1
10570   OUTTEXT$=LEFT$(OUTTEXT$,P)
10580   GOTO 10430
10590 IF ASCC=ASCESC THEN 10370                                         'Escape
10600 IF ASCC=ASCRET THEN 10730
10610 IF ASCC=ASCCUP THEN CKEY=CKEYUP:IF INSTR(CKEYMASK$,CHR$(CKEY+ASC0))>0 THEN 10730
10620 IF ASCC=ASCCDN THEN CKEY=CKEYDN:IF INSTR(CKEYMASK$,CHR$(CKEY+ASC0))>0 THEN 10730
10630 IF VNFLAG=0 THEN 10660
10640   IF ASCC=ASCCLT THEN CKEY=CKEYLT:IF INSTR(CKEYMASK$,CHR$(CKEY+ASC0))>0 THEN 10730 
10650   IF ASCC=ASCCRT THEN CKEY=CKEYRT:IF INSTR(CKEYMASK$,CHR$(CKEY+ASC0))>0 THEN 10730 
10660 IF ASCC<ASCF1 OR ASCC>ASCF9 THEN 10690                            'F-Keys
10670   FKEY=ASCC-ASCF1+1
10680   IF INSTR(FKEYMASK$,CHR$(FKEY+ASC1-1))>0 THEN 10780
10690 N$="Ungültige Eingabe - Bitte wiederholen":GOSUB 12890         'Nachricht
10700 BADINPUT=1
10710 GOTO 10430
10720 '
10730 IF MUSS=0 OR LEN(OUTTEXT$)>0 THEN 10770
10740   N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 12890
10750   BADINPUT=1 
10760   GOTO 10430
10770 IF CKEY<>0 THEN GOSUB 10830                     'Block cursor auto-repeat
10780 PRINT CUROFF$                                            'LW NO semicolon
10790 RETURN 
10800 '
10810 ' *** Block cursor auto-repeat ********************************* REVIEWED
10820 '
10830 A$=INKEY$:IF A$>=CHR$(ASCCUP) AND A$<=CHR$(ASCCRT) THEN 10830
10840 RETURN
10850 '
10860 ' *** Debounce keyboard **************************************** REVIEWED
10870 '
10880 FOR K=1 TO 30:A$=INKEY$:NEXT K
10890 RETURN
10900 '
10910 ' *** Einlesen Stammdatenfelder-Definition ********************* REVIEWED
10920 '
10930 OPEN "I",#1,"B:FELD.DAT"                        
10940   INPUT #1,TMPFELDMAX                                             'ignore
10950   FOR I=1 TO FELDMAX
10960     INPUT #1,F$
10970     FENAM$(I)=LEFT$(F$,14)
10980     FEMUSS(I)=-(MID$(F$,13,1)="=")                      'TODO Verify this
10990     FECOL(I)=VAL(MID$(F$,15,2))
11000     FEROW(I)=VAL(MID$(F$,17,2))
11010     FELNG(I)=VAL(MID$(F$,19,2))
11020     FETYP(I)=VAL(RIGHT$(F$,1)) 
11030   NEXT
11040 CLOSE #1
11050 RETURN
11060 '
11070 ' *** Clear DAT$() ********************************************* REVIEWED
11080 '
11090 FOR K=1 TO FELDMAX:DAT$(K)="":NEXT K
11100 RETURN
11110 '
11120 ' *** Clear SUCH$()
11130 '
11140 FOR K=1 TO FELDMAX:SUCH$(K)="":NEXT K
11150 RETURN
11160 '
11170 ' *** Lesen MTGLMAX ******************************************** REVIEWED
11180 '
11190 ' OUT: MTGLMAX
11200 OPEN "I",#1,"B:MTGLZAHL.DAT"
11210   INPUT #1,MTGLMAX$
11220   MTGLMAX=VAL(MTGLMAX$)
11230 CLOSE #1
11240 RETURN
11250 '
11260 ' *** Schreiben MTGLMAX **************************************** REVIEWED
11270 '
11280 ' IN: MTGLMAX
11290 OPEN "O",#1,"B:MTGLZAHL.DAT"
11300   PRINT #1,STR$(MTGLMAX)
11310 CLOSE #1
11320 RETURN
11330 '
11340 ' *** Maske Feldnamen und Feldplatzhalter ausgeben
11350 '
11360 ' IN: FELDMAX
11370 ' IN: MTGLZAHL
11380 FOR K=1 TO FELDMAX
11390   Z=FEROW(K)
11400   S=FECOL(K)
11410   PRINT FNCUR$(Z,S);FENAM$(K)
11420   S=S+XOFF
11430   PRINT FNCUR$(Z,S);STRING$(FELNG(K),".")
11440 NEXT K
11450 RETURN
11460 '
11470 ' *** Maske Feldwerte ausgeben
11480 '
11490 ' IN: FELDMAX
11500 PRINT HIGHON$
11510 FOR K=1 TO FELDMAX
11520   Z=FEROW(K)
11530   S=FECOL(K)+XOFF
11540   PRINT FNCUR$(Z,S);DAT$(K)
11550 NEXT K
11560 PRINT HIGHOFF$
11570 RETURN
11580 '
11590 ' *** Maske Feldnamen, Eingabefelder und Filler ausgeben
11600 '
11610 ' IN: FELDMAX
11620 FOR K=1 TO FELDMAX
11630   Z=FEROW(K)
11640   S=FECOL(K)
11650   PRINT FNCUR$(Z,S);FENAM$(K)
11660   S=S+XOFF
11670   PRINT FNCUR$(Z,S);HIGHON$;DAT$(K);HIGHOFF$;
11680   PRINT STRING$(FELNG(K)-LEN(DAT$(K)),".")               'LW NO semicolon
11690 NEXT K
11700 RETURN
11710 '
11720 ' *** FIELDen der Stammsatz-Datei ****************************** REVIEWED
11730 '
11740 OPEN "R",#2,"B:MV.DAT",462
11750 FIELD #2,236 AS DD1$,225 AS DD2$
11760 RETURN
11770 '
11780 ' *** Einlesen der Felddaten von Disk ***********************************
11790 '
11800 GET #2,MTGLZAHL
11810 GOSUB 11090                                                 'Clear DAT$()
11820 I=1
11830 FOR K=1 TO 14
11840   LNG=FELNG(K)
11850   TMP$=MID$(DD1$,I,LNG):GOSUB 11960:DAT$(K)=TMP$
11860   I=I+LNG
11870 NEXT
11880 I=1
11890 FOR K=15 TO FELDMAX
11900   LNG=FELNG(K)
11910   TMP$=MID$(DD2$,I,LNG):GOSUB 11960:DAT$(K)=TMP$
11920   I=I+LNG
11930 NEXT
11940 RETURN
11950 '
11960 FOR KK=LEN(TMP$) TO 1 STEP -1
11970   IF MID$(TMP$,KK,1)<>" " THEN 11990
11980 NEXT KK
11990 TMP$=LEFT$(TMP$,KK)
12000 RETURN
12010 '
12020 ' *** Speichern der Felddaten auf Disk **********************************
12030 '
12040 ' IN: MTGLZAHL
12050 TMP$=""
12060 FOR K=1 TO 14
12070   LNG=FELNG(K)
12080   TMP$=TMP$+DAT$(K)+SPACE$(LNG-LEN(DAT$(K)))
12090 NEXT K
12100 LSET DD1$=TMP$
12110 TMP$=""
12120 FOR K=15 TO FELDMAX
12130   LNG=FELNG(K)
12140   TMP$=TMP$+DAT$(K)+SPACE$(LNG-LEN(DAT$(K)))
12150 NEXT K
12160 LSET DD2$=TMP$
12170 TMP$=""
12180 PUT #2,MTGLZAHL
12190 RETURN
12200 '
12210 ' *** Ja/Nein-Frage und F-Tastenleiste löschen ***************** REVIEWED
12220 '
12230 ' IN:  Q$ - Frage
12240 ' OUT: JA - (1=yes, 0=no)
12250 N$=Q$+" (J=Ja/N=Nein)":GOSUB 12890                             'Nachricht
12260 GOSUB 13470                                                'Delete F-keys
12270 ANSWER$=INPUT$(1)
12280 IF ANSWER$="J" OR ANSWER$="j" THEN JA=1:RETURN
12290 IF ANSWER$="N" OR ANSWER$="n" THEN JA=0:RETURN
12300 GOTO 12270
12310 '
12320 ' *** Testbereich 1 (Alpha) *********************************************
12330 '
12340 VALID=0
12350 IF ASCC>57 AND ASCC<127 THEN VALID=1:GOTO 12370
12360 IF ASCC>31 AND ASCC<48 THEN VALID=1
12370 RETURN
12380 '
12390 ' *** Testbereich 2 (Numerisch) *****************************************
12400 '
12410 VALID=0
12420 IF ASCC>47 AND ASCC<58 THEN VALID=1
12430 RETURN
12440 '
12450 ' *** Testbereich 3 (Alphanumerisch) ************************************
12460 '
12470 VALID=0
12480 IF ASCC>31 AND ASCC<127 THEN VALID=1
12490 RETURN
12500 '
12510 ' *** Testbereich 4 (Numerisch + "/" und ".") ***************************
12520 '
12530 VALID=0
12540 IF ASCC>45 AND ASCC<58 THEN VALID=1
12550 RETURN
12560 '
12570 ' *** Testbereich 5 (Numerisch + und ".") *******************************
12580 '
12590 VALID=0
12600 IF (ASCC>=48 AND ASCC<=57) OR ASCC=46 THEN VALID=1
12610 RETURN
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
13380 GOSUB 10880                                            'Debounce keyboard
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
13540 IF JA=1 THEN 19370                                          'Programmende
13550 GOSUB 13210                                    'F-Tastenleiste einblenden
13560 RETURN
13570 '
13580 ' *** Rückkehr ins Hauptmenü Frage (einfach) ******************* REVIEWED
13590 '
13600 ' OUT: JA (1=true, 0=false)
13610 Q$="Wollen Sie wirklich ins Hauptmenü?":GOSUB 12250        'Ja/Nein-Frage
13620 GOSUB 13030                                            'Nachricht löschen
13630 RETURN
13640 '
13650 ' *** Rückkehr aus Hilfe-Text? ********************************* REVIEWED
13660 '
13670 N$="Zurück? (J=Ja)":GOSUB 12890                                'Nachricht
13680 GOSUB 12260                                           'skip string concat
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
14070 GET #5,CPOINT:GOSUB 15100                                       'Einlesen
14080 BLATT=1
14090 GET #4,CPOINT                                              'Pointer lesen
14100 RETURN
14110 '
14120 ' **** SEITE RÜCKWÄRTS **************************************************
14130 '
14140 TMPOINT=VAL(POINT1$)
14150 IF TMPOINT<>0 THEN 14180
14160 N$="Erste Seite":GOSUB 12890                                   'Nachricht
14170 RETURN
14180 CPOINT=TMPOINT
14190 BLATT=BLATT-1
14200 GET #5,CPOINT:GOSUB 15100   
14210 GET #4,CPOINT:GOSUB 15260                             'Neue Pointer lesen
14220 HP=1:VP=1
14230 GOSUB 14470
14240 RETURN
14250 '
14260 ' **** SEITE VORWÄRTS  **************************************************
14270 '
14280 TMPOINT=VAL(POINT2$)
14290 IF TMPOINT=0 THEN 14360
14300 CPOINT=TMPOINT
14310 BLATT=BLATT+1
14320 GET #5,CPOINT:GOSUB 15100
14330 GET #4,CPOINT:GOSUB 15260
14340 HP=1:VP=1
14350 RETURN
14360 N$="Letzte Seite"
14370 IF INSTR(SPACE$(20),DAT$(26))=0 THEN 14400 
14380 GOSUB 12890                                                    'Nachricht
14390 RETURN
14400 Q$="Letzte Seite - Neue Seite anlegen?":GOSUB 12250        'Ja/Nein-Frage
14410 GOSUB 13030                                            'Nachricht löschen
14420 IF JA=0 THEN 14470
14430 GOSUB 14710
14440 BLATT=BLATT+1
14450 GOSUB 15260
14460 HP=1:VP=1
14470 GOSUB 13210                                    'F-Tastenleiste einblenden
14480 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=vorwärts";
14490 PRINT TAB(54);"<F9>=Blatt drucken";
14500 PRINT HOME$;" "
14510 RETURN
14520 '
14530 ' **** ERSTE SEITE EINES MITGLIEDS ANLEGEN ******************************
14540 '
14550 GET #3,1
14560 LSET POINT0$=STR$(VAL(POINT0$)+1)                                  'NDX+1
14570 PUT #3,1
14580 LSET POINT1$="0000"                                        'Page pointers
14590 LSET POINT2$="0000"
14600 PUT #4,VAL(POINT0$)
14610 LSET POINT0$=POINT0$                                          'First page
14620 PUT #3,MTGLZAHL+1
14630 LSET DDD1$=""                                                 'Datenblatt
14640 LSET DDD2$=""
14650 LSET DDD3$=""
14660 PUT #5,VAL(POINT0$)
14670 RETURN
14680 '
14690 ' **** ALLGEMEIN EINE NEUE SEITE ANLEGEN ********************************
14700 '
14710 GET #3,1                                                           'NDX+1
14720 LSET POINT0$=STR$(VAL(POINT0$)+1)
14730 PUT #3,1:TMPOINT$=POINT0$    'Vektoren der bisher letzten Seite verbiegen
14740 LSET POINT2$=POINT0$                                'Verbiege Vektoren...
14750 PUT #4,CPOINT                                         '...der alten Seite
14760 LSET POINT1$=STR$(CPOINT)                             '...der neuen Seite
14770 LSET POINT2$="0000"                                                '.. ""
14780 CPOINT=VAL(TMPOINT$)                                       'Nr neue Seite
14790 PUT #4,CPOINT
14800 LSET DDD1$=""                                           'Write empty page
14810 LSET DDD2$=""
14820 LSET DDD3$="" 
14830 PUT #5,CPOINT
14840 DATEN$=SPACE$(20)
14850 FOR II=1 TO 26
14860   DAT$(II)=DATEN$
14870 NEXT
14880 RETURN
14890 '
14900 ' **** SEITE SPEICHERN **************************************************
14910 '
14920 DATEN$=""
14930 FOR II=1 TO 12
14940   DATEN$=DATEN$+DAT$(II)
14950 NEXT
14960 LSET DDD1$=DATEN$:DATEN$=""
14970 FOR II=13 TO 24
14980   DATEN$=DATEN$+DAT$(II)
14990 NEXT
15000 LSET DDD2$=DATEN$:DATEN$=""
15010 FOR II=25 TO 26
15020   DATEN$=DATEN$+DAT$(II)
15030 NEXT 
15040 LSET DDD3$=DATEN$:DATEN$=""
15050 PUT #5,CPOINT
15060 RETURN
15070 '
15080 ' **** HACK ! (ZERSTÜCKELN DER DATEN-STRINGS) ***************************
15090 '
15100 FOR II=1 TO 12
15110   DAT$(II)=MID$(DDD1$,(II-1)*20+1,20)
15120 NEXT
15130 FOR II=13 TO 24
15140   DAT$(II)=MID$(DDD2$,(II-13)*20+1,20)
15150 NEXT 
15160 FOR II=25 TO 26
15170   DAT$(II)=MID$(DDD3$,(II-25)*20+1,20)
15180 NEXT 
15190 RETURN
15200 '
15210 ' **** UPRO DATENAUSGABE VN  ********************************************
15220 '
15230 ' IN: MTGLZAHL
15240 ' IN: NAMM$
15250 ' IN: BLATT
15260 PRINT DELHIGH$;                                          'TODO Necessary?
15270 PRINT HIGHON$
15280 PRINT FNCUR$(5,16);MTGLZAHL
15290 PRINT FNCUR$(5,29);NAMM$
15300 PRINT FNCUR$(3,8);BLATT
15310 FOR K=1 TO 26
15320   HOR=6-39*(K>13)
15330   VER=6+K+13*(K>13)
15340   FOR J=1 TO 3                                                 'Von-Datum
15350     PRINT FNCUR$(VER,HOR+(J-1)*3);MID$(DAT$(K),-1+J*2,2)
15360   NEXT
15370   FOR J=1 TO 3                                                 'Bis-Datum
15380     PRINT FNCUR$(VER,HOR+13+(J-1)*3);MID$(DAT$(K),5+J*2,2)
15390   NEXT
15400   PRINT FNCUR$(VER,HOR+26);MID$(DAT$(K),13,8)                        'Amt
15410 NEXT
15420 PRINT HIGHOFF$
15430 RETURN
15440 '
15450 ' *** Verwendungsnachweis drucken ***************************************
15460 '
15470 ' IN: BLATT
15480 ' IN: MITGLZAHL
15490 ' IN: NAMM$
15500 Q$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 12250 'Ja/Nein-Frage TODO Back to main code?
15510 IF JA=0 THEN 15750                                  'LW TODO Remove this!
15520 '
15530 RETURN                                         '*** TMP Print Disable ***
15540 LIND$=SPACE$(2)
15550 '
15560 LPRINT STRICH$
15570 LPRINT
15580 LPRINT LIND$;"Blatt:";BLATT
15590 LPRINT
15600 LPRINT LIND$;"Mitglieds-Nr.:";MTGLZAHL;TAB(24);"Name:";NAMM$
15610 LPRINT
15620 FOR K=1 TO 13
15630   KK=K:GOSUB 15720
15640   LPRINT LIND$;"Von:";VON$;" Bis:";BIS$;" Amt:";AMT$;
15650   KK=K+13:GOSUB 15720
15660   LPRINT " Von:";VON$;" Bis:";BIS$;" Amt:";AMT$
15670 NEXT 
15680 LPRINT
15690 LPRINT STRICH$
15700 RETURN
15710 'Format: VVVVVV BBBBBB AAAAAAAA
15720 VON$=LEFT$(DAT$(KK),2)+"."+MID$(DAT$(KK),3,2)+"."+MID$(DAT$(KK),5,2)
15730 BIS$=MID$(DAT$(KK),7,2)+"."+MID$(DAT$(KK),9,2)+"."+MID$(DAT$(KK),11,2)
15740 AMT$=RIGHT$(DAT$(KK),8)
15750 RETURN
15760 '
15770 ' ### Unterprogramme "Mitgliedsliste drucken" ###########################
15780 '
15790 '
15800 ' *** Ist der Drucker eingeschaltet? Frage *******************************
15810 '
15820 ' OUT: JA (1=Ja, 0=Nein)
15830 Q$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 12250 'Ja/Nein-Frage
15840 GOSUB 13030                                            'Nachricht löschen
15850 RETURN
15860 '
15870 ' *** Mitgliedsliste drucken (Kopf)
15880 '
15890 ' IN: DATUM$
15900 LPRINT TAB(16);"MITGLIEDSLISTE 25*******************************"
15910 LPRINT 
15920 LPRINT TAB(32);"Stand:";DATUM$
15930 LPRINT
15940 LPRINT TAB(3);"Mitgl."; 
15950 LPRINT TAB(10);"Name";
15960 LPRINT TAB(36);"Vorname, Anschrift";
15970 LPRINT TAB(63);"Status";
15980 LPRINT TAB(71);"Amt"
15990 RETURN
16000 '
16010 ' *** Mitgliedsliste drucken (Mitglied)
16020 '
16030 ' IN:  MTGLZAHL
16040 AMT$=""
16050 IN=INSTR(DAT$(6),"22**"):IF IN<>0 THEN AMT$=MID$(DAT$(6),IN,6):GOTO 16090
16060 IN=INSTR(DAT$(6),"23**"):IF IN<>0 THEN AMT$=MID$(DAT$(6),IN,5):GOTO 16090
16070 IN=INSTR(DAT$(6),"24**"):IF IN<>0 THEN AMT$=MID$(DAT$(6),IN,3)
16080 '
16090 LPRINT
16100 LPRINT TAB(5);MTGLZAHL;
16110 LPRINT TAB(10);DAT$(1);                                         'Nachname
16120 LPRINT TAB(36);DAT$(2);                                          'Vorname
16130 LPRINT TAB(65);"(";LEFT$(DAT$(5),1);")";                          'Status
16140 LPRINT TAB(71);AMT$                                                  'Amt
16150 LPRINT TAB(36);DAT$(10)                                           'Straße
16160 LPRINT TAB(36);DAT$(11);                                    'Postleitzahl
16170 LPRINT TAB(41);DAT$(12)                                              'Ort
16180 LPRINT TAB(36);"Tel.:";DAT$(13)                                  'Telefon
16190 LPRINT
16200 RETURN
16210 '
16220 ' *** Druck abbrechen? Frage
16230 '
16240 ' OUT: JA (1=true,0=false)
16250 JA=0 
16260 IF INKEY$="" THEN 16300
16270 Q$="Drucken abbrechen?":GOSUB 12250                        'Ja/Nein-Frage
16280 IF JA=1 THEN 16300
16290 N$="Mitgliedsliste wird gedruckt... Abbrechen mit Taste.":GOSUB 12890 'Nachricht
16300 RETURN
16310 '
16320 ' *** Adressaufkleber drucken
16330 '                                                           Ja/Nein-Frage
16340 LIND$=SPACE$(3)
16350 '
16360 LPRINT LIND$;DAT$(7)                                              'Anrede
16370 LPRINT LIND$;LEFT$(DAT$(2),INSTR(DAT$(2)," "));DAT$(1)  'Vorname Nachname
16380 LPRINT
16390 LPRINT LIND$;DAT$(10)                                  'Straße Hausnummer
16400 LPRINT
16410 LPRINT LIND$;DAT$(11);" ";DAT$(12)                      'Postleitzahl Ort
16420 LPRINT
16430 LPRINT
16440 LPRINT
16450 LPRINT
16460 LPRINT
16470 LPRINT
16480 RETURN
16490 '
16500 ' *** Stammsatz drucken (Bildschirmformat)
16510 '
16520 RETURN                                         '*** TMP Print Disable ***
16530 XCOL1=8
16540 XCOL2=43
16550 PRINT STRICH$
16560 PRINT
16570 PRINT TAB(31);"Mitgliedsnummer";MTGLZAHL
16580 PRINT
16590 RESTORE 16650
16600 FOR K=1 TO 7:READ A,B
16610   IF A<>0 THEN PRINT TAB(XCOL1);FENAM$(A);DAT$(A);
16620   IF B<>0 THEN PRINT TAB(XCOL2);FENAM$(B);DAT$(B);
16630   PRINT
16640 NEXT K
16650 DATA 1,0,2,15,3,16,4,0,5,17,6,18,0,19
16660 FOR K=7 TO 14
16670   PRINT TAB(XCOL1);FENAM$(K);DAT$(K);
16680   PRINT TAB(XCOL2);FENAM$(K+13);DAT$(K+13);
16690 NEXT K
16700 PRINT
16710 PRINT
16720 PRINT
16730 PRINT STRICH$
16740 RETURN
16750 '
16760 ' *** Mitgliedsvermerke drucken
16770 '
16780 ' IN: NAMM$ =
16790 RETURN                                         '*** TMP Print Disable ***
16800 LIND$=SPACE$(9)
16810 SLOT1$="___"
16820 SLOT2$="___ / ___"
16830 SLOT3$="___ / ___ / ___"
16840 '
16850 LPRINT TAB(10);"Mitgliedsnr.: ";SLOT1$;
16860 LPRINT TAB(28);"Name: ";NAMM$;
16870 LPRINT TAB(65);"Datum: ";SLOT3$
16880 LPRINT
16890 LPRINT LIND$;"26*************************** ";SLOT3$
16900 LRPINT
16910 LPRINT LIND$;"27*************************** ";SLOT3$
16920 LRPINT
16930 LPRINT LIND$;"28*************************** ";SLOT3$
16940 LRPINT
16950 LPRINT LIND$;"29*************************** ";SLOT3$
16960 LRPINT
16970 LPRINT LIND$;"30*************************** ";SLOT2$
16980 LRPINT
16990 LPRINT LIND$;"31*************************** ";SLOT3$
17000 LRPINT
17010 LPRINT LIND$;"32*************************** ";SLOT3$
17020 LRPINT
17030 LPRINT LIND$;"33*************************** ";SLOT1$
17040 LRPINT
17050 LPRINT LIND$;"34*************************** ";SLOT3$
17060 LRPINT
17070 LPRINT LIND$;"35*************************** ";SLOT3$
17080 LRPINT
17090 LPRINT LIND$;"36*************************** ";SLOT3$
17100 LRPINT
17110 LPRINT LIND$;"37*************************** ";SLOT3$
17120 LRPINT
17130 LPRINT LIND$;"38*************************** ";SLOT3$
17140 RETURN
17150 '
17160 ' ### Hilfe-Texte #######################################################
17170 '
17180 ' *** Hilfe-Text "Mitgliederverwaltung" 
17190 '
17200 MT$="HILFE":GOSUB 13100                                      'Basis Maske
17210 PRINT HOME$
17220 PRINT IN$;"Die MITGLIEDERVERWALTUNG bietet folgende Funktionen an :"
17230 PRINT 
17240 PRINT IN$;"1 - STAMMSATZPFLEGE"
17250 PRINT IN$;"    Bearbeiten von Mitgliedsdaten gemäß dem Personalstamm-"
17260 PRINT IN$;"    blatt" 
17270 PRINT
17280 PRINT IN$;"2 - VERWENDUNGSNACHWEIS"
17290 PRINT IN$;"    Bearbeiten von Mitgliedsdaten, die Ämter umd Amtsperioden"
17300 PRINT IN$;"    eines Mitglieds enthalten"
17310 PRINT
17320 PRINT IN$;"3 - MITGLIEDSLISTE DRUCKEN"
17330 PRINT IN$;"    Drucken von Mitgliedslisten nach verschiedenen Gesichts-"
17340 PRINT IN$;"    punkten"
17350 PRINT
17360 PRINT IN$;"Wählen Sie aus, indem Sie die zugehörige Zahlentaste drücken."
17370 GOSUB 13670                                      'Rückkehr aus Hilfe-Text
17380 RETURN
17390 '
17400 ' *** Hilfe-Text "Stammsatzpflege" 
17410 '
17420 MT$="HILFE":GOSUB 13100                                      'Basis Maske
17430 PRINT HOME$
17440 PRINT IN$;"Die STAMMSATZPFLEGE bietet folgende Funktionen an:"
17450 PRINT
17460 PRINT IN$;"1 - STAMMSATZ ANLEGEN"
17470 PRINT IN$;"    Erfassen der Daten eines neuen Mitglieds"
17480 PRINT
17490 PRINT IN$;"2 - STAMMSATZ ÄNDERN"
17500 PRINT IN$;"    Verändern eines bereits bestehenden Stammsatzes"
17510 PRINT
17520 PRINT IN$;"3 - STAMMSATZ LÖSCHEN"
17530 PRINT IN$;"    Verändern des Mitgliedsstatus auf `ausgetreten`"
17540 PRINT
17550 PRINT IN$;"4 - STAMMSATZ DRUCKEN"
17560 PRINT IN$;"    Drucken eines kompletten Stammsatzes"
17570 PRINT
17580 PRINT IN$;"5 - STAMMSATZ SUCHEN"
17590 PRINT IN$;"    Suchen vom Stammsätzen nach bestimmten Gesichtspunkten"
17600 PRINT
17610 PRINT IN$;"Wählen Sie aus, indem Sie die zugehörige Zahlentaste drücken."
17620 GOSUB 13670                                      'Rückkehr aus Hilfe-Text
17630 RETURN
17640 '
17650 ' *** Hilfe-Text "Stammsatz anlegen"
17660 '
17670 SAVEMT$=MT$:MT$="HILFE":GOSUB 13100:MT$=SAVEMT$              'Basis Maske
17680 PRINT HOME$
17690 PRINT IN$;"Sie können jetzt Daten eines neuen Mitglieds eingeben, jedoch"
17700 PRINT IN$;"nicht die eines bestehenden Mitglieds ändern."
17710 PRINT
17720 PRINT IN$;"Die Mitgliedsnummer wird laufend vergeben."
17730 PRINT
17740 PRINT IN$;"Während der Dateneingebe können Sie mit der DELETE-Taste"
17750 PRINT IN$;"korrigieren. Ist ein Feld ausgefüllt worden, muß RETURN"
17760 PRINT IN$;"gedrückt werden, da sonst die Eingabe nicht gespeichert wird."
17770 PRINT IN$;"Mit den Cursortasten kann das Feld gewechselt werden."
17780 PRINT
17790 PRINT IN$;"`=>`-Felder müssen ausgefüllt werden."
17800 PRINT
17810 PRINT IN$;"Weitere Funktionstasten:"
17820 PRINT IN$;"<F7>  Diesen Stammsatz speichern"
17830 GOSUB 13670                                      'Rückkehr aus Hilfe-Text
17840 DATENAUSG=1                                                          '???
17850 RETURN
17860 '
17870 ' *** Hilfe-Text "Stammsatz ändern (I)"
17880 '
17890 MT$="HILFE":GOSUB 13100                                      'Basis Maske
17900 PRINT HOME$
17910 PRINT IN$;"Sie können jetzt einen bestehenden Stammsatz ändern."
17920 PRINT
17930 PRINT IN$;"Geben Sie die Mitgliedsnummer des Stammsatzes ein."
17940 PRINT IN$;"Bei Tippfehlern können Sie mit der DELETE-Taste korrigieren."
17950 GOSUB 13670                                      'Rückkehr aus Hilfe-Text
17960 RETURN
17970 '
17980 ' *** Hilfe-Text "Stammsatz ändern (II)"
17990 '
18000 SAVEMT$=MT$:MT$="HILFE":GOSUB 13100:MT$=SAVEMT$              'Basis Maske
18010 PRINT HOME$
18020 PRINT IN$;"Sie können jetzt einen bestehenden Stammsatz ändern."
18030 PRINT
18040 PRINT IN$;"Während der Dateneingabe können Sie mit der DELETE-Taste"
18050 PRINT IN$;"korrigieren. Ist ein Feld geändert worden, muß RETURN"
18060 PRINT IN$;"gedrückt werden, da sonst die Änderung nicht gespeichert wird."
18070 PRINT IN$;"Mit den Cursortasten kann das Feld gewechselt werden."
18080 PRINT
18090 PRINT IN$;"`=>`-Felder müssen ausgefüllt werden."
18100 PRINT
18110 PRINT IN$;"Weitere Funktionstasten:"
18120 PRINT IN$;"<F7>  Diesen Stammsatz speichern"
18130 GOSUB 13670                                      'Rückkehr aus Hilfe-Text
18140 GOSUB 13100                                                  'Basis Maske
18150 GOSUB 13210                                    'F-Tastenleiste einblenden
18160 RETURN
18170 '
18180 ' *** Hilfe-Text "Stammsatz löschen"
18190 '
18200 MT$="HILFE":GOSUB 13100                                      'Basis Maske
18210 PRINT HOME$
18220 PRINT IN$;"Sie können jetzt den Status eines Mitglieds auf `ausgetreten`"
18230 PRINT IN$;"ändern. Damit wird der Stammsatz beim Druck von Mitglieds-"
18240 PRINT IN$;"listen nicht berüchsichtigt."
18250 PRINT IN$;"Auf keinen Fall werden die Daten WIRKLICH gelöscht."
18260 PRINT
18270 PRINT IN$;"Geben Sie die Mitgliedsnummer des Stammsatzes ein."
18280 PRINT IN$;"Bei Tippfehlern können Sie mit der DELETE-Taste korrigieren."
18290 GOSUB 13670                                      'Rückkehr aus Hilfe-Text
18300 RETURN
18310 '
18320 ' *** Hilfe-Text "Stammsatz suchen"
18330 '
18340 IF DRUCKFLAG THEN 19010                                              '???
18350 SAVEMT$=MT$:MT$="HILFE":GOSUB 13100:MT$=SAVEMT$              'Basis Maske
18360 PRINT HOME$
18370 PRINT IN$;"Sie können jetzt Stammsätze suchen."
18380 PRINT
18390 PRINT IN$;"1 - Suchen nach Nummern"
18400 PRINT IN$;"    Geben Sie ein Intervall von Mitgliedsnummern ein."
18410 PRINT IN$;"    Es werden alle Stammsätze ausgegeben, deren Nummern im"
18420 PRINT IN$;"    Intervall liegen."
18430 PRINT
18440 PRINT IN$;"2 - Suchen nach Maske"
18450 PRINT IN$;"    Sie füllen ein Formular aus. Diejenigen Stammsätze, die"
18460 PRINT IN$;"    mit dem Formular übereinstimmen, werden ausgegeben."
18470 PRINT IN$;"    Leere Felder bezeichnen beliebige Stammsatzdaten."
18480 PRINT IN$;"    Ein * zu Beginn des Feldes bedeutet: Das folgende Wort"
18490 PRINT IN$;"    darf sich an beliebiger Stelle des Feldes befinden."
18500 PRINT IN$;"    Ein oder mehrere ? zu Beginn des Feldes stellen Platz-"
18510 PRINT IN$;"    halter für beliebige Zeichen dar."
18520 PRINT
18530 PRINT IN$;"Wählen Sie aus, indem Sie die zugehörige Zahlentaste drücken."
18540 GOSUB 13670                                      'Rückkehr aus Hilfe-Text
18550 RETURN
18560 ' 
18570 ' *** Hilfe-Text für "Stammsatz drucken"
18580 '
18590 SAVEMT$=MT$:MT$="HILFE":GOSUB 13100:MT$=SAVEMT$              'Basis Maske
18600 PRINT HOME$
18610 PRINT IN$;"Sie können jetzt einen kompletten Stammsatz drucken, so, wie"
18620 PRINT IN$;"er auf dem Bildschirm steht."
18630 PRINT
18640 PRINT IN$;"Geben Sie die Mitgliedsnummer des Stammsatzes ein."
18650 PRINT IN$;"Bei Tippfehlern können Sie mit der DELETE-Taste korrigieren."
18660 GOSUB 13670                                      'Rückkehr aus Hilfe-Text
18670 RETURN
18680 '
18690 ' *** Hilfe-Text "Verwendungsnachweis (I)"
18700 '
18710 MT$="HILFE":GOSUB 13100                                      'Basis Maske
18720 PRINT HOME$
18730 PRINT IN$;"Sie können jetzt einen Verwendungsnachweis ändern."
18740 PRINT
18750 PRINT IN$;"Geben Sie die Mitgliedsnummer des Verwendungsnachweises ein."
18760 PRINT IN$;"Bei Tippfehlern können Sie mit der DELETE-Taste korrigieren."
18770 GOSUB 13670                                      'Rückkehr aus Hilfe-Text
18780 RETURN
18790 '
18800 ' *** Hilfe-Text "Verwendungsnachweis (II)"
18810 '
18820 SAVEMT$=MT$:MT$="HILFE":GOSUB 13100:MT$=SAVEMT$              'Basis Maske
18830 PRINT HOME$
18840 PRINT IN$;"Sie können jetzt einen bestehenden Verwendungsnachweis ändern."
18850 PRINT
18860 PRINT IN$;"Während der Dateneingabe können Sie mit der DELETE-Taste"
18870 PRINT IN$;"korrigieren. Ist ein Feld ausgefüllt worden, muß RETURN"
18880 PRINT IN$;"gedrückt werden, da sonst die Änderung nicht gespeichert"
18890 PRINT IN$;"wird. Mit den Cursortasten kann das Feld gewechselt werden."
18900 PRINT
18910 PRINT IN$;"Weitere Funktionstasten:"
18920 PRINT IN$;"<F5>  Zurückblättern (ohne die jetzige Seite zu speichern)"
18930 PRINT IN$;"<F6>  Vorblättern (ohne die jetzige Seite zu speichern)"
18940 PRINT IN$;"<F7>  Aktuelle Seite speichern"
18950 PRINT IN$;"<F9>  Aktuelle Seite drucken"
18960 GOSUB 13670                                      'Rückkehr aus Hilfe-Text
18970 RETURN
18980 '
18990 ' *** Hilfe-Text "Mitgliedsliste drucken"
19000 '
19010 SAVEMT$=MT$:MT$="HILFE":GOSUB 13100:MT$=SAVEMT$              'Basis Maske
19020 PRINT HOME$
19030 PRINT IN$;"Sie können jetzt eine Mitgliedsliste drucken." 
19040 PRINT
19050 PRINT IN$;"1 - Suchen & Drucken"
19060 PRINT IN$;"    Sie können Stammsätze nach verschiedenen Gesichtspunkten"
19070 PRINT IN$;"    suchen und drucken."
19080 PRINT
19090 PRINT IN$;"2 - Komplett drucken"
19100 PRINT IN$;"    Es wird eine komplette Mitgliedsliste ausgedruckt."
19110 PRINT IN$;"    Ausgetretene Mitglieder werden nicht berücksichtigt."
19120 PRINT
19130 PRINT IN$;"3 - Datenblatt drucken"
19140 PRINT IN$;"    Der Stammsatz und der Verwendungsnachweis werden im"
19150 PRINT IN$;"    Bildschirmformat mit einer Liste von Mitgliedsvermerken"
19160 PRINT IN$;"    ausgedruckt."
19170 PRINT
19180 PRINT IN$;"Wählen Sie aus, indem Sie die zugehörige Zahlentaste drücken."
19190 GOSUB 13670                                      'Rückkehr aus Hilfe-Text
19200 RETURN
19210 '
19220 ' *** Programmfehler ******************************************* REVIEWED
19230 '
19240 LIND$=SPACE$(15)
19250 GOSUB 13100                                                  'Basis Maske
19260 PRINT HOME$
19270 PRINT LIND$;"Ein Programmfehler ist aufgetreten."
19280 PRINT   
19290 PRINT LIND$;"- Notieren Sie folgende Fehlerinformation:"
19300 PRINT LIND$;"    ";HIGHON$;"ERL";HIGHOFF$;" (Error Line)";HIGHON$;ERL;HIGHOFF$
19310 PRINT LIND$;"    ";HIGHON$;"ERR";HIGHOFF$;" (Error Code)";HIGHON$;ERR;HIGHOFF$
19320 PRINT LIND$;"- Drücken Sie am Rechner den Knopf hinten rechts."
19330 PRINT LIND$;"19**************************"
19340 '
19350 ' *** Programmende ********************************************* REVIEWED
19360 '
19370 PRINT FNCUR$(22,1);HIGHOFF$;CURON$;
19380 RESET
19390 'LW RUN "DISKMENU.EXE"
19400 END                                                           '1596 lines
