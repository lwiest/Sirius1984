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
1731 ' TIP: Use semicolons with PRINT carefully -> display driver bug
1740 '
1750 ON ERROR GOTO 17890                                 'Enable error trapping
1760 '
1770 GOSUB 10050    'Tasten deaktivieren, ASCII Codes für Cursortasten vergeben
1780 PRINT CUROFF$;
1790 '
1800 ' *** Beginn des Mitgliederverwaltungsprogramms **************************
1810 '
1820 MT$="MITGLIEDERVERWALTUNG":MI$="G-01000":GOSUB 13100          'Basis Maske
1830 Q$="Bitte Daten-Diskette in Laufwerk B einlegen. Bereit?":GOSUB 12310
1840 IF JA=0 THEN 18020                                           'Programmende
1850 '
1860 N$="Einen Moment bitte...":GOSUB 12970    'Nachricht und F-keys ausblenden
1870 GOSUB 10990                          'Einlesen Stammdatenfelder-Definition
1880 GOSUB 11800                                   'Fielden der Stammsatz-Datei
1890 GOSUB 13770                       'Fielden der Verwendungsnachweis-Dateien
1900 GOSUB 11260:MTGLZAHL=MTGLMAX                            'Einlesen MTGLZAHL
1910 GOSUB 13030                                             'Nachricht löschen
1920 '
1930 ' ################# Mitgliederverwaltungsmenü (MVW-Menü) #################
1940 '
1950 MT$="MITGLIEDERVERWALTUNG":MI$="G-01000":GOSUB 13100          'Basis Maske
1960 FKEYS$="":GOSUB 13210                           'F-Tastenleiste einblenden
1970 VNFLAG=0                                                              '???
1980 PRINT FNCUR$(4,26);"1 - Stammsatzpflege"                         'Menütext
1990 PRINT FNCUR$(6,26);"2 - Verwendungsnachweis"
2000 PRINT FNCUR$(8,26);"3 - Mitgliedsliste drucken"
2010 FKEYMAX=4:NKEYMAX=3:GOSUB 13370                              'Menü-Eingabe
2020 DRUCKFLAG=0                                                           '???
2030 IF NKEY THEN ON NKEY GOTO 2100,5890,6710
2040 ON FKEY GOTO 2050,2060,1950,2060         'Hilfe, Zurück, Anfang, Hauptmenü
2050 GOSUB 17080:GOTO 1950                                      'F1->Hilfe-Text
2060 GOSUB 13530:GOTO 2010                           'F2->Zurück, F4->Hauptmenü
2070 '
2080 ' ########################### Stammsatzpflege ############################
2090 '
2100 IF DRUCKFLAG THEN RETURN                                              '???
2110 '
2120 ' *** Stammsatzpflege-Menü ***********************************************
2130 '
2140 MT$="STAMMSATZPFLEGE":MI$="G-01100":GOSUB 13100               'Basis Maske
2150 FKEYS$="":GOSUB 13210                           'F-Tastenleiste einblenden
2160 PRINT FNCUR$(4,29);"1 - Stammsatz anlegen"
2170 PRINT FNCUR$(6,29);"2 - Stammsatz ändern"
2180 PRINT FNCUR$(8,29);"3 - Stammsatz löschen"
2190 PRINT FNCUR$(10,29);"4 - Stammsatz drucken"
2200 PRINT FNCUR$(12,29);"5 - Stammsatz suchen"
2210 FKEYMAX=4:NKEYMAX=5:GOSUB 13370                              'Menü-Eingabe
2220 IF NKEY THEN ON NKEY GOTO 2290,2830,3450,5520,3880
2230 ON FKEY GOTO 2240,1950,1950,2250         'Hilfe, Zurück, Anfang, Hauptmenü
2240 GOSUB 17150:GOTO 2140                                      'F1->Hilfe-Text
2250 GOSUB 13530:GOTO 2210                                       'F4->Hauptmenü
2260 '
2270 ' *** Stammsatz erfassen von 02************ ******************************
2280 '
2290 MTGLZAHL=MTGLMAX+1
2300 F=1
2310 DATENAUSG=1                                                           '???
2320 MT$="STAMMSATZ ANLEGEN":MI$="G-01101":GOSUB 13100             'Basis Maske
2330 FKEYS$="<F7>=Speichern":GOSUB 13210             'F-Tastenleiste einblenden
2340 GOSUB 11150                                                  'Clear DAT$()
2350 GOSUB 12760                                           'Defaultwerte setzen
2360 PRINT FNCUR$(3,31);"Mitgliedsnummer";HIGHON$;MTGLZAHL;HIGHOFF$
2370 IF DATENAUSG=1 THEN GOSUB 11680:DATENAUSG=0     'Felder und Feldwerte '???
2380 Z=FEROW(F):S=FECOL(F)+XOFF:LNG=FELNG(F):TYP=FETYP(F):MUSS=FEMUSS(F)
2390 FKEYMASK$="12347":CKEYMASK$="12":INTEXT$=DAT$(F)
2400 GOSUB 10430                                                         'Input
2410 IF ASCC<>ASCRET THEN 2450                                 'Carriage return
2420   DAT$(F)=OUTTEXT$
2430   F=F+1:IF F>FELDMAX THEN F=1
2440   GOTO 2380
2450 IF CKEY=0 THEN 2500                                           'Cursor keys
2460   PRINT FNCUR$(Z,S);HIGHON$;DAT$(F);HIGHOFF$;STRING$(LNG-LEN(DAT$(F)),".")
2470   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FELDMAX
2480   IF CKEY=CKEYDN THEN F=F+1:IF F>FELDMAX THEN F=1
2490   GOTO 2380
2500 IF FKEY=1 THEN GOSUB 17220:GOTO 2320                       'F1->Hilfe-Text
2510 IF FKEY=2 THEN GOSUB 2600:GOTO 2140                       'Exit "Erfassen"
2520 IF FKEY=3 THEN GOSUB 2600:GOTO 1950                       'Exit "Erfassen"
2530 IF FKEY<>4 THEN 2580
2540   GOSUB 13600                                'Rückkehr ins Hauptmenü Frage
2550   IF JA=0 THEN GOSUB 13210:GOTO 2400                    'F-Keys einblenden
2560   GOSUB 2600                                              'Exit "Erfassen"
2570   GOTO 18020                                                 'Programmende
2580 IF FKEY=7 THEN GOSUB 2740:GOTO 2290
2590 '
2600 ' TMPF=1                                                              '???
2610 ' IF INSTR(SPACE$(25),DAT$(TMPF))=0 THEN 2670
2620 '   N$="DEBUG: DAT$() with 25 spaces!":GOSUB 12510              'Nachricht
2630 '   TMPF=TMPF+1:IF TMPF<(FELDMAX+1) THEN 2620
2640 '   MTGLZAHL=MTGLZAHL-1
2650 '   GOTO 2750
2660 '
2670 GOSUB 10940                                             'Debounce keyboard
2680 Q$="Wollen Sie vorher den Stammsatz speichern?":GOSUB 12310 'Ja/Nein-Frage
2690 IF JA=1 THEN 2740                                        'Nachricht und...
2700   N$="Stammsatz wird nicht gespeichert.":GOSUB 12970    '...F-Keys löschen
2710   MTGLZAHL=MTGLZAHL-1
2720   GOTO 2780
2730 '                                                            'Nachricht...
2740 N$="Stammsatz wird gespeichert...":GOSUB 12970      '...und F-Keys löschen
2750 GOSUB 12110                                       'Speichern der Felddaten
2760 GOSUB 13880                                        'Erste VN-Seite anlegen
2770 MTGLMAX=MTGLZAHL:GOSUB 11350                            'Schreiben MTGLMAX
2780 GOSUB 13030                                             'Nachricht löschen
2790 RETURN
2800 ' 
2810 ' *** Stammsatz ändern von 03************ ********************************
2820 '
2830 MT$="STAMMSATZ ÄNDERN":MI$="G-01102":GOSUB 13100              'Basis Maske
2840 FKEYS$="":GOSUB 13210                           'F-Tastenleiste einblenden
2850 PRINT FNCUR$(4,24)"Bitte geben Sie die Mitgliedsnummer" 
2860 PRINT FNCUR$(5,25)"des zu ändernden Stammsatzes ein."
2870 PRINT FNCUR$(6,23);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
2880 PRINT FNCUR$(7,24)"Beenden Sie die Eingabe mit RETURN."
2890 PRINT FNCUR$(9,39)"=>..."
2900 BADINPUT=0
2910 Z=9:S=41:LNG=3:TYP=2:MUSS=1
2920 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
2930 GOSUB 10440                                                       'Input++
2940 IF FKEY=1 THEN GOSUB 17300:GOTO 2830                               'F-Keys
2950 IF FKEY=2 THEN 2140
2960 IF FKEY=3 THEN 1950
2970 IF FKEY=4 THEN GOSUB 13530:GOTO 2930         'Rückkehr ins Hauptmenü-Frage
2980 '
2990 M=VAL(OUTTEXT$)
3000 IF M>=1 AND M<=MTGLMAX THEN 3050
3010   N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12890       'Nachricht
3020   BADINPUT=1
3030   INTEXT$=OUTTEXT$                                                    '???
3040   GOTO 2910
3050 MTGLZAHL=M
3060 N$="Stammsatz wird gelesen...":GOSUB 12970   'Nachricht und F-Keys löschen
3070 GOSUB 11860                                                   'Daten lesen
3080 GOSUB 13030                                             'Nachricht löschen
3090 '
3100 GOSUB 13100                                                   'Basis Maske
3110 FKEYS$="<F7>=Speichern":GOSUB 13210             'F-Tastenleiste einblenden
3120 PRINT FNCUR$(3,31);"Mitgliedsnummer";HIGHON$;MTGLZAHL;HIGHOFF$
3130 GOSUB 11680                                 'Felder und Feldwerte ausgeben
3140 F=1
3150 Z=FEROW(F):S=FECOL(F)+XOFF:LNG=FELNG(F):TYP=FETYP(F):MUSS=FEMUSS(F)
3160 FKEYMASK$="12347":CKEYMASK$="12":INTEXT$=DAT$(F)
3170 GOSUB 10430                                                         'Input
3180 IF ASCC<>ASCRET THEN 3220
3190   DAT$(F)=OUTTEXT$
3200   F=F+1:IF F>FELDMAX THEN F=1
3210   GOTO 3150
3220 IF CKEY=0 THEN 3270
3230   PRINT FNCUR$(Z,S);HIGHON$;DAT$(F);HIGHOFF$;STRING$(LNG-LEN(DAT$(F)),".")
3240   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FELDMAX
3250   IF CKEY=CKEYDN THEN F=F+1:IF F>FELDMAX THEN F=1
3260   GOTO 3150
3270 IF FKEY=1 THEN GOSUB 17370:GOTO 3100
3280 IF FKEY=2 THEN GOSUB 3360:GOTO 2830               'Eingabe Mitgliedsnummer
3290 IF FKEY=3 THEN GOSUB 3360:GOTO 1950                              'MVW-Menü
3300 IF FKEY=7 THEN GOSUB 3380:GOTO 2830
3310 GOSUB 13600                             'F4-> Rückkehr ins Hauptmenü-Frage
3320 IF JA=0 THEN GOSUB 13210:GOTO 3170                      'F-Keys einblenden
3330 GOSUB 3360                                                  'Exit "Ändern'
3340 GOTO 18020                                                   'Programmende
3350 '
3360 Q$="Wollen Sie den Stammsatz speichern?":GOSUB 12310        'Ja/Nein-Frage
3370 IF JA=0 THEN RETURN
3380 N$="Stammsatz wird gespeichert...":GOSUB 12970 'Nachricht und F-Keys löschen
3390 GOSUB 12110                                               'Daten speichern
3400 GOSUB 13030                                             'Nachricht löschen
3410 RETURN
3420 '
3430 ' *** Stammsatz löschen von 04********** *********************************
3440 '
3450 MT$="STAMMSATZ LÖSCHEN":MI$="G-01103":GOSUB 13100             'Basis Maske
3460 FKEYS$="":GOSUB 13210                           'F-Tastenleiste einblenden
3470 PRINT FNCUR$(4,24);"Bitte geben Sie die Mitgliedsnummer"         'Menütext
3480 PRINT FNCUR$(5,25);"des zu löschenden Mitglieds ein."
3490 PRINT FNCUR$(6,23);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
3500 PRINT FNCUR$(7,15);"(Dabei wird nur der Status des Mitglieds verändert.)"
3510 PRINT FNCUR$(8,24);"Beenden Sie die Eingabe mit RETURN."
3520 PRINT FNCUR$(10,39);"=>..."
3530 BADINPUT=0
3540 Z=10:S=41:LNG=3:TYP=2:MUSS=1
3550 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
3560 GOSUB 10440                                                       'Input++
3570 IF FKEY=1 THEN GOSUB 17460:GOTO 3450
3580 IF FKEY=2 THEN 2140                                                'F-Keys
3590 IF FKEY=3 THEN 1950
3600 IF FKEY=4 THEN GOSUB 13530:GOTO 3560 'Rückkehr ins Hauptmenü-Frage 'F-keys einblenden
3610 '
3620 M=VAL(OUTTEXT$)
3630 IF M>=1 AND M<=MTGLMAX THEN 3680
3640   N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12890       'Nachricht
3650   BADINPUT=1
3660   INTEXT$=OUTTEXT$                                                    '???
3670   GOTO 3540
3680 MTGLZAHL=M
3690 N$="Stammsatz wird gelesen...":GOSUB 12970   'Nachricht und F-Keys löschen
3700 GOSUB 11860                                                   'Daten lesen
3710 GOSUB 13030                                             'Nachricht löschen
3720 '
3730 GOSUB 13100                                                   'Basis Maske
3740 PRINT FNCUR$(3,31);"Mitgliedsnummer";HIGHON$;MTGLZAHL;HIGHOFF$
3750 GOSUB 11680                                 'Felder und Feldwerte ausgeben
3760 Q$="Mitglied endgültig löschen?":GOSUB 12310                'Ja/Nein-Frage
3770 IF JA=0 THEN 3840
3780   DAT$(5)="ausgetreten"
3790   PRINT FNCUR$(FEROW(5),FECOL(5)+XOFF);HIGHON$;DAT$(5);HIGHOFF$;
3800   PRINT STRING$(FELNG(5)-LEN(DAT$(5)),".")
3810   N$="Stammsatz wird gespeichert...":GOSUB 12970 'Nachricht und F-Keys löschen
3820   GOSUB 12110                                             'Daten speichern
3830   GOSUB 13030                                           'Nachricht löschen
3840 GOTO 3450
3850 '
3860 ' *** Stammsatz suchen (und drucken) von 05*************** ***************
3870 '
3880 IF DRUCKFLAG THEN MT$="SUCHEN & DRUCKEN" ELSE MT$="STAMMSATZ SUCHEN"
3890 MI$="G-01105":GOSUB 13100          'TODO IF DRUCKFLAG THEN... 'Basis Maske
3900 FKEYS$="":GOSUB 13210                           'F-Tastenleiste einblenden
3910 IF DRUCKFLAG THEN TMP$="Drucken" ELSE TMP$="Suchen"
3920 PRINT FNCUR$(4,30)"1 - ";TMP$;" nach Nummern"
3930 PRINT FNCUR$(6,30)"2 - ";TMP$;" nach Maske"
3940 FKEYMAX=4:NKEYMAX=2:GOSUB 13370                              'Menü-Eingabe
3950 IF FKEY=1 THEN GOSUB 17530:GOTO 3880   'TODO IF DRUCKFLAG THEN ... 'F-Keys
3960 IF FKEY=2 THEN IF DRUCKFLAG THEN 6710 ELSE 2140
3970 IF FKEY=3 THEN 1950
3980 IF FKEY=4 THEN GOSUB 13530:GOTO 3940         'Rückkehr ins Hauptmenü-Frage
3990 ON NKEY GOTO 4010,4530 
4000 ' *** Suchen nach Nummern ************************************************
4010 NUMSUCH1=0
4020 NUMSUCH2=0
4030 TMP$=STR$(1):NUMSUCH$(1)=RIGHT$(TMP$,LEN(TMP$)-1)
4040 TMP$=STR$(MTGLMAX):NUMSUCH$(2)=RIGHT$(TMP$,LEN(TMP$)-1)
4050 GOSUB 13100                                                   'Basis Maske
4060 FKEYS$="<F5>=Suchen":IF DRUCKFLAG THEN FKEYS$="<F5>=Suchen & Drucken>"
4070 GOSUB 13210                                     'F-Tastenleiste einblenden
4080 TMP$="gesucht":IF DRUCKFLAG THEN TMP$="gesucht und gedruckt"     
4090 PRINT FNCUR$(4,25);"Gültige Mitgliedsnummern: 1 -";STR$(MTGLMAX)
4100 PRINT FNCUR$(6,13);"Ab welcher Mitgliedsnummer soll ";TMP$;" werden?     =>..."
4110 PRINT FNCUR$(8,13);"Bis zu welcher Mitgliedsnummer soll ";TMP$;" werden? =>..."
4120 PRINT FNCUR$(6,67);HIGHON$;NUMSUCH$(1);HIGHOFF$
4130 PRINT FNCUR$(8,67);HIGHON$;NUMSUCH$(2);HIGHOFF$
4140 F=1
4150 BADINPUT=0
4160 Z=NUMSUCHROW(F):S=67:LNG=3:TYP=2:MUSS=1
4170 FKEYMASK$="12345":CKEYMASK$="":INTEXT$=NUMSUCH$(F)
4180 GOSUB 10440                                                       'Input++
4190 IF FKEY=1 THEN GOSUB 17530:GOTO 4050
4200 IF FKEY=2 THEN 3880
4210 IF FKEY=3 THEN 1950
4220 IF FKEY=4 THEN GOSUB 13530:GOTO 4180         'Rückkehr ins Hauptmenü-Frage
4230 IF FKEY<>5 THEN 4280
4240   SEARCH=BYNUMS
4250   NUMSUCH1=VAL(NUMSUCH$(1))
4260   NUMSUCH2=VAL(NUMSUCH$(2))
4270   GOTO 4770                                                      '-> Suche
4280 M=VAL(OUTTEXT$)                                             'Handle RETURN
4290 IF M>=1 AND M<=MTGLMAX THEN 4330
4300   N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12890       'Nachricht
4310   GOSUB 4460
4320   GOTO 4180
4330 NUMSUCH$(F)=OUTTEXT$
4340 NUMSUCH1=VAL(NUMSUCH$(1))
4350 NUMSUCH2=VAL(NUMSUCH$(2))
4360 IF (NUMSUCH1<=NUMSUCH2) THEN 4400
4370   N$="Ungültiger Bereich":GOSUB 12890                           'Nachricht
4380   GOSUB 4460
4390   GOTO 4180
4400 F=F+1:IF F>NUMSUCHMAX THEN F=1
4410 FKEYMASK$="12345"
4420 FKEYS$="<F5>=Suchen":IF DRUCKFLAG THEN FKEYS$="<F5>=Suchen & Drucken>"
4430 GOSUB 13470:GOSUB 13210             'F-Tastenleiste löschen und einblenden
4440 GOTO 4160
4450 '
4460 BADINPUT=1
4470 INTEXT$=OUTTEXT$
4480 FKEYMASK$="1234"
4490 FKEYS$=""
4500 GOSUB 13470:GOSUB 13210             'F-Tastenleiste löschen und einblenden
4510 RETURN
4520 ' *** Suchen nach Maske **************************************************
4530 GOSUB 11200                                                 'Clear SUCH$()
4540 GOSUB 13100                                                   'Basis Maske
4550 FKEYS$="<F5>=Suchen":IF DRUCKFLAG THEN FKEYS$="<F5>=Suchen & Drucken>" 
4560 GOSUB 13210                                     'F-Tastenleiste einblenden
4570 GOSUB 11440                       'Feldnamen und Werteplatzhalter ausgeben
4580 F=1
4590 Z=FEROW(F):S=FECOL(F)+XOFF:LNG=FELNG(F):TYP=3
4600 FKEYMASK$="12345":CKEYMASK$="12":INTEXT$=""
4610 GOSUB 10430                                                         'Input
4620 IF FKEY=0 THEN 4680
4630   IF FKEY=1 THEN GOSUB 17530:GOTO 4540         'TODO IF DRUCKFLAG THEN ...
4640   IF FKEY=2 THEN 3880
4650   IF FKEY=3 THEN 1950
4660   IF FKEY=4 THEN GOSUB 13530:GOTO 4610       'Rückkehr ins Hauptmenü-Frage
4670   IF FKEY=5 THEN SEARCH=BYMASK:GOTO 4770                         '-> Suche
4680 IF CKEY=0 THEN 4730                                           'Cursor keys
4690   PRINT FNCUR$(Z,S);HIGHON$;SUCH$(F);HIGHOFF$;STRING$(LNG-LEN(SUCH$(F)),".")
4700   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FELDMAX
4710   IF CKEY=CKEYDN THEN F=F+1:IF F>FELDMAX THEN F=1
4720   GOTO 4590
4730 SUCH$(F)=OUTTEXT$                                           'Handle RETURN
4740 F=F+1:IF F>FELDMAX THEN F=1
4750 GOTO 4590
4760 ' *** Suche **************************************************************
4770 IF SEARCH=BYMASK THEN MTGLZAHL=1 ELSE MTGLZAHL=NUMSUCH1
4780 IF DRUCKFLAG THEN GOSUB 15830               'Mitgliedsliste drucken (Kopf)
4790 GOSUB 13100                                                   'Basis Maske
4800 PRINT FNCUR$(3,31);"Mitgliedsnummer"
4810 GOSUB 11440                       'Feldnamen und Werteplatzhalter ausgeben
4820 IF DRUCKFLAG THEN 4870                       '-> Suche Schleifen-End-Check
4830   TMP$="<F6>=Suchen vorwärts <F7>=Adressaufkleber"
4840   IF SEARCH=BYMASK THEN FKEYS$=TMP$ ELSE FKEYS$="<F5>=Suchen rückwärts "+TMP$
4850   GOSUB 13210                                   'F-Tastenleiste einblenden
4860 '
4870 IF SEARCH=BYMASK AND MTGLZAHL<=MTGLMAX THEN 4950
4880 IF SEARCH=BYNUMS AND MTGLZAHL<=NUMSUCH2 THEN 4950
4890 '                                                            End of search
4900 IF DRUCKFLAG THEN 3880                         'Entry point Stammsatzsuche
4910 MTGLZAHL=MTGLZAHL-1                                                   '???
4920 N$="Keine weiteren Daten vorhanden. Mit <F2> neue Suche.":GOSUB 12890 'Nachricht
4930 GOTO 5210                                                  '->Menü-Eingabe
4940 '
4950 GOSUB 11860                          'Daten lesen TODO Nachricht ausgeben?
4960 IF SEARCH=BYNUMS THEN 5100                          '-> Feldwerte ausgeben
4970   FOR K=1 TO FELDMAX                                    'Daten durchsuchen
4980     IF LEFT$(SUCH$(K),1)<>"*" THEN 5020
4990     IF INSTR(DAT$(K),RIGHT$(SUCH$(K),LEN(SUCH$(K))-1))>0 THEN 5080 'NEXT K
5000       MTGLZAHL=MTGLZAHL+1
5010       GOTO 4870                                 '-> Suchschleife-End-Check
5020     I=1
5030     IF MID$(SUCH$(K),I,1)="?" THEN I=I+1:GOTO 5030
5040     I=I-1
5050     IF INSTR(DAT$(K),RIGHT$(SUCH$(K),LEN(SUCH$(K))-I))=I+1 THEN 5080 'NEXT K
5060       MTGLZAHL=MTGLZAHL+1
5070       GOTO 4870                                 '-> Suchschleife-End-Check
5080   NEXT K
5090 '
5100 GOSUB 11560                                            'Feldwerte ausgeben
5110 PRINT FNCUR$(3,46);HIGHON$;STR$(MTGLZAHL);HIGHOFF$ 
5120 IF DRUCKFLAG=0 THEN 5210
5130   GOSUB 15980                           'Mitgliedsliste drucken (Mitglied)
5140   IF ISBRKPRNT THEN 5180
5150     N$="":GOSUB 12890                                   'Nachricht löschen
5160     MTGLZAHL=MTGLZAHL+1
5170     GOTO 4870                                   'Suche Schleifen-End-Check
5180   ISBRKPRNT=0
5190   N$="":GOSUB 12890                                     'Nachricht löschen
5200   GOTO 3880                                   'Stammsatz suche start entry
5210 FKEMAX=7:NKEYMAX=0:GOSUB 13370      'Menü-Eingabe 'TODO Nachricht löschen?
5220 ON FKEY GOTO 5240,5310,1950,5330,5360,5420,5450
5230 '
5240 GOSUB 17530                                                    '<F1> Hilfe
5250 GOSUB 13100                                                   'Basis Maske
5260 PRINT FNCUR$(3,30);"Mitgliedsnummer"
5270 GOSUB 11440                       'Feldnamen und Werteplatzhalter ausgeben
5280 IF NOT DRUCKFLAG THEN GOSUB 13210               'F-Tastenleiste einblenden
5290 GOTO 5100                                '-> Feldwerte (MTGLZAHL) ausgeben
5300 '
5310 IF SEARCH=BYNUMS THEN 4010 ELSE 4530                          '<F2> Zurück
5320 '
5330 GOSUB 13530                             '<F4> Rückkehr ins Hauptmenü-Frage
5340 GOTO 5100                                '-> Feldwerte (MTGLZAHL) ausgeben
5350 '
5360 IF SEARCH=BYMASK OR MTGLZAHL<=NUMSUCH1 THEN 5390                     '<F5>
5370   MTGLZAHL=MTGLZAHL-1
5380   GOTO 4950                                                   'Daten lesen
5390 N$="Erster Suchtreffer":GOSUB 12890                    'Nachricht ausgeben
5400 GOTO 5210                                                    'Neue Eingabe
5410 '
5420 MTGLZAHL=MTGLZAHL+1                                                  '<F6>
5430 GOTO 4870                                          'Suchschleife-End-Check
5440 '
5450 GOSUB 16220                    '<F7> Auf Nachfrage Adressaufkleber drucken
5460 GOSUB 13210                                             'F-keys einblenden
5470 N$="":GOSUB 12890                                       'Nachricht löschen
5480 GOTO 5210                                                    'Neue Eingabe
5490 '
5500 ' *** Stammsatz drucken von 02************ & 04********** ****************
5510 '
5520 MT$="STAMMSATZ DRUCKEN":MI$="G-01104":GOSUB 13100             'Basis Maske
5530 FKEYS$="":GOSUB 13210                           'F-Tastenleiste einblenden
5540 PRINT FNCUR$(4,24)"Bitte geben Sie die Mitgliedsnummer"
5550 PRINT FNCUR$(5,23)"des auszudruckenden Stammsatzes ein."
5560 PRINT FNCUR$(6,23);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
5570 PRINT FNCUR$(7,24)"Beenden Sie die Eingabe mit RETURN."
5580 PRINT FNCUR$(9,39);"=>..."
5590 BADINPUT=0
5600 Z=9:S=41:LNG=3:TYP=2:MUSS=1
5610 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
5620 GOSUB 10440                                                       'Input++
5630 IF FKEY=1 THEN GOSUB 17610:GOTO 5520                               'F-Keys
5640 IF FKEY=2 THEN 2140
5650 IF FKEY=3 THEN 1950
5660 IF FKEY=4 THEN GOSUB 13530:GOTO 5620         'Rückkehr ins Hauptmenü-Frage
5670 '
5680 M=VAL(OUTTEXT$)
5690   IF M>=1 AND M<=MTGLMAX THEN 5740
5700   N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12890       'Nachricht
5710   BADINPUT=1
5720   INTEXT$=OUTTEXT$                                                    '???
5730   GOTO 3540
5740 MTGLZAHL=M
5750 N$="Stammsatz wird gelesen...":GOSUB 12970   'Nachricht und F-Keys löschen
5760 GOSUB 11860                                                   'Daten lesen
5770 GOSUB 13030                                             'Nachricht löschen
5780 '
5790 GOSUB 13100                                                   'Basis Maske
5800 PRINT FNCUR$(3,31);"Mitgliedsnummer";HIGHON$;MTGLZAHL;HIGHOFF$
5810 GOSUB 11680                                 'Felder und Feldwerte ausgeben
5820 Q$="Ist der Drucker eingeschaltet und druckbereit?":GOSUB 12310 'Ja/Nein-Frage
5830 IF JA=0 THEN 5850
5840 GOSUB 16420                          'Stammsatz drucken (Bildschirmformat)
5850 GOTO 5520
5860 '
5870 ' ######################### Verwendungsnachweis ##########################
5880 '
5890 GOSUB 11260                                              'Einlesen MTGLMAX
5900 VNFLAG=-1
5910 '
5920 ' Verwendungsnachweis-Menü ***********************************************
5930 '
5940 MT$="VERWENDUNGSNACHWEIS":MI$="G-01200":GOSUB 13100           'Basis Maske
5950 F=4                                                                'TODO ?
5960 FKEYS$="":GOSUB 13210                           'F-Tastenleiste einblenden
5970 PRINT CURON$;
5980 PRINT HIGHOFF$;
5990 PRINT FNCUR$(4,20)"   Bitte geben Sie die Mitgliedsnummer"
6000 PRINT FNCUR$(5,20)"des zu ändernden Verwendungsnachweises ein"
6010 PRINT FNCUR$(6,20)" und beenden Sie die Eingabe mit RETURN."
6020 PRINT FNCUR$(8,20)"                 =>..."
6030 Z=8:S=39:LNG=3:TYP=2
6040 GOSUB 10430                                                         'INPUT
6050 IF FKEY=1 THEN GOSUB 17680:GOTO 5940
6060 IF FKEY=2 OR FKEY=3 THEN GOSUB 11150:VNFLAG=0:GOTO 1950      'Clear DAT$()
6070 IF FKEY=4 THEN GOSUB 13530
6080 IF FKEY>4 THEN 6030
6090 IF CKEY<>0 THEN 6030
6100 IF LEN(OUTTEXT$)=0 THEN N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 12890:GOTO 6020 ELSE MITGLIED=VAL(OUTTEXT$):IF MITGLIED=0 THEN 6040 'Nachricht
6110 IF MITGLIED>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12890:PRINT HOME$;HIGHOFF$:GOTO 6020 'Nachricht
6120 MTGLZAHL=MITGLIED:MTGL$=STR$(MITGLIED)
6130 '
6140 ' *** Einlesen des Mitgliedsnamens ***************************************
6150 '
6160 PRINT FNCUR$(21,1);DELLINE$;
6170 GET #2,MITGLIED                                        'EINLESEN DATEN STS
6180 NNAM$=LEFT$(DD1$,25)                                 'NACHNAME RAUSFISCHEN
6190 VNAM$=MID$(DD1$,26,25)                               'VORNAME  RAUSFISCHEN
6200 '
6210 ' ** LEERZEICHEN HINTER NACHNAME ABSCHNEIDEN *****************************
6220 '
6230 FOR I=LEN(NNAM$) TO 1 STEP -1
6240   IF MID$(NNAM$,I,1)<>" " THEN NNAM$=LEFT$(NNAM$,I):I=1
6250 NEXT
6260 NAMM$=NNAM$+" "+VNAM$
6270 GOSUB 14050                                          'EINLESEN 1. VN-BLATT
6280 BLATT=1
6290 VP=1:HP=1                                                 'INITIALISIERUNG
6300 F=8
6310 GOSUB 13100                                                   'Basis Maske
6320 FKEYS$="<F7>=Speichern":GOSUB 13210             'F-Tastenleiste einblenden
6330 PRINT HIGHOFF$;
6340 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=Vorwärts";TAB(53);"<F9>=Blatt drucken";
6350 PRINT FNCUR$(3,2);"Blatt:"
6360 PRINT FNCUR$(5,2);"Mitglieds-Nr.:"
6370 PRINT FNCUR$(5,24);"Name:"
6380 MSK$="Von:  .  .   Bis:  .  .   Amt:"
6390 FOR KK=7 TO 19 
6400   PRINT FNCUR$(KK,2);MSK$;TAB(43);MSK$
6410 NEXT
6420 GOSUB 15290                                                 'Ausgabe Daten
6430 LNG=2-(HP=7)*6                                    'BEGIN OF THE INPUT-LOOP
6440 TYP=2-(HP=7)
6450 Z=VP+13*(VP>13)+6
6460 S=6-39*(VP>13)-(HP<7)*(HP-1)*3+(HP>3)*(HP<7)*4-(HP=7)*26
6470 N$="":GOSUB 12890                         'ERASE MESSAGE-LINE   'Nachricht
6480 GOSUB 10430                                                       'INPUT+1
6490 IF HP<7 THEN IF ASCC=ASCRET THEN OUTTEXT$=STRING$(2-LEN(OUTTEXT$),"0")+OUTTEXT$   '+NULLEN
6500 IF CKEY<>0 OR ASCC=ASCRET THEN PRINT FNCUR$(Z,S);OUTTEXT$
6510 IF ASCC=ASCRET THEN IF HP<7 THEN MID$(DAT$(VP),-1+2*HP,2)=OUTTEXT$ ELSE DAT$(VP)=LEFT$(DAT$(VP),12)+OUTTEXT$+SPACE$(8-LEN(OUTTEXT$))
6520 IF ASCC=ASCRET AND HP<7 THEN JA=1:TEST$=OUTTEXT$:ON HP GOSUB 12650,12680,12710,12650,12680,12710:IF JA=0 THEN N$="Ungültiges Datum":GOSUB 12890:GOTO 6480 'Datums-Check 'Nachricht
6530 IF ASCC=ASCRET THEN IF HP=7 THEN HP=1:VP=VP-(VP<26)-(VP=26)*(-25):GOTO 6430 ELSE HP=HP+1:GOTO 6430     'NEUE SCREEN-POSITION
6540 IF CKEY<>0 THEN TEST$=MID$(DAT$(VP),-(HP<7)*2*HP-1-(HP=7)*14,-(HP<7)*2-(HP=7)*8):PRINT FNCUR$(Z,S);TEST$
6550 IF CKEY<>0 THEN IF HP<7 AND NOT TEST$="  " THEN JA=1:ON HP GOSUB 12650,12680,12710,12650,12680,12710:IF JA=0 THEN N$="Ungültiges Datum":GOSUB 12890:GOTO 6480 'Nachricht
6560 IF CKEY=CKEYDN THEN VP=VP-(VP<26)-(VP=26)*(-25):GOTO 6430
6570 IF CKEY=CKEYUP THEN VP=VP+(VP>1)-(VP=1)*25     :GOTO 6430
6580 IF CKEY=CKEYRT THEN HP=HP-(HP<7)-(HP=7)*(-6)   :GOTO 6430
6590 IF CKEY=CKEYLT THEN HP=HP+(HP>1)+(HP=1)*(-6)   :GOTO 6430
6600 IF FKEY=1 THEN GOSUB 17750 :GOTO 6290           'HILFE - ZU INIT UND MASKE
6610 IF FKEY=2 THEN 5940                                       'ZU MTGL-EINGABE
6620 IF FKEY=3 THEN VNFLAG=0:GOSUB 11150:GOTO 1950                'Clear DAT$()
6630 IF FKEY=4 THEN GOSUB 13530:GOSUB 13270:GOTO 6480 
6640 IF FKEY=5 THEN GOSUB 14140:GOTO 6430
6650 IF FKEY=6 THEN GOSUB 14300:GOTO 6430
6660 IF FKEY=7 THEN GOSUB 14950:GOTO 6480                 'SPEICHERN DANN INPUT
6670 IF FKEY=9 THEN GOSUB 15530:GOSUB 14500:GOTO 6470
6680 '
6690 ' ############# Mitgliedsliste drucken von 05*************** #############
6700 '
6710 DRUCKFLAG=-1
6720 MTGL$=""
6730 DATUM$="  .  .  "
6740 ' GOSUB 1680                                               'TODO Sad hack!
6750 GOSUB 11260:MTGLZAHL=MTGLMAX           'Einlesen MTGLZAHL TODO: necessary?
6760 GOSUB 11150                                 'Clear DAT$() TODO: Necessary?
6770 GOSUB 11260                                'Clear SUCH$() TODO: Necessary?
6780 '
6790 PRINT CLS$;
6800 PRINT HIGHOFF$;
6810 PRINT CUROFF$;
6820 MT$="MITGLIEDSLISTE DRUCKEN":MI$="G-01300":GOSUB 13100        'Basis Maske
6830 FKEYS$="":GOSUB 13210                           'F-Tastenleiste einblenden
6840 PRINT FNCUR$(4,28);"1 - Suchen & Drucken"
6850 PRINT FNCUR$(6,28);"2 - Komplett drucken"
6860 PRINT FNCUR$(8,28);"3 - Datenblatt drucken"
6870 FKEYMAX=4:NKEYMAX=3:GOSUB 13370                              'Menü-Eingabe
6880 IF FKEY=1 THEN GOSUB 17820:GOTO 6790
6890 IF FKEY=2 OR FKEY=3 THEN GOSUB 11150:DRUCKFLAG=0:GOTO 1950   'Clear DAT$()
6900 IF FKEY=4 THEN GOSUB 13530:GOTO 6870
6910 IF NKEY=1 THEN 6970
6920 IF NKEY=2 THEN DRUCKFLAG=0:GOTO 6970
6930 IF NKEY=3 THEN 7400
6940 '
6950 ' *** Suchen & Drucken, Komplett drucken *********************************
6960 '
6970 GOSUB 13100                                                   'Basis Maske
6980 GOSUB 13210                                     'F-Tastenleiste einblenden
6990 PRINT FNCUR$(7,13);"Geben Sie bitte das heutige Datum ein :   .  .  ":Z=7:LNG=2:TYP=2
7000 PRINT HIGHON$;CURON$
7010 FOR K=0 TO 2
7020   S=53+K*3
7030   VNFLAG=-1:GOSUB 10430:VNFLAG=0    'in order to activate right-left shift 'INPUT+1
7040   IF CKEY<>0 THEN JA=1:TEST$=MID$(DATUM$,K*3+1,2):ON K+1 GOSUB 12650,12680,12710:IF JA=0 THEN N$="Ungültiges Datum":GOSUB 12890:GOTO 7020 'Nachricht
7050   IF FKEY=1 THEN GOSUB 17820:GOTO 6970
7060   IF FKEY=2 THEN 6710
7070   IF FKEY=3 THEN GOSUB 11150:DRUCKFLAG=0:GOTO 1950           'Clear DAT$()
7080   IF FKEY=4 THEN GOSUB 13530:GOTO 7020
7090   IF CKEY=CKEYLT THEN IF K=0 THEN 7020 ELSE K=K-1:GOTO 7020
7100   IF CKEY=CKEYRT THEN IF K=2 THEN K=0:GOTO 7020 ELSE K=K+1:GOTO 7020
7110   IF ASCC<>13 THEN 7020
7120   OUTTEXT$=STRING$(2-LEN(OUTTEXT$),"0")+OUTTEXT$:PRINT FNCUR$(Z,S);OUTTEXT$
7130   TEST$=OUTTEXT$:JA=1:ON K+1 GOSUB 12650,12680,12710:IF JA=0 THEN N$="Ungültige Datumseingabe":GOSUB 12890:GOTO 7020 'Nachricht
7140   MID$(DATUM$,K*3+1,2)=OUTTEXT$
7150 NEXT
7160 PRINT HIGHOFF$;
7170 PRINT CUROFF$
7180 Q$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 12310   'Ja/Nein-Frage
7190 IF JA=0 THEN N$="":GOSUB 12890:GOTO 6970                        'Nachricht
7200 '
7210 ' *** Drucken Anfang *****************************************************
7220 '
7230 IF DRUCKFLAG THEN 7270
7240 PRINT FNCUR$(7,1);DELLINE$;
7250 PRINT CUROFF$
7260 N$="Mitgliedsliste wird gedruckt. Abbrechen mit Taste.":GOSUB 12890 'Nachricht
7270 IF DRUCKFLAG THEN 3880
7280 PRINT FNCUR$(23,1);DELLINE$;
7290 GOSUB 15830                                 'Mitgliedsliste drucken (Kopf)
7300 FOR MTGLZAHL=1 TO MTGLMAX
7310   GOSUB 11860:IF INSTR(DAT$(5),"ausgetreten")>0 THEN 7340
7320   GOSUB 15980                           'Mitgliedsliste drucken (Mitglied)
7330   IF ISBRKPRNT THEN ISBRKPRNT=0:N$="Drucken abgebrochen":GOSUB 12890:PRINT CUROFF$:GOTO 7350 ELSE GOSUB 12890:PRINT CUROFF$ 'Nachricht
7340 NEXT MTGLZAHL
7350 GOSUB 11150                                                  'Clear DAT$()
7360 DRUCKFLAG=0:GOTO 6710
7370 '
7380 ' *** Datenblatt drucken *************************************************
7390 '
7400 MT$="DATENBLATT DRUCKEN":MI$="G-01303":GOSUB 13100            'Basis Maske
7410 FKEYS$="":GOSUB 13210                           'F-Tastenleiste einblenden
7420 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer"
7430 PRINT FNCUR$(5,24)"des zu druckenden Datenblattes ein."
7440 PRINT FNCUR$(7,23)"Beenden Sie die Eingabe mit RETURN"
7450 PRINT FNCUR$(9,37)"=>..."
7460 LNG=3:TYP=2:Z=9:S=39
7470 PRINT HIGHON$:GOSUB 10430:PRINT HIGHOFF$                          'INPUT+1
7480 IF FKEY=2 THEN 6710                                                'F-Keys
7490 IF FKEY=3 THEN GOSUB 11150: GOTO 1950                        'Clear DAT$()
7500 IF FKEY=1 THEN GOSUB 17820:PRINT CURON$:MT$="DATENBLATT DRUCKEN":GOTO 7400
7510 IF FKEY=4 THEN GOSUB 13530:GOTO 7400
7520 IF FKEY>4 THEN CKEY=CKEYDN 
7530 IF CKEY<>0 THEN N$="Ungültige Eingabe - Bitte wiederholen":GOSUB 12890:GOTO 7450 'Nachricht
7540 MTGLZAHL=VAL(OUTTEXT$):IF MTGLZAHL=0 THEN MTGLZAHL=MTGLMAX+1       'Errors
7550 IF MTGLZAHL>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12890:GOTO 7450 'Nachricht
7560 Q$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 12310        'Ja/Nein-Frage
7570 IF JA=0 THEN N$="Bitte schalten Sie den Drucker ein.":GOSUB 12890:DELAY=1000:GOSUB 10280:GOTO 7400     'Drucker bereit ? 'Nachricht 'Delay
7580 GOSUB 11860                                            'S.S.Daten einlesen
7590 NNAM$ = LEFT$(DD1$,25) : NNAM$ = LEFT$(NNAM$,INSTR(NNAM$," "))
7600 NAMM$ = NNAM$ + MID$(DD1$,26,25)
7610 GOSUB 16420                          'Stammsatz drucken (Bildschirmformat)
7620 GOSUB 14050                                            'V.N Daten einlesen
7630 GOSUB 15570                                             'V.N Daten drucken
7640 GOSUB 16680                                             'Mitgliedsvermerke
7650 GOTO 7400
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
10250 ' *** Delay ***
10260 '
10270 ' IN: DELAY
10280 FOR DD=0 TO DELAY:NEXT DD
10290 RETURN
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
10520 ON TYP GOSUB 12400,12470,12530,12590                          'Type check
10530 IF VALID=0 THEN 10590
10540   IF P>=LNG THEN 10500                                   'Valid character
10550   PRINT HIGHON$;A$;HIGHOFF$
10560   OUTTEXT$=OUTTEXT$+A$
10570   P=P+1
10580   GOTO 10490
10590 IF ASCC<>ASCBSP THEN 10650
10600   IF P<1 THEN 10500                                            'Backspace
10610   PRINT DEL$
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
10840 PRINT CUROFF$
10850 RETURN 
10860 '
10870 ' *** Block cursor auto-repeat OK
10880 '
10890 A$=INKEY$:IF A$>=CHR$(ASCCUP) AND A$<=CHR$(ASCCRT) THEN 10890
10900 RETURN
10910 '
10920 ' *** Debounce keyboard OK
10930 '
10940 FOR K=1 TO 30:A$=INKEY$:NEXT K
10950 RETURN
10960 '
10970 ' *** Einlesen Stammdatenfelder-Definition OK
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
11130 ' *** Clear DAT$() OK
11140 '
11150 FOR K=1 TO FELDMAX:DAT$(K)="":NEXT K
11160 RETURN
11170 '
11180 ' *** Clear SUCH$()
11190 '
11200 FOR K=1 TO FELDMAX:SUCH$(K)="":NEXT K
11210 RETURN
11220 '
11230 ' *** Lesen MTGLMAX OK
11240 '
11250 ' OUT: MTGLMAX
11260 OPEN "I",#1,"B:MTGLZAHL.DAT"
11270   INPUT #1,MTGLMAX$
11280   MTGLMAX=VAL(MTGLMAX$)
11290 CLOSE #1
11300 RETURN
11310 '
11320 ' *** Schreiben MTGLMAX OK
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
11740   PRINT STRING$(FELNG(K)-LEN(DAT$(K)),".")
11750 NEXT K
11760 RETURN
11770 '
11780 ' *** FIELDen der Stammsatz-Datei OK
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
12270 ' *** Ja/Nein-Frage und F-Tastenleiste löschen OK
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
12380 ' *** Testbereich 1 (Alpha) ***
12390 '
12400 VALID=0
12410 IF ASCC>57 AND ASCC<127 THEN VALID=1:GOTO 12430
12420 IF ASCC>31 AND ASCC<48 THEN VALID=1
12430 RETURN
12440 '
12450 ' *** Testbereich 2 (Numerisch) ***
12460 '
12470 VALID=0
12480 IF ASCC>47 AND ASCC<58 THEN VALID=1
12490 RETURN
12500 '
12510 ' *** Testbereich 3 (Alphanumerisch) ***
12520 '
12530 VALID=0
12540 IF ASCC>31 AND ASCC<127 THEN VALID=1
12550 RETURN
12560 '
12570 ' *** Testbereich 4 (Numerisch + "/" und ".") ***
12580 '
12590 VALID=0
12600 IF ASCC>45 AND ASCC<58 THEN VALID=1
12610 RETURN
12620 '
12630 ' *** DATUMS-TEST (bei VN und M-Liste drucken) ***
12640 '
12650 IF VAL(TEST$)=0 OR VAL(TEST$)>31 THEN JA=0
12660 RETURN
12670 '
12680 IF VAL(TEST$)=0 OR VAL(TEST$)>12 THEN JA=0
12690 RETURN
12700 '
12710 IF VAL(TEST$)=0 THEN JA=0
12720 RETURN
12730 '
12740 ' *** Voreinstellungen im ERFASSEN (Defaults) OK
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
12860 ' *** Nachricht anzeigen OK
12870 '
12880 ' IN: N$ - Nachricht
12890 PRINT FNCUR$(21,1);DELLINE$;
12900 IF N$<>"" THEN PRINT INVON$;" ";N$;" ";INVOFF$;
12910 PRINT                              'LW workaround quirk in display driver
12920 RETURN
12930 '
12940 ' *** Nachricht anzeigen und F-Tastenleiste ausblenden OK
12950 '
12960 ' IN: N$ - Nachricht
12970 GOSUB 12890                                                    'Nachricht
12980 GOSUB 13470                                                'Delete F-Keys
12990 RETURN
13000 '
13010 ' *** Nachricht löschen OK
13020 '
13030 N$="":GOSUB 12890                                      'Nachricht löschen
13040 RETURN
13050 '
13060 ' *** Bildschirmmaske - Basis OK
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
13180 ' *** F-Tastenleiste einblenden OK
13190 '
13200 ' IN: FKEYS$ - Additional function key labels 
13210 PRINT FNCUR$(23,1);"<F1>=Hilfe <F2>=Zurück <F3>=Anfang <F4>=Hauptmenü";
13220 IF FKEYS$<>"" THEN PRINT " ";FKEYS$ ELSE PRINT
13230 RETURN
13240 '
13250 ' *** restliche funktionstasten bei Verw.-nachweis ausgeben
13260 '
13270 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=Vorwärts             <F8>=Drucken";
13280 PRINT HOME$
13290 RETURN
13300 '
13310 ' *** Eingabe bei Menübildern OK
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
13450 ' *** F-Tastenleiste ausblenden OK
13460 '
13470 PRINT FNCUR$(23,1);DELLINE$
13480 PRINT FNCUR$(24,1);DELLINE$;
13490 RETURN
13500 '
13510 ' *** Rückkehr ins Hauptmenü? (Original) OK
13520 '
13530 GOSUB 13600                                 'Rückkehr ins Hauptmenü Frage
13540 IF JA=1 THEN 18020                                          'Programmende
13550 GOSUB 13210                                    'F-Tastenleiste einblenden
13560 RETURN
13570 '
13580 ' *** Rückkehr ins Hauptmenü Frage (einfach) OK
13590 '
13600 ' OUT: JA (1=true, 0=false)
13610 Q$="Wollen Sie wirklich ins Hauptmenü?":GOSUB 12310        'Ja/Nein-Frage
13620 N$="":GOSUB 12890                                      'Nachricht löschen
13630 RETURN
13640 '
13650 ' *** Rückkehr aus Hilfe-Text? OK
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
13850 ' *** VERWENDUNGSNACHWEIS FÜR NEUES MITGLIED ANLEGEN ***
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
14020 ' *** ERSTE SEITE FINDEN & LESEN ***
14030 '
14040 ' IN: MTGLZAHL
14050 GET #3,MTGLZAHL+1                                            'Index_Datei
14060 CPOINT=VAL(POINT0$)
14070 GET #5,CPOINT:GOSUB 15130                                       'Einlesen
14080 BLATT=1
14090 GET #4,CPOINT                                              'Pointer lesen
14100 RETURN
14110 '
14120 ' *** SEITE RÜCKWÄRTS ***
14130 '
14140 TMPOINT=VAL(POINT1$)
14150 IF TMPOINT<>0 THEN 14190
14160 N$="Erste Seite":GOSUB 12890                                   'Nachricht
14170 DELAY=9999:GOSUB 10280                                             'Delay
14180 RETURN
14190 CPOINT=TMPOINT
14200 BLATT=BLATT-1
14210 GET #5,CPOINT:GOSUB 15130   
14220 GET #4,CPOINT:GOSUB 15290                             'Neue Pointer lesen
14230 HP=1:VP=1
14240 GOSUB 14500
14250 DELAY=100:GOSUB 10280                                              'Delay
14260 RETURN
14270 '
14280 ' *** SEITE VORWÄRTS ***
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
14410 DELAY=20000:GOSUB 10280                                            'Delay
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
14560 ' *** ERSTE SEITE EINES MITGLIEDS ANLEGEN ***
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
14720 ' *** ALLGEMEIN EINE NEUE SEITE ANLEGEN ***
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
14930 ' *** SEITE SPEICHERN ***
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
15110 ' *** HACK ! (ZERSTÜCKELN DER DATEN-STRINGS) ***
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
15240 ' *** UPRO DATENAUSGABE VN ***
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
15480 ' *** Verwendungsnachweis drucken ***
15490 '
15500 ' IN: BLATT
15510 ' IN: MITGLZAHL
15520 ' IN: NAMM$
15530 Q$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 12310 'Ja/Nein-Frage TODO Back to main code?
15540 IF JA=0 THEN 15760
15550 LIND$=SPACE$(2)
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
15720 ' Format: VVVVVV BBBBBB AAAAAAAA
15730 VON$=LEFT$(DAT$(KK),2)+"."+MID$(DAT$(KK),3,2)+"."+MID$(DAT$(KK),5,2)
15740 BIS$=MID$(DAT$(KK),7,2)+"."+MID$(DAT$(KK),9,2)+"."+MID$(DAT$(KK),11,2)
15750 AMT$=RIGHT$(DAT$(KK),8)
15760 RETURN
15770 '
15780 ' ### Unterprogramme "Mitgliedsliste drucken" ###########################
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
15970 ' OUT: ISBRKPRNT (1=stop printing, 0=continue printing)
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
16140 '
16150 IF INKEY$="" THEN 16180          'TODO Move back to main code? Used twice
16160 Q$="Drucken abbrechen?":GOSUB 12310                        'Ja/Nein-Frage
16170 IF JA THEN ISBRKPRNT=1 ELSE ISBRKPRNT=0
16180 RETURN
16190 '
16200 ' *** Adressaufkleber drucken
16210 '                                                           Ja/Nein-Frage
16220 N$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 12310
16230 IF JA=0 THEN RETURN
16240 LIND$=SPACE$(3)
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
16420 XCOL1=8
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
16680 LIND$=SPACE$(9)
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
17100 GOSUB 13670                                      'Rückkehr aus Hilfe-Text
17110 RETURN
17120 '
17130 ' *** Hilfe-Text "Stammsatzpflege" 
17140 '
17150 MT$="HILFE":GOSUB 13100                                      'Basis Maske
17160 PRINT HOME$
17170 GOSUB 13670                                      'Rückkehr aus Hilfe-Text
17180 RETURN
17190 '
17200 ' *** Hilfe-Text "Stammsatz anlegen"
17210 '
17220 SAVEMT$=MT$:MT$="HILFE":GOSUB 13100:MT$=SAVEMT$              'Basis Maske
17230 PRINT HOME$
17240 GOSUB 13670                                      'Rückkehr aus Hilfe-Text
17250 DATENAUSG=1                                                          '???
17260 RETURN
17270 '
17280 ' *** Hilfe-Text "Stammsatz ändern (I)"
17290 '
17300 MT$="HILFE":GOSUB 13100                                      'Basis Maske
17310 PRINT HOME$
17320 GOSUB 13670                                      'Rückkehr aus Hilfe-Text
17330 RETURN
17340 '
17350 ' *** Hilfe-Text "Stammsatz ändern (II)"
17360 '
17370 SAVEMT$=MT$:MT$="HILFE":GOSUB 13100:MT$=SAVEMT$              'Basis Maske
17380 PRINT HOME$
17390 GOSUB 13670                                      'Rückkehr aus Hilfe-Text
17400 GOSUB 13100                                                  'Basis Maske
17410 GOSUB 13210                                    'F-Tastenleiste einblenden
17420 RETURN
17430 '
17440 ' *** Hilfe-Text "Stammsatz löschen"
17450 '
17460 MT$="HILFE":GOSUB 13100                                      'Basis Maske
17470 PRINT HOME$
17480 GOSUB 13670                                      'Rückkehr aus Hilfe-Text
17490 RETURN
17500 '
17510 ' *** Hilfe-Text "Stammsatz suchen"
17520 '
17530 IF DRUCKFLAG THEN 17820                                              '???
17540 SAVEMT$=MT$:MT$="HILFE":GOSUB 13100:MT$=SAVEMT$              'Basis Maske
17550 PRINT HOME$
17560 GOSUB 13670                                      'Rückkehr aus Hilfe-Text
17570 RETURN
17580 ' 
17590 ' *** Hilfe-Text für "Stammsatz drucken"
17600 '
17610 SAVEMT$=MT$:MT$="HILFE":GOSUB 13100:MT$=SAVEMT$              'Basis Maske
17620 PRINT HOME$
17630 GOSUB 13670                                      'Rückkehr aus Hilfe-Text
17640 RETURN
17650 '
17660 ' *** Hilfe-Text "Verwendungsnachweis (I)"
17670 '
17680 MT$="HILFE":GOSUB 13100                                      'Basis Maske
17690 PRINT HOME$
17700 GOSUB 13670                                      'Rückkehr aus Hilfe-Text
17710 RETURN
17720 '
17730 ' *** Hilfe-Text "Verwendungsnachweis (II)"
17740 '
17750 SAVEMT$=MT$:MT$="HILFE":GOSUB 13100:MT$=SAVEMT$              'Basis Maske
17760 PRINT HOME$
17770 GOSUB 13670                                      'Rückkehr aus Hilfe-Text
17780 RETURN
17790 '
17800 ' *** Hilfe-Text "Mitgliedsliste drucken"
17810 '
17820 SAVEMT$=MT$:MT$="HILFE":GOSUB 13100:MT$=SAVEMT$              'Basis Maske
17830 PRINT HOME$
17840 GOSUB 13670                                      'Rückkehr aus Hilfe-Text
17850 RETURN
17860 '
17870 ' *** Programmfehler OK
17880 '
17890 LIND$=SPACE$(15)
17900 GOSUB 13100                                                  'Basis Maske
17910 PRINT HOME$
17920 PRINT LIND$;"Ein Programmfehler ist aufgetreten."
17930 PRINT   
17940 PRINT LIND$;"- Notieren Sie folgende Fehlerinformation:"
17950 PRINT LIND$;"    ";HIGHON$;"ERL";HIGHOFF$;" (Error Line)";HIGHON$;ERL;HIGHOFF$
17960 PRINT LIND$;"    ";HIGHON$;"ERR";HIGHOFF$;" (Error Code)";HIGHON$;ERR;HIGHOFF$
17970 PRINT LIND$;"- Drücken Sie am Rechner den Knopf hinten rechts."
17980 PRINT LIND$;"19**************************"
17990 '
18000 ' *** Programmende OK
18010 '
18020 PRINT FNCUR$(22,1);HIGHOFF$;CURON$;
18030 RESET
18040 'LW RUN "DISKMENU.EXE"
18050 END                                                           '1340 lines
