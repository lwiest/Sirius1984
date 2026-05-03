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
1181 NUMSUCHMAX=2
1182 DIM NUMSUCH$(NUMSUCHMAX)
1183 DIM NUMSUCHROW(NUMSUCHMAX)
1184 NUMSUCHROW(1)=6
1185 NUMSUCHROW(2)=8
1186 '
1187 BYNUMS=1 'Value of SEARCH
1188 BYMASK=2
1189 '
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
1450 ASC0=48
1460 ASC1=49
1470 ASCCUP=200
1480 ASCCDN=201
1490 ASCCLT=202
1500 ASCCRT=203
1510 ASCF1=241
1520 ASCF4=244
1530 ASCF9=249
1540 '
1550 ' Other global variables:
1560 ' -----------------------
1570 ' LIND$    - Local text indent
1580 ' F        - Feldindex 1..27 (FELDMAX)
1590 ' MTGLMAX  - Höchste auf Disk gespeicherte Mitgliedsnummer
1600 ' MTGLZAHL - Momentane Mitgliedsnummer
1610 '
1620 XOFF=14             'Offset Colum Feldname to Column Wertefeld (Pünktchen)
1630 '
1640 ' TIP: Do not let the cursor slip into line 25 -> display will scroll up
1650 '
1660 ON ERROR GOTO 17880                                 'Enable error trapping
1670 '
1680 GOSUB 10050    'Tasten deaktivieren, ASCII Codes für Cursortasten vergeben
1690 PRINT CUROFF$;
1700 '
1710 ' *** Beginn des Mitgliederverwaltungsprogramms **************************
1720 '
1730 MT$="MITGLIEDERVERWALTUNG":MI$="G-01000":GOSUB 13100          'Basis Maske
1740 Q$="Bitte Daten-Diskette in Laufwerk B einlegen. Bereit?":GOSUB 12310
1750 IF JA=0 THEN 18010                                           'Programmende
1760 '
1770 N$="Einen Moment bitte...":GOSUB 12970    'Nachricht und F-keys ausblenden
1780 GOSUB 10990                          'Einlesen Stammdatenfelder-Definition
1790 GOSUB 11800                                   'Fielden der Stammsatz-Datei
1800 GOSUB 13760                       'Fielden der Verwendungsnachweis-Dateien
1810 GOSUB 11260:MTGLZAHL=MTGLMAX                            'Einlesen MTGLZAHL
1820 GOSUB 13030                                             'Nachricht löschen
1830 '
1840 ' ################# Mitgliederverwaltungsmenü (MVW-Menü) #################
1850 '
1860 MT$="MITGLIEDERVERWALTUNG":MI$="G-01000":GOSUB 13100          'Basis Maske
1870 FKEYS$="":GOSUB 13210                           'F-Tastenleiste einblenden
1880 VNFLAG=0                                                              '???
1890 PRINT FNCUR$(4,26);"1 - Stammsatzpflege"                         'Menütext
1900 PRINT FNCUR$(6,26);"2 - Verwendungsnachweis"
1910 PRINT FNCUR$(8,26);"3 - Mitgliedsliste drucken"
1920 FKEYMAX=4:NKEYMAX=3:GOSUB 13360                              'Menü-Eingabe
1930 DRUCKFLAG=0                                                           '???
1940 IF NKEY THEN ON NKEY GOTO 2010,5530,6350
1950 ON FKEY GOTO 1960,1970,1860,1970         'Hilfe, Zurück, Anfang, Hauptmenü
1960 GOSUB 17070:GOTO 1860                                      'F1->Hilfe-Text
1970 GOSUB 13520:GOTO 1920                           'F2->Zurück, F4->Hauptmenü
1980 '
1990 ' ########################### Stammsatzpflege ############################
2000 '
2010 IF DRUCKFLAG THEN RETURN                                              '???
2020 '
2030 ' *** Stammsatzpflege-Menü ***********************************************
2040 '
2050 MT$="STAMMSATZPFLEGE":MI$="G-01100":GOSUB 13100               'Basis Maske
2060 FKEYS$="":GOSUB 13210                           'F-Tastenleiste einblenden
2070 PRINT FNCUR$(4,29);"1 - Stammsatz anlegen"
2080 PRINT FNCUR$(6,29);"2 - Stammsatz ändern"
2090 PRINT FNCUR$(8,29);"3 - Stammsatz löschen"
2100 PRINT FNCUR$(10,29);"4 - Stammsatz drucken"
2110 PRINT FNCUR$(12,29);"5 - Stammsatz suchen"
2120 FKEYMAX=4:NKEYMAX=5:GOSUB 13360                              'Menü-Eingabe
2130 IF NKEY THEN ON NKEY GOTO 2200,2740,3360,5160,3790
2140 ON FKEY GOTO 2150,1860,1860,2160         'Hilfe, Zurück, Anfang, Hauptmenü
2150 GOSUB 17140:GOTO 2050                                      'F1->Hilfe-Text
2160 GOSUB 13520:GOTO 2120                                       'F4->Hauptmenü
2170 '
2180 ' *** Stammsatz erfassen von 02************ ******************************
2190 '
2200 MTGLZAHL=MTGLMAX+1
2210 F=1
2220 DATENAUSG=1                                                           '???
2230 MT$="STAMMSATZ ANLEGEN":MI$="G-01101":GOSUB 13100             'Basis Maske
2240 FKEYS$="<F7>=Speichern":GOSUB 13210             'F-Tastenleiste einblenden
2250 GOSUB 11150                                                  'Clear DAT$()
2260 GOSUB 12760                                           'Defaultwerte setzen
2270 PRINT FNCUR$(3,31);"Mitgliedsnummer";HIGHON$;MTGLZAHL;HIGHOFF$
2280 IF DATENAUSG=1 THEN GOSUB 11680:DATENAUSG=0     'Felder und Feldwerte '???
2290 Z=FEROW(F):S=FECOL(F)+XOFF:LNG=FELNG(F):TYP=FETYP(F):MUSS=FEMUSS(F)
2300 FKEYMASK$="12347":CKEYMASK$="12":INTEXT$=DAT$(F)
2310 GOSUB 10430                                                         'Input
2320 IF ASCC<>ASCRET THEN 2360                                 'Carriage return
2330   DAT$(F)=OUTTEXT$
2340   F=F+1:IF F>FELDMAX THEN F=1
2350   GOTO 2290
2360 IF CKEY=0 THEN 2410                                           'Cursor keys
2370   PRINT FNCUR$(Z,S);HIGHON$;DAT$(F);HIGHOFF$;STRING$(LNG-LEN(DAT$(F)),".")
2380   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FELDMAX
2390   IF CKEY=CKEYDN THEN F=F+1:IF F>FELDMAX THEN F=1
2400   GOTO 2290
2410 IF FKEY=1 THEN GOSUB 17210:GOTO 2230                       'F1->Hilfe-Text
2420 IF FKEY=2 THEN GOSUB 2510:GOTO 2050                       'Exit "Erfassen"
2430 IF FKEY=3 THEN GOSUB 2510:GOTO 1860                       'Exit "Erfassen"
2440 IF FKEY<>4 THEN 2490
2450   GOSUB 13590                                'Rückkehr ins Hauptmenü Frage
2460   IF JA=0 THEN GOSUB 13210:GOTO 2310                    'F-Keys einblenden
2470   GOSUB 2510                                              'Exit "Erfassen"
2480   GOTO 18010                                                 'Programmende
2490 IF FKEY=7 THEN GOSUB 2650:GOTO 2200
2500 '
2510 ' TMPF=1                                                              '???
2520 ' IF INSTR(SPACE$(25),DAT$(TMPF))=0 THEN 2670
2530 '   N$="DEBUG: DAT$() with 25 spaces!":GOSUB 12510              'Nachricht
2540 '   TMPF=TMPF+1:IF TMPF<(FELDMAX+1) THEN 2620
2550 '   MTGLZAHL=MTGLZAHL-1
2560 '   GOTO 2750
2570 '
2580 GOSUB 10940                                             'Debounce keyboard
2590 Q$="Wollen Sie vorher den Stammsatz speichern?":GOSUB 12310 'Ja/Nein-Frage
2600 IF JA=1 THEN 2650                                        'Nachricht und...
2610   N$="Stammsatz wird nicht gespeichert.":GOSUB 12970    '...F-Keys löschen
2620   MTGLZAHL=MTGLZAHL-1
2630   GOTO 2690
2640 '                                                            'Nachricht...
2650 N$="Stammsatz wird gespeichert...":GOSUB 12970      '...und F-Keys löschen
2660 GOSUB 12110                                       'Speichern der Felddaten
2670 GOSUB 13870                                        'Erste VN-Seite anlegen
2680 MTGLMAX=MTGLZAHL:GOSUB 11350                            'Schreiben MTGLMAX
2690 GOSUB 13030                                             'Nachricht löschen
2700 RETURN
2710 ' 
2720 ' *** Stammsatz ändern von 03************ ********************************
2730 '
2740 MT$="STAMMSATZ ÄNDERN":MI$="G-01102":GOSUB 13100              'Basis Maske
2750 FKEYS$="":GOSUB 13210                           'F-Tastenleiste einblenden
2760 PRINT FNCUR$(4,24)"Bitte geben Sie die Mitgliedsnummer" 
2770 PRINT FNCUR$(5,25)"des zu ändernden Stammsatzes ein."
2780 PRINT FNCUR$(6,23);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
2790 PRINT FNCUR$(7,24)"Beenden Sie die Eingabe mit RETURN."
2800 PRINT FNCUR$(9,39)"=>..."
2810 BADINPUT=0
2820 Z=9:S=41:LNG=3:TYP=2:MUSS=1
2830 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
2840 GOSUB 10440                                                       'Input++
2850 IF FKEY=1 THEN GOSUB 17290:GOTO 2740                               'F-Keys
2860 IF FKEY=2 THEN 2050
2870 IF FKEY=3 THEN 1860
2880 IF FKEY=4 THEN GOSUB 13520:GOTO 2840         'Rückkehr ins Hauptmenü-Frage
2890 '
2900 M=VAL(OUTTEXT$)
2910 IF M>=1 AND M<=MTGLMAX THEN 2960
2920   N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12890       'Nachricht
2930   BADINPUT=1
2940   INTEXT$=OUTTEXT$                                                    '???
2950   GOTO 2820
2960 MTGLZAHL=M
2970 N$="Stammsatz wird gelesen...":GOSUB 12970   'Nachricht und F-Keys löschen
2980 GOSUB 11860                                                   'Daten lesen
2990 GOSUB 13030                                             'Nachricht löschen
3000 '
3010 GOSUB 13100                                                   'Basis Maske
3020 FKEYS$="<F7>=Speichern":GOSUB 13210             'F-Tastenleiste einblenden
3030 PRINT FNCUR$(3,31);"Mitgliedsnummer";HIGHON$;MTGLZAHL;HIGHOFF$
3040 GOSUB 11680                                 'Felder und Feldwerte ausgeben
3050 F=1
3060 Z=FEROW(F):S=FECOL(F)+XOFF:LNG=FELNG(F):TYP=FETYP(F):MUSS=FEMUSS(F)
3070 FKEYMASK$="12347":CKEYMASK$="12":INTEXT$=DAT$(F)
3080 GOSUB 10430                                                         'Input
3090 IF ASCC<>ASCRET THEN 3130
3100   DAT$(F)=OUTTEXT$
3110   F=F+1:IF F>FELDMAX THEN F=1
3120   GOTO 3060
3130 IF CKEY=0 THEN 3180
3140   PRINT FNCUR$(Z,S);HIGHON$;DAT$(F);HIGHOFF$;STRING$(LNG-LEN(DAT$(F)),".")
3150   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FELDMAX
3160   IF CKEY=CKEYDN THEN F=F+1:IF F>FELDMAX THEN F=1
3170   GOTO 3060
3180 IF FKEY=1 THEN GOSUB 17360:GOTO 3010
3190 IF FKEY=2 THEN GOSUB 3270:GOTO 2740               'Eingabe Mitgliedsnummer
3200 IF FKEY=3 THEN GOSUB 3270:GOTO 1860                              'MVW-Menü
3210 IF FKEY=7 THEN GOSUB 3290:GOTO 2740
3220 GOSUB 13590                             'F4-> Rückkehr ins Hauptmenü-Frage
3230 IF JA=0 THEN GOSUB 13210:GOTO 3080                      'F-Keys einblenden
3240 GOSUB 3270                                                  'Exit "Ändern'
3250 GOTO 18010                                                   'Programmende
3260 '
3270 Q$="Wollen Sie den Stammsatz speichern?":GOSUB 12310        'Ja/Nein-Frage
3280 IF JA=0 THEN RETURN
3290 N$="Stammsatz wird gespeichert...":GOSUB 12970 'Nachricht und F-Keys löschen
3300 GOSUB 12110                                               'Daten speichern
3310 GOSUB 13030                                             'Nachricht löschen
3320 RETURN
3330 '
3340 ' *** Stammsatz löschen von 04********** *********************************
3350 '
3360 MT$="STAMMSATZ LÖSCHEN":MI$="G-01103":GOSUB 13100             'Basis Maske
3370 FKEYS$="":GOSUB 13210                           'F-Tastenleiste einblenden
3380 PRINT FNCUR$(4,24);"Bitte geben Sie die Mitgliedsnummer"         'Menütext
3390 PRINT FNCUR$(5,25);"des zu löschenden Mitglieds ein."
3400 PRINT FNCUR$(6,23);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
3410 PRINT FNCUR$(7,15);"(Dabei wird nur der Status des Mitglieds verändert.)"
3420 PRINT FNCUR$(8,24);"Beenden Sie die Eingabe mit RETURN."
3430 PRINT FNCUR$(10,39);"=>..."
3440 BADINPUT=0
3450 Z=10:S=41:LNG=3:TYP=2:MUSS=1
3460 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
3470 GOSUB 10440                                                       'Input++
3480 IF FKEY=1 THEN GOSUB 17450:GOTO 3360
3490 IF FKEY=2 THEN 2050                                                'F-Keys
3500 IF FKEY=3 THEN 1860
3510 IF FKEY=4 THEN GOSUB 13520:GOTO 3470 'Rückkehr ins Hauptmenü-Frage 'F-keys einblenden
3520 '
3530 M=VAL(OUTTEXT$)
3540 IF M>=1 AND M<=MTGLMAX THEN 3590
3550   N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12890       'Nachricht
3560   BADINPUT=1
3570   INTEXT$=OUTTEXT$                                                    '???
3580   GOTO 3450
3590 MTGLZAHL=M
3600 N$="Stammsatz wird gelesen...":GOSUB 12970   'Nachricht und F-Keys löschen
3610 GOSUB 11860                                                   'Daten lesen
3620 GOSUB 13030                                             'Nachricht löschen
3630 '
3640 GOSUB 13100                                                   'Basis Maske
3650 PRINT FNCUR$(3,31);"Mitgliedsnummer";HIGHON$;MTGLZAHL;HIGHOFF$
3660 GOSUB 11680                                 'Felder und Feldwerte ausgeben
3670 Q$="Mitglied endgültig löschen?":GOSUB 12310                'Ja/Nein-Frage
3680 IF JA=0 THEN 3750
3690   DAT$(5)="ausgetreten"
3700   PRINT FNCUR$(FEROW(5),FECOL(5)+XOFF);HIGHON$;DAT$(5);HIGHOFF$;
3710   PRINT STRING$(FELNG(5)-LEN(DAT$(5)),".")                   'NO semicolon
3720   N$="Stammsatz wird gespeichert...":GOSUB 12970 'Nachricht und F-Keys löschen
3730   GOSUB 12110                                             'Daten speichern
3740   GOSUB 13030                                           'Nachricht löschen
3750 GOTO 3360
3760 '
3770 ' *** Stammsatz suchen (und drucken) von 05*************** ***************
3780 '
3790 IF DRUCKFLAG THEN MT$="SUCHEN & DRUCKEN" ELSE MT$="STAMMSATZ SUCHEN"
3810 MI$="G-01105":GOSUB 13100          'TODO IF DRUCKFLAG THEN... 'Basis Maske
3820 FKEYS$="":GOSUB 13210                           'F-Tastenleiste einblenden
3830 IF DRUCKFLAG THEN TMP$="Drucken" ELSE TMP$="Suchen"
3840 PRINT FNCUR$(4,30)"1 - ";TMP$;" nach Nummern"
3850 PRINT FNCUR$(6,30)"2 - ";TMP$;" nach Maske"
3860 FKEYMAX=4:NKEYMAX=2:GOSUB 13360                              'Menü-Eingabe
3870 IF FKEY=1 THEN GOSUB 17520:GOTO 3790   'TODO IF DRUCKFLAG THEN ... 'F-Keys
3880 IF FKEY=2 THEN IF DRUCKFLAG THEN 6350 ELSE 2050
3890 IF FKEY=3 THEN 1860
3900 IF FKEY=4 THEN GOSUB 13520:GOTO 3860         'Rückkehr ins Hauptmenü-Frage
3910 ON NKEY GOTO 4180,4622 
4170 ' *** Suchen nach Nummern ************************************************
4180 NUMSUCH1=0
4181 NUMSUCH2=0
4182 TMP$=STR$(1):NUMSUCH$(1)=RIGHT$(TMP$,LEN(TMP$)-1)
4210 TMP$=STR$(MTGLMAX):NUMSUCH$(2)=RIGHT$(TMP$,LEN(TMP$)-1)
4240 GOSUB 13100                                                   'Basis Maske
4250 FKEYS$="<F5>=Suchen":IF DRUCKFLAG THEN FKEYS$="<F5>=Suchen & Drucken>"
4260 GOSUB 13210                                     'F-Tastenleiste einblenden
4270 TMP$="gesucht":IF DRUCKFLAG THEN TMP$="gesucht und gedruckt"     
4280 PRINT FNCUR$(4,25);"Gültige Mitgliedsnummern: 1 -";STR$(MTGLMAX)
4290 PRINT FNCUR$(6,13);"Ab welcher Mitgliedsnummer soll ";TMP$;" werden?     =>..."
4300 PRINT FNCUR$(8,13);"Bis zu welcher Mitgliedsnummer soll ";TMP$;" werden? =>..."
4310 PRINT FNCUR$(6,67);HIGHON$;NUMSUCH$(1);HIGHOFF$
4320 PRINT FNCUR$(8,67);HIGHON$;NUMSUCH$(2);HIGHOFF$
4321 F=1
4330 BADINPUT=0
4340 Z=NUMSUCHROW(F):S=67:LNG=3:TYP=2:MUSS=1
4350 FKEYMASK$="12345":CKEYMASK$="":INTEXT$=NUMSUCH$(F)
4360 GOSUB 10440                                                       'Input++
4370 IF FKEY=1 THEN GOSUB 17520:GOTO 4240
4380 IF FKEY=2 THEN 3790
4390 IF FKEY=3 THEN 1860
4400 IF FKEY=4 THEN GOSUB 13520:GOTO 4360         'Rückkehr ins Hauptmenü-Frage
4410 IF FKEY<>5 THEN 4420
4411   SEARCH=BYNUMS
4412   NUMSUCH1=VAL(NUMSUCH$(1))
4413   NUMSUCH2=VAL(NUMSUCH$(2))
4415   GOTO 4646                                                      '-> Suche
4420 M=VAL(OUTTEXT$)                                             'Handle RETURN
4430 IF M>=1 AND M<=MTGLMAX THEN 4470
4440   N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12890       'Nachricht
4450   GOSUB 4590
4460   GOTO 4360
4470 NUMSUCH$(F)=OUTTEXT$
4471 NUMSUCH1=VAL(NUMSUCH$(1))
4472 NUMSUCH2=VAL(NUMSUCH$(2))
4490 IF (NUMSUCH1<=NUMSUCH2) THEN 4530
4500   N$="Ungültiger Bereich":GOSUB 12890                           'Nachricht
4510   GOSUB 4590
4520   GOTO 4360
4530 F=F+1:IF F>NUMSUCHMAX THEN F=1
4540 FKEYMASK$="12345"
4550 FKEYS$="<F5>=Suchen":IF DRUCKFLAG THEN FKEYS$="<F5>=Suchen & Drucken>"
4560 GOSUB 13460:GOSUB 13210             'F-Tastenleiste löschen und einblenden
4570 GOTO 4340
4580 '
4590 BADINPUT=1
4600 INTEXT$=OUTTEXT$
4610 FKEYMASK$="1234"
4611 FKEYS$=""
4612 GOSUB 13460:GOSUB 13210             'F-Tastenleiste löschen und einblenden
4620 RETURN
4621 ' *** Suchen nach Maske **************************************************
4622 GOSUB 11200                                                 'Clear SUCH$()
4623 GOSUB 13100                                                   'Basis Maske
4624 FKEYS$="<F5>=Suchen":IF DRUCKFLAG THEN FKEYS$="<F5>=Suchen & Drucken>" 
4625 GOSUB 13210                                     'F-Tastenleiste einblenden
4626 GOSUB 11440                       'Feldnamen und Werteplatzhalter ausgeben
4627 F=1
4628 Z=FEROW(F):S=FECOL(F)+XOFF:LNG=FELNG(F):TYP=3
4629 FKEYMASK$="12345":CKEYMASK$="12":INTEXT$=""
4630 GOSUB 10430                                                         'Input
4631 IF FKEY=0 THEN 4637
4632   IF FKEY=1 THEN GOSUB 17520:GOTO 4623         'TODO IF DRUCKFLAG THEN ...
4633   IF FKEY=2 THEN 3790
4634   IF FKEY=3 THEN 1860
4635   IF FKEY=4 THEN GOSUB 13520:GOTO 4630       'Rückkehr ins Hauptmenü-Frage
4636   IF FKEY=5 THEN SEARCH=BYMASK:GOTO 4646                         '-> Suche
4637 IF CKEY=0 THEN 4642                                         'Cursor keys
4638   PRINT FNCUR$(Z,S);HIGHON$;SUCH$(F);HIGHOFF$;STRING$(LNG-LEN(SUCH$(F)),".")
4639   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FELDMAX
4640   IF CKEY=CKEYDN THEN F=F+1:IF F>FELDMAX THEN F=1
4641   GOTO 4628
4642 SUCH$(F)=OUTTEXT$                                           'Handle RETURN
4643 F=F+1:IF F>FELDMAX THEN F=1
4644 GOTO 4628
4645 ' *** Suche **************************************************************
4646 IF SEARCH=BYMASK THEN MTGLZAHL=1 ELSE MTGLZAHL=NUMSUCH1
4647 IF DRUCKFLAG THEN GOSUB 15820               'Mitgliedsliste drucken (Kopf)
4680 GOSUB 13100                                                   'Basis Maske
4681 PRINT FNCUR$(3,31);"Mitgliedsnummer"
4683 GOSUB 11440                       'Feldnamen und Werteplatzhalter ausgeben
4684 IF DRUCKFLAG THEN 4700                       '-> Suche Schleifen-End-Check
4685   TMP$="<F6>=Suchen vorwärts <F7>=Adressaufkleber"
4686   IF SEARCH=BYMASK THEN FKEYS$=TMP$ ELSE FKEYS$="<F5>=Suchen rückwärts "+TMP$
4687   GOSUB 13210                                   'F-Tastenleiste einblenden
4688 '
4700 IF SEARCH=BYMASK AND MTGLZAHL<=MTGLMAX THEN 4770
4710 IF SEARCH=BYNUMS AND MTGLZAHL<=NUMSUCH2 THEN 4770
4719 '                                                            End of search
4720 IF DRUCKFLAG THEN 3790                         'Entry point Stammsatzsuche
4731 MTGLZAHL=MTGLZAHL-1                                                   '???
4740 N$="Keine weiteren Daten vorhanden. Mit <F2> neue Suche.":GOSUB 12890 'Nachricht
4760 GOTO 4940                                                  '->Menü-Eingabe
4761 '
4770 GOSUB 11860                          'Daten lesen TODO Nachricht ausgeben?
4780 IF SEARCH=BYNUMS THEN 4880                          '-> Feldwerte ausgeben
4790   FOR K=1 TO FELDMAX                                    'Daten durchsuchen
4800     IF LEFT$(SUCH$(K),1)<>"*" THEN 4830
4810     IF INSTR(DAT$(K),RIGHT$(SUCH$(K),LEN(SUCH$(K))-1))>0 THEN 4870 'NEXT K 
4811       MTGLZAHL=MTGLZAHL+1
4820       GOTO 4700                                 '-> Suchschleife-End-Check
4830     I=1
4840     IF MID$(SUCH$(K),I,1)="?" THEN I=I+1:GOTO 4840
4850     I=I-1
4860     IF INSTR(DAT$(K),RIGHT$(SUCH$(K),LEN(SUCH$(K))-I))=I+1 THEN 4870 'NEXT K
4861       MTGLZAHL=MTGLZAHL+1
4862       GOTO 4700                                 '-> Suchschleife-End-Check
4870   NEXT K
4871 '
4880 GOSUB 11560                                            'Feldwerte ausgeben
4900 PRINT FNCUR$(3,46);HIGHON$;STR$(MTGLZAHL);HIGHOFF$ 
4910 IF DRUCKFLAG=0 THEN 4940
4920   GOSUB 15970                           'Mitgliedsliste drucken (Mitglied)
4930   IF ISBRKPRNT THEN 4935
4931     N$="":GOSUB 12890                                   'Nachricht löschen
4932     MTGLZAHL=MTGLZAHL+1
4933     GOTO 4700                                   'Suche Schleifen-End-Check
4935   ISBRKPRNT=0
4936   N$="":GOSUB 12890                                     'Nachricht löschen
4937   GOTO 3790                                   'Stammsatz suche start entry
4940 FKEMAX=7:NKEYMAX=0:GOSUB 13360      'Menü-Eingabe 'TODO Nachricht löschen?
4941 ON FKEY GOTO 4960,4970,1860,4990,5000,5010,5020
4942 '
4960 GOSUB 17520                                                    '<F1> Hilfe
4961 GOSUB 13100                                                   'Basis Maske
4962 PRINT FNCUR$(3,30);"Mitgliedsnummer"
4963 GOSUB 11440                       'Feldnamen und Werteplatzhalter ausgeben
4964 IF NOT DRUCKFLAG THEN GOSUB 13210               'F-Tastenleiste einblenden
4965 GOTO 4880                                '-> Feldwerte (MTGLZAHL) ausgeben
4966 '
4970 IF SEARCH=BYNUMS THEN 4180 ELSE 4622                          '<F2> Zurück
4980 '
4990 GOSUB 13520                             '<F4> Rückkehr ins Hauptmenü-Frage
4991 GOTO 4880                                '-> Feldwerte (MTGLZAHL) ausgeben
4992 '
5000 IF SEARCH=BYMASK OR MTGLZAHL<=NUMSUCH1 THEN 5005                     '<F5>
5001   MTGLZAHL=MTGLZAHL-1
5002   GOTO 4770                                                   'Daten lesen
5005 N$="Erster Suchtreffer":GOSUB 12890                    'Nachricht ausgeben
5006 GOTO 4940                                                    'Neue Eingabe
5007 '
5010 MTGLZAHL=MTGLZAHL+1                                                  '<F6>
5011 GOTO 4700                                          'Suchschleife-End-Check
5012 '
5020 GOSUB 16210                    '<F7> Auf Nachfrage Adressaufkleber drucken 
5021 GOSUB 13210                                             'F-keys einblenden
5022 N$="":GOSUB 12890                                       'Nachricht löschen
5023 GOTO 4940                                                    'Neue Eingabe
5130 '
5140 ' *** Stammsatz drucken von 02************ & 04********** ****************
5150 '
5160 MT$="STAMMSATZ DRUCKEN":MI$="G-01104":GOSUB 13100             'Basis Maske
5170 FKEYS$="":GOSUB 13210                           'F-Tastenleiste einblenden
5180 PRINT FNCUR$(4,24)"Bitte geben Sie die Mitgliedsnummer"
5190 PRINT FNCUR$(5,23)"des auszudruckenden Stammsatzes ein."
5200 PRINT FNCUR$(6,23);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
5210 PRINT FNCUR$(7,24)"Beenden Sie die Eingabe mit RETURN."
5220 PRINT FNCUR$(9,39);"=>..."
5230 BADINPUT=0
5240 Z=9:S=41:LNG=3:TYP=2:MUSS=1
5250 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
5260 GOSUB 10440                                                       'Input++
5270 IF FKEY=1 THEN GOSUB 17600:GOTO 5160                               'F-Keys
5280 IF FKEY=2 THEN 2050
5290 IF FKEY=3 THEN 1860
5300 IF FKEY=4 THEN GOSUB 13520:GOTO 5260         'Rückkehr ins Hauptmenü-Frage
5310 '
5320 M=VAL(OUTTEXT$)
5330   IF M>=1 AND M<=MTGLMAX THEN 5380
5340   N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12890       'Nachricht
5350   BADINPUT=1
5360   INTEXT$=OUTTEXT$                                                    '???
5370   GOTO 3450
5380 MTGLZAHL=M
5390 N$="Stammsatz wird gelesen...":GOSUB 12970   'Nachricht und F-Keys löschen
5400 GOSUB 11860                                                   'Daten lesen
5410 GOSUB 13030                                             'Nachricht löschen
5420 '
5430 GOSUB 13100                                                   'Basis Maske
5440 PRINT FNCUR$(3,31);"Mitgliedsnummer";HIGHON$;MTGLZAHL;HIGHOFF$
5450 GOSUB 11680                                 'Felder und Feldwerte ausgeben
5460 Q$="Ist der Drucker eingeschaltet und druckbereit?":GOSUB 12310 'Ja/Nein-Frage
5470 IF JA=0 THEN 5490
5480 GOSUB 16410                          'Stammsatz drucken (Bildschirmformat)
5490 GOTO 5160
5500 '
5510 ' ######################### Verwendungsnachweis ##########################
5520 '
5530 GOSUB 11260                                              'Einlesen MTGLMAX
5540 VNFLAG=-1
5550 '
5560 ' Verwendungsnachweis-Menü ***********************************************
5570 '
5580 MT$="VERWENDUNGSNACHWEIS":MI$="G-01200":GOSUB 13100           'Basis Maske
5590 F=4                                                                'TODO ?
5600 FKEYS$="":GOSUB 13210                           'F-Tastenleiste einblenden
5610 PRINT CURON$;
5620 PRINT HIGHOFF$;
5630 PRINT FNCUR$(4,20)"   Bitte geben Sie die Mitgliedsnummer"
5640 PRINT FNCUR$(5,20)"des zu ändernden Verwendungsnachweises ein"
5650 PRINT FNCUR$(6,20)" und beenden Sie die Eingabe mit RETURN."
5660 PRINT FNCUR$(8,20)"                 =>..."
5670 Z=8:S=39:LNG=3:TYP=2
5680 GOSUB 10430                                                         'INPUT
5690 IF FKEY=1 THEN GOSUB 17670:GOTO 5580
5700 IF FKEY=2 OR FKEY=3 THEN GOSUB 11150:VNFLAG=0:GOTO 1860      'Clear DAT$()
5710 IF FKEY=4 THEN GOSUB 13520
5720 IF FKEY>4 THEN 5670
5730 IF CKEY<>0 THEN 5670
5740 IF LEN(OUTTEXT$)=0 THEN N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 12890:GOTO 5660 ELSE MITGLIED=VAL(OUTTEXT$):IF MITGLIED=0 THEN 5680 'Nachricht
5750 IF MITGLIED>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12890:PRINT HOME$;HIGHOFF$:GOTO 5660 'Nachricht
5760 MTGLZAHL=MITGLIED:MTGL$=STR$(MITGLIED)
5770 '
5780 ' *** Einlesen des Mitgliedsnamens ***************************************
5790 '
5800 PRINT FNCUR$(21,1);DELLINE$;
5810 GET #2,MITGLIED                                        'EINLESEN DATEN STS
5820 NNAM$=LEFT$(DD1$,25)                                 'NACHNAME RAUSFISCHEN
5830 VNAM$=MID$(DD1$,26,25)                               'VORNAME  RAUSFISCHEN
5840 '
5850 ' ** LEERZEICHEN HINTER NACHNAME ABSCHNEIDEN *****************************
5860 '
5870 FOR I=LEN(NNAM$) TO 1 STEP -1
5880   IF MID$(NNAM$,I,1)<>" " THEN NNAM$=LEFT$(NNAM$,I):I=1
5890 NEXT
5900 NAMM$=NNAM$+" "+VNAM$
5910 GOSUB 14040                                          'EINLESEN 1. VN-BLATT
5920 BLATT=1
5930 VP=1:HP=1                                                 'INITIALISIERUNG
5940 F=8
5950 GOSUB 13100                                                   'Basis Maske
5960 FKEYS$="<F7>=Speichern":GOSUB 13210             'F-Tastenleiste einblenden
5970 PRINT HIGHOFF$;
5980 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=Vorwärts";TAB(53);"<F9>=Blatt drucken";
5990 PRINT FNCUR$(3,2);"Blatt:"
6000 PRINT FNCUR$(5,2);"Mitglieds-Nr.:"
6010 PRINT FNCUR$(5,24);"Name:"
6020 MSK$="Von:  .  .   Bis:  .  .   Amt:"
6030 FOR KK=7 TO 19 
6040   PRINT FNCUR$(KK,2);MSK$;TAB(43);MSK$                    'LW NO semicolon
6050 NEXT
6060 GOSUB 15280                                                 'Ausgabe Daten
6070 LNG=2-(HP=7)*6                                    'BEGIN OF THE INPUT-LOOP
6080 TYP=2-(HP=7)
6090 Z=VP+13*(VP>13)+6
6100 S=6-39*(VP>13)-(HP<7)*(HP-1)*3+(HP>3)*(HP<7)*4-(HP=7)*26
6110 N$="":GOSUB 12890                         'ERASE MESSAGE-LINE   'Nachricht
6120 GOSUB 10430                                                       'INPUT+1
6130 IF HP<7 THEN IF ASCC=ASCRET THEN OUTTEXT$=STRING$(2-LEN(OUTTEXT$),"0")+OUTTEXT$   '+NULLEN
6140 IF CKEY<>0 OR ASCC=ASCRET THEN PRINT FNCUR$(Z,S);OUTTEXT$
6150 IF ASCC=ASCRET THEN IF HP<7 THEN MID$(DAT$(VP),-1+2*HP,2)=OUTTEXT$ ELSE DAT$(VP)=LEFT$(DAT$(VP),12)+OUTTEXT$+SPACE$(8-LEN(OUTTEXT$))
6160 IF ASCC=ASCRET AND HP<7 THEN JA=1:TEST$=OUTTEXT$:ON HP GOSUB 12650,12680,12710,12650,12680,12710:IF JA=0 THEN N$="Ungültiges Datum":GOSUB 12890:GOTO 6120 'Datums-Check 'Nachricht
6170 IF ASCC=ASCRET THEN IF HP=7 THEN HP=1:VP=VP-(VP<26)-(VP=26)*(-25):GOTO 6070 ELSE HP=HP+1:GOTO 6070     'NEUE SCREEN-POSITION
6180 IF CKEY<>0 THEN TEST$=MID$(DAT$(VP),-(HP<7)*2*HP-1-(HP=7)*14,-(HP<7)*2-(HP=7)*8):PRINT FNCUR$(Z,S);TEST$
6190 IF CKEY<>0 THEN IF HP<7 AND NOT TEST$="  " THEN JA=1:ON HP GOSUB 12650,12680,12710,12650,12680,12710:IF JA=0 THEN N$="Ungültiges Datum":GOSUB 12890:GOTO 6120 'Nachricht
6200 IF CKEY=CKEYDN THEN VP=VP-(VP<26)-(VP=26)*(-25):GOTO 6070
6210 IF CKEY=CKEYUP THEN VP=VP+(VP>1)-(VP=1)*25     :GOTO 6070
6220 IF CKEY=CKEYRT THEN HP=HP-(HP<7)-(HP=7)*(-6)   :GOTO 6070
6230 IF CKEY=CKEYLT THEN HP=HP+(HP>1)+(HP=1)*(-6)   :GOTO 6070
6240 IF FKEY=1 THEN GOSUB 17740 :GOTO 5930           'HILFE - ZU INIT UND MASKE
6250 IF FKEY=2 THEN 5580                                       'ZU MTGL-EINGABE
6260 IF FKEY=3 THEN VNFLAG=0:GOSUB 11150:GOTO 1860                'Clear DAT$()
6270 IF FKEY=4 THEN GOSUB 13520:GOSUB 13270:GOTO 6120 
6280 IF FKEY=5 THEN GOSUB 14130:GOTO 6070
6290 IF FKEY=6 THEN GOSUB 14290:GOTO 6070
6300 IF FKEY=7 THEN GOSUB 14940:GOTO 6120                 'SPEICHERN DANN INPUT
6310 IF FKEY=9 THEN GOSUB 15520:GOSUB 14490:GOTO 6110
6320 '
6330 ' ############# Mitgliedsliste drucken von 05*************** #############
6340 '
6350 DRUCKFLAG=-1
6360 MTGL$=""
6370 DATUM$="  .  .  "
6380 ' GOSUB 1680                                               'TODO Sad hack!
6390 GOSUB 11260:MTGLZAHL=MTGLMAX           'Einlesen MTGLZAHL TODO: necessary?
6400 GOSUB 11150                                 'Clear DAT$() TODO: Necessary?
6410 GOSUB 11260                                'Clear SUCH$() TODO: Necessary?
6420 '
6430 PRINT CLS$;
6440 PRINT HIGHOFF$;
6450 PRINT CUROFF$;
6460 MT$="MITGLIEDSLISTE DRUCKEN":MI$="G-01300":GOSUB 13100        'Basis Maske
6470 FKEYS$="":GOSUB 13210                           'F-Tastenleiste einblenden
6480 PRINT FNCUR$(4,28);"1 - Suchen & Drucken"
6490 PRINT FNCUR$(6,28);"2 - Komplett drucken"
6500 PRINT FNCUR$(8,28);"3 - Datenblatt drucken"
6510 FKEYMAX=4:NKEYMAX=3:GOSUB 13360                              'Menü-Eingabe
6520 IF FKEY=1 THEN GOSUB 17810:GOTO 6430
6530 IF FKEY=2 OR FKEY=3 THEN GOSUB 11150:DRUCKFLAG=0:GOTO 1860   'Clear DAT$()
6540 IF FKEY=4 THEN GOSUB 13520:GOTO 6510
6550 IF NKEY=1 THEN 6610
6560 IF NKEY=2 THEN DRUCKFLAG=0:GOTO 6610
6570 IF NKEY=3 THEN 7040
6580 '
6590 ' *** Suchen & Drucken, Komplett drucken *********************************
6600 '
6610 GOSUB 13100                                                   'Basis Maske
6620 GOSUB 13210                                     'F-Tastenleiste einblenden
6630 PRINT FNCUR$(7,13);"Geben Sie bitte das heutige Datum ein :   .  .  ":Z=7:LNG=2:TYP=2
6640 PRINT HIGHON$;CURON$
6650 FOR K=0 TO 2
6660   S=53+K*3
6670   VNFLAG=-1:GOSUB 10430:VNFLAG=0    'in order to activate right-left shift 'INPUT+1
6680   IF CKEY<>0 THEN JA=1:TEST$=MID$(DATUM$,K*3+1,2):ON K+1 GOSUB 12650,12680,12710:IF JA=0 THEN N$="Ungültiges Datum":GOSUB 12890:GOTO 6660 'Nachricht
6690   IF FKEY=1 THEN GOSUB 17810:GOTO 6610
6700   IF FKEY=2 THEN 6350
6710   IF FKEY=3 THEN GOSUB 11150:DRUCKFLAG=0:GOTO 1860           'Clear DAT$()
6720   IF FKEY=4 THEN GOSUB 13520:GOTO 6660
6730   IF CKEY=CKEYLT THEN IF K=0 THEN 6660 ELSE K=K-1:GOTO 6660
6740   IF CKEY=CKEYRT THEN IF K=2 THEN K=0:GOTO 6660 ELSE K=K+1:GOTO 6660
6750   IF ASCC<>13 THEN 6660
6760   OUTTEXT$=STRING$(2-LEN(OUTTEXT$),"0")+OUTTEXT$:PRINT FNCUR$(Z,S);OUTTEXT$
6770   TEST$=OUTTEXT$:JA=1:ON K+1 GOSUB 12650,12680,12710:IF JA=0 THEN N$="Ungültige Datumseingabe":GOSUB 12890:GOTO 6660 'Nachricht
6780   MID$(DATUM$,K*3+1,2)=OUTTEXT$
6790 NEXT
6800 PRINT HIGHOFF$;
6810 PRINT CUROFF$
6820 Q$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 12310   'Ja/Nein-Frage
6830 IF JA=0 THEN N$="":GOSUB 12890:GOTO 6610                        'Nachricht
6840 '
6850 ' *** Drucken Anfang *****************************************************
6860 '
6870 IF DRUCKFLAG THEN 6910
6880 PRINT FNCUR$(7,1);DELLINE$;
6890 PRINT CUROFF$
6900 N$="Mitgliedsliste wird gedruckt. Abbrechen mit Taste.":GOSUB 12890 'Nachricht
6910 IF DRUCKFLAG THEN 3790
6920 PRINT FNCUR$(23,1);DELLINE$;
6930 GOSUB 15820                                 'Mitgliedsliste drucken (Kopf)
6940 FOR MTGLZAHL=1 TO MTGLMAX
6950   GOSUB 11860:IF INSTR(DAT$(5),"ausgetreten")>0 THEN 6980
6960   GOSUB 15970                           'Mitgliedsliste drucken (Mitglied)
6970   IF ISBRKPRNT THEN ISBRKPRNT=0:N$="Drucken abgebrochen":GOSUB 12890:PRINT CUROFF$:GOTO 6990 ELSE GOSUB 12890:PRINT CUROFF$ 'Nachricht
6980 NEXT MTGLZAHL
6990 GOSUB 11150                                                  'Clear DAT$()
7000 DRUCKFLAG=0:GOTO 6350
7010 '
7020 ' *** Datenblatt drucken *************************************************
7030 '
7040 MT$="DATENBLATT DRUCKEN":MI$="G-01303":GOSUB 13100            'Basis Maske
7050 FKEYS$="":GOSUB 13210                           'F-Tastenleiste einblenden
7060 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer"
7070 PRINT FNCUR$(5,24)"des zu druckenden Datenblattes ein."
7080 PRINT FNCUR$(7,23)"Beenden Sie die Eingabe mit RETURN"
7090 PRINT FNCUR$(9,37)"=>..."
7100 LNG=3:TYP=2:Z=9:S=39
7110 PRINT HIGHON$:GOSUB 10430:PRINT HIGHOFF$                          'INPUT+1
7120 IF FKEY=2 THEN 6350                                                'F-Keys
7130 IF FKEY=3 THEN GOSUB 11150: GOTO 1860                        'Clear DAT$()
7140 IF FKEY=1 THEN GOSUB 17810:PRINT CURON$:MT$="DATENBLATT DRUCKEN":GOTO 7040
7150 IF FKEY=4 THEN GOSUB 13520:GOTO 7040
7160 IF FKEY>4 THEN CKEY=CKEYDN 
7170 IF CKEY<>0 THEN N$="Ungültige Eingabe - Bitte wiederholen":GOSUB 12890:GOTO 7090 'Nachricht
7180 MTGLZAHL=VAL(OUTTEXT$):IF MTGLZAHL=0 THEN MTGLZAHL=MTGLMAX+1       'Errors
7190 IF MTGLZAHL>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12890:GOTO 7090 'Nachricht
7200 Q$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 12310        'Ja/Nein-Frage
7210 IF JA=0 THEN N$="Bitte schalten Sie den Drucker ein.":GOSUB 12890:DELAY=1000:GOSUB 10280:GOTO 7040     'Drucker bereit ? 'Nachricht 'Delay
7220 GOSUB 11860                                            'S.S.Daten einlesen
7230 NNAM$ = LEFT$(DD1$,25) : NNAM$ = LEFT$(NNAM$,INSTR(NNAM$," "))
7240 NAMM$ = NNAM$ + MID$(DD1$,26,25)
7250 GOSUB 16410                          'Stammsatz drucken (Bildschirmformat)
7260 GOSUB 14040                                            'V.N Daten einlesen
7270 GOSUB 15560                                             'V.N Daten drucken
7280 GOSUB 16670                                             'Mitgliedsvermerke
7290 GOTO 7040
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
11400 ' *** Maske Feldnamen und Eingabefelder (Pünktchen) ausgeben
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
12320 GOSUB 13460                                                'Delete F-keys
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
12980 GOSUB 13460                                                'Delete F-Keys
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
13330 ' IN:  NKEYMAX - max valid numeric key (1, 2, 3, ...)
13331 ' IN:  FKEYMAX - max valid function key (1, 2, 3, ...)
13340 ' OUT: FKEY - number of pressed function key or 0
13350 ' OUT: NKEY - number of pressed numeric key or 0
13360 NKEY=0:FKEY=0
13370 GOSUB 10940                                            'Debounce keyboard
13380 A$=INPUT$(1):ASCC=ASC(A$)
13390 IF ASCC>=ASC1 AND ASCC<=ASC1+NKEYMAX-1 THEN NKEY=ASCC-ASC1+1:RETURN
13400 IF ASCC>=ASCF1 AND ASCC<=ASCF1+FKEYMAX-1 THEN FKEY=ASCC-ASCF1+1:RETURN
13410 N$="Sie haben eine falsche Taste gedrückt - Bitte neue Auswahl treffen.":GOSUB 12890 'Nachricht
13420 GOTO 13380
13430 '
13440 ' *** F-Tastenleiste ausblenden ******************************** REVIEWED
13450 '
13460 PRINT FNCUR$(23,1);DELLINE$
13470 PRINT FNCUR$(24,1);DELLINE$;
13480 RETURN
13490 '
13500 ' *** Rückkehr ins Hauptmenü? (Original) *********************** REVIEWED
13510 '
13520 GOSUB 13590                                 'Rückkehr ins Hauptmenü Frage
13530 IF JA=1 THEN 18010                                          'Programmende
13540 GOSUB 13210                                    'F-Tastenleiste einblenden
13550 RETURN
13560 '
13570 ' *** Rückkehr ins Hauptmenü Frage (einfach) ******************* REVIEWED
13580 '
13590 ' OUT: JA (1=true, 0=false)
13600 Q$="Wollen Sie wirklich ins Hauptmenü?":GOSUB 12310        'Ja/Nein-Frage
13610 N$="":GOSUB 12890                                      'Nachricht löschen
13620 RETURN
13630 '
13640 ' *** Rückkehr aus Hilfe-Text? ********************************* REVIEWED
13650 '
13660 N$="Zurück? (J=Ja)":GOSUB 12890                                'Nachricht
13670 GOSUB 12320                                           'skip string concat
13680 IF JA=0 THEN 13660
13690 RETURN
13700 '
13710 ' **** VN - Subroutines, permitting one page handling *******************
13720 ' ****         by 04********** 21*******              *******************
13730 '
13740 ' **** OPEN & FIELDING VERWENDUNGSNACHWEIS ******************************
13750 '
13760 OPEN "R",#3,"B:VN.NDX",4
13770 FIELD #3,4 AS POINT0$
13780 OPEN "R",#4,"B:VN.POI",8
13790 FIELD #4,4 AS POINT1$, 4 AS POINT2$
13800 OPEN "R",#5,"B:VN.DAT",560
13810 FIELD #5,240 AS DDD1$,240 AS DDD2$,80 AS DDD3$
13820 RETURN
13830 '
13840 ' *** VERWENDUNGSNACHWEIS FÜR NEUES MITGLIED ANLEGEN ********************
13850 '
13860 ' IN: MTGLZAHL
13870 GET #3,1                            'letzte Seitennummer einlesen dann +1
13880 TMPOINT=VAL(POINT0$)+1
13890 LSET DDD1$=""                                    'leeren Record speichern
13900 LSET DDD2$=""
13910 LSET DDD3$=""
13920 PUT #5,TMPOINT
13930 LSET POINT0$=STR$(TMPOINT)             'letzte Seitennummer neu speichern
13940 PUT #3,1
13950 PUT #3,MTGLZAHL+1
13960 LSET POINT1$="0000"
13970 LSET POINT2$="0000"
13980 PUT #4,TMPOINT
13990 RETURN
14000 '
14010 ' **** ERSTE SEITE FINDEN & LESEN ***************************************
14020 '
14030 ' IN: MTGLZAHL
14040 GET #3,MTGLZAHL+1                                            'Index_Datei
14050 CPOINT=VAL(POINT0$)
14060 GET #5,CPOINT:GOSUB 15120                                       'Einlesen
14070 BLATT=1
14080 GET #4,CPOINT                                              'Pointer lesen
14090 RETURN
14100 '
14110 ' **** SEITE RÜCKWÄRTS **************************************************
14120 '
14130 TMPOINT=VAL(POINT1$)
14140 IF TMPOINT<>0 THEN 14180
14150 N$="Erste Seite":GOSUB 12890                                   'Nachricht
14160 DELAY=9999:GOSUB 10280                                             'Delay
14170 RETURN
14180 CPOINT=TMPOINT
14190 BLATT=BLATT-1
14200 GET #5,CPOINT:GOSUB 15120   
14210 GET #4,CPOINT:GOSUB 15280                             'Neue Pointer lesen
14220 HP=1:VP=1
14230 GOSUB 14490
14240 DELAY=100:GOSUB 10280                                              'Delay
14250 RETURN
14260 '
14270 ' **** SEITE VORWÄRTS  **************************************************
14280 '
14290 TMPOINT=VAL(POINT2$)
14300 IF TMPOINT=0 THEN 14370
14310 CPOINT=TMPOINT
14320 BLATT=BLATT+1
14330 GET #5,CPOINT:GOSUB 15120
14340 GET #4,CPOINT:GOSUB 15280
14350 HP=1:VP=1
14360 RETURN
14370 N$="Letzte Seite"
14380 IF INSTR(SPACE$(20),DAT$(26))=0 THEN 14420 
14390 GOSUB 12890                                                    'Nachricht
14400 DELAY=20000:GOSUB 10280                                            'Delay
14410 RETURN
14420 Q$="Letzte Seite - Neue Seite anlegen?":GOSUB 12310        'Ja/Nein-Frage
14430 N$="":GOSUB 12890                                              'Nachricht
14440 IF JA=0 THEN 14490
14450 GOSUB 14730
14460 BLATT=BLATT+1
14470 GOSUB 15280
14480 HP=1:VP=1
14490 GOSUB 13210                                    'F-Tastenleiste einblenden
14500 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=vorwärts";
14510 PRINT TAB(54);"<F9>=Blatt drucken";
14520 PRINT HOME$;" "
14530 RETURN
14540 '
14550 ' **** ERSTE SEITE EINES MITGLIEDS ANLEGEN ******************************
14560 '
14570 GET #3,1
14580 LSET POINT0$=STR$(VAL(POINT0$)+1)                                  'NDX+1
14590 PUT #3,1
14600 LSET POINT1$="0000"                                        'Page pointers
14610 LSET POINT2$="0000"
14620 PUT #4,VAL(POINT0$)
14630 LSET POINT0$=POINT0$                                          'First page
14640 PUT #3,MTGLZAHL+1
14650 LSET DDD1$=""                                                 'Datenblatt
14660 LSET DDD2$=""
14670 LSET DDD3$=""
14680 PUT #5,VAL(POINT0$)
14690 RETURN
14700 '
14710 ' **** ALLGEMEIN EINE NEUE SEITE ANLEGEN ********************************
14720 '
14730 GET #3,1                                                           'NDX+1
14740 LSET POINT0$=STR$(VAL(POINT0$)+1)
14750 PUT #3,1:TMPOINT$=POINT0$    'Vektoren der bisher letzten Seite verbiegen
14760 LSET POINT2$=POINT0$                                'Verbiege Vektoren...
14770 PUT #4,CPOINT                                         '...der alten Seite
14780 LSET POINT1$=STR$(CPOINT)                             '...der neuen Seite
14790 LSET POINT2$="0000"                                                '.. ""
14800 CPOINT=VAL(TMPOINT$)                                       'Nr neue Seite
14810 PUT #4,CPOINT
14820 LSET DDD1$=""                                           'Write empty page
14830 LSET DDD2$=""
14840 LSET DDD3$="" 
14850 PUT #5,CPOINT
14860 DATEN$=SPACE$(20)
14870 FOR II=1 TO 26
14880   DAT$(II)=DATEN$
14890 NEXT
14900 RETURN
14910 '
14920 ' **** SEITE SPEICHERN **************************************************
14930 '
14940 DATEN$=""
14950 FOR II=1 TO 12
14960   DATEN$=DATEN$+DAT$(II)
14970 NEXT
14980 LSET DDD1$=DATEN$:DATEN$=""
14990 FOR II=13 TO 24
15000   DATEN$=DATEN$+DAT$(II)
15010 NEXT
15020 LSET DDD2$=DATEN$:DATEN$=""
15030 FOR II=25 TO 26
15040   DATEN$=DATEN$+DAT$(II)
15050 NEXT 
15060 LSET DDD3$=DATEN$:DATEN$=""
15070 PUT #5,CPOINT
15080 RETURN
15090 '
15100 ' **** HACK ! (ZERSTÜCKELN DER DATEN-STRINGS) ***************************
15110 '
15120 FOR II=1 TO 12
15130   DAT$(II)=MID$(DDD1$,(II-1)*20+1,20)
15140 NEXT
15150 FOR II=13 TO 24
15160   DAT$(II)=MID$(DDD2$,(II-13)*20+1,20)
15170 NEXT 
15180 FOR II=25 TO 26
15190   DAT$(II)=MID$(DDD3$,(II-25)*20+1,20)
15200 NEXT 
15210 RETURN
15220 '
15230 ' **** UPRO DATENAUSGABE VN  ********************************************
15240 '
15250 ' IN: MTGLZAHL
15260 ' IN: NAMM$
15270 ' IN: BLATT
15280 PRINT DELHIGH$;                                          'TODO Necessary?
15290 PRINT HIGHON$
15300 PRINT FNCUR$(5,16);MTGLZAHL
15310 PRINT FNCUR$(5,29);NAMM$
15320 PRINT FNCUR$(3,8);BLATT
15330 FOR K=1 TO 26
15340   HOR=6-39*(K>13)
15350   VER=6+K+13*(K>13)
15360   FOR J=1 TO 3                                                 'Von-Datum
15370     PRINT FNCUR$(VER,HOR+(J-1)*3);MID$(DAT$(K),-1+J*2,2)
15380   NEXT
15390   FOR J=1 TO 3                                                 'Bis-Datum
15400     PRINT FNCUR$(VER,HOR+13+(J-1)*3);MID$(DAT$(K),5+J*2,2)
15410   NEXT
15420   PRINT FNCUR$(VER,HOR+26);MID$(DAT$(K),13,8)                        'Amt
15430 NEXT
15440 PRINT HIGHOFF$
15450 RETURN
15460 '
15470 ' *** Verwendungsnachweis drucken ***************************************
15480 '
15490 ' IN: BLATT
15500 ' IN: MITGLZAHL
15510 ' IN: NAMM$
15520 Q$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 12310 'Ja/Nein-Frage TODO Back to main code?
15530 IF JA=0 THEN 15750
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
15710                                           'Format: VVVVVV BBBBBB AAAAAAAA
15720 VON$=LEFT$(DAT$(KK),2)+"."+MID$(DAT$(KK),3,2)+"."+MID$(DAT$(KK),5,2)
15730 BIS$=MID$(DAT$(KK),7,2)+"."+MID$(DAT$(KK),9,2)+"."+MID$(DAT$(KK),11,2)
15740 AMT$=RIGHT$(DAT$(KK),8)
15750 RETURN
15760 '
15770 ' ### Unterprogramme "Mitgliedsliste drucken" ###########################
15780 '
15790 ' *** Mitgliedsliste drucken (Kopf)
15800 '
15810 ' IN: DATUM$
15820 LPRINT TAB(16);"MITGLIEDSLISTE 25*******************************"
15830 LPRINT 
15840 LPRINT TAB(32);"Stand:";DATUM$
15850 LPRINT
15860 LPRINT TAB(3);"Mitgl."; 
15870 LPRINT TAB(10);"Name";
15880 LPRINT TAB(36);"Vorname, Anschrift";
15890 LPRINT TAB(63);"Status";
15900 LPRINT TAB(71);"Amt"
15910 RETURN
15920 '
15930 ' *** Mitgliedsliste drucken (Mitglied)
15940 '
15950 ' IN:  MTGLZAHL
15960 ' OUT: ISBRKPRNT (1=stop printing, 0=continue printing)
15970 AMT$=""
15980 IN=INSTR(DAT$(6),"22**"):IF IN<>0 THEN AMT$=MID$(DAT$(6),IN,6):GOTO 16020
15990 IN=INSTR(DAT$(6),"23**"):IF IN<>0 THEN AMT$=MID$(DAT$(6),IN,5):GOTO 16020
16000 IN=INSTR(DAT$(6),"24**"):IF IN<>0 THEN AMT$=MID$(DAT$(6),IN,3)
16010 '
16020 LPRINT
16030 LPRINT TAB(5);MTGLZAHL;
16040 LPRINT TAB(10);DAT$(1);                                         'Nachname
16050 LPRINT TAB(36);DAT$(2);                                          'Vorname
16060 LPRINT TAB(65);"(";LEFT$(DAT$(5),1);")";                          'Status
16070 LPRINT TAB(71);AMT$                                                  'Amt
16080 LPRINT TAB(36);DAT$(10)                                           'Straße
16090 LPRINT TAB(36);DAT$(11);                                    'Postleitzahl
16100 LPRINT TAB(41);DAT$(12)                                              'Ort
16110 LPRINT TAB(36);"Tel.:";DAT$(13)                                  'Telefon
16120 LPRINT
16130 '
16140 IF INKEY$="" THEN 16170          'TODO Move back to main code? Used twice
16150 Q$="Drucken abbrechen?":GOSUB 12310                        'Ja/Nein-Frage
16160 IF JA THEN ISBRKPRNT=1 ELSE ISBRKPRNT=0
16170 RETURN
16180 '
16190 ' *** Adressaufkleber drucken
16200 '                                                           Ja/Nein-Frage
16210 N$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 12310
16220 IF JA=0 THEN RETURN
16230 LIND$=SPACE$(3)
16240 '
16250 LPRINT LIND$;DAT$(7)                                              'Anrede
16260 LPRINT LIND$;LEFT$(DAT$(2),INSTR(DAT$(2)," "));DAT$(1)  'Vorname Nachname
16270 LPRINT
16280 LPRINT LIND$;DAT$(10)                                  'Straße Hausnummer
16290 LPRINT
16300 LPRINT LIND$;DAT$(11);" ";DAT$(12)                      'Postleitzahl Ort
16310 LPRINT
16320 LPRINT
16330 LPRINT
16340 LPRINT
16350 LPRINT
16360 LPRINT
16370 RETURN
16380 '
16390 ' *** Stammsatz drucken (Bildschirmformat)
16400 '
16410 XCOL1=8
16420 XCOL2=43
16430 PRINT STRICH$
16440 PRINT
16450 PRINT TAB(31);"Mitgliedsnummer";MTGLZAHL
16460 PRINT
16470 RESTORE 16530
16480 FOR K=1 TO 7:READ A,B
16490   IF A<>0 THEN PRINT TAB(XCOL1);FENAM$(A);DAT$(A);
16500   IF B<>0 THEN PRINT TAB(XCOL2);FENAM$(B);DAT$(B);
16510   PRINT
16520 NEXT K
16530 DATA 1,0,2,15,3,16,4,0,5,17,6,18,0,19
16540 FOR K=7 TO 14
16550   PRINT TAB(XCOL1);FENAM$(K);DAT$(K);
16560   PRINT TAB(XCOL2);FENAM$(K+13);DAT$(K+13);
16570 NEXT K
16580 PRINT
16590 PRINT
16600 PRINT
16610 PRINT STRICH$
16620 RETURN
16630 '
16640 ' *** Mitgliedsvermerke drucken
16650 '
16660 ' IN: NAMM$ =
16670 LIND$=SPACE$(9)
16680 SLOT1$="___"
16690 SLOT2$="___ / ___"
16700 SLOT3$="___ / ___ / ___"
16710 '
16720 LPRINT TAB(10);"Mitgliedsnr.: ";SLOT1$;
16730 LPRINT TAB(28);"Name: ";NAMM$;
16740 LPRINT TAB(65);"Datum: ";SLOT3$
16750 LPRINT
16760 LPRINT LIND$;"26*************************** ";SLOT3$
16770 LRPINT
16780 LPRINT LIND$;"27*************************** ";SLOT3$
16790 LRPINT
16800 LPRINT LIND$;"28*************************** ";SLOT3$
16810 LRPINT
16820 LPRINT LIND$;"29*************************** ";SLOT3$
16830 LRPINT
16840 LPRINT LIND$;"30*************************** ";SLOT2$
16850 LRPINT
16860 LPRINT LIND$;"31*************************** ";SLOT3$
16870 LRPINT
16880 LPRINT LIND$;"32*************************** ";SLOT3$
16890 LRPINT
16900 LPRINT LIND$;"33*************************** ";SLOT1$
16910 LRPINT
16920 LPRINT LIND$;"34*************************** ";SLOT3$
16930 LRPINT
16940 LPRINT LIND$;"35*************************** ";SLOT3$
16950 LRPINT
16960 LPRINT LIND$;"36*************************** ";SLOT3$
16970 LRPINT
16980 LPRINT LIND$;"37*************************** ";SLOT3$
16990 LRPINT
17000 LPRINT LIND$;"38*************************** ";SLOT3$
17010 RETURN
17020 '
17030 ' ### Hilfe-Texte #######################################################
17040 '
17050 ' *** Hilfe-Text "Mitgliederverwaltung" 
17060 '
17070 MT$="HILFE":GOSUB 13100                                      'Basis Maske
17080 PRINT HOME$
17090 GOSUB 13660                                      'Rückkehr aus Hilfe-Text
17100 RETURN
17110 '
17120 ' *** Hilfe-Text "Stammsatzpflege" 
17130 '
17140 MT$="HILFE":GOSUB 13100                                      'Basis Maske
17150 PRINT HOME$
17160 GOSUB 13660                                      'Rückkehr aus Hilfe-Text
17170 RETURN
17180 '
17190 ' *** Hilfe-Text "Stammsatz anlegen"
17200 '
17210 SAVEMT$=MT$:MT$="HILFE":GOSUB 13100:MT$=SAVEMT$              'Basis Maske
17220 PRINT HOME$
17230 GOSUB 13660                                      'Rückkehr aus Hilfe-Text
17240 DATENAUSG=1                                                          '???
17250 RETURN
17260 '
17270 ' *** Hilfe-Text "Stammsatz ändern (I)"
17280 '
17290 MT$="HILFE":GOSUB 13100                                      'Basis Maske
17300 PRINT HOME$
17310 GOSUB 13660                                      'Rückkehr aus Hilfe-Text
17320 RETURN
17330 '
17340 ' *** Hilfe-Text "Stammsatz ändern (II)"
17350 '
17360 SAVEMT$=MT$:MT$="HILFE":GOSUB 13100:MT$=SAVEMT$              'Basis Maske
17370 PRINT HOME$
17380 GOSUB 13660                                      'Rückkehr aus Hilfe-Text
17390 GOSUB 13100                                                  'Basis Maske
17400 GOSUB 13210                                    'F-Tastenleiste einblenden
17410 RETURN
17420 '
17430 ' *** Hilfe-Text "Stammsatz löschen"
17440 '
17450 MT$="HILFE":GOSUB 13100                                      'Basis Maske
17460 PRINT HOME$
17470 GOSUB 13660                                      'Rückkehr aus Hilfe-Text
17480 RETURN
17490 '
17500 ' *** Hilfe-Text "Stammsatz suchen"
17510 '
17520 IF DRUCKFLAG THEN 17810                                              '???
17530 SAVEMT$=MT$:MT$="HILFE":GOSUB 13100:MT$=SAVEMT$              'Basis Maske
17540 PRINT HOME$
17550 GOSUB 13660                                      'Rückkehr aus Hilfe-Text
17560 RETURN
17570 ' 
17580 ' *** Hilfe-Text für "Stammsatz drucken"
17590 '
17600 SAVEMT$=MT$:MT$="HILFE":GOSUB 13100:MT$=SAVEMT$              'Basis Maske
17610 PRINT HOME$
17620 GOSUB 13660                                      'Rückkehr aus Hilfe-Text
17630 RETURN
17640 '
17650 ' *** Hilfe-Text "Verwendungsnachweis (I)"
17660 '
17670 MT$="HILFE":GOSUB 13100                                      'Basis Maske
17680 PRINT HOME$
17690 GOSUB 13660                                      'Rückkehr aus Hilfe-Text
17700 RETURN
17710 '
17720 ' *** Hilfe-Text "Verwendungsnachweis (II)"
17730 '
17740 SAVEMT$=MT$:MT$="HILFE":GOSUB 13100:MT$=SAVEMT$              'Basis Maske
17750 PRINT HOME$
17760 GOSUB 13660                                      'Rückkehr aus Hilfe-Text
17770 RETURN
17780 '
17790 ' *** Hilfe-Text "Mitgliedsliste drucken"
17800 '
17810 SAVEMT$=MT$:MT$="HILFE":GOSUB 13100:MT$=SAVEMT$              'Basis Maske
17820 PRINT HOME$
17830 GOSUB 13660                                      'Rückkehr aus Hilfe-Text
17840 RETURN
17850 '
17860 ' *** Programmfehler ******************************************* REVIEWED
17870 '
17880 LIND$=SPACE$(15)
17890 GOSUB 13100                                                  'Basis Maske
17900 PRINT HOME$
17910 PRINT LIND$;"Ein Programmfehler ist aufgetreten."
17920 PRINT   
17930 PRINT LIND$;"- Notieren Sie folgende Fehlerinformation:"
17940 PRINT LIND$;"    ";HIGHON$;"ERL";HIGHOFF$;" (Error Line)";HIGHON$;ERL;HIGHOFF$
17950 PRINT LIND$;"    ";HIGHON$;"ERR";HIGHOFF$;" (Error Code)";HIGHON$;ERR;HIGHOFF$
17960 PRINT LIND$;"- Drücken Sie am Rechner den Knopf hinten rechts."
17970 PRINT LIND$;"19**************************"
17980 '
17990 ' *** Programmende ********************************************* REVIEWED
18000 '
18010 PRINT FNCUR$(22,1);HIGHOFF$;CURON$;
18030 RESET
18040 'LW RUN "DISKMENU.EXE"
18050 END                                                           '1340 lines
