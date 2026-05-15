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
1760 ON ERROR GOTO 19270                                 'Enable error trapping
1770 '
1780 GOSUB 10050    'Tasten deaktivieren, ASCII Codes für Cursortasten vergeben
1790 PRINT CUROFF$;
1800 '
1810 ' *** Beginn des Mitgliederverwaltungsprogramms **************************
1820 '
1830 MT$="MITGLIEDERVERWALTUNG":MI$="G-01000":GOSUB 13130          'Basis Maske
1840 Q$="Bitte Daten-Diskette in Laufwerk B einlegen. Bereit?":GOSUB 12280
1850 IF JA=0 THEN 19400                                           'Programmende
1860 '
1870 N$="Einen Moment bitte...":GOSUB 13000    'Nachricht und F-keys ausblenden
1880 GOSUB 10960                          'Einlesen Stammdatenfelder-Definition
1890 GOSUB 11770                                   'Fielden der Stammsatz-Datei
1900 GOSUB 13800                       'Fielden der Verwendungsnachweis-Dateien
1910 GOSUB 11230                                              'Einlesen MTGLMAX
1920 GOSUB 13060                                             'Nachricht löschen
1930 '
1940 ' ################# Mitgliederverwaltungsmenü (MVW-Menü) #################
1950 '
1960 MT$="MITGLIEDERVERWALTUNG":MI$="G-01000":GOSUB 13130          'Basis Maske
1970 FKEYS$="":GOSUB 13240                           'F-Tastenleiste einblenden
1980 VNFLAG=0                                                              '???
1990 PRINT FNCUR$(4,26);"1 - Stammsatzpflege"                         'Menütext
2000 PRINT FNCUR$(6,26);"2 - Verwendungsnachweis"
2010 PRINT FNCUR$(8,26);"3 - Mitgliedsliste drucken"
2020 FKEYMAX=4:NKEYMAX=3:GOSUB 13400                              'Menü-Eingabe
2030 DRUCKFLAG=0                                                           '???
2040 IF NKEY THEN ON NKEY GOTO 2130,6030,6850
2050 ON FKEY GOTO 2060,2070,1960,2070         'Hilfe, Zurück, Anfang, Hauptmenü
2060 GOSUB 17230:GOTO 1960                                      'F1->Hilfe-Text
2070 GOSUB 13560:GOTO 2020                           'F2->Zurück, F4->Hauptmenü
2080 '
2090 ' ########################### Stammsatzpflege ############################
2100 '
2110 ' *** Stammsatzpflege-Menü ***********************************************
2120 '
2130 MT$="STAMMSATZPFLEGE":MI$="G-01100":GOSUB 13130               'Basis Maske
2140 FKEYS$="":GOSUB 13240                           'F-Tastenleiste einblenden
2150 PRINT FNCUR$(4,29);"1 - Stammsatz anlegen"
2160 PRINT FNCUR$(6,29);"2 - Stammsatz ändern"
2170 PRINT FNCUR$(8,29);"3 - Stammsatz löschen"
2180 PRINT FNCUR$(10,29);"4 - Stammsatz drucken"
2190 PRINT FNCUR$(12,29);"5 - Stammsatz suchen"
2200 FKEYMAX=4:NKEYMAX=5:GOSUB 13400                              'Menü-Eingabe
2210 IF NKEY THEN ON NKEY GOTO 2280,2820,3440,5660,3870
2220 ON FKEY GOTO 2230,1960,1960,2240         'Hilfe, Zurück, Anfang, Hauptmenü
2230 GOSUB 17450:GOTO 2130                                      'F1->Hilfe-Text
2240 GOSUB 13560:GOTO 2200                                       'F4->Hauptmenü
2250 '
2260 ' *** Stammsatz erfassen von 02************ ******************************
2270 '
2280 MTGLZAHL=MTGLMAX+1
2290 F=1
2300 DATENAUSG=1                                                           '???
2310 MT$="STAMMSATZ ANLEGEN":MI$="G-01101":GOSUB 13130             'Basis Maske
2320 FKEYS$="<F7>=Speichern":GOSUB 13240             'F-Tastenleiste einblenden
2330 GOSUB 11120                                                  'Clear DAT$()
2340 GOSUB 12790                                           'Defaultwerte setzen
2350 PRINT FNCUR$(3,31);"Mitgliedsnummer";HIGHON$;MTGLZAHL;HIGHOFF$
2360 IF DATENAUSG=1 THEN GOSUB 11650:DATENAUSG=0     'Felder und Feldwerte '???
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
2490 IF FKEY=1 THEN GOSUB 17700:GOTO 2310                       'F1->Hilfe-Text
2500 IF FKEY=2 THEN GOSUB 2590:GOTO 2130                       'Exit "Erfassen"
2510 IF FKEY=3 THEN GOSUB 2590:GOTO 1960                       'Exit "Erfassen"
2520 IF FKEY<>4 THEN 2570
2530   GOSUB 13630                                'Rückkehr ins Hauptmenü Frage
2540   IF JA=0 THEN GOSUB 13240:GOTO 2390                    'F-Keys einblenden
2550   GOSUB 2590                                              'Exit "Erfassen"
2560   GOTO 19400                                                 'Programmende
2570 IF FKEY=7 THEN GOSUB 2730:GOTO 2280
2580 '
2590 ' TMPF=1                                                              '???
2600 ' IF INSTR(SPACE$(25),DAT$(TMPF))=0 THEN 2670
2610 '   N$="DEBUG: DAT$() with 25 spaces!":GOSUB 12510              'Nachricht
2620 '   TMPF=TMPF+1:IF TMPF<(FELDMAX+1) THEN 2620
2630 '   MTGLZAHL=MTGLZAHL-1
2640 '   GOTO 2750
2650 '
2660 GOSUB 10910                                             'Debounce keyboard
2670 Q$="Wollen Sie vorher den Stammsatz speichern?":GOSUB 12280 'Ja/Nein-Frage
2680 IF JA=1 THEN 2730                                        'Nachricht und...
2690   N$="Stammsatz wird nicht gespeichert.":GOSUB 13000    '...F-Keys löschen
2700   MTGLZAHL=MTGLZAHL-1
2710   GOTO 2770
2720 '                                                            'Nachricht...
2730 N$="Stammsatz wird gespeichert...":GOSUB 13000      '...und F-Keys löschen
2740 GOSUB 12080                                       'Speichern der Felddaten
2750 GOSUB 13910                                        'Erste VN-Seite anlegen
2760 MTGLMAX=MTGLZAHL:GOSUB 11320                            'Schreiben MTGLMAX
2770 GOSUB 13060                                             'Nachricht löschen
2780 RETURN
2790 ' 
2800 ' *** Stammsatz ändern von 03************ ********************************
2810 '
2820 MT$="STAMMSATZ ÄNDERN":MI$="G-01102":GOSUB 13130              'Basis Maske
2830 FKEYS$="":GOSUB 13240                           'F-Tastenleiste einblenden
2840 PRINT FNCUR$(4,24)"Bitte geben Sie die Mitgliedsnummer" 
2850 PRINT FNCUR$(5,25)"des zu ändernden Stammsatzes ein."
2860 PRINT FNCUR$(6,23);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
2870 PRINT FNCUR$(7,24)"Beenden Sie die Eingabe mit RETURN."
2880 PRINT FNCUR$(9,39)"=>..."
2890 BADINPUT=0
2900 Z=9:S=41:LNG=3:TYP=2:MUSS=1
2910 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
2920 GOSUB 10380                                                       'Input++
2930 IF FKEY=1 THEN GOSUB 17920:GOTO 2820                               'F-Keys
2940 IF FKEY=2 THEN 2130
2950 IF FKEY=3 THEN 1960
2960 IF FKEY=4 THEN GOSUB 13560:GOTO 2920         'Rückkehr ins Hauptmenü-Frage
2970 '
2980 M=VAL(OUTTEXT$)
2990 IF M>=1 AND M<=MTGLMAX THEN 3040
3000   N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12920       'Nachricht
3010   BADINPUT=1
3020   INTEXT$=OUTTEXT$                                                    '???
3030   GOTO 2900
3040 MTGLZAHL=M
3050 N$="Stammsatz wird gelesen...":GOSUB 13000   'Nachricht und F-Keys löschen
3060 GOSUB 11830                                                   'Daten lesen
3070 GOSUB 13060                                             'Nachricht löschen
3080 '
3090 GOSUB 13130                                                   'Basis Maske
3100 FKEYS$="<F7>=Speichern":GOSUB 13240             'F-Tastenleiste einblenden
3110 PRINT FNCUR$(3,31);"Mitgliedsnummer";HIGHON$;MTGLZAHL;HIGHOFF$
3120 GOSUB 11650                                 'Felder und Feldwerte ausgeben
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
3260 IF FKEY=1 THEN GOSUB 18030:GOTO 3090
3270 IF FKEY=2 THEN GOSUB 3350:GOTO 2820               'Eingabe Mitgliedsnummer
3280 IF FKEY=3 THEN GOSUB 3350:GOTO 1960                              'MVW-Menü
3290 IF FKEY=7 THEN GOSUB 3370:GOTO 2820
3300 GOSUB 13630                             'F4-> Rückkehr ins Hauptmenü-Frage
3310 IF JA=0 THEN GOSUB 13240:GOTO 3160                      'F-Keys einblenden
3320 GOSUB 3350                                                  'Exit "Ändern'
3330 GOTO 19400                                                   'Programmende
3340 '
3350 Q$="Wollen Sie den Stammsatz speichern?":GOSUB 12280        'Ja/Nein-Frage
3360 IF JA=0 THEN RETURN
3370 N$="Stammsatz wird gespeichert...":GOSUB 13000 'Nachricht und F-Keys löschen
3380 GOSUB 12080                                               'Daten speichern
3390 GOSUB 13060                                             'Nachricht löschen
3400 RETURN
3410 '
3420 ' *** Stammsatz löschen von 04********** *********************************
3430 '
3440 MT$="STAMMSATZ LÖSCHEN":MI$="G-01103":GOSUB 13130             'Basis Maske
3450 FKEYS$="":GOSUB 13240                           'F-Tastenleiste einblenden
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
3560 IF FKEY=1 THEN GOSUB 18230:GOTO 3440
3570 IF FKEY=2 THEN 2130                                                'F-Keys
3580 IF FKEY=3 THEN 1960
3590 IF FKEY=4 THEN GOSUB 13560:GOTO 3550 'Rückkehr ins Hauptmenü-Frage 'F-keys einblenden
3600 '
3610 M=VAL(OUTTEXT$)
3620 IF M>=1 AND M<=MTGLMAX THEN 3670
3630   N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12920       'Nachricht
3640   BADINPUT=1
3650   INTEXT$=OUTTEXT$                                                    '???
3660   GOTO 3530
3670 MTGLZAHL=M
3680 N$="Stammsatz wird gelesen...":GOSUB 13000   'Nachricht und F-Keys löschen
3690 GOSUB 11830                                                   'Daten lesen
3700 GOSUB 13060                                             'Nachricht löschen
3710 '
3720 GOSUB 13130                                                   'Basis Maske
3730 PRINT FNCUR$(3,31);"Mitgliedsnummer";HIGHON$;MTGLZAHL;HIGHOFF$
3740 GOSUB 11650                                 'Felder und Feldwerte ausgeben
3750 Q$="Mitglied endgültig löschen?":GOSUB 12280                'Ja/Nein-Frage
3760 IF JA=0 THEN 3830
3770   DAT$(5)="ausgetreten"
3780   PRINT FNCUR$(FEROW(5),FECOL(5)+XOFF);HIGHON$;DAT$(5);HIGHOFF$;
3790   PRINT STRING$(FELNG(5)-LEN(DAT$(5)),".")                'LW NO semicolon
3800   N$="Stammsatz wird gespeichert...":GOSUB 13000 'Nachricht und F-Keys löschen
3810   GOSUB 12080                                             'Daten speichern
3820   GOSUB 13060                                           'Nachricht löschen
3830 GOTO 3440
3840 '
3850 ' *** Stammsatz suchen (und drucken) von 05*************** ***************
3860 '
3870 DRUCKFLAG=0
3880 MT$="STAMMSATZ SUCHEN":IF DRUCKFLAG THEN MT$="SUCHEN & DRUCKEN"
3890 MI$="G-01105":IF DRUCKFLAG THEN MI$="TODO***"
3900 GOSUB 13130                                                   'Basis Maske
3910 FKEYS$="":GOSUB 13240                           'F-Tastenleiste einblenden
3920 TMP$="Suchen":IF DRUCKFLAG THEN TMP$="Drucken" 
3930 PRINT FNCUR$(4,30)"1 - ";TMP$;" nach Nummern"
3940 PRINT FNCUR$(6,30)"2 - ";TMP$;" nach Maske"
3950 FKEYMAX=4:NKEYMAX=2:GOSUB 13400                              'Menü-Eingabe
3960 IF FKEY=1 THEN GOSUB 18370:GOTO 3880   'TODO IF DRUCKFLAG THEN ... 'F-Keys
3970 IF FKEY=2 THEN IF DRUCKFLAG THEN 6850 ELSE 2130
3980 IF FKEY=3 THEN 1960
3990 IF FKEY=4 THEN GOSUB 13560:GOTO 3950         'Rückkehr ins Hauptmenü-Frage
4000 ON NKEY GOTO 4040,4570 
4010 '
4020 ' *** Suchen nach Nummern
4030 '
4040 TMP$=STR$(1):NUMSUCH$(1)=RIGHT$(TMP$,LEN(TMP$)-1)
4050 TMP$=STR$(MTGLMAX):NUMSUCH$(2)=RIGHT$(TMP$,LEN(TMP$)-1)
4060 GOSUB 13130                                                   'Basis Maske
4070 FKEYS$="<F5>=Suchen":IF DRUCKFLAG THEN FKEYS$="<F5>=Suchen & Drucken"
4080 GOSUB 13240                                     'F-Tastenleiste einblenden
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
4200 IF FKEY=1 THEN GOSUB 18370:GOTO 4060                       'TODO DRUCKFLAG
4210 IF FKEY=2 THEN 3880
4220 IF FKEY=3 THEN 1960
4230 IF FKEY=4 THEN GOSUB 13560:GOTO 4190         'Rückkehr ins Hauptmenü-Frage
4240 IF FKEY<>5 THEN 4290
4250   SEARCH=BYNUMS
4260   NUMSUCH1=VAL(NUMSUCH$(1))
4270   NUMSUCH2=VAL(NUMSUCH$(2))
4280   IF DRUCKFLAG THEN 7640 ELSE 4840                         'Reentry, Suche
4290 M=VAL(OUTTEXT$)                                             'Handle RETURN
4300 IF M>=1 AND M<=MTGLMAX THEN 4340
4310   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 12920         'Nachricht
4320   GOSUB 4480
4330   GOTO 4190
4340 NUMSUCH$(F)=OUTTEXT$
4350 NUMSUCH1=VAL(NUMSUCH$(1))
4360 NUMSUCH2=VAL(NUMSUCH$(2))
4370 IF NUMSUCH1<=NUMSUCH2 THEN 4410
4380   N$="Ungültiger Bereich":GOSUB 12920                           'Nachricht
4390   GOSUB 4480
4400   GOTO 4190
4410 F=F+1:IF F>NUMSUCHMAX THEN F=1
4420 FKEYMASK$="12345"
4430 FKEYS$="<F5>=Suchen":IF DRUCKFLAG THEN FKEYS$="<F5>=Suchen & Drucken" 'TODO Required?
4440 GOSUB 13500                                        'F-Tastenleiste löschen
4450 GOSUB 13240                                     'F-Tastenleiste einblenden
4460 GOTO 4170
4470 '
4480 BADINPUT=1                                  'Finish handling invalid input
4490 INTEXT$=OUTTEXT$
4500 FKEYMASK$="1234"
4510 GOSUB 13500                                        'F-Tastenleiste löschen
4520 FKEYS$="":GOSUB 13240                           'F-Tastenleiste einblenden
4530 RETURN
4540 '
4550 ' *** Suchen nach Maske
4560 '
4570 GOSUB 11170                                                 'Clear SUCH$()
4580 GOSUB 13130                                                   'Basis Maske
4590 FKEYS$="<F5>=Suchen":IF DRUCKFLAG THEN FKEYS$="<F5>=Suchen & Drucken" 
4600 GOSUB 13240                                     'F-Tastenleiste einblenden
4610 GOSUB 11410                       'Feldnamen und Werteplatzhalter ausgeben
4620 F=1
4630 Z=FEROW(F):S=FECOL(F)+XOFF:LNG=FELNG(F):TYP=3
4640 FKEYMASK$="12345":CKEYMASK$="12":INTEXT$=""
4650 GOSUB 10370                                                         'Input
4660 IF FKEY=1 THEN GOSUB 18370:GOTO 4580           'TODO IF DRUCKFLAG THEN ...
4670 IF FKEY=2 THEN 3880
4680 IF FKEY=3 THEN 1960
4690 IF FKEY=4 THEN GOSUB 13560:GOTO 4650         'Rückkehr ins Hauptmenü-Frage
4700 IF FKEY<>5 THEN 4730 
4710   SEARCH=BYMASK                                                      '<F5>
4720   IF DRUCKFLAG THEN 7640 ELSE 4840                         'Reentry, Suche
4730 IF CKEY=0 THEN 4780                                    'Handle cursor keys
4740   PRINT FNCUR$(Z,S);HIGHON$;SUCH$(F);HIGHOFF$;STRING$(LNG-LEN(SUCH$(F)),".")
4750   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FELDMAX
4760   IF CKEY=CKEYDN THEN F=F+1:IF F>FELDMAX THEN F=1
4770   GOTO 4630
4780 SUCH$(F)=OUTTEXT$                                           'Handle RETURN
4790 F=F+1:IF F>FELDMAX THEN F=1
4800 GOTO 4630
4810 '
4820 ' *** Suche
4830 '
4840 GOSUB 13130                                                   'Basis Maske
4850 FKEYS$="<F5>=Suchen rückwärts <F6>=Suchen vorwärts <F7>=Adressaufkleber" 
4860 GOSUB 13240                                     'F-Tastenleiste einblenden
4870 PRINT FNCUR$(3,31);"Mitgliedsnummer"
4880 GOSUB 11410                       'Feldnamen und Werteplatzhalter ausgeben
4890 '
4900 IF SEARCH=BYMASK THEN 4950
4910 '
4920 MTGLZAHL=NUMSUCH1                      'Initialisierung Suche nach Nummern
4930 GOTO 5050
4940 '
4950 MTGLZAHL=0                               'Initialisierung Suche nach Maske
4960 GOSUB 8050                                     'next match IN/OUT:MTGLZAHL
4970 IF MTGLZAHL<>-1 THEN 5050
4980   N$="Keine Suchtreffer. Mit <F2> neue Suche.":GOSUB 12920      'Nachricht
4990   FKEYS$="":GOSUB 13240                         'F-Tastenleiste einblenden
5000   FKEYMASK$="1234":NKEYMASK="":GOSUB 13400
5010   IF FKEY=1 THEN GOSUB 18370:GOTO 4580 'TODO IF DRUCKFLAG THEN ... 'F-Keys
5020   IF FKEY=2 THEN 4580
5030   IF FKEY=3 THEN 1960
5040   IF FKEY=4 THEN GOSUB 13560:GOTO 5000       'Rückkehr ins Hauptmenü-Frage
5050 GOSUB 5500                              'Suchtreffer einlesen und anzeigen
5060 '
5070 FKEMAX=7:NKEYMAX=0:GOSUB 13400      'Menü-Eingabe 'TODO Nachricht löschen?
5080 ON FKEY GOTO 5100,5130,1960,5150,5180,5330,5560
5090 ' *** <F1> Hilfe
5100 GOSUB 18370                                                'TODO DRUCKFLAG
5110 GOTO 5070 
5120 ' *** <F2> Zurück
5130 IF SEARCH=BYNUMS THEN 4040 ELSE 4580            'TODO BYNUMS:Initialwerte?
5140 ' *** <F4> Rückkehr ins Hauptmenü
5150 GOSUB 13560                                  'Rückkehr ins Hauptmenü-Frage
5160 GOTO 5070
5170 ' *** <F5> Suchen rückwärts
5180 IF SEARCH=BYMASK THEN 5240
5190 '
5200 MTGLZAHL=MTGLZAHL-1                          'Suchen rückwärts nach Nummer
5210 IF MTGLZAHL<NUMSUCH1 THEN 5300
5220   GOSUB 5500:GOTO 5070                  'Suchtreffer einlesen und anzeigen
5230 '       
5240 TMP=MTGLZAHL                                  'Suchen rückwärts nach Maske
5250 GOSUB 7970                                     'prev match IN/OUT:MTGLZAHL
5260 IF MTGLZAHL=-1 THEN 5290
5270   GOSUB 5500:GOTO 5070                  'Suchtreffer einlesen und anzeigen
5280 '
5290 MTGLZAHL=TMP                                               'No match found
5300 N$="Erster Suchtreffer":GOSUB 12920                             'Nachricht
5310 GOTO 5070
5320 ' *** <F6> Suchen vorwärts
5330 IF SEARCH=BYMASK THEN 5390
5340 '
5350 MTGLZAHL=MTGLZAHL+1                           'Suchen vorwärts nach Nummer
5360 IF MTGLZAHL>NUMSUCH2 THEN 5450
5370   GOSUB 5500:GOTO 5070                  'Suchtreffer einlesen und anzeigen
5380 '
5390 TMP=MTGLZAHL                                    'Suche vorwärts nach Maske
5400 GOSUB 8050                                     'next match IN/OUT:MTGLZAHL
5410 IF MTGLZAHL=-1 THEN 5440
5420   GOSUB 5500:GOTO 5070                  'Suchtreffer einlesen und anzeigen
5430 '
5440 MTGLZAHL=TMP                                               'No match found
5450 N$="Letzter Suchtreffer":GOSUB 12920                            'Nachricht
5460 GOTO 5070
5470 '
5480 ' *** Suchtreffer einlesen und anzeigen
5490 '
5500 GOSUB 13060                            'Nachricht löschen 'TODO notwendig?
5510 GOSUB 11830                                       'Stammsatzdaten einlesen
5520 GOSUB 11530                                            'Feldwerte ausgeben
5530 PRINT FNCUR$(3,46);HIGHON$;STR$(MTGLZAHL);HIGHOFF$
5540 RETURN
5550 ' *** <F7> Adressaufkleber drucken
5560 GOSUB 15860                          'Ist der Drucker eingeschaltet? Frage
5570 IF JA=0 THEN 5620                              'LW TODO F-Keys einblenden?
5580   N$="Adressaufkleber wird gedruckt...":GOSUB 12920             'Nachricht
5590   GOSUB 16370                                     'Adressaufkleber drucken
5600   GOSUB 13060                                           'Nachricht löschen
5610   GOSUB 13240                                           'F-keys einblenden
5620 GOTO 5070                                                    'Neue Eingabe
5630 '
5640 ' *** Stammsatz drucken von 02************ & 04********** ****************
5650 '
5660 MT$="STAMMSATZ DRUCKEN":MI$="G-01104":GOSUB 13130             'Basis Maske
5670 FKEYS$="":GOSUB 13240                           'F-Tastenleiste einblenden
5680 PRINT FNCUR$(4,24)"Bitte geben Sie die Mitgliedsnummer"
5690 PRINT FNCUR$(5,23)"des auszudruckenden Stammsatzes ein."
5700 PRINT FNCUR$(6,23);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
5710 PRINT FNCUR$(7,24)"Beenden Sie die Eingabe mit RETURN."
5720 PRINT FNCUR$(9,39);"=>..."
5730 BADINPUT=0
5740 Z=9:S=41:LNG=3:TYP=2:MUSS=1
5750 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
5760 GOSUB 10380                                                       'Input++
5770 IF FKEY=1 THEN GOSUB 18620:GOTO 5660                               'F-Keys
5780 IF FKEY=2 THEN 2130
5790 IF FKEY=3 THEN 1960
5800 IF FKEY=4 THEN GOSUB 13560:GOTO 5760         'Rückkehr ins Hauptmenü-Frage
5810 '
5820 M=VAL(OUTTEXT$)                                             'Handle RETURN
5830   IF M>=1 AND M<=MTGLMAX THEN 5880
5840   N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12920       'Nachricht
5850   BADINPUT=1
5860   INTEXT$=OUTTEXT$                                                    '???
5870   GOTO 3530
5880 MTGLZAHL=M
5890 N$="Stammsatz wird gelesen...":GOSUB 13000   'Nachricht und F-Keys löschen
5900 GOSUB 11830                                                   'Daten lesen
5910 GOSUB 13060                                             'Nachricht löschen
5920 '
5930 GOSUB 13130                                                   'Basis Maske
5940 PRINT FNCUR$(3,31);"Mitgliedsnummer";HIGHON$;MTGLZAHL;HIGHOFF$
5950 GOSUB 11650                                 'Felder und Feldwerte ausgeben
5960 GOSUB 15860                                  'Drucker eingeschaltet? Frage
5970 IF JA=0 THEN 5990
5980 GOSUB 16550                          'Stammsatz drucken (Bildschirmformat)
5990 GOTO 5660
6000 '
6010 ' ######################### Verwendungsnachweis ##########################
6020 '
6030 GOSUB 11230                                              'Einlesen MTGLMAX
6040 VNFLAG=-1
6050 '
6060 ' Verwendungsnachweis-Menü ***********************************************
6070 '
6080 MT$="VERWENDUNGSNACHWEIS":MI$="G-01200":GOSUB 13130           'Basis Maske
6090 F=4                                                                'TODO ?
6100 FKEYS$="":GOSUB 13240                           'F-Tastenleiste einblenden
6110 PRINT CURON$;
6120 PRINT HIGHOFF$;
6130 PRINT FNCUR$(4,20)"   Bitte geben Sie die Mitgliedsnummer"
6140 PRINT FNCUR$(5,20)"des zu ändernden Verwendungsnachweises ein"
6150 PRINT FNCUR$(6,20)" und beenden Sie die Eingabe mit RETURN."
6160 PRINT FNCUR$(8,20)"                 =>..."
6170 Z=8:S=39:LNG=3:TYP=2
6180 GOSUB 10370                                                         'INPUT
6190 IF FKEY=1 THEN GOSUB 18740:GOTO 6080
6200 IF FKEY=2 OR FKEY=3 THEN GOSUB 11120:VNFLAG=0:GOTO 1960      'Clear DAT$()
6210 IF FKEY=4 THEN GOSUB 13560
6220 IF FKEY>4 THEN 6170
6230 IF CKEY<>0 THEN 6170
6240 IF LEN(OUTTEXT$)=0 THEN N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 12920:GOTO 6160 ELSE MITGLIED=VAL(OUTTEXT$):IF MITGLIED=0 THEN 6180 'Nachricht
6250 IF MITGLIED>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12920:PRINT HOME$;HIGHOFF$:GOTO 6160 'Nachricht
6260 MTGLZAHL=MITGLIED:MTGL$=STR$(MITGLIED)
6270 '
6280 ' *** Einlesen des Mitgliedsnamens ***************************************
6290 '
6300 PRINT FNCUR$(21,1);DELLINE$;
6310 GET #2,MITGLIED                                        'EINLESEN DATEN STS
6320 NNAM$=LEFT$(DD1$,25)                                 'NACHNAME RAUSFISCHEN
6330 VNAM$=MID$(DD1$,26,25)                               'VORNAME  RAUSFISCHEN
6340 '
6350 ' ** LEERZEICHEN HINTER NACHNAME ABSCHNEIDEN *****************************
6360 '
6370 FOR I=LEN(NNAM$) TO 1 STEP -1
6380   IF MID$(NNAM$,I,1)<>" " THEN NNAM$=LEFT$(NNAM$,I):I=1
6390 NEXT
6400 NAMM$=NNAM$+" "+VNAM$
6410 GOSUB 14080                                          'EINLESEN 1. VN-BLATT
6420 BLATT=1
6430 VP=1:HP=1                                                 'INITIALISIERUNG
6440 F=8
6450 GOSUB 13130                                                   'Basis Maske
6460 FKEYS$="<F7>=Speichern":GOSUB 13240             'F-Tastenleiste einblenden
6470 PRINT HIGHOFF$;
6480 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=Vorwärts";TAB(53);"<F9>=Blatt drucken";
6490 PRINT FNCUR$(3,2);"Blatt:"
6500 PRINT FNCUR$(5,2);"Mitglieds-Nr.:"
6510 PRINT FNCUR$(5,24);"Name:"
6520 MSK$="Von:  .  .   Bis:  .  .   Amt:"
6530 FOR KK=7 TO 19 
6540   PRINT FNCUR$(KK,2);MSK$;TAB(43);MSK$                    'LW NO semicolon
6550 NEXT
6560 GOSUB 15290                                                 'Ausgabe Daten
6570 LNG=2-(HP=7)*6                                    'BEGIN OF THE INPUT-LOOP
6580 TYP=2-(HP=7)
6590 Z=VP+13*(VP>13)+6
6600 S=6-39*(VP>13)-(HP<7)*(HP-1)*3+(HP>3)*(HP<7)*4-(HP=7)*26
6610 GOSUB 13060                                             'Nachricht löschen
6620 GOSUB 10370                                                       'INPUT+1
6630 IF HP<7 THEN IF ASCC=ASCRET THEN OUTTEXT$=STRING$(2-LEN(OUTTEXT$),"0")+OUTTEXT$   '+NULLEN
6640 IF CKEY<>0 OR ASCC=ASCRET THEN PRINT FNCUR$(Z,S);OUTTEXT$
6650 IF ASCC=ASCRET THEN IF HP<7 THEN MID$(DAT$(VP),-1+2*HP,2)=OUTTEXT$ ELSE DAT$(VP)=LEFT$(DAT$(VP),12)+OUTTEXT$+SPACE$(8-LEN(OUTTEXT$))
6660 IF ASCC=ASCRET AND HP<7 THEN JA=1:TEST$=OUTTEXT$:ON HP GOSUB 12680,12710,12740,12680,12710,12740:IF JA=0 THEN N$="Ungültiges Datum":GOSUB 12920:GOTO 6620 'Datums-Check 'Nachricht
6670 IF ASCC=ASCRET THEN IF HP=7 THEN HP=1:VP=VP-(VP<26)-(VP=26)*(-25):GOTO 6570 ELSE HP=HP+1:GOTO 6570     'NEUE SCREEN-POSITION
6680 IF CKEY<>0 THEN TEST$=MID$(DAT$(VP),-(HP<7)*2*HP-1-(HP=7)*14,-(HP<7)*2-(HP=7)*8):PRINT FNCUR$(Z,S);TEST$
6690 IF CKEY<>0 THEN IF HP<7 AND NOT TEST$="  " THEN JA=1:ON HP GOSUB 12680,12710,12740,12680,12710,12740:IF JA=0 THEN N$="Ungültiges Datum":GOSUB 12920:GOTO 6620 'Nachricht
6700 IF CKEY=CKEYDN THEN VP=VP-(VP<26)-(VP=26)*(-25):GOTO 6570
6710 IF CKEY=CKEYUP THEN VP=VP+(VP>1)-(VP=1)*25     :GOTO 6570
6720 IF CKEY=CKEYRT THEN HP=HP-(HP<7)-(HP=7)*(-6)   :GOTO 6570
6730 IF CKEY=CKEYLT THEN HP=HP+(HP>1)+(HP=1)*(-6)   :GOTO 6570
6740 IF FKEY=1 THEN GOSUB 18850 :GOTO 6430           'HILFE - ZU INIT UND MASKE
6750 IF FKEY=2 THEN 6080                                       'ZU MTGL-EINGABE
6760 IF FKEY=3 THEN VNFLAG=0:GOSUB 11120:GOTO 1960                'Clear DAT$()
6770 IF FKEY=4 THEN GOSUB 13560:GOSUB 13300:GOTO 6620 
6780 IF FKEY=5 THEN GOSUB 14170:GOTO 6570
6790 IF FKEY=6 THEN GOSUB 14310:GOTO 6570
6800 IF FKEY=7 THEN GOSUB 14950:GOTO 6620                 'SPEICHERN DANN INPUT
6810 IF FKEY=9 THEN GOSUB 15530:GOSUB 14500:GOTO 6610
6820 '
6830 ' ############# Mitgliedsliste drucken von 05*************** #############
6840 '
6850 MT$="MITGLIEDSLISTE DRUCKEN":MI$="G-01300":GOSUB 13130        'Basis Maske
6860 FKEYS$="":GOSUB 13240                           'F-Tastenleiste einblenden
6870 PRINT FNCUR$(4,28);"1 - Suchen & Drucken"
6880 PRINT FNCUR$(6,28);"2 - Komplett drucken"
6890 PRINT FNCUR$(8,28);"3 - Datenblatt drucken"
6900 FKEYMAX=4:NKEYMAX=3:GOSUB 13400                              'Menü-Eingabe
6910 IF FKEY=1 THEN GOSUB 19040:GOTO 6850
6920 IF FKEY=2 THEN 1960
6930 IF FKEY=3 THEN 1960
6940 IF FKEY=4 THEN GOSUB 13560:GOTO 6900
6950 ON NKEY GOTO 6990,6990,8340
6960 '
6970 ' *** Suchen & Drucken, Komplett drucken *********************************
6980 '
6990 IF NKEY=1 THEN MT$="SUCHEN & DRUCKEN":MI$="TODOAAAA"
7000 IF NKEY=2 THEN MT$="KOMPLETT DRUCKEN":MI$="TODOBBBB"
7010 GOSUB 13130                                                   'Basis Maske
7020 GOSUB 13240                                     'F-Tastenleiste einblenden
7030 PRINT FNCUR$(4,21);"Bitte geben Sie das heutige Datum ein."
7040 PRINT FNCUR$(5,22);"Beenden Sie die Eingabe mit RETURN."
7050 PRINT FNCUR$(7,35);"=>........"
7060 BADINPUT=0
7070 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
7080 Z=7:S=37:LNG=8:TYP=5:MUSS=1
7090 GOSUB 10380                                                       'Input++
7100 IF FKEY=1 THEN GOSUB 19040:GOTO 6990             'TODO Hilfe-Text anpassen
7110 IF FKEY=2 THEN 6850                                                'F-Keys
7120 IF FKEY=3 THEN 1960
7130 IF FKEY=4 THEN GOSUB 13560:GOTO 7090         'Rückkehr ins Hauptmenü-Frage
7140 '
7150 POS1=INSTR(OUTTEXT$,".")                                    'Handle RETURN
7160 POS2=INSTR(POS1+1,OUTTEXT$,".")
7170 IF POS2=0 THEN 7410
7180 POS3=INSTR(POS2+1,OUTTEXT$,".")
7190 IF POS3<>0 THEN 7410
7200 D1$=LEFT$(OUTTEXT$,POS1-1)
7210 IF LEN(D1$)=0 OR LEN(D1$)>2 THEN 7410
7220 D2$=MID$(OUTTEXT$,POS1+1,POS2-POS1-1)
7230 IF LEN(D2$)=0 OR LEN(D2$)>2 THEN 7410
7240 D3$=RIGHT$(OUTTEXT$,LEN(OUTTEXT$)-POS2)
7250 IF LEN(D3$)=0 OR LEN(D3$)>2 THEN 7410
7260 '
7270 D1=VAL(D1$):IF D1<1 OR D1>31 THEN 7430
7280 D2=VAL(D2$):IF D2<1 OR D2>12 THEN 7430
7290 D3=VAL(D3$) 
7300 ' TODO: Deeper validation
7310 D1$=STR$(D1):D1$=MID$(D1$,2,99):IF D1<10 THEN D1$="0"+D1$
7320 D2$=STR$(D2):D2$=MID$(D2$,2,99):IF D2<10 THEN D2$="0"+D2$
7330 D3$=STR$(D3):D3$=MID$(D3$,2,99):IF D3<10 THEN D3$="0"+D3$
7340 DATUM$=D1$+"."+D2$+"."+D3$
7350 '
7360 PRINT FNCUR$(7,37);HIGHON$;DATUM$;HIGHOFF$
7370 GOTO 7480
7380 '
7390 ' *** Fehlerbehandlung Datum
7400 '
7410 N$="Datum muss Format TT.MM.JJ haben. Bitte korrigieren.":GOSUB 12920 'Nachricht
7420 GOTO 7440
7430 N$="Ungültiges Datum. Bitte korrigieren.":GOSUB 12920           'Nachricht
7440 BADINPUT=1
7450 INTEXT$=OUTTEXT$                                                      '???
7460 GOTO 7080
7470 '
7480 IF NKEY=1 THEN 7580
7490 '
7500 ' *** Komplette Mitgliedliste drucken
7510   SEARCH=BYNUMS                          'Drucken komplette Mitgliedsliste
7520   NUMSUCH1=1:NUMSUCH2=MTGLMAX
7530   GOSUB 15860                              '"Drucker eingeschaltet?"-Frage
7540   IF JA=0 THEN 6990
7550   GOTO 7660                                           'LW OK-ish target...
7560 '
7570 ' *** Suchen nach Nummern und Maske
7580 DRUCKFLAG=1
7590 GOTO 3880                                  'Stammsatz suchen (und drucken)
7600 ' ---> Reentry <----------------------------------------------------------
7610 ' IN: SEARCH (BYNUMS, BYMASK)
7620 ' IN: NUMSUCH1, NUMSUCH2
7630 ' IN: SUCH$()
7640 GOSUB 15860                                '"Drucker eingeschaltet?"-Frage
7650 IF JA=0 THEN 7930
7660 N$="Mitgliedsliste wird gedruckt... Abbrechen mit Taste.":GOSUB 12920 'Nachricht
7670 GOSUB 15930                                 'Mitgliedsliste drucken (Kopf)
7680 IF SEARCH=BYMASK THEN 7810
7690 '
7700 ' *** Suchen nach Nummern
7710 FOR MTGLZAHL=NUMSUCH1 TO NUMSUCH2
7720   GOSUB 11830                                     'Stammsatzdaten einlesen
7730   IF INSTR(DAT$(5),"ausgetreten")>0 THEN 7750
7740     GOSUB 16070                         'Mitgliedsliste drucken (Mitglied)
7750   GOSUB 16280                                      'Druck abbrechen? Frage
7760   IF JA=1 THEN 7920
7770 NEXT MTGLZAHL
7780 GOTO 7920
7790 ' 
7800 ' *** Suchen nach Maske
7810 MTGLZAHL=0
7820 GOSUB 8050                                     'next match IN/OUT:MTGLZAHL
7830 IF MTGLZAHL=-1 THEN 7920
7840   GOSUB 11830            'Stammsatzdaten einlesen, IN:MTGLZAHL, OUT DAT$()
7850   IF INSTR(DAT$(5),"ausgetreten")>0 THEN 7870
7860     GOSUB 16070                         'Mitgliedsliste drucken (Mitglied)
7870   GOSUB 16280                                      'Druck abbrechen? Frage
7880   IF JA=1 THEN 7920
7890 GOTO 7830
7900 '
7910 ' *** Suchen zuende
7920 GOSUB 13060                                             'Nachricht löschen
7930 IF SEARCH=BYNUMS THEN 4040 ELSE 4570                  'LW Feels hackish...
7940 '
7950 ' *** Subroutine Prev match 
7960 ' IN: MTGLZAHL OUT: MTGLZAHL or -1 if not found
7970 MTGLZAHL=MTGLZAHL-1
7980 IF MTGLZAHL<1 THEN 8110
7990 GOSUB 8150
8000 IF NOT ISMATCH THEN 7970 
8010 RETURN
8020 '
8030 ' *** Subroutine Next match 
8040 ' IN: MTGLZAHL OUT: MTGLZAHL or -1 if not found
8050 MTGLZAHL=MTGLZAHL+1
8060 IF MTGLZAHL>MTGLMAX THEN 8110
8070 GOSUB 8150
8080 IF NOT ISMATCH THEN 8050 
8090 RETURN
8100 '
8110 MTGLZAHL=-1
8120 RETURN
8130 '
8140 ' *** Subroutine Is match? 
8150 GOSUB 11830                                                   'Daten lesen
8160 FOR K=1 TO FELDMAX
8170   IF LEFT$(SUCH$(K),1)<>"*" THEN 8200
8180     IF INSTR(DAT$(K),RIGHT$(SUCH$(K),LEN(SUCH$(K))-1))>0 THEN 8250
8190     GOTO 8290
8200   I=1
8210   IF MID$(SUCH$(K),I,1)="?" THEN I=I+1:GOTO 8210
8220   I=I-1
8230   IF INSTR(DAT$(K),RIGHT$(SUCH$(K),LEN(SUCH$(K))-I))=I+1 THEN 8250
8240   GOTO 8290
8250 NEXT K
8260 ISMATCH=1
8270 RETURN
8280 '
8290 ISMATCH=0
8300 RETURN
8310 '
8320 ' *** Datenblatt drucken *************************************************
8330 '
8340 MT$="DATENBLATT DRUCKEN":MI$="G-01303":GOSUB 13130            'Basis Maske
8350 FKEYS$="":GOSUB 13240                           'F-Tastenleiste einblenden
8360 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer"
8370 PRINT FNCUR$(5,23)"des zu druckenden Datenblattes ein."
8380 PRINT FNCUR$(7,23)"Beenden Sie die Eingabe mit RETURN."
8390 PRINT FNCUR$(9,39)"=>..."
8400 BADINPUT=0
8410 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
8420 Z=9:S=41:LNG=3:TYP=2:MUSS=1
8430 GOSUB 10380                                                       'Input++
8440 IF FKEY=1 THEN GOSUB 19040:GOTO 8340
8450 IF FKEY=2 THEN 6850                                                'F-Keys
8460 IF FKEY=3 THEN 1960
8470 IF FKEY=4 THEN GOSUB 13560:GOTO 8430         'Rückkehr ins Hauptmenü-Frage
8480 '
8490 M=VAL(OUTTEXT$)                                             'Handle RETURN
8500 IF M>=1 AND M<=MTGLMAX THEN 8550
8510   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 12920         'Nachricht
8520   BADINPUT=1
8530   INTEXT$=OUTTEXT$                                                    '???
8540   GOTO 8420
8550 MTGLZAHL=M
8560 GOSUB 15860                                '"Drucker eingeschaltet?"-Frage
8570 IF JA=1 THEN 8610
8580   GOSUB 13240                                   'F-Tastenleiste einblenden
8590   INTEXT$=OUTTEXT$
8600   GOTO 8420
8610 N$="Datenblatt drucken...":GOSUB 13000    'Nachricht und F-keys ausblenden
8620 GOSUB 11830                                          'Stammsatzdaten lesen
8630 NAMM$ = DAT$(1)+" "+DAT$(2)
8640 GOSUB 16550                          'Stammsatz drucken (Bildschirmformat)
8650 GOSUB 14080                             'Verwendungsnachweisdaten einlesen
8660 GOSUB 15560                              'Verwendungsnachweisdaten drucken
8670 GOSUB 16820                                     'Mitgliedsvermerke drucken
8680 GOSUB 13060                                             'Nachricht löschen
8690 GOTO 8340
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
10450 IF NOT BADINPUT THEN 10490
10460   GOSUB 13060                                          'Nachricht löschen
10470   PRINT FNCUR$(Z,S+P);
10480   BADINPUT=0
10490 ON TYP GOSUB 12370,12440,12500,12560,12620                    'Type check
10500 IF VALID=0 THEN 10560
10510   IF P>=LNG THEN 10440                                   'Valid character
10520   PRINT HIGHON$;A$;HIGHOFF$                              'LW NO semicolon
10530   OUTTEXT$=OUTTEXT$+A$
10540   P=P+1
10550   GOTO 10430
10560 IF ASCC<>ASCBSP THEN 10620
10570   IF P<1 THEN 10440                                            'Backspace
10580   PRINT DEL$                                             'LW NO semicolon
10590   P=P-1
10600   OUTTEXT$=LEFT$(OUTTEXT$,P)
10610   GOTO 10430
10620 IF ASCC=ASCESC THEN 10370                                         'Escape
10630 IF ASCC=ASCRET THEN 10760
10640 IF ASCC=ASCCUP THEN CKEY=CKEYUP:IF INSTR(CKEYMASK$,CHR$(CKEY+ASC0))>0 THEN 10760
10650 IF ASCC=ASCCDN THEN CKEY=CKEYDN:IF INSTR(CKEYMASK$,CHR$(CKEY+ASC0))>0 THEN 10760
10660 IF VNFLAG=0 THEN 10690
10670   IF ASCC=ASCCLT THEN CKEY=CKEYLT:IF INSTR(CKEYMASK$,CHR$(CKEY+ASC0))>0 THEN 10760 
10680   IF ASCC=ASCCRT THEN CKEY=CKEYRT:IF INSTR(CKEYMASK$,CHR$(CKEY+ASC0))>0 THEN 10760 
10690 IF ASCC<ASCF1 OR ASCC>ASCF9 THEN 10720                            'F-Keys
10700   FKEY=ASCC-ASCF1+1
10710   IF INSTR(FKEYMASK$,CHR$(FKEY+ASC1-1))>0 THEN 10810
10720 N$="Ungültige Eingabe - Bitte wiederholen":GOSUB 12920         'Nachricht
10730 BADINPUT=1
10740 GOTO 10430
10750 '
10760 IF MUSS=0 OR LEN(OUTTEXT$)>0 THEN 10800
10770   N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 12920
10780   BADINPUT=1 
10790   GOTO 10430
10800 IF CKEY<>0 THEN GOSUB 10860                     'Block cursor auto-repeat
10810 PRINT CUROFF$                                            'LW NO semicolon
10820 RETURN 
10830 '
10840 ' *** Block cursor auto-repeat ********************************* REVIEWED
10850 '
10860 A$=INKEY$:IF A$>=CHR$(ASCCUP) AND A$<=CHR$(ASCCRT) THEN 10860
10870 RETURN
10880 '
10890 ' *** Debounce keyboard **************************************** REVIEWED
10900 '
10910 FOR K=1 TO 30:A$=INKEY$:NEXT K
10920 RETURN
10930 '
10940 ' *** Einlesen Stammdatenfelder-Definition ********************* REVIEWED
10950 '
10960 OPEN "I",#1,"B:FELD.DAT"                        
10970   INPUT #1,TMPFELDMAX                                             'ignore
10980   FOR I=1 TO FELDMAX
10990     INPUT #1,F$
11000     FENAM$(I)=LEFT$(F$,14)
11010     FEMUSS(I)=-(MID$(F$,13,1)="=")                      'TODO Verify this
11020     FECOL(I)=VAL(MID$(F$,15,2))
11030     FEROW(I)=VAL(MID$(F$,17,2))
11040     FELNG(I)=VAL(MID$(F$,19,2))
11050     FETYP(I)=VAL(RIGHT$(F$,1)) 
11060   NEXT
11070 CLOSE #1
11080 RETURN
11090 '
11100 ' *** Clear DAT$() ********************************************* REVIEWED
11110 '
11120 FOR K=1 TO FELDMAX:DAT$(K)="":NEXT K
11130 RETURN
11140 '
11150 ' *** Clear SUCH$()
11160 '
11170 FOR K=1 TO FELDMAX:SUCH$(K)="":NEXT K
11180 RETURN
11190 '
11200 ' *** Lesen MTGLMAX ******************************************** REVIEWED
11210 '
11220 ' OUT: MTGLMAX
11230 OPEN "I",#1,"B:MTGLZAHL.DAT"
11240   INPUT #1,MTGLMAX$
11250   MTGLMAX=VAL(MTGLMAX$)
11260 CLOSE #1
11270 RETURN
11280 '
11290 ' *** Schreiben MTGLMAX **************************************** REVIEWED
11300 '
11310 ' IN: MTGLMAX
11320 OPEN "O",#1,"B:MTGLZAHL.DAT"
11330   PRINT #1,STR$(MTGLMAX)
11340 CLOSE #1
11350 RETURN
11360 '
11370 ' *** Maske Feldnamen und Feldplatzhalter ausgeben
11380 '
11390 ' IN: FELDMAX
11400 ' IN: MTGLZAHL
11410 FOR K=1 TO FELDMAX
11420   Z=FEROW(K)
11430   S=FECOL(K)
11440   PRINT FNCUR$(Z,S);FENAM$(K)
11450   S=S+XOFF
11460   PRINT FNCUR$(Z,S);STRING$(FELNG(K),".")
11470 NEXT K
11480 RETURN
11490 '
11500 ' *** Maske Feldwerte ausgeben
11510 '
11520 ' IN: FELDMAX
11530 PRINT HIGHON$
11540 FOR K=1 TO FELDMAX
11550   Z=FEROW(K)
11560   S=FECOL(K)+XOFF
11570   PRINT FNCUR$(Z,S);DAT$(K)
11580 NEXT K
11590 PRINT HIGHOFF$
11600 RETURN
11610 '
11620 ' *** Maske Feldnamen, Eingabefelder und Filler ausgeben
11630 '
11640 ' IN: FELDMAX
11650 FOR K=1 TO FELDMAX
11660   Z=FEROW(K)
11670   S=FECOL(K)
11680   PRINT FNCUR$(Z,S);FENAM$(K)
11690   S=S+XOFF
11700   PRINT FNCUR$(Z,S);HIGHON$;DAT$(K);HIGHOFF$;
11710   PRINT STRING$(FELNG(K)-LEN(DAT$(K)),".")               'LW NO semicolon
11720 NEXT K
11730 RETURN
11740 '
11750 ' *** FIELDen der Stammsatz-Datei ****************************** REVIEWED
11760 '
11770 OPEN "R",#2,"B:MV.DAT",462
11780 FIELD #2,236 AS DD1$,225 AS DD2$
11790 RETURN
11800 '
11810 ' *** Einlesen der Felddaten von Disk ***********************************
11820 '
11830 GET #2,MTGLZAHL
11840 GOSUB 11120                                                 'Clear DAT$()
11850 I=1
11860 FOR K=1 TO 14
11870   LNG=FELNG(K)
11880   TMP$=MID$(DD1$,I,LNG):GOSUB 11990:DAT$(K)=TMP$
11890   I=I+LNG
11900 NEXT
11910 I=1
11920 FOR K=15 TO FELDMAX
11930   LNG=FELNG(K)
11940   TMP$=MID$(DD2$,I,LNG):GOSUB 11990:DAT$(K)=TMP$
11950   I=I+LNG
11960 NEXT
11970 RETURN
11980 '
11990 FOR KK=LEN(TMP$) TO 1 STEP -1
12000   IF MID$(TMP$,KK,1)<>" " THEN 12020
12010 NEXT KK
12020 TMP$=LEFT$(TMP$,KK)
12030 RETURN
12040 '
12050 ' *** Speichern der Felddaten auf Disk **********************************
12060 '
12070 ' IN: MTGLZAHL
12080 TMP$=""
12090 FOR K=1 TO 14
12100   LNG=FELNG(K)
12110   TMP$=TMP$+DAT$(K)+SPACE$(LNG-LEN(DAT$(K)))
12120 NEXT K
12130 LSET DD1$=TMP$
12140 TMP$=""
12150 FOR K=15 TO FELDMAX
12160   LNG=FELNG(K)
12170   TMP$=TMP$+DAT$(K)+SPACE$(LNG-LEN(DAT$(K)))
12180 NEXT K
12190 LSET DD2$=TMP$
12200 TMP$=""
12210 PUT #2,MTGLZAHL
12220 RETURN
12230 '
12240 ' *** Ja/Nein-Frage und F-Tastenleiste löschen ***************** REVIEWED
12250 '
12260 ' IN:  Q$ - Frage
12270 ' OUT: JA - (1=yes, 0=no)
12280 N$=Q$+" (J=Ja/N=Nein)":GOSUB 12920                             'Nachricht
12290 GOSUB 13500                                                'Delete F-keys
12300 ANSWER$=INPUT$(1)
12310 IF ANSWER$="J" OR ANSWER$="j" THEN JA=1:RETURN
12320 IF ANSWER$="N" OR ANSWER$="n" THEN JA=0:RETURN
12330 GOTO 12300
12340 '
12350 ' *** Testbereich 1 (Alpha) *********************************************
12360 '
12370 VALID=0
12380 IF ASCC>57 AND ASCC<127 THEN VALID=1:GOTO 12400
12390 IF ASCC>31 AND ASCC<48 THEN VALID=1
12400 RETURN
12410 '
12420 ' *** Testbereich 2 (Numerisch) *****************************************
12430 '
12440 VALID=0
12450 IF ASCC>47 AND ASCC<58 THEN VALID=1
12460 RETURN
12470 '
12480 ' *** Testbereich 3 (Alphanumerisch) ************************************
12490 '
12500 VALID=0
12510 IF ASCC>31 AND ASCC<127 THEN VALID=1
12520 RETURN
12530 '
12540 ' *** Testbereich 4 (Numerisch + "/" und ".") ***************************
12550 '
12560 VALID=0
12570 IF ASCC>45 AND ASCC<58 THEN VALID=1
12580 RETURN
12590 '
12600 ' *** Testbereich 5 (Numerisch + und ".") *******************************
12610 '
12620 VALID=0
12630 IF (ASCC>=48 AND ASCC<=57) OR ASCC=46 THEN VALID=1
12640 RETURN
12650 '
12660 ' *** DATUMS-TEST (bei VN und M-Liste drucken) **************************
12670 '
12680 IF VAL(TEST$)=0 OR VAL(TEST$)>31 THEN JA=0                           'Tag
12690 RETURN
12700 '
12710 IF VAL(TEST$)=0 OR VAL(TEST$)>12 THEN JA=0                         'Monat
12720 RETURN
12730 '
12740 IF VAL(TEST$)=0 THEN JA=0                                           'Jahr
12750 RETURN
12760 '
12770 ' *** Voreinstellungen im ERFASSEN (Defaults) ******************* REVIWED
12780 '
12790 DAT$(5)="10***"
12800 DAT$(7)=""11**""
12810 DAT$(11)="12**"
12820 DAT$(12)="13******"
12830 DAT$(21)="3"
12840 DAT$(22)="Nein"
12850 DAT$(23)="Nein"
12860 DAT$(25)="Nein"
12870 RETURN
12880 '
12890 ' *** Nachricht anzeigen *************************************** REVIEWED
12900 '
12910 ' IN: N$ - Nachricht
12920 PRINT FNCUR$(21,1);DELLINE$;
12930 IF N$<>"" THEN PRINT INVON$;" ";N$;" ";INVOFF$;
12940 PRINT                              'LW workaround quirk in display driver
12950 RETURN
12960 '
12970 ' *** Nachricht anzeigen und F-Tastenleiste ausblenden ********* REVIEWED
12980 '
12990 ' IN: N$ - Nachricht
13000 GOSUB 12920                                                    'Nachricht
13010 GOSUB 13500                                                'Delete F-Keys
13020 RETURN
13030 '
13040 ' *** Nachricht löschen **************************************** REVIEWED
13050 '
13060 N$="":GOSUB 12920                                      'Nachricht löschen
13070 RETURN
13080 '
13090 ' *** Bildschirmmaske - Basis ********************************** REVIEWED
13100 '
13110 ' IN: MT$ - Menu title
13120 ' IN: MI$ - Menu ID
13130 PRINT CLS$;
13140 PRINT "00**********************";
13150 PRINT FNCUR$(1,(80-LEN(MT$))/2+1);MT$;FNCUR$(1,73);MI$
13160 PRINT STRICH$
13170 PRINT FNCUR$(21,1);
13180 PRINT STRICH$             'quirk in display driver places this in line 22
13190 RETURN
13200 '
13210 ' *** F-Tastenleiste einblenden ******************************** REVIEWED
13220 '
13230 ' IN: FKEYS$ - Additional function key labels 
13240 PRINT FNCUR$(23,1);"<F1>=Hilfe <F2>=Zurück <F3>=Anfang <F4>=Hauptmenü";
13250 IF FKEYS$<>"" THEN PRINT " ";FKEYS$ ELSE PRINT           'LW NO semicolon
13260 RETURN
13270 '
13280 ' *** restliche funktionstasten bei Verw.-nachweis ausgeben
13290 '
13300 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=Vorwärts             <F8>=Drucken";
13310 PRINT HOME$
13320 RETURN
13330 '
13340 ' *** Eingabe bei Menübildern ********************************** REVIEWED
13350 '
13360 ' IN:  NKEYMAX - max valid numeric key (1,2,3,...)
13370 ' IN:  FKEYMAX - max valid function key (1,2,3,...)
13380 ' OUT: FKEY - number of pressed function key or 0
13390 ' OUT: NKEY - number of pressed numeric key or 0
13400 NKEY=0:FKEY=0
13410 GOSUB 10910                                            'Debounce keyboard
13420 A$=INPUT$(1):ASCC=ASC(A$)
13430 IF ASCC>=ASC1 AND ASCC<=ASC1+NKEYMAX-1 THEN NKEY=ASCC-ASC1+1:RETURN
13440 IF ASCC>=ASCF1 AND ASCC<=ASCF1+FKEYMAX-1 THEN FKEY=ASCC-ASCF1+1:RETURN
13450 N$="Sie haben eine falsche Taste gedrückt - Bitte neue Auswahl treffen.":GOSUB 12920 'Nachricht
13460 GOTO 13420
13470 '
13480 ' *** F-Tastenleiste ausblenden ******************************** REVIEWED
13490 '
13500 PRINT FNCUR$(23,1);DELLINE$
13510 PRINT FNCUR$(24,1);DELLINE$;
13520 RETURN
13530 '
13540 ' *** Rückkehr ins Hauptmenü? (Original) *********************** REVIEWED
13550 '
13560 GOSUB 13630                                 'Rückkehr ins Hauptmenü Frage
13570 IF JA=1 THEN 19400                                          'Programmende
13580 GOSUB 13240                                    'F-Tastenleiste einblenden
13590 RETURN
13600 '
13610 ' *** Rückkehr ins Hauptmenü Frage (einfach) ******************* REVIEWED
13620 '
13630 ' OUT: JA (1=true, 0=false)
13640 Q$="Wollen Sie wirklich ins Hauptmenü?":GOSUB 12280        'Ja/Nein-Frage
13650 GOSUB 13060                                            'Nachricht löschen
13660 RETURN
13670 '
13680 ' *** Rückkehr aus Hilfe-Text? ********************************* REVIEWED
13690 '
13700 N$="Zurück? (J=Ja)":GOSUB 12920                                'Nachricht
13710 GOSUB 12290                                           'skip string concat
13720 IF JA=0 THEN 13700
13730 RETURN
13740 '
13750 ' **** VN - Subroutines, permitting one page handling *******************
13760 ' ****         by 04********** 21*******              *******************
13770 '
13780 ' **** OPEN & FIELDING VERWENDUNGSNACHWEIS ******************************
13790 '
13800 OPEN "R",#3,"B:VN.NDX",4
13810 FIELD #3,4 AS POINT0$
13820 OPEN "R",#4,"B:VN.POI",8
13830 FIELD #4,4 AS POINT1$, 4 AS POINT2$
13840 OPEN "R",#5,"B:VN.DAT",560
13850 FIELD #5,240 AS DDD1$,240 AS DDD2$,80 AS DDD3$
13860 RETURN
13870 '
13880 ' *** VERWENDUNGSNACHWEIS FÜR NEUES MITGLIED ANLEGEN ********************
13890 '
13900 ' IN: MTGLZAHL
13910 GET #3,1                            'letzte Seitennummer einlesen dann +1
13920 TMPOINT=VAL(POINT0$)+1
13930 LSET DDD1$=""                                    'leeren Record speichern
13940 LSET DDD2$=""
13950 LSET DDD3$=""
13960 PUT #5,TMPOINT
13970 LSET POINT0$=STR$(TMPOINT)             'letzte Seitennummer neu speichern
13980 PUT #3,1
13990 PUT #3,MTGLZAHL+1
14000 LSET POINT1$="0000"
14010 LSET POINT2$="0000"
14020 PUT #4,TMPOINT
14030 RETURN
14040 '
14050 ' **** ERSTE SEITE FINDEN & LESEN ***************************************
14060 '
14070 ' IN: MTGLZAHL
14080 GET #3,MTGLZAHL+1                                            'Index_Datei
14090 CPOINT=VAL(POINT0$)
14100 GET #5,CPOINT:GOSUB 15130                                       'Einlesen
14110 BLATT=1
14120 GET #4,CPOINT                                              'Pointer lesen
14130 RETURN
14140 '
14150 ' **** SEITE RÜCKWÄRTS **************************************************
14160 '
14170 TMPOINT=VAL(POINT1$)
14180 IF TMPOINT<>0 THEN 14210
14190 N$="Erste Seite":GOSUB 12920                                   'Nachricht
14200 RETURN
14210 CPOINT=TMPOINT
14220 BLATT=BLATT-1
14230 GET #5,CPOINT:GOSUB 15130   
14240 GET #4,CPOINT:GOSUB 15290                             'Neue Pointer lesen
14250 HP=1:VP=1
14260 GOSUB 14500
14270 RETURN
14280 '
14290 ' **** SEITE VORWÄRTS  **************************************************
14300 '
14310 TMPOINT=VAL(POINT2$)
14320 IF TMPOINT=0 THEN 14390
14330 CPOINT=TMPOINT
14340 BLATT=BLATT+1
14350 GET #5,CPOINT:GOSUB 15130
14360 GET #4,CPOINT:GOSUB 15290
14370 HP=1:VP=1
14380 RETURN
14390 N$="Letzte Seite"
14400 IF INSTR(SPACE$(20),DAT$(26))=0 THEN 14430 
14410 GOSUB 12920                                                    'Nachricht
14420 RETURN
14430 Q$="Letzte Seite - Neue Seite anlegen?":GOSUB 12280        'Ja/Nein-Frage
14440 GOSUB 13060                                            'Nachricht löschen
14450 IF JA=0 THEN 14500
14460 GOSUB 14740
14470 BLATT=BLATT+1
14480 GOSUB 15290
14490 HP=1:VP=1
14500 GOSUB 13240                                    'F-Tastenleiste einblenden
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
15530 Q$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 12280 'Ja/Nein-Frage TODO Back to main code?
15540 IF JA=0 THEN 15780                                  'LW TODO Remove this!
15550 '
15560 RETURN                                         '*** TMP Print Disable ***
15570 LIND$=SPACE$(2)
15580 '
15590 LPRINT STRICH$
15600 LPRINT
15610 LPRINT LIND$;"Blatt:";BLATT
15620 LPRINT
15630 LPRINT LIND$;"Mitglieds-Nr.:";MTGLZAHL;TAB(24);"Name:";NAMM$
15640 LPRINT
15650 FOR K=1 TO 13
15660   KK=K:GOSUB 15750
15670   LPRINT LIND$;"Von:";VON$;" Bis:";BIS$;" Amt:";AMT$;
15680   KK=K+13:GOSUB 15750
15690   LPRINT " Von:";VON$;" Bis:";BIS$;" Amt:";AMT$
15700 NEXT 
15710 LPRINT
15720 LPRINT STRICH$
15730 RETURN
15740 'Format: VVVVVV BBBBBB AAAAAAAA
15750 VON$=LEFT$(DAT$(KK),2)+"."+MID$(DAT$(KK),3,2)+"."+MID$(DAT$(KK),5,2)
15760 BIS$=MID$(DAT$(KK),7,2)+"."+MID$(DAT$(KK),9,2)+"."+MID$(DAT$(KK),11,2)
15770 AMT$=RIGHT$(DAT$(KK),8)
15780 RETURN
15790 '
15800 ' ### Unterprogramme "Mitgliedsliste drucken" ###########################
15810 '
15820 '
15830 ' *** Ist der Drucker eingeschaltet? Frage *******************************
15840 '
15850 ' OUT: JA (1=Ja, 0=Nein)
15860 Q$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 12280 'Ja/Nein-Frage
15870 GOSUB 13060                                            'Nachricht löschen
15880 RETURN
15890 '
15900 ' *** Mitgliedsliste drucken (Kopf)
15910 '
15920 ' IN: DATUM$
15930 LPRINT TAB(16);"MITGLIEDSLISTE 25*******************************"
15940 LPRINT 
15950 LPRINT TAB(32);"Stand:";DATUM$
15960 LPRINT
15970 LPRINT TAB(3);"Mitgl."; 
15980 LPRINT TAB(10);"Name";
15990 LPRINT TAB(36);"Vorname, Anschrift";
16000 LPRINT TAB(63);"Status";
16010 LPRINT TAB(71);"Amt"
16020 RETURN
16030 '
16040 ' *** Mitgliedsliste drucken (Mitglied)
16050 '
16060 ' IN:  MTGLZAHL
16070 AMT$=""
16080 IN=INSTR(DAT$(6),"22**"):IF IN<>0 THEN AMT$=MID$(DAT$(6),IN,6):GOTO 16120
16090 IN=INSTR(DAT$(6),"23**"):IF IN<>0 THEN AMT$=MID$(DAT$(6),IN,5):GOTO 16120
16100 IN=INSTR(DAT$(6),"24**"):IF IN<>0 THEN AMT$=MID$(DAT$(6),IN,3)
16110 '
16120 LPRINT
16130 LPRINT TAB(5);MTGLZAHL;
16140 LPRINT TAB(10);DAT$(1);                                         'Nachname
16150 LPRINT TAB(36);DAT$(2);                                          'Vorname
16160 LPRINT TAB(65);"(";LEFT$(DAT$(5),1);")";                          'Status
16170 LPRINT TAB(71);AMT$                                                  'Amt
16180 LPRINT TAB(36);DAT$(10)                                           'Straße
16190 LPRINT TAB(36);DAT$(11);                                    'Postleitzahl
16200 LPRINT TAB(41);DAT$(12)                                              'Ort
16210 LPRINT TAB(36);"Tel.:";DAT$(13)                                  'Telefon
16220 LPRINT
16230 RETURN
16240 '
16250 ' *** Druck abbrechen? Frage
16260 '
16270 ' OUT: JA (1=true,0=false)
16280 JA=0 
16290 IF INKEY$="" THEN 16330
16300 Q$="Drucken abbrechen?":GOSUB 12280                        'Ja/Nein-Frage
16310 IF JA=1 THEN 16330
16320 N$="Mitgliedsliste wird gedruckt... Abbrechen mit Taste.":GOSUB 12920 'Nachricht
16330 RETURN
16340 '
16350 ' *** Adressaufkleber drucken
16360 '                                                           Ja/Nein-Frage
16370 LIND$=SPACE$(3)
16380 '
16390 LPRINT LIND$;DAT$(7)                                              'Anrede
16400 LPRINT LIND$;LEFT$(DAT$(2),INSTR(DAT$(2)," "));DAT$(1)  'Vorname Nachname
16410 LPRINT
16420 LPRINT LIND$;DAT$(10)                                  'Straße Hausnummer
16430 LPRINT
16440 LPRINT LIND$;DAT$(11);" ";DAT$(12)                      'Postleitzahl Ort
16450 LPRINT
16460 LPRINT
16470 LPRINT
16480 LPRINT
16490 LPRINT
16500 LPRINT
16510 RETURN
16520 '
16530 ' *** Stammsatz drucken (Bildschirmformat)
16540 '
16550 RETURN                                         '*** TMP Print Disable ***
16560 XCOL1=8
16570 XCOL2=43
16580 PRINT STRICH$
16590 PRINT
16600 PRINT TAB(31);"Mitgliedsnummer";MTGLZAHL
16610 PRINT
16620 RESTORE 16680
16630 FOR K=1 TO 7:READ A,B
16640   IF A<>0 THEN PRINT TAB(XCOL1);FENAM$(A);DAT$(A);
16650   IF B<>0 THEN PRINT TAB(XCOL2);FENAM$(B);DAT$(B);
16660   PRINT
16670 NEXT K
16680 DATA 1,0,2,15,3,16,4,0,5,17,6,18,0,19
16690 FOR K=7 TO 14
16700   PRINT TAB(XCOL1);FENAM$(K);DAT$(K);
16710   PRINT TAB(XCOL2);FENAM$(K+13);DAT$(K+13);
16720 NEXT K
16730 PRINT
16740 PRINT
16750 PRINT
16760 PRINT STRICH$
16770 RETURN
16780 '
16790 ' *** Mitgliedsvermerke drucken
16800 '
16810 ' IN: NAMM$ =
16820 RETURN                                         '*** TMP Print Disable ***
16830 LIND$=SPACE$(9)
16840 SLOT1$="___"
16850 SLOT2$="___ / ___"
16860 SLOT3$="___ / ___ / ___"
16870 '
16880 LPRINT TAB(10);"Mitgliedsnr.: ";SLOT1$;
16890 LPRINT TAB(28);"Name: ";NAMM$;
16900 LPRINT TAB(65);"Datum: ";SLOT3$
16910 LPRINT
16920 LPRINT LIND$;"26*************************** ";SLOT3$
16930 LRPINT
16940 LPRINT LIND$;"27*************************** ";SLOT3$
16950 LRPINT
16960 LPRINT LIND$;"28*************************** ";SLOT3$
16970 LRPINT
16980 LPRINT LIND$;"29*************************** ";SLOT3$
16990 LRPINT
17000 LPRINT LIND$;"30*************************** ";SLOT2$
17010 LRPINT
17020 LPRINT LIND$;"31*************************** ";SLOT3$
17030 LRPINT
17040 LPRINT LIND$;"32*************************** ";SLOT3$
17050 LRPINT
17060 LPRINT LIND$;"33*************************** ";SLOT1$
17070 LRPINT
17080 LPRINT LIND$;"34*************************** ";SLOT3$
17090 LRPINT
17100 LPRINT LIND$;"35*************************** ";SLOT3$
17110 LRPINT
17120 LPRINT LIND$;"36*************************** ";SLOT3$
17130 LRPINT
17140 LPRINT LIND$;"37*************************** ";SLOT3$
17150 LRPINT
17160 LPRINT LIND$;"38*************************** ";SLOT3$
17170 RETURN
17180 '
17190 ' ### Hilfe-Texte #######################################################
17200 '
17210 ' *** Hilfe-Text "Mitgliederverwaltung" 
17220 '
17230 MT$="HILFE":GOSUB 13130                                      'Basis Maske
17240 PRINT HOME$
17250 PRINT IN$;"Die MITGLIEDERVERWALTUNG bietet folgende Funktionen an :"
17260 PRINT 
17270 PRINT IN$;"1 - STAMMSATZPFLEGE"
17280 PRINT IN$;"    Bearbeiten von Mitgliedsdaten gemäß dem Personalstamm-"
17290 PRINT IN$;"    blatt" 
17300 PRINT
17310 PRINT IN$;"2 - VERWENDUNGSNACHWEIS"
17320 PRINT IN$;"    Bearbeiten von Mitgliedsdaten, die Ämter umd Amtsperioden"
17330 PRINT IN$;"    eines Mitglieds enthalten"
17340 PRINT
17350 PRINT IN$;"3 - MITGLIEDSLISTE DRUCKEN"
17360 PRINT IN$;"    Drucken von Mitgliedslisten nach verschiedenen Gesichts-"
17370 PRINT IN$;"    punkten"
17380 PRINT
17390 PRINT IN$;"Wählen Sie aus, indem Sie die zugehörige Zahlentaste drücken."
17400 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
17410 RETURN
17420 '
17430 ' *** Hilfe-Text "Stammsatzpflege" 
17440 '
17450 MT$="HILFE":GOSUB 13130                                      'Basis Maske
17460 PRINT HOME$
17470 PRINT IN$;"Die STAMMSATZPFLEGE bietet folgende Funktionen an:"
17480 PRINT
17490 PRINT IN$;"1 - STAMMSATZ ANLEGEN"
17500 PRINT IN$;"    Erfassen der Daten eines neuen Mitglieds"
17510 PRINT
17520 PRINT IN$;"2 - STAMMSATZ ÄNDERN"
17530 PRINT IN$;"    Verändern eines bereits bestehenden Stammsatzes"
17540 PRINT
17550 PRINT IN$;"3 - STAMMSATZ LÖSCHEN"
17560 PRINT IN$;"    Verändern des Mitgliedsstatus auf `ausgetreten`"
17570 PRINT
17580 PRINT IN$;"4 - STAMMSATZ DRUCKEN"
17590 PRINT IN$;"    Drucken eines kompletten Stammsatzes"
17600 PRINT
17610 PRINT IN$;"5 - STAMMSATZ SUCHEN"
17620 PRINT IN$;"    Suchen vom Stammsätzen nach bestimmten Gesichtspunkten"
17630 PRINT
17640 PRINT IN$;"Wählen Sie aus, indem Sie die zugehörige Zahlentaste drücken."
17650 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
17660 RETURN
17670 '
17680 ' *** Hilfe-Text "Stammsatz anlegen"
17690 '
17700 SAVEMT$=MT$:MT$="HILFE":GOSUB 13130:MT$=SAVEMT$              'Basis Maske
17710 PRINT HOME$
17720 PRINT IN$;"Sie können jetzt Daten eines neuen Mitglieds eingeben, jedoch"
17730 PRINT IN$;"nicht die eines bestehenden Mitglieds ändern."
17740 PRINT
17750 PRINT IN$;"Die Mitgliedsnummer wird laufend vergeben."
17760 PRINT
17770 PRINT IN$;"Während der Dateneingebe können Sie mit der DELETE-Taste"
17780 PRINT IN$;"korrigieren. Ist ein Feld ausgefüllt worden, muß RETURN"
17790 PRINT IN$;"gedrückt werden, da sonst die Eingabe nicht gespeichert wird."
17800 PRINT IN$;"Mit den Cursortasten kann das Feld gewechselt werden."
17810 PRINT
17820 PRINT IN$;"`=>`-Felder müssen ausgefüllt werden."
17830 PRINT
17840 PRINT IN$;"Weitere Funktionstasten:"
17850 PRINT IN$;"<F7>  Diesen Stammsatz speichern"
17860 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
17870 DATENAUSG=1                                                          '???
17880 RETURN
17890 '
17900 ' *** Hilfe-Text "Stammsatz ändern (I)"
17910 '
17920 MT$="HILFE":GOSUB 13130                                      'Basis Maske
17930 PRINT HOME$
17940 PRINT IN$;"Sie können jetzt einen bestehenden Stammsatz ändern."
17950 PRINT
17960 PRINT IN$;"Geben Sie die Mitgliedsnummer des Stammsatzes ein."
17970 PRINT IN$;"Bei Tippfehlern können Sie mit der DELETE-Taste korrigieren."
17980 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
17990 RETURN
18000 '
18010 ' *** Hilfe-Text "Stammsatz ändern (II)"
18020 '
18030 SAVEMT$=MT$:MT$="HILFE":GOSUB 13130:MT$=SAVEMT$              'Basis Maske
18040 PRINT HOME$
18050 PRINT IN$;"Sie können jetzt einen bestehenden Stammsatz ändern."
18060 PRINT
18070 PRINT IN$;"Während der Dateneingabe können Sie mit der DELETE-Taste"
18080 PRINT IN$;"korrigieren. Ist ein Feld geändert worden, muß RETURN"
18090 PRINT IN$;"gedrückt werden, da sonst die Änderung nicht gespeichert wird."
18100 PRINT IN$;"Mit den Cursortasten kann das Feld gewechselt werden."
18110 PRINT
18120 PRINT IN$;"`=>`-Felder müssen ausgefüllt werden."
18130 PRINT
18140 PRINT IN$;"Weitere Funktionstasten:"
18150 PRINT IN$;"<F7>  Diesen Stammsatz speichern"
18160 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
18170 GOSUB 13130                                                  'Basis Maske
18180 GOSUB 13240                                    'F-Tastenleiste einblenden
18190 RETURN
18200 '
18210 ' *** Hilfe-Text "Stammsatz löschen"
18220 '
18230 MT$="HILFE":GOSUB 13130                                      'Basis Maske
18240 PRINT HOME$
18250 PRINT IN$;"Sie können jetzt den Status eines Mitglieds auf `ausgetreten`"
18260 PRINT IN$;"ändern. Damit wird der Stammsatz beim Druck von Mitglieds-"
18270 PRINT IN$;"listen nicht berüchsichtigt."
18280 PRINT IN$;"Auf keinen Fall werden die Daten WIRKLICH gelöscht."
18290 PRINT
18300 PRINT IN$;"Geben Sie die Mitgliedsnummer des Stammsatzes ein."
18310 PRINT IN$;"Bei Tippfehlern können Sie mit der DELETE-Taste korrigieren."
18320 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
18330 RETURN
18340 '
18350 ' *** Hilfe-Text "Stammsatz suchen"
18360 '
18370 IF DRUCKFLAG THEN 19040                                              '???
18380 SAVEMT$=MT$:MT$="HILFE":GOSUB 13130:MT$=SAVEMT$              'Basis Maske
18390 PRINT HOME$
18400 PRINT IN$;"Sie können jetzt Stammsätze suchen."
18410 PRINT
18420 PRINT IN$;"1 - Suchen nach Nummern"
18430 PRINT IN$;"    Geben Sie ein Intervall von Mitgliedsnummern ein."
18440 PRINT IN$;"    Es werden alle Stammsätze ausgegeben, deren Nummern im"
18450 PRINT IN$;"    Intervall liegen."
18460 PRINT
18470 PRINT IN$;"2 - Suchen nach Maske"
18480 PRINT IN$;"    Sie füllen ein Formular aus. Diejenigen Stammsätze, die"
18490 PRINT IN$;"    mit dem Formular übereinstimmen, werden ausgegeben."
18500 PRINT IN$;"    Leere Felder bezeichnen beliebige Stammsatzdaten."
18510 PRINT IN$;"    Ein * zu Beginn des Feldes bedeutet: Das folgende Wort"
18520 PRINT IN$;"    darf sich an beliebiger Stelle des Feldes befinden."
18530 PRINT IN$;"    Ein oder mehrere ? zu Beginn des Feldes stellen Platz-"
18540 PRINT IN$;"    halter für beliebige Zeichen dar."
18550 PRINT
18560 PRINT IN$;"Wählen Sie aus, indem Sie die zugehörige Zahlentaste drücken."
18570 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
18580 RETURN
18590 ' 
18600 ' *** Hilfe-Text für "Stammsatz drucken"
18610 '
18620 SAVEMT$=MT$:MT$="HILFE":GOSUB 13130:MT$=SAVEMT$              'Basis Maske
18630 PRINT HOME$
18640 PRINT IN$;"Sie können jetzt einen kompletten Stammsatz drucken, so, wie"
18650 PRINT IN$;"er auf dem Bildschirm steht."
18660 PRINT
18670 PRINT IN$;"Geben Sie die Mitgliedsnummer des Stammsatzes ein."
18680 PRINT IN$;"Bei Tippfehlern können Sie mit der DELETE-Taste korrigieren."
18690 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
18700 RETURN
18710 '
18720 ' *** Hilfe-Text "Verwendungsnachweis (I)"
18730 '
18740 MT$="HILFE":GOSUB 13130                                      'Basis Maske
18750 PRINT HOME$
18760 PRINT IN$;"Sie können jetzt einen Verwendungsnachweis ändern."
18770 PRINT
18780 PRINT IN$;"Geben Sie die Mitgliedsnummer des Verwendungsnachweises ein."
18790 PRINT IN$;"Bei Tippfehlern können Sie mit der DELETE-Taste korrigieren."
18800 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
18810 RETURN
18820 '
18830 ' *** Hilfe-Text "Verwendungsnachweis (II)"
18840 '
18850 SAVEMT$=MT$:MT$="HILFE":GOSUB 13130:MT$=SAVEMT$              'Basis Maske
18860 PRINT HOME$
18870 PRINT IN$;"Sie können jetzt einen bestehenden Verwendungsnachweis ändern."
18880 PRINT
18890 PRINT IN$;"Während der Dateneingabe können Sie mit der DELETE-Taste"
18900 PRINT IN$;"korrigieren. Ist ein Feld ausgefüllt worden, muß RETURN"
18910 PRINT IN$;"gedrückt werden, da sonst die Änderung nicht gespeichert"
18920 PRINT IN$;"wird. Mit den Cursortasten kann das Feld gewechselt werden."
18930 PRINT
18940 PRINT IN$;"Weitere Funktionstasten:"
18950 PRINT IN$;"<F5>  Zurückblättern (ohne die jetzige Seite zu speichern)"
18960 PRINT IN$;"<F6>  Vorblättern (ohne die jetzige Seite zu speichern)"
18970 PRINT IN$;"<F7>  Aktuelle Seite speichern"
18980 PRINT IN$;"<F9>  Aktuelle Seite drucken"
18990 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
19000 RETURN
19010 '
19020 ' *** Hilfe-Text "Mitgliedsliste drucken"
19030 '
19040 SAVEMT$=MT$:MT$="HILFE":GOSUB 13130:MT$=SAVEMT$              'Basis Maske
19050 PRINT HOME$
19060 PRINT IN$;"Sie können jetzt eine Mitgliedsliste drucken." 
19070 PRINT
19080 PRINT IN$;"1 - Suchen & Drucken"
19090 PRINT IN$;"    Sie können Stammsätze nach verschiedenen Gesichtspunkten"
19100 PRINT IN$;"    suchen und drucken."
19110 PRINT
19120 PRINT IN$;"2 - Komplett drucken"
19130 PRINT IN$;"    Es wird eine komplette Mitgliedsliste ausgedruckt."
19140 PRINT IN$;"    Ausgetretene Mitglieder werden nicht berücksichtigt."
19150 PRINT
19160 PRINT IN$;"3 - Datenblatt drucken"
19170 PRINT IN$;"    Der Stammsatz und der Verwendungsnachweis werden im"
19180 PRINT IN$;"    Bildschirmformat mit einer Liste von Mitgliedsvermerken"
19190 PRINT IN$;"    ausgedruckt."
19200 PRINT
19210 PRINT IN$;"Wählen Sie aus, indem Sie die zugehörige Zahlentaste drücken."
19220 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
19230 RETURN
19240 '
19250 ' *** Programmfehler ******************************************* REVIEWED
19260 '
19270 LIND$=SPACE$(15)
19280 GOSUB 13130                                                  'Basis Maske
19290 PRINT HOME$
19300 PRINT LIND$;"Ein Programmfehler ist aufgetreten."
19310 PRINT   
19320 PRINT LIND$;"- Notieren Sie folgende Fehlerinformation:"
19330 PRINT LIND$;"    ";HIGHON$;"ERL";HIGHOFF$;" (Error Line)";HIGHON$;ERL;HIGHOFF$
19340 PRINT LIND$;"    ";HIGHON$;"ERR";HIGHOFF$;" (Error Code)";HIGHON$;ERR;HIGHOFF$
19350 PRINT LIND$;"- Drücken Sie am Rechner den Knopf hinten rechts."
19360 PRINT LIND$;"19**************************"
19370 '
19380 ' *** Programmende ********************************************* REVIEWED
19390 '
19400 PRINT FNCUR$(22,1);HIGHOFF$;CURON$;
19410 RESET
19420 'LW RUN "DISKMENU.EXE"
19430 END                                                           '1596 lines
