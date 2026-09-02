1000 ' RELEASE 01***** Neubearbeitung durch 04**********
1010 '
1020 ' LW 31-JAN-2026: Load MSBASIC.COM with /S:560 /F:6
1030 '
1040 '
1050 ' Weitere Variablen
1060 ' -----------------
1070 ' F        - Feldindex 1..27 (FLDMAX)
1080 ' I$       - Lokale Texteinrückung
1100 ' MTGLMAX  - Höchste auf Disk gespeicherte Mitgliedsnummer
1110 ' MTGLZAHL - Momentane Mitgliedsnummer
1120 '
1130 ' TIP: Do not let the cursor slip into line 25 -> display will scroll up
1140 ' TIP: Use semicolons with PRINT carefully -> display driver bug
1150 '
1160 ' *** Initialisierung, Dimensionierung ***********************************
1170 '
1180 DEFINT A-Z
1190 '
1200 TRUE=-1
1210 FALSE=0
1220 '
1230 FLDMAX=27                                        'Anzahl Stammsatzfelder
1240 DIM FLDNAM$(FLDMAX)                                   'Name des Feldes
1250 DIM FLDISMAND(FLDMAX)                         'Muss-Feld (TRUE, FALSE)
1260 DIM FLDROW(FLDMAX)                       'Zeile des Feldnamens (1..25)
1270 DIM FLDCOL(FLDMAX)                      'Spalte des Feldnamens (1..80)
1280 DIM FLDLNG(FLDMAX)                                   'Länge des Feldes
1290 DIM FLDTYP(FLDMAX)                                     'Typ des Feldes
1300 '
1310 DIM FLDDAT$(FLDMAX)                                     'Stammsatzfelddaten
1320 DIM FLDSEARCH$(FLDMAX)                                  'Stammsatzsuchdaten
1330 '
1340 NUMSEARCHMAX=2
1350 DIM NUMSEARCH$(NUMSEARCHMAX)
1360 DIM NUMSEARCHROW(NUMSEARCHMAX)
1370 NUMSEARCHROW(1)=4
1380 NUMSEARCHROW(2)=6
1390 '
1400 BYNUMS=1                                             'Werte von SEARCHMODE
1410 BYMASK=2
1420 '
1430 BSP$   =CHR$(8)                                                 'Backspace
1440 DEL$   =BSP$+"."+BSP$                                              'Delete
1450 ESC$   =CHR$(27)                                                   'Escape
1460 ROWSEP$=STRING$(79,"-")                                            'Strich
1470 '
1480 CLS$    =ESC$+"E"                                      'Bildschirm löschen
1490 INVON$  =ESC$+"p"                                              'Invers ein
1500 INVOFF$ =ESC$+"q"                                              'Invers aus
1510 CURON$  =ESC$+"y5"                                             'Cursor ein
1520 CUROFF$ =ESC$+"x5"                                             'Cursor aus
1530 HIGHON$ =ESC$+"("                                           'Helle Schrift
1540 HIGHOFF$=ESC$+")"                                          'Dunkle Schrift
1550 DELLINE$=ESC$+"K"                                           'Zeile löschen
1560 DELHIGH$=ESC$+"+"                                'LW Helle Schrift löschen
1570 '
1580 DEF FNCUR$(Z,S)=ESC$+"Y"+CHR$(31+Z)+CHR$(31+S)       'Cursor positionieren
1590 HOME$=FNCUR$(3,1)                                    'Cursor Home Position
1600 ' 
1610 CKEYUP=1                                    'Werte von CKEY (Cursortasten)
1620 CKEYDN=2
1630 CKEYLT=3
1640 CKEYRT=4
1650 '
1660 ASCBSP=8    'Werte von ASCC (ASCII-Code des ersten eingegebenen Feldwerts)
1670 ASCRET=13
1680 ASCESC=27
1690 ASC0=48
1700 ASC1=49
1710 ASCCUP=200
1720 ASCCDN=201
1730 ASCCLT=202
1740 ASCCRT=203
1750 ASCF1=241
1760 ASCF4=244
1770 ASCF9=249
1780 ASCF9=249
1790 '
1800 HELPIDMAX=23                                     'Anzahl Hilfe-Text-Seiten
1810 DIM HELPFIRSTLINE(HELPIDMAX)
1820 DIM HELPLASTLINE(HELPIDMAX)
1830 '
1840 G0100000=1                                           'Mitgliederverwaltung
1850 G0110000=2                                                'Stammsatzpflege
1860 G0110100=3                                              'Stammsatz anlegen
1870 G0110201=4                                           'Stammsatz ändern (I)
1880 G0110202=5                                          'Stammsatz ändern (II)
1890 G0110300=6                                              'Stammsatz löschen
1900 G0110400=7                                              'Stammsatz drucken
1910 G0110501=8                                           'Stammsatz suchen (I)
1920 G0110510=9                                  'Stammsatz suchen nach Nummern
1930 G0110520=10                                   'Stammsatz suchen nach Maske
1940 G0110502=11                           'Stammsatz suchen (II) Keine Treffer
1950 G0110503=12                               'Stammsatz suchen (III) Blättern
1960 G0120001=13                                       'Verwendungsnachweis (I)
1970 G0120002=14                                      'Verwendungsnachweis (II)
1980 G0130000=15                                        'Mitgliedsliste drucken
1990 G0130101=16                                    'Suchen & Drucken (I) Datum
2000 G0130102=17                                         'Suchen & Drucken (II)
2010 G0130110=18                                 'Suchen & Drucken nach Nummern
2020 G0130120=19                                   'Suchen & Drucken nach Maske
2030 G0130103=20                           'Suchen & Drucken (II) Keine Treffer
2040 G0130200=21                                           'Komplettdruck Datum
2050 G0130300=22                                            'Datenblatt drucken
2060 '
2070 COLSEP=14           'Offset Colum Feldname to Column Wertefeld (Pünktchen)
2080 '
2090 ON ERROR GOTO 18100                               'Programmfehler abfangen
2100 '
2110 GOSUB 10050    'Tasten deaktivieren, ASCII-Codes für Cursortasten vergeben
2120 PRINT CUROFF$;
2130 '
2140 ' *** Beginn des Mitgliederverwaltungsprogramms **************************
2150 '
2160 MT$="MITGLIEDERVERWALTUNG":MI$="G-010000":GOSUB 13690         'Basis-Maske
2170 Q$="Bitte Daten-Diskette in Laufwerk B einlegen. Bereit?":GOSUB 12580 'Ja/Nein-Frage
2180 IF NOT ISYES THEN 18230                                      'Programmende
2190 '
2200 N$="Einen Moment bitte...":GOSUB 13480                 'Nachricht anzeigen
2210 GOSUB 11500                                                 'MTGLMAX lesen
2220 GOSUB 10960                              'Stammsatzfeld-Definitionen lesen
2230 GOSUB 12090                                   'Fielden der Stammsatz-Datei
2240 GOSUB 14360                       'Fielden der Verwendungsnachweis-Dateien
2250 GOSUB 17770      'Fielden der Hilfe-Text-Dateien, lesen der Zeilenbereiche
2260 GOSUB 13620                                             'Nachricht löschen
2270 '
2280 ' ###################### Mitgliederverwaltungs-Menü ######################
2290 '
2300 MT$="MITGLIEDERVERWALTUNG":MI$="G-010000":GOSUB 13690         'Basis-Maske
2310 FKEYS$="":GOSUB 13800                                 'F-Tasten einblenden
2320 PRINT FNCUR$(4,28);"1 - Stammsatzpflege"
2330 PRINT FNCUR$(6,28);"2 - Verwendungsnachweis"
2340 PRINT FNCUR$(8,28);"3 - Mitgliedsliste drucken"
2350 FKEYMAX=4:NKEYMAX=3:GOSUB 13960                              'Menü-Eingabe
2360 IF FKEY THEN ON FKEY GOTO 2380,2390,2300,2390
2370 ON NKEY GOTO 2450,7100,7930
2380 HELPID=G0100000:GOSUB 17930:GOTO 2300                          'Hilfe-Text
2390 GOSUB 14120:GOTO 2350                                'Zum Hauptmenü?-Frage
2400 '
2410 ' ########################### Stammsatzpflege ############################
2420 '
2430 ' *** Stammsatzpflege-Menü ***********************************************
2440 '
2450 MT$="STAMMSATZPFLEGE":MI$="G-011000":GOSUB 13690              'Basis-Maske
2460 FKEYS$="":GOSUB 13800                                 'F-Tasten einblenden
2470 PRINT FNCUR$(4,30);"1 - Stammsatz anlegen"
2480 PRINT FNCUR$(6,30);"2 - Stammsatz ändern"
2490 PRINT FNCUR$(8,30);"3 - Stammsatz löschen"
2500 PRINT FNCUR$(10,30);"4 - Stammsatz drucken"
2510 PRINT FNCUR$(12,30);"5 - Stammsatz suchen"
2520 FKEYMAX=4:NKEYMAX=5:GOSUB 13960                              'Menü-Eingabe
2530 IF FKEY THEN ON FKEY GOTO 2550,2300,2300,2560
2540 ON NKEY GOTO 2600,3090,3750,4170,4530
2550 HELPID=G0110000:GOSUB 17930:GOTO 2450                          'Hilfe-Text
2560 GOSUB 14120:GOTO 2520                                'Zum Hauptmenü?-Frage
2570 '
2580 ' *** Stammsatz anlegen von 02************ *******************************
2590 '
2600 MTGLZAHL=MTGLMAX+1
2610 F=1
2620 GOSUB 11390                                                 'FLDDAT$() leeren
2630 GOSUB 13350                                     'Voreinstellungen zuweisen
2640 MT$="STAMMSATZ ANLEGEN":MI$="G-011010":GOSUB 13690            'Basis-Maske
2650 FKEYS$="<F7>=Speichern":GOSUB 13800                   'F-Tasten einblenden
2660 GOSUB 12040                                      'Mitgliedsnummer ausgeben
2670 GOSUB 11780          'Feldnamen und Feldwerte mit Feldplatzhalter ausgeben
2680 Z=FLDROW(F):S=FLDCOL(F)+COLSEP:LNG=FLDLNG(F):TYP=FLDTYP(F):ISMAND=FLDISMAND(F)
2690 FKEYMASK$="12347":CKEYMASK$="12":TEXTIN$=FLDDAT$(F)
2700 GOSUB 10360                                                  'Feld-Eingabe
2710 '
2720 IF ASCC<>ASCRET THEN 2760                              'RETURN verarbeiten
2730   FLDDAT$(F)=TEXTOUT$
2740   F=F+1:IF F>FLDMAX THEN F=1
2750   GOTO 2680
2760 IF CKEY=0 THEN 2810                              'Cursortasten verarbeiten
2770   PRINT FNCUR$(Z,S);HIGHON$;FLDDAT$(F);HIGHOFF$;STRING$(LNG-LEN(FLDDAT$(F)),".")
2780   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FLDMAX
2790   IF CKEY=CKEYDN THEN F=F+1:IF F>FLDMAX THEN F=1
2800   GOTO 2680
2810 IF FKEY=1 THEN HELPID=G0110100:GOSUB 17930:GOTO 2640           'Hilfe-Text
2820 IF FKEY=2 THEN GOSUB 2920:GOTO 2450              'Exit "Stammsatz anlegen"
2830 IF FKEY=3 THEN GOSUB 2920:GOTO 2300              'Exit "Stammsatz anlegen"
2840 IF FKEY<>4 THEN 2890
2850   GOSUB 14200                              'Zum Hauptmenü?-Frage (einfach)
2860   IF NOT ISYES THEN GOSUB 13800:GOTO 2700             'F-Tasten einblenden
2870   GOSUB 2920                                     'Exit "Stammsatz anlegen"
2880   GOTO 18230                                                 'Programmende
2890 IF FKEY=7 THEN GOSUB 2990:GOTO 2600            'Exit "Stammsatz anlegen" 2
2900 '
2910 ' *** SUB Exit "Stammsatz anlegen"
2920 GOSUB 10910                                           'Tastatur entprellen
2930 Q$="Wollen Sie vorher den Stammsatz speichern?":GOSUB 12580 'Ja/Nein-Frage
2940 IF ISYES THEN 2990
2950   MTGLZAHL=MTGLZAHL-1
2960   GOTO 3040
2970 '                                                            
2980 ' *** SUB Exit "Stammsatz anlegen" 2
2990 N$="Stammsatz wird gespeichert...":GOSUB 13560 'Nachricht anzeigen, F-Tasten ausblenden
3000 GOSUB 12370                                           'Stammsatz speichern
3010 GOSUB 14470                       'Erste Verwendungsnachweis-Seite anlegen
3020 MTGLMAX=MTGLZAHL:GOSUB 11590                             'MTGLMAX speichen
3030 '
3040 GOSUB 13620                                             'Nachricht löschen
3050 RETURN
3060 ' 
3070 ' *** Stammsatz ändern von 03************ ********************************
3080 '
3090 MT$="STAMMSATZ ÄNDERN":MI$="G-011020":GOSUB 13690             'Basis-Maske
3100 FKEYS$="":GOSUB 13800                                 'F-Tasten einblenden
3110 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer" 
3120 PRINT FNCUR$(5,24)"des zu ändernden Stammsatzes ein."
3130 PRINT FNCUR$(6,22);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
3140 PRINT FNCUR$(7,23)"Beenden Sie die Eingabe mit RETURN."
3150 PRINT FNCUR$(9,38)"=>..."
3160 ISINPUTOK=TRUE
3170 FKEYMASK$="1234":CKEYMASK$="":TEXTIN$=""
3180 Z=9:S=40:LNG=3:TYP=2:ISMAND=TRUE
3190 GOSUB 10370                                                'Feld-Eingabe+1
3200 IF FKEY=1 THEN HELPID=G0110201:GOSUB 17930:GOTO 3090 'Hilfe-Text 'TODO: Wert geht verloren
3210 IF FKEY=2 THEN 2450
3220 IF FKEY=3 THEN 2300
3230 IF FKEY=4 THEN GOSUB 14120:GOTO 3190                 'Zum Hauptmenü?-Frage
3240 '
3250 M=VAL(TEXTOUT$)                                        'RETURN verarbeiten
3260 IF M>=1 AND M<=MTGLMAX THEN 3300
3270   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 13480         'Nachricht anzeigen
3280   ISINPUTOK=FALSE
3290   GOTO 3180
3300 MTGLZAHL=M
3310 N$="Stammsatz wird gelesen...":GOSUB 13560 'Nachricht anzeigen, F-Tasten ausblenden
3320 GOSUB 12150                                               'Stammsatz lesen
3330 GOSUB 13620                                             'Nachricht löschen
3340 '
3350 GOSUB 13690                                                   'Basis-Maske
3360 FKEYS$="<F7>=Speichern":GOSUB 13800                   'F-Tasten einblenden
3370 GOSUB 12040                                      'Mitgliedsnummer ausgeben
3380 GOSUB 11780          'Feldnamen und Feldwerte mit Feldplatzhalter ausgeben
3390 F=1
3400 Z=FLDROW(F):S=FLDCOL(F)+COLSEP:LNG=FLDLNG(F):TYP=FLDTYP(F):ISMAND=FLDISMAND(F)
3410 FKEYMASK$="12347":CKEYMASK$="12":TEXTIN$=FLDDAT$(F)
3420 GOSUB 10360                                                  'Feld-Eingabe
3430 '
3440 IF ASCC<>ASCRET THEN 3480                              'RETURN verarbeiten
3450   FLDDAT$(F)=TEXTOUT$
3460   F=F+1:IF F>FLDMAX THEN F=1
3470   GOTO 3400
3480 IF CKEY=0 THEN 3530                              'Cursortasten verarbeiten
3490   PRINT FNCUR$(Z,S);HIGHON$;FLDDAT$(F);HIGHOFF$;STRING$(LNG-LEN(FLDDAT$(F)),".")
3500   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FLDMAX
3510   IF CKEY=CKEYDN THEN F=F+1:IF F>FLDMAX THEN F=1
3520   GOTO 3400
3530 IF FKEY=1 THEN HELPID=G0110202:GOSUB 17930:GOTO 3350           'Hilfe-Text
3540 IF FKEY=2 THEN GOSUB 3640:GOTO 3090               'Exit "Stammsatz ändern"
3550 IF FKEY=3 THEN GOSUB 3640:GOTO 2300               'Exit "Stammsatz ändern"
3560 IF FKEY<>4 THEN 3610 
3570   GOSUB 14200                              'Zum Hauptmenü?-Frage (einfach)
3580   IF NOT ISYES THEN GOSUB 13800:GOTO 3420             'F-Tasten einblenden
3590   GOSUB 3640                                      'Exit "Stammsatz ändern"
3600   GOTO 18230                                                 'Programmende
3610 IF FKEY=7 THEN GOSUB 3680:GOTO 3090             'Exit "Stammsatz ändern" 2
3620 '
3630 ' *** SUB Exit "Stammsatz ändern"
3640 Q$="Wollen Sie vorher den Stammsatz speichern?":GOSUB 12580 'Ja/Nein-Frage
3650 IF NOT ISYES THEN RETURN
3660 '
3670 ' *** SUB Exit "Stammsatz ändern" 2
3680 N$="Stammsatz wird gespeichert...":GOSUB 13560 'Nachricht anzeigen, F-Tasten ausblenden
3690 GOSUB 12370                                           'Stammsatz speichern
3700 GOSUB 13620                                             'Nachricht löschen
3710 RETURN
3720 '
3730 ' *** Stammsatz löschen von 04********** *********************************
3740 '
3750 MT$="STAMMSATZ LÖSCHEN":MI$="G-011030":GOSUB 13690            'Basis-Maske
3760 FKEYS$="":GOSUB 13800                                 'F-Tasten einblenden
3770 PRINT FNCUR$(4,23);"Bitte geben Sie die Mitgliedsnummer"         'Menütext
3780 PRINT FNCUR$(5,24);"des zu löschenden Mitglieds ein."
3790 PRINT FNCUR$(6,22);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
3800 PRINT FNCUR$(7,15);"(Dabei wird nur der Status des Mitglieds verändert.)"
3810 PRINT FNCUR$(8,23);"Beenden Sie die Eingabe mit RETURN."
3820 PRINT FNCUR$(10,38);"=>..."
3830 ISINPUTOK=TRUE
3840 FKEYMASK$="1234":CKEYMASK$="":TEXTIN$=""
3850 Z=10:S=40:LNG=3:TYP=2:ISMAND=TRUE
3860 GOSUB 10370                                                'Feld-Eingabe+1
3870 IF FKEY=1 THEN HELPID=G0110300:GOSUB 17930:GOTO 3750 'Hilfe-Text 'TODO: Wert geht verloren
3880 IF FKEY=2 THEN 2450
3890 IF FKEY=3 THEN 2300
3900 IF FKEY=4 THEN GOSUB 14120:GOTO 3860                 'Zum Hauptmenü?-Frage
3910 '
3920 M=VAL(TEXTOUT$)                                        'RETURN verarbeiten
3930 IF M>=1 AND M<=MTGLMAX THEN 3970
3940   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 13480         'Nachricht anzeigen
3950   ISINPUTOK=FALSE
3960   GOTO 3850
3970 MTGLZAHL=M
3980 N$="Stammsatz wird gelesen...":GOSUB 13560 'Nachricht anzeigen, F-Tasten ausblenden
3990 GOSUB 12150                                               'Stammsatz lesen
4000 '
4010 GOSUB 13690                                                   'Basis-Maske
4020 GOSUB 12040                                      'Mitgliedsnummer ausgeben
4030 GOSUB 11780          'Feldnamen und Feldwerte mit Feldplatzhalter ausgeben
4040 Q$="Mitglied endgültig löschen?":GOSUB 12580                'Ja/Nein-Frage
4050 IF NOT ISYES THEN 4130
4060   F=5
4070   FLDDAT$(F)="ausgetreten"
4080   Z=FLDROW(F):S=FLDCOL(F)+COLSEP:LNG=FLDLNG(F)
4090   PRINT FNCUR$(Z,S);HIGHON$;FLDDAT$(F);HIGHOFF$;STRING$(LNG-LEN(FLDDAT$(F)),".")               
4100   N$="Stammsatz wird gespeichert...":GOSUB 13560 'Nachricht anzeigen, F-Tasten ausblenden
4110   GOSUB 12370                                         'Stammsatz speichern
4120   GOSUB 13620                                           'Nachricht löschen
4130 GOTO 3750
4140 '
4150 ' *** Stammsatz drucken von 02************ & 04********** ****************
4160 '
4170 MT$="STAMMSATZ DRUCKEN":MI$="G-011040":GOSUB 13690            'Basis-Maske
4180 FKEYS$="":GOSUB 13800                                 'F-Tasten einblenden
4190 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer"
4200 PRINT FNCUR$(5,23)"des auszudruckenden Stammsatzes ein."
4210 PRINT FNCUR$(6,22);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
4220 PRINT FNCUR$(7,23)"Beenden Sie die Eingabe mit RETURN."
4230 PRINT FNCUR$(9,38);"=>..."
4240 ISINPUTOK=TRUE
4250 FKEYMASK$="1234":CKEYMASK$="":TEXTIN$=""
4260 Z=9:S=40:LNG=3:TYP=2:ISMAND=TRUE
4270 GOSUB 10370                                                'Feld-Eingabe+1
4280 IF FKEY=1 THEN HELPID=G0110400:GOSUB 17930:GOTO 4170 'Hilfe-Text 'TODO: Wert geht verloren
4290 IF FKEY=2 THEN 2450
4300 IF FKEY=3 THEN 2300
4310 IF FKEY=4 THEN GOSUB 14120:GOTO 4270                 'Zum Hauptmenü?-Frage
4320 '
4330 M=VAL(TEXTOUT$)                                        'RETURN verarbeiten
4340   IF M>=1 AND M<=MTGLMAX THEN 4380
4350   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 13480         'Nachricht anzeigen
4360   ISINPUTOK=FALSE
4370   GOTO 4260
4380 MTGLZAHL=M
4390 N$="Stammsatz wird gelesen...":GOSUB 13560 'Nachricht anzeigen, F-Tasten ausblenden
4400 GOSUB 12150                                               'Stammsatz lesen
4410 GOSUB 13620                                             'Nachricht löschen
4420 '
4430 GOSUB 13690                                                   'Basis-Maske
4440 GOSUB 12040                                      'Mitgliedsnummer ausgeben
4450 GOSUB 11780          'Feldnamen und Feldwerte mit Feldplatzhalter ausgeben
4460 GOSUB 16410                                         'Drucker bereit?-Frage
4470 IF NOT ISYES THEN 4490
4480   GOSUB 17100                        'Stammsatz drucken (Bildschirmformat)
4490 GOTO 4170
4500 '
4510 ' *** Stammsatz suchen (und drucken) von 05*************** ***************
4520 '
4530 ISPRINT=FALSE
4540 MT$="STAMMSATZ SUCHEN":IF ISPRINT THEN MT$="SUCHEN & DRUCKEN" 'TODO Einsprung Suchen & Drucken
4550 MI$="G-011050":IF ISPRINT THEN MI$="G-013010"
4560 GOSUB 13690                                                   'Basis-Maske
4570 FKEYS$="":GOSUB 13800                                 'F-Tasten einblenden
4580 TMP$="Suchen":IF ISPRINT THEN TMP$="Suchen & drucken" 
4590 TMP1=29:IF ISPRINT THEN TMP1=24
4600 PRINT FNCUR$(4,TMP1)"1 - ";TMP$;" nach Nummern"
4610 PRINT FNCUR$(6,TMP1)"2 - ";TMP$;" nach Maske"
4620 NKEYMAX=2:FKEYMAX=4:GOSUB 13960                              'Menü-Eingabe
4630 IF NKEY THEN ON NKEY GOTO 4730,5330
4640 IF FKEY<>1 THEN 4680 
4650   HELPID=G0110501:IF ISPRINT THEN HELPID=G0130102
4660   GOSUB 17930                                                  'Hilfe-Text
4670   GOTO 4540
4680 IF FKEY=2 THEN IF ISPRINT THEN 8060 ELSE 2450    'Absprung F2 Suchen & Drucken oder Suchen
4690 IF FKEY=3 THEN 2300
4700 IF FKEY=4 THEN GOSUB 14120:GOTO 4620                 'Zum Hauptmenü?-Frage
4710 '
4720 ' *** Suchen nach Nummern
4730 TMP$=STR$(1):NUMSEARCH$(1)=MID$(TMP$,2,99) 'Einsprung nach Ende Suchen & Drucken
4740 TMP$=STR$(MTGLMAX):NUMSEARCH$(2)=MID$(TMP$,2,99)
4750 MT$="STAMMSATZ SUCHEN NACH NUMMERN":MI$="G-011051"
4760 IF ISPRINT THEN MT$="SUCHEN & DRUCKEN NACH NUMMERN":MI$="G-013011"
4770 GOSUB 13690                                                   'Basis-Maske
4780 FKEYS$="<F5>=Suchen":IF ISPRINT THEN FKEYS$="<F5>=Suchen & Drucken"
4790 GOSUB 13800                                           'F-Tasten einblenden
4800 TMP$="gesucht":IF ISPRINT THEN TMP$="gesucht und gedruckt"
4810 TMP1=12:IF ISPRINT THEN TMP1=6
4820 TMP2=66:IF ISPRINT THEN TMP2=73
4830 PRINT FNCUR$(4,TMP1);"Ab welcher Mitgliedsnummer soll ";TMP$;" werden?     =>..."
4840 PRINT FNCUR$(6,TMP1);"Bis zu welcher Mitgliedsnummer soll ";TMP$;" werden? =>..."
4850 PRINT FNCUR$(4,TMP2);HIGHON$;NUMSEARCH$(1);HIGHOFF$
4860 PRINT FNCUR$(6,TMP2);HIGHON$;NUMSEARCH$(2);HIGHOFF$
4870 PRINT FNCUR$(8,22);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
4880 F=1
4890 ISINPUTOK=TRUE
4900 FKEYMASK$="12345":CKEYMASK$="":TEXTIN$=NUMSEARCH$(F)
4910 Z=NUMSEARCHROW(F):LNG=3:TYP=2:ISMAND=TRUE
4920 S=66:IF ISPRINT THEN S=73
4930 GOSUB 10370                                                'Feld-Eingabe+1
4940 IF FKEY<>1 THEN 4980                                 'F-Tasten verarbeiten
4950   HELPID=G0110510:IF ISPRINT THEN HELPID=G0130110
4960   GOSUB 17930                                                  'Hilfe-Text
4970   GOTO 4750                                    'TODO: Werte gehen verloren
4980 IF FKEY=2 THEN 4540
4990 IF FKEY=3 THEN 2300
5000 IF FKEY=4 THEN GOSUB 14120:GOTO 4930                 'Zum Hauptmenü?-Frage
5010 IF FKEY<>5 THEN 5070
5020   SEARCHMODE=BYNUMS
5030   NUMSUCH1=VAL(NUMSEARCH$(1))
5040   NUMSUCH2=VAL(NUMSEARCH$(2))
5050   IF ISPRINT THEN 8370 ELSE 5650    'Absprung Suchen & Drucken oder Suchen
5060 '
5070 M=VAL(TEXTOUT$)                                        'RETURN verarbeiten
5080 IF M>=1 AND M<=MTGLMAX THEN 5120
5090   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 13480         'Nachricht anzeigen
5100   GOSUB 5260                                     'Eingabefehler-Behandlung
5110   GOTO 4910
5120 NUMSEARCH$(F)=TEXTOUT$
5130 NUMSUCH1=VAL(NUMSEARCH$(1))
5140 NUMSUCH2=VAL(NUMSEARCH$(2))
5150 IF NUMSUCH1<=NUMSUCH2 THEN 5190
5160   N$="Ungültiger Bereich":GOSUB 13480                  'Nachricht anzeigen
5170   GOSUB 5260                                     'Eingabefehler-Behandlung
5180   GOTO 4910
5190 F=F+1:IF F>NUMSEARCHMAX THEN F=1
5200 GOSUB 14060                                           'F-Tasten ausblenden
5210 FKEYS$="<F5>=Suchen":IF ISPRINT THEN FKEYS$="<F5>=Suchen & Drucken" 
5220 GOSUB 13800                                           'F-Tasten einblenden
5230 GOTO 4900
5240 '
5250 ' *** SUB Eingabefehler-Behandlung
5260 ISINPUTOK=FALSE
5270 FKEYMASK$="1234"
5280 GOSUB 14060                                           'F-Tasten ausblenden
5290 FKEYS$="":GOSUB 13800                                 'F-Tasten einblenden
5300 RETURN
5310 '
5320 ' *** Suchen nach Maske
5330 GOSUB 11440                                              'FLDSEARCH$() leeren
5340 MT$="STAMMSATZ SUCHEN NACH MASKE":MI$="G-011052" 'Einsprung nach Ende Suchen & Drucken
5350 IF ISPRINT THEN MT$="SUCHEN & DRUCKEN NACH MASKE":MI$="G-013012"
5360 GOSUB 13690                                                   'Basis-Maske
5370 FKEYS$="<F5>=Suchen":IF ISPRINT THEN FKEYS$="<F5>=Suchen & Drucken" 
5380 GOSUB 13800                                           'F-Tasten einblenden
5390 GOSUB 11910      'Feldnamen und Suchfeldwerte und Feldplatzhalter ausgeben
5400 F=1
5410 Z=FLDROW(F):S=FLDCOL(F)+COLSEP:LNG=FLDLNG(F):TYP=FLDTYP(F):ISMAND=FLDISMAND(F)
5420 FKEYMASK$="12345":CKEYMASK$="12":TEXTIN$=FLDSEARCH$(F)
5430 GOSUB 10360                                                  'Feld-Eingabe
5440 IF FKEY<>1 THEN 5480                                 'F-Tasten verarbeiten
5450   HELPID=G0110520:IF ISPRINT THEN HELPID=G0130120
5460   GOSUB 17930                                                  'Hilfe-Text
5470   GOTO 5340
5480 IF FKEY=2 THEN 4540
5490 IF FKEY=3 THEN 2300
5500 IF FKEY=4 THEN GOSUB 14120:GOTO 5430                 'Zum Hauptmenü?-Frage
5510 IF FKEY<>5 THEN 5540 
5520   SEARCHMODE=BYMASK                                                  '<F5>
5530   IF ISPRINT THEN 8370 ELSE 5650             'Suchen oder Suchen & Drucken
5540 IF CKEY=0 THEN 5600                              'Cursortasten verarbeiten
5550   PRINT FNCUR$(Z,S);HIGHON$;FLDSEARCH$(F);HIGHOFF$;
5560   PRINT STRING$(LNG-LEN(FLDSEARCH$(F)),".")
5570   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FLDMAX
5580   IF CKEY=CKEYDN THEN F=F+1:IF F>FLDMAX THEN F=1
5590   GOTO 5410
5600 FLDSEARCH$(F)=TEXTOUT$                                    'RETURN verarbeiten
5610 F=F+1:IF F>FLDMAX THEN F=1
5620 GOTO 5410
5630 '
5640 ' *** Suche
5650 IF SEARCHMODE=BYMASK THEN 5720
5660   GOSUB 13690                                                 'Basis-Maske
5670   GOSUB 11390                                                'Leere FLDDAT$()
5680   GOSUB 11780        'Feldnamen und Feldwerte mit Feldplatzhalter ausgeben
5690   MTGLZAHL=NUMSUCH1                               'Init Suche nach Nummern
5700   GOTO 5880
5710 '
5720 GOSUB 14060                                           'F-Tasten ausblenden
5730 MTGLZAHL=0                                          'Init Suche nach Maske
5740 GOSUB 6740                                        'Nächsten Treffer suchen
5750 IF MTGLZAHL<>-1 THEN 5880
5760   N$="Kein Suchtreffer. Mit <F2> neue Suche.":GOSUB 13480       'Nachricht anzeigen
5770   FKEYS$="":GOSUB 13800                               'F-Tasten einblenden
5780   FKEYMASK$="1234":NKEYMASK$="":GOSUB 13960                  'Menü-Eingabe
5790   ON FKEY GOTO 5800,5340,2300,5860
5800   HELPID=G0110502:IF ISPRINT THEN HELPID=G0130103          'F1 verarbeiten
5810   GOSUB 17930                                                  'Hilfe-Text
5820   GOSUB 13690                                                 'Basis-Maske
5830   GOSUB 11910    'Feldnamen und Suchfeldwerte mit Feldplatzhalter ausgeben
5840   GOTO 5760
5850 '
5860   GOSUB 14120:GOTO 5760              'F4 verarbeiten 'Zum Hauptmenü?-Frage
5870 '
5880 GOSUB 6500                                 'Suchtreffer lesen und anzeigen
5890 '
5900 FKEYS$="<F5>=Suchen rückwärts <F6>=Suchen vorwärts <F7>=Adressaufkleber" 
5910 GOSUB 13800                                           'F-Tasten einblenden
5920 NKEYMAX=0:FKEYMAX=7:GOSUB 13960                              'Menü-Eingabe
5930 ON FKEY GOTO 5960,6030,2300,6060,6100,6300,6570
5940 '
5950 ' *** <F1> Hilfe
5960 HELPID=G0110503:GOSUB 17930                                    'Hilfe-Text
5970 GOSUB 13690                                                   'Basis-Maske
5980 GOSUB 12040                                      'Mitgliedsnummer ausgeben
5990 GOSUB 11780          'Feldnamen und Feldwerte mit Feldplatzhalter ausgeben
6000 GOTO 5900
6010 '
6020 ' *** <F2> Zurück
6030 IF SEARCHMODE=BYNUMS THEN 4740 ELSE 5340       'TODO: Werte gehen verloren
6040 '
6050 ' *** <F4> Hauptmenü
6060 GOSUB 14120                                          'Zum Hauptmenü?-Frage
6070 GOTO 5920
6080 '
6090 ' *** <F5> Suchen rückwärts
6100 GOSUB 14060                                           'F-Tasten ausblenden
6110 IF SEARCHMODE=BYMASK THEN 6180
6120 '
6130 MTGLZAHL=MTGLZAHL-1                          'Suchen rückwärts nach Nummer
6140 IF MTGLZAHL>=NUMSUCH1 THEN 6260 
6150 MTGLZAHL=NUMSUCH1
6160 GOTO 6230
6170 '       
6180 TMP=MTGLZAHL                                  'Suchen rückwärts nach Maske
6190 GOSUB 6660                                      'Vorherigen Treffer suchen
6200 IF MTGLZAHL<>-1 THEN 6260
6210 MTGLZAHL=TMP                                      'Keinen Treffer gefunden
6220 '
6230 N$="Suchbeginn erreicht":GOSUB 13480       'Suchbeginn erreicht 'Nachricht anzeigen
6240 GOTO 5900
6250 '
6260 GOSUB 6500                                 'Suchtreffer lesen und anzeigen
6270 GOTO 5900
6280 '
6290 ' *** <F6> Suchen vorwärts
6300 GOSUB 14060                                           'F-Tasten ausblenden
6310 IF SEARCHMODE=BYMASK THEN 6380
6320 '
6330 MTGLZAHL=MTGLZAHL+1                           'Suchen vorwärts nach Nummer
6340 IF MTGLZAHL<=NUMSUCH2 THEN 6460
6350 MTGLZAHL=NUMSUCH2
6360 GOTO 6430
6370 '
6380 TMP=MTGLZAHL                                    'Suche vorwärts nach Maske
6390 GOSUB 6740                                        'Nächsten Treffer suchen
6400 IF MTGLZAHL<>-1 THEN 6460
6410 MTGLZAHL=TMP                                      'Keinen Treffer gefunden
6420 '
6430 N$="Suchende erreicht":GOSUB 13480           'Suchende erreicht 'Nachricht anzeigen
6440 GOTO 5900
6450 '
6460 GOSUB 6500                                 'Suchtreffer lesen und anzeigen
6470 GOTO 5900
6480 '
6490 ' *** SUB Suchtreffer lesen und anzeigen
6500 GOSUB 13620                                             'Nachricht löschen
6510 GOSUB 12150                                               'Stammsatz lesen
6520 GOSUB 12040                                      'Mitgliedsnummer ausgeben
6530 GOSUB 11670                        'Feldwerte mit Feldplatzhalter ausgeben
6540 RETURN
6550 '
6560 ' *** <F7> Adressaufkleber drucken
6570 GOSUB 16410                                         'Drucker bereit?-Frage
6580 IF NOT ISYES THEN 6620
6590   N$="Adressaufkleber wird gedruckt...":GOSUB 13480    'Nachricht anzeigen
6600   GOSUB 16920                                     'Adressaufkleber drucken
6610   GOSUB 13620                                           'Nachricht löschen
6620 GOTO 5900                                                    'Neue Eingabe
6630 '
6640 ' *** SUB Vorherigen Treffer suchen
6650 ' IN: MTGLZAHL OUT: MTGLZAHL oder -1 wenn kein Treffer
6660 MTGLZAHL=MTGLZAHL-1
6670 IF MTGLZAHL<1 THEN 6800
6680 GOSUB 6840                                                       'Treffer?
6690 IF NOT ISMATCH THEN 6660 
6700 RETURN
6710 '
6720 ' *** SUB Nächsten Treffer suchen
6730 ' IN: MTGLZAHL OUT: MTGLZAHL oder -1 wenn kein Treffer
6740 MTGLZAHL=MTGLZAHL+1
6750 IF MTGLZAHL>MTGLMAX THEN 6800
6760 GOSUB 6840                                                       'Treffer?
6770 IF NOT ISMATCH THEN 6740 
6780 RETURN
6790 '
6800 MTGLZAHL=-1
6810 RETURN
6820 '
6830 ' *** SUB Treffer?
6840 N$="Vergleiche Stammsatz"+STR$(MTGLZAHL)+"...":GOSUB 13480      'Nachricht anzeigen
6850 GOSUB 12150                                               'Stammsatz lesen
6860 FOR I=1 TO FLDMAX
6870   S$=FLDSEARCH$(I)
6880   IF LEN(S$)=0 THEN 7010
6890   F$=LEFT$(S$,1)
6900   IF F$<>"*" THEN 6940
6910     IF LEN(S$)=1 THEN 7010
6920     IF INSTR(FLDDAT$(I),RIGHT$(S$,LEN(S$)-1))>0 THEN 7010
6930     GOTO 7050
6940   IF F$<>"?" THEN 7000
6950     P=2
6960     IF MID$(S$,P,1)="?" THEN P=P+1:GOTO 6960
6970     IF P=LEN(FLDDAT$(I))+1 THEN 7010         
6980     IF INSTR(FLDDAT$(I),RIGHT$(S$,LEN(S$)-P+1))=P THEN 7010
6990     GOTO 7050
7000   IF S$<>FLDDAT$(I) THEN 7050 
7010 NEXT I
7020 ISMATCH=TRUE
7030 RETURN
7040 '
7050 ISMATCH=FALSE
7060 RETURN
7070 '
7080 ' ######################### Verwendungsnachweis ##########################
7090 '
7100 GOSUB 11500                             'Einsprung MVW-Menü 'MTGLMAX lesen
7110 VNFLAG=TRUE
7120 '
7130 ' Verwendungsnachweis-Menü ***********************************************
7140 '
7150 MT$="VERWENDUNGSNACHWEIS":MI$="G-012000":GOSUB 13690          'Basis-Maske
7160 F=4                                                                'TODO ?
7170 FKEYS$="":GOSUB 13800                                 'F-Tasten einblenden
7180 PRINT CURON$;
7190 PRINT HIGHOFF$;
7200 PRINT FNCUR$(4,23);"Bitte geben Sie die Mitgliedsnummer"
7210 PRINT FNCUR$(5,20);"des zu ändernden Verwendungsnachweises ein"
7220 'LW PRINT FNCUR$(6,23);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
7230 PRINT FNCUR$(6,20);" und beenden Sie die Eingabe mit RETURN."
7240 PRINT FNCUR$(8,38);"=>..."
7250 Z=8:S=39:LNG=3:TYP=2
7260 GOSUB 10360                                                  'Feld-Eingabe
7270 IF FKEY=1 THEN HELPID=G0120001:GOSUB 17930:GOTO 7150           'Hilfe-Text
7280 IF FKEY=2 OR FKEY=3 THEN GOSUB 11390:VNFLAG=FALSE:GOTO 2300 'FLDDAT$() leeren
7290 IF FKEY=4 THEN GOSUB 14120                           'Zum Hauptmenü?-Frage
7300 IF FKEY>4 THEN 7250
7310 IF CKEY<>0 THEN 7250
7320 IF LEN(TEXTOUT$)=0 THEN N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 13480:GOTO 7240 ELSE MITGLIED=VAL(TEXTOUT$):IF MITGLIED=0 THEN 7260 'Nachricht anzeigen
7330 IF MITGLIED>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 13480:PRINT HOME$;HIGHOFF$:GOTO 7240 'Nachricht anzeigen
7340 MTGLZAHL=MITGLIED:MTGL$=STR$(MITGLIED)
7350 '
7360 ' *** Lesen des Mitgliedsnamens ******************************************
7370 '
7380 PRINT FNCUR$(21,1);DELLINE$;
7390 GET #2,MITGLIED                                           'DATEN STS lesen
7400 NNAM$=LEFT$(DD1$,25)                                 'NACHNAME RAUSFISCHEN
7410 VNAM$=MID$(DD1$,26,25)                               'VORNAME  RAUSFISCHEN
7420 '
7430 ' ** LEERZEICHEN HINTER NACHNAME ABSCHNEIDEN *****************************
7440 '
7450 FOR I=LEN(NNAM$) TO 1 STEP -1
7460   IF MID$(NNAM$,I,1)<>" " THEN NNAM$=LEFT$(NNAM$,I):I=1
7470 NEXT I
7480 NACHVORNAME$=NNAM$+" "+VNAM$
7490 GOSUB 14640                                             '1. VN-BLATT lesen
7500 BLATT=1
7510 VP=1:HP=1                                                 'INITIALISIERUNG
7520 F=8
7530 GOSUB 13690                                                   'Basis-Maske
7540 FKEYS$="<F7>=Speichern":GOSUB 13800                   'F-Tasten einblenden
7550 PRINT HIGHOFF$;
7560 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=Vorwärts";TAB(53);"<F9>=Blatt drucken";
7570 PRINT FNCUR$(3,2);"Blatt:"
7580 PRINT FNCUR$(5,2);"Mitglieds-Nr.:"
7590 PRINT FNCUR$(5,24);"Name:"
7600 MSK$="Von:  .  .   Bis:  .  .   Amt:"
7610 FOR I=7 TO 19 
7620   PRINT FNCUR$(I,2);MSK$;TAB(43);MSK$
7630 NEXT I
7640 GOSUB 15850                                                 'Ausgabe Daten
7650 LNG=2-(HP=7)*6                                    'BEGIN OF THE INPUT-LOOP
7660 TYP=2-(HP=7)
7670 Z=VP+13*(VP>13)+6
7680 S=6-39*(VP>13)-(HP<7)*(HP-1)*3+(HP>3)*(HP<7)*4-(HP=7)*26
7690 GOSUB 13620                                             'Nachricht löschen
7700 GOSUB 10360                                                  'Feld-Eingabe
7710 IF HP<7 THEN IF ASCC=ASCRET THEN TEXTOUT$=STRING$(2-LEN(TEXTOUT$),"0")+TEXTOUT$   '+NULLEN
7720 IF CKEY<>0 OR ASCC=ASCRET THEN PRINT FNCUR$(Z,S);TEXTOUT$
7730 IF ASCC=ASCRET THEN IF HP<7 THEN MID$(FLDDAT$(VP),-1+2*HP,2)=TEXTOUT$ ELSE FLDDAT$(VP)=LEFT$(FLDDAT$(VP),12)+TEXTOUT$+SPACE$(8-LEN(TEXTOUT$))
7740 IF ASCC=ASCRET AND HP<7 THEN ISYES=TRUE:TEST$=TEXTOUT$:ON HP GOSUB 12880,12910,12940,12880,12910,12940:IF NOT ISYES THEN N$="Ungültiges Datum":GOSUB 13480:GOTO 7700 'Datums-Check 'Nachricht anzeigen
7750 IF ASCC=ASCRET THEN IF HP=7 THEN HP=1:VP=VP-(VP<26)-(VP=26)*(-25):GOTO 7650 ELSE HP=HP+1:GOTO 7650     'NEUE SCREEN-POSITION
7760 IF CKEY<>0 THEN TEST$=MID$(FLDDAT$(VP),-(HP<7)*2*HP-1-(HP=7)*14,-(HP<7)*2-(HP=7)*8):PRINT FNCUR$(Z,S);TEST$
7770 IF CKEY<>0 THEN IF HP<7 AND NOT TEST$="  " THEN ISYES=TRUE:ON HP GOSUB 12880,12910,12940,12880,12910,12940:IF NOT ISYES THEN N$="Ungültiges Datum":GOSUB 13480:GOTO 7700 'Nachricht anzeigen
7780 IF CKEY=CKEYDN THEN VP=VP-(VP<26)-(VP=26)*(-25):GOTO 7650
7790 IF CKEY=CKEYUP THEN VP=VP+(VP>1)-(VP=1)*25     :GOTO 7650
7800 IF CKEY=CKEYRT THEN HP=HP-(HP<7)-(HP=7)*(-6)   :GOTO 7650
7810 IF CKEY=CKEYLT THEN HP=HP+(HP>1)+(HP=1)*(-6)   :GOTO 7650
7820 IF FKEY=1 THEN HELPID=G0120002:GOSUB 17930:GOTO 7510           'Hilfe-Text
7830 IF FKEY=2 THEN 7150                                       'ZU MTGL-EINGABE
7840 IF FKEY=3 THEN VNFLAG=FALSE:GOSUB 11390:GOTO 2300           'FLDDAT$() leeren
7850 IF FKEY=4 THEN GOSUB 14120:GOSUB 13860:GOTO 7700     'Zum Hauptmenü?-Frage
7860 IF FKEY=5 THEN GOSUB 14730:GOTO 7650
7870 IF FKEY=6 THEN GOSUB 14870:GOTO 7650
7880 IF FKEY=7 THEN GOSUB 15510:GOTO 7700                 'SPEICHERN DANN INPUT
7890 IF FKEY=9 THEN GOSUB 16090:GOSUB 15060:GOTO 7690
7900 '
7910 ' ############# Mitgliedsliste drucken von 05*************** #############
7920 '
7930 MT$="MITGLIEDSLISTE DRUCKEN":MI$="G-013000":GOSUB 13690       'Basis-Maske
7940 FKEYS$="":GOSUB 13800                                 'F-Tasten einblenden
7950 PRINT FNCUR$(4,30);"1 - Suchen & Drucken"
7960 PRINT FNCUR$(6,30);"2 - Komplett drucken"
7970 PRINT FNCUR$(8,30);"3 - Datenblatt drucken"
7980 NKEYMAX=3:FKEYMAX=4:GOSUB 13960                              'Menü-Eingabe
7990 IF NKEY THEN ON NKEY GOTO 8060,8060,8820
8000 ON FKEY GOTO 8010,2300,2300,8020
8010 HELPID=G0130000:GOSUB 17930:GOTO 7930                          'Hilfe-Text
8020 GOSUB 14120:GOTO 7980                                'Zum Hauptmenü?-Frage
8030 '
8040 ' *** Suchen & Drucken, Komplett drucken *********************************
8050 '
8060 IF NKEY=1 THEN MT$="SUCHEN & DRUCKEN":MI$="G-013010"
8070 IF NKEY=2 THEN MT$="KOMPLETT DRUCKEN":MI$="G-013020"
8080 GOSUB 13690                                                   'Basis-Maske
8090 GOSUB 13800                                           'F-Tasten einblenden
8100 PRINT FNCUR$(4,22);"Bitte geben Sie das heutige Datum ein."
8110 PRINT FNCUR$(5,23);"Beenden Sie die Eingabe mit RETURN."
8120 PRINT FNCUR$(7,36);"=>........"
8130 ISINPUTOK=TRUE
8140 FKEYMASK$="1234":CKEYMASK$="":TEXTIN$=""
8150 Z=7:S=38:LNG=8:TYP=4:ISMAND=TRUE
8160 GOSUB 10370                                                'Feld-Eingabe+1
8170 IF FKEY<>1 THEN 8220       'F-Tasten verarbeiten 'TODO: Wert geht verloren
8180   IF NKEY=1 THEN HELPID=G0130101
8190   IF NKEY=2 THEN HELPID=G0130200
8200   GOSUB 17930                                                  'Hilfe-Text
8210   GOTO 8060
8220 IF FKEY=2 THEN 7930
8230 IF FKEY=3 THEN 2300
8240 IF FKEY=4 THEN GOSUB 14120:GOTO 8160                 'Zum Hauptmenü?-Frage
8250 '
8260 D$=TEXTOUT$:GOSUB 13020                    'Test Datum 'RETURN verarbeiten
8270 IF NOT ISINPUTOK THEN N$=DATUM$:GOSUB 13480:GOTO 8150  'Nachricht anzeigen
8280 PRINT FNCUR$(7,37);HIGHON$;DATUM$;HIGHOFF$
8290 '
8300 ISPRINT=TRUE 
8310 IF NKEY=2 THEN 8430
8320 '
8330 ' *** Suchen nach Nummern und Maske
8340 GOTO 4540                                  'Stammsatz suchen (und drucken)
8350 ' *** Rückkehr aus "Stammsatz suchen"
8360 ' IN: SEARCHMODE (BYNUMS, BYMASK), NUMSUCH1, NUMSUCH2, FLDSEARCH$()
8370 GOSUB 16410                                         'Drucker bereit?-Frage
8380 IF ISYES THEN 8500
8390 GOSUB 13800                                           'F-Tasten einblenden
8400 IF SEARCHMODE=BYNUMS THEN 4930 ELSE 5430 
8410 '
8420 ' *** Komplette Mitgliedsliste drucken
8430 SEARCHMODE=BYNUMS
8440 ISCOMPLETE=TRUE
8450 NUMSUCH1=1:NUMSUCH2=MTGLMAX
8460 GOSUB 16410                                         'Drucker bereit?-Frage
8470 IF NOT ISYES THEN 8160
8480 '
8490 ' *** Drucken
8500 N$="Mitgliedsliste wird gedruckt... Abbrechen mit Taste.":GOSUB 13480 'Nachricht anzeigen
8510 GOSUB 16580                                 'Mitgliedsliste drucken (Kopf)
8520 IF SEARCHMODE=BYMASK THEN 8650
8530 '
8540 ' *** Suchen nach Nummern
8550 FOR MTGLZAHL=NUMSUCH1 TO NUMSUCH2
8560   GOSUB 12150                                             'Stammsatz lesen
8570   IF INSTR(FLDDAT$(5),"ausgetreten")>0 THEN 8590
8580     GOSUB 16720                         'Mitgliedsliste drucken (Mitglied)
8590   GOSUB 16480                                      'Druck abbrechen?-Frage
8600   IF ISYES THEN 8760
8610 NEXT MTGLZAHL
8620 GOTO 8760
8630 ' 
8640 ' *** Suchen nach Maske
8650 MTGLZAHL=0
8660 GOSUB 6740                                        'Nächsten Treffer suchen
8670 IF MTGLZAHL=-1 THEN 8760
8680   GOSUB 12150                                             'Stammsatz lesen
8690   IF INSTR(FLDDAT$(5),"ausgetreten")>0 THEN 8710
8700     GOSUB 16720                         'Mitgliedsliste drucken (Mitglied)
8710   GOSUB 16480                                      'Druck abbrechen?-Frage
8720   IF ISYES THEN 8760
8730 GOTO 8660
8740 '
8750 ' *** Suchen zuende
8760 GOSUB 13620                                             'Nachricht löschen
8770 IF ISCOMPLETE THEN ISCOMPLETE=FALSE:GOTO 7930       'TODO: Very hackish...
8780 IF SEARCHMODE=BYNUMS THEN 4730 ELSE 5340            'TODO: Very hackish...
8790 '
8800 ' *** Datenblatt drucken *************************************************
8810 '
8820 MT$="DATENBLATT DRUCKEN":MI$="G-013030":GOSUB 13690           'Basis-Maske
8830 FKEYS$="":GOSUB 13800                                 'F-Tasten einblenden
8840 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer"
8850 PRINT FNCUR$(5,23)"des zu druckenden Datenblattes ein."
8860 PRINT FNCUR$(6,22);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
8870 PRINT FNCUR$(7,23)"Beenden Sie die Eingabe mit RETURN."
8880 PRINT FNCUR$(10,38)"=>..."
8890 ISINPUTOK=TRUE
8900 FKEYMASK$="1234":CKEYMASK$="":TEXTIN$=""
8910 Z=10:S=40:LNG=3:TYP=2:ISMAND=TRUE
8920 GOSUB 10370                                                'Feld-Eingabe+1
8930 IF FKEY=1 THEN HELPID=G0130300:GOSUB 17930:GOTO 8820 'Hilfe-Text 'TODO: Wert geht verloren
8940 IF FKEY=2 THEN 7930
8950 IF FKEY=3 THEN 2300
8960 IF FKEY=4 THEN GOSUB 14120:GOTO 8920                 'Zum Hauptmenü?-Frage
8970 '
8980 M=VAL(TEXTOUT$)                                        'RETURN verarbeiten
8990 IF M>=1 AND M<=MTGLMAX THEN 9030
9000   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 13480         'Nachricht anzeigen
9010   ISINPUTOK=FALSE
9020   GOTO 8910
9030 MTGLZAHL=M
9040 GOSUB 16410                                         'Drucker bereit?-Frage
9050 IF ISYES THEN 9090
9060   GOSUB 13800                                         'F-Tasten einblenden
9070   TEXTIN$=TEXTOUT$
9080   GOTO 8910
9090 N$="Datenblatt drucken...":GOSUB 13560     'Nachricht anzeigen, F-Tasten ausblenden
9100 GOSUB 12150                                               'Stammsatz lesen
9110 FULLNAME$ = FLDDAT$(1)+" "+FLDDAT$(2)
9120 GOSUB 17100                          'Stammsatz drucken (Bildschirmformat)
9130 GOSUB 14640                                     'Verwendungsnachweis lesen
9140 GOSUB 16120                                   'Verwendungsnachweis drucken
9150 GOSUB 17370                                     'Mitgliedsvermerke drucken
9160 GOSUB 13620                                             'Nachricht löschen
9170 GOTO 8820
10000 '
10010 ' ### Unterprogramme von 06*****, 07****, 08**** und 09**** #############
10020 '
10030 ' *** Tasten deaktivieren, ASCII Codes für Cursortasten vergeben ********
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
10200 ' Map unshifted and shifted logical tab key codes to ASCII code "ESC"
10210 PRINT ESC$;"41";CHR$(33);CHR$(ASCESC);            'Key 33 "ESC"         .
10220 PRINT ESC$;"42";CHR$(33);CHR$(ASCESC);            'Key 33 "ESC"         .
10230 RETURN
10240 '
10250 ' *** Feld-Eingabe ******************************************************
10260 '
10270 ' IN: Z   - Zeile  OUT: TEXTOUT$ TMP: P
10280 ' IN: S   - Spalte OUT: FKEY     TMP: ISVALID (TRUE, FALSE)
10290 ' IN: LNG - Länge  OUT: CKEY     TMP: ISINPUTOK (TRUE, FALSE)
10300 ' IN: TYP - Typ    OUT: ASCC
10310 ' IN: ISMAND (TRUE, FALSE)
10320 ' IN: TEXTIN$
10330 ' IN: FKEYMASK$
10340 ' IN: CKEYMASK$
10350 ' IN: VNFLAG
10360 ISINPUTOK=TRUE
10370 IF NOT ISINPUTOK THEN TEXTIN$=TEXTOUT$
10380 P=LEN(TEXTIN$)
10390 PRINT FNCUR$(Z,S);HIGHON$;TEXTIN$;HIGHOFF$;STRING$(LNG-P,".")
10400 TEXTOUT$=TEXTIN$
10410 PRINT CURON$;
10420 CKEY=0:FKEY=0
10430 PRINT FNCUR$(Z,S+P);
10440 A$=INPUT$(1):ASCC=ASC(A$)
10450 IF ISINPUTOK THEN 10490
10460   GOSUB 13620                                          'Nachricht löschen
10470   PRINT FNCUR$(Z,S+P);
10480   ISINPUTOK=TRUE
10490 ON TYP GOSUB 12670,12730,12780,12830                          'Typ prüfen
10500 IF NOT ISVALID THEN 10560
10510   IF P>=LNG THEN 10440                                  'Gültiges Zeichen
10520   PRINT HIGHON$;A$;HIGHOFF$         
10530   TEXTOUT$=TEXTOUT$+A$
10540   P=P+1
10550   GOTO 10430
10560 IF ASCC<>ASCBSP THEN 10620                         'Backspace verarbeiten
10570   IF P<1 THEN 10440
10580   PRINT DEL$                        
10590   P=P-1
10600   TEXTOUT$=LEFT$(TEXTOUT$,P)
10610   GOTO 10430
10620 IF ASCC=ASCESC THEN 10360                             'ESCAPE verarbeiten
10630 IF ASCC=ASCRET THEN 10760                             'RETURN verarbeiten
10640 IF ASCC=ASCCUP THEN CKEY=CKEYUP:IF INSTR(CKEYMASK$,CHR$(CKEY+ASC0))>0 THEN 10760
10650 IF ASCC=ASCCDN THEN CKEY=CKEYDN:IF INSTR(CKEYMASK$,CHR$(CKEY+ASC0))>0 THEN 10760
10660 IF VNFLAG=FALSE THEN 10690
10670   IF ASCC=ASCCLT THEN CKEY=CKEYLT:IF INSTR(CKEYMASK$,CHR$(CKEY+ASC0))>0 THEN 10760 
10680   IF ASCC=ASCCRT THEN CKEY=CKEYRT:IF INSTR(CKEYMASK$,CHR$(CKEY+ASC0))>0 THEN 10760 
10690 IF ASCC<ASCF1 OR ASCC>ASCF9 THEN 10720              'F-Tasten verarbeiten
10700   FKEY=ASCC-ASCF1+1
10710   IF INSTR(FKEYMASK$,CHR$(FKEY+ASC0))>0 THEN 10810
10720 N$="Ungültige Eingabe - Bitte wiederholen":GOSUB 13480         'Nachricht anzeigen
10730 ISINPUTOK=FALSE
10740 GOTO 10430
10750 '
10760 IF NOT ISMAND OR LEN(TEXTOUT$)>0 THEN 10800           'RETURN verarbeiten
10770   N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 13480   'Nachricht anzeigen
10780   ISINPUTOK=FALSE 
10790   GOTO 10430
10800 IF CKEY<>0 THEN GOSUB 10860       'Cursortasten Auto-Wiederholung sperren
10810 PRINT CUROFF$
10820 RETURN 
10830 '
10840 ' *** Cursortasten Auto-Wiederholung sperren ****************************
10850 '
10860 TMP$=INKEY$:IF TMP$>=CHR$(ASCCUP) AND TMP$<=CHR$(ASCCRT) THEN 10860
10870 RETURN
10880 '
10890 ' *** Tastatur entprellen ***********************************************
10900 '
10910 FOR I=1 TO 30:A$=INKEY$:NEXT I
10920 RETURN
10930 '
10940 ' *** Stammsatzfelder-Definitionen lesen ******************************** 
10950 '
10960 RESTORE 11080
10970 FOR I=1 TO FLDMAX
10980   READ NAM$,COL,ROW,LNG,TYP
10990   FLDNAM$(I)=NAM$
11000   IF MID$(NAM$,13,1)="=" THEN FLDISMAND(I)=TRUE ELSE FLDISMAND(I)=FALSE                           
11010   FLDCOL(I)=COL
11020   FLDROW(I)=ROW
11030   FLDLNG(I)=LNG
11040   FLDTYP(I)=TYP 
11050 NEXT I
11060 RETURN
11070 '
11080 DATA "Nachname:     ",08,05,17,1
11090 DATA "Vorname:      ",08,06,17,1
11100 DATA "Feld 3:       ",08,07,17,1
11110 DATA "Feld 4:       ",08,08,17,1
11120 DATA "Status:       ",08,09,17,1
11130 DATA "Amt:          ",08,10,17,3
11140 DATA "Anrede:       ",08,12,17,1
11150 DATA "Feld 8:       ",08,13,17,1
11160 DATA "Feld 9:       ",08,14,17,1
11170 DATA "Straße Hausnr:",08,15,17,3
11180 DATA "Postleitzahl: ",08,16,17,2
11190 DATA "Ort:          ",08,17,17,1
11200 DATA "Telefonnummer:",08,18,17,3
11210 DATA "Feld 14:      ",08,19,15,1
11220 '
11230 DATA "Beruf:        ",43,06,17,1
11240 DATA "Feld 16:      ",43,07,17,1
11250 DATA "Feld 17:      ",43,09,17,1
11260 DATA "Feld 18:      ",43,10,17,1
11270 DATA "Feld 19:      ",43,11,17,1
11280 DATA "Feld 20:      ",43,12,17,1
11290 DATA "Feld 21:      ",43,13,17,1
11300 DATA "Feld 22:      ",43,14,17,1
11310 DATA "Feld 23:      ",43,15,17,1
11320 DATA "Feld 24:      ",43,16,17,1
11330 DATA "Feld 25:      ",43,17,17,1
11340 DATA "Feld 26:      ",43,18,17,1
11350 DATA "Feld 27:      ",43,19,21,1
11360 '
11370 ' *** FLDDAT$() leeren *****************************************************
11380 '
11390 FOR I=1 TO FLDMAX:FLDDAT$(I)="":NEXT I
11400 RETURN
11410 '
11420 ' *** FLDSEARCH$() leeren ****************************************************
11430 '
11440 FOR I=1 TO FLDMAX:FLDSEARCH$(I)="":NEXT I
11450 RETURN
11460 '
11470 ' *** MTGLMAX lesen ***************************************************** 
11480 '
11490 ' OUT: MTGLMAX
11500 OPEN "I",#1,"B:MTGLZAHL.DAT"
11510   INPUT #1,MTGLMAX$
11520   MTGLMAX=VAL(MTGLMAX$)
11530 CLOSE #1
11540 RETURN
11550 '
11560 ' *** MTGLMAX speichern *************************************************
11570 '
11580 ' IN: MTGLMAX
11590 OPEN "O",#1,"B:MTGLZAHL.DAT"
11600   PRINT #1,STR$(MTGLMAX)
11610 CLOSE #1
11620 RETURN
11630 '
11640 ' *** Maske Feldwerte mit Feldplatzhalter ausgeben **********************
11650 '
11660 ' IN: FLDMAX
11670 FOR I=1 TO FLDMAX
11680   Z=FLDROW(I)
11690   S=FLDCOL(I)+COLSEP
11700   PRINT FNCUR$(Z,S);HIGHON$;FLDDAT$(I);HIGHOFF$;
11710   PRINT STRING$(FLDLNG(I)-LEN(FLDDAT$(I)),".")
11720 NEXT I
11730 RETURN
11740 '
11750 ' *** Maske Feldnamen und Feldwerte mit Feldplatzhalter ausgeben ********
11760 '
11770 ' IN: FLDMAX
11780 FOR I=1 TO FLDMAX
11790   Z=FLDROW(I)
11800   S=FLDCOL(I)
11810   PRINT FNCUR$(Z,S);FLDNAM$(I)
11820   S=S+COLSEP
11830   PRINT FNCUR$(Z,S);HIGHON$;FLDDAT$(I);HIGHOFF$;
11840   PRINT STRING$(FLDLNG(I)-LEN(FLDDAT$(I)),".")               
11850 NEXT I
11860 RETURN
11870 '
11880 ' *** Maske Feldnamen und Suchfeldwerte mit Feldplatzhalter ausgeben ****
11890 '
11900 ' IN: FLDMAX
11910 FOR I=1 TO FLDMAX
11920   Z=FLDROW(I)
11930   S=FLDCOL(I)
11940   PRINT FNCUR$(Z,S);FLDNAM$(I)
11950   S=S+COLSEP
11960   PRINT FNCUR$(Z,S);HIGHON$;FLDSEARCH$(I);HIGHOFF$;
11970   PRINT STRING$(FLDLNG(I)-LEN(FLDSEARCH$(I)),".")               
11980 NEXT I
11990 RETURN
12000 '
12010 ' *** Mitgliedsnummer ausgeben ******************************************
12020 '
12030 ' IN: MTGLZAHL
12040 PRINT FNCUR$(3,31);"Mitgliedsnummer";HIGHON$;MTGLZAHL;HIGHOFF$;"   "
12050 RETURN
12060 '
12070 ' *** FIELDen der Stammsatz-Datei *************************************** 
12080 '
12090 OPEN "R",#2,"B:MV.DAT",462+27
12100 FIELD #2,236+14 AS XSTAMMSATZDAT1$,225+13 AS XSTAMMSATZDAT2$
12110 RETURN
12120 '
12130 ' *** Stammsatz von Disk lesen ******************************************
12140 '
12150 GET #2,MTGLZAHL                                             'LW Compress?
12160 P=1
12170 FOR I=1 TO 14
12180   LNG=FLDLNG(I)+1
12190   TMP$=MID$(XSTAMMSATZDAT1$,P,LNG)
12200   L=ASC(LEFT$(TMP$,1))
12210   FLDDAT$(I)=MID$(TMP$,2,L)
12220   P=P+LNG
12230 NEXT I
12240 P=1
12250 FOR I=15 TO FLDMAX
12260   LNG=FLDLNG(I)+1
12270   TMP$=MID$(XSTAMMSATZDAT2$,P,LNG)
12280   L=ASC(LEFT$(TMP$,1))
12290   FLDDAT$(I)=MID$(TMP$,2,L)
12300   P=P+LNG
12310 NEXT I
12320 RETURN
12330 '
12340 ' *** Stammsatz auf Disk speichern **************************************
12350 '
12360 ' IN: MTGLZAHL
12370 TMP$=""                                                     'LW Compress?
12380 FOR I=1 TO 14
12390   LNG=FLDLNG(I)
12400   L=LEN(FLDDAT$(I))
12410   TMP$=TMP$+CHR$(L)+FLDDAT$(I)+SPACE$(LNG-LEN(FLDDAT$(I)))
12420 NEXT I
12430 LSET XSTAMMSATZDAT1$=TMP$
12440 TMP$=""
12450 FOR I=15 TO FLDMAX
12460   LNG=FLDLNG(I)
12470   L=LEN(FLDDAT$(I))
12480   TMP$=TMP$+CHR$(L)+FLDDAT$(I)+SPACE$(LNG-LEN(FLDDAT$(I)))
12490 NEXT I
12500 LSET XSTAMMSATZDAT2$=TMP$
12510 PUT #2,MTGLZAHL
12520 RETURN
12530 '
12540 ' *** Ja/Nein-Frage und F-Tasten ausblenden *****************************
12550 '
12560 ' IN:  Q$   - Frage
12570 ' OUT: ISYES - (TRUE, FALSE)
12580 N$=Q$+" (J=Ja/N=Nein)":GOSUB 13480                    'Nachricht anzeigen
12590 GOSUB 14060                                          'F-Tasten ausblenden
12600 ANSWER$=INPUT$(1)
12610 IF ANSWER$="J" OR ANSWER$="j" THEN ISYES=TRUE:RETURN
12620 IF ANSWER$="N" OR ANSWER$="n" THEN ISYES=FALSE:RETURN
12630 GOTO 12600
12640 '
12650 ' *** Testbereich 1 (Alpha) *********************************************
12660 '
12670 ISVALID=FALSE
12680 IF (ASCC>57 AND ASCC<127) OR (ASCC>31 AND ASCC<48) THEN ISVALID=TRUE
12690 RETURN
12700 '
12710 ' *** Testbereich 2 (Numerisch) *****************************************
12720 '
12730 ISVALID=FALSE: IF ASCC>47 AND ASCC<58 THEN ISVALID=TRUE
12740 RETURN
12750 '
12760 ' *** Testbereich 3 (Alphanumerisch) ************************************
12770 '
12780 ISVALID=FALSE: IF ASCC>31 AND ASCC<127 THEN ISVALID=TRUE
12790 RETURN
12800 '
12810 ' *** Testbereich 4 (Numerisch und ".") *********************************
12820 '
12830 ISVALID=FALSE: IF (ASCC>=48 AND ASCC<=57) OR ASCC=46 THEN ISVALID=TRUE
12840 RETURN
12850 '
12860 ' *** DATUMS-TEST (bei VN und Mitgliederliste drucken) ******************
12870 '
12880 IF VAL(TEST$)=0 OR VAL(TEST$)>31 THEN ISYES=FALSE                    'Tag
12890 RETURN
12900 '
12910 IF VAL(TEST$)=0 OR VAL(TEST$)>12 THEN ISYES=FALSE                  'Monat
12920 RETURN
12930 '
12940 IF VAL(TEST$)=0 THEN ISYES=FALSE                                    'Jahr
12950 RETURN
12960 '
12970 ' *** Test Datum
12980 '
12990 ' IN: D$
13000 ' OUT: DATUM$
13010 ' OUT: ISINPUTOK (TRUE, FALSE)
13020 ISINPUTOK=TRUE
13030 POS1=INSTR(D$,".")
13040 POS2=INSTR(POS1+1,D$,".")
13050 IF POS2=0 THEN 13250
13060 POS3=INSTR(POS2+1,D$,".")
13070 IF POS3<>0 THEN 13250
13080 D1$=LEFT$(D$,POS1-1)
13090 IF LEN(D1$)=0 OR LEN(D1$)>2 THEN 13250
13100 D2$=MID$(D$,POS1+1,POS2-POS1-1)
13110 IF LEN(D2$)=0 OR LEN(D2$)>2 THEN 13250
13120 D3$=RIGHT$(D$,LEN(D$)-POS2)
13130 IF LEN(D3$)=0 OR LEN(D3$)>2 THEN 13250
13140 '
13150 D1=VAL(D1$):IF D1<1 OR D1>31 THEN 13280
13160 D2=VAL(D2$):IF D2<1 OR D2>12 THEN 13280
13170 D3=VAL(D3$) 
13180 ' TODO: Genauere Verprobung
13190 D1$=STR$(D1):D1$=MID$(D1$,2,99):IF D1<10 THEN D1$="0"+D1$
13200 D2$=STR$(D2):D2$=MID$(D2$,2,99):IF D2<10 THEN D2$="0"+D2$
13210 D3$=STR$(D3):D3$=MID$(D3$,2,99):IF D3<10 THEN D3$="0"+D3$
13220 DATUM$=D1$+"."+D2$+"."+D3$
13230 GOTO 13310
13240 '
13250 DATUM$="Datum muss Format TT.MM.JJ haben. Bitte Eingabe korrigieren."
13260 GOTO 13290
13270 '
13280 DATUM$="Ungültiges Datum. Bitte Eingabe korrigieren."
13290 ISINPUTOK=FALSE
13300 '
13310 RETURN
13320 '
13330 ' *** Voreinstellungen zuweisen in "Stammsatz anlegen" ******************
13340 '
13350 FLDDAT$(5)="10***"
13360 FLDDAT$(7)=""11**""
13370 FLDDAT$(11)="12**"
13380 FLDDAT$(12)="13******"
13390 FLDDAT$(21)="3"
13400 FLDDAT$(22)="Nein"
13410 FLDDAT$(23)="Nein"
13420 FLDDAT$(25)="Nein"
13430 RETURN
13440 '
13450 ' *** Nachricht anzeigen ************************************************
13460 '
13470 ' IN: N$ - Nachricht
13480 PRINT FNCUR$(21,1);DELLINE$;
13490 IF N$<>"" THEN PRINT INVON$;" ";N$;" ";INVOFF$;
13500 PRINT                              'LW workaround quirk in display driver
13510 RETURN
13520 '
13530 ' *** Nachricht anzeigen, F-Tasten ausblenden *************************** 
13540 '
13550 ' IN: N$ - Nachricht
13560 GOSUB 13480                                           'Nachricht anzeigen
13570 GOSUB 14060                                          'F-Tasten ausblenden
13580 RETURN
13590 '
13600 ' *** Nachricht löschen *************************************************
13610 '
13620 N$="":GOSUB 13480                                     'Nachricht anzeigen
13630 RETURN
13640 '
13650 ' *** Bildschirmmaske - Basis *******************************************
13660 '
13670 ' IN: MT$ - Menü-Titel
13680 ' IN: MI$ - Menu-ID
13690 PRINT CLS$;
13700 PRINT "00**********************";
13710 PRINT FNCUR$(1,(80-LEN(MT$))/2+1);MT$;FNCUR$(1,72);MI$
13720 PRINT ROWSEP$
13730 PRINT FNCUR$(21,1);
13740 PRINT ROWSEP$             'quirk in display driver places this in line 22
13750 RETURN
13760 '
13770 ' *** F-Tasten einblenden ***********************************************
13780 '
13790 ' IN: FKEYS$ - Zusätzliche F-Tasten 
13800 PRINT FNCUR$(23,1);"<F1>=Hilfe <F2>=Zurück <F3>=Anfang <F4>=Hauptmenü";
13810 IF FKEYS$<>"" THEN PRINT " ";FKEYS$;HOME$ ELSE PRINT             'LW Hack
13820 RETURN
13830 '
13840 ' *** restliche funktionstasten bei Verw.-nachweis ausgeben
13850 '
13860 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=Vorwärts             <F8>=Drucken";
13870 PRINT HOME$
13880 RETURN
13890 '
13900 ' *** Eingabe bei Menübildern *******************************************
13910 '
13920 ' IN:  NKEYMAX - Max gültige Zifferntaste (1,2,3,...)
13930 ' IN:  FKEYMAX - Max gültige F-Taste (1,2,3,...)
13940 ' OUT: NKEY    - Ziffer der gedrückten Zifferntaste oder 0
13950 ' OUT: FKEY    - Ziffer der gedrückten F-Taste oder 0
13960 NKEY=0:FKEY=0
13970 GOSUB 10910                                          'Tastatur entprellen
13980 A$=INPUT$(1):ASCC=ASC(A$)
13990 IF ASCC>=ASC1 AND ASCC<=ASC1+NKEYMAX-1 THEN NKEY=ASCC-ASC1+1:RETURN
14000 IF ASCC>=ASCF1 AND ASCC<=ASCF1+FKEYMAX-1 THEN FKEY=ASCC-ASCF1+1:RETURN
14010 N$="Sie haben eine falsche Taste gedrückt - Bitte neue Auswahl treffen.":GOSUB 13480 'Nachricht anzeigen
14020 GOTO 13980
14030 '
14040 ' *** F-Tasten ausblenden ***********************************************
14050 '
14060 PRINT FNCUR$(23,1);DELLINE$
14070 PRINT FNCUR$(24,1);DELLINE$;
14080 RETURN
14090 '
14100 ' *** 'Zum Hauptmenü?-Frage (Original) **********************************
14110 '
14120 GOSUB 14200                               'Zum Hauptmenü?-Frage (einfach)
14130 IF ISYES THEN 18230                                         'Programmende
14140 GOSUB 13800                                          'F-Tasten einblenden
14150 RETURN
14160 '
14170 ' *** 'Zum Hauptmenü?-Frage (einfach) ***********************************
14180 '
14190 ' OUT: ISYES (TRUE, FALSE)
14200 Q$="Wollen Sie wirklich ins Hauptmenü?":GOSUB 12580        'Ja/Nein-Frage
14210 GOSUB 13620                                            'Nachricht löschen
14220 RETURN
14230 '
14240 ' *** Rückkehr aus Hilfe-Text? ******************************************
14250 '
14260 N$="Zurück? (J=Ja)":GOSUB 13480                       'Nachricht anzeigen
14270 GOSUB 12590                             'Ja/Nein-Frage ohne String-Koncat
14280 IF NOT ISYES THEN 14260
14290 RETURN
14300 '
14310 ' **** VN - Subroutines, permitting one page handling *******************
14320 ' ****         by 04********** 21*******              *******************
14330 '
14340 ' **** OPEN & FIELDING VERWENDUNGSNACHWEIS ******************************
14350 '
14360 OPEN "R",#3,"B:VN.NDX",4
14370 FIELD #3,4 AS XVNPOINT0$
14380 OPEN "R",#4,"B:VN.POI",8
14390 FIELD #4,4 AS XVNPOINT1$, 4 AS XVNPOINT2$
14400 OPEN "R",#5,"B:VN.DAT",560
14410 FIELD #5,240 AS XVNDAT1$,240 AS XVNDAT2$,80 AS XVNDAT3$
14420 RETURN
14430 '
14440 ' *** VERWENDUNGSNACHWEIS FÜR NEUES MITGLIED ANLEGEN ********************
14450 '
14460 ' IN: MTGLZAHL
14470 GET #3,1                               'letzte Seitennummer lesen dann +1
14480 TMPPOINT=VAL(XVNPOINT0$)+1
14490 LSET XVNDAT1$=""                                 'leeren Record speichern
14500 LSET XVNDAT2$=""
14510 LSET XVNDAT3$=""
14520 PUT #5,TMPPOINT
14530 LSET XVNPOINT0$=STR$(TMPPOINT)         'letzte Seitennummer neu speichern
14540 PUT #3,1
14550 PUT #3,MTGLZAHL+1
14560 LSET XVNPOINT1$="0000"
14570 LSET XVNPOINT2$="0000"
14580 PUT #4,TMPPOINT
14590 RETURN
14600 '
14610 ' **** ERSTE SEITE FINDEN & LESEN ***************************************
14620 '
14630 ' IN: MTGLZAHL
14640 GET #3,MTGLZAHL+1                                            'Index-Datei
14650 CPOINT=VAL(XVNPOINT0$)
14660 GET #5,CPOINT:GOSUB 15690                                    'Daten lesen
14670 BLATT=1
14680 GET #4,CPOINT                                              'Pointer lesen
14690 RETURN
14700 '
14710 ' **** SEITE RÜCKWÄRTS **************************************************
14720 '
14730 TMPPOINT=VAL(XVNPOINT1$)
14740 IF TMPPOINT<>0 THEN 14770
14750 N$="Erste Seite":GOSUB 13480                          'Nachricht anzeigen
14760 RETURN
14770 CPOINT=TMPPOINT
14780 BLATT=BLATT-1
14790 GET #5,CPOINT:GOSUB 15690                                    'Daten lesen
14800 GET #4,CPOINT:GOSUB 15850                             'Neue Pointer lesen
14810 HP=1:VP=1
14820 GOSUB 15060
14830 RETURN
14840 '
14850 ' **** SEITE VORWÄRTS  **************************************************
14860 '
14870 TMPPOINT=VAL(XVNPOINT2$)
14880 IF TMPPOINT=0 THEN 14950
14890 CPOINT=TMPPOINT
14900 BLATT=BLATT+1
14910 GET #5,CPOINT:GOSUB 15690
14920 GET #4,CPOINT:GOSUB 15850
14930 HP=1:VP=1
14940 RETURN
14950 N$="Letzte Seite"
14960 IF INSTR(SPACE$(20),FLDDAT$(26))=0 THEN 14990 
14970 GOSUB 13480                                           'Nachricht anzeigen
14980 RETURN
14990 Q$="Letzte Seite - Neue Seite anlegen?":GOSUB 12580        'Ja/Nein-Frage
15000 GOSUB 13620                                            'Nachricht löschen
15010 IF NOT ISYES THEN 15060
15020 GOSUB 15300
15030 BLATT=BLATT+1
15040 GOSUB 15850
15050 HP=1:VP=1
15060 GOSUB 13800                                          'F-Tasten einblenden
15070 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=vorwärts";
15080 PRINT TAB(54);"<F9>=Blatt drucken";
15090 PRINT HOME$;" "
15100 RETURN
15110 '
15120 ' **** ERSTE SEITE EINES MITGLIEDS ANLEGEN ******************************
15130 '
15140 GET #3,1
15150 LSET XVNPOINT0$=STR$(VAL(XVNPOINT0$)+1)                            'NDX+1
15160 PUT #3,1
15170 LSET XVNPOINT1$="0000"                                     'Page pointers
15180 LSET XVNPOINT2$="0000"
15190 PUT #4,VAL(XVNPOINT0$)
15200 LSET XVNPOINT0$=XVNPOINT0$                                    'First page
15210 PUT #3,MTGLZAHL+1
15220 LSET XVNDAT1$=""                                              'Datenblatt
15230 LSET XVNDAT2$=""
15240 LSET XVNDAT3$=""
15250 PUT #5,VAL(XVNPOINT0$)
15260 RETURN
15270 '
15280 ' **** ALLGEMEIN EINE NEUE SEITE ANLEGEN ********************************
15290 '
15300 GET #3,1                                                           'NDX+1
15310 LSET XVNPOINT0$=STR$(VAL(XVNPOINT0$)+1)
15320 PUT #3,1:TMPPOINT$=XVNPOINT0$    'Vektoren der bisher letzten Seite verbiegen
15330 LSET XVNPOINT2$=XVNPOINT0$                          'Verbiege Vektoren...
15340 PUT #4,CPOINT                                         '...der alten Seite
15350 LSET XVNPOINT1$=STR$(CPOINT)                          '...der neuen Seite
15360 LSET XVNPOINT2$="0000"                                             '.. ""
15370 CPOINT=VAL(TMPPOINT$)                                      'Nr neue Seite
15380 PUT #4,CPOINT
15390 LSET XVNDAT1$=""                                        'Write empty page
15400 LSET XVNDAT2$=""
15410 LSET XVNDAT3$="" 
15420 PUT #5,CPOINT
15430 DATEN$=SPACE$(20)
15440 FOR I=1 TO 26
15450   FLDDAT$(I)=DATEN$
15460 NEXT I
15470 RETURN
15480 '
15490 ' **** SEITE SPEICHERN **************************************************
15500 '
15510 DATEN$=""
15520 FOR I=1 TO 12
15530   DATEN$=DATEN$+FLDDAT$(I)
15540 NEXT I
15550 LSET XVNDAT1$=DATEN$:DATEN$=""
15560 FOR I=13 TO 24
15570   DATEN$=DATEN$+FLDDAT$(I)
15580 NEXT I
15590 LSET XVNDAT2$=DATEN$:DATEN$=""
15600 FOR I=25 TO 26
15610   DATEN$=DATEN$+FLDDAT$(I)
15620 NEXT I
15630 LSET XVNDAT3$=DATEN$:DATEN$=""
15640 PUT #5,CPOINT
15650 RETURN
15660 '
15670 ' **** HACK ! (ZERSTÜCKELN DER DATEN-STRINGS) ***************************
15680 '
15690 FOR I=1 TO 12
15700   FLDDAT$(I)=MID$(XVNDAT1$,(I-1)*20+1,20)
15710 NEXT I
15720 FOR I=13 TO 24
15730   FLDDAT$(I)=MID$(XVNDAT2$,(I-13)*20+1,20)
15740 NEXT I
15750 FOR I=25 TO 26
15760   FLDDAT$(I)=MID$(XVNDAT3$,(I-25)*20+1,20)
15770 NEXT I
15780 RETURN
15790 '
15800 ' **** UPRO DATENAUSGABE VN  ********************************************
15810 '
15820 ' IN: MTGLZAHL
15830 ' IN: NACHVORNAME$
15840 ' IN: BLATT
15850 PRINT DELHIGH$;                                          'TODO Necessary?
15860 PRINT HIGHON$
15870 PRINT FNCUR$(5,16);MTGLZAHL
15880 PRINT FNCUR$(5,29);NACHVORNAME$
15890 PRINT FNCUR$(3,8);BLATT
15900 FOR I=1 TO 26
15910   HOR=6-39*(I>13)
15920   VER=6+I+13*(I>13)
15930   FOR J=1 TO 3                                                 'Von-Datum
15940     PRINT FNCUR$(VER,HOR+(J-1)*3);MID$(FLDDAT$(I),-1+J*2,2)
15950   NEXT
15960   FOR J=1 TO 3                                                 'Bis-Datum
15970     PRINT FNCUR$(VER,HOR+13+(J-1)*3);MID$(FLDDAT$(I),5+J*2,2)
15980   NEXT
15990   PRINT FNCUR$(VER,HOR+26);MID$(FLDDAT$(I),13,8)                        'Amt
16000 NEXT
16010 PRINT HIGHOFF$
16020 RETURN
16030 '
16040 ' *** Verwendungsnachweis drucken ***************************************
16050 '
16060 ' IN: BLATT
16070 ' IN: MITGLZAHL
16080 ' IN: NACHVORNAME$
16090 Q$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 12580 'Ja/Nein-Frage TODO Back to main code?
16100 IF NOT ISYES THEN 16340                             'LW TODO Remove this!
16110 '
16120 RETURN                                         '*** TMP Print Disable ***
16130 I$=SPACE$(2)
16140 '
16150 PRINT ROWSEP$
16160 PRINT
16170 PRINT I$;"Blatt:";BLATT
16180 PRINT
16190 PRINT I$;"Mitglieds-Nr.:";MTGLZAHL;TAB(24);"Name:";NACHVORNAME$
16200 PRINT
16210 FOR I=1 TO 13
16220   KK=I:GOSUB 16310
16230   PRINT I$;"Von:";VON$;" Bis:";BIS$;" Amt:";AMT$;
16240   KK=I+13:GOSUB 16310
16250   PRINT " Von:";VON$;" Bis:";BIS$;" Amt:";AMT$
16260 NEXT 
16270 PRINT
16280 PRINT ROWSEP$
16290 RETURN
16300 'Format: VVVVVV BBBBBB AAAAAAAA
16310 VON$=LEFT$(FLDDAT$(KK),2)+"."+MID$(FLDDAT$(KK),3,2)+"."+MID$(FLDDAT$(KK),5,2)
16320 BIS$=MID$(FLDDAT$(KK),7,2)+"."+MID$(FLDDAT$(KK),9,2)+"."+MID$(FLDDAT$(KK),11,2)
16330 AMT$=RIGHT$(FLDDAT$(KK),8)
16340 RETURN
16350 '
16360 ' ### Unterprogramme "Mitgliedsliste drucken" ###########################
16370 '
16380 ' *** Drucker bereit?-Frage *********************************************
16390 '
16400 ' OUT: ISYES (TRUE, FALSE)
16410 Q$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 12580 'Ja/Nein-Frage
16420 GOSUB 13620                                            'Nachricht löschen
16430 RETURN
16440 '
16450 ' *** Druck abbrechen?-Frage ********************************************
16460 '
16470 ' OUT: ISYES (TRUE, FALSE)
16480 ISYES=FALSE 
16490 IF INKEY$="" THEN 16530
16500 Q$="Drucken abbrechen?":GOSUB 12580                        'Ja/Nein-Frage
16510 IF ISYES THEN 16530
16520 N$="Mitgliedsliste wird gedruckt... Abbrechen mit Taste.":GOSUB 13480 'Nachricht anzeigen
16530 RETURN
16540 '
16550 ' *** Mitgliedsliste drucken (Kopf) *************************************
16560 '
16570 ' IN: DATUM$
16580 PRINT TAB(16);"MITGLIEDSLISTE 25*******************************"
16590 PRINT 
16600 PRINT TAB(32);"Stand:"; DATUM$
16610 PRINT
16620 PRINT TAB(3);"Mitgl."; 
16630 PRINT TAB(10);"Name";
16640 PRINT TAB(36);"Vorname, Anschrift";
16650 PRINT TAB(63);"Status";
16660 PRINT TAB(71);"Amt"
16670 RETURN
16680 '
16690 ' *** Mitgliedsliste drucken (Mitglied) *********************************
16700 '
16710 ' IN:  MTGLZAHL
16720 AMT$=""
16730 IN=INSTR(FLDDAT$(6),"22**"):IF IN>0 THEN AMT$=MID$(FLDDAT$(6),IN,6):GOTO 16770
16740 IN=INSTR(FLDDAT$(6),"23**"):IF IN>0 THEN AMT$=MID$(FLDDAT$(6),IN,5):GOTO 16770
16750 IN=INSTR(FLDDAT$(6),"24**"):IF IN>0 THEN AMT$=MID$(FLDDAT$(6),IN,3)
16760 '
16770 PRINT
16780 PRINT TAB(5);MTGLZAHL;
16790 PRINT TAB(10);FLDDAT$(1);                                          'Nachname
16800 PRINT TAB(36);FLDDAT$(2);                                           'Vorname
16810 PRINT TAB(65);"(";LEFT$(FLDDAT$(5),1);")";                           'Status
16820 PRINT TAB(71);AMT$                                                   'Amt
16830 PRINT TAB(36);FLDDAT$(10)                                            'Straße
16840 PRINT TAB(36);FLDDAT$(11);                                     'Postleitzahl
16850 PRINT TAB(41);FLDDAT$(12)                                               'Ort
16860 PRINT TAB(36);"Tel.:";FLDDAT$(13)                                   'Telefon
16870 PRINT
16880 RETURN
16890 '
16900 ' *** Adressaufkleber drucken *******************************************
16910 '
16920 I$=SPACE$(3)
16930 '
16940 PRINT I$;FLDDAT$(7)                                                  'Anrede
16950 PRINT I$;FLDDAT$(2);" ";FLDDAT$(1)                            'Vorname Nachname
16960 PRINT
16970 PRINT I$;FLDDAT$(10)                                      'Straße Hausnummer
16980 PRINT
16990 PRINT I$;FLDDAT$(11);" ";FLDDAT$(12)                          'Postleitzahl Ort
17000 PRINT
17010 PRINT
17020 PRINT
17030 PRINT
17040 PRINT
17050 PRINT
17060 RETURN
17070 '
17080 ' *** Stammsatz drucken (Bildschirmformat)
17090 '
17100 TMP1=8
17110 TMP2=43
17120 '
17130 PRINT ROWSEP$
17140 PRINT
17150 PRINT TAB(31);"Mitgliedsnummer";MTGLZAHL
17160 PRINT
17170 RESTORE 17230
17180 FOR I=1 TO 7:READ A,B
17190   IF A<>0 THEN PRINT TAB(TMP1);FLDNAM$(A);FLDDAT$(A);
17200   IF B<>0 THEN PRINT TAB(TMP2);FLDNAM$(B);FLDDAT$(B);
17210   PRINT
17220 NEXT I
17230 DATA 1,0,2,15,3,16,4,0,5,17,6,18,0,19
17240 FOR I=7 TO 14
17250   PRINT TAB(TMP1);FLDNAM$(I);FLDDAT$(I);
17260   PRINT TAB(TMP2);FLDNAM$(I+13);FLDDAT$(I+13);
17270 NEXT I
17280 PRINT
17290 PRINT
17300 PRINT
17310 PRINT ROWSEP$
17320 RETURN
17330 '
17340 ' *** Mitgliedsvermerke drucken
17350 '
17360 ' IN: FULLNAME$
17370 I$=SPACE$(9)
17380 FORM1$="___"
17390 FORM2$="___ / ___"
17400 FORM3$="___ / ___ / ___"
17410 '
17420 PRINT TAB(10);"Mitgliedsnr.: ";FORM1$;
17430 PRINT TAB(28);"Name: ";FULLNAME$;
17440 PRINT TAB(58);"Datum: ";FORM3$
17450 PRINT
17460 PRINT I$;"26*************************** ";FORM3$
17470 PRINT
17480 PRINT I$;"27*************************** ";FORM3$
17490 PRINT
17500 PRINT I$;"28*************************** ";FORM3$
17510 PRINT
17520 PRINT I$;"29*************************** ";FORM3$
17530 PRINT
17540 PRINT I$;"30*************************** ";FORM2$
17550 PRINT
17560 PRINT I$;"31*************************** ";FORM3$
17570 PRINT
17580 PRINT I$;"32*************************** ";FORM3$
17590 PRINT
17600 PRINT I$;"33*************************** ";FORM1$
17610 PRINT
17620 PRINT I$;"34*************************** ";FORM3$
17630 PRINT
17640 PRINT I$;"35*************************** ";FORM3$
17650 PRINT
17660 PRINT I$;"36*************************** ";FORM3$
17670 PRINT
17680 PRINT I$;"37*************************** ";FORM3$
17690 PRINT
17700 PRINT I$;"38*************************** ";FORM3$
17710 RETURN
17720 '
17730 ' ### HILFE-TEXT ########################################################
17740 '
17750 ' *** Fielden der Hilfe-Text-Dateien ************************************
17760 '
17770 OPEN "R",#6,"MVWHELP.NDX",4
17780   FIELD #6,2 AS XFIRSTLINE$,2 AS XLASTLINE$
17790   FOR I=1 TO HELPIDMAX
17800     GET #6,I
17810     HELPFIRSTLINE(I)=CVI(XFIRSTLINE$)
17820     HELPLASTLINE(I)=CVI(XLASTLINE$)
17830   NEXT I
17840 CLOSE #6
17850 '
17860 OPEN "R",#6,"MVWHELP.DAT",64
17870 FIELD #6,2 AS XINDENT$,2 AS XLENGTH$,60 AS XLINE$
17880 RETURN
17890 '
17900 ' *** Hilfe-Text-Seite anzeigen *****************************************
17910 '
17920 ' IN: HELPID
17930 SAVEMT$=MT$:MT$="HILFE":GOSUB 13690:MT$=SAVEMT$              'Basis-Maske
17940 PRINT HOME$
17950 FIRSTLINE=HELPFIRSTLINE(HELPID)
17960 LASTLINE=HELPLASTLINE(HELPID)
17970 FOR I=FIRSTLINE TO LASTLINE
17980   GET #6,I
17990   INDENT=CVI(XINDENT$)
18000	LENGTH=CVI(XLENGTH$)
18010   PRINT TAB(INDENT);LEFT$(XLINE$,LENGTH)
18020 NEXT I
18030 GOSUB 14260                                      'Rückkehr aus Hilfe-Text
18040 RETURN
18050 '
18060 ' ### PROGRAMMFEHLER, PROGRAMMENDE ######################################
18070 '
18080 ' *** Programmfehler ****************************************************
18090 '
18100 I$=SPACE$(15)
18110 GOSUB 13690                                                  'Basis-Maske
18120 PRINT HOME$
18130 PRINT I$;"Ein Programmfehler ist aufgetreten."
18140 PRINT   
18150 PRINT I$;"- Notieren Sie folgende Fehlerinformation:"
18160 PRINT I$;"  ";HIGHON$;"ERL";HIGHOFF$;" (Error Line)";HIGHON$;ERL;HIGHOFF$
18170 PRINT I$;"  ";HIGHON$;"ERR";HIGHOFF$;" (Error Code)";HIGHON$;ERR;HIGHOFF$
18180 PRINT I$;"- Drücken Sie am Rechner den Knopf hinten rechts."
18190 PRINT I$;"19**************************"
18200 '
18210 ' *** Programmende ******************************************************
18220 '
18230 PRINT FNCUR$(22,1);HIGHOFF$;CURON$;
18240 RESET
18250 'LW RUN "DISKMENU.EXE"
18260 END                                                           '1622 lines
;ERR;HIOFF$
18190 PRINT I$;"- Drücken Sie am Rechner den Knopf hinten rechts."
18200 PRINT I$;"19**************************"
18210 '
18220 ' *** Programmende ******************************************************
18230 '
18240 RESET
18250 PRINT FNCUR$(22,1);HIOFF$;CURON$;
18260 'LW RUN "DISKMENU.EXE"
18270 END                                                           '1622 lines
