1000 ' RELEASE 01***** Neubearbeitung durch 04**********
1010 '
1020 ' LW 31-JAN-2026: Load MSBASIC.COM with /S:560 /F:6
1030 '
1040 ' *** Initialisierung, Dimensionierung ***********************************
1050 '
1060 DEFINT A-Z
1070 '
1071 TRUE=-1
1072 FALSE=0
1073 '
1080 FELDMAX = 27                                       'Anzahl Stammsatzfelder
1090 DIM FENAM$(FELDMAX)                                       'Name des Feldes
1100 DIM FEMUSS(FELDMAX)                             'Muss-Feld (-1=Ja, 0=Nein)
1110 DIM FEROW(FELDMAX)                           'Zeile des Feldnamens (1..25)
1120 DIM FECOL(FELDMAX)                          'Spalte des Feldnamens (1..80)
1130 DIM FELNG(FELDMAX)                                       'Länge des Feldes
1140 DIM FETYP(FELDMAX)                                         'Typ des Feldes
1150 '
1160 DIM DAT$(FELDMAX)                                      'Stammsatzfelddaten
1170 DIM SUCH$(FELDMAX)                                     'Stammsatzsuchdaten
1180 '
1190 NUMSUCHMAX=2
1200 DIM NUMSUCH$(NUMSUCHMAX)
1210 DIM NUMSUCHROW(NUMSUCHMAX)
1220 NUMSUCHROW(1)=4
1230 NUMSUCHROW(2)=6
1240 '
1250 BYNUMS=1                                                 'Werte von SEARCH
1260 BYMASK=2
1270 '
1280 BSP$   =CHR$(8)                                                 'Backspace
1290 DEL$   =BSP$+"."+BSP$                                              'Delete
1300 ESC$   =CHR$(27)                                'Einleitung Escape-Sequenz
1310 STRICH$=STRING$(79,"-")                                            'Strich
1311 PUNKTE$=STRING$(30,".")                                            'Punkte
1320 '
1330 CLS$    =ESC$+"E"                                      'Bildschirm löschen
1340 INVON$  =ESC$+"p"                                              'Invers ein
1350 INVOFF$ =ESC$+"q"                                              'Invers aus
1360 CURON$  =ESC$+"y5"                                             'Cursor ein
1370 CUROFF$ =ESC$+"x5"                                             'Cursor aus
1380 HIGHON$ =ESC$+"("                                           'Helle Schrift
1390 HIGHOFF$=ESC$+")"                                          'Dunkle Schrift
1400 DELLINE$=ESC$+"K"                                           'Zeile löschen
1410 DELHIGH$=ESC$+"+"                                   'Helle Schrift löschen
1420 '
1430 DEF FNCUR$(Z,S)=ESC$+"Y"+CHR$(31+Z)+CHR$(31+S)       'Cursor positionieren
1440 HOME$=FNCUR$(3,1)                                    'Cursor Home Position
1450 ' 
1460 CKEYUP=1                                    'Werte von CKEY (Cursortasten)
1470 CKEYDN=2
1480 CKEYLT=3
1490 CKEYRT=4
1500 '
1510 ASCBSP=8    'Werte von ASCC (ASCII-Code des ersten eingegebenen Feldwerts)
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
1630 ASCF9=249
1640 '
1650 HELPIDMAX=23                                     'Anzahl Hilfe-Text-Seiten
1660 DIM HELPFIRSTLINE(HELPIDMAX)
1670 DIM HELPLASTLINE(HELPIDMAX)
1680 '
1690 G0100000=1                                           'Mitgliederverwaltung
1700 G0110000=2                                                'Stammsatzpflege
1710 G0110100=3                                              'Stammsatz anlegen
1720 G0110201=4                                           'Stammsatz ändern (I)
1730 G0110202=5                                          'Stammsatz ändern (II)
1740 G0110300=6                                              'Stammsatz löschen
1750 G0110400=7                                              'Stammsatz drucken
1760 G0110501=8                                           'Stammsatz suchen (I)
1770 G0110510=9                                  'Stammsatz suchen nach Nummern
1780 G0110520=10                                   'Stammsatz suchen nach Maske
1790 G0110502=11                           'Stammsatz suchen (II) Keine Treffer
1800 G0110503=12                               'Stammsatz suchen (III) Blättern
1810 G0120001=13                                       'Verwendungsnachweis (I)
1820 G0120002=14                                      'Verwendungsnachweis (II)
1830 G0130000=15                                        'Mitgliedsliste drucken
1840 G0130101=16                                    'Suchen & Drucken (I) Datum
1850 G0130102=17                                         'Suchen & Drucken (II)
1860 G0130110=18                                 'Suchen & Drucken nach Nummern
1870 G0130120=19                                   'Suchen & Drucken nach Maske
1880 G0130103=20                           'Suchen & Drucken (II) Keine Treffer
1900 G0130200=21                                           'Komplettdruck Datum
1910 G0130300=22                                            'Datenblatt drucken
1920 '
1930 ' Weitere Variablen
1940 ' -----------------
1950 ' F        - Feldindex 1..27 (FELDMAX)
1960 ' I$       - Lokale Texteinrückung
1970 ' K        - Schleifenindex
1980 ' MTGLMAX  - Höchste auf Disk gespeicherte Mitgliedsnummer
1990 ' MTGLZAHL - Momentane Mitgliedsnummer
2000 '
2010 XOFF=14             'Offset Colum Feldname to Column Wertefeld (Pünktchen)
2020 '
2030 ' TIP: Do not let the cursor slip into line 25 -> display will scroll up
2040 ' TIP: Use semicolons with PRINT carefully -> display driver bug
2050 '
2060 ON ERROR GOTO 17540                               'Programmfehler abfangen
2070 '
2080 GOSUB 10050    'Tasten deaktivieren, ASCII-Codes für Cursortasten vergeben
2090 PRINT CUROFF$;
2100 '
2110 ' *** Beginn des Mitgliederverwaltungsprogramms **************************
2120 '
2130 MT$="MITGLIEDERVERWALTUNG":MI$="G-010000":GOSUB 13130         'Basis-Maske
2140 Q$="Bitte Daten-Diskette in Laufwerk B einlegen. Bereit?":GOSUB 12330 'Ja/Nein-Frage
2150 IF NOT ISJA THEN 17670                                       'Programmende
2160 '
2170 N$="Einen Moment bitte...":GOSUB 13000     'Nachricht. F-Tasten ausblenden
2180 GOSUB 11240                                                 'MTGLMAX lesen
2190 GOSUB 10970                              'Stammsatzfeld-Definitionen lesen
2200 GOSUB 11840                                   'Fielden der Stammsatz-Datei
2210 GOSUB 13800                       'Fielden der Verwendungsnachweis-Dateien
2220 GOSUB 17230      'Fielden der Hilfe-Text-Dateien, lesen der Zeilenbereiche
2230 GOSUB 13060                                             'Nachricht löschen
2240 '
2250 ' ###################### Mitgliederverwaltungs-Menü ######################
2260 '
2270 MT$="MITGLIEDERVERWALTUNG":MI$="G-010000":GOSUB 13130         'Basis-Maske
2280 FKEYS$="":GOSUB 13240                                 'F-Tasten einblenden
2290 PRINT FNCUR$(4,28);"1 - Stammsatzpflege"
2300 PRINT FNCUR$(6,28);"2 - Verwendungsnachweis"
2310 PRINT FNCUR$(8,28);"3 - Mitgliedsliste drucken"
2320 FKEYMAX=4:NKEYMAX=3:GOSUB 13400                              'Menü-Eingabe
2330 IF NKEY THEN ON NKEY GOTO 2420,6820,7650
2340 ON FKEY GOTO 2350,2360,2270,2360
2350 HELPID=G0100000:GOSUB 17390:GOTO 2270                          'Hilfe-Text
2360 GOSUB 13560:GOTO 2320                                'Zum Hauptmenü?-Frage
2370 '
2380 ' ########################### Stammsatzpflege ############################
2390 '
2400 ' *** Stammsatzpflege-Menü ***********************************************
2410 '
2420 MT$="STAMMSATZPFLEGE":MI$="G-011000":GOSUB 13130              'Basis-Maske
2430 FKEYS$="":GOSUB 13240                                 'F-Tasten einblenden
2440 PRINT FNCUR$(4,30);"1 - Stammsatz anlegen"
2450 PRINT FNCUR$(6,30);"2 - Stammsatz ändern"
2460 PRINT FNCUR$(8,30);"3 - Stammsatz löschen"
2470 PRINT FNCUR$(10,30);"4 - Stammsatz drucken"
2480 PRINT FNCUR$(12,30);"5 - Stammsatz suchen"
2490 FKEYMAX=4:NKEYMAX=5:GOSUB 13400                              'Menü-Eingabe
2500 IF NKEY THEN ON NKEY GOTO 2570,3050,3700,4130,4490
2510 ON FKEY GOTO 2520,2270,2270,2530
2520 HELPID=G0110000:GOSUB 17390:GOTO 2420                          'Hilfe-Text
2530 GOSUB 13560:GOTO 2490                                'Zum Hauptmenü?-Frage
2540 '
2550 ' *** Stammsatz anlegen von 02************ *******************************
2560 '
2570 MTGLZAHL=MTGLMAX+1
2580 F=1
2590 GOSUB 11130                                                 'DAT$() leeren
2600 GOSUB 12790                                     'Voreinstellungen zuweisen
2610 MT$="STAMMSATZ ANLEGEN":MI$="G-011010":GOSUB 13130            'Basis-Maske
2620 FKEYS$="<F7>=Speichern":GOSUB 13240                   'F-Tasten einblenden
2630 GOSUB 11790                                      'Mitgliedsnummer ausgeben
2640 GOSUB 11660             'Feldnamen, Feldwerte und Feldplatzhalter ausgeben
2650 Z=FEROW(F):S=FECOL(F)+XOFF:LNG=FELNG(F):TYP=FETYP(F):ISMUSS=FEMUSS(F)
2660 FKEYMASK$="12347":CKEYMASK$="12":INTEXT$=DAT$(F)
2670 GOSUB 10370                                                  'Feld-Eingabe
2680 '
2690 IF ASCC<>ASCRET THEN 2730                              'RETURN verarbeiten
2700   DAT$(F)=OUTTEXT$
2710   F=F+1:IF F>FELDMAX THEN F=1
2720   GOTO 2650
2730 IF CKEY=0 THEN 2780                              'Cursortasten verarbeiten
2740   PRINT FNCUR$(Z,S);HIGHON$;DAT$(F);HIGHOFF$;STRING$(LNG-LEN(DAT$(F)),".")
2750   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FELDMAX
2760   IF CKEY=CKEYDN THEN F=F+1:IF F>FELDMAX THEN F=1
2770   GOTO 2650
2780 IF FKEY=1 THEN HELPID=G0110100:GOSUB 17390:GOTO 2610           'Hilfe-Text
2790 IF FKEY=2 THEN GOSUB 2890:GOTO 2420              'Exit "Stammsatz anlegen"
2800 IF FKEY=3 THEN GOSUB 2890:GOTO 2270              'Exit "Stammsatz anlegen"
2810 IF FKEY<>4 THEN 2860
2820   GOSUB 13640                              'Zum Hauptmenü?-Frage (einfach)
2830   IF NOT ISJA THEN GOSUB 13240:GOTO 2670              'F-Tasten einblenden
2840   GOSUB 2890                                     'Exit "Stammsatz anlegen"
2850   GOTO 17670                                                 'Programmende
2860 IF FKEY=7 THEN GOSUB 2960:GOTO 2570            'Exit "Stammsatz anlegen" 2
2870 '
2880 ' *** Exit "Stammsatz anlegen"
2890 GOSUB 10920                                           'Tastatur entprellen
2900 Q$="Wollen Sie vorher den Stammsatz speichern?":GOSUB 12330 'Ja/Nein-Frage
2910 IF ISJA THEN 2960
2920   MTGLZAHL=MTGLZAHL-1
2930   GOTO 3000
2940 '                                                            
2950 ' *** Exit "Stammsatz anlegen" 2
2960 N$="Stammsatz wird gespeichert...":GOSUB 13000 'Nachricht. F-Tasten ausblenden
2970 GOSUB 12140                                           'Stammsatz speichern
2980 GOSUB 13910                       'Erste Verwendungsnachweis-Seite anlegen
2990 MTGLMAX=MTGLZAHL:GOSUB 11330                             'MTGLMAX speichen
3000 GOSUB 13060                                             'Nachricht löschen
3010 RETURN
3020 ' 
3030 ' *** Stammsatz ändern von 03************ ********************************
3040 '
3050 MT$="STAMMSATZ ÄNDERN":MI$="G-011020":GOSUB 13130             'Basis-Maske
3060 FKEYS$="":GOSUB 13240                                 'F-Tasten einblenden
3070 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer" 
3080 PRINT FNCUR$(5,24)"des zu ändernden Stammsatzes ein."
3090 PRINT FNCUR$(6,22);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
3100 PRINT FNCUR$(7,23)"Beenden Sie die Eingabe mit RETURN."
3110 PRINT FNCUR$(9,38)"=>..."
3120 ISBADINPUT=FALSE
3130 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
3140 Z=9:S=40:LNG=3:TYP=2:ISMUSS=TRUE
3150 GOSUB 10380                                                'Feld-Eingabe+1
3160 IF FKEY=1 THEN HELPID=G0110201:GOSUB 17390:GOTO 3050 'Hilfe-Text 'TODO: Wert geht verloren
3170 IF FKEY=2 THEN 2420
3180 IF FKEY=3 THEN 2270
3190 IF FKEY=4 THEN GOSUB 13560:GOTO 3150                 'Zum Hauptmenü?-Frage
3200 '
3210 M=VAL(OUTTEXT$)                                        'RETURN verarbeiten
3220 IF M>=1 AND M<=MTGLMAX THEN 3260
3230   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 12920         'Nachricht
3240   ISBADINPUT=TRUE
3250   GOTO 3140
3260 MTGLZAHL=M
3270 N$="Stammsatz wird gelesen...":GOSUB 13000 'Nachricht. F-Tasten ausblenden
3280 GOSUB 11900                                               'Stammsatz lesen
3290 GOSUB 13060                                             'Nachricht löschen
3300 '
3310 GOSUB 13130                                                   'Basis-Maske
3320 FKEYS$="<F7>=Speichern":GOSUB 13240                   'F-Tasten einblenden
3330 GOSUB 11790                                      'Mitgliedsnummer ausgeben
3340 GOSUB 11660             'Feldnamen, Feldwerte und Feldplatzhalter ausgeben
3350 F=1
3360 Z=FEROW(F):S=FECOL(F)+XOFF:LNG=FELNG(F):TYP=FETYP(F):ISMUSS=FEMUSS(F)
3370 FKEYMASK$="12347":CKEYMASK$="12":INTEXT$=DAT$(F)
3380 GOSUB 10370                                                  'Feld-Eingabe
3390 IF ASCC<>ASCRET THEN 3430                              'RETURN verarbeiten
3400   DAT$(F)=OUTTEXT$
3410   F=F+1:IF F>FELDMAX THEN F=1
3420   GOTO 3360
3430 IF CKEY=0 THEN 3480                              'Cursortasten verarbeiten
3440   PRINT FNCUR$(Z,S);HIGHON$;DAT$(F);HIGHOFF$;STRING$(LNG-LEN(DAT$(F)),".")
3450   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FELDMAX
3460   IF CKEY=CKEYDN THEN F=F+1:IF F>FELDMAX THEN F=1
3470   GOTO 3360
3480 IF FKEY=1 THEN HELPID=G0110202:GOSUB 17390:GOTO 3310           'Hilfe-Text
3490 IF FKEY=2 THEN GOSUB 3590:GOTO 3050               'Exit "Stammsatz ändern"
3500 IF FKEY=3 THEN GOSUB 3590:GOTO 2270               'Exit "Stammsatz ändern"
3510 IF FKEY<>4 THEN 3560 
3520   GOSUB 13640                              'Zum Hauptmenü?-Frage (einfach)
3530   IF NOT ISJA THEN GOSUB 13240:GOTO 3380              'F-Tasten einblenden
3540   GOSUB 3590                                      'Exit "Stammsatz ändern"
3550   GOTO 17670                                                 'Programmende
3560 IF FKEY=7 THEN GOSUB 3630:GOTO 3050             'Exit "Stammsatz ändern" 2
3570 '
3580 ' *** Exit "Stammsatz ändern"
3590 Q$="Wollen Sie vorher den Stammsatz speichern?":GOSUB 12330 'Ja/Nein-Frage
3600 IF NOT ISJA THEN RETURN
3610 '
3620 ' *** Exit "Stammsatz ändern" 2
3630 N$="Stammsatz wird gespeichert...":GOSUB 13000 'Nachricht. F-Tasten ausblenden
3640 GOSUB 12140                                           'Stammsatz speichern
3650 GOSUB 13060                                             'Nachricht löschen
3660 RETURN
3670 '
3680 ' *** Stammsatz löschen von 04********** *********************************
3690 '
3700 MT$="STAMMSATZ LÖSCHEN":MI$="G-011030":GOSUB 13130            'Basis-Maske
3710 FKEYS$="":GOSUB 13240                                 'F-Tasten einblenden
3720 PRINT FNCUR$(4,23);"Bitte geben Sie die Mitgliedsnummer"         'Menütext
3730 PRINT FNCUR$(5,24);"des zu löschenden Mitglieds ein."
3740 PRINT FNCUR$(6,22);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
3750 PRINT FNCUR$(7,15);"(Dabei wird nur der Status des Mitglieds verändert.)"
3760 PRINT FNCUR$(8,23);"Beenden Sie die Eingabe mit RETURN."
3770 PRINT FNCUR$(10,38);"=>..."
3780 ISBADINPUT=FALSE
3790 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
3800 Z=10:S=40:LNG=3:TYP=2:ISMUSS=TRUE
3810 GOSUB 10380                                                'Feld-Eingabe+1
3820 IF FKEY=1 THEN HELPID=G0110300:GOSUB 17390:GOTO 3700 'Hilfe-Text 'TODO: Wert geht verloren
3830 IF FKEY=2 THEN 2420
3840 IF FKEY=3 THEN 2270
3850 IF FKEY=4 THEN GOSUB 13560:GOTO 3810                 'Zum Hauptmenü?-Frage
3860 '
3870 M=VAL(OUTTEXT$)                                        'RETURN verarbeiten
3880 IF M>=1 AND M<=MTGLMAX THEN 3920
3890   N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12920       'Nachricht
3900   ISBADINPUT=TRUE
3910   GOTO 3800
3920 MTGLZAHL=M
3930 N$="Stammsatz wird gelesen...":GOSUB 13000 'Nachricht. F-Tasten ausblenden
3940 GOSUB 11900                                               'Stammsatz lesen
3950 GOSUB 13060                                             'Nachricht löschen
3960 '
3970 GOSUB 13130                                                   'Basis-Maske
3980 GOSUB 11790                                      'Mitgliedsnummer ausgeben
3990 GOSUB 11660             'Feldnamen, Feldwerte und Feldplatzhalter ausgeben
4000 Q$="Mitglied endgültig löschen?":GOSUB 12330                'Ja/Nein-Frage
4010 IF NOT ISJA THEN 4090
4020   F=5
4030   DAT$(F)="ausgetreten"
4040   Z=FEROW(F):S=FECOL(F)+XOFF:LNG=FELNG(F)
4050   PRINT FNCUR$(Z,S);HIGHON$;DAT$(F);HIGHOFF$;STRING$(LNG-LEN(DAT$(F)),".")               
4060   N$="Stammsatz wird gespeichert...":GOSUB 13000 'Nachricht. F-Tasten ausblenden
4070   GOSUB 12140                                         'Stammsatz speichern
4080   GOSUB 13060                                           'Nachricht löschen
4090 GOTO 3700
4100 '
4110 ' *** Stammsatz drucken von 02************ & 04********** ****************
4120 '
4130 MT$="STAMMSATZ DRUCKEN":MI$="G-011040":GOSUB 13130            'Basis-Maske
4140 FKEYS$="":GOSUB 13240                                 'F-Tasten einblenden
4150 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer"
4160 PRINT FNCUR$(5,23)"des auszudruckenden Stammsatzes ein."
4170 PRINT FNCUR$(6,22);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
4180 PRINT FNCUR$(7,23)"Beenden Sie die Eingabe mit RETURN."
4190 PRINT FNCUR$(9,38);"=>..."
4200 ISBADINPUT=FALSE
4210 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
4220 Z=9:S=40:LNG=3:TYP=2:ISMUSS=TRUE
4230 GOSUB 10380                                                'Feld-Eingabe+1
4240 IF FKEY=1 THEN HELPID=G0110400:GOSUB 17390:GOTO 4130 'Hilfe-Text 'TODO: Wert geht verloren
4250 IF FKEY=2 THEN 2420
4260 IF FKEY=3 THEN 2270
4270 IF FKEY=4 THEN GOSUB 13560:GOTO 4230                 'Zum Hauptmenü?-Frage
4280 '
4290 M=VAL(OUTTEXT$)                                        'RETURN verarbeiten
4300   IF M>=1 AND M<=MTGLMAX THEN 4340
4310   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 12920         'Nachricht
4320   ISBADINPUT=TRUE
4330   GOTO 4220
4340 MTGLZAHL=M
4350 N$="Stammsatz wird gelesen...":GOSUB 13000 'Nachricht. F-Tasten ausblenden
4360 GOSUB 11900                                               'Stammsatz lesen
4370 GOSUB 13060                                             'Nachricht löschen
4380 '
4390 GOSUB 13130                                                   'Basis-Maske
4400 GOSUB 11790                                      'Mitgliedsnummer ausgeben
4410 GOSUB 11660             'Feldnamen, Feldwerte und Feldplatzhalter ausgeben
4420 GOSUB 15850                                         'Drucker bereit?-Frage
4430 IF NOT ISJA THEN 4450
4440   GOSUB 16540                        'Stammsatz drucken (Bildschirmformat)
4450 GOTO 4130
4460 '
4470 ' *** Stammsatz suchen (und drucken) von 05*************** ***************
4480 '
4490 ISDRUCK=FALSE
4500 MT$="STAMMSATZ SUCHEN":IF ISDRUCK THEN MT$="SUCHEN & DRUCKEN"
4510 MI$="G-011050":IF ISDRUCK THEN MI$="G-013010"
4520 GOSUB 13130                                                   'Basis-Maske
4530 FKEYS$="":GOSUB 13240                                 'F-Tasten einblenden
4540 TMP$="Suchen":IF ISDRUCK THEN TMP$="Suchen & drucken" 
4541 TMP1=29:IF ISDRUCK THEN TMP1=24
4550 PRINT FNCUR$(4,TMP1)"1 - ";TMP$;" nach Nummern"
4560 PRINT FNCUR$(6,TMP1)"2 - ";TMP$;" nach Maske"
4570 FKEYMAX=4:NKEYMAX=2:GOSUB 13400                              'Menü-Eingabe
4580 IF NKEY THEN ON NKEY GOTO 4680,5280
4590 IF FKEY<>1 THEN 4630 
4600   HELPID=G0110501:IF ISDRUCK THEN HELPID=G0130102
4610   GOSUB 17390                                                  'Hilfe-Text
4620   GOTO 4500
4630 IF FKEY=2 THEN IF ISDRUCK THEN 7650 ELSE 2420
4640 IF FKEY=3 THEN 2270
4650 IF FKEY=4 THEN GOSUB 13560:GOTO 4570                 'Zum Hauptmenü?-Frage
4660 '
4670 ' *** Suchen nach Nummern
4680 TMP$=STR$(1):NUMSUCH$(1)=MID$(TMP$,2,99)
4690 TMP$=STR$(MTGLMAX):NUMSUCH$(2)=MID$(TMP$,2,99)
4700 IF NOT ISDRUCK THEN MT$="STAMMSATZ SUCHEN NACH NUMMERN":MI$="G-011051"
4710 IF ISDRUCK THEN MT$="SUCHEN & DRUCKEN NACH NUMMERN":MI$="G-013011"
4720 GOSUB 13130                                                   'Basis-Maske
4730 FKEYS$="<F5>=Suchen":IF ISDRUCK THEN FKEYS$="<F5>=Suchen & Drucken"
4740 GOSUB 13240                                           'F-Tasten einblenden
4750 TMP$="gesucht":IF ISDRUCK THEN TMP$="gesucht und gedruckt"
4760 TMP1=12:IF ISDRUCK THEN TMP1=6
4770 TMP2=66:IF ISDRUCK THEN TMP2=73
4780 PRINT FNCUR$(4,TMP1);"Ab welcher Mitgliedsnummer soll ";TMP$;" werden?     =>..."
4790 PRINT FNCUR$(6,TMP1);"Bis zu welcher Mitgliedsnummer soll ";TMP$;" werden? =>..."
4800 PRINT FNCUR$(4,TMP2);HIGHON$;NUMSUCH$(1);HIGHOFF$
4810 PRINT FNCUR$(6,TMP2);HIGHON$;NUMSUCH$(2);HIGHOFF$
4820 PRINT FNCUR$(8,22);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
4830 F=1
4840 ISBADINPUT=FALSE
4850 FKEYMASK$="12345":CKEYMASK$="":INTEXT$=NUMSUCH$(F)
4860 Z=NUMSUCHROW(F):S=67:LNG=3:TYP=2:ISMUSS=TRUE
4870 S=66:IF ISDRUCK THEN S=73
4880 GOSUB 10380                                                'Feld-Eingabe+1
4890 IF FKEY<>1 THEN 4930                                 'F-Tasten verarbeiten
4900   HELPID=G0110510:IF ISDRUCK THEN HELPID=G0130110
4910   GOSUB 17390                                                  'Hilfe-Text
4920   GOTO 4700                                    'TODO: Werte gehen verloren
4930 IF FKEY=2 THEN 4500
4940 IF FKEY=3 THEN 2270
4950 IF FKEY=4 THEN GOSUB 13560:GOTO 4880                 'Zum Hauptmenü?-Frage
4960 IF FKEY<>5 THEN 5020
4970   SEARCH=BYNUMS
4980   NUMSUCH1=VAL(NUMSUCH$(1))
4990   NUMSUCH2=VAL(NUMSUCH$(2))
5000   IF ISDRUCK THEN 8370 ELSE 5600             'Suchen & Drucken oder Suchen
5010 '
5020 M=VAL(OUTTEXT$)                                        'RETURN verarbeiten
5030 IF M>=1 AND M<=MTGLMAX THEN 5070
5040   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 12920         'Nachricht
5050   GOSUB 5210                                     'Eingabefehler-Behandlung
5060   GOTO 4860
5070 NUMSUCH$(F)=OUTTEXT$
5080 NUMSUCH1=VAL(NUMSUCH$(1))
5090 NUMSUCH2=VAL(NUMSUCH$(2))
5100 IF NUMSUCH1<=NUMSUCH2 THEN 5140
5110   N$="Ungültiger Bereich":GOSUB 12920                           'Nachricht
5120   GOSUB 5210                                     'Eingabefehler-Behandlung
5130   GOTO 4860
5140 F=F+1:IF F>NUMSUCHMAX THEN F=1
5150 GOSUB 13500                                           'F-Tasten ausblenden
5160 FKEYS$="<F5>=Suchen":IF ISDRUCK THEN FKEYS$="<F5>=Suchen & Drucken" 
5170 GOSUB 13240                                           'F-Tasten einblenden
5180 GOTO 4850
5190 '
5200 ' *** Eingabefehler-Behandlung
5210 ISBADINPUT=TRUE
5220 FKEYMASK$="1234"
5230 GOSUB 13500                                           'F-Tasten ausblenden
5240 FKEYS$="":GOSUB 13240                                 'F-Tasten einblenden
5250 RETURN
5260 '
5270 ' *** Suchen nach Maske
5280 GOSUB 11180                                                'SUCH$() leeren
5290 IF NOT ISDRUCK THEN MT$="STAMMSATZ SUCHEN NACH MASKE":MI$="G-011052"
5300 IF ISDRUCK THEN MT$="SUCHEN & DRUCKEN NACH MASKE":MI$="G-013012"
5310 GOSUB 13130                                                   'Basis-Maske
5320 FKEYS$="<F5>=Suchen":IF ISDRUCK THEN FKEYS$="<F5>=Suchen & Drucken" 
5330 GOSUB 13240                                           'F-Tasten einblenden
5340 GOSUB 11420                        'Feldnamen und Feldplatzhalter ausgeben
5350 F=1
5360 Z=FEROW(F):S=FECOL(F)+XOFF:LNG=FELNG(F):TYP=FETYP(F):ISMUSS=FEMUSS(F)
5370 FKEYMASK$="12345":CKEYMASK$="12":INTEXT$=""
5380 GOSUB 10370                                                  'Feld-Eingabe
5390 IF FKEY<>1 THEN 5430                                 'F-Tasten verarbeiten
5400   HELPID=G0110520:IF ISDRUCK THEN HELPID=G0130120
5410   GOSUB 17390                                                  'Hilfe-Text
5420   GOTO 5290
5430 IF FKEY=2 THEN 4500
5440 IF FKEY=3 THEN 2270
5450 IF FKEY=4 THEN GOSUB 13560:GOTO 5380                 'Zum Hauptmenü?-Frage
5460 IF FKEY<>5 THEN 5490 
5470   SEARCH=BYMASK                                                      '<F5>
5480   IF ISDRUCK THEN 8370 ELSE 5600             'Suchen oder Suchen & Drucken
5490 IF CKEY=0 THEN 5550                              'Cursortasten verarbeiten
5500   PRINT FNCUR$(Z,S);HIGHON$;SUCH$(F);HIGHOFF$;
5510   PRINT STRING$(LNG-LEN(SUCH$(F)),".")
5520   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FELDMAX
5530   IF CKEY=CKEYDN THEN F=F+1:IF F>FELDMAX THEN F=1
5540   GOTO 5360
5550 SUCH$(F)=OUTTEXT$                                      'RETURN verarbeiten
5560 F=F+1:IF F>FELDMAX THEN F=1
5570 GOTO 5360
5580 '
5590 ' *** Suche
5600 GOSUB 13130                                                   'Basis-Maske
5610 FKEYS$="<F5>=Suchen rückwärts <F6>=Suchen vorwärts <F7>=Adressaufkleber" 
5620 GOSUB 13240                                           'F-Tasten einblenden
5630 PRINT FNCUR$(3,31);"Mitgliedsnummer"
5640 GOSUB 11420                        'Feldnamen und Feldplatzhalter ausgeben
5650 '
5660 IF SEARCH=BYMASK THEN 5710
5670 '
5680 MTGLZAHL=NUMSUCH1                      'Initialisierung Suche nach Nummern
5690 GOTO 5840
5700 '
5710 MTGLZAHL=0                               'Initialisierung Suche nach Maske
5720 GOSUB 6530                        'Nächsten Treffer suchen IN/OUT:MTGLZAHL
5730 IF MTGLZAHL<>-1 THEN 5840
5740   N$="Kein Suchtreffer. Mit <F2> neue Suche.":GOSUB 12920       'Nachricht
5750   FKEYS$="":GOSUB 13240                               'F-Tasten einblenden
5760   FKEYMASK$="1234":NKEYMASK$="":GOSUB 13400                  'Menü-Eingabe
5770   IF FKEY<>1 THEN 5810                               'F-Tasten verarbeiten
5780     HELPID=G0110502:IF ISDRUCK THEN HELPID=G0130103 
5790     GOSUB 17390                                                'Hilfe-Text
5800     GOTO 5290
5810   IF FKEY=2 THEN 5290
5820   IF FKEY=3 THEN 2270
5830   IF FKEY=4 THEN GOSUB 13560:GOTO 5760               'Zum Hauptmenü?-Frage
5840 GOSUB 6280                                 'Suchtreffer lesen und anzeigen
5850 '
5860 FKEYMAX=7:NKEYMAX=0:GOSUB 13400                               'Menü-Eingabe
5870 ON FKEY GOTO 5900,5950,2270,5980,6010,6140,6350
5880 '
5890 ' *** <F1> Hilfe
5900 HELPID=G0110503
5910 GOSUB 17390                                                    'Hilfe-Text
5920 GOTO 5600 'TODO <F6><F6><F1> -> wieder beim 3 Treffer? 'TODO: Werte gehen verloren
5930 '
5940 ' *** <F2> Zurück
5950 IF SEARCH=BYNUMS THEN 4680 ELSE 5290           'TODO: Werte gehen verloren
5960 '
5970 ' *** <F4> Hauptmenü
5980 GOSUB 13560:GOTO 5860                                'Zum Hauptmenü?-Frage
5990 '
6000 ' *** <F5> Suchen rückwärts
6010 IF SEARCH=BYMASK THEN 6050
6020   MTGLZAHL=MTGLZAHL-1                        'Suchen rückwärts nach Nummer
6030   IF MTGLZAHL<NUMSUCH1 THEN MTGLZAHL=NUMSUCH1:GOTO 6100
6040     GOSUB 6280:GOTO 5860                   'Suchtreffer lesen und anzeigen
6050 TMP=MTGLZAHL                                  'Suchen rückwärts nach Maske
6060 GOSUB 6450                      'Vorherigen Treffer suchen IN/OUT:MTGLZAHL
6070 IF MTGLZAHL=-1 THEN 6090
6080   GOSUB 6280:GOTO 5860                     'Suchtreffer lesen und anzeigen
6090 MTGLZAHL=TMP                                               'No match found
6100 N$="Erster Suchtreffer":GOSUB 12920                             'Nachricht
6110 GOTO 5860
6120 '
6130 ' *** <F6> Suchen vorwärts
6140 IF SEARCH=BYMASK THEN 6190
6150   MTGLZAHL=MTGLZAHL+1                         'Suchen vorwärts nach Nummer
6160   IF MTGLZAHL>NUMSUCH2 THEN MTGLZAHL=NUMSUCH2:GOTO 6240
6170     GOSUB 6280:GOTO 5860                   'Suchtreffer lesen und anzeigen
6180 '
6190 TMP=MTGLZAHL                                    'Suche vorwärts nach Maske
6200 GOSUB 6530                        'Nächsten Treffer suchen IN/OUT:MTGLZAHL
6210 IF MTGLZAHL=-1 THEN 6230
6220   GOSUB 6280:GOTO 5860                     'Suchtreffer lesen und anzeigen
6230 MTGLZAHL=TMP                                               'No match found
6240 N$="Letzter Suchtreffer":GOSUB 12920                            'Nachricht
6250 GOTO 5860
6260 '
6270 ' *** Suchtreffer lesen und anzeigen
6280 GOSUB 13060                                             'Nachricht löschen
6290 GOSUB 11900                                               'Stammsatz lesen
6300 GOSUB 11790                                      'Mitgliedsnummer ausgeben
6310 GOSUB 11540                        'Feldwerte und Feldplatzhalter ausgeben
6320 RETURN
6330 '
6340 ' *** <F7> Adressaufkleber drucken
6350 GOSUB 15850                                         'Drucker bereit?-Frage
6360 IF NOT ISJA THEN 6400
6370   N$="Adressaufkleber wird gedruckt...":GOSUB 12920             'Nachricht
6380   GOSUB 16360                                     'Adressaufkleber drucken
6390   GOSUB 13060                                           'Nachricht löschen
6400 GOSUB 13240                                           'F-Tasten einblenden
6410 GOTO 5860                                                    'Neue Eingabe
6420 '
6430 ' *** Vorherigen Treffer suchen
6440 ' IN: MTGLZAHL OUT: MTGLZAHL oder -1 wenn kein Treffer
6450 MTGLZAHL=MTGLZAHL-1
6460 IF MTGLZAHL<1 THEN 6590
6470 GOSUB 6630
6480 IF NOT ISMATCH THEN 6450 
6490 RETURN
6500 '
6510 ' *** Nächsten Treffer suchen
6520 ' IN: MTGLZAHL OUT: MTGLZAHL oder -1 wenn kein Treffer
6530 MTGLZAHL=MTGLZAHL+1
6540 IF MTGLZAHL>MTGLMAX THEN 6590
6550 GOSUB 6630
6560 IF NOT ISMATCH THEN 6530 
6570 RETURN
6580 '
6590 MTGLZAHL=-1
6600 RETURN
6610 '
6620 ' *** Treffer?
6630 N$="Vergleiche Stammsatz"+STR$(MTGLZAHL)+"...":GOSUB 12920 'LW 'Nachricht
6631 GOSUB 11900                                               'Stammsatz lesen
6640 FOR K=1 TO FELDMAX
6650   IF LEFT$(SUCH$(K),1)<>"*" THEN 6680
6660     IF INSTR(DAT$(K),RIGHT$(SUCH$(K),LEN(SUCH$(K))-1))>0 THEN 6730
6670     GOTO 6770
6680   I=1
6690   IF MID$(SUCH$(K),I,1)="?" THEN I=I+1:GOTO 6690
6700   I=I-1
6710   IF INSTR(DAT$(K),RIGHT$(SUCH$(K),LEN(SUCH$(K))-I))=I+1 THEN 6730
6720   GOTO 6770
6730 NEXT K
6740 ISMATCH=TRUE
6750 RETURN
6760 '
6770 ISMATCH=FALSE
6780 RETURN
6790 '
6800 ' ######################### Verwendungsnachweis ##########################
6810 '
6820 GOSUB 11240                                                 'MTGLMAX lesen
6830 VNFLAG=TRUE
6840 '
6850 ' Verwendungsnachweis-Menü ***********************************************
6860 '
6870 MT$="VERWENDUNGSNACHWEIS":MI$="G-012000":GOSUB 13130          'Basis-Maske
6880 F=4                                                                'TODO ?
6890 FKEYS$="":GOSUB 13240                                 'F-Tasten einblenden
6900 PRINT CURON$;
6910 PRINT HIGHOFF$;
6920 PRINT FNCUR$(4,20)"   Bitte geben Sie die Mitgliedsnummer"
6930 PRINT FNCUR$(5,20)"des zu ändernden Verwendungsnachweises ein"
6940 'LW PRINT FNCUR$(6,23);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
6950 PRINT FNCUR$(6,20)" und beenden Sie die Eingabe mit RETURN."
6960 PRINT FNCUR$(8,20)"                 =>..."
6970 Z=8:S=39:LNG=3:TYP=2
6980 GOSUB 10370                                                  'Feld-Eingabe
6990 IF FKEY=1 THEN HELPID=G0120001:GOSUB 17390:GOTO 6870           'Hilfe-Text
7000 IF FKEY=2 OR FKEY=3 THEN GOSUB 11130:VNFLAG=FALSE:GOTO 2270     'DAT$() leeren
7010 IF FKEY=4 THEN GOSUB 13560                           'Zum Hauptmenü?-Frage
7020 IF FKEY>4 THEN 6970
7030 IF CKEY<>0 THEN 6970
7040 IF LEN(OUTTEXT$)=0 THEN N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 12920:GOTO 6960 ELSE MITGLIED=VAL(OUTTEXT$):IF MITGLIED=0 THEN 6980 'Nachricht
7050 IF MITGLIED>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12920:PRINT HOME$;HIGHOFF$:GOTO 6960 'Nachricht
7060 MTGLZAHL=MITGLIED:MTGL$=STR$(MITGLIED)
7070 '
7080 ' *** Lesen des Mitgliedsnamens ******************************************
7090 '
7100 PRINT FNCUR$(21,1);DELLINE$;
7110 GET #2,MITGLIED                                           'DATEN STS lesen
7120 NNAM$=LEFT$(DD1$,25)                                 'NACHNAME RAUSFISCHEN
7130 VNAM$=MID$(DD1$,26,25)                               'VORNAME  RAUSFISCHEN
7140 '
7150 ' ** LEERZEICHEN HINTER NACHNAME ABSCHNEIDEN *****************************
7160 '
7170 FOR I=LEN(NNAM$) TO 1 STEP -1
7180   IF MID$(NNAM$,I,1)<>" " THEN NNAM$=LEFT$(NNAM$,I):I=1
7190 NEXT
7200 NACHVORNAME$=NNAM$+" "+VNAM$
7210 GOSUB 14080                                             '1. VN-BLATT lesen
7220 BLATT=1
7230 VP=1:HP=1                                                 'INITIALISIERUNG
7240 F=8
7250 GOSUB 13130                                                   'Basis-Maske
7260 FKEYS$="<F7>=Speichern":GOSUB 13240                   'F-Tasten einblenden
7270 PRINT HIGHOFF$;
7280 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=Vorwärts";TAB(53);"<F9>=Blatt drucken";
7290 PRINT FNCUR$(3,2);"Blatt:"
7300 PRINT FNCUR$(5,2);"Mitglieds-Nr.:"
7310 PRINT FNCUR$(5,24);"Name:"
7320 MSK$="Von:  .  .   Bis:  .  .   Amt:"
7330 FOR KK=7 TO 19 
7340   PRINT FNCUR$(KK,2);MSK$;TAB(43);MSK$                    'LW NO semicolon
7350 NEXT
7360 GOSUB 15290                                                 'Ausgabe Daten
7370 LNG=2-(HP=7)*6                                    'BEGIN OF THE INPUT-LOOP
7380 TYP=2-(HP=7)
7390 Z=VP+13*(VP>13)+6
7400 S=6-39*(VP>13)-(HP<7)*(HP-1)*3+(HP>3)*(HP<7)*4-(HP=7)*26
7410 GOSUB 13060                                             'Nachricht löschen
7420 GOSUB 10370                                                  'Feld-Eingabe
7430 IF HP<7 THEN IF ASCC=ASCRET THEN OUTTEXT$=STRING$(2-LEN(OUTTEXT$),"0")+OUTTEXT$   '+NULLEN
7440 IF CKEY<>0 OR ASCC=ASCRET THEN PRINT FNCUR$(Z,S);OUTTEXT$
7450 IF ASCC=ASCRET THEN IF HP<7 THEN MID$(DAT$(VP),-1+2*HP,2)=OUTTEXT$ ELSE DAT$(VP)=LEFT$(DAT$(VP),12)+OUTTEXT$+SPACE$(8-LEN(OUTTEXT$))
7460 IF ASCC=ASCRET AND HP<7 THEN ISJA=TRUE:TEST$=OUTTEXT$:ON HP GOSUB 12680,12710,12740,12680,12710,12740:IF NOT ISJA THEN N$="Ungültiges Datum":GOSUB 12920:GOTO 7420 'Datums-Check 'Nachricht
7470 IF ASCC=ASCRET THEN IF HP=7 THEN HP=1:VP=VP-(VP<26)-(VP=26)*(-25):GOTO 7370 ELSE HP=HP+1:GOTO 7370     'NEUE SCREEN-POSITION
7480 IF CKEY<>0 THEN TEST$=MID$(DAT$(VP),-(HP<7)*2*HP-1-(HP=7)*14,-(HP<7)*2-(HP=7)*8):PRINT FNCUR$(Z,S);TEST$
7490 IF CKEY<>0 THEN IF HP<7 AND NOT TEST$="  " THEN ISJA=TRUE:ON HP GOSUB 12680,12710,12740,12680,12710,12740:IF NOT ISJA THEN N$="Ungültiges Datum":GOSUB 12920:GOTO 7420 'Nachricht
7500 IF CKEY=CKEYDN THEN VP=VP-(VP<26)-(VP=26)*(-25):GOTO 7370
7510 IF CKEY=CKEYUP THEN VP=VP+(VP>1)-(VP=1)*25     :GOTO 7370
7520 IF CKEY=CKEYRT THEN HP=HP-(HP<7)-(HP=7)*(-6)   :GOTO 7370
7530 IF CKEY=CKEYLT THEN HP=HP+(HP>1)+(HP=1)*(-6)   :GOTO 7370
7540 IF FKEY=1 THEN HELPID=G0120002:GOSUB 17390:GOTO 7230           'Hilfe-Text
7550 IF FKEY=2 THEN 6870                                       'ZU MTGL-EINGABE
7560 IF FKEY=3 THEN VNFLAG=FALSE:GOSUB 11130:GOTO 2270               'DAT$() leeren
7570 IF FKEY=4 THEN GOSUB 13560:GOSUB 13300:GOTO 7420     'Zum Hauptmenü?-Frage
7580 IF FKEY=5 THEN GOSUB 14170:GOTO 7370
7590 IF FKEY=6 THEN GOSUB 14310:GOTO 7370
7600 IF FKEY=7 THEN GOSUB 14950:GOTO 7420                 'SPEICHERN DANN INPUT
7610 IF FKEY=9 THEN GOSUB 15530:GOSUB 14500:GOTO 7410
7620 '
7630 ' ############# Mitgliedsliste drucken von 05*************** #############
7640 '
7650 MT$="MITGLIEDSLISTE DRUCKEN":MI$="G-013000":GOSUB 13130       'Basis-Maske
7660 FKEYS$="":GOSUB 13240                                 'F-Tasten einblenden
7670 PRINT FNCUR$(4,30);"1 - Suchen & Drucken"
7680 PRINT FNCUR$(6,30);"2 - Komplett drucken"
7690 PRINT FNCUR$(8,30);"3 - Datenblatt drucken"
7700 FKEYMAX=4:NKEYMAX=3:GOSUB 13400                              'Menü-Eingabe
7710 IF NKEY THEN ON NKEY GOTO 7790,7790,8780
7720 IF FKEY=1 THEN HELPID=G0130000:GOSUB 17390:GOTO 7650           'Hilfe-Text
7730 IF FKEY=2 THEN 2270
7740 IF FKEY=3 THEN 2270
7750 IF FKEY=4 THEN GOSUB 13560:GOTO 7700                 'Zum Hauptmenü?-Frage
7760 '
7770 ' *** Suchen & Drucken, Komplett drucken *********************************
7780 '
7790 IF NKEY=1 THEN MT$="SUCHEN & DRUCKEN":MI$="G-013010"
7800 IF NKEY=2 THEN MT$="KOMPLETT DRUCKEN":MI$="G-013020"
7810 GOSUB 13130                                                   'Basis-Maske
7820 GOSUB 13240                                           'F-Tasten einblenden
7830 PRINT FNCUR$(4,22);"Bitte geben Sie das heutige Datum ein."
7840 PRINT FNCUR$(5,23);"Beenden Sie die Eingabe mit RETURN."
7850 PRINT FNCUR$(7,36);"=>........"
7860 ISBADINPUT=FALSE
7870 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
7880 Z=7:S=38:LNG=8:TYP=5:ISMUSS=TRUE
7890 GOSUB 10380                                                'Feld-Eingabe+1
7900 IF FKEY<>1 THEN 7940       'F-Tasten verarbeiten 'TODO: Wert geht verloren
7910   IF NKEY=1 THEN HELPID=G0130101
7911   IF NKEY=2 THEN HELPID=G0130200
7920   GOSUB 17390                                                  'Hilfe-Text
7930   GOTO 7790
7940 IF FKEY=2 THEN 7650
7950 IF FKEY=3 THEN 2270
7960 IF FKEY=4 THEN GOSUB 13560:GOTO 7890                 'Zum Hauptmenü?-Frage
7970 '
7980 POS1=INSTR(OUTTEXT$,".")                               'RETURN verarbeiten
7990 POS2=INSTR(POS1+1,OUTTEXT$,".")
8000 IF POS2=0 THEN 8230
8010 POS3=INSTR(POS2+1,OUTTEXT$,".")
8020 IF POS3<>0 THEN 8230
8030 D1$=LEFT$(OUTTEXT$,POS1-1)
8040 IF LEN(D1$)=0 OR LEN(D1$)>2 THEN 8230
8050 D2$=MID$(OUTTEXT$,POS1+1,POS2-POS1-1)
8060 IF LEN(D2$)=0 OR LEN(D2$)>2 THEN 8230
8070 D3$=RIGHT$(OUTTEXT$,LEN(OUTTEXT$)-POS2)
8080 IF LEN(D3$)=0 OR LEN(D3$)>2 THEN 8230
8090 '
8100 D1=VAL(D1$):IF D1<1 OR D1>31 THEN 8260
8110 D2=VAL(D2$):IF D2<1 OR D2>12 THEN 8260
8120 D3=VAL(D3$) 
8130 ' TODO: Genauere Verprobung
8140 D1$=STR$(D1):D1$=MID$(D1$,2,99):IF D1<10 THEN D1$="0"+D1$
8150 D2$=STR$(D2):D2$=MID$(D2$,2,99):IF D2<10 THEN D2$="0"+D2$
8160 D3$=STR$(D3):D3$=MID$(D3$,2,99):IF D3<10 THEN D3$="0"+D3$
8170 DATUM$=D1$+"."+D2$+"."+D3$
8180 '
8190 PRINT FNCUR$(7,37);HIGHON$;DATUM$;HIGHOFF$
8200 GOTO 8300
8210 '
8220 ' *** Eingabe-Fehlerbehandlung Datum
8230 N$="Datum muss Format TT.MM.JJ haben. Bitte korrigieren.":GOSUB 12920 'Nachricht
8240 GOTO 8270
8250 '
8260 N$="Ungültiges Datum. Bitte korrigieren.":GOSUB 12920           'Nachricht
8270 ISBADINPUT=TRUE
8280 GOTO 7880
8290 '
8300 IF NKEY=2 THEN 8420
8310 '
8320 ' *** Suchen nach Nummern und Maske
8330 ISDRUCK=TRUE
8340 GOTO 4500                                  'Stammsatz suchen (und drucken)
8350 ' *** Rückkehr aus "Stammsatz suchen"
8360 ' IN: SEARCH (BYNUMS, BYMASK), NUMSUCH1, NUMSUCH2, SUCH$()
8370 GOSUB 15850                                         'Drucker bereit?-Frage
8380 IF NOT ISJA THEN 8740
8390 GOTO 8470
8400 '
8410 ' *** Komplette Mitgliedliste drucken
8420 SEARCH=BYNUMS
8430 NUMSUCH1=1:NUMSUCH2=MTGLMAX
8440 GOSUB 15850                                         'Drucker bereit?-Frage
8450 IF NOT ISJA THEN 7790
8460 '
8470 N$="Mitgliedsliste wird gedruckt... Abbrechen mit Taste.":GOSUB 12920 'Nachricht
8480 GOSUB 16020                                 'Mitgliedsliste drucken (Kopf)
8490 IF SEARCH=BYMASK THEN 8620
8500 '
8510 ' *** Suchen nach Nummern
8520 FOR MTGLZAHL=NUMSUCH1 TO NUMSUCH2
8530   GOSUB 11900                                             'Stammsatz lesen
8540   IF INSTR(DAT$(5),"ausgetreten")>0 THEN 8560
8550     GOSUB 16160                         'Mitgliedsliste drucken (Mitglied)
8560   GOSUB 15920                                      'Druck abbrechen?-Frage
8570   IF ISJA THEN 8730
8580 NEXT MTGLZAHL
8590 GOTO 8730
8600 ' 
8610 ' *** Suchen nach Maske
8620 MTGLZAHL=0
8630 GOSUB 6530                        'Nächsten Treffer suchen IN/OUT:MTGLZAHL
8640 IF MTGLZAHL=-1 THEN 8730
8650   GOSUB 11900                    'Stammsatz lesen, IN:MTGLZAHL, OUT:DAT$()
8660   IF INSTR(DAT$(5),"ausgetreten")>0 THEN 8680
8670     GOSUB 16160                         'Mitgliedsliste drucken (Mitglied)
8680   GOSUB 15920                                      'Druck abbrechen?-Frage
8690   IF ISJA THEN 8730
8700 GOTO 8640
8710 '
8720 ' *** Suchen zuende
8730 GOSUB 13060                                             'Nachricht löschen
8740 IF SEARCH=BYNUMS THEN 4680 ELSE 5280                'TODO: Very hackish...
8750 '
8760 ' *** Datenblatt drucken *************************************************
8770 '
8780 MT$="DATENBLATT DRUCKEN":MI$="G-013030":GOSUB 13130           'Basis-Maske
8790 FKEYS$="":GOSUB 13240                                 'F-Tasten einblenden
8800 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer"
8810 PRINT FNCUR$(5,23)"des zu druckenden Datenblattes ein."
8820 PRINT FNCUR$(6,22);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
8830 PRINT FNCUR$(7,23)"Beenden Sie die Eingabe mit RETURN."
8840 PRINT FNCUR$(10,38)"=>..."
8850 ISBADINPUT=FALSE
8860 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
8870 Z=10:S=40:LNG=3:TYP=2:ISMUSS=TRUE
8880 GOSUB 10380                                                'Feld-Eingabe+1
8890 IF FKEY=1 THEN HELPID=G0130300:GOSUB 17390:GOTO 8780 'Hilfe-Text 'TODO: Wert geht verloren
8900 IF FKEY=2 THEN 7650
8910 IF FKEY=3 THEN 2270
8920 IF FKEY=4 THEN GOSUB 13560:GOTO 8880                 'Zum Hauptmenü?-Frage
8930 '
8940 M=VAL(OUTTEXT$)                                        'RETURN verarbeiten
8950 IF M>=1 AND M<=MTGLMAX THEN 8990
8960   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 12920         'Nachricht
8970   ISBADINPUT=TRUE
8980   GOTO 8870
8990 MTGLZAHL=M
9000 GOSUB 15850                                         'Drucker bereit?-Frage
9010 IF ISJA THEN 9050
9020   GOSUB 13240                                         'F-Tasten einblenden
9030   INTEXT$=OUTTEXT$
9040   GOTO 8870
9050 N$="Datenblatt drucken...":GOSUB 13000     'Nachricht. F-Tasten ausblenden
9060 GOSUB 11900                                               'Stammsatz lesen
9070 NACHVORNAME$ = DAT$(1)+" "+DAT$(2)
9080 GOSUB 16540                          'Stammsatz drucken (Bildschirmformat)
9090 GOSUB 14080                                     'Verwendungsnachweis lesen
9100 GOSUB 15560                                   'Verwendungsnachweis drucken
9110 GOSUB 16810                                     'Mitgliedsvermerke drucken
9120 GOSUB 13060                                             'Nachricht löschen
9130 GOTO 8780
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
10270 ' IN: Z   - Zeile  OUT: OUTTEXT$ TMP: P
10280 ' IN: S   - Spalte OUT: FKEY     TMP: ISVALID (-1=Ja, 0=Nein)
10290 ' IN: LNG - Länge  OUT: CKEY     TMP: ISBADINPUT (-1=Ja, 0=Nein)
10300 ' IN: TYP - Typ    OUT: ASCC
10310 ' IN: ISMUSS (-1=Ja, 0=Nein)
10320 ' IN: INTEXT$
10330 ' IN: FKEYMASK$
10340 ' IN: CKEYMASK$
10350 ' IN: VNFLAG
10360 '
10370 ISBADINPUT=FALSE
10380 IF ISBADINPUT THEN INTEXT$=OUTTEXT$
10390 P=LEN(INTEXT$)
10400 PRINT FNCUR$(Z,S);HIGHON$;INTEXT$;HIGHOFF$;STRING$(LNG-P,".")
10410 OUTTEXT$=INTEXT$
10420 PRINT CURON$;
10430 CKEY=0:FKEY=0
10440 PRINT FNCUR$(Z,S+P);
10450 A$=INPUT$(1):ASCC=ASC(A$)
10460 IF NOT ISBADINPUT THEN 10500
10470   GOSUB 13060                                          'Nachricht löschen
10480   PRINT FNCUR$(Z,S+P);
10490   ISBADINPUT=FALSE
10500 ON TYP GOSUB 12420,12480,12530,12580,12630                    'Typ prüfen
10510 IF NOT ISVALID THEN 10570
10520   IF P>=LNG THEN 10450                                  'Gültiges Zeichen
10530   PRINT HIGHON$;A$;HIGHOFF$         
10540   OUTTEXT$=OUTTEXT$+A$
10550   P=P+1
10560   GOTO 10440
10570 IF ASCC<>ASCBSP THEN 10630                         'Backspace verarbeiten
10580   IF P<1 THEN 10450
10590   PRINT DEL$                        
10600   P=P-1
10610   OUTTEXT$=LEFT$(OUTTEXT$,P)
10620   GOTO 10440
10630 IF ASCC=ASCESC THEN 10370                             'ESCAPE verarbeiten
10640 IF ASCC=ASCRET THEN 10770                             'RETURN verarbeiten
10650 IF ASCC=ASCCUP THEN CKEY=CKEYUP:IF INSTR(CKEYMASK$,CHR$(CKEY+ASC0))>0 THEN 10770
10660 IF ASCC=ASCCDN THEN CKEY=CKEYDN:IF INSTR(CKEYMASK$,CHR$(CKEY+ASC0))>0 THEN 10770
10670 IF VNFLAG=FALSE THEN 10700
10680   IF ASCC=ASCCLT THEN CKEY=CKEYLT:IF INSTR(CKEYMASK$,CHR$(CKEY+ASC0))>0 THEN 10770 
10690   IF ASCC=ASCCRT THEN CKEY=CKEYRT:IF INSTR(CKEYMASK$,CHR$(CKEY+ASC0))>0 THEN 10770 
10700 IF ASCC<ASCF1 OR ASCC>ASCF9 THEN 10730              'F-Tasten verarbeiten
10710   FKEY=ASCC-ASCF1+1
10720   IF INSTR(FKEYMASK$,CHR$(FKEY+ASC0))>0 THEN 10820
10730 N$="Ungültige Eingabe - Bitte wiederholen":GOSUB 12920         'Nachricht
10740 ISBADINPUT=TRUE
10750 GOTO 10440
10760 '
10770 IF NOT ISMUSS OR LEN(OUTTEXT$)>0 THEN 10810           'RETURN verarbeiten
10780   N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 12920
10790   ISBADINPUT=TRUE 
10800   GOTO 10440
10810 IF CKEY<>0 THEN GOSUB 10870       'Cursortasten Auto-Wiederholung sperren
10820 PRINT CUROFF$
10830 RETURN 
10840 '
10850 ' *** Cursortasten Auto-Wiederholung sperren ****************************
10860 '
10870 TMP$=INKEY$:IF TMP$>=CHR$(ASCCUP) AND TMP$<=CHR$(ASCCRT) THEN 10870
10880 RETURN
10890 '
10900 ' *** Tastatur entprellen ***********************************************
10910 '
10920 FOR K=1 TO 30:A$=INKEY$:NEXT K
10930 RETURN
10940 '
10950 ' *** Stammsatzfelde-Definitionen lesen ********************************* 
10960 '
10970 OPEN "I",#1,"B:FELD.DAT"                        
10980   INPUT #1,DUMMY
10990   FOR K=1 TO FELDMAX
11000     INPUT #1,F$
11010     FENAM$(K)=LEFT$(F$,14)
11020     IF MID$(F$,13,1)="=" THEN FEMUSS(K)=TRUE ELSE FEMUSS(K)=FALSE                           
11030     FECOL(K)=VAL(MID$(F$,15,2))
11040     FEROW(K)=VAL(MID$(F$,17,2))
11050     FELNG(K)=VAL(MID$(F$,19,2))
11060     FETYP(K)=VAL(RIGHT$(F$,1)) 
11070   NEXT K
11080 CLOSE #1
11090 RETURN
11100 '
11110 ' *** DAT$() leeren *****************************************************
11120 '
11130 FOR K=1 TO FELDMAX:DAT$(K)="":NEXT K
11140 RETURN
11150 '
11160 ' *** SUCH$() leeren ****************************************************
11170 '
11180 FOR K=1 TO FELDMAX:SUCH$(K)="":NEXT K
11190 RETURN
11200 '
11210 ' *** MTGLMAX lesen ***************************************************** 
11220 '
11230 ' OUT: MTGLMAX
11240 OPEN "I",#1,"B:MTGLZAHL.DAT"
11250   INPUT #1,MTGLMAX$
11260   MTGLMAX=VAL(MTGLMAX$)
11270 CLOSE #1
11280 RETURN
11290 '
11300 ' *** MTGLMAX speichern *************************************************
11310 '
11320 ' IN: MTGLMAX
11330 OPEN "O",#1,"B:MTGLZAHL.DAT"
11340   PRINT #1,STR$(MTGLMAX)
11350 CLOSE #1
11360 RETURN
11370 '
11380 ' *** Maske Feldnamen und Feldplatzhalter ausgeben **********************
11390 '
11400 ' IN: FELDMAX
11410 ' IN: MTGLZAHL
11420 FOR K=1 TO FELDMAX
11430   Z=FEROW(K)
11440   S=FECOL(K)
11450   PRINT FNCUR$(Z,S);FENAM$(K)
11460   S=S+XOFF
11470   PRINT FNCUR$(Z,S);STRING$(FELNG(K),".")
11480 NEXT K
11490 RETURN
11500 '
11510 ' *** Maske Feldwerte und Feldplatzhalter ausgeben **********************
11520 '
11530 ' IN: FELDMAX
11540 FOR K=1 TO FELDMAX
11560   Z=FEROW(K)
11570   S=FECOL(K)+XOFF
11580   PRINT FNCUR$(Z,S);HIGHON$;DAT$(K);HIGHOFF$;
11581   PRINT LEFT$(PUNKTE$,FELNG(K)-LEN(DAT$(K)))         
11590 NEXT K
11610 RETURN
11620 '
11630 ' *** Maske Feldnamen, Feldwerte und Feldplatzhalter ausgeben ***********
11640 '
11650 ' IN: FELDMAX
11660 FOR K=1 TO FELDMAX
11670   Z=FEROW(K)
11680   S=FECOL(K)
11690   PRINT FNCUR$(Z,S);FENAM$(K)
11700   S=S+XOFF
11710   PRINT FNCUR$(Z,S);HIGHON$;DAT$(K);HIGHOFF$;
11720   PRINT STRING$(FELNG(K)-LEN(DAT$(K)),".")               
11730 NEXT K
11740 RETURN
11750 '
11760 ' *** Mitgliedsnummer ausgeben ******************************************
11770 '
11780 ' IN: MTGLZAHL
11790 PRINT FNCUR$(3,31);"Mitgliedsnummer";HIGHON$;MTGLZAHL;HIGHOFF$;"   "
11800 RETURN
11810 '
11820 ' *** FIELDen der Stammsatz-Datei *************************************** 
11830 '
11840 OPEN "R",#2,"B:MV.DAT",462
11850 FIELD #2,236 AS XSTAMMSATZ1$,225 AS XSTAMMSATZ2$
11860 RETURN
11870 '
11880 ' *** Stammsatz von Disk lesen ******************************************
11890 '
11900 GET #2,MTGLZAHL
11910 P=1
11920 FOR K=1 TO 14
11930   LNG=FELNG(K)
11940   TMP$=MID$(XSTAMMSATZ1$,P,LNG):GOSUB 12050:DAT$(K)=TMP$
11950   P=P+LNG
11960 NEXT
11970 P=1
11980 FOR K=15 TO FELDMAX
11990   LNG=FELNG(K)
12000   TMP$=MID$(XSTAMMSATZ2$,P,LNG):GOSUB 12050:DAT$(K)=TMP$
12010   P=P+LNG
12020 NEXT
12030 RETURN
12040 '
12050 FOR KK=LEN(TMP$) TO 1 STEP -1
12060   IF MID$(TMP$,KK,1)<>" " THEN 12080
12070 NEXT KK
12080 TMP$=LEFT$(TMP$,KK)
12090 RETURN
12100 '
12110 ' *** Stammsatz auf Disk speichern **************************************
12120 '
12130 ' IN: MTGLZAHL
12140 TMP$=""
12150 FOR K=1 TO 14
12160   LNG=FELNG(K)
12170   TMP$=TMP$+DAT$(K)+SPACE$(LNG-LEN(DAT$(K)))
12180 NEXT K
12190 LSET XSTAMMSATZ1$=TMP$
12200 TMP$=""
12210 FOR K=15 TO FELDMAX
12220   LNG=FELNG(K)
12230   TMP$=TMP$+DAT$(K)+SPACE$(LNG-LEN(DAT$(K)))
12240 NEXT K
12250 LSET XSTAMMSATZ2$=TMP$
12260 PUT #2,MTGLZAHL
12270 RETURN
12280 '
12290 ' *** Ja/Nein-Frage und F-Tasten ausblenden *****************************
12300 '
12310 ' IN:  Q$   - Frage
12320 ' OUT: ISJA - (-1=Ja, 0=Nein)
12330 N$=Q$+" (J=Ja/N=Nein)":GOSUB 12920                             'Nachricht
12340 GOSUB 13500                                          'F-Tasten ausblenden
12350 ANSWER$=INPUT$(1)
12360 IF ANSWER$="J" OR ANSWER$="j" THEN ISJA=TRUE:RETURN
12370 IF ANSWER$="N" OR ANSWER$="n" THEN ISJA=FALSE:RETURN
12380 GOTO 12350
12390 '
12400 ' *** Testbereich 1 (Alpha) *********************************************
12410 '
12420 ISVALID=FALSE
12430 IF (ASCC>57 AND ASCC<127) OR (ASCC>31 AND ASCC<48) THEN ISVALID=TRUE
12440 RETURN
12450 '
12460 ' *** Testbereich 2 (Numerisch) *****************************************
12470 '
12480 ISVALID=FALSE: IF ASCC>47 AND ASCC<58 THEN ISVALID=TRUE
12490 RETURN
12500 '
12510 ' *** Testbereich 3 (Alphanumerisch) ************************************
12520 '
12530 ISVALID=FALSE: IF ASCC>31 AND ASCC<127 THEN ISVALID=TRUE
12540 RETURN
12550 '
12560 ' *** Testbereich 4 (Numerisch + "/" und ".") ***************************
12570 '
12580 ISVALID=FALSE: IF ASCC>45 AND ASCC<58 THEN ISVALID=TRUE
12590 RETURN
12600 '
12610 ' *** Testbereich 5 (Numerisch + und ".") *******************************
12620 '
12630 ISVALID=FALSE: IF (ASCC>=48 AND ASCC<=57) OR ASCC=46 THEN ISVALID=TRUE
12640 RETURN
12650 '
12660 ' *** DATUMS-TEST (bei VN und M-Liste drucken) **************************
12670 '
12680 IF VAL(TEST$)=0 OR VAL(TEST$)>31 THEN ISJA=FALSE                         'Tag
12690 RETURN
12700 '
12710 IF VAL(TEST$)=0 OR VAL(TEST$)>12 THEN ISJA=FALSE                       'Monat
12720 RETURN
12730 '
12740 IF VAL(TEST$)=0 THEN ISJA=FALSE                                        'Jahr
12750 RETURN
12760 '
12770 ' *** Voreinstellungen zuweisen in "Stammsatz anlegen" ******************
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
12890 ' *** Nachricht anzeigen ************************************************
12900 '
12910 ' IN: N$ - Nachricht
12920 PRINT FNCUR$(21,1);DELLINE$;
12930 IF N$<>"" THEN PRINT INVON$;" ";N$;" ";INVOFF$;
12940 PRINT                              'LW workaround quirk in display driver
12950 RETURN
12960 '
12970 ' *** Nachricht anzeigen und F-Tasten ausblenden ************************ 
12980 '
12990 ' IN: N$ - Nachricht
13000 GOSUB 12920                                                    'Nachricht
13010 GOSUB 13500                                          'F-Tasten ausblenden
13020 RETURN
13030 '
13040 ' *** Nachricht löschen *************************************************
13050 '
13060 N$="":GOSUB 12920                                              'Nachricht
13070 RETURN
13080 '
13090 ' *** Bildschirmmaske - Basis *******************************************
13100 '
13110 ' IN: MT$ - Menü-Titel
13120 ' IN: MI$ - Menu-ID
13130 PRINT CLS$;
13140 PRINT "00**********************";
13150 PRINT FNCUR$(1,(80-LEN(MT$))/2+1);MT$;FNCUR$(1,72);MI$
13160 PRINT STRICH$
13170 PRINT FNCUR$(21,1);
13180 PRINT STRICH$             'quirk in display driver places this in line 22
13190 RETURN
13200 '
13210 ' *** F-Tasten einblenden ***********************************************
13220 '
13230 ' IN: FKEYS$ - Zusätzliche F-Tasten 
13240 PRINT FNCUR$(23,1);"<F1>=Hilfe <F2>=Zurück <F3>=Anfang <F4>=Hauptmenü";
13250 IF FKEYS$<>"" THEN PRINT " ";FKEYS$;HOME$ ELSE PRINT             'LW Hack           
13260 RETURN
13270 '
13280 ' *** restliche funktionstasten bei Verw.-nachweis ausgeben
13290 '
13300 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=Vorwärts             <F8>=Drucken";
13310 PRINT HOME$
13320 RETURN
13330 '
13340 ' *** Eingabe bei Menübildern *******************************************
13350 '
13360 ' IN:  NKEYMAX - Max gültige Zifferntaste (1,2,3,...)
13370 ' IN:  FKEYMAX - Max gültige F-Taste (1,2,3,...)
13380 ' OUT: NKEY    - Ziffer der gedrückten Zifferntaste oder 0
13390 ' OUT: FKEY    - Ziffer der gedrückten F-Taste oder 0
13400 NKEY=0:FKEY=0
13410 GOSUB 10920                                          'Tastatur entprellen
13420 A$=INPUT$(1):ASCC=ASC(A$)
13430 IF ASCC>=ASC1 AND ASCC<=ASC1+NKEYMAX-1 THEN NKEY=ASCC-ASC1+1:RETURN
13440 IF ASCC>=ASCF1 AND ASCC<=ASCF1+FKEYMAX-1 THEN FKEY=ASCC-ASCF1+1:RETURN
13450 N$="Sie haben eine falsche Taste gedrückt - Bitte neue Auswahl treffen.":GOSUB 12920 'Nachricht
13460 GOTO 13420
13470 '
13480 ' *** F-Tasten ausblenden ************************************** REVIEWED
13490 '
13500 PRINT FNCUR$(23,1);DELLINE$
13510 PRINT FNCUR$(24,1);DELLINE$;
13520 RETURN
13530 '
13540 ' *** 'Zum Hauptmenü?-Frage (Original) ************************* REVIEWED
13550 '
13560 GOSUB 13640                               'Zum Hauptmenü?-Frage (einfach)
13570 IF ISJA THEN 17670                                          'Programmende
13580 GOSUB 13240                                          'F-Tasten einblenden
13590 RETURN
13600 '
13610 ' *** 'Zum Hauptmenü?-Frage (einfach) ************************** REVIEWED
13620 '
13630 ' OUT: ISJA (-1=Ja, 0=Nein)
13640 Q$="Wollen Sie wirklich ins Hauptmenü?":GOSUB 12330        'Ja/Nein-Frage
13650 GOSUB 13060                                            'Nachricht löschen
13660 RETURN
13670 '
13680 ' *** Rückkehr aus Hilfe-Text? ********************************* REVIEWED
13690 '
13700 N$="Zurück? (J=Ja)":GOSUB 12920                                'Nachricht
13710 GOSUB 12340                             'Ja/Nein-Frage ohne String-Koncat
13720 IF NOT ISJA THEN 13700
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
13910 GET #3,1                               'letzte Seitennummer lesen dann +1
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
14100 GET #5,CPOINT:GOSUB 15130                                          'Lesen
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
14430 Q$="Letzte Seite - Neue Seite anlegen?":GOSUB 12330        'Ja/Nein-Frage
14440 GOSUB 13060                                            'Nachricht löschen
14450 IF NOT ISJA THEN 14500
14460 GOSUB 14740
14470 BLATT=BLATT+1
14480 GOSUB 15290
14490 HP=1:VP=1
14500 GOSUB 13240                                          'F-Tasten einblenden
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
15270 ' IN: NACHVORNAME$
15280 ' IN: BLATT
15290 PRINT DELHIGH$;                                          'TODO Necessary?
15300 PRINT HIGHON$
15310 PRINT FNCUR$(5,16);MTGLZAHL
15320 PRINT FNCUR$(5,29);NACHVORNAME$
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
15520 ' IN: NACHVORNAME$
15530 Q$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 12330 'Ja/Nein-Frage TODO Back to main code?
15540 IF NOT ISJA THEN 15780                              'LW TODO Remove this!
15550 '
15560 RETURN                                         '*** TMP Print Disable ***
15570 I$=SPACE$(2)
15580 '
15590 LPRINT STRICH$
15600 LPRINT
15610 LPRINT I$;"Blatt:";BLATT
15620 LPRINT
15630 LPRINT I$;"Mitglieds-Nr.:";MTGLZAHL;TAB(24);"Name:";NACHVORNAME$
15640 LPRINT
15650 FOR K=1 TO 13
15660   KK=K:GOSUB 15750
15670   LPRINT I$;"Von:";VON$;" Bis:";BIS$;" Amt:";AMT$;
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
15820 ' *** Drucker bereit?-Frage *********************************************
15830 '
15840 ' OUT: ISJA (-1=Ja, 0=Nein)
15850 Q$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 12330 'Ja/Nein-Frage
15860 GOSUB 13060                                            'Nachricht löschen
15870 RETURN
15880 '
15890 ' *** Druck abbrechen?-Frage ********************************************
15900 '
15910 ' OUT: ISJA (-1=Ja, 0=Nein)
15920 ISJA=FALSE 
15930 IF INKEY$="" THEN 15970
15940 Q$="Drucken abbrechen?":GOSUB 12330                        'Ja/Nein-Frage
15950 IF ISJA THEN 15970
15960 N$="Mitgliedsliste wird gedruckt... Abbrechen mit Taste.":GOSUB 12920 'Nachricht
15970 RETURN
15980 '
15990 ' *** Mitgliedsliste drucken (Kopf) *************************************
16000 '
16010 ' IN: DATUM$
16020 LPRINT TAB(16);"MITGLIEDSLISTE 25*******************************"
16030 LPRINT 
16040 LPRINT TAB(32);"Stand:";DATUM$
16050 LPRINT
16060 LPRINT TAB(3);"Mitgl."; 
16070 LPRINT TAB(10);"Name";
16080 LPRINT TAB(36);"Vorname, Anschrift";
16090 LPRINT TAB(63);"Status";
16100 LPRINT TAB(71);"Amt"
16110 RETURN
16120 '
16130 ' *** Mitgliedsliste drucken (Mitglied) *********************************
16140 '
16150 ' IN:  MTGLZAHL
16160 AMT$=""
16170 IN=INSTR(DAT$(6),"22**"):IF IN<>0 THEN AMT$=MID$(DAT$(6),IN,6):GOTO 16210
16180 IN=INSTR(DAT$(6),"23**"):IF IN<>0 THEN AMT$=MID$(DAT$(6),IN,5):GOTO 16210
16190 IN=INSTR(DAT$(6),"24**"):IF IN<>0 THEN AMT$=MID$(DAT$(6),IN,3)
16200 '
16210 LPRINT
16220 LPRINT TAB(5);MTGLZAHL;
16230 LPRINT TAB(10);DAT$(1);                                         'Nachname
16240 LPRINT TAB(36);DAT$(2);                                          'Vorname
16250 LPRINT TAB(65);"(";LEFT$(DAT$(5),1);")";                          'Status
16260 LPRINT TAB(71);AMT$                                                  'Amt
16270 LPRINT TAB(36);DAT$(10)                                           'Straße
16280 LPRINT TAB(36);DAT$(11);                                    'Postleitzahl
16290 LPRINT TAB(41);DAT$(12)                                              'Ort
16300 LPRINT TAB(36);"Tel.:";DAT$(13)                                  'Telefon
16310 LPRINT
16320 RETURN
16330 '
16340 ' *** Adressaufkleber drucken *******************************************
16350 '                                                           Ja/Nein-Frage
16360 I$=SPACE$(3)
16370 '
16380 LPRINT I$;DAT$(7)                                                 'Anrede
16390 LPRINT I$;DAT$(2);" ";DAT$(1)                           'Vorname Nachname
16400 LPRINT
16410 LPRINT I$;DAT$(10)                                     'Straße Hausnummer
16420 LPRINT
16430 LPRINT I$;DAT$(11);" ";DAT$(12)                         'Postleitzahl Ort
16440 LPRINT
16450 LPRINT
16460 LPRINT
16470 LPRINT
16480 LPRINT
16490 LPRINT
16500 RETURN
16510 '
16520 ' *** Stammsatz drucken (Bildschirmformat)
16530 '
16540 XCOL1=8
16560 XCOL2=43
16570 LPRINT STRICH$
16580 LPRINT
16590 LPRINT TAB(31);"Mitgliedsnummer";MTGLZAHL
16600 LPRINT
16610 RESTORE 16670
16620 FOR K=1 TO 7:READ A,B
16630   IF A<>0 THEN LPRINT TAB(XCOL1);FENAM$(A);DAT$(A);
16640   IF B<>0 THEN LPRINT TAB(XCOL2);FENAM$(B);DAT$(B);
16650   LPRINT
16660 NEXT K
16670 DATA 1,0,2,15,3,16,4,0,5,17,6,18,0,19
16680 FOR K=7 TO 14
16690   LPRINT TAB(XCOL1);FENAM$(K);DAT$(K);
16700   LPRINT TAB(XCOL2);FENAM$(K+13);DAT$(K+13);
16710 NEXT K
16720 LPRINT
16730 LPRINT
16740 LPRINT
16750 LPRINT STRICH$
16760 RETURN
16770 '
16780 ' *** Mitgliedsvermerke drucken
16790 '
16800 ' IN: NACHVORNAME$
16810 RETURN                                         '*** TMP Print Disable ***
16820 I$=SPACE$(9)
16830 FORM1$="___"
16840 FORM2$="___ / ___"
16850 FORM3$="___ / ___ / ___"
16860 '
16870 LPRINT TAB(10);"Mitgliedsnr.: ";FORM1$;
16880 LPRINT TAB(28);"Name: ";NACHVORNAME$;
16890 LPRINT TAB(65);"Datum: ";FORM3$
16900 LPRINT
16910 LPRINT I$;"26*************************** ";FORM3$
16920 LRPINT
16930 LPRINT I$;"27*************************** ";FORM3$
16940 LRPINT
16950 LPRINT I$;"28*************************** ";FORM3$
16960 LRPINT
16970 LPRINT I$;"29*************************** ";FORM3$
16980 LRPINT
16990 LPRINT I$;"30*************************** ";FORM2$
17000 LRPINT
17010 LPRINT I$;"31*************************** ";FORM3$
17020 LRPINT
17030 LPRINT I$;"32*************************** ";FORM3$
17040 LRPINT
17050 LPRINT I$;"33*************************** ";FORM1$
17060 LRPINT
17070 LPRINT I$;"34*************************** ";FORM3$
17080 LRPINT
17090 LPRINT I$;"35*************************** ";FORM3$
17100 LRPINT
17110 LPRINT I$;"36*************************** ";FORM3$
17120 LRPINT
17130 LPRINT I$;"37*************************** ";FORM3$
17140 LRPINT
17150 LPRINT I$;"38*************************** ";FORM3$
17160 RETURN
17170 '
17180 ' ### HILFE-TEXT ########################################################
17190 '
17200 '
17210 ' *** Fielden der Hilfe-Text-Dateien ************************************
17220 '
17230 OPEN "R",#6,"MVWHELP.NDX",4
17240   FIELD #6,2 AS XFIRSTLINE$,2 AS XLASTLINE$
17250   FOR K=1 TO HELPIDMAX
17260     GET #6,K
17270     HELPFIRSTLINE(K)=CVI(XFIRSTLINE$)
17280     HELPLASTLINE(K)=CVI(XLASTLINE$)
17290   NEXT K
17300 CLOSE #6
17310 '
17320 OPEN "R",#6,"MVWHELP.DAT",2+2+60
17330 FIELD #6,2 AS XINDENT$,2 AS XLENGTH$,60 AS XLINE$
17340 RETURN
17350 '
17360 ' *** Hilfe-Text-Seite anzeigen *****************************************
17370 '
17380 ' IN: HELPID
17390 SAVEMT$=MT$:MT$="HILFE":GOSUB 13130:MT$=SAVEMT$              'Basis-Maske
17400 PRINT HOME$
17410 FIRSTLINE=HELPFIRSTLINE(HELPID)
17420 LASTLINE=HELPLASTLINE(HELPID)
17430 FOR K=FIRSTLINE TO LASTLINE
17440   GET #6,K
17450   INDENT=CVI(XINDENT$)
17460	LENGTH=CVI(XLENGTH$)
17470   PRINT SPC(INDENT);LEFT$(XLINE$,LENGTH)
17480 NEXT K
17490 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
17500 RETURN
17510 '
17520 ' *** Programmfehler ****************************************************
17530 '
17540 I$=SPACE$(15)
17550 GOSUB 13130                                                  'Basis-Maske
17560 PRINT HOME$
17570 PRINT I$;"Ein Programmfehler ist aufgetreten."
17580 PRINT   
17590 PRINT I$;"- Notieren Sie folgende Fehlerinformation:"
17600 PRINT I$;"  ";HIGHON$;"ERL";HIGHOFF$;" (Error Line)";HIGHON$;ERL;HIGHOFF$
17610 PRINT I$;"  ";HIGHON$;"ERR";HIGHOFF$;" (Error Code)";HIGHON$;ERR;HIGHOFF$
17620 PRINT I$;"- Drücken Sie am Rechner den Knopf hinten rechts."
17630 PRINT I$;"19**************************"
17640 '
17650 ' *** Programmende ******************************************************
17660 '
17670 PRINT FNCUR$(22,1);HIGHOFF$;CURON$;
17680 RESET
17690 'LW RUN "DISKMENU.EXE"
17700 END                                                           '1595 lines
