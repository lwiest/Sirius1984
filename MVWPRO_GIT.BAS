1000 ' RELEASE 01***** Neubearbeitung durch 04**********
1010 '
1020 ' LW 31-JAN-2026: Load MSBASIC.COM with /S:560 /F:6
1030 '
1040 ' *** Initialisierung, Dimensionierung ***********************************
1050 '
1060 DEFINT A-Z
1070 '
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
1200 DIM NUMSUCH$(NUMSUCHMAX)A
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
1641 HELPIDMAX=23                                     'Anzahl Hilfe-Text-Seiten
1642 DIM HELPFIRSTLINE(HELPIDMAX)
1643 DIM HELPLASTLINE(HELPIDMAX)
1644 '
1650 G0100000=1                                           'Mitgliederverwaltung
1660 G0110000=2                                                'Stammsatzpflege
1670 G0110100=3                                              'Stammsatz anlegen
1680 G0110201=4                                           'Stammsatz ändern (I)
1690 G0110202=5                                          'Stammsatz ändern (II)
1700 G0110300=6                                              'Stammsatz löschen
1710 G0110400=7                                              'Stammsatz drucken
1720 G0110501=8                                           'Stammsatz suchen (I)
1730 G0110510=9                                  'Stammsatz suchen nach Nummern
1740 G0110520=10                                   'Stammsatz suchen nach Maske
1750 G0110502=11                           'Stammsatz suchen (II) Keine Treffer
1760 G0110503=12                               'Stammsatz suchen (III) Blättern
1770 G0120001=13                                       'Verwendungsnachweis (I)
1780 G0120002=14                                      'Verwendungsnachweis (II)
1790 G0130000=15                                        'Mitgliedsliste drucken
1800 G0130101=16                                          'Suchen & Drucken (I)
1810 G0130110=17                                 'Suchen & Drucken nach Nummern
1820 G0130120=18                                   'Suchen & Drucken nach Maske
1830 G0130102=19                           'Suchen & Drucken (II) Keine Treffer
1840 G0130103=20                               'Suchen & Drucken (III) Blättern
1850 G0130104=21                                   'Suchen & Drucken (IB) Datum
1860 G0130200=22                                           'Komplettdruck Datum
1870 G0130300=23                                            'Datenblatt drucken
1880 '
1890 ' Weitere Variablen
1900 ' -----------------
1910 ' F        - Feldindex 1..27 (FELDMAX)
1920 ' I$       - Lokale Texteinrückung
1921 ' K        - Schleifenindex
1930 ' MTGLMAX  - Höchste auf Disk gespeicherte Mitgliedsnummer
1940 ' MTGLZAHL - Momentane Mitgliedsnummer
1950 '
1960 XOFF=14             'Offset Colum Feldname to Column Wertefeld (Pünktchen)
1970 '
1980 ' TIP: Do not let the cursor slip into line 25 -> display will scroll up
1990 ' TIP: Use semicolons with PRINT carefully -> display driver bug
2000 '
2010 ON ERROR GOTO 17480                               'Programmfehler abfangen
2020 '
2030 GOSUB 10050    'Tasten deaktivieren, ASCII-Codes für Cursortasten vergeben
2040 PRINT CUROFF$;
2050 '
2060 ' *** Beginn des Mitgliederverwaltungsprogramms **************************
2070 '
2080 MT$="MITGLIEDERVERWALTUNG":MI$="G-010000":GOSUB 13130         'Basis-Maske
2090 Q$="Bitte Daten-Diskette in Laufwerk B einlegen. Bereit?":GOSUB 12340 'Ja/Nein-Frage
2100 IF NOT ISJA THEN 17610                                       'Programmende
2110 '
2120 N$="Einen Moment bitte...":GOSUB 13000     'Nachricht. F-Tasten ausblenden
2130 GOSUB 11230                                                 'MTGLMAX lesen
2140 GOSUB 10960                              'Stammsatzfeld-Definitionen lesen
2150 GOSUB 11830                                   'Fielden der Stammsatz-Datei
2160 GOSUB 13800                       'Fielden der Verwendungsnachweis-Dateien
2170 GOSUB 17230      'Fielden der Hilfe-Text-Dateien, lesen der Zeilenbereiche
2180 GOSUB 13060                                             'Nachricht löschen
2190 '
2200 ' ###################### Mitgliederverwaltungs-Menü ######################
2210 '
2220 MT$="MITGLIEDERVERWALTUNG":MI$="G-010000":GOSUB 13130         'Basis-Maske
2230 FKEYS$="":GOSUB 13240                                 'F-Tasten einblenden
2250 PRINT FNCUR$(4,28);"1 - Stammsatzpflege"
2260 PRINT FNCUR$(6,28);"2 - Verwendungsnachweis"
2270 PRINT FNCUR$(8,28);"3 - Mitgliedsliste drucken"
2280 FKEYMAX=4:NKEYMAX=3:GOSUB 13400                              'Menü-Eingabe
2290 IF NKEY THEN ON NKEY GOTO 2380,6830,7660
2300 ON FKEY GOTO 2310,2320,2220,2320
2310 HELPID=G0100000:GOSUB 17320:GOTO 2220                          'Hilfe-Text
2320 GOSUB 13560:GOTO 2280                                'Zum Hauptmenü?-Frage
2330 '
2340 ' ########################### Stammsatzpflege ############################
2350 '
2360 ' *** Stammsatzpflege-Menü ***********************************************
2370 '
2380 MT$="STAMMSATZPFLEGE":MI$="G-011000":GOSUB 13130              'Basis-Maske
2390 FKEYS$="":GOSUB 13240                                 'F-Tasten einblenden
2400 PRINT FNCUR$(4,30);"1 - Stammsatz anlegen"
2410 PRINT FNCUR$(6,30);"2 - Stammsatz ändern"
2420 PRINT FNCUR$(8,30);"3 - Stammsatz löschen"
2430 PRINT FNCUR$(10,30);"4 - Stammsatz drucken"
2440 PRINT FNCUR$(12,30);"5 - Stammsatz suchen"
2450 FKEYMAX=4:NKEYMAX=5:GOSUB 13400                              'Menü-Eingabe
2460 IF NKEY THEN ON NKEY GOTO 2530,3010,3670,4100,4470
2470 ON FKEY GOTO 2480,2220,2220,2490
2480 HELPID=G0110000:GOSUB 17320:GOTO 2380                          'Hilfe-Text
2490 GOSUB 13560:GOTO 2450                                'Zum Hauptmenü?-Frage
2500 '
2510 ' *** Stammsatz anlegen von 02************ *******************************
2520 '
2530 MTGLZAHL=MTGLMAX+1
2540 F=1
2550 GOSUB 11120                                                 'DAT$() leeren
2560 GOSUB 12790                                     'Voreinstellungen zuweisen
2570 MT$="STAMMSATZ ANLEGEN":MI$="G-011010":GOSUB 13130            'Basis-Maske
2580 FKEYS$="<F7>=Speichern":GOSUB 13240                   'F-Tasten einblenden
2590 GOSUB 11780                                      'Mitgliedsnummer ausgeben
2600 GOSUB 11650             'Feldnamen, Feldwerte und Feldplatzhalter ausgeben
2610 Z=FEROW(F):S=FECOL(F)+XOFF:LNG=FELNG(F):TYP=FETYP(F):ISMUSS=FEMUSS(F)
2620 FKEYMASK$="12347":CKEYMASK$="12":INTEXT$=DAT$(F)
2630 GOSUB 10370                                                  'Feld-Eingabe
2631 '
2640 IF ASCC<>ASCRET THEN 2680                              'RETURN verarbeiten
2650   DAT$(F)=OUTTEXT$
2660   F=F+1:IF F>FELDMAX THEN F=1
2670   GOTO 2610
2680 IF CKEY=0 THEN 2730                              'Cursortasten verarbeiten
2690   PRINT FNCUR$(Z,S);HIGHON$;DAT$(F);HIGHOFF$;STRING$(LNG-LEN(DAT$(F)),".")
2700   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FELDMAX
2710   IF CKEY=CKEYDN THEN F=F+1:IF F>FELDMAX THEN F=1
2720   GOTO 2610
2730 IF FKEY=1 THEN HELPID=G0110100:GOSUB 17320:GOTO 2570           'Hilfe-Text
2740 IF FKEY=2 THEN GOSUB 2840:GOTO 2380              'Exit "Stammsatz anlegen"
2750 IF FKEY=3 THEN GOSUB 2840:GOTO 2220              'Exit "Stammsatz anlegen"
2760 IF FKEY<>4 THEN 2810
2770   GOSUB 13640                              'Zum Hauptmenü?-Frage (einfach)
2780   IF NOT ISJA THEN GOSUB 13240:GOTO 2630              'F-Tasten einblenden
2790   GOSUB 2840                                     'Exit "Stammsatz anlegen"
2800   GOTO 17610                                                 'Programmende
2810 IF FKEY=7 THEN GOSUB 2920:GOTO 2530            'Exit "Stammsatz anlegen" 2
2820 '
2830 ' *** Exit "Stammsatz anlegen"
2840 GOSUB 10910                                           'Tastatur entprellen
2850 Q$="Wollen Sie vorher den Stammsatz speichern?":GOSUB 12340 'Ja/Nein-Frage
2860 IF ISJA THEN 2920
2880   MTGLZAHL=MTGLZAHL-1
2890   GOTO 2960
2900 '                                                            
2910 ' *** Exit "Stammsatz anlegen" 2
2920 N$="Stammsatz wird gespeichert...":GOSUB 13000 'Nachricht. F-Tasten ausblenden
2930 GOSUB 12140                                           'Stammsatz speichern
2940 GOSUB 13910                       'Erste Verwendungsnachweis-Seite anlegen
2950 MTGLMAX=MTGLZAHL:GOSUB 11320                             'MTGLMAX speichen
2960 GOSUB 13060                                             'Nachricht löschen
2970 RETURN
2980 ' 
2990 ' *** Stammsatz ändern von 03************ ********************************
3000 '
3010 MT$="STAMMSATZ ÄNDERN":MI$="G-011020":GOSUB 13130             'Basis-Maske
3020 FKEYS$="":GOSUB 13240                                 'F-Tasten einblenden
3030 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer" 
3040 PRINT FNCUR$(5,24)"des zu ändernden Stammsatzes ein."
3050 PRINT FNCUR$(6,22);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
3060 PRINT FNCUR$(7,23)"Beenden Sie die Eingabe mit RETURN."
3070 PRINT FNCUR$(9,38)"=>..."
3080 ISBADINPUT=0
3090 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
3100 Z=9:S=40:LNG=3:TYP=2:ISMUSS=-1
3110 GOSUB 10380                                                'Feld-Eingabe+1
3120 IF FKEY=1 THEN HELPID=G0110201:GOSUB 17320:GOTO 3010 'Hilfe-Text 'TODO: Wert geht verloren
3130 IF FKEY=2 THEN 2380
3140 IF FKEY=3 THEN 2220
3150 IF FKEY=4 THEN GOSUB 13560:GOTO 3110                 'Zum Hauptmenü?-Frage
3160 '
3170 M=VAL(OUTTEXT$)                                        'RETURN verarbeiten
3180 IF M>=1 AND M<=MTGLMAX THEN 3230
3190   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 12920         'Nachricht
3200   ISBADINPUT=-1
3220   GOTO 3100
3230 MTGLZAHL=M
3240 N$="Stammsatz wird gelesen...":GOSUB 13000 'Nachricht. F-Tasten ausblenden
3250 GOSUB 11890                                               'Stammsatz lesen
3260 GOSUB 13060                                             'Nachricht löschen
3270 '
3280 GOSUB 13130                                                   'Basis-Maske
3290 FKEYS$="<F7>=Speichern":GOSUB 13240                   'F-Tasten einblenden
3300 GOSUB 11780                                      'Mitgliedsnummer ausgeben
3310 GOSUB 11650             'Feldnamen, Feldwerte und Feldplatzhalter ausgeben
3320 F=1
3330 Z=FEROW(F):S=FECOL(F)+XOFF:LNG=FELNG(F):TYP=FETYP(F):ISMUSS=FEMUSS(F)
3340 FKEYMASK$="12347":CKEYMASK$="12":INTEXT$=DAT$(F)
3350 GOSUB 10370                                                  'Feld-Eingabe
3360 IF ASCC<>ASCRET THEN 3400                              'RETURN verarbeiten
3370   DAT$(F)=OUTTEXT$
3380   F=F+1:IF F>FELDMAX THEN F=1
3390   GOTO 3330
3400 IF CKEY=0 THEN 3450                              'Cursortasten verarbeiten
3410   PRINT FNCUR$(Z,S);HIGHON$;DAT$(F);HIGHOFF$;STRING$(LNG-LEN(DAT$(F)),".")
3420   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FELDMAX
3430   IF CKEY=CKEYDN THEN F=F+1:IF F>FELDMAX THEN F=1
3440   GOTO 3330
3450 IF FKEY=1 THEN HELPID=G0110202:GOSUB 17320:GOTO 3280           'Hilfe-Text
3460 IF FKEY=2 THEN GOSUB 3560:GOTO 3010               'Exit "Stammsatz ändern"
3470 IF FKEY=3 THEN GOSUB 3560:GOTO 2220               'Exit "Stammsatz ändern"
3480 IF FKEY<>4 THEN 3530 
3490   GOSUB 13640                              'Zum Hauptmenü?-Frage (einfach)
3500   IF NOT ISJA THEN GOSUB 13240:GOTO 3350              'F-Tasten einblenden
3510   GOSUB 3560                                      'Exit "Stammsatz ändern"
3520   GOTO 17610                                                 'Programmende
3530 IF FKEY=7 THEN GOSUB 3600:GOTO 3010             'Exit "Stammsatz ändern" 2
3540 '
3550 ' *** Exit "Stammsatz ändern"
3560 Q$="Wollen Sie vorher den Stammsatz speichern?":GOSUB 12340 'Ja/Nein-Frage
3570 IF NOT ISJA THEN RETURN
3580 '
3590 ' *** Exit "Stammsatz ändern" 2
3600 N$="Stammsatz wird gespeichert...":GOSUB 13000 'Nachricht. F-Tasten ausblenden
3610 GOSUB 12140                                           'Stammsatz speichern
3620 GOSUB 13060                                             'Nachricht löschen
3630 RETURN
3640 '
3650 ' *** Stammsatz löschen von 04********** *********************************
3660 '
3670 MT$="STAMMSATZ LÖSCHEN":MI$="G-011030":GOSUB 13130            'Basis-Maske
3680 FKEYS$="":GOSUB 13240                                 'F-Tasten einblenden
3690 PRINT FNCUR$(4,23);"Bitte geben Sie die Mitgliedsnummer"         'Menütext
3700 PRINT FNCUR$(5,24);"des zu löschenden Mitglieds ein."
3710 PRINT FNCUR$(6,22);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
3720 PRINT FNCUR$(7,15);"(Dabei wird nur der Status des Mitglieds verändert.)"
3730 PRINT FNCUR$(8,23);"Beenden Sie die Eingabe mit RETURN."
3740 PRINT FNCUR$(10,38);"=>..."
3750 ISBADINPUT=0
3760 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
3770 Z=10:S=40:LNG=3:TYP=2:ISMUSS=-1
3780 GOSUB 10380                                                'Feld-Eingabe+1
3790 IF FKEY=1 THEN HELPID=G0110300:GOSUB 17320:GOTO 3670 'Hilfe-Text 'TODO: Wert geht verloren
3800 IF FKEY=2 THEN 2380
3810 IF FKEY=3 THEN 2220
3820 IF FKEY=4 THEN GOSUB 13560:GOTO 3780                 'Zum Hauptmenü?-Frage
3830 '
3840 M=VAL(OUTTEXT$)                                        'RETURN verarbeiten
3850 IF M>=1 AND M<=MTGLMAX THEN 3900
3860   N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12920       'Nachricht
3870   ISBADINPUT=1
3890   GOTO 3770
3900 MTGLZAHL=M
3910 N$="Stammsatz wird gelesen...":GOSUB 13000 'Nachricht. F-Tasten ausblenden
3920 GOSUB 11890                                               'Stammsatz lesen
3930 GOSUB 13060                                             'Nachricht löschen
3940 '
3950 GOSUB 13130                                                   'Basis-Maske
3960 GOSUB 11780                                      'Mitgliedsnummer ausgeben
3970 GOSUB 11650             'Feldnamen, Feldwerte und Feldplatzhalter ausgeben
3980 Q$="Mitglied endgültig löschen?":GOSUB 12340                'Ja/Nein-Frage
3990 IF NOT ISJA THEN 4060
4000   F=5
4001   DAT$(F)="ausgetreten"
4002   Z=FEROW(F):S=FECOL(F)+XOFF:LNG=FELNG(F)
4003   PRINT FNCUR$(Z,S);HIGHON$;DAT$(F);HIGHOFF$;STRING$(LNG-LEN(DAT$(F)),".")               
4030   N$="Stammsatz wird gespeichert...":GOSUB 13000 'Nachricht. F-Tasten ausblenden
4040   GOSUB 12140                                         'Stammsatz speichern
4050   GOSUB 13060                                           'Nachricht löschen
4060 GOTO 3670
4070 '
4080 ' *** Stammsatz drucken von 02************ & 04********** ****************
4090 '
4100 MT$="STAMMSATZ DRUCKEN":MI$="G-011040":GOSUB 13130            'Basis-Maske
4110 FKEYS$="":GOSUB 13240                                 'F-Tasten einblenden
4120 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer"
4130 PRINT FNCUR$(5,23)"des auszudruckenden Stammsatzes ein."
4140 PRINT FNCUR$(6,22);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
4150 PRINT FNCUR$(7,23)"Beenden Sie die Eingabe mit RETURN."
4160 PRINT FNCUR$(9,38);"=>..."
4170 ISBADINPUT=0
4180 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
4190 Z=9:S=40:LNG=3:TYP=2:ISMUSS=-1
4200 GOSUB 10380                                                'Feld-Eingabe+1
4210 IF FKEY=1 THEN HELPID=G0110400:GOSUB 17320:GOTO 4100 'Hilfe-Text 'TODO: Wert geht verloren
4220 IF FKEY=2 THEN 2380
4230 IF FKEY=3 THEN 2220
4240 IF FKEY=4 THEN GOSUB 13560:GOTO 4200                 'Zum Hauptmenü?-Frage
4250 '
4260 M=VAL(OUTTEXT$)                                        'RETURN verarbeiten
4270   IF M>=1 AND M<=MTGLMAX THEN 4320
4280   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 12920         'Nachricht
4290   ISBADINPUT=1
4310   GOTO 4190
4320 MTGLZAHL=M
4330 N$="Stammsatz wird gelesen...":GOSUB 13000 'Nachricht. F-Tasten ausblenden
4340 GOSUB 11890                                               'Stammsatz lesen
4350 GOSUB 13060                                             'Nachricht löschen
4360 '
4370 GOSUB 13130                                                   'Basis-Maske
4380 GOSUB 11780                                      'Mitgliedsnummer ausgeben
4390 GOSUB 11650             'Feldnamen, Feldwerte und Feldplatzhalter ausgeben
4400 GOSUB 15850                                         'Drucker bereit?-Frage
4410 IF NOT ISJA THEN 4430
4420   GOSUB 16540                        'Stammsatz drucken (Bildschirmformat)
4430 GOTO 4100
4440 '
4450 ' *** Stammsatz suchen (und drucken) von 05*************** ***************
4460 '
4470 ISDRUCK=0
4480 MT$="STAMMSATZ SUCHEN":IF ISDRUCK THEN MT$="SUCHEN & DRUCKEN"
4490 MI$="G-011050":IF ISDRUCK THEN MI$="G-013010"
4500 GOSUB 13130                                                   'Basis-Maske
4510 FKEYS$="":GOSUB 13240                                 'F-Tasten einblenden
4520 TMP$="Suchen":IF ISDRUCK THEN TMP$="Drucken" 
4530 PRINT FNCUR$(4,29)"1 - ";TMP$;" nach Nummern"
4540 PRINT FNCUR$(6,29)"2 - ";TMP$;" nach Maske"
4550 FKEYMAX=4:NKEYMAX=2:GOSUB 13400                              'Menü-Eingabe
4560 IF NKEY THEN ON NKEY GOTO 4660,5270
4570 IF FKEY<>1 THEN 4610 
4580   HELPID=G0110501:IF ISDRUCK THEN HELPID=G0130101
4590   GOSUB 17320                                                  'Hilfe-Text
4600   GOTO 4480
4610 IF FKEY=2 THEN IF ISDRUCK THEN 7660 ELSE 2380
4620 IF FKEY=3 THEN 2220
4630 IF FKEY=4 THEN GOSUB 13560:GOTO 4550                 'Zum Hauptmenü?-Frage
4640 '
4650 ' *** Suchen nach Nummern
4660 TMP$=STR$(1):NUMSUCH$(1)=MID$(TMP$,2,99)
4670 TMP$=STR$(MTGLMAX):NUMSUCH$(2)=MID$(TMP$,2,99)
4680 IF NOT ISDRUCK THEN MT$="STAMMSATZ SUCHEN NACH NUMMERN":MI$="G-011051"
4690 IF ISDRUCK THEN MT$="SUCHEN & DRUCKEN NACH NUMMERN":MI$="G-013011"
4700 GOSUB 13130                                                   'Basis-Maske
4710 FKEYS$="<F5>=Suchen":IF ISDRUCK THEN FKEYS$="<F5>=Suchen & Drucken"
4720 GOSUB 13240                                           'F-Tasten einblenden
4730 TMP$="gesucht":IF ISDRUCK THEN TMP$="gesucht und gedruckt"
4740 TMP1=12:IF ISDRUCK THEN TMP1=6
4750 TMP2=66:IF ISDRUCK THEN TMP2=73
4770 PRINT FNCUR$(4,TMP1);"Ab welcher Mitgliedsnummer soll ";TMP$;" werden?     =>..."
4780 PRINT FNCUR$(6,TMP1);"Bis zu welcher Mitgliedsnummer soll ";TMP$;" werden? =>..."
4790 PRINT FNCUR$(4,TMP2);HIGHON$;NUMSUCH$(1);HIGHOFF$
4800 PRINT FNCUR$(6,TMP2);HIGHON$;NUMSUCH$(2);HIGHOFF$
4801 PRINT FNCUR$(8,22);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
4810 F=1
4820 ISBADINPUT=0
4830 FKEYMASK$="12345":CKEYMASK$="":INTEXT$=NUMSUCH$(F)
4840 Z=NUMSUCHROW(F):S=67:LNG=3:TYP=2:ISMUSS=-1
4841 S=66:IF ISDRUCK THEN S=73
4850 GOSUB 10380                                                'Feld-Eingabe+1
4860 IF FKEY<>1 THEN 4900                                 'F-Tasten vrearbeiten
4870   HELPID=G0110510:IF ISDRUCK THEN HELPID=G0130110
4880   GOSUB 17320                                                  'Hilfe-Text
4890   GOTO 4680                                    'TODO: Werte gehen verloren
4900 IF FKEY=2 THEN 4480
4910 IF FKEY=3 THEN 2220
4920 IF FKEY=4 THEN GOSUB 13560:GOTO 4850                 'Zum Hauptmenü?-Frage
4930 IF FKEY<>5 THEN 4990
4940   SEARCH=BYNUMS
4950   NUMSUCH1=VAL(NUMSUCH$(1))
4960   NUMSUCH2=VAL(NUMSUCH$(2))
4970   IF ISDRUCK THEN 8410 ELSE 5580             'Suchen & Drucken oder Suchen
4980 '
4990 M=VAL(OUTTEXT$)                                        'RETURN verarbeiten
5000 IF M>=1 AND M<=MTGLMAX THEN 5040
5010   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 12920         'Nachricht
5020   GOSUB 5190                                     'Eingabefehler-Behandlung
5030   GOTO 4840
5040 NUMSUCH$(F)=OUTTEXT$
5050 NUMSUCH1=VAL(NUMSUCH$(1))
5060 NUMSUCH2=VAL(NUMSUCH$(2))
5070 IF NUMSUCH1<=NUMSUCH2 THEN 5110
5080   N$="Ungültiger Bereich":GOSUB 12920                           'Nachricht
5090   GOSUB 5190                                     'Eingabefehler-Behandlung
5100   GOTO 4840
5110 F=F+1:IF F>NUMSUCHMAX THEN F=1
5120 GOSUB 13500                                           'F-Tasten ausblenden
5140 FKEYS$="<F5>=Suchen":IF ISDRUCK THEN FKEYS$="<F5>=Suchen & Drucken" 
5150 GOSUB 13240                                           'F-Tasten einblenden
5160 GOTO 4830
5170 '
5180 ' *** Eingabefehler-Behandlung
5190 ISBADINPUT=1
5210 FKEYMASK$="1234"
5220 GOSUB 13500                                           'F-Tasten ausblenden
5230 FKEYS$="":GOSUB 13240                                 'F-Tasten einblenden
5240 RETURN
5250 '
5260 ' *** Suchen nach Maske
5270 GOSUB 11170                                                'SUCH$() leeren
5280 IF NOT ISDRUCK THEN MT$="STAMMSATZ SUCHEN NACH MASKE":MI$="G-011052"
5290 IF ISDRUCK THEN MT$="SUCHEN & DRUCKEN NACH MASKE":MI$="G-013012"
5300 GOSUB 13130                                                   'Basis-Maske
5310 FKEYS$="<F5>=Suchen":IF ISDRUCK THEN FKEYS$="<F5>=Suchen & Drucken" 
5320 GOSUB 13240                                           'F-Tasten einblenden
5330 GOSUB 11410                        'Feldnamen und Feldplatzhalter ausgeben
5340 F=1
5350 Z=FEROW(F):S=FECOL(F)+XOFF:LNG=FELNG(F):TYP=3
5360 FKEYMASK$="12345":CKEYMASK$="12":INTEXT$=""
5370 GOSUB 10370                                                  'Feld-Eingabe
5380 IF FKEY<>1 THEN 5420                                 'F-Tasten verarbeiten
5390   HELPID=G0110520:IF ISDRUCK THEN HELPID=G0130120
5400   GOSUB 17320                                                  'Hilfe-Text
5410   GOTO 5280
5420 IF FKEY=2 THEN 4480
5430 IF FKEY=3 THEN 2220
5440 IF FKEY=4 THEN GOSUB 13560:GOTO 5370                 'Zum Hauptmenü?-Frage
5450 IF FKEY<>5 THEN 5480 
5460   SEARCH=BYMASK                                                      '<F5>
5470   IF ISDRUCK THEN 8410 ELSE 5580             'Suchen oder Suchen & Drucken
5480 IF CKEY=0 THEN 5530                              'Cursortasten verarbeiten
5490   PRINT FNCUR$(Z,S);HIGHON$;SUCH$(F);HIGHOFF$;
5491   PRINT STRING$(LNG-LEN(SUCH$(F)),".")
5500   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FELDMAX
5510   IF CKEY=CKEYDN THEN F=F+1:IF F>FELDMAX THEN F=1
5520   GOTO 5350
5530 SUCH$(F)=OUTTEXT$                                      'RETURN verarbeiten
5540 F=F+1:IF F>FELDMAX THEN F=1
5550 GOTO 5350
5560 '
5570 ' *** Suche
5580 GOSUB 13130                                                   'Basis-Maske
5590 FKEYS$="<F5>=Suchen rückwärts <F6>=Suchen vorwärts <F7>=Adressaufkleber" 
5600 GOSUB 13240                                           'F-Tasten einblenden
5610 PRINT FNCUR$(3,31);"Mitgliedsnummer"
5620 GOSUB 11410                        'Feldnamen und Feldplatzhalter ausgeben
5630 '
5640 IF SEARCH=BYMASK THEN 5690
5650 '
5660 MTGLZAHL=NUMSUCH1                      'Initialisierung Suche nach Nummern
5670 GOTO 5830
5680 '
5690 MTGLZAHL=0                               'Initialisierung Suche nach Maske
5700 GOSUB 6540                        'Nächsten Treffer suchen IN/OUT:MTGLZAHL
5710 IF MTGLZAHL<>-1 THEN 5830
5720   N$="Kein Suchtreffer. Mit <F2> neue Suche.":GOSUB 12920       'Nachricht
5730   FKEYS$="":GOSUB 13240                               'F-Tasten einblenden
5740   FKEYMASK$="1234":NKEYMASK="":GOSUB 13400                   'Menü-Eingabe
5750   IF FKEY<>1 THEN 5790                               'F-Tasten verarbeiten
5760     HELPID=G0110502:IF ISDRUCK THEN HELPID=G0130102 
5770     GOSUB 17320                                                'Hilfe-Text
5780     GOTO 5280
5790   IF FKEY=2 THEN 5280
5800   IF FKEY=3 THEN 2220
5810   IF FKEY=4 THEN GOSUB 13560:GOTO 5740               'Zum Hauptmenü?-Frage
5830 GOSUB 6290                                 'Suchtreffer lesen und anzeigen
5840 '
5850 FKEMAX=7:NKEYMAX=0:GOSUB 13400                               'Menü-Eingabe
5860 ON FKEY GOTO 5890,5940,2220,5970,6000,6140,6360
5870 '
5880 ' *** <F1> Hilfe
5890 HELPID=G0110503:IF ISDRUCK THEN HELPID=G0130103 
5900 GOSUB 17320                                                    'Hilfe-Text
5910 GOTO 5580 'TODO <F6><F6><F1> -> wieder beim 3 Treffer? 'TODO: Werte gehen verloren
5920 '
5930 ' *** <F2> Zurück
5940 IF SEARCH=BYNUMS THEN 4660 ELSE 5280           'TODO: Werte gehen verloren
5950 '
5960 ' *** <F4> Hauptmenü
5970 GOSUB 13560:GOTO 5850                                'Zum Hauptmenü?-Frage
5980 '
5990 ' *** <F5> Suchen rückwärts
6000 IF SEARCH=BYMASK THEN 6040
6010   MTGLZAHL=MTGLZAHL-1                        'Suchen rückwärts nach Nummer
6020   IF MTGLZAHL<NUMSUCH1 THEN 6100
6030     GOSUB 6290:GOTO 5850                   'Suchtreffer lesen und anzeigen
6040 TMP=MTGLZAHL                                  'Suchen rückwärts nach Maske
6050 GOSUB 6460                      'Vorherigen Treffer suchen IN/OUT:MTGLZAHL
6060 IF MTGLZAHL=-1 THEN 6090
6070   GOSUB 6290:GOTO 5850                     'Suchtreffer lesen und anzeigen
6090 MTGLZAHL=TMP                                               'No match found
6100 N$="Erster Suchtreffer":GOSUB 12920                             'Nachricht
6110 GOTO 5850
6120 '
6130 ' *** <F6> Suchen vorwärts
6140 IF SEARCH=BYMASK THEN 6190
6150   MTGLZAHL=MTGLZAHL+1                         'Suchen vorwärts nach Nummer
6160   IF MTGLZAHL>NUMSUCH2 THEN 6250
6170     GOSUB 6290:GOTO 5850                   'Suchtreffer lesen und anzeigen
6180 '
6190 TMP=MTGLZAHL                                    'Suche vorwärts nach Maske
6200 GOSUB 6540                        'Nächsten Treffer suchen IN/OUT:MTGLZAHL
6210 IF MTGLZAHL=-1 THEN 6240
6220   GOSUB 6290:GOTO 5850                     'Suchtreffer lesen und anzeigen
6240 MTGLZAHL=TMP                                               'No match found
6250 N$="Letzter Suchtreffer":GOSUB 12920                            'Nachricht
6260 GOTO 5850
6270 '
6280 ' *** Suchtreffer lesen und anzeigen
6290 GOSUB 13060                                             'Nachricht löschen
6300 GOSUB 11890                                               'Stammsatz lesen
6310 GOSUB 11530                                            'Feldwerte ausgeben
6320 GOSUB 11780                                      'Mitgliedsnummer ausgeben
6330 RETURN
6340 '
6350 ' *** <F7> Adressaufkleber drucken
6360 GOSUB 15850                                         'Drucker bereit?-Frage
6370 IF NOT ISJA THEN 6420                          'TODO: F-Tasten einblenden?
6380   N$="Adressaufkleber wird gedruckt...":GOSUB 12920             'Nachricht
6390   GOSUB 16360                                     'Adressaufkleber drucken
6400   GOSUB 13060                                           'Nachricht löschen
6410   GOSUB 13240                                         'F-Tasten einblenden
6420 GOTO 5850                                                    'Neue Eingabe
6430 '
6440 ' *** Vorherigen Treffer suchen
6450 ' IN: MTGLZAHL OUT: MTGLZAHL oder -1 wenn kein Treffer
6460 MTGLZAHL=MTGLZAHL-1
6470 IF MTGLZAHL<1 THEN 6600
6480 GOSUB 6640
6490 IF NOT ISMATCH THEN 6460 
6500 RETURN
6510 '
6520 ' *** Nächsten Treffer suchen
6530 ' IN: MTGLZAHL OUT: MTGLZAHL oder -1 wenn kein Treffer
6540 MTGLZAHL=MTGLZAHL+1
6550 IF MTGLZAHL>MTGLMAX THEN 6600
6560 GOSUB 6640
6570 IF NOT ISMATCH THEN 6540 
6580 RETURN
6590 '
6600 MTGLZAHL=-1
6610 RETURN
6620 '
6630 ' *** Treffer?
6640 GOSUB 11890                                               'Stammsatz lesen
6650 FOR K=1 TO FELDMAX
6660   IF LEFT$(SUCH$(K),1)<>"*" THEN 6690
6670     IF INSTR(DAT$(K),RIGHT$(SUCH$(K),LEN(SUCH$(K))-1))>0 THEN 6740
6680     GOTO 6780
6690   I=1
6700   IF MID$(SUCH$(K),I,1)="?" THEN I=I+1:GOTO 6700
6710   I=I-1
6720   IF INSTR(DAT$(K),RIGHT$(SUCH$(K),LEN(SUCH$(K))-I))=I+1 THEN 6740
6730   GOTO 6780
6740 NEXT K
6750 ISMATCH=1
6760 RETURN
6770 '
6780 ISMATCH=0
6790 RETURN
6800 '
6810 ' ######################### Verwendungsnachweis ##########################
6820 '
6830 GOSUB 11230                                                 'MTGLMAX lesen
6840 VNFLAG=-1
6850 '
6860 ' Verwendungsnachweis-Menü ***********************************************
6870 '
6880 MT$="VERWENDUNGSNACHWEIS":MI$="G-012000":GOSUB 13130          'Basis-Maske
6890 F=4                                                                'TODO ?
6900 FKEYS$="":GOSUB 13240                                 'F-Tasten einblenden
6910 PRINT CURON$;
6920 PRINT HIGHOFF$;
6930 PRINT FNCUR$(4,20)"   Bitte geben Sie die Mitgliedsnummer"
6940 PRINT FNCUR$(5,20)"des zu ändernden Verwendungsnachweises ein"
6950 'LW PRINT FNCUR$(6,23);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
6960 PRINT FNCUR$(6,20)" und beenden Sie die Eingabe mit RETURN."
6970 PRINT FNCUR$(8,20)"                 =>..."
6980 Z=8:S=39:LNG=3:TYP=2
6990 GOSUB 10370                                                  'Feld-Eingabe
7000 IF FKEY=1 THEN HELPID=G0120001:GOSUB 17320:GOTO 6880           'Hilfe-Text
7010 IF FKEY=2 OR FKEY=3 THEN GOSUB 11120:VNFLAG=0:GOTO 2220     'DAT$() leeren
7020 IF FKEY=4 THEN GOSUB 13560                           'Zum Hauptmenü?-Frage
7030 IF FKEY>4 THEN 6980
7040 IF CKEY<>0 THEN 6980
7050 IF LEN(OUTTEXT$)=0 THEN N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 12920:GOTO 6970 ELSE MITGLIED=VAL(OUTTEXT$):IF MITGLIED=0 THEN 6990 'Nachricht
7060 IF MITGLIED>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12920:PRINT HOME$;HIGHOFF$:GOTO 6970 'Nachricht
7070 MTGLZAHL=MITGLIED:MTGL$=STR$(MITGLIED)
7080 '
7090 ' *** Lesen des Mitgliedsnamens ******************************************
7100 '
7110 PRINT FNCUR$(21,1);DELLINE$;
7120 GET #2,MITGLIED                                           'DATEN STS lesen
7130 NNAM$=LEFT$(DD1$,25)                                 'NACHNAME RAUSFISCHEN
7140 VNAM$=MID$(DD1$,26,25)                               'VORNAME  RAUSFISCHEN
7150 '
7160 ' ** LEERZEICHEN HINTER NACHNAME ABSCHNEIDEN *****************************
7170 '
7180 FOR I=LEN(NNAM$) TO 1 STEP -1
7190   IF MID$(NNAM$,I,1)<>" " THEN NNAM$=LEFT$(NNAM$,I):I=1
7200 NEXT
7210 NACHVORNAME$=NNAM$+" "+VNAM$
7220 GOSUB 14080                                             '1. VN-BLATT lesen
7230 BLATT=1
7240 VP=1:HP=1                                                 'INITIALISIERUNG
7250 F=8
7260 GOSUB 13130                                                   'Basis-Maske
7270 FKEYS$="<F7>=Speichern":GOSUB 13240                   'F-Tasten einblenden
7280 PRINT HIGHOFF$;
7290 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=Vorwärts";TAB(53);"<F9>=Blatt drucken";
7300 PRINT FNCUR$(3,2);"Blatt:"
7310 PRINT FNCUR$(5,2);"Mitglieds-Nr.:"
7320 PRINT FNCUR$(5,24);"Name:"
7330 MSK$="Von:  .  .   Bis:  .  .   Amt:"
7340 FOR KK=7 TO 19 
7350   PRINT FNCUR$(KK,2);MSK$;TAB(43);MSK$                    'LW NO semicolon
7360 NEXT
7370 GOSUB 15290                                                 'Ausgabe Daten
7380 LNG=2-(HP=7)*6                                    'BEGIN OF THE INPUT-LOOP
7390 TYP=2-(HP=7)
7400 Z=VP+13*(VP>13)+6
7410 S=6-39*(VP>13)-(HP<7)*(HP-1)*3+(HP>3)*(HP<7)*4-(HP=7)*26
7420 GOSUB 13060                                             'Nachricht löschen
7430 GOSUB 10370                                                  'Feld-Eingabe
7440 IF HP<7 THEN IF ASCC=ASCRET THEN OUTTEXT$=STRING$(2-LEN(OUTTEXT$),"0")+OUTTEXT$   '+NULLEN
7450 IF CKEY<>0 OR ASCC=ASCRET THEN PRINT FNCUR$(Z,S);OUTTEXT$
7460 IF ASCC=ASCRET THEN IF HP<7 THEN MID$(DAT$(VP),-1+2*HP,2)=OUTTEXT$ ELSE DAT$(VP)=LEFT$(DAT$(VP),12)+OUTTEXT$+SPACE$(8-LEN(OUTTEXT$))
7470 IF ASCC=ASCRET AND HP<7 THEN ISJA=-1:TEST$=OUTTEXT$:ON HP GOSUB 12680,12710,12740,12680,12710,12740:IF NOT ISJA THEN N$="Ungültiges Datum":GOSUB 12920:GOTO 7430 'Datums-Check 'Nachricht
7480 IF ASCC=ASCRET THEN IF HP=7 THEN HP=1:VP=VP-(VP<26)-(VP=26)*(-25):GOTO 7380 ELSE HP=HP+1:GOTO 7380     'NEUE SCREEN-POSITION
7490 IF CKEY<>0 THEN TEST$=MID$(DAT$(VP),-(HP<7)*2*HP-1-(HP=7)*14,-(HP<7)*2-(HP=7)*8):PRINT FNCUR$(Z,S);TEST$
7500 IF CKEY<>0 THEN IF HP<7 AND NOT TEST$="  " THEN ISJA=-1:ON HP GOSUB 12680,12710,12740,12680,12710,12740:IF NOT ISJA THEN N$="Ungültiges Datum":GOSUB 12920:GOTO 7430 'Nachricht
7510 IF CKEY=CKEYDN THEN VP=VP-(VP<26)-(VP=26)*(-25):GOTO 7380
7520 IF CKEY=CKEYUP THEN VP=VP+(VP>1)-(VP=1)*25     :GOTO 7380
7530 IF CKEY=CKEYRT THEN HP=HP-(HP<7)-(HP=7)*(-6)   :GOTO 7380
7540 IF CKEY=CKEYLT THEN HP=HP+(HP>1)+(HP=1)*(-6)   :GOTO 7380
7550 IF FKEY=1 THEN HELPID=G0120002:GOSUB 17320:GOTO 7240           'Hilfe-Text
7560 IF FKEY=2 THEN 6880                                       'ZU MTGL-EINGABE
7570 IF FKEY=3 THEN VNFLAG=0:GOSUB 11120:GOTO 2220               'DAT$() leeren
7580 IF FKEY=4 THEN GOSUB 13560:GOSUB 13300:GOTO 7430     'Zum Hauptmenü?-Frage
7590 IF FKEY=5 THEN GOSUB 14170:GOTO 7380
7600 IF FKEY=6 THEN GOSUB 14310:GOTO 7380
7610 IF FKEY=7 THEN GOSUB 14950:GOTO 7430                 'SPEICHERN DANN INPUT
7620 IF FKEY=9 THEN GOSUB 15530:GOSUB 14500:GOTO 7420
7630 '
7640 ' ############# Mitgliedsliste drucken von 05*************** #############
7650 '
7660 MT$="MITGLIEDSLISTE DRUCKEN":MI$="G-013000":GOSUB 13130       'Basis-Maske
7670 FKEYS$="":GOSUB 13240                                 'F-Tasten einblenden
7680 PRINT FNCUR$(4,30);"1 - Suchen & Drucken"
7690 PRINT FNCUR$(6,30);"2 - Komplett drucken"
7700 PRINT FNCUR$(8,30);"3 - Datenblatt drucken"
7710 FKEYMAX=4:NKEYMAX=3:GOSUB 13400                              'Menü-Eingabe
7720 IF NKEY THEN ON NKEY GOTO 7800,7800,8820
7730 IF FKEY=1 THEN HELPID=G0130000:GOSUB 17320:GOTO 7660           'Hilfe-Text
7740 IF FKEY=2 THEN 2220
7750 IF FKEY=3 THEN 2220
7760 IF FKEY=4 THEN GOSUB 13560:GOTO 7710                 'Zum Hauptmenü?-Frage
7770 '
7780 ' *** Suchen & Drucken, Komplett drucken *********************************
7790 '
7800 IF NKEY=1 THEN MT$="SUCHEN & DRUCKEN":MI$="G-013010"
7810 IF NKEY=2 THEN MT$="KOMPLETT DRUCKEN":MI$="G-013020"
7820 GOSUB 13130                                                   'Basis-Maske
7830 GOSUB 13240                                           'F-Tasten einblenden
7840 PRINT FNCUR$(4,22);"Bitte geben Sie das heutige Datum ein."
7850 PRINT FNCUR$(5,23);"Beenden Sie die Eingabe mit RETURN."
7860 PRINT FNCUR$(7,36);"=>........"
7870 ISBADINPUT=0
7880 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
7890 Z=7:S=38:LNG=8:TYP=5:ISMUSS=-1
7900 GOSUB 10380                                                'Feld-Eingabe+1
7910 IF FKEY<>1 THEN 7950       'F-Tasten verarbeiten 'TODO: Wert geht verloren
7920   IF NKEY=1 THEN HELPID=G0130104 ELSE HELPID=G0130200
7930   GOSUB 17320                                                  'Hilfe-Text
7940   GOTO 7800
7950 IF FKEY=2 THEN 7660
7960 IF FKEY=3 THEN 2220
7970 IF FKEY=4 THEN GOSUB 13560:GOTO 7900                 'Zum Hauptmenü?-Frage
7980 '
7990 POS1=INSTR(OUTTEXT$,".")                               'RETURN verarbeiten
8000 POS2=INSTR(POS1+1,OUTTEXT$,".")
8010 IF POS2=0 THEN 8240
8020 POS3=INSTR(POS2+1,OUTTEXT$,".")
8030 IF POS3<>0 THEN 8240
8040 D1$=LEFT$(OUTTEXT$,POS1-1)
8050 IF LEN(D1$)=0 OR LEN(D1$)>2 THEN 8240
8060 D2$=MID$(OUTTEXT$,POS1+1,POS2-POS1-1)
8070 IF LEN(D2$)=0 OR LEN(D2$)>2 THEN 8240
8080 D3$=RIGHT$(OUTTEXT$,LEN(OUTTEXT$)-POS2)
8090 IF LEN(D3$)=0 OR LEN(D3$)>2 THEN 8240
8100 '
8110 D1=VAL(D1$):IF D1<1 OR D1>31 THEN 8270
8120 D2=VAL(D2$):IF D2<1 OR D2>12 THEN 8270
8130 D3=VAL(D3$) 
8140 ' TODO: Genauere Verprobung
8150 D1$=STR$(D1):D1$=MID$(D1$,2,99):IF D1<10 THEN D1$="0"+D1$
8160 D2$=STR$(D2):D2$=MID$(D2$,2,99):IF D2<10 THEN D2$="0"+D2$
8170 D3$=STR$(D3):D3$=MID$(D3$,2,99):IF D3<10 THEN D3$="0"+D3$
8180 DATUM$=D1$+"."+D2$+"."+D3$
8190 '
8200 PRINT FNCUR$(7,37);HIGHON$;DATUM$;HIGHOFF$
8210 GOTO 8320
8220 '
8230 ' *** Eingabe-Fehlerbehandlung Datum
8240 N$="Datum muss Format TT.MM.JJ haben. Bitte korrigieren.":GOSUB 12920 'Nachricht
8250 GOTO 8280
8260 '
8270 N$="Ungültiges Datum. Bitte korrigieren.":GOSUB 12920           'Nachricht
8280 ISBADINPUT=1
8300 GOTO 7890
8310 '
8320 IF NKEY=2 THEN 8460
8330 '
8340 ' *** Suchen nach Nummern und Maske
8350 ISDRUCK=-1
8360 GOTO 4480                                  'Stammsatz suchen (und drucken)
8370 ' *** Rückkehr aus "Stammsatz suchen"
8380 ' IN: SEARCH (BYNUMS, BYMASK), NUMSUCH1, NUMSUCH2, SUCH$()
8410 GOSUB 15850                                         'Drucker bereit?-Frage
8420 IF NOT ISJA THEN 8780
8430 GOTO 8510
8440 '
8450 ' *** Komplette Mitgliedliste drucken
8460 SEARCH=BYNUMS
8470 NUMSUCH1=1:NUMSUCH2=MTGLMAX
8480 GOSUB 15850                                         'Drucker bereit?-Frage
8490 IF NOT ISJA THEN 7800
8500 '
8510 N$="Mitgliedsliste wird gedruckt... Abbrechen mit Taste.":GOSUB 12920 'Nachricht
8520 GOSUB 16020                                 'Mitgliedsliste drucken (Kopf)
8530 IF SEARCH=BYMASK THEN 8660
8540 '
8550 ' *** Suchen nach Nummern
8560 FOR MTGLZAHL=NUMSUCH1 TO NUMSUCH2
8570   GOSUB 11890                                             'Stammsatz lesen
8580   IF INSTR(DAT$(5),"ausgetreten")>0 THEN 8600
8590     GOSUB 16160                         'Mitgliedsliste drucken (Mitglied)
8600   GOSUB 15920                                      'Druck abbrechen?-Frage
8610   IF ISJA THEN 8770
8620 NEXT MTGLZAHL
8630 GOTO 8770
8640 ' 
8650 ' *** Suchen nach Maske
8660 MTGLZAHL=0
8670 GOSUB 6540                        'Nächsten Treffer suchen IN/OUT:MTGLZAHL
8680 IF MTGLZAHL=-1 THEN 8770
8690   GOSUB 11890                    'Stammsatz lesen, IN:MTGLZAHL, OUT:DAT$()
8700   IF INSTR(DAT$(5),"ausgetreten")>0 THEN 8720
8710     GOSUB 16160                         'Mitgliedsliste drucken (Mitglied)
8720   GOSUB 15920                                      'Druck abbrechen?-Frage
8730   IF ISJA THEN 8770
8740 GOTO 8680
8750 '
8760 ' *** Suchen zuende
8770 GOSUB 13060                                             'Nachricht löschen
8780 IF SEARCH=BYNUMS THEN 4660 ELSE 5270                'TODO: Very hackish...
8790 '
8800 ' *** Datenblatt drucken *************************************************
8810 '
8820 MT$="DATENBLATT DRUCKEN":MI$="G-013030":GOSUB 13130           'Basis-Maske
8830 FKEYS$="":GOSUB 13240                                 'F-Tasten einblenden
8840 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer"
8850 PRINT FNCUR$(5,23)"des zu druckenden Datenblattes ein."
8860 PRINT FNCUR$(6,22);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
8870 PRINT FNCUR$(7,23)"Beenden Sie die Eingabe mit RETURN."
8880 PRINT FNCUR$(10,38)"=>..."
8890 ISBADINPUT=0
8900 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
8910 Z=10:S=40:LNG=3:TYP=2:ISMUSS=-1
8920 GOSUB 10380                                                'Feld-Eingabe+1
8930 IF FKEY=1 THEN HELPID=G0130300:GOSUB 17320:GOTO 8820 'Hilfe-Text 'TODO: Wert geht verloren
8940 IF FKEY=2 THEN 7660
8950 IF FKEY=3 THEN 2220
8960 IF FKEY=4 THEN GOSUB 13560:GOTO 8920                 'Zum Hauptmenü?-Frage
8970 '
8980 M=VAL(OUTTEXT$)                                        'RETURN verarbeiten
8990 IF M>=1 AND M<=MTGLMAX THEN 9040
9000   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 12920         'Nachricht
9010   ISBADINPUT=1
9030   GOTO 8910
9040 MTGLZAHL=M
9050 GOSUB 15850                                         'Drucker bereit?-Frage
9060 IF ISJA THEN 9100
9070   GOSUB 13240                                         'F-Tasten einblenden
9080   INTEXT$=OUTTEXT$
9090   GOTO 8910
9100 N$="Datenblatt drucken...":GOSUB 13000     'Nachricht. F-Tasten ausblenden
9110 GOSUB 11890                                               'Stammsatz lesen
9120 NACHVORNAME$ = DAT$(1)+" "+DAT$(2)
9130 GOSUB 16540                          'Stammsatz drucken (Bildschirmformat)
9140 GOSUB 14080                                     'Verwendungsnachweis lesen
9150 GOSUB 15560                                   'Verwendungsnachweis drucken
9160 GOSUB 16810                                     'Mitgliedsvermerke drucken
9170 GOSUB 13060                                             'Nachricht löschen
9180 GOTO 8820
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
10341 ' IN: VNFLAG
10342 '
10370 ISBADINPUT=0
10380 IF ISBADINPUT THEN INTEXT$=OUTTEXT$
10381 P=LEN(INTEXT$)
10390 PRINT FNCUR$(Z,S);HIGHON$;INTEXT$;HIGHOFF$;STRING$(LNG-P,".")
10400 OUTTEXT$=INTEXT$
10410 PRINT CURON$;
10420 CKEY=0:FKEY=0
10430 PRINT FNCUR$(Z,S+P);
10440 A$=INPUT$(1):ASCC=ASC(A$)
10450 IF NOT ISBADINPUT THEN 10490
10460   GOSUB 13060                                          'Nachricht löschen
10470   PRINT FNCUR$(Z,S+P);
10480   ISBADINPUT=0
10490 ON TYP GOSUB 12430,12480,12530,12580,12630                    'Typ prüfen
10500 IF NOT ISVALID THEN 10560
10510   IF P>=LNG THEN 10440                                  'Gültiges Zeichen
10520   PRINT HIGHON$;A$;HIGHOFF$         
10530   OUTTEXT$=OUTTEXT$+A$
10540   P=P+1
10550   GOTO 10430
10560 IF ASCC<>ASCBSP THEN 10620                         'Backspace verarbeiten
10570   IF P<1 THEN 10440
10580   PRINT DEL$                        
10590   P=P-1
10600   OUTTEXT$=LEFT$(OUTTEXT$,P)
10610   GOTO 10430
10620 IF ASCC=ASCESC THEN 10370                             'ESCAPE verarbeiten
10630 IF ASCC=ASCRET THEN 10760                             'RETURN verarbeiten
10640 IF ASCC=ASCCUP THEN CKEY=CKEYUP:IF INSTR(CKEYMASK$,CHR$(CKEY+ASC0))>0 THEN 10760
10650 IF ASCC=ASCCDN THEN CKEY=CKEYDN:IF INSTR(CKEYMASK$,CHR$(CKEY+ASC0))>0 THEN 10760
10660 IF VNFLAG=0 THEN 10690
10670   IF ASCC=ASCCLT THEN CKEY=CKEYLT:IF INSTR(CKEYMASK$,CHR$(CKEY+ASC0))>0 THEN 10760 
10680   IF ASCC=ASCCRT THEN CKEY=CKEYRT:IF INSTR(CKEYMASK$,CHR$(CKEY+ASC0))>0 THEN 10760 
10690 IF ASCC<ASCF1 OR ASCC>ASCF9 THEN 10720              'F-Tasten verarbeiten
10700   FKEY=ASCC-ASCF1+1
10710   IF INSTR(FKEYMASK$,CHR$(FKEY+ASC0))>0 THEN 10810
10720 N$="Ungültige Eingabe - Bitte wiederholen":GOSUB 12920         'Nachricht
10730 ISBADINPUT=1
10740 GOTO 10430
10750 '
10760 IF NOT ISMUSS OR LEN(OUTTEXT$)>0 THEN 10800           'RETURN verarbeiten
10770   N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 12920
10780   ISBADINPUT=1 
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
10910 FOR K=1 TO 30:A$=INKEY$:NEXT K
10920 RETURN
10930 '
10940 ' *** Stammsatzfelde-Definitionen lesen ********************************* 
10950 '
10960 OPEN "I",#1,"B:FELD.DAT"                        
10970   INPUT #1,DUMMY
10980   FOR K=1 TO FELDMAX
10990     INPUT #1,F$
11000     FENAM$(K)=LEFT$(F$,14)
11010     FEMUSS(K)=MID$(F$,13,1)="="                          '(-1=Ja, 0=Nein)
11020     FECOL(K)=VAL(MID$(F$,15,2))
11030     FEROW(K)=VAL(MID$(F$,17,2))
11040     FELNG(K)=VAL(MID$(F$,19,2))
11050     FETYP(K)=VAL(RIGHT$(F$,1)) 
11060   NEXT K
11070 CLOSE #1
11080 RETURN
11090 '
11100 ' *** DAT$() leeren *****************************************************
11110 '
11120 FOR K=1 TO FELDMAX:DAT$(K)="":NEXT K
11130 RETURN
11140 '
11150 ' *** SUCH$() leeren ****************************************************
11160 '
11170 FOR K=1 TO FELDMAX:SUCH$(K)="":NEXT K
11180 RETURN
11190 '
11200 ' *** MTGLMAX lesen ***************************************************** 
11210 '
11220 ' OUT: MTGLMAX
11230 OPEN "I",#1,"B:MTGLZAHL.DAT"
11240   INPUT #1,MTGLMAX$
11250   MTGLMAX=VAL(MTGLMAX$)
11260 CLOSE #1
11270 RETURN
11280 '
11290 ' *** MTGLMAX speichern *************************************************
11300 '
11310 ' IN: MTGLMAX
11320 OPEN "O",#1,"B:MTGLZAHL.DAT"
11330   PRINT #1,STR$(MTGLMAX)
11340 CLOSE #1
11350 RETURN
11360 '
11370 ' *** Maske Feldnamen und Feldplatzhalter ausgeben **********************
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
11500 ' *** Maske Feldwerte ausgeben ******************************************
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
11620 ' *** Maske Feldnamen, Feldwerte und Feldplatzhalter ausgeben ***********
11630 '
11640 ' IN: FELDMAX
11650 FOR K=1 TO FELDMAX
11660   Z=FEROW(K)
11670   S=FECOL(K)
11680   PRINT FNCUR$(Z,S);FENAM$(K)
11690   S=S+XOFF
11700   PRINT FNCUR$(Z,S);HIGHON$;DAT$(K);HIGHOFF$;
11710   PRINT STRING$(FELNG(K)-LEN(DAT$(K)),".")               
11720 NEXT K
11730 RETURN
11740 '
11750 ' *** Mitgliedsnummer ausgeben ******************************************
11760 '
11770 ' IN: MTGLZAHL
11780 PRINT FNCUR$(3,31);"Mitgliedsnummer";HIGHON$;MTGLZAHL;HIGHOFF$;"   "
11790 RETURN
11800 '
11810 ' *** FIELDen der Stammsatz-Datei *************************************** 
11820 '
11830 OPEN "R",#2,"B:MV.DAT",462
11840 FIELD #2,236 AS XSTAMMSATZ1$,225 AS XSTAMMSATZ2$
11850 RETURN
11860 '
11870 ' *** Stammsatz von Disk lesen ******************************************
11880 '
11890 GET #2,MTGLZAHL
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
12270 PUT #2,MTGLZAHL
12280 RETURN
12290 '
12300 ' *** Ja/Nein-Frage und F-Tasten ausblenden *****************************
12310 '
12320 ' IN:  Q$   - Frage
12330 ' OUT: ISJA - (-1=Ja, 0=Nein)
12340 N$=Q$+" (J=Ja/N=Nein)":GOSUB 12920                             'Nachricht
12350 GOSUB 13500                                          'F-Tasten ausblenden
12360 ANSWER$=INPUT$(1)
12370 IF ANSWER$="J" OR ANSWER$="j" THEN ISJA=-1:RETURN
12380 IF ANSWER$="N" OR ANSWER$="n" THEN ISJA=0:RETURN
12390 GOTO 12360
12400 '
12410 ' *** Testbereich 1 (Alpha) *********************************************
12420 '
12430 ISVALID=0
12431 IF (ASCC>57 AND ASCC<127) OR (ASCC>31 AND ASCC<48) THEN ISVALID=-1
12440 RETURN
12450 '
12460 ' *** Testbereich 2 (Numerisch) *****************************************
12470 '
12480 ISVALID=0: IF ASCC>47 AND ASCC<58 THEN ISVALID=-1
12490 RETURN
12500 '
12510 ' *** Testbereich 3 (Alphanumerisch) ************************************
12520 '
12530 ISVALID=0: IF ASCC>31 AND ASCC<127 THEN ISVALID=-1
12540 RETURN
12550 '
12560 ' *** Testbereich 4 (Numerisch + "/" und ".") ***************************
12570 '
12580 ISVALID=0: IF ASCC>45 AND ASCC<58 THEN ISVALID=-1
12590 RETURN
12600 '
12610 ' *** Testbereich 5 (Numerisch + und ".") *******************************
12620 '
12630 ISVALID=0: IF (ASCC>=48 AND ASCC<=57) OR ASCC=46 THEN ISVALID=-1
12640 RETURN
12650 '
12660 ' *** DATUMS-TEST (bei VN und M-Liste drucken) **************************
12670 '
12680 IF VAL(TEST$)=0 OR VAL(TEST$)>31 THEN ISJA=0                         'Tag
12690 RETURN
12700 '
12710 IF VAL(TEST$)=0 OR VAL(TEST$)>12 THEN ISJA=0                       'Monat
12720 RETURN
12730 '
12740 IF VAL(TEST$)=0 THEN ISJA=0                                         'Jahr
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
13250 IF FKEYS$<>"" THEN PRINT " ";FKEYS$ ELSE PRINT           
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
13410 GOSUB 10910                                          'Tastatur entprellen
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
13570 IF ISJA THEN 17610                                          'Programmende
13580 GOSUB 13240                                          'F-Tasten einblenden
13590 RETURN
13600 '
13610 ' *** 'Zum Hauptmenü?-Frage (einfach) ************************** REVIEWED
13620 '
13630 ' OUT: ISJA (-1=Ja, 0=Nein)
13640 Q$="Wollen Sie wirklich ins Hauptmenü?":GOSUB 12340        'Ja/Nein-Frage
13650 GOSUB 13060                                            'Nachricht löschen
13660 RETURN
13670 '
13680 ' *** Rückkehr aus Hilfe-Text? ********************************* REVIEWED
13690 '
13700 N$="Zurück? (J=Ja)":GOSUB 12920                                'Nachricht
13710 GOSUB 12350                             'Ja/Nein-Frage ohne String-Koncat
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
14430 Q$="Letzte Seite - Neue Seite anlegen?":GOSUB 12340        'Ja/Nein-Frage
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
15530 Q$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 12340 'Ja/Nein-Frage TODO Back to main code?
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
15850 Q$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 12340 'Ja/Nein-Frage
15860 GOSUB 13060                                            'Nachricht löschen
15870 RETURN
15880 '
15890 ' *** Druck abbrechen?-Frage ********************************************
15900 '
15910 ' OUT: ISJA (-1=Ja, 0=Nein)
15920 ISJA=0 
15930 IF INKEY$="" THEN 15970
15940 Q$="Drucken abbrechen?":GOSUB 12340                        'Ja/Nein-Frage
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
16540 RETURN                                         '*** TMP Print Disable ***
16550 XCOL1=8
16560 XCOL2=43
16570 LPRINT STRICH$
16580 LPRINT
16590 LPRINT TAB(31);"Mitgliedsnummer";MTGLZAHL
16600 LPRINT
16610 RESTORE 16670
16620 FOR K=1 TO 7:READ A,B
16630   IF A<>0 THEN LPRINT TAB(XCOL1);FENAM$(A);DAT$(A);
16640   IF B<>0 THEN LPRINT TAB(XCOL2);FENAM$(B);DAT$(B);
16650   PRINT
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
17230 OPEN "R",#6,"HELP.NDX",4
17231   FIELD #6,2 AS XFIRSTLINE$,2 AS XLASTLINE$
17232   FOR K=1 TO HELPIDMAX
17233     GET #6,K
17234     HELPFIRSTLINE(K)=CVI(XFIRSTLINE$)
17235     HELPLASTLINE(K)=CVI(XLASTLINE$)
17236   NEXT K
17237 CLOSE #6
17238 '
17250 OPEN "R",#6,"HELP.DAT",2+2+60
17260 FIELD #6,2 AS XINDENT$,2 AS XLENGTH$,60 AS XLINE$
17270 RETURN
17280 '
17290 ' *** Hilfe-Text-Seite anzeigen *****************************************
17300 '
17310 ' IN: HELPID
17320 SAVEMT$=MT$:MT$="HILFE":GOSUB 13130:MT$=SAVEMT$              'Basis-Maske
17330 PRINT HOME$
17361 FIRSTLINE=HELPFIRSTLINE(HELPID)
17362 LASTLINE=HELPLASTLINE(HELPID)
17370 FOR K=FIRSTLINE TO LASTLINE
17380   GET #6,K
17390   INDENT=CVI(XINDENT$)
17400	LENGTH=CVI(XLENGTH$)
17410   PRINT SPC(INDENT);LEFT$(XLINE$,LENGTH)
17420 NEXT K
17430 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
17440 RETURN
17450 '
17460 ' *** Programmfehler ******************************************* REVIEWED
17470 '
17480 I$=SPACE$(15)
17490 GOSUB 13130                                                  'Basis-Maske
17500 PRINT HOME$
17510 PRINT I$;"Ein Programmfehler ist aufgetreten."
17520 PRINT   
17530 PRINT I$;"- Notieren Sie folgende Fehlerinformation:"
17540 PRINT I$;"  ";HIGHON$;"ERL";HIGHOFF$;" (Error Line)";HIGHON$;ERL;HIGHOFF$
17550 PRINT I$;"  ";HIGHON$;"ERR";HIGHOFF$;" (Error Code)";HIGHON$;ERR;HIGHOFF$
17560 PRINT I$;"- Drücken Sie am Rechner den Knopf hinten rechts."
17570 PRINT I$;"19**************************"
17580 '
17590 ' *** Programmende ********************************************* REVIEWED
17600 '
17610 PRINT FNCUR$(22,1);HIGHOFF$;CURON$;
17620 RESET
17630 'LW RUN "DISKMENU.EXE"
17640 END                                                           '1595 lines
