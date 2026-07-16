1000 ' RELEASE 01***** Neubearbeitung durch 04**********
1010 '
1020 ' LW 31-JAN-2026: Load MSBASIC.COM with /S:560 /F:6
1030 '
1040 ' *** Initialisierung, Dimensionierung ***********************************
1050 '
1060 DEFINT A-Z
1070 '
1080 TRUE=-1
1090 FALSE=0
1100 '
1110 FIELDMAX=27                                         'Anzahl Stammsatzfelder
1120 DIM FIELDNAM$(FIELDMAX)                                       'Name des Feldes
1130 DIM FIELDISMAND(FIELDMAX)                             'Muss-Feld (TRUE, FALSE)
1140 DIM FIELDROW(FIELDMAX)                           'Zeile des Feldnamens (1..25)
1150 DIM FIELDCOL(FIELDMAX)                          'Spalte des Feldnamens (1..80)
1160 DIM FIELDLNG(FIELDMAX)                                       'Länge des Feldes
1170 DIM FIELDTYP(FIELDMAX)                                         'Typ des Feldes
1180 '
1190 DIM DAT$(FIELDMAX)                                      'Stammsatzfelddaten
1200 DIM SUCH$(FIELDMAX)                                     'Stammsatzsuchdaten
1210 '
1220 NUMSUCHMAX=2
1230 DIM NUMSUCH$(NUMSUCHMAX)
1240 DIM NUMSUCHROW(NUMSUCHMAX)
1250 NUMSUCHROW(1)=4
1260 NUMSUCHROW(2)=6
1270 '
1280 BYNUMS=1                                                 'Werte von SEARCH
1290 BYMASK=2
1300 '
1310 BSP$   =CHR$(8)                                                 'Backspace
1320 DEL$   =BSP$+"."+BSP$                                              'Delete
1330 ESC$   =CHR$(27)                                                   'Escape
1340 STRICH$=STRING$(79,"-")                                            'Strich
1350 '
1360 CLS$    =ESC$+"E"                                      'Bildschirm löschen
1370 INVON$  =ESC$+"p"                                              'Invers ein
1380 INVOFF$ =ESC$+"q"                                              'Invers aus
1390 CURON$  =ESC$+"y5"                                             'Cursor ein
1400 CUROFF$ =ESC$+"x5"                                             'Cursor aus
1410 HIGHON$ =ESC$+"("                                           'Helle Schrift
1420 HIGHOFF$=ESC$+")"                                          'Dunkle Schrift
1430 DELLINE$=ESC$+"K"                                           'Zeile löschen
1440 DELHIGH$=ESC$+"+"                                'LW Helle Schrift löschen
1450 '
1460 DEF FNCUR$(Z,S)=ESC$+"Y"+CHR$(31+Z)+CHR$(31+S)       'Cursor positionieren
1470 HOME$=FNCUR$(3,1)                                    'Cursor Home Position
1480 ' 
1490 CKEYUP=1                                    'Werte von CKEY (Cursortasten)
1500 CKEYDN=2
1510 CKEYLT=3
1520 CKEYRT=4
1530 '
1540 ASCBSP=8    'Werte von ASCC (ASCII-Code des ersten eingegebenen Feldwerts)
1550 ASCRET=13
1560 ASCESC=27
1570 ASC0=48
1580 ASC1=49
1590 ASCCUP=200
1600 ASCCDN=201
1610 ASCCLT=202
1620 ASCCRT=203
1630 ASCF1=241
1640 ASCF4=244
1650 ASCF9=249
1660 ASCF9=249
1670 '
1680 HELPIDMAX=23                                     'Anzahl Hilfe-Text-Seiten
1690 DIM HELPFIRSTLINE(HELPIDMAX)
1700 DIM HELPLASTLINE(HELPIDMAX)
1710 '
1720 G0100000=1                                           'Mitgliederverwaltung
1730 G0110000=2                                                'Stammsatzpflege
1740 G0110100=3                                              'Stammsatz anlegen
1750 G0110201=4                                           'Stammsatz ändern (I)
1760 G0110202=5                                          'Stammsatz ändern (II)
1770 G0110300=6                                              'Stammsatz löschen
1780 G0110400=7                                              'Stammsatz drucken
1790 G0110501=8                                           'Stammsatz suchen (I)
1800 G0110510=9                                  'Stammsatz suchen nach Nummern
1810 G0110520=10                                   'Stammsatz suchen nach Maske
1820 G0110502=11                           'Stammsatz suchen (II) Keine Treffer
1830 G0110503=12                               'Stammsatz suchen (III) Blättern
1840 G0120001=13                                       'Verwendungsnachweis (I)
1850 G0120002=14                                      'Verwendungsnachweis (II)
1860 G0130000=15                                        'Mitgliedsliste drucken
1870 G0130101=16                                    'Suchen & Drucken (I) Datum
1880 G0130102=17                                         'Suchen & Drucken (II)
1890 G0130110=18                                 'Suchen & Drucken nach Nummern
1900 G0130120=19                                   'Suchen & Drucken nach Maske
1910 G0130103=20                           'Suchen & Drucken (II) Keine Treffer
1920 G0130200=21                                           'Komplettdruck Datum
1930 G0130300=22                                            'Datenblatt drucken
1940 '
1950 ' Weitere Variablen
1960 ' -----------------
1970 ' F        - Feldindex 1..27 (FIELDMAX)
1980 ' I$       - Lokale Texteinrückung
1990 ' K        - Schleifenindex
2000 ' MTGLMAX  - Höchste auf Disk gespeicherte Mitgliedsnummer
2010 ' MTGLZAHL - Momentane Mitgliedsnummer
2020 '
2030 COLSEP=14           'Offset Colum Feldname to Column Wertefeld (Pünktchen)
2040 '
2050 ' TIP: Do not let the cursor slip into line 25 -> display will scroll up
2060 ' TIP: Use semicolons with PRINT carefully -> display driver bug
2070 '
2080 ON ERROR GOTO 18100                               'Programmfehler abfangen
2090 '
2100 GOSUB 10050    'Tasten deaktivieren, ASCII-Codes für Cursortasten vergeben
2110 PRINT CUROFF$;
2120 '
2130 ' *** Beginn des Mitgliederverwaltungsprogramms **************************
2140 '
2150 MT$="MITGLIEDERVERWALTUNG":MI$="G-010000":GOSUB 13690         'Basis-Maske
2160 Q$="Bitte Daten-Diskette in Laufwerk B einlegen. Bereit?":GOSUB 12580 'Ja/Nein-Frage
2170 IF NOT ISJA THEN 18230                                       'Programmende
2180 '
2190 N$="Einen Moment bitte...":GOSUB 13480                          'Nachricht
2200 GOSUB 11500                                                 'MTGLMAX lesen
2210 GOSUB 10960                              'Stammsatzfeld-Definitionen lesen
2220 GOSUB 12090                                   'Fielden der Stammsatz-Datei
2230 GOSUB 14360                       'Fielden der Verwendungsnachweis-Dateien
2240 GOSUB 17770      'Fielden der Hilfe-Text-Dateien, lesen der Zeilenbereiche
2250 GOSUB 13620                                             'Nachricht löschen
2260 '
2270 ' ###################### Mitgliederverwaltungs-Menü ######################
2280 '
2290 MT$="MITGLIEDERVERWALTUNG":MI$="G-010000":GOSUB 13690         'Basis-Maske
2300 FKEYS$="":GOSUB 13800                                 'F-Tasten einblenden
2310 PRINT FNCUR$(4,28);"1 - Stammsatzpflege"
2320 PRINT FNCUR$(6,28);"2 - Verwendungsnachweis"
2330 PRINT FNCUR$(8,28);"3 - Mitgliedsliste drucken"
2340 FKEYMAX=4:NKEYMAX=3:GOSUB 13960                              'Menü-Eingabe
2350 IF FKEY THEN ON FKEY GOTO 2370,2380,2290,2380
2360 ON NKEY GOTO 2440,7090,7920
2370 HELPID=G0100000:GOSUB 17930:GOTO 2290                          'Hilfe-Text
2380 GOSUB 14120:GOTO 2340                                'Zum Hauptmenü?-Frage
2390 '
2400 ' ########################### Stammsatzpflege ############################
2410 '
2420 ' *** Stammsatzpflege-Menü ***********************************************
2430 '
2440 MT$="STAMMSATZPFLEGE":MI$="G-011000":GOSUB 13690              'Basis-Maske
2450 FKEYS$="":GOSUB 13800                                 'F-Tasten einblenden
2460 PRINT FNCUR$(4,30);"1 - Stammsatz anlegen"
2470 PRINT FNCUR$(6,30);"2 - Stammsatz ändern"
2480 PRINT FNCUR$(8,30);"3 - Stammsatz löschen"
2490 PRINT FNCUR$(10,30);"4 - Stammsatz drucken"
2500 PRINT FNCUR$(12,30);"5 - Stammsatz suchen"
2510 FKEYMAX=4:NKEYMAX=5:GOSUB 13960                              'Menü-Eingabe
2520 IF FKEY THEN ON FKEY GOTO 2540,2290,2290,2550
2530 ON NKEY GOTO 2590,3080,3740,4160,4520
2540 HELPID=G0110000:GOSUB 17930:GOTO 2440                          'Hilfe-Text
2550 GOSUB 14120:GOTO 2510                                'Zum Hauptmenü?-Frage
2560 '
2570 ' *** Stammsatz anlegen von 02************ *******************************
2580 '
2590 MTGLZAHL=MTGLMAX+1
2600 F=1
2610 GOSUB 11390                                                 'DAT$() leeren
2620 GOSUB 13350                                     'Voreinstellungen zuweisen
2630 MT$="STAMMSATZ ANLEGEN":MI$="G-011010":GOSUB 13690            'Basis-Maske
2640 FKEYS$="<F7>=Speichern":GOSUB 13800                   'F-Tasten einblenden
2650 GOSUB 12040                                      'Mitgliedsnummer ausgeben
2660 GOSUB 11780             'Feldnamen, Feldwerte mit Feldplatzhalter ausgeben
2670 Z=FIELDROW(F):S=FIELDCOL(F)+COLSEP:LNG=FIELDLNG(F):TYP=FIELDTYP(F):ISMUSS=FIELDISMAND(F)
2680 FKEYMASK$="12347":CKEYMASK$="12":INTEXT$=DAT$(F)
2690 GOSUB 10360                                                  'Feld-Eingabe
2700 '
2710 IF ASCC<>ASCRET THEN 2750                              'RETURN verarbeiten
2720   DAT$(F)=OUTTEXT$
2730   F=F+1:IF F>FIELDMAX THEN F=1
2740   GOTO 2670
2750 IF CKEY=0 THEN 2800                              'Cursortasten verarbeiten
2760   PRINT FNCUR$(Z,S);HIGHON$;DAT$(F);HIGHOFF$;STRING$(LNG-LEN(DAT$(F)),".")
2770   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FIELDMAX
2780   IF CKEY=CKEYDN THEN F=F+1:IF F>FIELDMAX THEN F=1
2790   GOTO 2670
2800 IF FKEY=1 THEN HELPID=G0110100:GOSUB 17930:GOTO 2630           'Hilfe-Text
2810 IF FKEY=2 THEN GOSUB 2910:GOTO 2440              'Exit "Stammsatz anlegen"
2820 IF FKEY=3 THEN GOSUB 2910:GOTO 2290              'Exit "Stammsatz anlegen"
2830 IF FKEY<>4 THEN 2880
2840   GOSUB 14200                              'Zum Hauptmenü?-Frage (einfach)
2850   IF NOT ISJA THEN GOSUB 13800:GOTO 2690              'F-Tasten einblenden
2860   GOSUB 2910                                     'Exit "Stammsatz anlegen"
2870   GOTO 18230                                                 'Programmende
2880 IF FKEY=7 THEN GOSUB 2980:GOTO 2590            'Exit "Stammsatz anlegen" 2
2890 '
2900 ' *** SUB Exit "Stammsatz anlegen"
2910 GOSUB 10910                                           'Tastatur entprellen
2920 Q$="Wollen Sie vorher den Stammsatz speichern?":GOSUB 12580 'Ja/Nein-Frage
2930 IF ISJA THEN 2980
2940   MTGLZAHL=MTGLZAHL-1
2950   GOTO 3030
2960 '                                                            
2970 ' *** SUB Exit "Stammsatz anlegen" 2
2980 N$="Stammsatz wird gespeichert...":GOSUB 13560 'Nachricht. F-Tasten ausblenden
2990 GOSUB 12370                                           'Stammsatz speichern
3000 GOSUB 14470                       'Erste Verwendungsnachweis-Seite anlegen
3010 MTGLMAX=MTGLZAHL:GOSUB 11590                             'MTGLMAX speichen
3020 '
3030 GOSUB 13620                                             'Nachricht löschen
3040 RETURN
3050 ' 
3060 ' *** Stammsatz ändern von 03************ ********************************
3070 '
3080 MT$="STAMMSATZ ÄNDERN":MI$="G-011020":GOSUB 13690             'Basis-Maske
3090 FKEYS$="":GOSUB 13800                                 'F-Tasten einblenden
3100 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer" 
3110 PRINT FNCUR$(5,24)"des zu ändernden Stammsatzes ein."
3120 PRINT FNCUR$(6,22);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
3130 PRINT FNCUR$(7,23)"Beenden Sie die Eingabe mit RETURN."
3140 PRINT FNCUR$(9,38)"=>..."
3150 ISINPUTOK=TRUE
3160 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
3170 Z=9:S=40:LNG=3:TYP=2:ISMUSS=TRUE
3180 GOSUB 10370                                                'Feld-Eingabe+1
3190 IF FKEY=1 THEN HELPID=G0110201:GOSUB 17930:GOTO 3080 'Hilfe-Text 'TODO: Wert geht verloren
3200 IF FKEY=2 THEN 2440
3210 IF FKEY=3 THEN 2290
3220 IF FKEY=4 THEN GOSUB 14120:GOTO 3180                 'Zum Hauptmenü?-Frage
3230 '
3240 M=VAL(OUTTEXT$)                                        'RETURN verarbeiten
3250 IF M>=1 AND M<=MTGLMAX THEN 3290
3260   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 13480         'Nachricht
3270   ISINPUTOK=FALSE
3280   GOTO 3170
3290 MTGLZAHL=M
3300 N$="Stammsatz wird gelesen...":GOSUB 13560 'Nachricht. F-Tasten ausblenden
3310 GOSUB 12150                                               'Stammsatz lesen
3320 GOSUB 13620                                             'Nachricht löschen
3330 '
3340 GOSUB 13690                                                   'Basis-Maske
3350 FKEYS$="<F7>=Speichern":GOSUB 13800                   'F-Tasten einblenden
3360 GOSUB 12040                                      'Mitgliedsnummer ausgeben
3370 GOSUB 11780             'Feldnamen, Feldwerte mit Feldplatzhalter ausgeben
3380 F=1
3390 Z=FIELDROW(F):S=FIELDCOL(F)+COLSEP:LNG=FIELDLNG(F):TYP=FIELDTYP(F):ISMUSS=FIELDISMAND(F)
3400 FKEYMASK$="12347":CKEYMASK$="12":INTEXT$=DAT$(F)
3410 GOSUB 10360                                                  'Feld-Eingabe
3420 '
3430 IF ASCC<>ASCRET THEN 3470                              'RETURN verarbeiten
3440   DAT$(F)=OUTTEXT$
3450   F=F+1:IF F>FIELDMAX THEN F=1
3460   GOTO 3390
3470 IF CKEY=0 THEN 3520                              'Cursortasten verarbeiten
3480   PRINT FNCUR$(Z,S);HIGHON$;DAT$(F);HIGHOFF$;STRING$(LNG-LEN(DAT$(F)),".")
3490   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FIELDMAX
3500   IF CKEY=CKEYDN THEN F=F+1:IF F>FIELDMAX THEN F=1
3510   GOTO 3390
3520 IF FKEY=1 THEN HELPID=G0110202:GOSUB 17930:GOTO 3340           'Hilfe-Text
3530 IF FKEY=2 THEN GOSUB 3630:GOTO 3080               'Exit "Stammsatz ändern"
3540 IF FKEY=3 THEN GOSUB 3630:GOTO 2290               'Exit "Stammsatz ändern"
3550 IF FKEY<>4 THEN 3600 
3560   GOSUB 14200                              'Zum Hauptmenü?-Frage (einfach)
3570   IF NOT ISJA THEN GOSUB 13800:GOTO 3410              'F-Tasten einblenden
3580   GOSUB 3630                                      'Exit "Stammsatz ändern"
3590   GOTO 18230                                                 'Programmende
3600 IF FKEY=7 THEN GOSUB 3670:GOTO 3080             'Exit "Stammsatz ändern" 2
3610 '
3620 ' *** SUB Exit "Stammsatz ändern"
3630 Q$="Wollen Sie vorher den Stammsatz speichern?":GOSUB 12580 'Ja/Nein-Frage
3640 IF NOT ISJA THEN RETURN
3650 '
3660 ' *** SUB Exit "Stammsatz ändern" 2
3670 N$="Stammsatz wird gespeichert...":GOSUB 13560 'Nachricht. F-Tasten ausblenden
3680 GOSUB 12370                                           'Stammsatz speichern
3690 GOSUB 13620                                             'Nachricht löschen
3700 RETURN
3710 '
3720 ' *** Stammsatz löschen von 04********** *********************************
3730 '
3740 MT$="STAMMSATZ LÖSCHEN":MI$="G-011030":GOSUB 13690            'Basis-Maske
3750 FKEYS$="":GOSUB 13800                                 'F-Tasten einblenden
3760 PRINT FNCUR$(4,23);"Bitte geben Sie die Mitgliedsnummer"         'Menütext
3770 PRINT FNCUR$(5,24);"des zu löschenden Mitglieds ein."
3780 PRINT FNCUR$(6,22);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
3790 PRINT FNCUR$(7,15);"(Dabei wird nur der Status des Mitglieds verändert.)"
3800 PRINT FNCUR$(8,23);"Beenden Sie die Eingabe mit RETURN."
3810 PRINT FNCUR$(10,38);"=>..."
3820 ISINPUTOK=TRUE
3830 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
3840 Z=10:S=40:LNG=3:TYP=2:ISMUSS=TRUE
3850 GOSUB 10370                                                'Feld-Eingabe+1
3860 IF FKEY=1 THEN HELPID=G0110300:GOSUB 17930:GOTO 3740 'Hilfe-Text 'TODO: Wert geht verloren
3870 IF FKEY=2 THEN 2440
3880 IF FKEY=3 THEN 2290
3890 IF FKEY=4 THEN GOSUB 14120:GOTO 3850                 'Zum Hauptmenü?-Frage
3900 '
3910 M=VAL(OUTTEXT$)                                        'RETURN verarbeiten
3920 IF M>=1 AND M<=MTGLMAX THEN 3960
3930   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 13480         'Nachricht
3940   ISINPUTOK=FALSE
3950   GOTO 3840
3960 MTGLZAHL=M
3970 N$="Stammsatz wird gelesen...":GOSUB 13560 'Nachricht. F-Tasten ausblenden
3980 GOSUB 12150                                               'Stammsatz lesen
3990 '
4000 GOSUB 13690                                                   'Basis-Maske
4010 GOSUB 12040                                      'Mitgliedsnummer ausgeben
4020 GOSUB 11780             'Feldnamen, Feldwerte mit Feldplatzhalter ausgeben
4030 Q$="Mitglied endgültig löschen?":GOSUB 12580                'Ja/Nein-Frage
4040 IF NOT ISJA THEN 4120
4050   F=5
4060   DAT$(F)="ausgetreten"
4070   Z=FIELDROW(F):S=FIELDCOL(F)+COLSEP:LNG=FIELDLNG(F)
4080   PRINT FNCUR$(Z,S);HIGHON$;DAT$(F);HIGHOFF$;STRING$(LNG-LEN(DAT$(F)),".")               
4090   N$="Stammsatz wird gespeichert...":GOSUB 13560 'Nachricht. F-Tasten ausblenden
4100   GOSUB 12370                                         'Stammsatz speichern
4110   GOSUB 13620                                           'Nachricht löschen
4120 GOTO 3740
4130 '
4140 ' *** Stammsatz drucken von 02************ & 04********** ****************
4150 '
4160 MT$="STAMMSATZ DRUCKEN":MI$="G-011040":GOSUB 13690            'Basis-Maske
4170 FKEYS$="":GOSUB 13800                                 'F-Tasten einblenden
4180 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer"
4190 PRINT FNCUR$(5,23)"des auszudruckenden Stammsatzes ein."
4200 PRINT FNCUR$(6,22);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
4210 PRINT FNCUR$(7,23)"Beenden Sie die Eingabe mit RETURN."
4220 PRINT FNCUR$(9,38);"=>..."
4230 ISINPUTOK=TRUE
4240 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
4250 Z=9:S=40:LNG=3:TYP=2:ISMUSS=TRUE
4260 GOSUB 10370                                                'Feld-Eingabe+1
4270 IF FKEY=1 THEN HELPID=G0110400:GOSUB 17930:GOTO 4160 'Hilfe-Text 'TODO: Wert geht verloren
4280 IF FKEY=2 THEN 2440
4290 IF FKEY=3 THEN 2290
4300 IF FKEY=4 THEN GOSUB 14120:GOTO 4260                 'Zum Hauptmenü?-Frage
4310 '
4320 M=VAL(OUTTEXT$)                                        'RETURN verarbeiten
4330   IF M>=1 AND M<=MTGLMAX THEN 4370
4340   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 13480         'Nachricht
4350   ISINPUTOK=FALSE
4360   GOTO 4250
4370 MTGLZAHL=M
4380 N$="Stammsatz wird gelesen...":GOSUB 13560 'Nachricht. F-Tasten ausblenden
4390 GOSUB 12150                                               'Stammsatz lesen
4400 GOSUB 13620                                             'Nachricht löschen
4410 '
4420 GOSUB 13690                                                   'Basis-Maske
4430 GOSUB 12040                                      'Mitgliedsnummer ausgeben
4440 GOSUB 11780             'Feldnamen, Feldwerte mit Feldplatzhalter ausgeben
4450 GOSUB 16410                                         'Drucker bereit?-Frage
4460 IF NOT ISJA THEN 4480
4470   GOSUB 17100                        'Stammsatz drucken (Bildschirmformat)
4480 GOTO 4160
4490 '
4500 ' *** Stammsatz suchen (und drucken) von 05*************** ***************
4510 '
4520 ISDRUCK=FALSE
4530 MT$="STAMMSATZ SUCHEN":IF ISDRUCK THEN MT$="SUCHEN & DRUCKEN" 'TODO Einsprung Suchen & Drucken
4540 MI$="G-011050":IF ISDRUCK THEN MI$="G-013010"
4550 GOSUB 13690                                                   'Basis-Maske
4560 FKEYS$="":GOSUB 13800                                 'F-Tasten einblenden
4570 TMP$="Suchen":IF ISDRUCK THEN TMP$="Suchen & drucken" 
4580 TMP1=29:IF ISDRUCK THEN TMP1=24
4590 PRINT FNCUR$(4,TMP1)"1 - ";TMP$;" nach Nummern"
4600 PRINT FNCUR$(6,TMP1)"2 - ";TMP$;" nach Maske"
4610 NKEYMAX=2:FKEYMAX=4:GOSUB 13960                              'Menü-Eingabe
4620 IF NKEY THEN ON NKEY GOTO 4720,5320
4630 IF FKEY<>1 THEN 4670 
4640   HELPID=G0110501:IF ISDRUCK THEN HELPID=G0130102
4650   GOSUB 17930                                                  'Hilfe-Text
4660   GOTO 4530
4670 IF FKEY=2 THEN IF ISDRUCK THEN 8050 ELSE 2440    'Absprung F2 Suchen & Drucken oder Suchen
4680 IF FKEY=3 THEN 2290
4690 IF FKEY=4 THEN GOSUB 14120:GOTO 4610                 'Zum Hauptmenü?-Frage
4700 '
4710 ' *** Suchen nach Nummern
4720 TMP$=STR$(1):NUMSUCH$(1)=MID$(TMP$,2,99) 'Einsprung nach Ende Suchen & Drucken
4730 TMP$=STR$(MTGLMAX):NUMSUCH$(2)=MID$(TMP$,2,99)
4740 MT$="STAMMSATZ SUCHEN NACH NUMMERN":MI$="G-011051"
4750 IF ISDRUCK THEN MT$="SUCHEN & DRUCKEN NACH NUMMERN":MI$="G-013011"
4760 GOSUB 13690                                                   'Basis-Maske
4770 FKEYS$="<F5>=Suchen":IF ISDRUCK THEN FKEYS$="<F5>=Suchen & Drucken"
4780 GOSUB 13800                                           'F-Tasten einblenden
4790 TMP$="gesucht":IF ISDRUCK THEN TMP$="gesucht und gedruckt"
4800 TMP1=12:IF ISDRUCK THEN TMP1=6
4810 TMP2=66:IF ISDRUCK THEN TMP2=73
4820 PRINT FNCUR$(4,TMP1);"Ab welcher Mitgliedsnummer soll ";TMP$;" werden?     =>..."
4830 PRINT FNCUR$(6,TMP1);"Bis zu welcher Mitgliedsnummer soll ";TMP$;" werden? =>..."
4840 PRINT FNCUR$(4,TMP2);HIGHON$;NUMSUCH$(1);HIGHOFF$
4850 PRINT FNCUR$(6,TMP2);HIGHON$;NUMSUCH$(2);HIGHOFF$
4860 PRINT FNCUR$(8,22);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
4870 F=1
4880 ISINPUTOK=TRUE
4890 FKEYMASK$="12345":CKEYMASK$="":INTEXT$=NUMSUCH$(F)
4900 Z=NUMSUCHROW(F):LNG=3:TYP=2:ISMUSS=TRUE
4910 S=66:IF ISDRUCK THEN S=73
4920 GOSUB 10370                                                'Feld-Eingabe+1
4930 IF FKEY<>1 THEN 4970                                 'F-Tasten verarbeiten
4940   HELPID=G0110510:IF ISDRUCK THEN HELPID=G0130110
4950   GOSUB 17930                                                  'Hilfe-Text
4960   GOTO 4740                                    'TODO: Werte gehen verloren
4970 IF FKEY=2 THEN 4530
4980 IF FKEY=3 THEN 2290
4990 IF FKEY=4 THEN GOSUB 14120:GOTO 4920                 'Zum Hauptmenü?-Frage
5000 IF FKEY<>5 THEN 5060
5010   SEARCH=BYNUMS
5020   NUMSUCH1=VAL(NUMSUCH$(1))
5030   NUMSUCH2=VAL(NUMSUCH$(2))
5040   IF ISDRUCK THEN 8360 ELSE 5640    'Absprung Suchen & Drucken oder Suchen
5050 '
5060 M=VAL(OUTTEXT$)                                        'RETURN verarbeiten
5070 IF M>=1 AND M<=MTGLMAX THEN 5110
5080   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 13480         'Nachricht
5090   GOSUB 5250                                     'Eingabefehler-Behandlung
5100   GOTO 4900
5110 NUMSUCH$(F)=OUTTEXT$
5120 NUMSUCH1=VAL(NUMSUCH$(1))
5130 NUMSUCH2=VAL(NUMSUCH$(2))
5140 IF NUMSUCH1<=NUMSUCH2 THEN 5180
5150   N$="Ungültiger Bereich":GOSUB 13480                           'Nachricht
5160   GOSUB 5250                                     'Eingabefehler-Behandlung
5170   GOTO 4900
5180 F=F+1:IF F>NUMSUCHMAX THEN F=1
5190 GOSUB 14060                                           'F-Tasten ausblenden
5200 FKEYS$="<F5>=Suchen":IF ISDRUCK THEN FKEYS$="<F5>=Suchen & Drucken" 
5210 GOSUB 13800                                           'F-Tasten einblenden
5220 GOTO 4890
5230 '
5240 ' *** SUB Eingabefehler-Behandlung
5250 ISINPUTOK=FALSE
5260 FKEYMASK$="1234"
5270 GOSUB 14060                                           'F-Tasten ausblenden
5280 FKEYS$="":GOSUB 13800                                 'F-Tasten einblenden
5290 RETURN
5300 '
5310 ' *** Suchen nach Maske
5320 GOSUB 11440                                                'SUCH$() leeren
5330 MT$="STAMMSATZ SUCHEN NACH MASKE":MI$="G-011052" 'Einsprung nach Ende Suchen & Drucken
5340 IF ISDRUCK THEN MT$="SUCHEN & DRUCKEN NACH MASKE":MI$="G-013012"
5350 GOSUB 13690                                                   'Basis-Maske
5360 FKEYS$="<F5>=Suchen":IF ISDRUCK THEN FKEYS$="<F5>=Suchen & Drucken" 
5370 GOSUB 13800                                           'F-Tasten einblenden
5380 GOSUB 11910         'Feldnamen, Suchfeldwerte und Feldplatzhalter ausgeben
5390 F=1
5400 Z=FIELDROW(F):S=FIELDCOL(F)+COLSEP:LNG=FIELDLNG(F):TYP=FIELDTYP(F):ISMUSS=FIELDISMAND(F)
5410 FKEYMASK$="12345":CKEYMASK$="12":INTEXT$=SUCH$(F)
5420 GOSUB 10360                                                  'Feld-Eingabe
5430 IF FKEY<>1 THEN 5470                                 'F-Tasten verarbeiten
5440   HELPID=G0110520:IF ISDRUCK THEN HELPID=G0130120
5450   GOSUB 17930                                                  'Hilfe-Text
5460   GOTO 5330
5470 IF FKEY=2 THEN 4530
5480 IF FKEY=3 THEN 2290
5490 IF FKEY=4 THEN GOSUB 14120:GOTO 5420                 'Zum Hauptmenü?-Frage
5500 IF FKEY<>5 THEN 5530 
5510   SEARCH=BYMASK                                                      '<F5>
5520   IF ISDRUCK THEN 8360 ELSE 5640             'Suchen oder Suchen & Drucken
5530 IF CKEY=0 THEN 5590                              'Cursortasten verarbeiten
5540   PRINT FNCUR$(Z,S);HIGHON$;SUCH$(F);HIGHOFF$;
5550   PRINT STRING$(LNG-LEN(SUCH$(F)),".")
5560   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FIELDMAX
5570   IF CKEY=CKEYDN THEN F=F+1:IF F>FIELDMAX THEN F=1
5580   GOTO 5400
5590 SUCH$(F)=OUTTEXT$                                      'RETURN verarbeiten
5600 F=F+1:IF F>FIELDMAX THEN F=1
5610 GOTO 5400
5620 '
5630 ' *** Suche
5640 IF SEARCH=BYMASK THEN 5710
5650   GOSUB 13690                                                 'Basis-Maske
5660   GOSUB 11390                                                'Leere DAT$()
5670   GOSUB 11780           'Feldnamen, Feldwerte mit Feldplatzhalter ausgeben
5680   MTGLZAHL=NUMSUCH1                               'Init Suche nach Nummern
5690   GOTO 5870
5700 '
5710 GOSUB 14060                                           'F-Tasten ausblenden
5720 MTGLZAHL=0                                          'Init Suche nach Maske
5730 GOSUB 6730                                        'Nächsten Treffer suchen
5740 IF MTGLZAHL<>-1 THEN 5870
5750   N$="Kein Suchtreffer. Mit <F2> neue Suche.":GOSUB 13480       'Nachricht
5760   FKEYS$="":GOSUB 13800                               'F-Tasten einblenden
5770   FKEYMASK$="1234":NKEYMASK$="":GOSUB 13960                  'Menü-Eingabe
5780   ON FKEY GOTO 5790,5330,2290,5850
5790   HELPID=G0110502:IF ISDRUCK THEN HELPID=G0130103          'F1 verarbeiten
5800   GOSUB 17930                                                  'Hilfe-Text
5810   GOSUB 13690                                                 'Basis-Maske
5820   GOSUB 11910       'Feldnamen, Suchfeldwerte mit Feldplatzhalter ausgeben
5830   GOTO 5750
5840 '
5850   GOSUB 14120:GOTO 5750              'F4 verarbeiten 'Zum Hauptmenü?-Frage
5860 '
5870 GOSUB 6490                                 'Suchtreffer lesen und anzeigen
5880 '
5890 FKEYS$="<F5>=Suchen rückwärts <F6>=Suchen vorwärts <F7>=Adressaufkleber" 
5900 GOSUB 13800                                           'F-Tasten einblenden
5910 NKEYMAX=0:FKEYMAX=7:GOSUB 13960                              'Menü-Eingabe
5920 ON FKEY GOTO 5950,6020,2290,6050,6090,6290,6560
5930 '
5940 ' *** <F1> Hilfe
5950 HELPID=G0110503:GOSUB 17930                                    'Hilfe-Text
5960 GOSUB 13690                                                   'Basis-Maske
5970 GOSUB 12040                                      'Mitgliedsnummer ausgeben
5980 GOSUB 11780             'Feldnamen, Feldwerte mit Feldplatzhalter ausgeben
5990 GOTO 5890
6000 '
6010 ' *** <F2> Zurück
6020 IF SEARCH=BYNUMS THEN 4730 ELSE 5330           'TODO: Werte gehen verloren
6030 '
6040 ' *** <F4> Hauptmenü
6050 GOSUB 14120                                          'Zum Hauptmenü?-Frage
6060 GOTO 5910
6070 '
6080 ' *** <F5> Suchen rückwärts
6090 GOSUB 14060                                           'F-Tasten ausblenden
6100 IF SEARCH=BYMASK THEN 6170
6110 '
6120 MTGLZAHL=MTGLZAHL-1                          'Suchen rückwärts nach Nummer
6130 IF MTGLZAHL>=NUMSUCH1 THEN 6250 
6140 MTGLZAHL=NUMSUCH1
6150 GOTO 6220
6160 '       
6170 TMP=MTGLZAHL                                  'Suchen rückwärts nach Maske
6180 GOSUB 6650                                      'Vorherigen Treffer suchen
6190 IF MTGLZAHL<>-1 THEN 6250
6200 MTGLZAHL=TMP                                      'Keinen Treffer gefunden
6210 '
6220 N$="Suchbeginn erreicht":GOSUB 13480       'Suchbeginn erreicht 'Nachricht
6230 GOTO 5890
6240 '
6250 GOSUB 6490                                 'Suchtreffer lesen und anzeigen
6260 GOTO 5890
6270 '
6280 ' *** <F6> Suchen vorwärts
6290 GOSUB 14060                                           'F-Tasten ausblenden
6300 IF SEARCH=BYMASK THEN 6370
6310 '
6320 MTGLZAHL=MTGLZAHL+1                           'Suchen vorwärts nach Nummer
6330 IF MTGLZAHL<=NUMSUCH2 THEN 6450
6340 MTGLZAHL=NUMSUCH2
6350 GOTO 6420
6360 '
6370 TMP=MTGLZAHL                                    'Suche vorwärts nach Maske
6380 GOSUB 6730                                        'Nächsten Treffer suchen
6390 IF MTGLZAHL<>-1 THEN 6450
6400 MTGLZAHL=TMP                                      'Keinen Treffer gefunden
6410 '
6420 N$="Suchende erreicht":GOSUB 13480           'Suchende erreicht 'Nachricht
6430 GOTO 5890
6440 '
6450 GOSUB 6490                                 'Suchtreffer lesen und anzeigen
6460 GOTO 5890
6470 '
6480 ' *** SUB Suchtreffer lesen und anzeigen
6490 GOSUB 13620                                             'Nachricht löschen
6500 GOSUB 12150                                               'Stammsatz lesen
6510 GOSUB 12040                                      'Mitgliedsnummer ausgeben
6520 GOSUB 11670                        'Feldwerte mit Feldplatzhalter ausgeben
6530 RETURN
6540 '
6550 ' *** <F7> Adressaufkleber drucken
6560 GOSUB 16410                                         'Drucker bereit?-Frage
6570 IF NOT ISJA THEN 6610
6580   N$="Adressaufkleber wird gedruckt...":GOSUB 13480             'Nachricht
6590   GOSUB 16920                                     'Adressaufkleber drucken
6600   GOSUB 13620                                           'Nachricht löschen
6610 GOTO 5890                                                    'Neue Eingabe
6620 '
6630 ' *** SUB Vorherigen Treffer suchen
6640 ' IN: MTGLZAHL OUT: MTGLZAHL oder -1 wenn kein Treffer
6650 MTGLZAHL=MTGLZAHL-1
6660 IF MTGLZAHL<1 THEN 6790
6670 GOSUB 6830                                                       'Treffer?
6680 IF NOT ISMATCH THEN 6650 
6690 RETURN
6700 '
6710 ' *** SUB Nächsten Treffer suchen
6720 ' IN: MTGLZAHL OUT: MTGLZAHL oder -1 wenn kein Treffer
6730 MTGLZAHL=MTGLZAHL+1
6740 IF MTGLZAHL>MTGLMAX THEN 6790
6750 GOSUB 6830                                                       'Treffer?
6760 IF NOT ISMATCH THEN 6730 
6770 RETURN
6780 '
6790 MTGLZAHL=-1
6800 RETURN
6810 '
6820 ' *** SUB Treffer?
6830 N$="Vergleiche Stammsatz"+STR$(MTGLZAHL)+"...":GOSUB 13480      'Nachricht
6840 GOSUB 12150                                               'Stammsatz lesen
6850 FOR K=1 TO FIELDMAX
6860   S$=SUCH$(K)
6870   IF LEN(S$)=0 THEN 7000
6880   F$=LEFT$(S$,1)
6890   IF F$<>"*" THEN 6930
6900     IF LEN(S$)=1 THEN 7000
6910     IF INSTR(DAT$(K),RIGHT$(S$,LEN(S$)-1))>0 THEN 7000
6920     GOTO 7040
6930   IF F$<>"?" THEN 6990
6940     P=2
6950     IF MID$(S$,P,1)="?" THEN P=P+1:GOTO 6950
6960     IF P=LEN(DAT$(K))+1 THEN 7000         
6970     IF INSTR(DAT$(K),RIGHT$(S$,LEN(S$)-P+1))=P THEN 7000
6980     GOTO 7040
6990   IF S$<>DAT$(K) THEN 7040 
7000 NEXT K
7010 ISMATCH=TRUE
7020 RETURN
7030 '
7040 ISMATCH=FALSE
7050 RETURN
7060 '
7070 ' ######################### Verwendungsnachweis ##########################
7080 '
7090 GOSUB 11500                             'Einsprung MVW-Menü 'MTGLMAX lesen
7100 VNFLAG=TRUE
7110 '
7120 ' Verwendungsnachweis-Menü ***********************************************
7130 '
7140 MT$="VERWENDUNGSNACHWEIS":MI$="G-012000":GOSUB 13690          'Basis-Maske
7150 F=4                                                                'TODO ?
7160 FKEYS$="":GOSUB 13800                                 'F-Tasten einblenden
7170 PRINT CURON$;
7180 PRINT HIGHOFF$;
7190 PRINT FNCUR$(4,23);"Bitte geben Sie die Mitgliedsnummer"
7200 PRINT FNCUR$(5,20);"des zu ändernden Verwendungsnachweises ein"
7210 'LW PRINT FNCUR$(6,23);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
7220 PRINT FNCUR$(6,20);" und beenden Sie die Eingabe mit RETURN."
7230 PRINT FNCUR$(8,38);"=>..."
7240 Z=8:S=39:LNG=3:TYP=2
7250 GOSUB 10360                                                  'Feld-Eingabe
7260 IF FKEY=1 THEN HELPID=G0120001:GOSUB 17930:GOTO 7140           'Hilfe-Text
7270 IF FKEY=2 OR FKEY=3 THEN GOSUB 11390:VNFLAG=FALSE:GOTO 2290 'DAT$() leeren
7280 IF FKEY=4 THEN GOSUB 14120                           'Zum Hauptmenü?-Frage
7290 IF FKEY>4 THEN 7240
7300 IF CKEY<>0 THEN 7240
7310 IF LEN(OUTTEXT$)=0 THEN N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 13480:GOTO 7230 ELSE MITGLIED=VAL(OUTTEXT$):IF MITGLIED=0 THEN 7250 'Nachricht
7320 IF MITGLIED>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 13480:PRINT HOME$;HIGHOFF$:GOTO 7230 'Nachricht
7330 MTGLZAHL=MITGLIED:MTGL$=STR$(MITGLIED)
7340 '
7350 ' *** Lesen des Mitgliedsnamens ******************************************
7360 '
7370 PRINT FNCUR$(21,1);DELLINE$;
7380 GET #2,MITGLIED                                           'DATEN STS lesen
7390 NNAM$=LEFT$(DD1$,25)                                 'NACHNAME RAUSFISCHEN
7400 VNAM$=MID$(DD1$,26,25)                               'VORNAME  RAUSFISCHEN
7410 '
7420 ' ** LEERZEICHEN HINTER NACHNAME ABSCHNEIDEN *****************************
7430 '
7440 FOR I=LEN(NNAM$) TO 1 STEP -1
7450   IF MID$(NNAM$,I,1)<>" " THEN NNAM$=LEFT$(NNAM$,I):I=1
7460 NEXT
7470 NACHVORNAME$=NNAM$+" "+VNAM$
7480 GOSUB 14640                                             '1. VN-BLATT lesen
7490 BLATT=1
7500 VP=1:HP=1                                                 'INITIALISIERUNG
7510 F=8
7520 GOSUB 13690                                                   'Basis-Maske
7530 FKEYS$="<F7>=Speichern":GOSUB 13800                   'F-Tasten einblenden
7540 PRINT HIGHOFF$;
7550 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=Vorwärts";TAB(53);"<F9>=Blatt drucken";
7560 PRINT FNCUR$(3,2);"Blatt:"
7570 PRINT FNCUR$(5,2);"Mitglieds-Nr.:"
7580 PRINT FNCUR$(5,24);"Name:"
7590 MSK$="Von:  .  .   Bis:  .  .   Amt:"
7600 FOR KK=7 TO 19 
7610   PRINT FNCUR$(KK,2);MSK$;TAB(43);MSK$
7620 NEXT
7630 GOSUB 15850                                                 'Ausgabe Daten
7640 LNG=2-(HP=7)*6                                    'BEGIN OF THE INPUT-LOOP
7650 TYP=2-(HP=7)
7660 Z=VP+13*(VP>13)+6
7670 S=6-39*(VP>13)-(HP<7)*(HP-1)*3+(HP>3)*(HP<7)*4-(HP=7)*26
7680 GOSUB 13620                                             'Nachricht löschen
7690 GOSUB 10360                                                  'Feld-Eingabe
7700 IF HP<7 THEN IF ASCC=ASCRET THEN OUTTEXT$=STRING$(2-LEN(OUTTEXT$),"0")+OUTTEXT$   '+NULLEN
7710 IF CKEY<>0 OR ASCC=ASCRET THEN PRINT FNCUR$(Z,S);OUTTEXT$
7720 IF ASCC=ASCRET THEN IF HP<7 THEN MID$(DAT$(VP),-1+2*HP,2)=OUTTEXT$ ELSE DAT$(VP)=LEFT$(DAT$(VP),12)+OUTTEXT$+SPACE$(8-LEN(OUTTEXT$))
7730 IF ASCC=ASCRET AND HP<7 THEN ISJA=TRUE:TEST$=OUTTEXT$:ON HP GOSUB 12880,12910,12940,12880,12910,12940:IF NOT ISJA THEN N$="Ungültiges Datum":GOSUB 13480:GOTO 7690 'Datums-Check 'Nachricht
7740 IF ASCC=ASCRET THEN IF HP=7 THEN HP=1:VP=VP-(VP<26)-(VP=26)*(-25):GOTO 7640 ELSE HP=HP+1:GOTO 7640     'NEUE SCREEN-POSITION
7750 IF CKEY<>0 THEN TEST$=MID$(DAT$(VP),-(HP<7)*2*HP-1-(HP=7)*14,-(HP<7)*2-(HP=7)*8):PRINT FNCUR$(Z,S);TEST$
7760 IF CKEY<>0 THEN IF HP<7 AND NOT TEST$="  " THEN ISJA=TRUE:ON HP GOSUB 12880,12910,12940,12880,12910,12940:IF NOT ISJA THEN N$="Ungültiges Datum":GOSUB 13480:GOTO 7690 'Nachricht
7770 IF CKEY=CKEYDN THEN VP=VP-(VP<26)-(VP=26)*(-25):GOTO 7640
7780 IF CKEY=CKEYUP THEN VP=VP+(VP>1)-(VP=1)*25     :GOTO 7640
7790 IF CKEY=CKEYRT THEN HP=HP-(HP<7)-(HP=7)*(-6)   :GOTO 7640
7800 IF CKEY=CKEYLT THEN HP=HP+(HP>1)+(HP=1)*(-6)   :GOTO 7640
7810 IF FKEY=1 THEN HELPID=G0120002:GOSUB 17930:GOTO 7500           'Hilfe-Text
7820 IF FKEY=2 THEN 7140                                       'ZU MTGL-EINGABE
7830 IF FKEY=3 THEN VNFLAG=FALSE:GOSUB 11390:GOTO 2290           'DAT$() leeren
7840 IF FKEY=4 THEN GOSUB 14120:GOSUB 13860:GOTO 7690     'Zum Hauptmenü?-Frage
7850 IF FKEY=5 THEN GOSUB 14730:GOTO 7640
7860 IF FKEY=6 THEN GOSUB 14870:GOTO 7640
7870 IF FKEY=7 THEN GOSUB 15510:GOTO 7690                 'SPEICHERN DANN INPUT
7880 IF FKEY=9 THEN GOSUB 16090:GOSUB 15060:GOTO 7680
7890 '
7900 ' ############# Mitgliedsliste drucken von 05*************** #############
7910 '
7920 MT$="MITGLIEDSLISTE DRUCKEN":MI$="G-013000":GOSUB 13690       'Basis-Maske
7930 FKEYS$="":GOSUB 13800                                 'F-Tasten einblenden
7940 PRINT FNCUR$(4,30);"1 - Suchen & Drucken"
7950 PRINT FNCUR$(6,30);"2 - Komplett drucken"
7960 PRINT FNCUR$(8,30);"3 - Datenblatt drucken"
7970 NKEYMAX=3:FKEYMAX=4:GOSUB 13960                              'Menü-Eingabe
7980 IF NKEY THEN ON NKEY GOTO 8050,8050,8810
7990 ON FKEY GOTO 8000,2290,2290,8010
8000 HELPID=G0130000:GOSUB 17930:GOTO 7920                          'Hilfe-Text
8010 GOSUB 14120:GOTO 7970                                'Zum Hauptmenü?-Frage
8020 '
8030 ' *** Suchen & Drucken, Komplett drucken *********************************
8040 '
8050 IF NKEY=1 THEN MT$="SUCHEN & DRUCKEN":MI$="G-013010"
8060 IF NKEY=2 THEN MT$="KOMPLETT DRUCKEN":MI$="G-013020"
8070 GOSUB 13690                                                   'Basis-Maske
8080 GOSUB 13800                                           'F-Tasten einblenden
8090 PRINT FNCUR$(4,22);"Bitte geben Sie das heutige Datum ein."
8100 PRINT FNCUR$(5,23);"Beenden Sie die Eingabe mit RETURN."
8110 PRINT FNCUR$(7,36);"=>........"
8120 ISINPUTOK=TRUE
8130 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
8140 Z=7:S=38:LNG=8:TYP=4:ISMUSS=TRUE
8150 GOSUB 10370                                                'Feld-Eingabe+1
8160 IF FKEY<>1 THEN 8210       'F-Tasten verarbeiten 'TODO: Wert geht verloren
8170   IF NKEY=1 THEN HELPID=G0130101
8180   IF NKEY=2 THEN HELPID=G0130200
8190   GOSUB 17930                                                  'Hilfe-Text
8200   GOTO 8050
8210 IF FKEY=2 THEN 7920
8220 IF FKEY=3 THEN 2290
8230 IF FKEY=4 THEN GOSUB 14120:GOTO 8150                 'Zum Hauptmenü?-Frage
8240 '
8250 D$=OUTTEXT$:GOSUB 13020                    'Test Datum 'RETURN verarbeiten
8260 IF NOT ISINPUTOK THEN N$=DATUM$:GOSUB 13480:GOTO 8140           'Nachricht
8270 PRINT FNCUR$(7,37);HIGHON$;DATUM$;HIGHOFF$
8280 '
8290 ISDRUCK=TRUE 
8300 IF NKEY=2 THEN 8420
8310 '
8320 ' *** Suchen nach Nummern und Maske
8330 GOTO 4530                                  'Stammsatz suchen (und drucken)
8340 ' *** Rückkehr aus "Stammsatz suchen"
8350 ' IN: SEARCH (BYNUMS, BYMASK), NUMSUCH1, NUMSUCH2, SUCH$()
8360 GOSUB 16410                                         'Drucker bereit?-Frage
8370 IF ISJA THEN 8490
8380 GOSUB 13800                                           'F-Tasten einblenden
8390 IF SEARCH=BYNUMS THEN 4920 ELSE 5420 
8400 '
8410 ' *** Komplette Mitgliedsliste drucken
8420 SEARCH=BYNUMS
8430 ISCOMPLETE=TRUE
8440 NUMSUCH1=1:NUMSUCH2=MTGLMAX
8450 GOSUB 16410                                         'Drucker bereit?-Frage
8460 IF NOT ISJA THEN 8150
8470 '
8480 ' *** Drucken
8490 N$="Mitgliedsliste wird gedruckt... Abbrechen mit Taste.":GOSUB 13480 'Nachricht
8500 GOSUB 16580                                 'Mitgliedsliste drucken (Kopf)
8510 IF SEARCH=BYMASK THEN 8640
8520 '
8530 ' *** Suchen nach Nummern
8540 FOR MTGLZAHL=NUMSUCH1 TO NUMSUCH2
8550   GOSUB 12150                                             'Stammsatz lesen
8560   IF INSTR(DAT$(5),"ausgetreten")>0 THEN 8580
8570     GOSUB 16720                         'Mitgliedsliste drucken (Mitglied)
8580   GOSUB 16480                                      'Druck abbrechen?-Frage
8590   IF ISJA THEN 8750
8600 NEXT MTGLZAHL
8610 GOTO 8750
8620 ' 
8630 ' *** Suchen nach Maske
8640 MTGLZAHL=0
8650 GOSUB 6730                                        'Nächsten Treffer suchen
8660 IF MTGLZAHL=-1 THEN 8750
8670   GOSUB 12150                                             'Stammsatz lesen
8680   IF INSTR(DAT$(5),"ausgetreten")>0 THEN 8700
8690     GOSUB 16720                         'Mitgliedsliste drucken (Mitglied)
8700   GOSUB 16480                                      'Druck abbrechen?-Frage
8710   IF ISJA THEN 8750
8720 GOTO 8650
8730 '
8740 ' *** Suchen zuende
8750 GOSUB 13620                                             'Nachricht löschen
8760 IF ISCOMPLETE THEN ISCOMPLETE=FALSE:GOTO 7920       'TODO: Very hackish...
8770 IF SEARCH=BYNUMS THEN 4720 ELSE 5330                'TODO: Very hackish...
8780 '
8790 ' *** Datenblatt drucken *************************************************
8800 '
8810 MT$="DATENBLATT DRUCKEN":MI$="G-013030":GOSUB 13690           'Basis-Maske
8820 FKEYS$="":GOSUB 13800                                 'F-Tasten einblenden
8830 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer"
8840 PRINT FNCUR$(5,23)"des zu druckenden Datenblattes ein."
8850 PRINT FNCUR$(6,22);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
8860 PRINT FNCUR$(7,23)"Beenden Sie die Eingabe mit RETURN."
8870 PRINT FNCUR$(10,38)"=>..."
8880 ISINPUTOK=TRUE
8890 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
8900 Z=10:S=40:LNG=3:TYP=2:ISMUSS=TRUE
8910 GOSUB 10370                                                'Feld-Eingabe+1
8920 IF FKEY=1 THEN HELPID=G0130300:GOSUB 17930:GOTO 8810 'Hilfe-Text 'TODO: Wert geht verloren
8930 IF FKEY=2 THEN 7920
8940 IF FKEY=3 THEN 2290
8950 IF FKEY=4 THEN GOSUB 14120:GOTO 8910                 'Zum Hauptmenü?-Frage
8960 '
8970 M=VAL(OUTTEXT$)                                        'RETURN verarbeiten
8980 IF M>=1 AND M<=MTGLMAX THEN 9020
8990   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 13480         'Nachricht
9000   ISINPUTOK=FALSE
9010   GOTO 8900
9020 MTGLZAHL=M
9030 GOSUB 16410                                         'Drucker bereit?-Frage
9040 IF ISJA THEN 9080
9050   GOSUB 13800                                         'F-Tasten einblenden
9060   INTEXT$=OUTTEXT$
9070   GOTO 8900
9080 N$="Datenblatt drucken...":GOSUB 13560     'Nachricht. F-Tasten ausblenden
9090 GOSUB 12150                                               'Stammsatz lesen
9100 VOLLNAME$ = DAT$(1)+" "+DAT$(2)
9110 GOSUB 17100                          'Stammsatz drucken (Bildschirmformat)
9120 GOSUB 14640                                     'Verwendungsnachweis lesen
9130 GOSUB 16120                                   'Verwendungsnachweis drucken
9140 GOSUB 17370                                     'Mitgliedsvermerke drucken
9150 GOSUB 13620                                             'Nachricht löschen
9160 GOTO 8810
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
10280 ' IN: S   - Spalte OUT: FKEY     TMP: ISVALID (TRUE, FALSE)
10290 ' IN: LNG - Länge  OUT: CKEY     TMP: ISINPUTOK (TRUE, FALSE)
10300 ' IN: TYP - Typ    OUT: ASCC
10310 ' IN: ISMUSS (TRUE, FALSE)
10320 ' IN: INTEXT$
10330 ' IN: FKEYMASK$
10340 ' IN: CKEYMASK$
10350 ' IN: VNFLAG
10360 ISINPUTOK=TRUE
10370 IF NOT ISINPUTOK THEN INTEXT$=OUTTEXT$
10380 P=LEN(INTEXT$)
10390 PRINT FNCUR$(Z,S);HIGHON$;INTEXT$;HIGHOFF$;STRING$(LNG-P,".")
10400 OUTTEXT$=INTEXT$
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
10530   OUTTEXT$=OUTTEXT$+A$
10540   P=P+1
10550   GOTO 10430
10560 IF ASCC<>ASCBSP THEN 10620                         'Backspace verarbeiten
10570   IF P<1 THEN 10440
10580   PRINT DEL$                        
10590   P=P-1
10600   OUTTEXT$=LEFT$(OUTTEXT$,P)
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
10720 N$="Ungültige Eingabe - Bitte wiederholen":GOSUB 13480         'Nachricht
10730 ISINPUTOK=FALSE
10740 GOTO 10430
10750 '
10760 IF NOT ISMUSS OR LEN(OUTTEXT$)>0 THEN 10800           'RETURN verarbeiten
10770   N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 13480
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
10910 FOR K=1 TO 30:A$=INKEY$:NEXT K
10920 RETURN
10930 '
10940 ' *** Stammsatzfelde-Definitionen lesen ********************************* 
10950 '
10960 RESTORE 11080
10970 FOR K=1 TO FIELDMAX
10980   READ NAMEN$,SPALTE,ZEILE,LAENGE,TYP
10990   FIELDNAM$(K)=NAMEN$
11000   IF MID$(NAMEN$,13,1)="=" THEN FIELDISMAND(K)=TRUE ELSE FIELDISMAND(K)=FALSE                           
11010   FIELDCOL(K)=SPALTE
11020   FIELDROW(K)=ZEILE
11030   FIELDLNG(K)=LAENGE
11040   FIELDTYP(K)=TYP 
11050 NEXT K
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
11370 ' *** DAT$() leeren *****************************************************
11380 '
11390 FOR K=1 TO FIELDMAX:DAT$(K)="":NEXT K
11400 RETURN
11410 '
11420 ' *** SUCH$() leeren ****************************************************
11430 '
11440 FOR K=1 TO FIELDMAX:SUCH$(K)="":NEXT K
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
11660 ' IN: FIELDMAX
11670 FOR K=1 TO FIELDMAX
11680   Z=FIELDROW(K)
11690   S=FIELDCOL(K)+COLSEP
11700   PRINT FNCUR$(Z,S);HIGHON$;DAT$(K);HIGHOFF$;
11710   PRINT STRING$(FIELDLNG(K)-LEN(DAT$(K)),".")
11720 NEXT K
11730 RETURN
11740 '
11750 ' *** Maske Feldnamen, Feldwerte mit Feldplatzhalter ausgeben ***********
11760 '
11770 ' IN: FIELDMAX
11780 FOR K=1 TO FIELDMAX
11790   Z=FIELDROW(K)
11800   S=FIELDCOL(K)
11810   PRINT FNCUR$(Z,S);FIELDNAM$(K)
11820   S=S+COLSEP
11830   PRINT FNCUR$(Z,S);HIGHON$;DAT$(K);HIGHOFF$;
11840   PRINT STRING$(FIELDLNG(K)-LEN(DAT$(K)),".")               
11850 NEXT K
11860 RETURN
11870 '
11880 ' *** Maske Feldnamen, Suchfeldwerte mit Feldplatzhalter ausgeben *******
11890 '
11900 ' IN: FIELDMAX
11910 FOR K=1 TO FIELDMAX
11920   Z=FIELDROW(K)
11930   S=FIELDCOL(K)
11940   PRINT FNCUR$(Z,S);FIELDNAM$(K)
11950   S=S+COLSEP
11960   PRINT FNCUR$(Z,S);HIGHON$;SUCH$(K);HIGHOFF$;
11970   PRINT STRING$(FIELDLNG(K)-LEN(SUCH$(K)),".")               
11980 NEXT K
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
12100 FIELD #2,236+14 AS XSTAMMSATZ1$,225+13 AS XSTAMMSATZ2$
12110 RETURN
12120 '
12130 ' *** Stammsatz von Disk lesen ******************************************
12140 '
12150 GET #2,MTGLZAHL                                             'LW Compress?
12160 P=1
12170 FOR K=1 TO 14
12180   LNG=FIELDLNG(K)+1
12190   TMP$=MID$(XSTAMMSATZ1$,P,LNG)
12200   L=ASC(LEFT$(TMP$,1))
12210   DAT$(K)=MID$(TMP$,2,L)
12220   P=P+LNG
12230 NEXT K
12240 P=1
12250 FOR K=15 TO FIELDMAX
12260   LNG=FIELDLNG(K)+1
12270   TMP$=MID$(XSTAMMSATZ2$,P,LNG)
12280   L=ASC(LEFT$(TMP$,1))
12290   DAT$(K)=MID$(TMP$,2,L)
12300   P=P+LNG
12310 NEXT K
12320 RETURN
12330 '
12340 ' *** Stammsatz auf Disk speichern **************************************
12350 '
12360 ' IN: MTGLZAHL
12370 TMP$=""                                                     'LW Compress?
12380 FOR K=1 TO 14
12390   LNG=FIELDLNG(K)
12400   L=LEN(DAT$(K))
12410   TMP$=TMP$+CHR$(L)+DAT$(K)+SPACE$(LNG-LEN(DAT$(K)))
12420 NEXT K
12430 LSET XSTAMMSATZ1$=TMP$
12440 TMP$=""
12450 FOR K=15 TO FIELDMAX
12460   LNG=FIELDLNG(K)
12470   L=LEN(DAT$(K))
12480   TMP$=TMP$+CHR$(L)+DAT$(K)+SPACE$(LNG-LEN(DAT$(K)))
12490 NEXT K
12500 LSET XSTAMMSATZ2$=TMP$
12510 PUT #2,MTGLZAHL
12520 RETURN
12530 '
12540 ' *** Ja/Nein-Frage und F-Tasten ausblenden *****************************
12550 '
12560 ' IN:  Q$   - Frage
12570 ' OUT: ISJA - (TRUE, FALSE)
12580 N$=Q$+" (J=Ja/N=Nein)":GOSUB 13480                             'Nachricht
12590 GOSUB 14060                                          'F-Tasten ausblenden
12600 ANSWER$=INPUT$(1)
12610 IF ANSWER$="J" OR ANSWER$="j" THEN ISJA=TRUE:RETURN
12620 IF ANSWER$="N" OR ANSWER$="n" THEN ISJA=FALSE:RETURN
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
12860 ' *** DATUMS-TEST (bei VN und M-Liste drucken) **************************
12870 '
12880 IF VAL(TEST$)=0 OR VAL(TEST$)>31 THEN ISJA=FALSE                     'Tag
12890 RETURN
12900 '
12910 IF VAL(TEST$)=0 OR VAL(TEST$)>12 THEN ISJA=FALSE                   'Monat
12920 RETURN
12930 '
12940 IF VAL(TEST$)=0 THEN ISJA=FALSE                                     'Jahr
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
13250 DATUM$="Datum muss Format TT.MM.JJ haben. Bitte korrigieren."
13260 GOTO 13290
13270 '
13280 DATUM$="Ungültiges Datum. Bitte korrigieren."
13290 ISINPUTOK=FALSE
13300 '
13310 RETURN
13320 '
13330 ' *** Voreinstellungen zuweisen in "Stammsatz anlegen" ******************
13340 '
13350 DAT$(5)="10***"
13360 DAT$(7)=""11**""
13370 DAT$(11)="12**"
13380 DAT$(12)="13******"
13390 DAT$(21)="3"
13400 DAT$(22)="Nein"
13410 DAT$(23)="Nein"
13420 DAT$(25)="Nein"
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
13530 ' *** Nachricht anzeigen und F-Tasten ausblenden ************************ 
13540 '
13550 ' IN: N$ - Nachricht
13560 GOSUB 13480                                                    'Nachricht
13570 GOSUB 14060                                          'F-Tasten ausblenden
13580 RETURN
13590 '
13600 ' *** Nachricht löschen *************************************************
13610 '
13620 N$="":GOSUB 13480                                              'Nachricht
13630 RETURN
13640 '
13650 ' *** Bildschirmmaske - Basis *******************************************
13660 '
13670 ' IN: MT$ - Menü-Titel
13680 ' IN: MI$ - Menu-ID
13690 PRINT CLS$;
13700 PRINT "00**********************";
13710 PRINT FNCUR$(1,(80-LEN(MT$))/2+1);MT$;FNCUR$(1,72);MI$
13720 PRINT STRICH$
13730 PRINT FNCUR$(21,1);
13740 PRINT STRICH$             'quirk in display driver places this in line 22
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
14010 N$="Sie haben eine falsche Taste gedrückt - Bitte neue Auswahl treffen.":GOSUB 13480 'Nachricht
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
14130 IF ISJA THEN 18230                                          'Programmende
14140 GOSUB 13800                                          'F-Tasten einblenden
14150 RETURN
14160 '
14170 ' *** 'Zum Hauptmenü?-Frage (einfach) ***********************************
14180 '
14190 ' OUT: ISJA (TRUE, FALSE)
14200 Q$="Wollen Sie wirklich ins Hauptmenü?":GOSUB 12580        'Ja/Nein-Frage
14210 GOSUB 13620                                            'Nachricht löschen
14220 RETURN
14230 '
14240 ' *** Rückkehr aus Hilfe-Text? ******************************************
14250 '
14260 N$="Zurück? (J=Ja)":GOSUB 13480                                'Nachricht
14270 GOSUB 12590                             'Ja/Nein-Frage ohne String-Koncat
14280 IF NOT ISJA THEN 14260
14290 RETURN
14300 '
14310 ' **** VN - Subroutines, permitting one page handling *******************
14320 ' ****         by 04********** 21*******              *******************
14330 '
14340 ' **** OPEN & FIELDING VERWENDUNGSNACHWEIS ******************************
14350 '
14360 OPEN "R",#3,"B:VN.NDX",4
14370 FIELD #3,4 AS POINT0$
14380 OPEN "R",#4,"B:VN.POI",8
14390 FIELD #4,4 AS POINT1$, 4 AS POINT2$
14400 OPEN "R",#5,"B:VN.DAT",560
14410 FIELD #5,240 AS DDD1$,240 AS DDD2$,80 AS DDD3$
14420 RETURN
14430 '
14440 ' *** VERWENDUNGSNACHWEIS FÜR NEUES MITGLIED ANLEGEN ********************
14450 '
14460 ' IN: MTGLZAHL
14470 GET #3,1                               'letzte Seitennummer lesen dann +1
14480 TMPPOINT=VAL(POINT0$)+1
14490 LSET DDD1$=""                                    'leeren Record speichern
14500 LSET DDD2$=""
14510 LSET DDD3$=""
14520 PUT #5,TMPPOINT
14530 LSET POINT0$=STR$(TMPPOINT)             'letzte Seitennummer neu speichern
14540 PUT #3,1
14550 PUT #3,MTGLZAHL+1
14560 LSET POINT1$="0000"
14570 LSET POINT2$="0000"
14580 PUT #4,TMPPOINT
14590 RETURN
14600 '
14610 ' **** ERSTE SEITE FINDEN & LESEN ***************************************
14620 '
14630 ' IN: MTGLZAHL
14640 GET #3,MTGLZAHL+1                                            'Index-Datei
14650 CPOINT=VAL(POINT0$)
14660 GET #5,CPOINT:GOSUB 15690                                    'Daten lesen
14670 BLATT=1
14680 GET #4,CPOINT                                              'Pointer lesen
14690 RETURN
14700 '
14710 ' **** SEITE RÜCKWÄRTS **************************************************
14720 '
14730 TMPPOINT=VAL(POINT1$)
14740 IF TMPPOINT<>0 THEN 14770
14750 N$="Erste Seite":GOSUB 13480                                   'Nachricht
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
14870 TMPPOINT=VAL(POINT2$)
14880 IF TMPPOINT=0 THEN 14950
14890 CPOINT=TMPPOINT
14900 BLATT=BLATT+1
14910 GET #5,CPOINT:GOSUB 15690
14920 GET #4,CPOINT:GOSUB 15850
14930 HP=1:VP=1
14940 RETURN
14950 N$="Letzte Seite"
14960 IF INSTR(SPACE$(20),DAT$(26))=0 THEN 14990 
14970 GOSUB 13480                                                    'Nachricht
14980 RETURN
14990 Q$="Letzte Seite - Neue Seite anlegen?":GOSUB 12580        'Ja/Nein-Frage
15000 GOSUB 13620                                            'Nachricht löschen
15010 IF NOT ISJA THEN 15060
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
15150 LSET POINT0$=STR$(VAL(POINT0$)+1)                                  'NDX+1
15160 PUT #3,1
15170 LSET POINT1$="0000"                                        'Page pointers
15180 LSET POINT2$="0000"
15190 PUT #4,VAL(POINT0$)
15200 LSET POINT0$=POINT0$                                          'First page
15210 PUT #3,MTGLZAHL+1
15220 LSET DDD1$=""                                                 'Datenblatt
15230 LSET DDD2$=""
15240 LSET DDD3$=""
15250 PUT #5,VAL(POINT0$)
15260 RETURN
15270 '
15280 ' **** ALLGEMEIN EINE NEUE SEITE ANLEGEN ********************************
15290 '
15300 GET #3,1                                                           'NDX+1
15310 LSET POINT0$=STR$(VAL(POINT0$)+1)
15320 PUT #3,1:TMPPOINT$=POINT0$    'Vektoren der bisher letzten Seite verbiegen
15330 LSET POINT2$=POINT0$                                'Verbiege Vektoren...
15340 PUT #4,CPOINT                                         '...der alten Seite
15350 LSET POINT1$=STR$(CPOINT)                             '...der neuen Seite
15360 LSET POINT2$="0000"                                                '.. ""
15370 CPOINT=VAL(TMPPOINT$)                                       'Nr neue Seite
15380 PUT #4,CPOINT
15390 LSET DDD1$=""                                           'Write empty page
15400 LSET DDD2$=""
15410 LSET DDD3$="" 
15420 PUT #5,CPOINT
15430 DATEN$=SPACE$(20)
15440 FOR K=1 TO 26
15450   DAT$(K)=DATEN$
15460 NEXT K
15470 RETURN
15480 '
15490 ' **** SEITE SPEICHERN **************************************************
15500 '
15510 DATEN$=""
15520 FOR K=1 TO 12
15530   DATEN$=DATEN$+DAT$(K)
15540 NEXT K
15550 LSET DDD1$=DATEN$:DATEN$=""
15560 FOR K=13 TO 24
15570   DATEN$=DATEN$+DAT$(K)
15580 NEXT K
15590 LSET DDD2$=DATEN$:DATEN$=""
15600 FOR K=25 TO 26
15610   DATEN$=DATEN$+DAT$(K)
15620 NEXT K
15630 LSET DDD3$=DATEN$:DATEN$=""
15640 PUT #5,CPOINT
15650 RETURN
15660 '
15670 ' **** HACK ! (ZERSTÜCKELN DER DATEN-STRINGS) ***************************
15680 '
15690 FOR K=1 TO 12
15700   DAT$(II)=MID$(DDD1$,(II-1)*20+1,20)
15710 NEXT K
15720 FOR K=13 TO 24
15730   DAT$(II)=MID$(DDD2$,(II-13)*20+1,20)
15740 NEXT K
15750 FOR K=25 TO 26
15760   DAT$(II)=MID$(DDD3$,(II-25)*20+1,20)
15770 NEXT K
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
15900 FOR K=1 TO 26
15910   HOR=6-39*(K>13)
15920   VER=6+K+13*(K>13)
15930   FOR J=1 TO 3                                                 'Von-Datum
15940     PRINT FNCUR$(VER,HOR+(J-1)*3);MID$(DAT$(K),-1+J*2,2)
15950   NEXT
15960   FOR J=1 TO 3                                                 'Bis-Datum
15970     PRINT FNCUR$(VER,HOR+13+(J-1)*3);MID$(DAT$(K),5+J*2,2)
15980   NEXT
15990   PRINT FNCUR$(VER,HOR+26);MID$(DAT$(K),13,8)                        'Amt
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
16100 IF NOT ISJA THEN 16340                              'LW TODO Remove this!
16110 '
16120 RETURN                                         '*** TMP Print Disable ***
16130 I$=SPACE$(2)
16140 '
16150 PRINT STRICH$
16160 PRINT
16170 PRINT I$;"Blatt:";BLATT
16180 PRINT
16190 PRINT I$;"Mitglieds-Nr.:";MTGLZAHL;TAB(24);"Name:";NACHVORNAME$
16200 PRINT
16210 FOR K=1 TO 13
16220   KK=K:GOSUB 16310
16230   PRINT I$;"Von:";VON$;" Bis:";BIS$;" Amt:";AMT$;
16240   KK=K+13:GOSUB 16310
16250   PRINT " Von:";VON$;" Bis:";BIS$;" Amt:";AMT$
16260 NEXT 
16270 PRINT
16280 PRINT STRICH$
16290 RETURN
16300 'Format: VVVVVV BBBBBB AAAAAAAA
16310 VON$=LEFT$(DAT$(KK),2)+"."+MID$(DAT$(KK),3,2)+"."+MID$(DAT$(KK),5,2)
16320 BIS$=MID$(DAT$(KK),7,2)+"."+MID$(DAT$(KK),9,2)+"."+MID$(DAT$(KK),11,2)
16330 AMT$=RIGHT$(DAT$(KK),8)
16340 RETURN
16350 '
16360 ' ### Unterprogramme "Mitgliedsliste drucken" ###########################
16370 '
16380 ' *** Drucker bereit?-Frage *********************************************
16390 '
16400 ' OUT: ISJA (TRUE, FALSE)
16410 Q$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 12580 'Ja/Nein-Frage
16420 GOSUB 13620                                            'Nachricht löschen
16430 RETURN
16440 '
16450 ' *** Druck abbrechen?-Frage ********************************************
16460 '
16470 ' OUT: ISJA (TRUE, FALSE)
16480 ISJA=FALSE 
16490 IF INKEY$="" THEN 16530
16500 Q$="Drucken abbrechen?":GOSUB 12580                        'Ja/Nein-Frage
16510 IF ISJA THEN 16530
16520 N$="Mitgliedsliste wird gedruckt... Abbrechen mit Taste.":GOSUB 13480 'Nachricht
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
16730 IN=INSTR(DAT$(6),"22**"):IF IN>0 THEN AMT$=MID$(DAT$(6),IN,6):GOTO 16770
16740 IN=INSTR(DAT$(6),"23**"):IF IN>0 THEN AMT$=MID$(DAT$(6),IN,5):GOTO 16770
16750 IN=INSTR(DAT$(6),"24**"):IF IN>0 THEN AMT$=MID$(DAT$(6),IN,3)
16760 '
16770 PRINT
16780 PRINT TAB(5);MTGLZAHL;
16790 PRINT TAB(10);DAT$(1);                                          'Nachname
16800 PRINT TAB(36);DAT$(2);                                           'Vorname
16810 PRINT TAB(65);"(";LEFT$(DAT$(5),1);")";                           'Status
16820 PRINT TAB(71);AMT$                                                   'Amt
16830 PRINT TAB(36);DAT$(10)                                            'Straße
16840 PRINT TAB(36);DAT$(11);                                     'Postleitzahl
16850 PRINT TAB(41);DAT$(12)                                               'Ort
16860 PRINT TAB(36);"Tel.:";DAT$(13)                                   'Telefon
16870 PRINT
16880 RETURN
16890 '
16900 ' *** Adressaufkleber drucken *******************************************
16910 '
16920 I$=SPACE$(3)
16930 '
16940 PRINT I$;DAT$(7)                                                  'Anrede
16950 PRINT I$;DAT$(2);" ";DAT$(1)                            'Vorname Nachname
16960 PRINT
16970 PRINT I$;DAT$(10)                                      'Straße Hausnummer
16980 PRINT
16990 PRINT I$;DAT$(11);" ";DAT$(12)                          'Postleitzahl Ort
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
17130 PRINT STRICH$
17140 PRINT
17150 PRINT TAB(31);"Mitgliedsnummer";MTGLZAHL
17160 PRINT
17170 RESTORE 17230
17180 FOR K=1 TO 7:READ A,B
17190   IF A<>0 THEN PRINT TAB(TMP1);FIELDNAM$(A);DAT$(A);
17200   IF B<>0 THEN PRINT TAB(TMP2);FIELDNAM$(B);DAT$(B);
17210   PRINT
17220 NEXT K
17230 DATA 1,0,2,15,3,16,4,0,5,17,6,18,0,19
17240 FOR K=7 TO 14
17250   PRINT TAB(TMP1);FIELDNAM$(K);DAT$(K);
17260   PRINT TAB(TMP2);FIELDNAM$(K+13);DAT$(K+13);
17270 NEXT K
17280 PRINT
17290 PRINT
17300 PRINT
17310 PRINT STRICH$
17320 RETURN
17330 '
17340 ' *** Mitgliedsvermerke drucken
17350 '
17360 ' IN: VOLLNAME$
17370 I$=SPACE$(9)
17380 FORM1$="___"
17390 FORM2$="___ / ___"
17400 FORM3$="___ / ___ / ___"
17410 '
17420 PRINT TAB(10);"Mitgliedsnr.: ";FORM1$;
17430 PRINT TAB(28);"Name: ";VOLLNAME$;
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
17790   FOR K=1 TO HELPIDMAX
17800     GET #6,K
17810     HELPFIRSTLINE(K)=CVI(XFIRSTLINE$)
17820     HELPLASTLINE(K)=CVI(XLASTLINE$)
17830   NEXT K
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
17970 FOR K=FIRSTLINE TO LASTLINE
17980   GET #6,K
17990   INDENT=CVI(XINDENT$)
18000	LENGTH=CVI(XLENGTH$)
18010   PRINT TAB(INDENT);LEFT$(XLINE$,LENGTH)
18020 NEXT K
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
