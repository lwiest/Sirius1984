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
1110 FELDMAX=27                                         'Anzahl Stammsatzfelder
1120 DIM FENAM$(FELDMAX)                                       'Name des Feldes
1130 DIM FEISMUSS(FELDMAX)                               'Muss-Feld (TRUE, FALSE)
1140 DIM FEROW(FELDMAX)                           'Zeile des Feldnamens (1..25)
1150 DIM FECOL(FELDMAX)                          'Spalte des Feldnamens (1..80)
1160 DIM FELNG(FELDMAX)                                       'Länge des Feldes
1170 DIM FETYP(FELDMAX)                                         'Typ des Feldes
1180 '
1190 DIM DAT$(FELDMAX)                                      'Stammsatzfelddaten
1200 DIM SUCH$(FELDMAX)                                     'Stammsatzsuchdaten
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
1970 ' F        - Feldindex 1..27 (FELDMAX)
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
2080 ON ERROR GOTO 17870                               'Programmfehler abfangen
2090 '
2100 GOSUB 10050    'Tasten deaktivieren, ASCII-Codes für Cursortasten vergeben
2110 PRINT CUROFF$;
2120 '
2130 ' *** Beginn des Mitgliederverwaltungsprogramms **************************
2140 '
2150 MT$="MITGLIEDERVERWALTUNG":MI$="G-010000":GOSUB 13460         'Basis-Maske
2160 Q$="Bitte Daten-Diskette in Laufwerk B einlegen. Bereit?":GOSUB 12710 'Ja/Nein-Frage
2170 IF NOT ISJA THEN 18000                                       'Programmende
2180 '
2190 N$="Einen Moment bitte...":GOSUB 13250                          'Nachricht
2200 GOSUB 11500                                                 'MTGLMAX lesen
2210 GOSUB 10960                              'Stammsatzfeld-Definitionen lesen
2220 GOSUB 12220                                   'Fielden der Stammsatz-Datei
2230 GOSUB 14130                       'Fielden der Verwendungsnachweis-Dateien
2240 GOSUB 17540      'Fielden der Hilfe-Text-Dateien, lesen der Zeilenbereiche
2250 GOSUB 13390                                             'Nachricht löschen
2260 '
2270 ' ###################### Mitgliederverwaltungs-Menü ######################
2280 '
2290 MT$="MITGLIEDERVERWALTUNG":MI$="G-010000":GOSUB 13460         'Basis-Maske
2300 FKEYS$="":GOSUB 13570                                 'F-Tasten einblenden
2310 PRINT FNCUR$(4,28);"1 - Stammsatzpflege"
2320 PRINT FNCUR$(6,28);"2 - Verwendungsnachweis"
2330 PRINT FNCUR$(8,28);"3 - Mitgliedsliste drucken"
2340 FKEYMAX=4:NKEYMAX=3:GOSUB 13730                              'Menü-Eingabe
2350 IF FKEY THEN ON FKEY GOTO 2370,2380,2290,2380
2360 ON NKEY GOTO 2440,7070,7900
2370 HELPID=G0100000:GOSUB 17700:GOTO 2290                          'Hilfe-Text
2380 GOSUB 13890:GOTO 2340                                'Zum Hauptmenü?-Frage
2390 '
2400 ' ########################### Stammsatzpflege ############################
2410 '
2420 ' *** Stammsatzpflege-Menü ***********************************************
2430 '
2440 MT$="STAMMSATZPFLEGE":MI$="G-011000":GOSUB 13460              'Basis-Maske
2450 FKEYS$="":GOSUB 13570                                 'F-Tasten einblenden
2460 PRINT FNCUR$(4,30);"1 - Stammsatz anlegen"
2470 PRINT FNCUR$(6,30);"2 - Stammsatz ändern"
2480 PRINT FNCUR$(8,30);"3 - Stammsatz löschen"
2490 PRINT FNCUR$(10,30);"4 - Stammsatz drucken"
2500 PRINT FNCUR$(12,30);"5 - Stammsatz suchen"
2510 FKEYMAX=4:NKEYMAX=5:GOSUB 13730                              'Menü-Eingabe
2520 IF FKEY THEN ON FKEY GOTO 2540,2290,2290,2550
2530 ON NKEY GOTO 2590,3080,3740,4170,4530
2540 HELPID=G0110000:GOSUB 17700:GOTO 2440                          'Hilfe-Text
2550 GOSUB 13890:GOTO 2510                                'Zum Hauptmenü?-Frage
2560 '
2570 ' *** Stammsatz anlegen von 02************ *******************************
2580 '
2590 MTGLZAHL=MTGLMAX+1
2600 F=1
2610 GOSUB 11390                                                 'DAT$() leeren
2620 GOSUB 13120                                     'Voreinstellungen zuweisen
2630 MT$="STAMMSATZ ANLEGEN":MI$="G-011010":GOSUB 13460            'Basis-Maske
2640 FKEYS$="<F7>=Speichern":GOSUB 13570                   'F-Tasten einblenden
2650 GOSUB 12170                                      'Mitgliedsnummer ausgeben
2660 GOSUB 11910             'Feldnamen, Feldwerte mit Feldplatzhalter ausgeben
2670 Z=FEROW(F):S=FECOL(F)+COLSEP:LNG=FELNG(F):TYP=FETYP(F):ISMUSS=FEISMUSS(F)
2680 FKEYMASK$="12347":CKEYMASK$="12":INTEXT$=DAT$(F)
2690 GOSUB 10360                                                  'Feld-Eingabe
2700 '
2710 IF ASCC<>ASCRET THEN 2750                              'RETURN verarbeiten
2720   DAT$(F)=OUTTEXT$
2730   F=F+1:IF F>FELDMAX THEN F=1
2740   GOTO 2670
2750 IF CKEY=0 THEN 2800                              'Cursortasten verarbeiten
2760   PRINT FNCUR$(Z,S);HIGHON$;DAT$(F);HIGHOFF$;STRING$(LNG-LEN(DAT$(F)),".")
2770   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FELDMAX
2780   IF CKEY=CKEYDN THEN F=F+1:IF F>FELDMAX THEN F=1
2790   GOTO 2670
2800 IF FKEY=1 THEN HELPID=G0110100:GOSUB 17700:GOTO 2630           'Hilfe-Text
2810 IF FKEY=2 THEN GOSUB 2910:GOTO 2440              'Exit "Stammsatz anlegen"
2820 IF FKEY=3 THEN GOSUB 2910:GOTO 2290              'Exit "Stammsatz anlegen"
2830 IF FKEY<>4 THEN 2880
2840   GOSUB 13970                              'Zum Hauptmenü?-Frage (einfach)
2850   IF NOT ISJA THEN GOSUB 13570:GOTO 2690              'F-Tasten einblenden
2860   GOSUB 2910                                     'Exit "Stammsatz anlegen"
2870   GOTO 18000                                                 'Programmende
2880 IF FKEY=7 THEN GOSUB 2980:GOTO 2590            'Exit "Stammsatz anlegen" 2
2890 '
2900 ' *** Exit "Stammsatz anlegen"
2910 GOSUB 10910                                           'Tastatur entprellen
2920 Q$="Wollen Sie vorher den Stammsatz speichern?":GOSUB 12710 'Ja/Nein-Frage
2930 IF ISJA THEN 2980
2940   MTGLZAHL=MTGLZAHL-1
2950   GOTO 3030
2960 '                                                            
2970 ' *** Exit "Stammsatz anlegen" 2
2980 N$="Stammsatz wird gespeichert...":GOSUB 13330 'Nachricht. F-Tasten ausblenden
2990 GOSUB 12500                                           'Stammsatz speichern
3000 GOSUB 14240                       'Erste Verwendungsnachweis-Seite anlegen
3010 MTGLMAX=MTGLZAHL:GOSUB 11590                             'MTGLMAX speichen
3020 '
3030 GOSUB 13390                                             'Nachricht löschen
3040 RETURN
3050 ' 
3060 ' *** Stammsatz ändern von 03************ ********************************
3070 '
3080 MT$="STAMMSATZ ÄNDERN":MI$="G-011020":GOSUB 13460             'Basis-Maske
3090 FKEYS$="":GOSUB 13570                                 'F-Tasten einblenden
3100 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer" 
3110 PRINT FNCUR$(5,24)"des zu ändernden Stammsatzes ein."
3120 PRINT FNCUR$(6,22);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
3130 PRINT FNCUR$(7,23)"Beenden Sie die Eingabe mit RETURN."
3140 PRINT FNCUR$(9,38)"=>..."
3150 ISBADINPUT=FALSE
3160 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
3170 Z=9:S=40:LNG=3:TYP=2:ISMUSS=TRUE
3180 GOSUB 10370                                                'Feld-Eingabe+1
3190 IF FKEY=1 THEN HELPID=G0110201:GOSUB 17700:GOTO 3080 'Hilfe-Text 'TODO: Wert geht verloren
3200 IF FKEY=2 THEN 2440
3210 IF FKEY=3 THEN 2290
3220 IF FKEY=4 THEN GOSUB 13890:GOTO 3180                 'Zum Hauptmenü?-Frage
3230 '
3240 M=VAL(OUTTEXT$)                                        'RETURN verarbeiten
3250 IF M>=1 AND M<=MTGLMAX THEN 3290
3260   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 13250         'Nachricht
3270   ISBADINPUT=TRUE
3280   GOTO 3170
3290 MTGLZAHL=M
3300 N$="Stammsatz wird gelesen...":GOSUB 13330 'Nachricht. F-Tasten ausblenden
3310 GOSUB 12280                                               'Stammsatz lesen
3320 GOSUB 13390                                             'Nachricht löschen
3330 '
3340 GOSUB 13460                                                   'Basis-Maske
3350 FKEYS$="<F7>=Speichern":GOSUB 13570                   'F-Tasten einblenden
3360 GOSUB 12170                                      'Mitgliedsnummer ausgeben
3370 GOSUB 11910             'Feldnamen, Feldwerte mit Feldplatzhalter ausgeben
3380 F=1
3390 Z=FEROW(F):S=FECOL(F)+COLSEP:LNG=FELNG(F):TYP=FETYP(F):ISMUSS=FEISMUSS(F)
3400 FKEYMASK$="12347":CKEYMASK$="12":INTEXT$=DAT$(F)
3410 GOSUB 10360                                                  'Feld-Eingabe
3420 '
3430 IF ASCC<>ASCRET THEN 3470                              'RETURN verarbeiten
3440   DAT$(F)=OUTTEXT$
3450   F=F+1:IF F>FELDMAX THEN F=1
3460   GOTO 3390
3470 IF CKEY=0 THEN 3520                              'Cursortasten verarbeiten
3480   PRINT FNCUR$(Z,S);HIGHON$;DAT$(F);HIGHOFF$;STRING$(LNG-LEN(DAT$(F)),".")
3490   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FELDMAX
3500   IF CKEY=CKEYDN THEN F=F+1:IF F>FELDMAX THEN F=1
3510   GOTO 3390
3520 IF FKEY=1 THEN HELPID=G0110202:GOSUB 17700:GOTO 3340           'Hilfe-Text
3530 IF FKEY=2 THEN GOSUB 3630:GOTO 3080               'Exit "Stammsatz ändern"
3540 IF FKEY=3 THEN GOSUB 3630:GOTO 2290               'Exit "Stammsatz ändern"
3550 IF FKEY<>4 THEN 3600 
3560   GOSUB 13970                              'Zum Hauptmenü?-Frage (einfach)
3570   IF NOT ISJA THEN GOSUB 13570:GOTO 3410              'F-Tasten einblenden
3580   GOSUB 3630                                      'Exit "Stammsatz ändern"
3590   GOTO 18000                                                 'Programmende
3600 IF FKEY=7 THEN GOSUB 3670:GOTO 3080             'Exit "Stammsatz ändern" 2
3610 '
3620 ' *** Exit "Stammsatz ändern"
3630 Q$="Wollen Sie vorher den Stammsatz speichern?":GOSUB 12710 'Ja/Nein-Frage
3640 IF NOT ISJA THEN RETURN
3650 '
3660 ' *** Exit "Stammsatz ändern" 2
3670 N$="Stammsatz wird gespeichert...":GOSUB 13330 'Nachricht. F-Tasten ausblenden
3680 GOSUB 12500                                           'Stammsatz speichern
3690 GOSUB 13390                                             'Nachricht löschen
3700 RETURN
3710 '
3720 ' *** Stammsatz löschen von 04********** *********************************
3730 '
3740 MT$="STAMMSATZ LÖSCHEN":MI$="G-011030":GOSUB 13460            'Basis-Maske
3750 FKEYS$="":GOSUB 13570                                 'F-Tasten einblenden
3760 PRINT FNCUR$(4,23);"Bitte geben Sie die Mitgliedsnummer"         'Menütext
3770 PRINT FNCUR$(5,24);"des zu löschenden Mitglieds ein."
3780 PRINT FNCUR$(6,22);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
3790 PRINT FNCUR$(7,15);"(Dabei wird nur der Status des Mitglieds verändert.)"
3800 PRINT FNCUR$(8,23);"Beenden Sie die Eingabe mit RETURN."
3810 PRINT FNCUR$(10,38);"=>..."
3820 ISBADINPUT=FALSE
3830 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
3840 Z=10:S=40:LNG=3:TYP=2:ISMUSS=TRUE
3850 GOSUB 10370                                                'Feld-Eingabe+1
3860 IF FKEY=1 THEN HELPID=G0110300:GOSUB 17700:GOTO 3740 'Hilfe-Text 'TODO: Wert geht verloren
3870 IF FKEY=2 THEN 2440
3880 IF FKEY=3 THEN 2290
3890 IF FKEY=4 THEN GOSUB 13890:GOTO 3850                 'Zum Hauptmenü?-Frage
3900 '
3910 M=VAL(OUTTEXT$)                                        'RETURN verarbeiten
3920 IF M>=1 AND M<=MTGLMAX THEN 3960
3930   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 13250         'Nachricht
3940   ISBADINPUT=TRUE
3950   GOTO 3840
3960 MTGLZAHL=M
3970 N$="Stammsatz wird gelesen...":GOSUB 13330 'Nachricht. F-Tasten ausblenden
3980 GOSUB 12280                                               'Stammsatz lesen
3990 GOSUB 13390                                             'Nachricht löschen
4000 '
4010 GOSUB 13460                                                   'Basis-Maske
4020 GOSUB 12170                                      'Mitgliedsnummer ausgeben
4030 GOSUB 11910             'Feldnamen, Feldwerte mit Feldplatzhalter ausgeben
4040 Q$="Mitglied endgültig löschen?":GOSUB 12710                'Ja/Nein-Frage
4050 IF NOT ISJA THEN 4130
4060   F=5
4070   DAT$(F)="ausgetreten"
4080   Z=FEROW(F):S=FECOL(F)+COLSEP:LNG=FELNG(F)
4090   PRINT FNCUR$(Z,S);HIGHON$;DAT$(F);HIGHOFF$;STRING$(LNG-LEN(DAT$(F)),".")               
4100   N$="Stammsatz wird gespeichert...":GOSUB 13330 'Nachricht. F-Tasten ausblenden
4110   GOSUB 12500                                         'Stammsatz speichern
4120   GOSUB 13390                                           'Nachricht löschen
4130 GOTO 3740
4140 '
4150 ' *** Stammsatz drucken von 02************ & 04********** ****************
4160 '
4170 MT$="STAMMSATZ DRUCKEN":MI$="G-011040":GOSUB 13460            'Basis-Maske
4180 FKEYS$="":GOSUB 13570                                 'F-Tasten einblenden
4190 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer"
4200 PRINT FNCUR$(5,23)"des auszudruckenden Stammsatzes ein."
4210 PRINT FNCUR$(6,22);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
4220 PRINT FNCUR$(7,23)"Beenden Sie die Eingabe mit RETURN."
4230 PRINT FNCUR$(9,38);"=>..."
4240 ISBADINPUT=FALSE
4250 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
4260 Z=9:S=40:LNG=3:TYP=2:ISMUSS=TRUE
4270 GOSUB 10370                                                'Feld-Eingabe+1
4280 IF FKEY=1 THEN HELPID=G0110400:GOSUB 17700:GOTO 4170 'Hilfe-Text 'TODO: Wert geht verloren
4290 IF FKEY=2 THEN 2440
4300 IF FKEY=3 THEN 2290
4310 IF FKEY=4 THEN GOSUB 13890:GOTO 4270                 'Zum Hauptmenü?-Frage
4320 '
4330 M=VAL(OUTTEXT$)                                        'RETURN verarbeiten
4340   IF M>=1 AND M<=MTGLMAX THEN 4380
4350   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 13250         'Nachricht
4360   ISBADINPUT=TRUE
4370   GOTO 4260
4380 MTGLZAHL=M
4390 N$="Stammsatz wird gelesen...":GOSUB 13330 'Nachricht. F-Tasten ausblenden
4400 GOSUB 12280                                               'Stammsatz lesen
4410 GOSUB 13390                                             'Nachricht löschen
4420 '
4430 GOSUB 13460                                                   'Basis-Maske
4440 GOSUB 12170                                      'Mitgliedsnummer ausgeben
4450 GOSUB 11910             'Feldnamen, Feldwerte mit Feldplatzhalter ausgeben
4460 GOSUB 16180                                         'Drucker bereit?-Frage
4470 IF NOT ISJA THEN 4490
4480   GOSUB 16870                        'Stammsatz drucken (Bildschirmformat)
4490 GOTO 4170
4500 '
4510 ' *** Stammsatz suchen (und drucken) von 05*************** ***************
4520 '
4530 ISDRUCK=FALSE
4540 MT$="STAMMSATZ SUCHEN":IF ISDRUCK THEN MT$="SUCHEN & DRUCKEN"
4550 MI$="G-011050":IF ISDRUCK THEN MI$="G-013010"
4560 GOSUB 13460                                                   'Basis-Maske
4570 FKEYS$="":GOSUB 13570                                 'F-Tasten einblenden
4580 TMP$="Suchen":IF ISDRUCK THEN TMP$="Suchen & drucken" 
4590 TMP1=29:IF ISDRUCK THEN TMP1=24
4600 PRINT FNCUR$(4,TMP1)"1 - ";TMP$;" nach Nummern"
4610 PRINT FNCUR$(6,TMP1)"2 - ";TMP$;" nach Maske"
4620 FKEYMAX=4:NKEYMAX=2:GOSUB 13730                              'Menü-Eingabe
4630 IF NKEY THEN ON NKEY GOTO 4730,5330
4640 IF FKEY<>1 THEN 4680 
4650   HELPID=G0110501:IF ISDRUCK THEN HELPID=G0130102
4660   GOSUB 17700                                                  'Hilfe-Text
4670   GOTO 4540
4680 IF FKEY=2 THEN IF ISDRUCK THEN 7900 ELSE 2440
4690 IF FKEY=3 THEN 2290
4700 IF FKEY=4 THEN GOSUB 13890:GOTO 4620                 'Zum Hauptmenü?-Frage
4710 '
4720 ' *** Suchen nach Nummern
4730 TMP$=STR$(1):NUMSUCH$(1)=MID$(TMP$,2,99)
4740 TMP$=STR$(MTGLMAX):NUMSUCH$(2)=MID$(TMP$,2,99)
4750 MT$="STAMMSATZ SUCHEN NACH NUMMERN":MI$="G-011051"
4760 IF ISDRUCK THEN MT$="SUCHEN & DRUCKEN NACH NUMMERN":MI$="G-013011"
4770 GOSUB 13460                                                   'Basis-Maske
4780 FKEYS$="<F5>=Suchen":IF ISDRUCK THEN FKEYS$="<F5>=Suchen & Drucken"
4790 GOSUB 13570                                           'F-Tasten einblenden
4800 TMP$="gesucht":IF ISDRUCK THEN TMP$="gesucht und gedruckt"
4810 TMP1=12:IF ISDRUCK THEN TMP1=6
4820 TMP2=66:IF ISDRUCK THEN TMP2=73
4830 PRINT FNCUR$(4,TMP1);"Ab welcher Mitgliedsnummer soll ";TMP$;" werden?     =>..."
4840 PRINT FNCUR$(6,TMP1);"Bis zu welcher Mitgliedsnummer soll ";TMP$;" werden? =>..."
4850 PRINT FNCUR$(4,TMP2);HIGHON$;NUMSUCH$(1);HIGHOFF$
4860 PRINT FNCUR$(6,TMP2);HIGHON$;NUMSUCH$(2);HIGHOFF$
4870 PRINT FNCUR$(8,22);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
4880 F=1
4890 ISBADINPUT=FALSE
4900 FKEYMASK$="12345":CKEYMASK$="":INTEXT$=NUMSUCH$(F)
4910 Z=NUMSUCHROW(F):LNG=3:TYP=2:ISMUSS=TRUE
4920 S=66:IF ISDRUCK THEN S=73
4930 GOSUB 10370                                                'Feld-Eingabe+1
4940 IF FKEY<>1 THEN 4980                                 'F-Tasten verarbeiten
4950   HELPID=G0110510:IF ISDRUCK THEN HELPID=G0130110
4960   GOSUB 17700                                                  'Hilfe-Text
4970   GOTO 4750                                    'TODO: Werte gehen verloren
4980 IF FKEY=2 THEN 4540
4990 IF FKEY=3 THEN 2290
5000 IF FKEY=4 THEN GOSUB 13890:GOTO 4930                 'Zum Hauptmenü?-Frage
5010 IF FKEY<>5 THEN 5070
5020   SEARCH=BYNUMS
5030   NUMSUCH1=VAL(NUMSUCH$(1))
5040   NUMSUCH2=VAL(NUMSUCH$(2))
5050   IF ISDRUCK THEN 8620 ELSE 5650             'Suchen & Drucken oder Suchen
5060 '
5070 M=VAL(OUTTEXT$)                                        'RETURN verarbeiten
5080 IF M>=1 AND M<=MTGLMAX THEN 5120
5090   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 13250         'Nachricht
5100   GOSUB 5260                                     'Eingabefehler-Behandlung
5110   GOTO 4910
5120 NUMSUCH$(F)=OUTTEXT$
5130 NUMSUCH1=VAL(NUMSUCH$(1))
5140 NUMSUCH2=VAL(NUMSUCH$(2))
5150 IF NUMSUCH1<=NUMSUCH2 THEN 5190
5160   N$="Ungültiger Bereich":GOSUB 13250                           'Nachricht
5170   GOSUB 5260                                     'Eingabefehler-Behandlung
5180   GOTO 4910
5190 F=F+1:IF F>NUMSUCHMAX THEN F=1
5200 GOSUB 13830                                           'F-Tasten ausblenden
5210 FKEYS$="<F5>=Suchen":IF ISDRUCK THEN FKEYS$="<F5>=Suchen & Drucken" 
5220 GOSUB 13570                                           'F-Tasten einblenden
5230 GOTO 4900
5240 '
5250 ' *** SUB Eingabefehler-Behandlung
5260 ISBADINPUT=TRUE
5270 FKEYMASK$="1234"
5280 GOSUB 13830                                           'F-Tasten ausblenden
5290 FKEYS$="":GOSUB 13570                                 'F-Tasten einblenden
5300 RETURN
5310 '
5320 ' *** Suchen nach Maske
5330 GOSUB 11440                                                'SUCH$() leeren
5340 MT$="STAMMSATZ SUCHEN NACH MASKE":MI$="G-011052"
5350 IF ISDRUCK THEN MT$="SUCHEN & DRUCKEN NACH MASKE":MI$="G-013012"
5360 GOSUB 13460                                                   'Basis-Maske
5370 FKEYS$="<F5>=Suchen":IF ISDRUCK THEN FKEYS$="<F5>=Suchen & Drucken" 
5380 GOSUB 13570                                           'F-Tasten einblenden
5390 GOSUB 12040         'Feldnamen, Suchfeldwerte und Feldplatzhalter ausgeben
5400 F=1
5410 Z=FEROW(F):S=FECOL(F)+COLSEP:LNG=FELNG(F):TYP=FETYP(F):ISMUSS=FEISMUSS(F)
5420 FKEYMASK$="12345":CKEYMASK$="12":INTEXT$=SUCH$(F)
5430 GOSUB 10360                                                  'Feld-Eingabe
5440 IF FKEY<>1 THEN 5480                                 'F-Tasten verarbeiten
5450   HELPID=G0110520:IF ISDRUCK THEN HELPID=G0130120
5460   GOSUB 17700                                                  'Hilfe-Text
5470   GOTO 5340
5480 IF FKEY=2 THEN 4540
5490 IF FKEY=3 THEN 2290
5500 IF FKEY=4 THEN GOSUB 13890:GOTO 5430                 'Zum Hauptmenü?-Frage
5510 IF FKEY<>5 THEN 5540 
5520   SEARCH=BYMASK                                                      '<F5>
5530   IF ISDRUCK THEN 8620 ELSE 5650             'Suchen oder Suchen & Drucken
5540 IF CKEY=0 THEN 5600                              'Cursortasten verarbeiten
5550   PRINT FNCUR$(Z,S);HIGHON$;SUCH$(F);HIGHOFF$;
5560   PRINT STRING$(LNG-LEN(SUCH$(F)),".")
5570   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FELDMAX
5580   IF CKEY=CKEYDN THEN F=F+1:IF F>FELDMAX THEN F=1
5590   GOTO 5410
5600 SUCH$(F)=OUTTEXT$                                      'RETURN verarbeiten
5610 F=F+1:IF F>FELDMAX THEN F=1
5620 GOTO 5410
5630 '
5640 ' *** Suche
5650 IF SEARCH=BYMASK THEN 5740
5651   GOSUB 13460                                                 'Basis-Maske
5652   GOSUB 11390                                                'Leere DAT$() 
5653   GOSUB 11910           'Feldnamen, Feldwerte mit Feldplatzhalter ausgeben
5654   MTGLZAHL=NUMSUCH1                               'Init Suche nach Nummern
5655   GOTO 5870
5730 '
5740 GOSUB 13830                                           'F-Tasten ausblenden
5750 MTGLZAHL=0                                          'Init Suche nach Maske
5760 GOSUB 6710                                        'Nächsten Treffer suchen
5770 IF MTGLZAHL<>-1 THEN 5870
5780   N$="Kein Suchtreffer. Mit <F2> neue Suche.":GOSUB 13250       'Nachricht
5790   FKEYS$="":GOSUB 13570                               'F-Tasten einblenden
5800   FKEYMASK$="1234":NKEYMASK$="":GOSUB 13730                  'Menü-Eingabe
5810   ON FKEY GOTO 5820,5340,2290,5850
5820   HELPID=G0110502:IF ISDRUCK THEN HELPID=G0130103          'F1 verarbeiten 
5830   GOSUB 17700                                                  'Hilfe-Text
5831   GOSUB 13460                                                 'Basis-Maske
5833   GOSUB 12040       'Feldnamen, Suchfeldwerte mit Feldplatzhalter ausgeben
5840   GOTO 5780
5841 '
5850   GOSUB 13890:GOTO 5780              'F4 verarbeiten 'Zum Hauptmenü?-Frage
5860 '
5870 GOSUB 6460                                 'Suchtreffer lesen und anzeigen
5880 '
5890 FKEYS$="<F5>=Suchen rückwärts <F6>=Suchen vorwärts <F7>=Adressaufkleber" 
5900 GOSUB 13570                                           'F-Tasten einblenden
5910 FKEYMAX=7:NKEYMAX=0:GOSUB 13730                              'Menü-Eingabe
5920 ON FKEY GOTO 5950,5990,2290,6020,6060,6260,6530
5930 '
5940 ' *** <F1> Hilfe
5950 HELPID=G0110503:GOSUB 17700                                    'Hilfe-Text
5951 GOSUB 13460                                                   'Basis-Maske
5952 GOSUB 12170                                      'Mitgliedsnummer ausgeben
5953 GOSUB 11910             'Feldnamen, Feldwerte mit Feldplatzhalter ausgeben
5954 GOTO 5890
5970 '
5980 ' *** <F2> Zurück
5990 IF SEARCH=BYNUMS THEN 4740 ELSE 5340           'TODO: Werte gehen verloren
6000 '
6010 ' *** <F4> Hauptmenü
6020 GOSUB 13890                                          'Zum Hauptmenü?-Frage
6030 GOTO 5910
6040 '
6050 ' *** <F5> Suchen rückwärts
6060 GOSUB 13830                                           'F-Tasten ausblenden
6070 IF SEARCH=BYMASK THEN 6140
6080 '
6090 MTGLZAHL=MTGLZAHL-1                          'Suchen rückwärts nach Nummer
6100 IF MTGLZAHL>=NUMSUCH1 THEN 6220 
6110 MTGLZAHL=NUMSUCH1
6120 GOTO 6190
6130 '       
6140 TMP=MTGLZAHL                                  'Suchen rückwärts nach Maske
6150 GOSUB 6630                                      'Vorherigen Treffer suchen
6160 IF MTGLZAHL<>-1 THEN 6220
6170 MTGLZAHL=TMP                                      'Keinen Treffer gefunden
6180 '
6190 N$="Suchbeginn erreicht":GOSUB 13250       'Suchbeginn erreicht 'Nachricht
6200 GOTO 5890
6210 '
6220 GOSUB 6460                                 'Suchtreffer lesen und anzeigen
6230 GOTO 5890
6240 '
6250 ' *** <F6> Suchen vorwärts
6260 GOSUB 13830                                           'F-Tasten ausblenden
6270 IF SEARCH=BYMASK THEN 6340
6280 '
6290 MTGLZAHL=MTGLZAHL+1                           'Suchen vorwärts nach Nummer
6300 IF MTGLZAHL<=NUMSUCH2 THEN 6420
6310 MTGLZAHL=NUMSUCH2
6320 GOTO 6390
6330 '
6340 TMP=MTGLZAHL                                    'Suche vorwärts nach Maske
6350 GOSUB 6710                                        'Nächsten Treffer suchen
6360 IF MTGLZAHL<>-1 THEN 6420
6370 MTGLZAHL=TMP                                      'Keinen Treffer gefunden
6380 '
6390 N$="Suchende erreicht":GOSUB 13250           'Suchende erreicht 'Nachricht
6400 GOTO 5890
6410 '
6420 GOSUB 6460                                 'Suchtreffer lesen und anzeigen
6430 GOTO 5890
6440 '
6450 ' *** SUB Suchtreffer lesen und anzeigen
6460 GOSUB 13390                                             'Nachricht löschen
6470 GOSUB 12280                                               'Stammsatz lesen
6480 GOSUB 12170                                      'Mitgliedsnummer ausgeben
6490 GOSUB 11800                        'Feldwerte mit Feldplatzhalter ausgeben
6500 RETURN
6510 '
6520 ' *** <F7> Adressaufkleber drucken
6530 GOSUB 16180                                         'Drucker bereit?-Frage
6540 IF NOT ISJA THEN 6590
6550   N$="Adressaufkleber wird gedruckt...":GOSUB 13250             'Nachricht
6560   GOSUB 16690                                     'Adressaufkleber drucken
6570   GOSUB 13390                                           'Nachricht löschen
6590 GOTO 5890                                                    'Neue Eingabe
6600 '
6610 ' *** SUB Vorherigen Treffer suchen
6620 ' IN: MTGLZAHL OUT: MTGLZAHL oder -1 wenn kein Treffer
6630 MTGLZAHL=MTGLZAHL-1
6640 IF MTGLZAHL<1 THEN 6770
6650 GOSUB 6810                                                       'Treffer?
6660 IF NOT ISMATCH THEN 6630 
6670 RETURN
6680 '
6690 ' *** SUB Nächsten Treffer suchen
6700 ' IN: MTGLZAHL OUT: MTGLZAHL oder -1 wenn kein Treffer
6710 MTGLZAHL=MTGLZAHL+1
6720 IF MTGLZAHL>MTGLMAX THEN 6770
6730 GOSUB 6810                                                       'Treffer?
6740 IF NOT ISMATCH THEN 6710 
6750 RETURN
6760 '
6770 MTGLZAHL=-1
6780 RETURN
6790 '
6800 ' *** SUB Treffer?
6810 N$="Vergleiche Stammsatz"+STR$(MTGLZAHL)+"...":GOSUB 13250      'Nachricht
6820 GOSUB 12280                                               'Stammsatz lesen
6830 FOR K=1 TO FELDMAX
6840   S$=SUCH$(K)
6850   IF LEN(S$)=0 THEN 6980
6860   F$=LEFT$(S$,1)
6870   IF F$<>"*" THEN 6910
6880     IF LEN(S$)=1 THEN 6980
6890     IF INSTR(DAT$(K),RIGHT$(S$,LEN(S$)-1))>0 THEN 6980
6900     GOTO 7020
6910   IF F$<>"?" THEN 6970
6930     P=2
6940     IF MID$(S$,P,1)="?" THEN P=P+1:GOTO 6940
6941     IF P=LEN(DAT$(K))+1 THEN 6980         
6950     IF INSTR(DAT$(K),RIGHT$(S$,LEN(S$)-P+1))=P THEN 6980
6960     GOTO 7020
6970   IF S$<>DAT$(K) THEN 7020 
6980 NEXT K
6990 ISMATCH=TRUE
7000 RETURN
7010 '
7020 ISMATCH=FALSE
7030 RETURN
7040 '
7050 ' ######################### Verwendungsnachweis ##########################
7060 '
7070 GOSUB 11500                             'Einsprung MVW-Menü 'MTGLMAX lesen
7080 VNFLAG=TRUE
7090 '
7100 ' Verwendungsnachweis-Menü ***********************************************
7110 '
7120 MT$="VERWENDUNGSNACHWEIS":MI$="G-012000":GOSUB 13460          'Basis-Maske
7130 F=4                                                                'TODO ?
7140 FKEYS$="":GOSUB 13570                                 'F-Tasten einblenden
7150 PRINT CURON$;
7160 PRINT HIGHOFF$;
7170 PRINT FNCUR$(4,20)"   Bitte geben Sie die Mitgliedsnummer"
7180 PRINT FNCUR$(5,20)"des zu ändernden Verwendungsnachweises ein"
7190 'LW PRINT FNCUR$(6,23);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
7200 PRINT FNCUR$(6,20)" und beenden Sie die Eingabe mit RETURN."
7210 PRINT FNCUR$(8,38)"=>..."
7220 Z=8:S=39:LNG=3:TYP=2
7230 GOSUB 10360                                                  'Feld-Eingabe
7240 IF FKEY=1 THEN HELPID=G0120001:GOSUB 17700:GOTO 7120           'Hilfe-Text
7250 IF FKEY=2 OR FKEY=3 THEN GOSUB 11390:VNFLAG=FALSE:GOTO 2290 'DAT$() leeren
7260 IF FKEY=4 THEN GOSUB 13890                           'Zum Hauptmenü?-Frage
7270 IF FKEY>4 THEN 7220
7280 IF CKEY<>0 THEN 7220
7290 IF LEN(OUTTEXT$)=0 THEN N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 13250:GOTO 7210 ELSE MITGLIED=VAL(OUTTEXT$):IF MITGLIED=0 THEN 7230 'Nachricht
7300 IF MITGLIED>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 13250:PRINT HOME$;HIGHOFF$:GOTO 7210 'Nachricht
7310 MTGLZAHL=MITGLIED:MTGL$=STR$(MITGLIED)
7320 '
7330 ' *** Lesen des Mitgliedsnamens ******************************************
7340 '
7350 PRINT FNCUR$(21,1);DELLINE$;
7360 GET #2,MITGLIED                                           'DATEN STS lesen
7370 NNAM$=LEFT$(DD1$,25)                                 'NACHNAME RAUSFISCHEN
7380 VNAM$=MID$(DD1$,26,25)                               'VORNAME  RAUSFISCHEN
7390 '
7400 ' ** LEERZEICHEN HINTER NACHNAME ABSCHNEIDEN *****************************
7410 '
7420 FOR I=LEN(NNAM$) TO 1 STEP -1
7430   IF MID$(NNAM$,I,1)<>" " THEN NNAM$=LEFT$(NNAM$,I):I=1
7440 NEXT
7450 NACHVORNAME$=NNAM$+" "+VNAM$
7460 GOSUB 14410                                             '1. VN-BLATT lesen
7470 BLATT=1
7480 VP=1:HP=1                                                 'INITIALISIERUNG
7490 F=8
7500 GOSUB 13460                                                   'Basis-Maske
7510 FKEYS$="<F7>=Speichern":GOSUB 13570                   'F-Tasten einblenden
7520 PRINT HIGHOFF$;
7530 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=Vorwärts";TAB(53);"<F9>=Blatt drucken";
7540 PRINT FNCUR$(3,2);"Blatt:"
7550 PRINT FNCUR$(5,2);"Mitglieds-Nr.:"
7560 PRINT FNCUR$(5,24);"Name:"
7570 MSK$="Von:  .  .   Bis:  .  .   Amt:"
7580 FOR KK=7 TO 19 
7590   PRINT FNCUR$(KK,2);MSK$;TAB(43);MSK$
7600 NEXT
7610 GOSUB 15620                                                 'Ausgabe Daten
7620 LNG=2-(HP=7)*6                                    'BEGIN OF THE INPUT-LOOP
7630 TYP=2-(HP=7)
7640 Z=VP+13*(VP>13)+6
7650 S=6-39*(VP>13)-(HP<7)*(HP-1)*3+(HP>3)*(HP<7)*4-(HP=7)*26
7660 GOSUB 13390                                             'Nachricht löschen
7670 GOSUB 10360                                                  'Feld-Eingabe
7680 IF HP<7 THEN IF ASCC=ASCRET THEN OUTTEXT$=STRING$(2-LEN(OUTTEXT$),"0")+OUTTEXT$   '+NULLEN
7690 IF CKEY<>0 OR ASCC=ASCRET THEN PRINT FNCUR$(Z,S);OUTTEXT$
7700 IF ASCC=ASCRET THEN IF HP<7 THEN MID$(DAT$(VP),-1+2*HP,2)=OUTTEXT$ ELSE DAT$(VP)=LEFT$(DAT$(VP),12)+OUTTEXT$+SPACE$(8-LEN(OUTTEXT$))
7710 IF ASCC=ASCRET AND HP<7 THEN ISJA=TRUE:TEST$=OUTTEXT$:ON HP GOSUB 13010,13040,13070,13010,13040,13070:IF NOT ISJA THEN N$="Ungültiges Datum":GOSUB 13250:GOTO 7670 'Datums-Check 'Nachricht
7720 IF ASCC=ASCRET THEN IF HP=7 THEN HP=1:VP=VP-(VP<26)-(VP=26)*(-25):GOTO 7620 ELSE HP=HP+1:GOTO 7620     'NEUE SCREEN-POSITION
7730 IF CKEY<>0 THEN TEST$=MID$(DAT$(VP),-(HP<7)*2*HP-1-(HP=7)*14,-(HP<7)*2-(HP=7)*8):PRINT FNCUR$(Z,S);TEST$
7740 IF CKEY<>0 THEN IF HP<7 AND NOT TEST$="  " THEN ISJA=TRUE:ON HP GOSUB 13010,13040,13070,13010,13040,13070:IF NOT ISJA THEN N$="Ungültiges Datum":GOSUB 13250:GOTO 7670 'Nachricht
7750 IF CKEY=CKEYDN THEN VP=VP-(VP<26)-(VP=26)*(-25):GOTO 7620
7760 IF CKEY=CKEYUP THEN VP=VP+(VP>1)-(VP=1)*25     :GOTO 7620
7770 IF CKEY=CKEYRT THEN HP=HP-(HP<7)-(HP=7)*(-6)   :GOTO 7620
7780 IF CKEY=CKEYLT THEN HP=HP+(HP>1)+(HP=1)*(-6)   :GOTO 7620
7790 IF FKEY=1 THEN HELPID=G0120002:GOSUB 17700:GOTO 7480           'Hilfe-Text
7800 IF FKEY=2 THEN 7120                                       'ZU MTGL-EINGABE
7810 IF FKEY=3 THEN VNFLAG=FALSE:GOSUB 11390:GOTO 2290           'DAT$() leeren
7820 IF FKEY=4 THEN GOSUB 13890:GOSUB 13630:GOTO 7670     'Zum Hauptmenü?-Frage
7830 IF FKEY=5 THEN GOSUB 14500:GOTO 7620
7840 IF FKEY=6 THEN GOSUB 14640:GOTO 7620
7850 IF FKEY=7 THEN GOSUB 15280:GOTO 7670                 'SPEICHERN DANN INPUT
7860 IF FKEY=9 THEN GOSUB 15860:GOSUB 14830:GOTO 7660
7870 '
7880 ' ############# Mitgliedsliste drucken von 05*************** #############
7890 '
7900 MT$="MITGLIEDSLISTE DRUCKEN":MI$="G-013000":GOSUB 13460       'Basis-Maske
7910 FKEYS$="":GOSUB 13570                                 'F-Tasten einblenden
7920 PRINT FNCUR$(4,30);"1 - Suchen & Drucken"
7930 PRINT FNCUR$(6,30);"2 - Komplett drucken"
7940 PRINT FNCUR$(8,30);"3 - Datenblatt drucken"
7950 FKEYMAX=4:NKEYMAX=3:GOSUB 13730                              'Menü-Eingabe
7960 IF NKEY THEN ON NKEY GOTO 8030,8030,9050
7970 ON FKEY GOTO 7980,2290,2290,7990
7980 HELPID=G0130000:GOSUB 17700:GOTO 7900                          'Hilfe-Text
7990 GOSUB 13890:GOTO 7950                                'Zum Hauptmenü?-Frage
8000 '
8010 ' *** Suchen & Drucken, Komplett drucken *********************************
8020 '
8030 IF NKEY=1 THEN MT$="SUCHEN & DRUCKEN":MI$="G-013010"
8040 IF NKEY=2 THEN MT$="KOMPLETT DRUCKEN":MI$="G-013020"
8050 GOSUB 13460                                                   'Basis-Maske
8060 GOSUB 13570                                           'F-Tasten einblenden
8070 PRINT FNCUR$(4,22);"Bitte geben Sie das heutige Datum ein."
8080 PRINT FNCUR$(5,23);"Beenden Sie die Eingabe mit RETURN."
8090 PRINT FNCUR$(7,36);"=>........"
8100 ISBADINPUT=FALSE
8110 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
8120 Z=7:S=38:LNG=8:TYP=4:ISMUSS=TRUE
8130 GOSUB 10370                                                'Feld-Eingabe+1
8140 IF FKEY<>1 THEN 8190       'F-Tasten verarbeiten 'TODO: Wert geht verloren
8150   IF NKEY=1 THEN HELPID=G0130101
8160   IF NKEY=2 THEN HELPID=G0130200
8170   GOSUB 17700                                                  'Hilfe-Text
8180   GOTO 8030
8190 IF FKEY=2 THEN 7900
8200 IF FKEY=3 THEN 2290
8210 IF FKEY=4 THEN GOSUB 13890:GOTO 8130                 'Zum Hauptmenü?-Frage
8220 '
8230 POS1=INSTR(OUTTEXT$,".")                               'RETURN verarbeiten
8240 POS2=INSTR(POS1+1,OUTTEXT$,".")
8250 IF POS2=0 THEN 8480
8260 POS3=INSTR(POS2+1,OUTTEXT$,".")
8270 IF POS3<>0 THEN 8480
8280 D1$=LEFT$(OUTTEXT$,POS1-1)
8290 IF LEN(D1$)=0 OR LEN(D1$)>2 THEN 8480
8300 D2$=MID$(OUTTEXT$,POS1+1,POS2-POS1-1)
8310 IF LEN(D2$)=0 OR LEN(D2$)>2 THEN 8480
8320 D3$=RIGHT$(OUTTEXT$,LEN(OUTTEXT$)-POS2)
8330 IF LEN(D3$)=0 OR LEN(D3$)>2 THEN 8480
8340 '
8350 D1=VAL(D1$):IF D1<1 OR D1>31 THEN 8510
8360 D2=VAL(D2$):IF D2<1 OR D2>12 THEN 8510
8370 D3=VAL(D3$) 
8380 ' TODO: Genauere Verprobung
8390 D1$=STR$(D1):D1$=MID$(D1$,2,99):IF D1<10 THEN D1$="0"+D1$
8400 D2$=STR$(D2):D2$=MID$(D2$,2,99):IF D2<10 THEN D2$="0"+D2$
8410 D3$=STR$(D3):D3$=MID$(D3$,2,99):IF D3<10 THEN D3$="0"+D3$
8420 DATUM$=D1$+"."+D2$+"."+D3$
8430 '
8440 PRINT FNCUR$(7,37);HIGHON$;DATUM$;HIGHOFF$
8450 GOTO 8550
8460 '
8470 ' *** Eingabe-Fehlerbehandlung Datum
8480 N$="Datum muss Format TT.MM.JJ haben. Bitte korrigieren.":GOSUB 13250 'Nachricht
8490 GOTO 8520
8500 '
8510 N$="Ungültiges Datum. Bitte korrigieren.":GOSUB 13250           'Nachricht
8520 ISBADINPUT=TRUE
8530 GOTO 8120
8540 '
8550 ISDRUCK=TRUE 
8560 IF NKEY=2 THEN 8670
8570 '
8580 ' *** Suchen nach Nummern und Maske
8590 GOTO 4540                                  'Stammsatz suchen (und drucken)
8600 ' *** Rückkehr aus "Stammsatz suchen"
8610 ' IN: SEARCH (BYNUMS, BYMASK), NUMSUCH1, NUMSUCH2, SUCH$()
8620 GOSUB 16180                                         'Drucker bereit?-Frage
8630 IF NOT ISJA THEN 9010                                 'LW too much refresh
8640 GOTO 8730
8650 '
8660 ' *** Komplette Mitgliedsliste drucken
8670 SEARCH=BYNUMS
8680 NUMSUCH1=1:NUMSUCH2=MTGLMAX
8690 GOSUB 16180                                         'Drucker bereit?-Frage
8700 IF NOT ISJA THEN 8030                                 'LW too much refresh
8710 '
8720 ' *** Drucken
8730 N$="Mitgliedsliste wird gedruckt... Abbrechen mit Taste.":GOSUB 13250 'Nachricht
8740 GOSUB 16350                                 'Mitgliedsliste drucken (Kopf)
8750 IF SEARCH=BYMASK THEN 8880
8760 '
8770 ' *** Suchen nach Nummern
8780 FOR MTGLZAHL=NUMSUCH1 TO NUMSUCH2
8790   GOSUB 12280                                             'Stammsatz lesen
8800   IF INSTR(DAT$(5),"ausgetreten")>0 THEN 8820
8810     GOSUB 16490                         'Mitgliedsliste drucken (Mitglied)
8820   GOSUB 16250                                      'Druck abbrechen?-Frage
8830   IF ISJA THEN 8990
8840 NEXT MTGLZAHL
8850 GOTO 8990
8860 ' 
8870 ' *** Suchen nach Maske
8880 MTGLZAHL=0
8890 GOSUB 6710                                        'Nächsten Treffer suchen
8900 IF MTGLZAHL=-1 THEN 8990
8910   GOSUB 12280                                             'Stammsatz lesen
8920   IF INSTR(DAT$(5),"ausgetreten")>0 THEN 8940
8930     GOSUB 16490                         'Mitgliedsliste drucken (Mitglied)
8940   GOSUB 16250                                      'Druck abbrechen?-Frage
8950   IF ISJA THEN 8990
8960 GOTO 8890
8970 '
8980 ' *** Suchen zuende
8990 GOSUB 13390                                             'Nachricht löschen
9000 IF NKEY=2 THEN 7900                                 'TODO: Very hackish...
9010 IF SEARCH=BYNUMS THEN 4740 ELSE 5340                'TODO: Very hackish...
9020 '
9030 ' *** Datenblatt drucken *************************************************
9040 '
9050 MT$="DATENBLATT DRUCKEN":MI$="G-013030":GOSUB 13460           'Basis-Maske
9060 FKEYS$="":GOSUB 13570                                 'F-Tasten einblenden
9070 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer"
9080 PRINT FNCUR$(5,23)"des zu druckenden Datenblattes ein."
9090 PRINT FNCUR$(6,22);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
9100 PRINT FNCUR$(7,23)"Beenden Sie die Eingabe mit RETURN."
9110 PRINT FNCUR$(10,38)"=>..."
9120 ISBADINPUT=FALSE
9130 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
9140 Z=10:S=40:LNG=3:TYP=2:ISMUSS=TRUE
9150 GOSUB 10370                                                'Feld-Eingabe+1
9160 IF FKEY=1 THEN HELPID=G0130300:GOSUB 17700:GOTO 9050 'Hilfe-Text 'TODO: Wert geht verloren
9170 IF FKEY=2 THEN 7900
9180 IF FKEY=3 THEN 2290
9190 IF FKEY=4 THEN GOSUB 13890:GOTO 9150                 'Zum Hauptmenü?-Frage
9200 '
9210 M=VAL(OUTTEXT$)                                        'RETURN verarbeiten
9220 IF M>=1 AND M<=MTGLMAX THEN 9260
9230   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 13250         'Nachricht
9240   ISBADINPUT=TRUE
9250   GOTO 9140
9260 MTGLZAHL=M
9270 GOSUB 16180                                         'Drucker bereit?-Frage
9280 IF ISJA THEN 9320
9290   GOSUB 13570                                         'F-Tasten einblenden
9300   INTEXT$=OUTTEXT$
9310   GOTO 9140
9320 N$="Datenblatt drucken...":GOSUB 13330     'Nachricht. F-Tasten ausblenden
9330 GOSUB 12280                                               'Stammsatz lesen
9340 VOLLNAME$ = DAT$(1)+" "+DAT$(2)
9350 GOSUB 16870                          'Stammsatz drucken (Bildschirmformat)
9360 GOSUB 14410                                     'Verwendungsnachweis lesen
9370 GOSUB 15890                                   'Verwendungsnachweis drucken
9380 GOSUB 17140                                     'Mitgliedsvermerke drucken
9390 GOSUB 13390                                             'Nachricht löschen
9400 GOTO 9050
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
10290 ' IN: LNG - Länge  OUT: CKEY     TMP: ISBADINPUT (TRUE, FALSE)
10300 ' IN: TYP - Typ    OUT: ASCC
10310 ' IN: ISMUSS (TRUE, FALSE)
10320 ' IN: INTEXT$
10330 ' IN: FKEYMASK$
10340 ' IN: CKEYMASK$
10350 ' IN: VNFLAG
10360 ISBADINPUT=FALSE
10370 IF ISBADINPUT THEN INTEXT$=OUTTEXT$
10380 P=LEN(INTEXT$)
10390 PRINT FNCUR$(Z,S);HIGHON$;INTEXT$;HIGHOFF$;STRING$(LNG-P,".")
10400 OUTTEXT$=INTEXT$
10410 PRINT CURON$;
10420 CKEY=0:FKEY=0
10430 PRINT FNCUR$(Z,S+P);
10440 A$=INPUT$(1):ASCC=ASC(A$)
10450 IF NOT ISBADINPUT THEN 10490
10460   GOSUB 13390                                          'Nachricht löschen
10470   PRINT FNCUR$(Z,S+P);
10480   ISBADINPUT=FALSE
10490 ON TYP GOSUB 12800,12860,12910,12960                          'Typ prüfen
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
10720 N$="Ungültige Eingabe - Bitte wiederholen":GOSUB 13250         'Nachricht
10730 ISBADINPUT=TRUE
10740 GOTO 10430
10750 '
10760 IF NOT ISMUSS OR LEN(OUTTEXT$)>0 THEN 10800           'RETURN verarbeiten
10770   N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 13250
10780   ISBADINPUT=TRUE 
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
10970 FOR K=1 TO FELDMAX
10980   READ NAMEN$,SPALTE,ZEILE,LAENGE,TYP
10990   FENAM$(K)=NAMEN$
11000   IF MID$(NAMEN$,13,1)="=" THEN FEISMUSS(K)=TRUE ELSE FEISMUSS(K)=FALSE                           
11010   FECOL(K)=SPALTE
11020   FEROW(K)=ZEILE
11030   FELNG(K)=LAENGE
11040   FETYP(K)=TYP 
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
11390 FOR K=1 TO FELDMAX:DAT$(K)="":NEXT K
11400 RETURN
11410 '
11420 ' *** SUCH$() leeren ****************************************************
11430 '
11440 FOR K=1 TO FELDMAX:SUCH$(K)="":NEXT K
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
11760 '
11770 ' *** Maske Feldwerte mit Feldplatzhalter ausgeben **********************
11780 '
11790 ' IN: FELDMAX
11800 FOR K=1 TO FELDMAX
11810   Z=FEROW(K)
11820   S=FECOL(K)+COLSEP
11830   PRINT FNCUR$(Z,S);HIGHON$;DAT$(K);HIGHOFF$;
11840   PRINT STRING$(FELNG(K)-LEN(DAT$(K)),".")
11850 NEXT K
11860 RETURN
11870 '
11880 ' *** Maske Feldnamen, Feldwerte mit Feldplatzhalter ausgeben ***********
11890 '
11900 ' IN: FELDMAX
11910 FOR K=1 TO FELDMAX
11920   Z=FEROW(K)
11930   S=FECOL(K)
11940   PRINT FNCUR$(Z,S);FENAM$(K)
11950   S=S+COLSEP
11960   PRINT FNCUR$(Z,S);HIGHON$;DAT$(K);HIGHOFF$;
11970   PRINT STRING$(FELNG(K)-LEN(DAT$(K)),".")               
11980 NEXT K
11990 RETURN
12000 '
12010 ' *** Maske Feldnamen, Suchfeldwerte mit Feldplatzhalter ausgeben *******
12020 '
12030 ' IN: FELDMAX
12040 FOR K=1 TO FELDMAX
12050   Z=FEROW(K)
12060   S=FECOL(K)
12070   PRINT FNCUR$(Z,S);FENAM$(K)
12080   S=S+COLSEP
12090   PRINT FNCUR$(Z,S);HIGHON$;SUCH$(K);HIGHOFF$;
12100   PRINT STRING$(FELNG(K)-LEN(SUCH$(K)),".")               
12110 NEXT K
12120 RETURN
12130 '
12140 ' *** Mitgliedsnummer ausgeben ******************************************
12150 '
12160 ' IN: MTGLZAHL
12170 PRINT FNCUR$(3,31);"Mitgliedsnummer";HIGHON$;MTGLZAHL;HIGHOFF$;"   "
12180 RETURN
12190 '
12200 ' *** FIELDen der Stammsatz-Datei *************************************** 
12210 '
12220 OPEN "R",#2,"B:MV.DAT",462+27
12230 FIELD #2,236+14 AS XSTAMMSATZ1$,225+13 AS XSTAMMSATZ2$
12240 RETURN
12250 '
12260 ' *** Stammsatz von Disk lesen ******************************************
12270 '
12280 GET #2,MTGLZAHL                                             'LW Compress?
12290 P=1
12300 FOR K=1 TO 14
12310   LNG=FELNG(K)+1
12320   TMP$=MID$(XSTAMMSATZ1$,P,LNG)
12330   L=ASC(LEFT$(TMP$,1))
12340   DAT$(K)=MID$(TMP$,2,L)
12350   P=P+LNG
12360 NEXT K
12370 P=1
12380 FOR K=15 TO FELDMAX
12390   LNG=FELNG(K)+1
12400   TMP$=MID$(XSTAMMSATZ2$,P,LNG)
12410   L=ASC(LEFT$(TMP$,1))
12420   DAT$(K)=MID$(TMP$,2,L)
12430   P=P+LNG
12440 NEXT K
12450 RETURN
12460 '
12470 ' *** Stammsatz auf Disk speichern **************************************
12480 '
12490 ' IN: MTGLZAHL
12500 TMP$=""                                                     'LW Compress?
12510 FOR K=1 TO 14
12520   LNG=FELNG(K)
12530   L=LEN(DAT$(K))
12540   TMP$=TMP$+CHR$(L)+DAT$(K)+SPACE$(LNG-LEN(DAT$(K)))
12550 NEXT K
12560 LSET XSTAMMSATZ1$=TMP$
12570 TMP$=""
12580 FOR K=15 TO FELDMAX
12590   LNG=FELNG(K)
12600   L=LEN(DAT$(K))
12610   TMP$=TMP$+CHR$(L)+DAT$(K)+SPACE$(LNG-LEN(DAT$(K)))
12620 NEXT K
12630 LSET XSTAMMSATZ2$=TMP$
12640 PUT #2,MTGLZAHL
12650 RETURN
12660 '
12670 ' *** Ja/Nein-Frage und F-Tasten ausblenden *****************************
12680 '
12690 ' IN:  Q$   - Frage
12700 ' OUT: ISJA - (TRUE, FALSE)
12710 N$=Q$+" (J=Ja/N=Nein)":GOSUB 13250                             'Nachricht
12720 GOSUB 13830                                          'F-Tasten ausblenden
12730 ANSWER$=INPUT$(1)
12740 IF ANSWER$="J" OR ANSWER$="j" THEN ISJA=TRUE:RETURN
12750 IF ANSWER$="N" OR ANSWER$="n" THEN ISJA=FALSE:RETURN
12760 GOTO 12730
12770 '
12780 ' *** Testbereich 1 (Alpha) *********************************************
12790 '
12800 ISVALID=FALSE
12810 IF (ASCC>57 AND ASCC<127) OR (ASCC>31 AND ASCC<48) THEN ISVALID=TRUE
12820 RETURN
12830 '
12840 ' *** Testbereich 2 (Numerisch) *****************************************
12850 '
12860 ISVALID=FALSE: IF ASCC>47 AND ASCC<58 THEN ISVALID=TRUE
12870 RETURN
12880 '
12890 ' *** Testbereich 3 (Alphanumerisch) ************************************
12900 '
12910 ISVALID=FALSE: IF ASCC>31 AND ASCC<127 THEN ISVALID=TRUE
12920 RETURN
12930 '
12940 ' *** Testbereich 4 (Numerisch und ".") *********************************
12950 '
12960 ISVALID=FALSE: IF (ASCC>=48 AND ASCC<=57) OR ASCC=46 THEN ISVALID=TRUE
12970 RETURN
12980 '
12990 ' *** DATUMS-TEST (bei VN und M-Liste drucken) **************************
13000 '
13010 IF VAL(TEST$)=0 OR VAL(TEST$)>31 THEN ISJA=FALSE                     'Tag
13020 RETURN
13030 '
13040 IF VAL(TEST$)=0 OR VAL(TEST$)>12 THEN ISJA=FALSE                   'Monat
13050 RETURN
13060 '
13070 IF VAL(TEST$)=0 THEN ISJA=FALSE                                     'Jahr
13080 RETURN
13090 '
13100 ' *** Voreinstellungen zuweisen in "Stammsatz anlegen" ******************
13110 '
13120 DAT$(5)="10***"
13130 DAT$(7)=""11**""
13140 DAT$(11)="12**"
13150 DAT$(12)="13******"
13160 DAT$(21)="3"
13170 DAT$(22)="Nein"
13180 DAT$(23)="Nein"
13190 DAT$(25)="Nein"
13200 RETURN
13210 '
13220 ' *** Nachricht anzeigen ************************************************
13230 '
13240 ' IN: N$ - Nachricht
13250 PRINT FNCUR$(21,1);DELLINE$;
13260 IF N$<>"" THEN PRINT INVON$;" ";N$;" ";INVOFF$;
13270 PRINT                              'LW workaround quirk in display driver
13280 RETURN
13290 '
13300 ' *** Nachricht anzeigen und F-Tasten ausblenden ************************ 
13310 '
13320 ' IN: N$ - Nachricht
13330 GOSUB 13250                                                    'Nachricht
13340 GOSUB 13830                                          'F-Tasten ausblenden
13350 RETURN
13360 '
13370 ' *** Nachricht löschen *************************************************
13380 '
13390 N$="":GOSUB 13250                                              'Nachricht
13400 RETURN
13410 '
13420 ' *** Bildschirmmaske - Basis *******************************************
13430 '
13440 ' IN: MT$ - Menü-Titel
13450 ' IN: MI$ - Menu-ID
13460 PRINT CLS$;
13470 PRINT "00**********************";
13480 PRINT FNCUR$(1,(80-LEN(MT$))/2+1);MT$;FNCUR$(1,72);MI$
13490 PRINT STRICH$
13500 PRINT FNCUR$(21,1);
13510 PRINT STRICH$             'quirk in display driver places this in line 22
13520 RETURN
13530 '
13540 ' *** F-Tasten einblenden ***********************************************
13550 '
13560 ' IN: FKEYS$ - Zusätzliche F-Tasten 
13570 PRINT FNCUR$(23,1);"<F1>=Hilfe <F2>=Zurück <F3>=Anfang <F4>=Hauptmenü";
13580 IF FKEYS$<>"" THEN PRINT " ";FKEYS$;HOME$ ELSE PRINT             'LW Hack
13590 RETURN
13600 '
13610 ' *** restliche funktionstasten bei Verw.-nachweis ausgeben
13620 '
13630 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=Vorwärts             <F8>=Drucken";
13640 PRINT HOME$
13650 RETURN
13660 '
13670 ' *** Eingabe bei Menübildern *******************************************
13680 '
13690 ' IN:  NKEYMAX - Max gültige Zifferntaste (1,2,3,...)
13700 ' IN:  FKEYMAX - Max gültige F-Taste (1,2,3,...)
13710 ' OUT: NKEY    - Ziffer der gedrückten Zifferntaste oder 0
13720 ' OUT: FKEY    - Ziffer der gedrückten F-Taste oder 0
13730 NKEY=0:FKEY=0
13740 GOSUB 10910                                          'Tastatur entprellen
13750 A$=INPUT$(1):ASCC=ASC(A$)
13760 IF ASCC>=ASC1 AND ASCC<=ASC1+NKEYMAX-1 THEN NKEY=ASCC-ASC1+1:RETURN
13770 IF ASCC>=ASCF1 AND ASCC<=ASCF1+FKEYMAX-1 THEN FKEY=ASCC-ASCF1+1:RETURN
13780 N$="Sie haben eine falsche Taste gedrückt - Bitte neue Auswahl treffen.":GOSUB 13250 'Nachricht
13790 GOTO 13750
13800 '
13810 ' *** F-Tasten ausblenden ***********************************************
13820 '
13830 PRINT FNCUR$(23,1);DELLINE$
13840 PRINT FNCUR$(24,1);DELLINE$;
13850 RETURN
13860 '
13870 ' *** 'Zum Hauptmenü?-Frage (Original) **********************************
13880 '
13890 GOSUB 13970                               'Zum Hauptmenü?-Frage (einfach)
13900 IF ISJA THEN 18000                                          'Programmende
13910 GOSUB 13570                                          'F-Tasten einblenden
13920 RETURN
13930 '
13940 ' *** 'Zum Hauptmenü?-Frage (einfach) ***********************************
13950 '
13960 ' OUT: ISJA (TRUE, FALSE)
13970 Q$="Wollen Sie wirklich ins Hauptmenü?":GOSUB 12710        'Ja/Nein-Frage
13980 GOSUB 13390                                            'Nachricht löschen
13990 RETURN
14000 '
14010 ' *** Rückkehr aus Hilfe-Text? ******************************************
14020 '
14030 N$="Zurück? (J=Ja)":GOSUB 13250                                'Nachricht
14040 GOSUB 12720                             'Ja/Nein-Frage ohne String-Koncat
14050 IF NOT ISJA THEN 14030
14060 RETURN
14070 '
14080 ' **** VN - Subroutines, permitting one page handling *******************
14090 ' ****         by 04********** 21*******              *******************
14100 '
14110 ' **** OPEN & FIELDING VERWENDUNGSNACHWEIS ******************************
14120 '
14130 OPEN "R",#3,"B:VN.NDX",4
14140 FIELD #3,4 AS POINT0$
14150 OPEN "R",#4,"B:VN.POI",8
14160 FIELD #4,4 AS POINT1$, 4 AS POINT2$
14170 OPEN "R",#5,"B:VN.DAT",560
14180 FIELD #5,240 AS DDD1$,240 AS DDD2$,80 AS DDD3$
14190 RETURN
14200 '
14210 ' *** VERWENDUNGSNACHWEIS FÜR NEUES MITGLIED ANLEGEN ********************
14220 '
14230 ' IN: MTGLZAHL
14240 GET #3,1                               'letzte Seitennummer lesen dann +1
14250 TMPOINT=VAL(POINT0$)+1
14260 LSET DDD1$=""                                    'leeren Record speichern
14270 LSET DDD2$=""
14280 LSET DDD3$=""
14290 PUT #5,TMPOINT
14300 LSET POINT0$=STR$(TMPOINT)             'letzte Seitennummer neu speichern
14310 PUT #3,1
14320 PUT #3,MTGLZAHL+1
14330 LSET POINT1$="0000"
14340 LSET POINT2$="0000"
14350 PUT #4,TMPOINT
14360 RETURN
14370 '
14380 ' **** ERSTE SEITE FINDEN & LESEN ***************************************
14390 '
14400 ' IN: MTGLZAHL
14410 GET #3,MTGLZAHL+1                                            'Index_Datei
14420 CPOINT=VAL(POINT0$)
14430 GET #5,CPOINT:GOSUB 15460                                          'Lesen
14440 BLATT=1
14450 GET #4,CPOINT                                              'Pointer lesen
14460 RETURN
14470 '
14480 ' **** SEITE RÜCKWÄRTS **************************************************
14490 '
14500 TMPOINT=VAL(POINT1$)
14510 IF TMPOINT<>0 THEN 14540
14520 N$="Erste Seite":GOSUB 13250                                   'Nachricht
14530 RETURN
14540 CPOINT=TMPOINT
14550 BLATT=BLATT-1
14560 GET #5,CPOINT:GOSUB 15460   
14570 GET #4,CPOINT:GOSUB 15620                             'Neue Pointer lesen
14580 HP=1:VP=1
14590 GOSUB 14830
14600 RETURN
14610 '
14620 ' **** SEITE VORWÄRTS  **************************************************
14630 '
14640 TMPOINT=VAL(POINT2$)
14650 IF TMPOINT=0 THEN 14720
14660 CPOINT=TMPOINT
14670 BLATT=BLATT+1
14680 GET #5,CPOINT:GOSUB 15460
14690 GET #4,CPOINT:GOSUB 15620
14700 HP=1:VP=1
14710 RETURN
14720 N$="Letzte Seite"
14730 IF INSTR(SPACE$(20),DAT$(26))=0 THEN 14760 
14740 GOSUB 13250                                                    'Nachricht
14750 RETURN
14760 Q$="Letzte Seite - Neue Seite anlegen?":GOSUB 12710        'Ja/Nein-Frage
14770 GOSUB 13390                                            'Nachricht löschen
14780 IF NOT ISJA THEN 14830
14790 GOSUB 15070
14800 BLATT=BLATT+1
14810 GOSUB 15620
14820 HP=1:VP=1
14830 GOSUB 13570                                          'F-Tasten einblenden
14840 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=vorwärts";
14850 PRINT TAB(54);"<F9>=Blatt drucken";
14860 PRINT HOME$;" "
14870 RETURN
14880 '
14890 ' **** ERSTE SEITE EINES MITGLIEDS ANLEGEN ******************************
14900 '
14910 GET #3,1
14920 LSET POINT0$=STR$(VAL(POINT0$)+1)                                  'NDX+1
14930 PUT #3,1
14940 LSET POINT1$="0000"                                        'Page pointers
14950 LSET POINT2$="0000"
14960 PUT #4,VAL(POINT0$)
14970 LSET POINT0$=POINT0$                                          'First page
14980 PUT #3,MTGLZAHL+1
14990 LSET DDD1$=""                                                 'Datenblatt
15000 LSET DDD2$=""
15010 LSET DDD3$=""
15020 PUT #5,VAL(POINT0$)
15030 RETURN
15040 '
15050 ' **** ALLGEMEIN EINE NEUE SEITE ANLEGEN ********************************
15060 '
15070 GET #3,1                                                           'NDX+1
15080 LSET POINT0$=STR$(VAL(POINT0$)+1)
15090 PUT #3,1:TMPOINT$=POINT0$    'Vektoren der bisher letzten Seite verbiegen
15100 LSET POINT2$=POINT0$                                'Verbiege Vektoren...
15110 PUT #4,CPOINT                                         '...der alten Seite
15120 LSET POINT1$=STR$(CPOINT)                             '...der neuen Seite
15130 LSET POINT2$="0000"                                                '.. ""
15140 CPOINT=VAL(TMPOINT$)                                       'Nr neue Seite
15150 PUT #4,CPOINT
15160 LSET DDD1$=""                                           'Write empty page
15170 LSET DDD2$=""
15180 LSET DDD3$="" 
15190 PUT #5,CPOINT
15200 DATEN$=SPACE$(20)
15210 FOR II=1 TO 26
15220   DAT$(II)=DATEN$
15230 NEXT
15240 RETURN
15250 '
15260 ' **** SEITE SPEICHERN **************************************************
15270 '
15280 DATEN$=""
15290 FOR II=1 TO 12
15300   DATEN$=DATEN$+DAT$(II)
15310 NEXT
15320 LSET DDD1$=DATEN$:DATEN$=""
15330 FOR II=13 TO 24
15340   DATEN$=DATEN$+DAT$(II)
15350 NEXT
15360 LSET DDD2$=DATEN$:DATEN$=""
15370 FOR II=25 TO 26
15380   DATEN$=DATEN$+DAT$(II)
15390 NEXT 
15400 LSET DDD3$=DATEN$:DATEN$=""
15410 PUT #5,CPOINT
15420 RETURN
15430 '
15440 ' **** HACK ! (ZERSTÜCKELN DER DATEN-STRINGS) ***************************
15450 '
15460 FOR II=1 TO 12
15470   DAT$(II)=MID$(DDD1$,(II-1)*20+1,20)
15480 NEXT
15490 FOR II=13 TO 24
15500   DAT$(II)=MID$(DDD2$,(II-13)*20+1,20)
15510 NEXT 
15520 FOR II=25 TO 26
15530   DAT$(II)=MID$(DDD3$,(II-25)*20+1,20)
15540 NEXT 
15550 RETURN
15560 '
15570 ' **** UPRO DATENAUSGABE VN  ********************************************
15580 '
15590 ' IN: MTGLZAHL
15600 ' IN: NACHVORNAME$
15610 ' IN: BLATT
15620 PRINT DELHIGH$;                                          'TODO Necessary?
15630 PRINT HIGHON$
15640 PRINT FNCUR$(5,16);MTGLZAHL
15650 PRINT FNCUR$(5,29);NACHVORNAME$
15660 PRINT FNCUR$(3,8);BLATT
15670 FOR K=1 TO 26
15680   HOR=6-39*(K>13)
15690   VER=6+K+13*(K>13)
15700   FOR J=1 TO 3                                                 'Von-Datum
15710     PRINT FNCUR$(VER,HOR+(J-1)*3);MID$(DAT$(K),-1+J*2,2)
15720   NEXT
15730   FOR J=1 TO 3                                                 'Bis-Datum
15740     PRINT FNCUR$(VER,HOR+13+(J-1)*3);MID$(DAT$(K),5+J*2,2)
15750   NEXT
15760   PRINT FNCUR$(VER,HOR+26);MID$(DAT$(K),13,8)                        'Amt
15770 NEXT
15780 PRINT HIGHOFF$
15790 RETURN
15800 '
15810 ' *** Verwendungsnachweis drucken ***************************************
15820 '
15830 ' IN: BLATT
15840 ' IN: MITGLZAHL
15850 ' IN: NACHVORNAME$
15860 Q$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 12710 'Ja/Nein-Frage TODO Back to main code?
15870 IF NOT ISJA THEN 16110                              'LW TODO Remove this!
15880 '
15890 RETURN                                         '*** TMP Print Disable ***
15900 I$=SPACE$(2)
15910 '
15920 PRINT STRICH$
15930 PRINT
15940 PRINT I$;"Blatt:";BLATT
15950 PRINT
15960 PRINT I$;"Mitglieds-Nr.:";MTGLZAHL;TAB(24);"Name:";NACHVORNAME$
15970 PRINT
15980 FOR K=1 TO 13
15990   KK=K:GOSUB 16080
16000   PRINT I$;"Von:";VON$;" Bis:";BIS$;" Amt:";AMT$;
16010   KK=K+13:GOSUB 16080
16020   PRINT " Von:";VON$;" Bis:";BIS$;" Amt:";AMT$
16030 NEXT 
16040 PRINT
16050 PRINT STRICH$
16060 RETURN
16070 'Format: VVVVVV BBBBBB AAAAAAAA
16080 VON$=LEFT$(DAT$(KK),2)+"."+MID$(DAT$(KK),3,2)+"."+MID$(DAT$(KK),5,2)
16090 BIS$=MID$(DAT$(KK),7,2)+"."+MID$(DAT$(KK),9,2)+"."+MID$(DAT$(KK),11,2)
16100 AMT$=RIGHT$(DAT$(KK),8)
16110 RETURN
16120 '
16130 ' ### Unterprogramme "Mitgliedsliste drucken" ###########################
16140 '
16150 ' *** Drucker bereit?-Frage *********************************************
16160 '
16170 ' OUT: ISJA (TRUE, FALSE)
16180 Q$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 12710 'Ja/Nein-Frage
16190 GOSUB 13390                                            'Nachricht löschen
16200 RETURN
16210 '
16220 ' *** Druck abbrechen?-Frage ********************************************
16230 '
16240 ' OUT: ISJA (TRUE, FALSE)
16250 ISJA=FALSE 
16260 IF INKEY$="" THEN 16300
16270 Q$="Drucken abbrechen?":GOSUB 12710                        'Ja/Nein-Frage
16280 IF ISJA THEN 16300
16290 N$="Mitgliedsliste wird gedruckt... Abbrechen mit Taste.":GOSUB 13250 'Nachricht
16300 RETURN
16310 '
16320 ' *** Mitgliedsliste drucken (Kopf) *************************************
16330 '
16340 ' IN: DATUM$
16350 PRINT TAB(16);"MITGLIEDSLISTE 25*******************************"
16360 PRINT 
16370 PRINT TAB(32);"Stand:"; DATUM$
16380 PRINT
16390 PRINT TAB(3);"Mitgl."; 
16400 PRINT TAB(10);"Name";
16410 PRINT TAB(36);"Vorname, Anschrift";
16420 PRINT TAB(63);"Status";
16430 PRINT TAB(71);"Amt"
16440 RETURN
16450 '
16460 ' *** Mitgliedsliste drucken (Mitglied) *********************************
16470 '
16480 ' IN:  MTGLZAHL
16490 AMT$=""
16500 IN=INSTR(DAT$(6),"22**"):IF IN>0 THEN AMT$=MID$(DAT$(6),IN,6):GOTO 16540
16510 IN=INSTR(DAT$(6),"23**"):IF IN>0 THEN AMT$=MID$(DAT$(6),IN,5):GOTO 16540
16520 IN=INSTR(DAT$(6),"24**"):IF IN>0 THEN AMT$=MID$(DAT$(6),IN,3)
16530 '
16540 PRINT
16550 PRINT TAB(5);MTGLZAHL;
16560 PRINT TAB(10);DAT$(1);                                          'Nachname
16570 PRINT TAB(36);DAT$(2);                                           'Vorname
16580 PRINT TAB(65);"(";LEFT$(DAT$(5),1);")";                           'Status
16590 PRINT TAB(71);AMT$                                                   'Amt
16600 PRINT TAB(36);DAT$(10)                                            'Straße
16610 PRINT TAB(36);DAT$(11);                                     'Postleitzahl
16620 PRINT TAB(41);DAT$(12)                                               'Ort
16630 PRINT TAB(36);"Tel.:";DAT$(13)                                   'Telefon
16640 PRINT
16650 RETURN
16660 '
16670 ' *** Adressaufkleber drucken *******************************************
16680 '
16690 I$=SPACE$(3)
16700 '
16710 PRINT I$;DAT$(7)                                                  'Anrede
16720 PRINT I$;DAT$(2);" ";DAT$(1)                            'Vorname Nachname
16730 PRINT
16740 PRINT I$;DAT$(10)                                      'Straße Hausnummer
16750 PRINT
16760 PRINT I$;DAT$(11);" ";DAT$(12)                          'Postleitzahl Ort
16770 PRINT
16780 PRINT
16790 PRINT
16800 PRINT
16810 PRINT
16820 PRINT
16830 RETURN
16840 '
16850 ' *** Stammsatz drucken (Bildschirmformat)
16860 '
16870 TMP1=8
16880 TMP2=43
16890 '
16900 PRINT STRICH$
16910 PRINT
16920 PRINT TAB(31);"Mitgliedsnummer";MTGLZAHL
16930 PRINT
16940 RESTORE 17000
16950 FOR K=1 TO 7:READ A,B
16960   IF A<>0 THEN PRINT TAB(TMP1);FENAM$(A);DAT$(A);
16970   IF B<>0 THEN PRINT TAB(TMP2);FENAM$(B);DAT$(B);
16980   PRINT
16990 NEXT K
17000 DATA 1,0,2,15,3,16,4,0,5,17,6,18,0,19
17010 FOR K=7 TO 14
17020   PRINT TAB(TMP1);FENAM$(K);DAT$(K);
17030   PRINT TAB(TMP2);FENAM$(K+13);DAT$(K+13);
17040 NEXT K
17050 PRINT
17060 PRINT
17070 PRINT
17080 PRINT STRICH$
17090 RETURN
17100 '
17110 ' *** Mitgliedsvermerke drucken
17120 '
17130 ' IN: VOLLNAME$
17140 I$=SPACE$(9)
17150 FORM1$="___"
17160 FORM2$="___ / ___"
17170 FORM3$="___ / ___ / ___"
17180 '
17190 PRINT TAB(10);"Mitgliedsnr.: ";FORM1$;
17200 PRINT TAB(28);"Name: ";VOLLNAME$;
17210 PRINT TAB(58);"Datum: ";FORM3$
17220 PRINT
17230 PRINT I$;"26*************************** ";FORM3$
17240 PRINT
17250 PRINT I$;"27*************************** ";FORM3$
17260 PRINT
17270 PRINT I$;"28*************************** ";FORM3$
17280 PRINT
17290 PRINT I$;"29*************************** ";FORM3$
17300 PRINT
17310 PRINT I$;"30*************************** ";FORM2$
17320 PRINT
17330 PRINT I$;"31*************************** ";FORM3$
17340 PRINT
17350 PRINT I$;"32*************************** ";FORM3$
17360 PRINT
17370 PRINT I$;"33*************************** ";FORM1$
17380 PRINT
17390 PRINT I$;"34*************************** ";FORM3$
17400 PRINT
17410 PRINT I$;"35*************************** ";FORM3$
17420 PRINT
17430 PRINT I$;"36*************************** ";FORM3$
17440 PRINT
17450 PRINT I$;"37*************************** ";FORM3$
17460 PRINT
17470 PRINT I$;"38*************************** ";FORM3$
17480 RETURN
17490 '
17500 ' ### HILFE-TEXT ########################################################
17510 '
17520 ' *** Fielden der Hilfe-Text-Dateien ************************************
17530 '
17540 OPEN "R",#6,"MVWHELP.NDX",4
17550   FIELD #6,2 AS XFIRSTLINE$,2 AS XLASTLINE$
17560   FOR K=1 TO HELPIDMAX
17570     GET #6,K
17580     HELPFIRSTLINE(K)=CVI(XFIRSTLINE$)
17590     HELPLASTLINE(K)=CVI(XLASTLINE$)
17600   NEXT K
17610 CLOSE #6
17620 '
17630 OPEN "R",#6,"MVWHELP.DAT",64
17640 FIELD #6,2 AS XINDENT$,2 AS XLENGTH$,60 AS XLINE$
17650 RETURN
17660 '
17670 ' *** Hilfe-Text-Seite anzeigen *****************************************
17680 '
17690 ' IN: HELPID
17700 SAVEMT$=MT$:MT$="HILFE":GOSUB 13460:MT$=SAVEMT$              'Basis-Maske
17710 PRINT HOME$
17720 FIRSTLINE=HELPFIRSTLINE(HELPID)
17730 LASTLINE=HELPLASTLINE(HELPID)
17740 FOR K=FIRSTLINE TO LASTLINE
17750   GET #6,K
17760   INDENT=CVI(XINDENT$)
17770	LENGTH=CVI(XLENGTH$)
17780   PRINT TAB(INDENT);LEFT$(XLINE$,LENGTH)
17790 NEXT K
17800 GOSUB 14030                                      'Rückkehr aus Hilfe-Text
17810 RETURN
17820 '
17830 ' ### PROGRAMMFEHLER, PROGRAMMENDE ######################################
17840 '
17850 ' *** Programmfehler ****************************************************
17860 '
17870 I$=SPACE$(15)
17880 GOSUB 13460                                                  'Basis-Maske
17890 PRINT HOME$
17900 PRINT I$;"Ein Programmfehler ist aufgetreten."
17910 PRINT   
17920 PRINT I$;"- Notieren Sie folgende Fehlerinformation:"
17930 PRINT I$;"  ";HIGHON$;"ERL";HIGHOFF$;" (Error Line)";HIGHON$;ERL;HIGHOFF$
17940 PRINT I$;"  ";HIGHON$;"ERR";HIGHOFF$;" (Error Code)";HIGHON$;ERR;HIGHOFF$
17950 PRINT I$;"- Drücken Sie am Rechner den Knopf hinten rechts."
17960 PRINT I$;"19**************************"
17970 '
17980 ' *** Programmende ******************************************************
17990 '
18000 PRINT FNCUR$(22,1);HIGHOFF$;CURON$;
18010 RESET
18020 'LW RUN "DISKMENU.EXE"
18030 END                                                           '1622 lines
