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
1280 BSP$   =CHR$(8)                                                 'Backspace
1290 DEL$   =BSP$+"."+BSP$                                              'Delete
1291 ESC$   =CHR$(27)                                 'Einleitung Escapesequenz
1292 IN$    =SPACE$(10)                                 'Indent für Hilfe-Texte
1293 STRICH$=STRING$(79,"-")                                            'Strich
1295 '
1310 CLS$    =ESC$+"E"                                      'Bildschirm löschen
1320 INVON$  =ESC$+"p"                                              'Invers ein
1330 INVOFF$ =ESC$+"q"                                              'Invers aus
1340 CURON$  =ESC$+"y5"                                             'Cursor ein
1350 CUROFF$ =ESC$+"x5"                                             'Cursor aus
1360 HIGHON$ =ESC$+"("                                           'Helle Schrift
1370 HIGHOFF$=ESC$+")"                                          'Dunkle Schrift
1380 DELLINE$=ESC$+"K"                                           'Zeile löschen
1390 DELHIGH$=ESC$+"+"                                   'Helle Schrift löschen
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
1760 ON ERROR GOTO 20050                               'Programmfehler abfangen
1770 '
1780 GOSUB 10050    'Tasten deaktivieren, ASCII-Codes für Cursortasten vergeben
1790 PRINT CUROFF$;
1800 '
1810 ' *** Beginn des Mitgliederverwaltungsprogramms **************************
1820 '
1830 MT$="MITGLIEDERVERWALTUNG":MI$="G-010000":GOSUB 13070         'Basis-Maske
1840 Q$="Bitte Daten-Diskette in Laufwerk B einlegen. Bereit?":GOSUB 12280 'Ja/Nein-Frage
1850 IF JA=0 THEN 20180                                           'Programmende
1860 '
1870 N$="Einen Moment bitte...":GOSUB 12940     'Nachricht. F-Tasten ausblenden
1880 GOSUB 10960                              'Stammsatzfelder-Definition lesen
1890 GOSUB 11770                                   'Fielden der Stammsatz-Datei
1900 GOSUB 13740                       'Fielden der Verwendungsnachweis-Dateien
1910 GOSUB 11230                                                 'MTGLMAX lesen
1920 GOSUB 13000                                             'Nachricht löschen
1930 '
1940 ' ################# Mitgliederverwaltungsmenü (MVW-Menü) #################
1950 '
1960 MT$="MITGLIEDERVERWALTUNG":MI$="G-010000":GOSUB 13070         'Basis-Maske
1970 FKEYS$="":GOSUB 13180                                 'F-Tasten einblenden
1980 VNFLAG=0                                                              '???
1990 PRINT FNCUR$(4,26);"1 - Stammsatzpflege"                         'Menütext
2000 PRINT FNCUR$(6,26);"2 - Verwendungsnachweis"
2010 PRINT FNCUR$(8,26);"3 - Mitgliedsliste drucken"
2020 FKEYMAX=4:NKEYMAX=3:GOSUB 13340                              'Menü-Eingabe
2040 IF NKEY THEN ON NKEY GOTO 2130,6060,6890
2050 ON FKEY GOTO 2060,2070,1960,2070
2060 GOSUB 17170:GOTO 1960
2070 GOSUB 13500:GOTO 2020                                'Zum Hauptmenü?-Frage
2080 '
2090 ' ########################### Stammsatzpflege ############################
2100 '
2110 ' *** Stammsatzpflege-Menü ***********************************************
2120 '
2130 MT$="STAMMSATZPFLEGE":MI$="G-011000":GOSUB 13070              'Basis-Maske
2140 FKEYS$="":GOSUB 13180                                 'F-Tasten einblenden
2150 PRINT FNCUR$(4,29);"1 - Stammsatz anlegen"
2160 PRINT FNCUR$(6,29);"2 - Stammsatz ändern"
2170 PRINT FNCUR$(8,29);"3 - Stammsatz löschen"
2180 PRINT FNCUR$(10,29);"4 - Stammsatz drucken"
2190 PRINT FNCUR$(12,29);"5 - Stammsatz suchen"
2200 FKEYMAX=4:NKEYMAX=5:GOSUB 13340                              'Menü-Eingabe
2210 IF NKEY THEN ON NKEY GOTO 2280,2730,3350,3780,4150
2220 ON FKEY GOTO 2230,1960,1960,2240
2230 GOSUB 17390:GOTO 2130
2240 GOSUB 13500:GOTO 2200                                'Zum Hauptmenü?-Frage
2250 '
2260 ' *** Stammsatz erfassen von 02************ ******************************
2270 '
2280 MTGLZAHL=MTGLMAX+1
2290 F=1
2291 GOSUB 11120                                                 'DAT$() leeren
2292 GOSUB 12730                                       'Voreinstellungen setzen
2300 MT$="STAMMSATZ ANLEGEN":MI$="G-011010":GOSUB 13070            'Basis-Maske
2310 FKEYS$="<F7>=Speichern":GOSUB 13180                   'F-Tasten einblenden
2340 GOSUB 11735                                      'Mitgliedsnummer ausgeben
2350 GOSUB 11650             'Feldnamen, Feldwerte und Feldplatzhalter ausgeben
2360 Z=FEROW(F):S=FECOL(F)+XOFF:LNG=FELNG(F):TYP=FETYP(F):MUSS=FEMUSS(F)
2370 FKEYMASK$="12347":CKEYMASK$="12":INTEXT$=DAT$(F)
2380 GOSUB 10370                                                  'Feld-Eingabe
2390 IF ASCC<>ASCRET THEN 2430                              'RETURN verarbeiten
2400   DAT$(F)=OUTTEXT$
2410   F=F+1:IF F>FELDMAX THEN F=1
2420   GOTO 2360
2430 IF CKEY=0 THEN 2480                              'Cursortasten verarbeiten
2440   PRINT FNCUR$(Z,S);HIGHON$;DAT$(F);HIGHOFF$;STRING$(LNG-LEN(DAT$(F)),".")
2450   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FELDMAX
2460   IF CKEY=CKEYDN THEN F=F+1:IF F>FELDMAX THEN F=1
2470   GOTO 2360
2480 IF FKEY=1 THEN GOSUB 17640:GOTO 2300                           'Hilfe-Text
2490 IF FKEY=2 THEN GOSUB 2570:GOTO 2130             'Exit "Stammsatz erfassen"
2500 IF FKEY=3 THEN GOSUB 2570:GOTO 1960             'Exit "Stammsatz erfassen"
2510 IF FKEY<>4 THEN 2560
2520   GOSUB 13580                              'Zum Hauptmenü?-Frage (einfach)
2530   IF JA=0 THEN GOSUB 13180:GOTO 2380                  'F-Tasten einblenden
2540   GOSUB 2570                                    'Exit "Stammsatz erfassen"
2550   GOTO 20180                                                 'Programmende
2560 IF FKEY=7 THEN GOSUB 2640:GOTO 2280           'Exit "Stammsatz erfassen" 2
2561 '
2562 ' *** Exit "Stammsatz erfassen"
2570 GOSUB 10910                                           'Tastatur entprellen
2580 Q$="Wollen Sie vorher den Stammsatz speichern?":GOSUB 12280 'Ja/Nein-Frage
2590 IF JA=1 THEN 2640
2600   N$="Stammsatz wird nicht gespeichert.":GOSUB 12940 'Nachricht. F-Tasten ausblenden
2610   MTGLZAHL=MTGLZAHL-1
2620   GOTO 2680
2630 '                                                            
2631 ' *** Exit "Stammsatz erfassen" 2
2640 N$="Stammsatz wird gespeichert...":GOSUB 12940 'Nachricht. F-Tasten ausblenden
2650 GOSUB 12080                                           'Stammsatz speichern
2660 GOSUB 13850                       'Erste Verwendungsnachweis-Seite anlegen
2670 MTGLMAX=MTGLZAHL:GOSUB 11320                             'MTGLMAX speichen
2680 GOSUB 13000                                             'Nachricht löschen
2690 RETURN
2700 ' 
2710 ' *** Stammsatz ändern von 03************ ********************************
2720 '
2730 MT$="STAMMSATZ ÄNDERN":MI$="G-011020":GOSUB 13070             'Basis-Maske
2740 FKEYS$="":GOSUB 13180                                 'F-Tasten einblenden
2750 PRINT FNCUR$(4,24)"Bitte geben Sie die Mitgliedsnummer" 
2760 PRINT FNCUR$(5,25)"des zu ändernden Stammsatzes ein."
2770 PRINT FNCUR$(6,23);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
2780 PRINT FNCUR$(7,24)"Beenden Sie die Eingabe mit RETURN."
2790 PRINT FNCUR$(9,39)"=>..."
2800 BADINPUT=0
2810 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
2820 Z=9:S=41:LNG=3:TYP=2:MUSS=1
2830 GOSUB 10380                                                'Feld-Eingabe+1
2840 IF FKEY=1 THEN GOSUB 17850:GOTO 2730 'Hilfe-Text 'TODO: Wert geht verloren
2850 IF FKEY=2 THEN 2130
2860 IF FKEY=3 THEN 1960
2870 IF FKEY=4 THEN GOSUB 13500:GOTO 2830                 'Zum Hauptmenü?-Frage
2880 '
2890 M=VAL(OUTTEXT$)                                        'RETURN verarbeiten
2900 IF M>=1 AND M<=MTGLMAX THEN 2950
2910   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 12860         'Nachricht
2920   BADINPUT=1
2930   INTEXT$=OUTTEXT$
2940   GOTO 2820
2950 MTGLZAHL=M
2960 N$="Stammsatz wird gelesen...":GOSUB 12940 'Nachricht. F-Tasten ausblenden
2970 GOSUB 11830                                               'Stammsatz lesen
2980 GOSUB 13000                                             'Nachricht löschen
2990 '
3000 GOSUB 13070                                                   'Basis-Maske
3010 FKEYS$="<F7>=Speichern":GOSUB 13180                   'F-Tasten einblenden
3020 GOSUB 11735                                      'Mitgliedsnummer ausgeben
3030 GOSUB 11650             'Feldnamen, Feldwerte und Feldplatzhalter ausgeben
3040 F=1
3050 Z=FEROW(F):S=FECOL(F)+XOFF:LNG=FELNG(F):TYP=FETYP(F):MUSS=FEMUSS(F)
3060 FKEYMASK$="12347":CKEYMASK$="12":INTEXT$=DAT$(F)
3070 GOSUB 10370                                                  'Feld-Eingabe
3080 IF ASCC<>ASCRET THEN 3120
3090   DAT$(F)=OUTTEXT$
3100   F=F+1:IF F>FELDMAX THEN F=1
3110   GOTO 3050
3120 IF CKEY=0 THEN 3170
3130   PRINT FNCUR$(Z,S);HIGHON$;DAT$(F);HIGHOFF$;STRING$(LNG-LEN(DAT$(F)),".")
3140   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FELDMAX
3150   IF CKEY=CKEYDN THEN F=F+1:IF F>FELDMAX THEN F=1
3160   GOTO 3050
3170 IF FKEY=1 THEN GOSUB 17960:GOTO 3000                           'Hilfe-Text
3180 IF FKEY=2 THEN GOSUB 3260:GOTO 2730               'Exit "Stammsatz ändern"
3190 IF FKEY=3 THEN GOSUB 3260:GOTO 1960               'Exit "Stammsatz ändern"
3191 IF FKEY<>4 THEN 3200 
3192   GOSUB 13580                              'Zum Hauptmenü?-Frage (einfach)
3193   IF JA=0 THEN GOSUB 13180:GOTO 3070                  'F-Tasten einblenden
3194   GOSUB 3260                                      'Exit "Stammsatz ändern"
3195   GOTO 20180                                                 'Programmende
3200 IF FKEY=7 THEN GOSUB 3280:GOTO 2730             'Exit "Stammsatz ändern" 2
3250 '
3251 ' *** Exit "Stammsatz ändern"
3260 Q$="Wollen Sie den Stammsatz speichern?":GOSUB 12280        'Ja/Nein-Frage
3270 IF JA=0 THEN RETURN
3271 '
3272 ' *** Exit "Stammsatz ändern" 2
3280 N$="Stammsatz wird gespeichert...":GOSUB 12940 'Nachricht. F-Tasten ausblenden
3290 GOSUB 12080                                           'Stammsatz speichern
3300 GOSUB 13000                                             'Nachricht löschen
3310 RETURN
3320 '
3330 ' *** Stammsatz löschen von 04********** *********************************
3340 '
3350 MT$="STAMMSATZ LÖSCHEN":MI$="G-011030":GOSUB 13070            'Basis-Maske
3360 FKEYS$="":GOSUB 13180                                 'F-Tasten einblenden
3370 PRINT FNCUR$(4,24);"Bitte geben Sie die Mitgliedsnummer"         'Menütext
3380 PRINT FNCUR$(5,25);"des zu löschenden Mitglieds ein."
3390 PRINT FNCUR$(6,23);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
3400 PRINT FNCUR$(7,15);"(Dabei wird nur der Status des Mitglieds verändert.)"
3410 PRINT FNCUR$(8,24);"Beenden Sie die Eingabe mit RETURN."
3420 PRINT FNCUR$(10,39);"=>..."
3430 BADINPUT=0
3440 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
3450 Z=10:S=41:LNG=3:TYP=2:MUSS=1
3460 GOSUB 10380                                                'Feld-Eingabe+1
3470 IF FKEY=1 THEN GOSUB 18140:GOTO 3350 'Hilfe-Text 'TODO: Wert geht verloren
3480 IF FKEY=2 THEN 2130
3490 IF FKEY=3 THEN 1960
3500 IF FKEY=4 THEN GOSUB 13500:GOTO 3460                 'Zum Hauptmenü?-Frage
3510 '
3520 M=VAL(OUTTEXT$)                                        'RETURN verarbeiten
3530 IF M>=1 AND M<=MTGLMAX THEN 3580
3540   N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12860       'Nachricht
3550   BADINPUT=1
3560   INTEXT$=OUTTEXT$
3570   GOTO 3450
3580 MTGLZAHL=M
3590 N$="Stammsatz wird gelesen...":GOSUB 12940 'Nachricht. F-Tasten ausblenden
3600 GOSUB 11830                                               'Stammsatz lesen
3610 GOSUB 13000                                             'Nachricht löschen
3620 '
3630 GOSUB 13070                                                   'Basis-Maske
3640 GOSUB 11735                                      'Mitgliedsnummer ausgeben
3650 GOSUB 11650             'Feldnamen, Feldwerte und Feldplatzhalter ausgeben
3660 Q$="Mitglied endgültig löschen?":GOSUB 12280                'Ja/Nein-Frage
3670 IF JA=0 THEN 3740
3680   DAT$(5)="ausgetreten"
3690   PRINT FNCUR$(FEROW(5),FECOL(5)+XOFF);HIGHON$;DAT$(5);HIGHOFF$;
3700   PRINT STRING$(FELNG(5)-LEN(DAT$(5)),".")   
3710   N$="Stammsatz wird gespeichert...":GOSUB 12940 'Nachricht. F-Tasten ausblenden
3720   GOSUB 12080                                         'Stammsatz speichern
3730   GOSUB 13000                                           'Nachricht löschen
3740 GOTO 3350
3750 '
3760 ' *** Stammsatz drucken von 02************ & 04********** ****************
3770 '
3780 MT$="STAMMSATZ DRUCKEN":MI$="G-011040":GOSUB 13070            'Basis-Maske
3790 FKEYS$="":GOSUB 13180                                 'F-Tasten einblenden
3800 PRINT FNCUR$(4,24)"Bitte geben Sie die Mitgliedsnummer"
3810 PRINT FNCUR$(5,23)"des auszudruckenden Stammsatzes ein."
3820 PRINT FNCUR$(6,23);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
3830 PRINT FNCUR$(7,24)"Beenden Sie die Eingabe mit RETURN."
3840 PRINT FNCUR$(9,39);"=>..."
3850 BADINPUT=0
3860 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
3870 Z=9:S=41:LNG=3:TYP=2:MUSS=1
3880 GOSUB 10380                                                'Feld-Eingabe+1
3890 IF FKEY=1 THEN GOSUB 18235:GOTO 3780 'Hilfe-Text 'TODO: Wert geht verloren
3900 IF FKEY=2 THEN 2130
3910 IF FKEY=3 THEN 1960
3920 IF FKEY=4 THEN GOSUB 13500:GOTO 3880                 'Zum Hauptmenü?-Frage
3930 '
3940 M=VAL(OUTTEXT$)                                        'RETURN verarbeiten
3950   IF M>=1 AND M<=MTGLMAX THEN 4000
3960   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 12860         'Nachricht
3970   BADINPUT=1
3980   INTEXT$=OUTTEXT$                  
3990   GOTO 3870
4000 MTGLZAHL=M
4010 N$="Stammsatz wird gelesen...":GOSUB 12940 'Nachricht. F-Tasten ausblenden
4020 GOSUB 11830                                               'Stammsatz lesen
4030 GOSUB 13000                                             'Nachricht löschen
4040 '
4050 GOSUB 13070                                                   'Basis-Maske
4060 GOSUB 11735                                      'Mitgliedsnummer ausgeben
4070 GOSUB 11650             'Feldnamen, Feldwerte und Feldplatzhalter ausgeben
4080 GOSUB 15800                                         'Drucker bereit?-Frage
4090 IF JA=0 THEN 4110
4100   GOSUB 16490                        'Stammsatz drucken (Bildschirmformat)
4110 GOTO 3780
4120 '
4130 ' *** Stammsatz suchen (und drucken) von 05*************** ***************
4140 '
4150 DRUCKFLAG=0
4160 MT$="STAMMSATZ SUCHEN":IF DRUCKFLAG THEN MT$="SUCHEN & DRUCKEN"
4170 MI$="G-011050":IF DRUCKFLAG THEN MI$="G-013010"
4180 GOSUB 13070                                                   'Basis-Maske
4190 FKEYS$="":GOSUB 13180                                 'F-Tasten einblenden
4200 TMP$="Suchen":IF DRUCKFLAG THEN TMP$="Drucken" 
4210 PRINT FNCUR$(4,30)"1 - ";TMP$;" nach Nummern"
4220 PRINT FNCUR$(6,30)"2 - ";TMP$;" nach Maske"
4230 FKEYMAX=4:NKEYMAX=2:GOSUB 13340                              'Menü-Eingabe
4231 IF NKEY THEN ON NKEY GOTO 4340,4910
4240 IF FKEY<>1 THEN 4270 
4250   IF DRUCKFLAG THEN GOSUB 19490 ELSE GOSUB 18280               'Hilfe-Text
4260   GOTO 4160
4270 IF FKEY=2 THEN IF DRUCKFLAG THEN 6890 ELSE 2130
4280 IF FKEY=3 THEN 1960
4290 IF FKEY=4 THEN GOSUB 13500:GOTO 4230                 'Zum Hauptmenü?-Frage
4310 '
4320 ' *** Suchen nach Nummern
4340 TMP$=STR$(1):NUMSUCH$(1)=MID$(TMP$,2,99)
4350 TMP$=STR$(MTGLMAX):NUMSUCH$(2)=MID$(TMP$,2,99)
4360 IF DRUCKFLAG=0 THEN MT$="STAMMSATZ SUCHEN NACH NUMMERN":MI$="G-011051"
4370 IF DRUCKFLAG=1 THEN MT$="SUCHEN & DRUCKEN NACH NUMMERN":MI$="G-013011"
4380 GOSUB 13070                                                   'Basis-Maske
4390 FKEYS$="<F5>=Suchen":IF DRUCKFLAG THEN FKEYS$="<F5>=Suchen & Drucken"
4400 GOSUB 13180                                           'F-Tasten einblenden
4410 TMP$="gesucht":IF DRUCKFLAG THEN TMP$="gesucht und gedruckt"
4411 TMP1=13:IF DURCKFLAG THEN TMP1=6
4412 TMP2=67:IF DURCKFLAG THEN TMP2=73
4420 PRINT FNCUR$(4,25);"Gültige Mitgliedsnummern: 1 -";STR$(MTGLMAX)
4430 PRINT FNCUR$(6,TMP1);"Ab welcher Mitgliedsnummer soll ";TMP$;" werden?     =>..."
4440 PRINT FNCUR$(8,TMP1);"Bis zu welcher Mitgliedsnummer soll ";TMP$;" werden? =>..."
4450 PRINT FNCUR$(6,TMP2);HIGHON$;NUMSUCH$(1);HIGHOFF$
4460 PRINT FNCUR$(8,TMP2);HIGHON$;NUMSUCH$(2);HIGHOFF$
4470 F=1
4480 BADINPUT=0
4490 FKEYMASK$="12345":CKEYMASK$="":INTEXT$=NUMSUCH$(F)
4500 Z=NUMSUCHROW(F):S=67:LNG=3:TYP=2:MUSS=1
4510 GOSUB 10380                                                'Feld-Eingabe+1
4520 IF FKEY<>1 THEN 4550                                           'Hilfe-Text
4530  IF DRUCKFLAG THEN GOSUB 19570 ELSE GOSUB 18520
4540  GOTO 4360                                     'TODO: Werte gehen verloren
4550 IF FKEY=2 THEN 4160
4560 IF FKEY=3 THEN 1960
4570 IF FKEY=4 THEN GOSUB 13500:GOTO 4510                 'Zum Hauptmenü?-Frage
4580 IF FKEY<>5 THEN 4630
4590   SEARCH=BYNUMS
4600   NUMSUCH1=VAL(NUMSUCH$(1))
4610   NUMSUCH2=VAL(NUMSUCH$(2))
4620   IF DRUCKFLAG THEN 7680 ELSE 5220           'Suchen oder Suchen & Drucken
4621 '
4630 M=VAL(OUTTEXT$)                                        'RETURN verarbeiten
4640 IF M>=1 AND M<=MTGLMAX THEN 4680
4650   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 12860         'Nachricht
4660   GOSUB 4820                                     'Eingabefehler-Behandlung
4670   GOTO 4500
4680 NUMSUCH$(F)=OUTTEXT$
4690 NUMSUCH1=VAL(NUMSUCH$(1))
4700 NUMSUCH2=VAL(NUMSUCH$(2))
4710 IF NUMSUCH1<=NUMSUCH2 THEN 4750
4720   N$="Ungültiger Bereich":GOSUB 12860                           'Nachricht
4730   GOSUB 4820                                     'Eingabefehler-Behandlung
4740   GOTO 4500
4750 F=F+1:IF F>NUMSUCHMAX THEN F=1
4751 GOSUB 13440                                           'F-Tasten ausblenden
4760 FKEYMASK$="12345"                                           'TODO: obsolet
4770 FKEYS$="<F5>=Suchen":IF DRUCKFLAG THEN FKEYS$="<F5>=Suchen & Drucken" 
4790 GOSUB 13180                                           'F-Tasten einblenden
4800 GOTO 4490
4810 '
4811 ' *** Eingabefehler-Behandlung
4820 BADINPUT=1
4830 INTEXT$=OUTTEXT$
4840 FKEYMASK$="1234"
4850 GOSUB 13440                                           'F-Tasten ausblenden
4860 FKEYS$="":GOSUB 13180                                 'F-Tasten einblenden
4870 RETURN
4880 '
4890 ' *** Suchen nach Maske
4910 GOSUB 11170                                                'SUCH$() leeren
4920 IF DRUCKFLAG=0 THEN MT$="STAMMSATZ SUCHEN NACH MASKE":MI$="G-011052"
4930 IF DRUCKFLAG=1 THEN MT$="SUCHEN & DRUCKEN NACH MASKE":MI$="G-013012"
4940 GOSUB 13070                                                   'Basis-Maske
4950 FKEYS$="<F5>=Suchen":IF DRUCKFLAG THEN FKEYS$="<F5>=Suchen & Drucken" 
4960 GOSUB 13180                                           'F-Tasten einblenden
4970 GOSUB 11410                        'Feldnamen und Feldplatzhalter ausgeben
4980 F=1
4990 Z=FEROW(F):S=FECOL(F)+XOFF:LNG=FELNG(F):TYP=3
5000 FKEYMASK$="12345":CKEYMASK$="12":INTEXT$=""
5010 GOSUB 10370                                                  'Feld-Eingabe
5020 IF FKEY<>1 THEN 5050 
5030   IF DRUCKFLAG THEN GOSUB 19650 ELSE GOSUB 18600
5040   GOTO 4920
5050 IF FKEY=2 THEN 4160
5060 IF FKEY=3 THEN 1960
5070 IF FKEY=4 THEN GOSUB 13500:GOTO 5010                 'Zum Hauptmenü?-Frage
5080 IF FKEY<>5 THEN 5110 
5090   SEARCH=BYMASK                                                      '<F5>
5100   IF DRUCKFLAG THEN 7680 ELSE 5220           'Suchen oder Suchen & Drucken
5110 IF CKEY=0 THEN 5160                              'Cursortasten verarbeiten
5120   PRINT FNCUR$(Z,S);HIGHON$;SUCH$(F);HIGHOFF$;STRING$(LNG-LEN(SUCH$(F)),".")
5130   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FELDMAX
5140   IF CKEY=CKEYDN THEN F=F+1:IF F>FELDMAX THEN F=1
5150   GOTO 4990
5160 SUCH$(F)=OUTTEXT$                                      'RETURN verarbeiten
5170 F=F+1:IF F>FELDMAX THEN F=1
5180 GOTO 4990
5190 '
5200 ' *** Suche
5220 GOSUB 13070                                                   'Basis-Maske
5230 FKEYS$="<F5>=Suchen rückwärts <F6>=Suchen vorwärts <F7>=Adressaufkleber" 
5240 GOSUB 13180                                           'F-Tasten einblenden
5250 PRINT FNCUR$(3,31);"Mitgliedsnummer"
5260 GOSUB 11410                        'Feldnamen und Feldplatzhalter ausgeben
5270 '
5280 IF SEARCH=BYMASK THEN 5330
5290 '
5300 MTGLZAHL=NUMSUCH1                      'Initialisierung Suche nach Nummern
5310 GOTO 5450
5320 '
5330 MTGLZAHL=0                               'Initialisierung Suche nach Maske
5340 GOSUB 6014                        'Nächsten Treffer suchen IN/OUT:MTGLZAHL
5350 IF MTGLZAHL<>-1 THEN 5450
5360   N$="Kein Suchtreffer. Mit <F2> neue Suche.":GOSUB 12860       'Nachricht
5370   FKEYS$="":GOSUB 13180                               'F-Tasten einblenden
5380   FKEYMASK$="1234":NKEYMASK="":GOSUB 13340                   'Menü-Eingabe
5390   IF FKEY<>1 THEN 5420
5400     IF DRUCKFLAG THEN GOSUB 19730 ELSE GOSUB 18680             'Hilfe-Text
5410     GOTO 4920
5420   IF FKEY=2 THEN 4920
5430   IF FKEY=3 THEN 1960
5440   IF FKEY=4 THEN GOSUB 13500:GOTO 5380               'Zum Hauptmenü?-Frage
5441 '
5450 GOSUB 5900                                 'Suchtreffer lesen und anzeigen
5460 '
5470 FKEMAX=7:NKEYMAX=0:GOSUB 13340                               'Menü-Eingabe
5480 ON FKEY GOTO 5500,5530,1960,5550,5580,5730,5960
5481 '
5490 ' *** <F1> Hilfe
5500 IF DRUCKFLAG THEN GOSUB 19810 ELSE GOSUB 18760                 'Hilfe-Text
5510 GOTO 5220 'TODO <F6><F6><F1> -> wieder beim 3 Treffer? 'TODO: Werte gehen verloren
5511 '
5520 ' *** <F2> Zurück
5530 IF SEARCH=BYNUMS THEN 4340 ELSE 4920           'TODO: Werte gehen verloren
5531 '
5540 ' *** <F4> Hauptmenü
5550 GOSUB 13500:GOTO 5470                                'Zum Hauptmenü?-Frage
5551 '
5570 ' *** <F5> Suchen rückwärts
5580 IF SEARCH=BYMASK THEN 5640
5600   MTGLZAHL=MTGLZAHL-1                        'Suchen rückwärts nach Nummer
5610   IF MTGLZAHL<NUMSUCH1 THEN 5700
5620     GOSUB 5900:GOTO 5470                   'Suchtreffer lesen und anzeigen
5640 TMP=MTGLZAHL                                  'Suchen rückwärts nach Maske
5650 GOSUB 6006                      'Vorherigen Treffer suchen IN/OUT:MTGLZAHL
5660 IF MTGLZAHL=-1 THEN 5690
5670   GOSUB 5900:GOTO 5470                     'Suchtreffer lesen und anzeigen
5680 '
5690 MTGLZAHL=TMP                                               'No match found
5700 N$="Erster Suchtreffer":GOSUB 12860                             'Nachricht
5710 GOTO 5470
5711 '
5720 ' *** <F6> Suchen vorwärts
5730 IF SEARCH=BYMASK THEN 5790
5750   MTGLZAHL=MTGLZAHL+1                         'Suchen vorwärts nach Nummer
5760   IF MTGLZAHL>NUMSUCH2 THEN 5850
5770     GOSUB 5900:GOTO 5470                   'Suchtreffer lesen und anzeigen
5780 '
5790 TMP=MTGLZAHL                                    'Suche vorwärts nach Maske
5800 GOSUB 6014                        'Nächsten Treffer suchen IN/OUT:MTGLZAHL
5810 IF MTGLZAHL=-1 THEN 5840
5820   GOSUB 5900:GOTO 5470                     'Suchtreffer lesen und anzeigen
5830 '
5840 MTGLZAHL=TMP                                               'No match found
5850 N$="Letzter Suchtreffer":GOSUB 12860                            'Nachricht
5860 GOTO 5470
5870 '
5880 ' *** Suchtreffer lesen und anzeigen
5900 GOSUB 13000                                             'Nachricht löschen
5910 GOSUB 11830                                               'Stammsatz lesen
5920 GOSUB 11530                                            'Feldwerte ausgeben
5930 PRINT FNCUR$(3,46);HIGHON$;STR$(MTGLZAHL);HIGHOFF$
5940 RETURN
5941 '
5950 ' *** <F7> Adressaufkleber drucken
5960 GOSUB 15800                                         'Drucker bereit?-Frage
5970 IF JA=0 THEN 6002                              'TODO: F-Tasten einblenden?
5980   N$="Adressaufkleber wird gedruckt...":GOSUB 12860             'Nachricht
5990   GOSUB 16310                                     'Adressaufkleber drucken
6000   GOSUB 13000                                           'Nachricht löschen
6001   GOSUB 13180                                         'F-Tasten einblenden
6002 GOTO 5470                                                    'Neue Eingabe
6003 '
6004 ' *** Subroutine Vorherigen Treffer suchen
6005 ' IN: MTGLZAHL OUT: MTGLZAHL or -1 if not found
6006 MTGLZAHL=MTGLZAHL-1
6007 IF MTGLZAHL<1 THEN 6020
6008 GOSUB 6024
6009 IF NOT ISMATCH THEN 6006 
6010 RETURN
6011 '
6012 ' *** Subroutine Nächsten Treffer suchen
6013 ' IN: MTGLZAHL OUT: MTGLZAHL or -1 if not found
6014 MTGLZAHL=MTGLZAHL+1
6015 IF MTGLZAHL>MTGLMAX THEN 6020
6016 GOSUB 6024
6017 IF NOT ISMATCH THEN 6014 
6018 RETURN
6019 '
6020 MTGLZAHL=-1
6021 RETURN
6022 '
6023 ' *** Subroutine Treffer?
6024 GOSUB 11830                                               'Stammsatz lesen
6025 FOR K=1 TO FELDMAX
6026   IF LEFT$(SUCH$(K),1)<>"*" THEN 6029
6027     IF INSTR(DAT$(K),RIGHT$(SUCH$(K),LEN(SUCH$(K))-1))>0 THEN 6034
6028     GOTO 6038
6029   I=1
6030   IF MID$(SUCH$(K),I,1)="?" THEN I=I+1:GOTO 6030
6031   I=I-1
6032   IF INSTR(DAT$(K),RIGHT$(SUCH$(K),LEN(SUCH$(K))-I))=I+1 THEN 6034
6033   GOTO 6038
6034 NEXT K
6035 ISMATCH=1
6036 RETURN
6037 '
6038 ISMATCH=0
6039 RETURN
6057 '
6058 ' ######################### Verwendungsnachweis ##########################
6059 '
6060 GOSUB 11230                                                 'MTGLMAX lesen
6070 VNFLAG=-1
6080 '
6090 ' Verwendungsnachweis-Menü ***********************************************
6100 '
6110 MT$="VERWENDUNGSNACHWEIS":MI$="G-012000":GOSUB 13070          'Basis-Maske
6120 F=4                                                                'TODO ?
6130 FKEYS$="":GOSUB 13180                                 'F-Tasten einblenden
6140 PRINT CURON$;
6150 PRINT HIGHOFF$;
6160 PRINT FNCUR$(4,20)"   Bitte geben Sie die Mitgliedsnummer"
6170 PRINT FNCUR$(5,20)"des zu ändernden Verwendungsnachweises ein"
6180 'LW PRINT FNCUR$(6,23);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
6190 PRINT FNCUR$(6,20)" und beenden Sie die Eingabe mit RETURN."
6200 PRINT FNCUR$(8,20)"                 =>..."
6210 Z=8:S=39:LNG=3:TYP=2
6220 GOSUB 10370                                                  'Feld-Eingabe
6230 IF FKEY=1 THEN GOSUB 18960:GOTO 6110                           'Hilfe-Text
6240 IF FKEY=2 OR FKEY=3 THEN GOSUB 11120:VNFLAG=0:GOTO 1960     'DAT$() leeren
6250 IF FKEY=4 THEN GOSUB 13500                           'Zum Hauptmenü?-Frage
6260 IF FKEY>4 THEN 6210
6270 IF CKEY<>0 THEN 6210
6280 IF LEN(OUTTEXT$)=0 THEN N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 12860:GOTO 6200 ELSE MITGLIED=VAL(OUTTEXT$):IF MITGLIED=0 THEN 6220 'Nachricht
6290 IF MITGLIED>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12860:PRINT HOME$;HIGHOFF$:GOTO 6200 'Nachricht
6300 MTGLZAHL=MITGLIED:MTGL$=STR$(MITGLIED)
6310 '
6320 ' *** Lesen des Mitgliedsnamens ******************************************
6330 '
6340 PRINT FNCUR$(21,1);DELLINE$;
6350 GET #2,MITGLIED                                           'DATEN STS lesen
6360 NNAM$=LEFT$(DD1$,25)                                 'NACHNAME RAUSFISCHEN
6370 VNAM$=MID$(DD1$,26,25)                               'VORNAME  RAUSFISCHEN
6380 '
6390 ' ** LEERZEICHEN HINTER NACHNAME ABSCHNEIDEN *****************************
6400 '
6410 FOR I=LEN(NNAM$) TO 1 STEP -1
6420   IF MID$(NNAM$,I,1)<>" " THEN NNAM$=LEFT$(NNAM$,I):I=1
6430 NEXT
6440 NAMM$=NNAM$+" "+VNAM$
6450 GOSUB 14020                                             '1. VN-BLATT lesen
6460 BLATT=1
6470 VP=1:HP=1                                                 'INITIALISIERUNG
6480 F=8
6490 GOSUB 13070                                                   'Basis-Maske
6500 FKEYS$="<F7>=Speichern":GOSUB 13180                   'F-Tasten einblenden
6510 PRINT HIGHOFF$;
6520 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=Vorwärts";TAB(53);"<F9>=Blatt drucken";
6530 PRINT FNCUR$(3,2);"Blatt:"
6540 PRINT FNCUR$(5,2);"Mitglieds-Nr.:"
6550 PRINT FNCUR$(5,24);"Name:"
6560 MSK$="Von:  .  .   Bis:  .  .   Amt:"
6570 FOR KK=7 TO 19 
6580   PRINT FNCUR$(KK,2);MSK$;TAB(43);MSK$                    'LW NO semicolon
6590 NEXT
6600 GOSUB 15230                                                 'Ausgabe Daten
6610 LNG=2-(HP=7)*6                                    'BEGIN OF THE INPUT-LOOP
6620 TYP=2-(HP=7)
6630 Z=VP+13*(VP>13)+6
6640 S=6-39*(VP>13)-(HP<7)*(HP-1)*3+(HP>3)*(HP<7)*4-(HP=7)*26
6650 GOSUB 13000                                             'Nachricht löschen
6660 GOSUB 10370                                                  'Feld-Eingabe
6670 IF HP<7 THEN IF ASCC=ASCRET THEN OUTTEXT$=STRING$(2-LEN(OUTTEXT$),"0")+OUTTEXT$   '+NULLEN
6680 IF CKEY<>0 OR ASCC=ASCRET THEN PRINT FNCUR$(Z,S);OUTTEXT$
6690 IF ASCC=ASCRET THEN IF HP<7 THEN MID$(DAT$(VP),-1+2*HP,2)=OUTTEXT$ ELSE DAT$(VP)=LEFT$(DAT$(VP),12)+OUTTEXT$+SPACE$(8-LEN(OUTTEXT$))
6700 IF ASCC=ASCRET AND HP<7 THEN JA=1:TEST$=OUTTEXT$:ON HP GOSUB 12620,12650,12680,12620,12650,12680:IF JA=0 THEN N$="Ungültiges Datum":GOSUB 12860:GOTO 6660 'Datums-Check 'Nachricht
6710 IF ASCC=ASCRET THEN IF HP=7 THEN HP=1:VP=VP-(VP<26)-(VP=26)*(-25):GOTO 6610 ELSE HP=HP+1:GOTO 6610     'NEUE SCREEN-POSITION
6720 IF CKEY<>0 THEN TEST$=MID$(DAT$(VP),-(HP<7)*2*HP-1-(HP=7)*14,-(HP<7)*2-(HP=7)*8):PRINT FNCUR$(Z,S);TEST$
6730 IF CKEY<>0 THEN IF HP<7 AND NOT TEST$="  " THEN JA=1:ON HP GOSUB 12620,12650,12680,12620,12650,12680:IF JA=0 THEN N$="Ungültiges Datum":GOSUB 12860:GOTO 6660 'Nachricht
6740 IF CKEY=CKEYDN THEN VP=VP-(VP<26)-(VP=26)*(-25):GOTO 6610
6750 IF CKEY=CKEYUP THEN VP=VP+(VP>1)-(VP=1)*25     :GOTO 6610
6760 IF CKEY=CKEYRT THEN HP=HP-(HP<7)-(HP=7)*(-6)   :GOTO 6610
6770 IF CKEY=CKEYLT THEN HP=HP+(HP>1)+(HP=1)*(-6)   :GOTO 6610
6780 IF FKEY=1 THEN GOSUB 19070 :GOTO 6470                          'Hilfe-Text
6790 IF FKEY=2 THEN 6110                                       'ZU MTGL-EINGABE
6800 IF FKEY=3 THEN VNFLAG=0:GOSUB 11120:GOTO 1960               'DAT$() leeren
6810 IF FKEY=4 THEN GOSUB 13500:GOSUB 13240:GOTO 6660     'Zum Hauptmenü?-Frage
6820 IF FKEY=5 THEN GOSUB 14110:GOTO 6610
6830 IF FKEY=6 THEN GOSUB 14250:GOTO 6610
6840 IF FKEY=7 THEN GOSUB 14890:GOTO 6660                 'SPEICHERN DANN INPUT
6850 IF FKEY=9 THEN GOSUB 15470:GOSUB 14440:GOTO 6650
6860 '
6870 ' ############# Mitgliedsliste drucken von 05*************** #############
6880 '
6890 MT$="MITGLIEDSLISTE DRUCKEN":MI$="G-013000":GOSUB 13070       'Basis-Maske
6900 FKEYS$="":GOSUB 13180                                 'F-Tasten einblenden
6910 PRINT FNCUR$(4,28);"1 - Suchen & Drucken"
6920 PRINT FNCUR$(6,28);"2 - Komplett drucken"
6930 PRINT FNCUR$(8,28);"3 - Datenblatt drucken"
6940 FKEYMAX=4:NKEYMAX=3:GOSUB 13340                              'Menü-Eingabe
6941 IF NKEY THEN ON NKEY GOTO 7030,7030,8380
6950 IF FKEY=1 THEN GOSUB 19260:GOTO 6890                           'Hilfe-Text
6960 IF FKEY=2 THEN 1960
6970 IF FKEY=3 THEN 1960
6980 IF FKEY=4 THEN GOSUB 13500:GOTO 6940                 'Zum Hauptmenü?-Frage
7000 '
7010 ' *** Suchen & Drucken, Komplett drucken *********************************
7020 '
7030 IF NKEY=1 THEN MT$="SUCHEN & DRUCKEN":MI$="G-013010"
7040 IF NKEY=2 THEN MT$="KOMPLETT DRUCKEN":MI$="G-013020"
7050 GOSUB 13070                                                   'Basis-Maske
7060 GOSUB 13180                                           'F-Tasten einblenden
7070 PRINT FNCUR$(4,21);"Bitte geben Sie das heutige Datum ein."
7080 PRINT FNCUR$(5,22);"Beenden Sie die Eingabe mit RETURN."
7090 PRINT FNCUR$(7,35);"=>........"
7100 BADINPUT=0
7110 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
7120 Z=7:S=37:LNG=8:TYP=5:MUSS=1
7130 GOSUB 10380                                                'Feld-Eingabe+1
7140 IF FKEY=1 THEN GOSUB 19890:GOTO 7030 'Hilfe-Text 'TODO: Wert geht verloren
7150 IF FKEY=2 THEN 6890
7160 IF FKEY=3 THEN 1960
7170 IF FKEY=4 THEN GOSUB 13500:GOTO 7130                 'Zum Hauptmenü?-Frage
7180 '
7190 POS1=INSTR(OUTTEXT$,".")                               'RETURN verarbeiten
7200 POS2=INSTR(POS1+1,OUTTEXT$,".")
7210 IF POS2=0 THEN 7450
7220 POS3=INSTR(POS2+1,OUTTEXT$,".")
7230 IF POS3<>0 THEN 7450
7240 D1$=LEFT$(OUTTEXT$,POS1-1)
7250 IF LEN(D1$)=0 OR LEN(D1$)>2 THEN 7450
7260 D2$=MID$(OUTTEXT$,POS1+1,POS2-POS1-1)
7270 IF LEN(D2$)=0 OR LEN(D2$)>2 THEN 7450
7280 D3$=RIGHT$(OUTTEXT$,LEN(OUTTEXT$)-POS2)
7290 IF LEN(D3$)=0 OR LEN(D3$)>2 THEN 7450
7300 '
7310 D1=VAL(D1$):IF D1<1 OR D1>31 THEN 7470
7320 D2=VAL(D2$):IF D2<1 OR D2>12 THEN 7470
7330 D3=VAL(D3$) 
7340 ' TODO: Deeper validation
7350 D1$=STR$(D1):D1$=MID$(D1$,2,99):IF D1<10 THEN D1$="0"+D1$
7360 D2$=STR$(D2):D2$=MID$(D2$,2,99):IF D2<10 THEN D2$="0"+D2$
7370 D3$=STR$(D3):D3$=MID$(D3$,2,99):IF D3<10 THEN D3$="0"+D3$
7380 DATUM$=D1$+"."+D2$+"."+D3$
7390 '
7400 PRINT FNCUR$(7,37);HIGHON$;DATUM$;HIGHOFF$
7410 GOTO 7520
7420 '
7430 ' *** Eingabe-Fehlerbehandlung Datum
7450 N$="Datum muss Format TT.MM.JJ haben. Bitte korrigieren.":GOSUB 12860 'Nachricht
7460 GOTO 7480
7461 '
7470 N$="Ungültiges Datum. Bitte korrigieren.":GOSUB 12860           'Nachricht
7480 BADINPUT=1
7490 INTEXT$=OUTTEXT$
7500 GOTO 7120
7510 '
7520 IF NKEY=2 THEN 7693
7600 '
7610 ' *** Suchen nach Nummern und Maske
7620 DRUCKFLAG=1
7630 GOTO 4160                                  'Stammsatz suchen (und drucken)
7640 ' *** Rückkehr aus "Stammsatz suchen"
7650 ' IN: SEARCH (BYNUMS, BYMASK)
7660 ' IN: NUMSUCH1, NUMSUCH2
7670 ' IN: SUCH$()
7680 GOSUB 15800                                         'Drucker bereit?-Frage
7689 IF JA=0 THEN 7970
7690 GOTO 7700
7691 '
7692 ' *** Komplette Mitgliedliste drucken
7693 SEARCH=BYNUMS
7694 NUMSUCH1=1:NUMSUCH2=MTGLMAX
7695 GOSUB 15800                                         'Drucker bereit?-Frage
7696 IF JA=0 THEN 7030
7697 '
7700 N$="Mitgliedsliste wird gedruckt... Abbrechen mit Taste.":GOSUB 12860 'Nachricht
7710 GOSUB 15870                                 'Mitgliedsliste drucken (Kopf)
7720 IF SEARCH=BYMASK THEN 7850
7730 '
7740 ' *** Suchen nach Nummern
7750 FOR MTGLZAHL=NUMSUCH1 TO NUMSUCH2
7760   GOSUB 11830                                             'Stammsatz lesen
7770   IF INSTR(DAT$(5),"ausgetreten")>0 THEN 7790
7780     GOSUB 16010                         'Mitgliedsliste drucken (Mitglied)
7790   GOSUB 15825                                      'Druck abbrechen? Frage
7800   IF JA=1 THEN 7960
7810 NEXT MTGLZAHL
7820 GOTO 7960
7830 ' 
7840 ' *** Suchen nach Maske
7850 MTGLZAHL=0
7860 GOSUB 6014                        'Nächsten Treffer suchen IN/OUT:MTGLZAHL
7870 IF MTGLZAHL=-1 THEN 7960
7880   GOSUB 11830                    'Stammsatz lesen, IN:MTGLZAHL, OUT DAT$()
7890   IF INSTR(DAT$(5),"ausgetreten")>0 THEN 7910
7900     GOSUB 16010                         'Mitgliedsliste drucken (Mitglied)
7910   GOSUB 15825                                      'Druck abbrechen? Frage
7920   IF JA=1 THEN 7960
7930 GOTO 7870
7940 '
7950 ' *** Suchen zuende
7960 GOSUB 13000                                             'Nachricht löschen
7970 IF SEARCH=BYNUMS THEN 4340 ELSE 4910                'TODO: Very hackish...
8350 '
8360 ' *** Datenblatt drucken *************************************************
8370 '
8380 MT$="DATENBLATT DRUCKEN":MI$="G-013030":GOSUB 13070           'Basis-Maske
8390 FKEYS$="":GOSUB 13180                                 'F-Tasten einblenden
8400 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer"
8410 PRINT FNCUR$(5,23)"des zu druckenden Datenblattes ein."
8420 PRINT FNCUR$(7,23)"Beenden Sie die Eingabe mit RETURN."
8430 PRINT FNCUR$(8,23);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
8440 PRINT FNCUR$(10,39)"=>..."
8450 BADINPUT=0
8460 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
8470 Z=10:S=41:LNG=3:TYP=2:MUSS=1
8480 GOSUB 10380                                                'Feld-Eingabe+1
8490 IF FKEY=1 THEN GOSUB 19970:GOTO 8380 'Hilfe-Text 'TODO: Wert geht verloren
8500 IF FKEY=2 THEN 6890
8510 IF FKEY=3 THEN 1960
8520 IF FKEY=4 THEN GOSUB 13500:GOTO 8480                 'Zum Hauptmenü?-Frage
8530 '
8540 M=VAL(OUTTEXT$)                                        'RETURN verarbeiten
8550 IF M>=1 AND M<=MTGLMAX THEN 8600
8560   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 12860         'Nachricht
8570   BADINPUT=1
8580   INTEXT$=OUTTEXT$                                           
8590   GOTO 8470
8600 MTGLZAHL=M
8610 GOSUB 15800                                         'Drucker bereit?-Frage
8620 IF JA=1 THEN 8660
8630   GOSUB 13180                                         'F-Tasten einblenden
8640   INTEXT$=OUTTEXT$
8650   GOTO 8470
8660 N$="Datenblatt drucken...":GOSUB 12940     'Nachricht. F-Tasten ausblenden
8670 GOSUB 11830                                               'Stammsatz lesen
8680 NAMM$ = DAT$(1)+" "+DAT$(2)
8690 GOSUB 16490                          'Stammsatz drucken (Bildschirmformat)
8700 GOSUB 14020                                     'Verwendungsnachweis lesen
8710 GOSUB 15500                                   'Verwendungsnachweis drucken
8720 GOSUB 16760                                     'Mitgliedsvermerke drucken
8730 GOSUB 13000                                             'Nachricht löschen
8740 GOTO 8380
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
10250 ' *** Feld-Eingabe ******************************************************
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
10460   GOSUB 13000                                          'Nachricht löschen
10470   PRINT FNCUR$(Z,S+P);
10480   BADINPUT=0
10490 ON TYP GOSUB 12370,12420,12470,12520,12570                    'Typ prüfen
10500 IF VALID=0 THEN 10560
10510   IF P>=LNG THEN 10440                                  'Gültiges Zeichen
10520   PRINT HIGHON$;A$;HIGHOFF$         
10530   OUTTEXT$=OUTTEXT$+A$
10540   P=P+1
10550   GOTO 10430
10560 IF ASCC<>ASCBSP THEN 10620
10570   IF P<1 THEN 10440                                'Backspace verarbeiten
10580   PRINT DEL$                        
10590   P=P-1
10600   OUTTEXT$=LEFT$(OUTTEXT$,P)
10610   GOTO 10430
10620 IF ASCC=ASCESC THEN 10370                             'Escape verarbeiten
10630 IF ASCC=ASCRET THEN 10760
10640 IF ASCC=ASCCUP THEN CKEY=CKEYUP:IF INSTR(CKEYMASK$,CHR$(CKEY+ASC0))>0 THEN 10760
10650 IF ASCC=ASCCDN THEN CKEY=CKEYDN:IF INSTR(CKEYMASK$,CHR$(CKEY+ASC0))>0 THEN 10760
10660 IF VNFLAG=0 THEN 10690
10670   IF ASCC=ASCCLT THEN CKEY=CKEYLT:IF INSTR(CKEYMASK$,CHR$(CKEY+ASC0))>0 THEN 10760 
10680   IF ASCC=ASCCRT THEN CKEY=CKEYRT:IF INSTR(CKEYMASK$,CHR$(CKEY+ASC0))>0 THEN 10760 
10690 IF ASCC<ASCF1 OR ASCC>ASCF9 THEN 10720              'F-Tasten verarbeiten
10700   FKEY=ASCC-ASCF1+1
10710   IF INSTR(FKEYMASK$,CHR$(FKEY+ASC0))>0 THEN 10810
10720 N$="Ungültige Eingabe - Bitte wiederholen":GOSUB 12860         'Nachricht
10730 BADINPUT=1
10740 GOTO 10430
10750 '
10760 IF MUSS=0 OR LEN(OUTTEXT$)>0 THEN 10800
10770   N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 12860
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
10890 ' *** Tastatur entprellen ************************************** REVIEWED
10900 '
10910 FOR K=1 TO 30:A$=INKEY$:NEXT K
10920 RETURN
10930 '
10940 ' *** Stammsatzfelder-Definition lesen ************************* REVIEWED
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
11100 ' *** DAT$() leeren ******************************************** REVIEWED
11110 '
11120 FOR K=1 TO FELDMAX:DAT$(K)="":NEXT K
11130 RETURN
11140 '
11150 ' *** SUCH$() leeren
11160 '
11170 FOR K=1 TO FELDMAX:SUCH$(K)="":NEXT K
11180 RETURN
11190 '
11200 ' *** MTGLMAX lesen ******************************************** REVIEWED
11210 '
11220 ' OUT: MTGLMAX
11230 OPEN "I",#1,"B:MTGLZAHL.DAT"
11240   INPUT #1,MTGLMAX$
11250   MTGLMAX=VAL(MTGLMAX$)
11260 CLOSE #1
11270 RETURN
11280 '
11290 ' *** MTGLMAX speichern **************************************** REVIEWED
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
11620 ' *** Maske Feldnamen, Feldwerte und Feldplatzhalter ausgeben
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
11731 '
11732 ' *** Mitgliedsnummer ausgeben
11733 '
11734 ' IN: MTGLZAHL
11735 PRINT FNCUR$(3,31);"Mitgliedsnummer";HIGHON$;MTGLZAHL;HIGHOFF$
11736 RETURN
11740 '
11750 ' *** FIELDen der Stammsatz-Datei ****************************** REVIEWED
11760 '
11770 OPEN "R",#2,"B:MV.DAT",462
11780 FIELD #2,236 AS DD1$,225 AS DD2$
11790 RETURN
11800 '
11810 ' *** Stammsatz von Disk lesen ******************************************
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
12050 ' *** Stammsatz auf Disk speichern **************************************
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
12240 ' *** Ja/Nein-Frage und F-Tasten ausblenden ******************** REVIEWED
12250 '
12260 ' IN:  Q$ - Frage
12270 ' OUT: JA - (1=yes, 0=no)
12280 N$=Q$+" (J=Ja/N=Nein)":GOSUB 12860                             'Nachricht
12290 GOSUB 13440                                          'F-Tasten ausblenden
12300 ANSWER$=INPUT$(1)
12310 IF ANSWER$="J" OR ANSWER$="j" THEN JA=1:RETURN
12320 IF ANSWER$="N" OR ANSWER$="n" THEN JA=0:RETURN
12330 GOTO 12300
12340 '
12350 ' *** Testbereich 1 (Alpha) *********************************************
12360 '
12370 VALID=0: IF (ASCC>57 AND ASCC<127) OR (ASCC>31 AND ASCC<48) THEN VALID=1
12380 RETURN
12390 '
12400 ' *** Testbereich 2 (Numerisch) *****************************************
12410 '
12420 VALID=0: IF ASCC>47 AND ASCC<58 THEN VALID=1
12430 RETURN
12440 '
12450 ' *** Testbereich 3 (Alphanumerisch) ************************************
12460 '
12470 VALID=0: IF ASCC>31 AND ASCC<127 THEN VALID=1
12480 RETURN
12490 '
12500 ' *** Testbereich 4 (Numerisch + "/" und ".") ***************************
12510 '
12520 VALID=0: IF ASCC>45 AND ASCC<58 THEN VALID=1
12530 RETURN
12540 '
12550 ' *** Testbereich 5 (Numerisch + und ".") *******************************
12560 '
12570 VALID=0: IF (ASCC>=48 AND ASCC<=57) OR ASCC=46 THEN VALID=1
12580 RETURN
12590 '
12600 ' *** DATUMS-TEST (bei VN und M-Liste drucken) **************************
12610 '
12620 IF VAL(TEST$)=0 OR VAL(TEST$)>31 THEN JA=0                           'Tag
12630 RETURN
12640 '
12650 IF VAL(TEST$)=0 OR VAL(TEST$)>12 THEN JA=0                         'Monat
12660 RETURN
12670 '
12680 IF VAL(TEST$)=0 THEN JA=0                                           'Jahr
12690 RETURN
12700 '
12710 ' *** Voreinstellungen setzen im ERFASSEN *********************** REVIWED
12720 '
12730 DAT$(5)="10***"
12740 DAT$(7)=""11**""
12750 DAT$(11)="12**"
12760 DAT$(12)="13******"
12770 DAT$(21)="3"
12780 DAT$(22)="Nein"
12790 DAT$(23)="Nein"
12800 DAT$(25)="Nein"
12810 RETURN
12820 '
12830 ' *** Nachricht anzeigen *************************************** REVIEWED
12840 '
12850 ' IN: N$ - Nachricht
12860 PRINT FNCUR$(21,1);DELLINE$;
12870 IF N$<>"" THEN PRINT INVON$;" ";N$;" ";INVOFF$;
12880 PRINT                              'LW workaround quirk in display driver
12890 RETURN
12900 '
12910 ' *** Nachricht anzeigen und F-Tasten ausblenden *************** REVIEWED
12920 '
12930 ' IN: N$ - Nachricht
12940 GOSUB 12860                                                    'Nachricht
12950 GOSUB 13440                                          'F-Tasten ausblenden
12960 RETURN
12970 '
12980 ' *** Nachricht löschen **************************************** REVIEWED
12990 '
13000 N$="":GOSUB 12860                                              'Nachricht
13010 RETURN
13020 '
13030 ' *** Bildschirmmaske - Basis ********************************** REVIEWED
13040 '
13050 ' IN: MT$ - Menu title
13060 ' IN: MI$ - Menu ID
13070 PRINT CLS$;
13080 PRINT "00**********************";
13090 PRINT FNCUR$(1,(80-LEN(MT$))/2+1);MT$;FNCUR$(1,72);MI$
13100 PRINT STRICH$
13110 PRINT FNCUR$(21,1);
13120 PRINT STRICH$             'quirk in display driver places this in line 22
13130 RETURN
13140 '
13150 ' *** F-Tasten einblenden ************************************** REVIEWED
13160 '
13170 ' IN: FKEYS$ - Additional function key labels 
13180 PRINT FNCUR$(23,1);"<F1>=Hilfe <F2>=Zurück <F3>=Anfang <F4>=Hauptmenü";
13190 IF FKEYS$<>"" THEN PRINT " ";FKEYS$ ELSE PRINT           
13200 RETURN
13210 '
13220 ' *** restliche funktionstasten bei Verw.-nachweis ausgeben
13230 '
13240 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=Vorwärts             <F8>=Drucken";
13250 PRINT HOME$
13260 RETURN
13270 '
13280 ' *** Eingabe bei Menübildern ********************************** REVIEWED
13290 '
13300 ' IN:  NKEYMAX - max valid numeric key (1,2,3,...)
13310 ' IN:  FKEYMAX - max valid function key (1,2,3,...)
13320 ' OUT: FKEY - number of pressed function key or 0
13330 ' OUT: NKEY - number of pressed numeric key or 0
13340 NKEY=0:FKEY=0
13350 GOSUB 10910                                          'Tastatur entprellen
13360 A$=INPUT$(1):ASCC=ASC(A$)
13370 IF ASCC>=ASC1 AND ASCC<=ASC1+NKEYMAX-1 THEN NKEY=ASCC-ASC1+1:RETURN
13380 IF ASCC>=ASCF1 AND ASCC<=ASCF1+FKEYMAX-1 THEN FKEY=ASCC-ASCF1+1:RETURN
13390 N$="Sie haben eine falsche Taste gedrückt - Bitte neue Auswahl treffen.":GOSUB 12860 'Nachricht
13400 GOTO 13360
13410 '
13420 ' *** F-Tasten ausblenden ************************************** REVIEWED
13430 '
13440 PRINT FNCUR$(23,1);DELLINE$
13450 PRINT FNCUR$(24,1);DELLINE$;
13460 RETURN
13470 '
13480 ' *** 'Zum Hauptmenü?-Frage (Original) ************************* REVIEWED
13490 '
13500 GOSUB 13580                               'Zum Hauptmenü?-Frage (einfach)
13510 IF JA=1 THEN 20180                                          'Programmende
13520 GOSUB 13180                                          'F-Tasten einblenden
13530 RETURN
13540 '
13550 ' *** 'Zum Hauptmenü?-Frage (einfach) ************************** REVIEWED
13560 '
13570 ' OUT: JA (1=true, 0=false)
13580 Q$="Wollen Sie wirklich ins Hauptmenü?":GOSUB 12280        'Ja/Nein-Frage
13590 GOSUB 13000                                            'Nachricht löschen
13600 RETURN
13610 '
13620 ' *** Rückkehr aus Hilfe-Text? ********************************* REVIEWED
13630 '
13640 N$="Zurück? (J=Ja)":GOSUB 12860                                'Nachricht
13650 GOSUB 12290                                           'skip string concat
13660 IF JA=0 THEN 13640
13670 RETURN
13680 '
13690 ' **** VN - Subroutines, permitting one page handling *******************
13700 ' ****         by 04********** 21*******              *******************
13710 '
13720 ' **** OPEN & FIELDING VERWENDUNGSNACHWEIS ******************************
13730 '
13740 OPEN "R",#3,"B:VN.NDX",4
13750 FIELD #3,4 AS POINT0$
13760 OPEN "R",#4,"B:VN.POI",8
13770 FIELD #4,4 AS POINT1$, 4 AS POINT2$
13780 OPEN "R",#5,"B:VN.DAT",560
13790 FIELD #5,240 AS DDD1$,240 AS DDD2$,80 AS DDD3$
13800 RETURN
13810 '
13820 ' *** VERWENDUNGSNACHWEIS FÜR NEUES MITGLIED ANLEGEN ********************
13830 '
13840 ' IN: MTGLZAHL
13850 GET #3,1                               'letzte Seitennummer lesen dann +1
13860 TMPOINT=VAL(POINT0$)+1
13870 LSET DDD1$=""                                    'leeren Record speichern
13880 LSET DDD2$=""
13890 LSET DDD3$=""
13900 PUT #5,TMPOINT
13910 LSET POINT0$=STR$(TMPOINT)             'letzte Seitennummer neu speichern
13920 PUT #3,1
13930 PUT #3,MTGLZAHL+1
13940 LSET POINT1$="0000"
13950 LSET POINT2$="0000"
13960 PUT #4,TMPOINT
13970 RETURN
13980 '
13990 ' **** ERSTE SEITE FINDEN & LESEN ***************************************
14000 '
14010 ' IN: MTGLZAHL
14020 GET #3,MTGLZAHL+1                                            'Index_Datei
14030 CPOINT=VAL(POINT0$)
14040 GET #5,CPOINT:GOSUB 15070                                          'Lesen
14050 BLATT=1
14060 GET #4,CPOINT                                              'Pointer lesen
14070 RETURN
14080 '
14090 ' **** SEITE RÜCKWÄRTS **************************************************
14100 '
14110 TMPOINT=VAL(POINT1$)
14120 IF TMPOINT<>0 THEN 14150
14130 N$="Erste Seite":GOSUB 12860                                   'Nachricht
14140 RETURN
14150 CPOINT=TMPOINT
14160 BLATT=BLATT-1
14170 GET #5,CPOINT:GOSUB 15070   
14180 GET #4,CPOINT:GOSUB 15230                             'Neue Pointer lesen
14190 HP=1:VP=1
14200 GOSUB 14440
14210 RETURN
14220 '
14230 ' **** SEITE VORWÄRTS  **************************************************
14240 '
14250 TMPOINT=VAL(POINT2$)
14260 IF TMPOINT=0 THEN 14330
14270 CPOINT=TMPOINT
14280 BLATT=BLATT+1
14290 GET #5,CPOINT:GOSUB 15070
14300 GET #4,CPOINT:GOSUB 15230
14310 HP=1:VP=1
14320 RETURN
14330 N$="Letzte Seite"
14340 IF INSTR(SPACE$(20),DAT$(26))=0 THEN 14370 
14350 GOSUB 12860                                                    'Nachricht
14360 RETURN
14370 Q$="Letzte Seite - Neue Seite anlegen?":GOSUB 12280        'Ja/Nein-Frage
14380 GOSUB 13000                                            'Nachricht löschen
14390 IF JA=0 THEN 14440
14400 GOSUB 14680
14410 BLATT=BLATT+1
14420 GOSUB 15230
14430 HP=1:VP=1
14440 GOSUB 13180                                          'F-Tasten einblenden
14450 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=vorwärts";
14460 PRINT TAB(54);"<F9>=Blatt drucken";
14470 PRINT HOME$;" "
14480 RETURN
14490 '
14500 ' **** ERSTE SEITE EINES MITGLIEDS ANLEGEN ******************************
14510 '
14520 GET #3,1
14530 LSET POINT0$=STR$(VAL(POINT0$)+1)                                  'NDX+1
14540 PUT #3,1
14550 LSET POINT1$="0000"                                        'Page pointers
14560 LSET POINT2$="0000"
14570 PUT #4,VAL(POINT0$)
14580 LSET POINT0$=POINT0$                                          'First page
14590 PUT #3,MTGLZAHL+1
14600 LSET DDD1$=""                                                 'Datenblatt
14610 LSET DDD2$=""
14620 LSET DDD3$=""
14630 PUT #5,VAL(POINT0$)
14640 RETURN
14650 '
14660 ' **** ALLGEMEIN EINE NEUE SEITE ANLEGEN ********************************
14670 '
14680 GET #3,1                                                           'NDX+1
14690 LSET POINT0$=STR$(VAL(POINT0$)+1)
14700 PUT #3,1:TMPOINT$=POINT0$    'Vektoren der bisher letzten Seite verbiegen
14710 LSET POINT2$=POINT0$                                'Verbiege Vektoren...
14720 PUT #4,CPOINT                                         '...der alten Seite
14730 LSET POINT1$=STR$(CPOINT)                             '...der neuen Seite
14740 LSET POINT2$="0000"                                                '.. ""
14750 CPOINT=VAL(TMPOINT$)                                       'Nr neue Seite
14760 PUT #4,CPOINT
14770 LSET DDD1$=""                                           'Write empty page
14780 LSET DDD2$=""
14790 LSET DDD3$="" 
14800 PUT #5,CPOINT
14810 DATEN$=SPACE$(20)
14820 FOR II=1 TO 26
14830   DAT$(II)=DATEN$
14840 NEXT
14850 RETURN
14860 '
14870 ' **** SEITE SPEICHERN **************************************************
14880 '
14890 DATEN$=""
14900 FOR II=1 TO 12
14910   DATEN$=DATEN$+DAT$(II)
14920 NEXT
14930 LSET DDD1$=DATEN$:DATEN$=""
14940 FOR II=13 TO 24
14950   DATEN$=DATEN$+DAT$(II)
14960 NEXT
14970 LSET DDD2$=DATEN$:DATEN$=""
14980 FOR II=25 TO 26
14990   DATEN$=DATEN$+DAT$(II)
15000 NEXT 
15010 LSET DDD3$=DATEN$:DATEN$=""
15020 PUT #5,CPOINT
15030 RETURN
15040 '
15050 ' **** HACK ! (ZERSTÜCKELN DER DATEN-STRINGS) ***************************
15060 '
15070 FOR II=1 TO 12
15080   DAT$(II)=MID$(DDD1$,(II-1)*20+1,20)
15090 NEXT
15100 FOR II=13 TO 24
15110   DAT$(II)=MID$(DDD2$,(II-13)*20+1,20)
15120 NEXT 
15130 FOR II=25 TO 26
15140   DAT$(II)=MID$(DDD3$,(II-25)*20+1,20)
15150 NEXT 
15160 RETURN
15170 '
15180 ' **** UPRO DATENAUSGABE VN  ********************************************
15190 '
15200 ' IN: MTGLZAHL
15210 ' IN: NAMM$
15220 ' IN: BLATT
15230 PRINT DELHIGH$;                                          'TODO Necessary?
15240 PRINT HIGHON$
15250 PRINT FNCUR$(5,16);MTGLZAHL
15260 PRINT FNCUR$(5,29);NAMM$
15270 PRINT FNCUR$(3,8);BLATT
15280 FOR K=1 TO 26
15290   HOR=6-39*(K>13)
15300   VER=6+K+13*(K>13)
15310   FOR J=1 TO 3                                                 'Von-Datum
15320     PRINT FNCUR$(VER,HOR+(J-1)*3);MID$(DAT$(K),-1+J*2,2)
15330   NEXT
15340   FOR J=1 TO 3                                                 'Bis-Datum
15350     PRINT FNCUR$(VER,HOR+13+(J-1)*3);MID$(DAT$(K),5+J*2,2)
15360   NEXT
15370   PRINT FNCUR$(VER,HOR+26);MID$(DAT$(K),13,8)                        'Amt
15380 NEXT
15390 PRINT HIGHOFF$
15400 RETURN
15410 '
15420 ' *** Verwendungsnachweis drucken ***************************************
15430 '
15440 ' IN: BLATT
15450 ' IN: MITGLZAHL
15460 ' IN: NAMM$
15470 Q$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 12280 'Ja/Nein-Frage TODO Back to main code?
15480 IF JA=0 THEN 15720                                  'LW TODO Remove this!
15490 '
15500 RETURN                                         '*** TMP Print Disable ***
15510 LIND$=SPACE$(2)
15520 '
15530 LPRINT STRICH$
15540 LPRINT
15550 LPRINT LIND$;"Blatt:";BLATT
15560 LPRINT
15570 LPRINT LIND$;"Mitglieds-Nr.:";MTGLZAHL;TAB(24);"Name:";NAMM$
15580 LPRINT
15590 FOR K=1 TO 13
15600   KK=K:GOSUB 15690
15610   LPRINT LIND$;"Von:";VON$;" Bis:";BIS$;" Amt:";AMT$;
15620   KK=K+13:GOSUB 15690
15630   LPRINT " Von:";VON$;" Bis:";BIS$;" Amt:";AMT$
15640 NEXT 
15650 LPRINT
15660 LPRINT STRICH$
15670 RETURN
15680 'Format: VVVVVV BBBBBB AAAAAAAA
15690 VON$=LEFT$(DAT$(KK),2)+"."+MID$(DAT$(KK),3,2)+"."+MID$(DAT$(KK),5,2)
15700 BIS$=MID$(DAT$(KK),7,2)+"."+MID$(DAT$(KK),9,2)+"."+MID$(DAT$(KK),11,2)
15710 AMT$=RIGHT$(DAT$(KK),8)
15720 RETURN
15730 '
15740 ' ### Unterprogramme "Mitgliedsliste drucken" ###########################
15760 '
15770 ' *** Drucker bereit?-Frage
15780 '
15790 ' OUT: JA (1=Ja, 0=Nein)
15800 Q$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 12280 'Ja/Nein-Frage
15810 GOSUB 13000                                            'Nachricht löschen
15820 RETURN
15821 '
15822 ' *** Druck abbrechen? Frage
15823 '
15824 ' OUT: JA (1=true,0=false)
15825 JA=0 
15826 IF INKEY$="" THEN 15830
15827 Q$="Drucken abbrechen?":GOSUB 12280                        'Ja/Nein-Frage
15828 IF JA=1 THEN 15830
15829 N$="Mitgliedsliste wird gedruckt... Abbrechen mit Taste.":GOSUB 12860 'Nachricht
15830 RETURN
15839 '
15840 ' *** Mitgliedsliste drucken (Kopf)
15850 '
15860 ' IN: DATUM$
15870 LPRINT TAB(16);"MITGLIEDSLISTE 25*******************************"
15880 LPRINT 
15890 LPRINT TAB(32);"Stand:";DATUM$
15900 LPRINT
15910 LPRINT TAB(3);"Mitgl."; 
15920 LPRINT TAB(10);"Name";
15930 LPRINT TAB(36);"Vorname, Anschrift";
15940 LPRINT TAB(63);"Status";
15950 LPRINT TAB(71);"Amt"
15960 RETURN
15970 '
15980 ' *** Mitgliedsliste drucken (Mitglied)
15990 '
16000 ' IN:  MTGLZAHL
16010 AMT$=""
16020 IN=INSTR(DAT$(6),"22**"):IF IN<>0 THEN AMT$=MID$(DAT$(6),IN,6):GOTO 16060
16030 IN=INSTR(DAT$(6),"23**"):IF IN<>0 THEN AMT$=MID$(DAT$(6),IN,5):GOTO 16060
16040 IN=INSTR(DAT$(6),"24**"):IF IN<>0 THEN AMT$=MID$(DAT$(6),IN,3)
16050 '
16060 LPRINT
16070 LPRINT TAB(5);MTGLZAHL;
16080 LPRINT TAB(10);DAT$(1);                                         'Nachname
16090 LPRINT TAB(36);DAT$(2);                                          'Vorname
16100 LPRINT TAB(65);"(";LEFT$(DAT$(5),1);")";                          'Status
16110 LPRINT TAB(71);AMT$                                                  'Amt
16120 LPRINT TAB(36);DAT$(10)                                           'Straße
16130 LPRINT TAB(36);DAT$(11);                                    'Postleitzahl
16140 LPRINT TAB(41);DAT$(12)                                              'Ort
16150 LPRINT TAB(36);"Tel.:";DAT$(13)                                  'Telefon
16160 LPRINT
16170 RETURN
16280 '
16290 ' *** Adressaufkleber drucken
16300 '                                                           Ja/Nein-Frage
16310 LIND$=SPACE$(3)
16320 '
16330 LPRINT LIND$;DAT$(7)                                              'Anrede
16340 LPRINT LIND$;LEFT$(DAT$(2),INSTR(DAT$(2)," "));DAT$(1)  'Vorname Nachname
16350 LPRINT
16360 LPRINT LIND$;DAT$(10)                                  'Straße Hausnummer
16370 LPRINT
16380 LPRINT LIND$;DAT$(11);" ";DAT$(12)                      'Postleitzahl Ort
16390 LPRINT
16400 LPRINT
16410 LPRINT
16420 LPRINT
16430 LPRINT
16440 LPRINT
16450 RETURN
16460 '
16470 ' *** Stammsatz drucken (Bildschirmformat)
16480 '
16490 RETURN                                         '*** TMP Print Disable ***
16500 XCOL1=8
16510 XCOL2=43
16520 PRINT STRICH$
16530 PRINT
16540 PRINT TAB(31);"Mitgliedsnummer";MTGLZAHL
16550 PRINT
16560 RESTORE 16620
16570 FOR K=1 TO 7:READ A,B
16580   IF A<>0 THEN PRINT TAB(XCOL1);FENAM$(A);DAT$(A);
16590   IF B<>0 THEN PRINT TAB(XCOL2);FENAM$(B);DAT$(B);
16600   PRINT
16610 NEXT K
16620 DATA 1,0,2,15,3,16,4,0,5,17,6,18,0,19
16630 FOR K=7 TO 14
16640   PRINT TAB(XCOL1);FENAM$(K);DAT$(K);
16650   PRINT TAB(XCOL2);FENAM$(K+13);DAT$(K+13);
16660 NEXT K
16670 PRINT
16680 PRINT
16690 PRINT
16700 PRINT STRICH$
16710 RETURN
16720 '
16730 ' *** Mitgliedsvermerke drucken
16740 '
16750 ' IN: NAMM$ =
16760 RETURN                                         '*** TMP Print Disable ***
16770 LIND$=SPACE$(9)
16780 SLOT1$="___"
16790 SLOT2$="___ / ___"
16800 SLOT3$="___ / ___ / ___"
16810 '
16820 LPRINT TAB(10);"Mitgliedsnr.: ";SLOT1$;
16830 LPRINT TAB(28);"Name: ";NAMM$;
16840 LPRINT TAB(65);"Datum: ";SLOT3$
16850 LPRINT
16860 LPRINT LIND$;"26*************************** ";SLOT3$
16870 LRPINT
16880 LPRINT LIND$;"27*************************** ";SLOT3$
16890 LRPINT
16900 LPRINT LIND$;"28*************************** ";SLOT3$
16910 LRPINT
16920 LPRINT LIND$;"29*************************** ";SLOT3$
16930 LRPINT
16940 LPRINT LIND$;"30*************************** ";SLOT2$
16950 LRPINT
16960 LPRINT LIND$;"31*************************** ";SLOT3$
16970 LRPINT
16980 LPRINT LIND$;"32*************************** ";SLOT3$
16990 LRPINT
17000 LPRINT LIND$;"33*************************** ";SLOT1$
17010 LRPINT
17020 LPRINT LIND$;"34*************************** ";SLOT3$
17030 LRPINT
17040 LPRINT LIND$;"35*************************** ";SLOT3$
17050 LRPINT
17060 LPRINT LIND$;"36*************************** ";SLOT3$
17070 LRPINT
17080 LPRINT LIND$;"37*************************** ";SLOT3$
17090 LRPINT
17100 LPRINT LIND$;"38*************************** ";SLOT3$
17110 RETURN
17120 '
17130 ' ### Hilfe-Texte #######################################################
17140 '
17150 ' *** Hilfe-Text "Mitgliederverwaltung" G-010000
17160 '
17170 MT$="HILFE":GOSUB 13070                                      'Basis-Maske
17180 PRINT HOME$
17190 PRINT IN$;"Die MITGLIEDERVERWALTUNG bietet folgende Funktionen an :"
17200 PRINT 
17210 PRINT IN$;"1 - STAMMSATZPFLEGE"
17220 PRINT IN$;"    Bearbeiten von Mitgliedsdaten gemäß dem Personalstamm-"
17230 PRINT IN$;"    blatt" 
17240 PRINT
17250 PRINT IN$;"2 - VERWENDUNGSNACHWEIS"
17260 PRINT IN$;"    Bearbeiten von Mitgliedsdaten, die Ämter umd Amtsperioden"
17270 PRINT IN$;"    eines Mitglieds enthalten"
17280 PRINT
17290 PRINT IN$;"3 - MITGLIEDSLISTE DRUCKEN"
17300 PRINT IN$;"    Drucken von Mitgliedslisten nach verschiedenen Gesichts-"
17310 PRINT IN$;"    punkten"
17320 PRINT
17330 PRINT IN$;"Wählen Sie aus, indem Sie die zugehörige Zahlentaste drücken."
17340 GOSUB 13640                                      'Rückkehr aus Hilfe-Text
17350 RETURN
17360 '
17370 ' *** Hilfe-Text "Stammsatzpflege" G-011000
17380 '
17390 MT$="HILFE":GOSUB 13070                                      'Basis-Maske
17400 PRINT HOME$
17410 PRINT IN$;"Die STAMMSATZPFLEGE bietet folgende Funktionen an:"
17420 PRINT
17430 PRINT IN$;"1 - STAMMSATZ ANLEGEN"
17440 PRINT IN$;"    Erfassen der Daten eines neuen Mitglieds"
17450 PRINT
17460 PRINT IN$;"2 - STAMMSATZ ÄNDERN"
17470 PRINT IN$;"    Verändern eines bereits bestehenden Stammsatzes"
17480 PRINT
17490 PRINT IN$;"3 - STAMMSATZ LÖSCHEN"
17500 PRINT IN$;"    Verändern des Mitgliedsstatus auf `ausgetreten`"
17510 PRINT
17520 PRINT IN$;"4 - STAMMSATZ DRUCKEN"
17530 PRINT IN$;"    Drucken eines kompletten Stammsatzes"
17540 PRINT
17550 PRINT IN$;"5 - STAMMSATZ SUCHEN"
17560 PRINT IN$;"    Suchen vom Stammsätzen nach bestimmten Gesichtspunkten"
17570 PRINT
17580 PRINT IN$;"Wählen Sie aus, indem Sie die zugehörige Zahlentaste drücken."
17590 GOSUB 13640                                      'Rückkehr aus Hilfe-Text
17600 RETURN
17610 '
17620 ' *** Hilfe-Text "Stammsatz anlegen" G-011010
17630 '
17640 MT$="HILFE":GOSUB 13070                                      'Basis-Maske
17650 PRINT HOME$
17660 PRINT IN$;"Sie können jetzt Daten eines neuen Mitglieds eingeben, jedoch"
17670 PRINT IN$;"nicht die eines bestehenden Mitglieds ändern."
17680 PRINT
17690 PRINT IN$;"Die Mitgliedsnummer wird laufend vergeben."
17700 PRINT
17710 PRINT IN$;"Während der Dateneingebe können Sie mit der DELETE-Taste"
17720 PRINT IN$;"korrigieren. Ist ein Feld ausgefüllt worden, muß RETURN"
17730 PRINT IN$;"gedrückt werden, da sonst die Eingabe nicht gespeichert wird."
17740 PRINT IN$;"Mit den Cursortasten kann das Feld gewechselt werden."
17750 PRINT
17760 PRINT IN$;"`=>`-Felder müssen ausgefüllt werden."
17770 PRINT
17780 PRINT IN$;"Weitere Funktionstasten:"
17790 PRINT IN$;"<F7>  Diesen Stammsatz speichern"
17800 GOSUB 13640                                      'Rückkehr aus Hilfe-Text
17810 RETURN
17820 '
17830 ' *** Hilfe-Text "Stammsatz ändern (I)" G-011020
17840 '
17850 MT$="HILFE":GOSUB 13070                                      'Basis-Maske
17860 PRINT HOME$
17870 PRINT IN$;"Sie können jetzt einen bestehenden Stammsatz ändern."
17880 PRINT
17890 PRINT IN$;"Geben Sie die Mitgliedsnummer des Stammsatzes ein."
17900 PRINT IN$;"Bei Tippfehlern können Sie mit der DELETE-Taste korrigieren."
17910 GOSUB 13640                                      'Rückkehr aus Hilfe-Text
17920 RETURN
17930 '
17940 ' *** Hilfe-Text "Stammsatz ändern (II)" G-011020
17950 '
17960 SAVEMT$=MT$:MT$="HILFE":GOSUB 13070:MT$=SAVEMT$              'Basis-Maske
17970 PRINT HOME$
17980 PRINT IN$;"Sie können jetzt einen bestehenden Stammsatz ändern."
17990 PRINT
18000 PRINT IN$;"Während der Dateneingabe können Sie mit der DELETE-Taste"
18010 PRINT IN$;"korrigieren. Ist ein Feld geändert worden, muß RETURN"
18020 PRINT IN$;"gedrückt werden, da sonst die Änderung nicht gespeichert wird."
18030 PRINT IN$;"Mit den Cursortasten kann das Feld gewechselt werden."
18040 PRINT
18050 PRINT IN$;"`=>`-Felder müssen ausgefüllt werden."
18060 PRINT
18070 PRINT IN$;"Weitere Funktionstasten:"
18080 PRINT IN$;"<F7>  Diesen Stammsatz speichern"
18090 GOSUB 13640                                      'Rückkehr aus Hilfe-Text
18100 RETURN
18110 '
18120 ' *** Hilfe-Text "Stammsatz löschen" G-011030
18130 '
18140 MT$="HILFE":GOSUB 13070                                      'Basis-Maske
18150 PRINT HOME$
18160 PRINT IN$;"Sie können jetzt den Status eines Mitglieds auf `ausgetreten`"
18170 PRINT IN$;"ändern. Damit wird der Stammsatz beim Druck von Mitglieds-"
18180 PRINT IN$;"listen nicht berüchsichtigt."
18190 PRINT IN$;"Auf keinen Fall werden die Daten WIRKLICH gelöscht."
18200 PRINT
18210 PRINT IN$;"Geben Sie die Mitgliedsnummer des Stammsatzes ein."
18220 PRINT IN$;"Bei Tippfehlern können Sie mit der DELETE-Taste korrigieren."
18230 GOSUB 13640                                      'Rückkehr aus Hilfe-Text
18231 RETURN
18232 ' 
18233 ' *** Hilfe-Text für "Stammsatz drucken" G-011040
18234 '
18235 SAVEMT$=MT$:MT$="HILFE":GOSUB 13070:MT$=SAVEMT$              'Basis-Maske
18236 PRINT HOME$
18237 PRINT IN$;"Sie können jetzt einen kompletten Stammsatz drucken, so, wie"
18238 PRINT IN$;"er auf dem Bildschirm steht."
18239 PRINT
18240 PRINT IN$;"Geben Sie die Mitgliedsnummer des Stammsatzes ein."
18241 PRINT IN$;"Bei Tippfehlern können Sie mit der DELETE-Taste korrigieren."
18242 GOSUB 13640                                      'Rückkehr aus Hilfe-Text
18243 RETURN
18250 '
18260 ' *** Hilfe-Text "Stammsatz suchen (I)" G-011050
18270 '
18280 MT$="HILFE":GOSUB 13070                                      'Basis-Maske
18290 PRINT HOME$
18300 PRINT IN$;"Sie können jetzt Stammsätze suchen."
18310 PRINT
18320 PRINT IN$;"1 - Suchen nach Nummern"
18330 PRINT IN$;"    Geben Sie ein Intervall von Mitgliedsnummern ein."
18340 PRINT IN$;"    Es werden alle Stammsätze ausgegeben, deren Nummern im"
18350 PRINT IN$;"    Intervall liegen."
18360 PRINT
18370 PRINT IN$;"2 - Suchen nach Maske"
18380 PRINT IN$;"    Sie füllen ein Formular aus. Diejenigen Stammsätze, die"
18390 PRINT IN$;"    mit dem Formular übereinstimmen, werden ausgegeben."
18400 PRINT IN$;"    Leere Felder bezeichnen beliebige Stammsatzdaten."
18410 PRINT IN$;"    Ein * zu Beginn des Feldes bedeutet: Das folgende Wort"
18420 PRINT IN$;"    darf sich an beliebiger Stelle des Feldes befinden."
18430 PRINT IN$;"    Ein oder mehrere ? zu Beginn des Feldes stellen Platz-"
18440 PRINT IN$;"    halter für beliebige Zeichen dar."
18450 PRINT
18460 PRINT IN$;"Wählen Sie aus, indem Sie die zugehörige Zahlentaste drücken."
18470 GOSUB 13640                                      'Rückkehr aus Hilfe-Text
18480 RETURN
18490 '
18500 ' *** Hilfe-Text "Stammsatz suchen nach Nummern" G-011051
18510 '
18520 MT$="HILFE":GOSUB 13070                                      'Basis-Maske
18530 PRINT HOME$
18540 PRINT IN$;"Hilfe-Text *Stammsatz suchen nach Nummern* G-011051"
18550 GOSUB 13640                                      'Rückkehr aus Hilfe-Text
18560 RETURN
18570 '
18580 ' *** Hilfe-Text "Stammsatz suchen nach Maske" G-011052
18590 '
18600 MT$="HILFE":GOSUB 13070                                      'Basis-Maske
18610 PRINT HOME$
18620 PRINT IN$;"Hilfe-Text *Stammsatz suchen nach Maske* G-011052"
18630 GOSUB 13640                                      'Rückkehr aus Hilfe-Text
18640 RETURN
18650 '
18660 ' *** Hilfe-Text "Stammsatz suchen (II)"  G-011050
18670 '
18680 MT$="HILFE":GOSUB 13070                                      'Basis-Maske
18690 PRINT HOME$
18700 PRINT IN$;"Hilfe-Text *Stammsatz suchen (II)* G-011050 Keine Suchtreffer"
18710 GOSUB 13640                                      'Rückkehr aus Hilfe-Text
18720 RETURN
18730 '
18740 ' *** Hilfe-Text "Stammsatz suchen (III)" G-011050
18750 '
18760 MT$="HILFE":GOSUB 13070                                      'Basis-Maske
18770 PRINT HOME$
18780 PRINT IN$;"Hilfe-Text *Stammsatz suchen (III)* G-011050 Blättern"
18790 GOSUB 13640                                      'Rückkehr aus Hilfe-Text
18800 RETURN
18930 '
18940 ' *** Hilfe-Text "Verwendungsnachweis (I)" G-012000
18950 '
18960 MT$="HILFE":GOSUB 13070                                      'Basis-Maske
18970 PRINT HOME$
18980 PRINT IN$;"Sie können jetzt einen Verwendungsnachweis ändern."
18990 PRINT
19000 PRINT IN$;"Geben Sie die Mitgliedsnummer des Verwendungsnachweises ein."
19010 PRINT IN$;"Bei Tippfehlern können Sie mit der DELETE-Taste korrigieren."
19020 GOSUB 13640                                      'Rückkehr aus Hilfe-Text
19030 RETURN
19040 '
19050 ' *** Hilfe-Text "Verwendungsnachweis (II)" G-012000
19060 '
19070 SAVEMT$=MT$:MT$="HILFE":GOSUB 13070:MT$=SAVEMT$              'Basis-Maske
19080 PRINT HOME$
19090 PRINT IN$;"Sie können jetzt einen bestehenden Verwendungsnachweis ändern."
19100 PRINT
19110 PRINT IN$;"Während der Dateneingabe können Sie mit der DELETE-Taste"
19120 PRINT IN$;"korrigieren. Ist ein Feld ausgefüllt worden, muß RETURN"
19130 PRINT IN$;"gedrückt werden, da sonst die Änderung nicht gespeichert"
19140 PRINT IN$;"wird. Mit den Cursortasten kann das Feld gewechselt werden."
19150 PRINT
19160 PRINT IN$;"Weitere Funktionstasten:"
19170 PRINT IN$;"<F5>  Zurückblättern (ohne die jetzige Seite zu speichern)"
19180 PRINT IN$;"<F6>  Vorblättern (ohne die jetzige Seite zu speichern)"
19190 PRINT IN$;"<F7>  Aktuelle Seite speichern"
19200 PRINT IN$;"<F9>  Aktuelle Seite drucken"
19210 GOSUB 13640                                      'Rückkehr aus Hilfe-Text
19220 RETURN
19230 '
19240 ' *** Hilfe-Text "Mitgliedsliste drucken" G-013000
19250 '
19260 MT$="HILFE":GOSUB 13070                                      'Basis-Maske
19270 PRINT HOME$
19280 PRINT IN$;"Sie können jetzt eine Mitgliedsliste drucken." 
19290 PRINT
19300 PRINT IN$;"1 - Suchen & Drucken"
19310 PRINT IN$;"    Sie können Stammsätze nach verschiedenen Gesichtspunkten"
19320 PRINT IN$;"    suchen und drucken."
19330 PRINT
19340 PRINT IN$;"2 - Komplett drucken"
19350 PRINT IN$;"    Es wird eine komplette Mitgliedsliste ausgedruckt."
19360 PRINT IN$;"    Ausgetretene Mitglieder werden nicht berücksichtigt."
19370 PRINT
19380 PRINT IN$;"3 - Datenblatt drucken"
19390 PRINT IN$;"    Der Stammsatz und der Verwendungsnachweis werden im"
19400 PRINT IN$;"    Bildschirmformat mit einer Liste von Mitgliedsvermerken"
19410 PRINT IN$;"    ausgedruckt."
19420 PRINT
19430 PRINT IN$;"Wählen Sie aus, indem Sie die zugehörige Zahlentaste drücken."
19440 GOSUB 13640                                      'Rückkehr aus Hilfe-Text
19450 RETURN
19460 '
19470 ' *** Hilfe-Text "Suchen & Drucken (I)" G-013010
19480 '
19490 MT$="HILFE":GOSUB 13070                                      'Basis-Maske
19500 PRINT HOME$
19510 PRINT IN$;"Hilfe-Text *Suchen & Drucken (I)* G-013010" 
19520 GOSUB 13640                                      'Rückkehr aus Hilfe-Text
19530 RETURN
19540 '
19550 ' *** Hilfe-Text "Suchen & Drucken nach Nummern" G-013011
19560 '
19570 MT$="HILFE":GOSUB 13070                                      'Basis-Maske
19580 PRINT HOME$
19590 PRINT IN$;"Hilfe-Text *Suchen & Drucken nach Nummern* G-013011"
19600 GOSUB 13640                                      'Rückkehr aus Hilfe-Text
19610 RETURN
19620 '
19630 ' *** Hilfe-Text "Suchen & Drucken nach Maske" G-013012
19640 '
19650 MT$="HILFE":GOSUB 13070                                      'Basis-Maske
19660 PRINT HOME$
19670 PRINT IN$;"Hilfe-Text *Suchen & Drucken nach Maske* G-013012"
19680 GOSUB 13640                                      'Rückkehr aus Hilfe-Text
19690 RETURN
19700 '
19710 ' *** Hilfe-Text "Suchen & Drucken (II)" G-013010
19720 '
19730 MT$="HILFE":GOSUB 13070                                      'Basis-Maske
19740 PRINT HOME$
19750 PRINT IN$;"Hilfe-Text *Suchen & Drucken (II)* G-013010 Keine Suchtreffer"
19760 GOSUB 13640                                      'Rückkehr aus Hilfe-Text
19770 RETURN
19780 '
19790 ' *** Hilfe-Text "Suchen & Drucken (III)" G-013010
19800 '
19810 MT$="HILFE":GOSUB 13070                                      'Basis-Maske
19820 PRINT HOME$
19830 PRINT IN$;"Hilfe-Text *Suchen & Drucken (III)* G-013010 Blättern"
19840 GOSUB 13640                                      'Rückkehr aus Hilfe-Text
19850 RETURN
19860 '
19870 ' *** Hilfe-Text "Suchen & Drucken (IB), Komplettdruck Datum" G-013010, G-013020
19880 '
19890 SAVEMT$=MT$:MT$="HILFE":GOSUB 13070:MT$=SAVEMT$              'Basis-Maske
19900 PRINT HOME$
19910 PRINT IN$;"Hilfe-Text *Suchen & Drucken (IB)* Komplettdruck Datum G-013010, G-013020"
19920 GOSUB 13640                                      'Rückkehr aus Hilfe-Text
19930 RETURN
19940 '
19950 ' *** Hilfe-Text "Datenblatt drucken" G-013030
19960 '
19970 MT$="HILFE":GOSUB 13070                                      'Basis-Maske
19980 PRINT HOME$
19990 PRINT IN$;"Hilfe-Text *Datenblatt drucken* G-013030
20000 GOSUB 13640                                      'Rückkehr aus Hilfe-Text
20010 RETURN
20020 '
20030 ' *** Programmfehler ******************************************* REVIEWED
20040 '
20050 I$=SPACE$(15)
20060 GOSUB 13070                                                  'Basis-Maske
20070 PRINT HOME$
20080 PRINT I$;"Ein Programmfehler ist aufgetreten."
20090 PRINT   
20100 PRINT I$;"- Notieren Sie folgende Fehlerinformation:"
20110 PRINT I$;"    ";HIGHON$;"ERL";HIGHOFF$;" (Error Line)";HIGHON$;ERL;HIGHOFF$
20120 PRINT I$;"    ";HIGHON$;"ERR";HIGHOFF$;" (Error Code)";HIGHON$;ERR;HIGHOFF$
20130 PRINT I$;"- Drücken Sie am Rechner den Knopf hinten rechts."
20140 PRINT I$;"19**************************"
20150 '
20160 ' *** Programmende ********************************************* REVIEWED
20170 '
20180 PRINT FNCUR$(22,1);HIGHOFF$;CURON$;
20190 RESET
20200 'LW RUN "DISKMENU.EXE"
20210 END                                                           '1799 lines
