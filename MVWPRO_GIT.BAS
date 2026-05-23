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
1300 ESC$   =CHR$(27)                                 'Einleitung Escapesequenz
1310 IN$    =SPACE$(10)                                 'Indent für Hilfe-Texte
1320 STRICH$=STRING$(79,"-")                                            'Strich
1330 '
1340 CLS$    =ESC$+"E"                                      'Bildschirm löschen
1350 INVON$  =ESC$+"p"                                              'Invers ein
1360 INVOFF$ =ESC$+"q"                                              'Invers aus
1370 CURON$  =ESC$+"y5"                                             'Cursor ein
1380 CUROFF$ =ESC$+"x5"                                             'Cursor aus
1390 HIGHON$ =ESC$+"("                                           'Helle Schrift
1400 HIGHOFF$=ESC$+")"                                          'Dunkle Schrift
1410 DELLINE$=ESC$+"K"                                           'Zeile löschen
1420 DELHIGH$=ESC$+"+"                                   'Helle Schrift löschen
1430 '
1440 DEF FNCUR$(Z,S)=ESC$+"Y"+CHR$(31+Z)+CHR$(31+S)       'Cursor positionieren
1450 HOME$=FNCUR$(3,1)                                    'Cursor Home Position
1460 ' 
1470 CKEYUP=1
1480 CKEYDN=2
1490 CKEYLT=3
1500 CKEYRT=4
1510 '
1520 ASCBSP=8
1530 ASCRET=13
1540 ASCESC=27
1550 ASC0=48
1560 ASC1=49
1570 ASCCUP=200
1580 ASCCDN=201
1590 ASCCLT=202
1600 ASCCRT=203
1610 ASCF1=241
1620 ASCF4=244
1630 ASCF9=249
1640 '
1650 ' Other global variables:
1660 ' -----------------------
1670 ' LIND$    - Local text indent
1680 ' F        - Feldindex 1..27 (FELDMAX)
1690 ' MTGLMAX  - Höchste auf Disk gespeicherte Mitgliedsnummer
1700 ' MTGLZAHL - Momentane Mitgliedsnummer
1710 '
1720 XOFF=14             'Offset Colum Feldname to Column Wertefeld (Pünktchen)
1730 '
1740 ' TIP: Do not let the cursor slip into line 25 -> display will scroll up
1750 ' TIP: Use semicolons with PRINT carefully -> display driver bug
1760 '
1770 ON ERROR GOTO 20100                               'Programmfehler abfangen
1780 '
1790 GOSUB 10050    'Tasten deaktivieren, ASCII-Codes für Cursortasten vergeben
1800 PRINT CUROFF$;
1810 '
1820 ' *** Beginn des Mitgliederverwaltungsprogramms **************************
1830 '
1840 MT$="MITGLIEDERVERWALTUNG":MI$="G-010000":GOSUB 13130         'Basis-Maske
1850 Q$="Bitte Daten-Diskette in Laufwerk B einlegen. Bereit?":GOSUB 12340 'Ja/Nein-Frage
1860 IF JA=0 THEN 20230                                           'Programmende
1870 '
1880 N$="Einen Moment bitte...":GOSUB 13000     'Nachricht. F-Tasten ausblenden
1890 GOSUB 10960                              'Stammsatzfelder-Definition lesen
1900 GOSUB 11830                                   'Fielden der Stammsatz-Datei
1910 GOSUB 13800                       'Fielden der Verwendungsnachweis-Dateien
1920 GOSUB 11230                                                 'MTGLMAX lesen
1930 GOSUB 13060                                             'Nachricht löschen
1940 '
1950 ' ################# Mitgliederverwaltungsmenü (MVW-Menü) #################
1960 '
1970 MT$="MITGLIEDERVERWALTUNG":MI$="G-010000":GOSUB 13130         'Basis-Maske
1980 FKEYS$="":GOSUB 13240                                 'F-Tasten einblenden
1990 VNFLAG=0                                                              '???
2000 PRINT FNCUR$(4,26);"1 - Stammsatzpflege"                         'Menütext
2010 PRINT FNCUR$(6,26);"2 - Verwendungsnachweis"
2020 PRINT FNCUR$(8,26);"3 - Mitgliedsliste drucken"
2030 FKEYMAX=4:NKEYMAX=3:GOSUB 13400                              'Menü-Eingabe
2040 IF NKEY THEN ON NKEY GOTO 2130,6530,7360
2050 ON FKEY GOTO 2060,2070,1970,2070
2060 GOSUB 17220:GOTO 1970
2070 GOSUB 13560:GOTO 2030                                'Zum Hauptmenü?-Frage
2080 '
2090 ' ########################### Stammsatzpflege ############################
2100 '
2110 ' *** Stammsatzpflege-Menü ***********************************************
2120 '
2130 MT$="STAMMSATZPFLEGE":MI$="G-011000":GOSUB 13130              'Basis-Maske
2140 FKEYS$="":GOSUB 13240                                 'F-Tasten einblenden
2150 PRINT FNCUR$(4,29);"1 - Stammsatz anlegen"
2160 PRINT FNCUR$(6,29);"2 - Stammsatz ändern"
2170 PRINT FNCUR$(8,29);"3 - Stammsatz löschen"
2180 PRINT FNCUR$(10,29);"4 - Stammsatz drucken"
2190 PRINT FNCUR$(12,29);"5 - Stammsatz suchen"
2200 FKEYMAX=4:NKEYMAX=5:GOSUB 13400                              'Menü-Eingabe
2210 IF NKEY THEN ON NKEY GOTO 2280,2760,3420,3850,4220
2220 ON FKEY GOTO 2230,1970,1970,2240
2230 GOSUB 17440:GOTO 2130
2240 GOSUB 13560:GOTO 2200                                'Zum Hauptmenü?-Frage
2250 '
2260 ' *** Stammsatz erfassen von 02************ ******************************
2270 '
2280 MTGLZAHL=MTGLMAX+1
2290 F=1
2300 GOSUB 11120                                                 'DAT$() leeren
2310 GOSUB 12790                                       'Voreinstellungen setzen
2320 MT$="STAMMSATZ ANLEGEN":MI$="G-011010":GOSUB 13130            'Basis-Maske
2330 FKEYS$="<F7>=Speichern":GOSUB 13240                   'F-Tasten einblenden
2340 GOSUB 11780                                      'Mitgliedsnummer ausgeben
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
2480 IF FKEY=1 THEN GOSUB 17690:GOTO 2320                           'Hilfe-Text
2490 IF FKEY=2 THEN GOSUB 2590:GOTO 2130             'Exit "Stammsatz erfassen"
2500 IF FKEY=3 THEN GOSUB 2590:GOTO 1970             'Exit "Stammsatz erfassen"
2510 IF FKEY<>4 THEN 2560
2520   GOSUB 13640                              'Zum Hauptmenü?-Frage (einfach)
2530   IF JA=0 THEN GOSUB 13240:GOTO 2380                  'F-Tasten einblenden
2540   GOSUB 2590                                    'Exit "Stammsatz erfassen"
2550   GOTO 20230                                                 'Programmende
2560 IF FKEY=7 THEN GOSUB 2670:GOTO 2280           'Exit "Stammsatz erfassen" 2
2570 '
2580 ' *** Exit "Stammsatz erfassen"
2590 GOSUB 10910                                           'Tastatur entprellen
2600 Q$="Wollen Sie vorher den Stammsatz speichern?":GOSUB 12340 'Ja/Nein-Frage
2610 IF JA=1 THEN 2670
2620   N$="Stammsatz wird nicht gespeichert.":GOSUB 13000 'Nachricht. F-Tasten ausblenden
2630   MTGLZAHL=MTGLZAHL-1
2640   GOTO 2710
2650 '                                                            
2660 ' *** Exit "Stammsatz erfassen" 2
2670 N$="Stammsatz wird gespeichert...":GOSUB 13000 'Nachricht. F-Tasten ausblenden
2680 GOSUB 12140                                           'Stammsatz speichern
2690 GOSUB 13910                       'Erste Verwendungsnachweis-Seite anlegen
2700 MTGLMAX=MTGLZAHL:GOSUB 11320                             'MTGLMAX speichen
2710 GOSUB 13060                                             'Nachricht löschen
2720 RETURN
2730 ' 
2740 ' *** Stammsatz ändern von 03************ ********************************
2750 '
2760 MT$="STAMMSATZ ÄNDERN":MI$="G-011020":GOSUB 13130             'Basis-Maske
2770 FKEYS$="":GOSUB 13240                                 'F-Tasten einblenden
2780 PRINT FNCUR$(4,24)"Bitte geben Sie die Mitgliedsnummer" 
2790 PRINT FNCUR$(5,25)"des zu ändernden Stammsatzes ein."
2800 PRINT FNCUR$(6,23);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
2810 PRINT FNCUR$(7,24)"Beenden Sie die Eingabe mit RETURN."
2820 PRINT FNCUR$(9,39)"=>..."
2830 BADINPUT=0
2840 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
2850 Z=9:S=41:LNG=3:TYP=2:MUSS=1
2860 GOSUB 10380                                                'Feld-Eingabe+1
2870 IF FKEY=1 THEN GOSUB 17900:GOTO 2760 'Hilfe-Text 'TODO: Wert geht verloren
2880 IF FKEY=2 THEN 2130
2890 IF FKEY=3 THEN 1970
2900 IF FKEY=4 THEN GOSUB 13560:GOTO 2860                 'Zum Hauptmenü?-Frage
2910 '
2920 M=VAL(OUTTEXT$)                                        'RETURN verarbeiten
2930 IF M>=1 AND M<=MTGLMAX THEN 2980
2940   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 12920         'Nachricht
2950   BADINPUT=1
2960   INTEXT$=OUTTEXT$
2970   GOTO 2850
2980 MTGLZAHL=M
2990 N$="Stammsatz wird gelesen...":GOSUB 13000 'Nachricht. F-Tasten ausblenden
3000 GOSUB 11890                                               'Stammsatz lesen
3010 GOSUB 13060                                             'Nachricht löschen
3020 '
3030 GOSUB 13130                                                   'Basis-Maske
3040 FKEYS$="<F7>=Speichern":GOSUB 13240                   'F-Tasten einblenden
3050 GOSUB 11780                                      'Mitgliedsnummer ausgeben
3060 GOSUB 11650             'Feldnamen, Feldwerte und Feldplatzhalter ausgeben
3070 F=1
3080 Z=FEROW(F):S=FECOL(F)+XOFF:LNG=FELNG(F):TYP=FETYP(F):MUSS=FEMUSS(F)
3090 FKEYMASK$="12347":CKEYMASK$="12":INTEXT$=DAT$(F)
3100 GOSUB 10370                                                  'Feld-Eingabe
3110 IF ASCC<>ASCRET THEN 3150
3120   DAT$(F)=OUTTEXT$
3130   F=F+1:IF F>FELDMAX THEN F=1
3140   GOTO 3080
3150 IF CKEY=0 THEN 3200
3160   PRINT FNCUR$(Z,S);HIGHON$;DAT$(F);HIGHOFF$;STRING$(LNG-LEN(DAT$(F)),".")
3170   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FELDMAX
3180   IF CKEY=CKEYDN THEN F=F+1:IF F>FELDMAX THEN F=1
3190   GOTO 3080
3200 IF FKEY=1 THEN GOSUB 18010:GOTO 3030                           'Hilfe-Text
3210 IF FKEY=2 THEN GOSUB 3310:GOTO 2760               'Exit "Stammsatz ändern"
3220 IF FKEY=3 THEN GOSUB 3310:GOTO 1970               'Exit "Stammsatz ändern"
3230 IF FKEY<>4 THEN 3280 
3240   GOSUB 13640                              'Zum Hauptmenü?-Frage (einfach)
3250   IF JA=0 THEN GOSUB 13240:GOTO 3100                  'F-Tasten einblenden
3260   GOSUB 3310                                      'Exit "Stammsatz ändern"
3270   GOTO 20230                                                 'Programmende
3280 IF FKEY=7 THEN GOSUB 3350:GOTO 2760             'Exit "Stammsatz ändern" 2
3290 '
3300 ' *** Exit "Stammsatz ändern"
3310 Q$="Wollen Sie den Stammsatz speichern?":GOSUB 12340        'Ja/Nein-Frage
3320 IF JA=0 THEN RETURN
3330 '
3340 ' *** Exit "Stammsatz ändern" 2
3350 N$="Stammsatz wird gespeichert...":GOSUB 13000 'Nachricht. F-Tasten ausblenden
3360 GOSUB 12140                                           'Stammsatz speichern
3370 GOSUB 13060                                             'Nachricht löschen
3380 RETURN
3390 '
3400 ' *** Stammsatz löschen von 04********** *********************************
3410 '
3420 MT$="STAMMSATZ LÖSCHEN":MI$="G-011030":GOSUB 13130            'Basis-Maske
3430 FKEYS$="":GOSUB 13240                                 'F-Tasten einblenden
3440 PRINT FNCUR$(4,24);"Bitte geben Sie die Mitgliedsnummer"         'Menütext
3450 PRINT FNCUR$(5,25);"des zu löschenden Mitglieds ein."
3460 PRINT FNCUR$(6,23);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
3470 PRINT FNCUR$(7,15);"(Dabei wird nur der Status des Mitglieds verändert.)"
3480 PRINT FNCUR$(8,24);"Beenden Sie die Eingabe mit RETURN."
3490 PRINT FNCUR$(10,39);"=>..."
3500 BADINPUT=0
3510 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
3520 Z=10:S=41:LNG=3:TYP=2:MUSS=1
3530 GOSUB 10380                                                'Feld-Eingabe+1
3540 IF FKEY=1 THEN GOSUB 18190:GOTO 3420 'Hilfe-Text 'TODO: Wert geht verloren
3550 IF FKEY=2 THEN 2130
3560 IF FKEY=3 THEN 1970
3570 IF FKEY=4 THEN GOSUB 13560:GOTO 3530                 'Zum Hauptmenü?-Frage
3580 '
3590 M=VAL(OUTTEXT$)                                        'RETURN verarbeiten
3600 IF M>=1 AND M<=MTGLMAX THEN 3650
3610   N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12920       'Nachricht
3620   BADINPUT=1
3630   INTEXT$=OUTTEXT$
3640   GOTO 3520
3650 MTGLZAHL=M
3660 N$="Stammsatz wird gelesen...":GOSUB 13000 'Nachricht. F-Tasten ausblenden
3670 GOSUB 11890                                               'Stammsatz lesen
3680 GOSUB 13060                                             'Nachricht löschen
3690 '
3700 GOSUB 13130                                                   'Basis-Maske
3710 GOSUB 11780                                      'Mitgliedsnummer ausgeben
3720 GOSUB 11650             'Feldnamen, Feldwerte und Feldplatzhalter ausgeben
3730 Q$="Mitglied endgültig löschen?":GOSUB 12340                'Ja/Nein-Frage
3740 IF JA=0 THEN 3810
3750   DAT$(5)="ausgetreten"
3760   PRINT FNCUR$(FEROW(5),FECOL(5)+XOFF);HIGHON$;DAT$(5);HIGHOFF$;
3770   PRINT STRING$(FELNG(5)-LEN(DAT$(5)),".")   
3780   N$="Stammsatz wird gespeichert...":GOSUB 13000 'Nachricht. F-Tasten ausblenden
3790   GOSUB 12140                                         'Stammsatz speichern
3800   GOSUB 13060                                           'Nachricht löschen
3810 GOTO 3420
3820 '
3830 ' *** Stammsatz drucken von 02************ & 04********** ****************
3840 '
3850 MT$="STAMMSATZ DRUCKEN":MI$="G-011040":GOSUB 13130            'Basis-Maske
3860 FKEYS$="":GOSUB 13240                                 'F-Tasten einblenden
3870 PRINT FNCUR$(4,24)"Bitte geben Sie die Mitgliedsnummer"
3880 PRINT FNCUR$(5,23)"des auszudruckenden Stammsatzes ein."
3890 PRINT FNCUR$(6,23);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
3900 PRINT FNCUR$(7,24)"Beenden Sie die Eingabe mit RETURN."
3910 PRINT FNCUR$(9,39);"=>..."
3920 BADINPUT=0
3930 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
3940 Z=9:S=41:LNG=3:TYP=2:MUSS=1
3950 GOSUB 10380                                                'Feld-Eingabe+1
3960 IF FKEY=1 THEN GOSUB 18330:GOTO 3850 'Hilfe-Text 'TODO: Wert geht verloren
3970 IF FKEY=2 THEN 2130
3980 IF FKEY=3 THEN 1970
3990 IF FKEY=4 THEN GOSUB 13560:GOTO 3950                 'Zum Hauptmenü?-Frage
4000 '
4010 M=VAL(OUTTEXT$)                                        'RETURN verarbeiten
4020   IF M>=1 AND M<=MTGLMAX THEN 4070
4030   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 12920         'Nachricht
4040   BADINPUT=1
4050   INTEXT$=OUTTEXT$                  
4060   GOTO 3940
4070 MTGLZAHL=M
4080 N$="Stammsatz wird gelesen...":GOSUB 13000 'Nachricht. F-Tasten ausblenden
4090 GOSUB 11890                                               'Stammsatz lesen
4100 GOSUB 13060                                             'Nachricht löschen
4110 '
4120 GOSUB 13130                                                   'Basis-Maske
4130 GOSUB 11780                                      'Mitgliedsnummer ausgeben
4140 GOSUB 11650             'Feldnamen, Feldwerte und Feldplatzhalter ausgeben
4150 GOSUB 15850                                         'Drucker bereit?-Frage
4160 IF JA=0 THEN 4180
4170   GOSUB 16540                        'Stammsatz drucken (Bildschirmformat)
4180 GOTO 3850
4190 '
4200 ' *** Stammsatz suchen (und drucken) von 05*************** ***************
4210 '
4220 DRUCKFLAG=0
4230 MT$="STAMMSATZ SUCHEN":IF DRUCKFLAG THEN MT$="SUCHEN & DRUCKEN"
4240 MI$="G-011050":IF DRUCKFLAG THEN MI$="G-013010"
4250 GOSUB 13130                                                   'Basis-Maske
4260 FKEYS$="":GOSUB 13240                                 'F-Tasten einblenden
4270 TMP$="Suchen":IF DRUCKFLAG THEN TMP$="Drucken" 
4280 PRINT FNCUR$(4,30)"1 - ";TMP$;" nach Nummern"
4290 PRINT FNCUR$(6,30)"2 - ";TMP$;" nach Maske"
4300 FKEYMAX=4:NKEYMAX=2:GOSUB 13400                              'Menü-Eingabe
4310 IF NKEY THEN ON NKEY GOTO 4400,5000
4320 IF FKEY<>1 THEN 4350 
4330   IF DRUCKFLAG THEN GOSUB 19540 ELSE GOSUB 18450               'Hilfe-Text
4340   GOTO 4230
4350 IF FKEY=2 THEN IF DRUCKFLAG THEN 7360 ELSE 2130
4360 IF FKEY=3 THEN 1970
4370 IF FKEY=4 THEN GOSUB 13560:GOTO 4300                 'Zum Hauptmenü?-Frage
4380 '
4390 ' *** Suchen nach Nummern
4400 TMP$=STR$(1):NUMSUCH$(1)=MID$(TMP$,2,99)
4410 TMP$=STR$(MTGLMAX):NUMSUCH$(2)=MID$(TMP$,2,99)
4420 IF DRUCKFLAG=0 THEN MT$="STAMMSATZ SUCHEN NACH NUMMERN":MI$="G-011051"
4430 IF DRUCKFLAG=1 THEN MT$="SUCHEN & DRUCKEN NACH NUMMERN":MI$="G-013011"
4440 GOSUB 13130                                                   'Basis-Maske
4450 FKEYS$="<F5>=Suchen":IF DRUCKFLAG THEN FKEYS$="<F5>=Suchen & Drucken"
4460 GOSUB 13240                                           'F-Tasten einblenden
4470 TMP$="gesucht":IF DRUCKFLAG THEN TMP$="gesucht und gedruckt"
4480 TMP1=13:IF DURCKFLAG THEN TMP1=6
4490 TMP2=67:IF DURCKFLAG THEN TMP2=73
4500 PRINT FNCUR$(4,25);"Gültige Mitgliedsnummern: 1 -";STR$(MTGLMAX)
4510 PRINT FNCUR$(6,TMP1);"Ab welcher Mitgliedsnummer soll ";TMP$;" werden?     =>..."
4520 PRINT FNCUR$(8,TMP1);"Bis zu welcher Mitgliedsnummer soll ";TMP$;" werden? =>..."
4530 PRINT FNCUR$(6,TMP2);HIGHON$;NUMSUCH$(1);HIGHOFF$
4540 PRINT FNCUR$(8,TMP2);HIGHON$;NUMSUCH$(2);HIGHOFF$
4550 F=1
4560 BADINPUT=0
4570 FKEYMASK$="12345":CKEYMASK$="":INTEXT$=NUMSUCH$(F)
4580 Z=NUMSUCHROW(F):S=67:LNG=3:TYP=2:MUSS=1
4590 GOSUB 10380                                                'Feld-Eingabe+1
4600 IF FKEY<>1 THEN 4630                                           'Hilfe-Text
4610  IF DRUCKFLAG THEN GOSUB 19620 ELSE GOSUB 18690
4620  GOTO 4420                                     'TODO: Werte gehen verloren
4630 IF FKEY=2 THEN 4230
4640 IF FKEY=3 THEN 1970
4650 IF FKEY=4 THEN GOSUB 13560:GOTO 4590                 'Zum Hauptmenü?-Frage
4660 IF FKEY<>5 THEN 4720
4670   SEARCH=BYNUMS
4680   NUMSUCH1=VAL(NUMSUCH$(1))
4690   NUMSUCH2=VAL(NUMSUCH$(2))
4700   IF DRUCKFLAG THEN 8080 ELSE 5300           'Suchen oder Suchen & Drucken
4710 '
4720 M=VAL(OUTTEXT$)                                        'RETURN verarbeiten
4730 IF M>=1 AND M<=MTGLMAX THEN 4770
4740   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 12920         'Nachricht
4750   GOSUB 4920                                     'Eingabefehler-Behandlung
4760   GOTO 4580
4770 NUMSUCH$(F)=OUTTEXT$
4780 NUMSUCH1=VAL(NUMSUCH$(1))
4790 NUMSUCH2=VAL(NUMSUCH$(2))
4800 IF NUMSUCH1<=NUMSUCH2 THEN 4840
4810   N$="Ungültiger Bereich":GOSUB 12920                           'Nachricht
4820   GOSUB 4920                                     'Eingabefehler-Behandlung
4830   GOTO 4580
4840 F=F+1:IF F>NUMSUCHMAX THEN F=1
4850 GOSUB 13500                                           'F-Tasten ausblenden
4860 FKEYMASK$="12345"                                           'TODO: obsolet
4870 FKEYS$="<F5>=Suchen":IF DRUCKFLAG THEN FKEYS$="<F5>=Suchen & Drucken" 
4880 GOSUB 13240                                           'F-Tasten einblenden
4890 GOTO 4570
4900 '
4910 ' *** Eingabefehler-Behandlung
4920 BADINPUT=1
4930 INTEXT$=OUTTEXT$
4940 FKEYMASK$="1234"
4950 GOSUB 13500                                           'F-Tasten ausblenden
4960 FKEYS$="":GOSUB 13240                                 'F-Tasten einblenden
4970 RETURN
4980 '
4990 ' *** Suchen nach Maske
5000 GOSUB 11170                                                'SUCH$() leeren
5010 IF DRUCKFLAG=0 THEN MT$="STAMMSATZ SUCHEN NACH MASKE":MI$="G-011052"
5020 IF DRUCKFLAG=1 THEN MT$="SUCHEN & DRUCKEN NACH MASKE":MI$="G-013012"
5030 GOSUB 13130                                                   'Basis-Maske
5040 FKEYS$="<F5>=Suchen":IF DRUCKFLAG THEN FKEYS$="<F5>=Suchen & Drucken" 
5050 GOSUB 13240                                           'F-Tasten einblenden
5060 GOSUB 11410                        'Feldnamen und Feldplatzhalter ausgeben
5070 F=1
5080 Z=FEROW(F):S=FECOL(F)+XOFF:LNG=FELNG(F):TYP=3
5090 FKEYMASK$="12345":CKEYMASK$="12":INTEXT$=""
5100 GOSUB 10370                                                  'Feld-Eingabe
5110 IF FKEY<>1 THEN 5140 
5120   IF DRUCKFLAG THEN GOSUB 19700 ELSE GOSUB 18770
5130   GOTO 5010
5140 IF FKEY=2 THEN 4230
5150 IF FKEY=3 THEN 1970
5160 IF FKEY=4 THEN GOSUB 13560:GOTO 5100                 'Zum Hauptmenü?-Frage
5170 IF FKEY<>5 THEN 5200 
5180   SEARCH=BYMASK                                                      '<F5>
5190   IF DRUCKFLAG THEN 8080 ELSE 5300           'Suchen oder Suchen & Drucken
5200 IF CKEY=0 THEN 5250                              'Cursortasten verarbeiten
5210   PRINT FNCUR$(Z,S);HIGHON$;SUCH$(F);HIGHOFF$;STRING$(LNG-LEN(SUCH$(F)),".")
5220   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FELDMAX
5230   IF CKEY=CKEYDN THEN F=F+1:IF F>FELDMAX THEN F=1
5240   GOTO 5080
5250 SUCH$(F)=OUTTEXT$                                      'RETURN verarbeiten
5260 F=F+1:IF F>FELDMAX THEN F=1
5270 GOTO 5080
5280 '
5290 ' *** Suche
5300 GOSUB 13130                                                   'Basis-Maske
5310 FKEYS$="<F5>=Suchen rückwärts <F6>=Suchen vorwärts <F7>=Adressaufkleber" 
5320 GOSUB 13240                                           'F-Tasten einblenden
5330 PRINT FNCUR$(3,31);"Mitgliedsnummer"
5340 GOSUB 11410                        'Feldnamen und Feldplatzhalter ausgeben
5350 '
5360 IF SEARCH=BYMASK THEN 5410
5370 '
5380 MTGLZAHL=NUMSUCH1                      'Initialisierung Suche nach Nummern
5390 GOTO 5540
5400 '
5410 MTGLZAHL=0                               'Initialisierung Suche nach Maske
5420 GOSUB 6240                        'Nächsten Treffer suchen IN/OUT:MTGLZAHL
5430 IF MTGLZAHL<>-1 THEN 5540
5440   N$="Kein Suchtreffer. Mit <F2> neue Suche.":GOSUB 12920       'Nachricht
5450   FKEYS$="":GOSUB 13240                               'F-Tasten einblenden
5460   FKEYMASK$="1234":NKEYMASK="":GOSUB 13400                   'Menü-Eingabe
5470   IF FKEY<>1 THEN 5500
5480     IF DRUCKFLAG THEN GOSUB 19780 ELSE GOSUB 18850             'Hilfe-Text
5490     GOTO 5010
5500   IF FKEY=2 THEN 5010
5510   IF FKEY=3 THEN 1970
5520   IF FKEY=4 THEN GOSUB 13560:GOTO 5460               'Zum Hauptmenü?-Frage
5530 '
5540 GOSUB 5990                                 'Suchtreffer lesen und anzeigen
5550 '
5560 FKEMAX=7:NKEYMAX=0:GOSUB 13400                               'Menü-Eingabe
5570 ON FKEY GOTO 5600,5640,1970,5670,5700,5840,6060
5580 '
5590 ' *** <F1> Hilfe
5600 IF DRUCKFLAG THEN GOSUB 19860 ELSE GOSUB 18930                 'Hilfe-Text
5610 GOTO 5300 'TODO <F6><F6><F1> -> wieder beim 3 Treffer? 'TODO: Werte gehen verloren
5620 '
5630 ' *** <F2> Zurück
5640 IF SEARCH=BYNUMS THEN 4400 ELSE 5010           'TODO: Werte gehen verloren
5650 '
5660 ' *** <F4> Hauptmenü
5670 GOSUB 13560:GOTO 5560                                'Zum Hauptmenü?-Frage
5680 '
5690 ' *** <F5> Suchen rückwärts
5700 IF SEARCH=BYMASK THEN 5740
5710   MTGLZAHL=MTGLZAHL-1                        'Suchen rückwärts nach Nummer
5720   IF MTGLZAHL<NUMSUCH1 THEN 5800
5730     GOSUB 5990:GOTO 5560                   'Suchtreffer lesen und anzeigen
5740 TMP=MTGLZAHL                                  'Suchen rückwärts nach Maske
5750 GOSUB 6160                      'Vorherigen Treffer suchen IN/OUT:MTGLZAHL
5760 IF MTGLZAHL=-1 THEN 5790
5770   GOSUB 5990:GOTO 5560                     'Suchtreffer lesen und anzeigen
5780 '
5790 MTGLZAHL=TMP                                               'No match found
5800 N$="Erster Suchtreffer":GOSUB 12920                             'Nachricht
5810 GOTO 5560
5820 '
5830 ' *** <F6> Suchen vorwärts
5840 IF SEARCH=BYMASK THEN 5890
5850   MTGLZAHL=MTGLZAHL+1                         'Suchen vorwärts nach Nummer
5860   IF MTGLZAHL>NUMSUCH2 THEN 5950
5870     GOSUB 5990:GOTO 5560                   'Suchtreffer lesen und anzeigen
5880 '
5890 TMP=MTGLZAHL                                    'Suche vorwärts nach Maske
5900 GOSUB 6240                        'Nächsten Treffer suchen IN/OUT:MTGLZAHL
5910 IF MTGLZAHL=-1 THEN 5940
5920   GOSUB 5990:GOTO 5560                     'Suchtreffer lesen und anzeigen
5930 '
5940 MTGLZAHL=TMP                                               'No match found
5950 N$="Letzter Suchtreffer":GOSUB 12920                            'Nachricht
5960 GOTO 5560
5970 '
5980 ' *** Suchtreffer lesen und anzeigen
5990 GOSUB 13060                                             'Nachricht löschen
6000 GOSUB 11890                                               'Stammsatz lesen
6010 GOSUB 11530                                            'Feldwerte ausgeben
6020 PRINT FNCUR$(3,46);HIGHON$;STR$(MTGLZAHL);HIGHOFF$
6030 RETURN
6040 '
6050 ' *** <F7> Adressaufkleber drucken
6060 GOSUB 15850                                         'Drucker bereit?-Frage
6070 IF JA=0 THEN 6120                              'TODO: F-Tasten einblenden?
6080   N$="Adressaufkleber wird gedruckt...":GOSUB 12920             'Nachricht
6090   GOSUB 16360                                     'Adressaufkleber drucken
6100   GOSUB 13060                                           'Nachricht löschen
6110   GOSUB 13240                                         'F-Tasten einblenden
6120 GOTO 5560                                                    'Neue Eingabe
6130 '
6140 ' *** Subroutine Vorherigen Treffer suchen
6150 ' IN: MTGLZAHL OUT: MTGLZAHL or -1 if not found
6160 MTGLZAHL=MTGLZAHL-1
6170 IF MTGLZAHL<1 THEN 6300
6180 GOSUB 6340
6190 IF NOT ISMATCH THEN 6160 
6200 RETURN
6210 '
6220 ' *** Subroutine Nächsten Treffer suchen
6230 ' IN: MTGLZAHL OUT: MTGLZAHL or -1 if not found
6240 MTGLZAHL=MTGLZAHL+1
6250 IF MTGLZAHL>MTGLMAX THEN 6300
6260 GOSUB 6340
6270 IF NOT ISMATCH THEN 6240 
6280 RETURN
6290 '
6300 MTGLZAHL=-1
6310 RETURN
6320 '
6330 ' *** Subroutine Treffer?
6340 GOSUB 11890                                               'Stammsatz lesen
6350 FOR K=1 TO FELDMAX
6360   IF LEFT$(SUCH$(K),1)<>"*" THEN 6390
6370     IF INSTR(DAT$(K),RIGHT$(SUCH$(K),LEN(SUCH$(K))-1))>0 THEN 6440
6380     GOTO 6480
6390   I=1
6400   IF MID$(SUCH$(K),I,1)="?" THEN I=I+1:GOTO 6400
6410   I=I-1
6420   IF INSTR(DAT$(K),RIGHT$(SUCH$(K),LEN(SUCH$(K))-I))=I+1 THEN 6440
6430   GOTO 6480
6440 NEXT K
6450 ISMATCH=1
6460 RETURN
6470 '
6480 ISMATCH=0
6490 RETURN
6500 '
6510 ' ######################### Verwendungsnachweis ##########################
6520 '
6530 GOSUB 11230                                                 'MTGLMAX lesen
6540 VNFLAG=-1
6550 '
6560 ' Verwendungsnachweis-Menü ***********************************************
6570 '
6580 MT$="VERWENDUNGSNACHWEIS":MI$="G-012000":GOSUB 13130          'Basis-Maske
6590 F=4                                                                'TODO ?
6600 FKEYS$="":GOSUB 13240                                 'F-Tasten einblenden
6610 PRINT CURON$;
6620 PRINT HIGHOFF$;
6630 PRINT FNCUR$(4,20)"   Bitte geben Sie die Mitgliedsnummer"
6640 PRINT FNCUR$(5,20)"des zu ändernden Verwendungsnachweises ein"
6650 'LW PRINT FNCUR$(6,23);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
6660 PRINT FNCUR$(6,20)" und beenden Sie die Eingabe mit RETURN."
6670 PRINT FNCUR$(8,20)"                 =>..."
6680 Z=8:S=39:LNG=3:TYP=2
6690 GOSUB 10370                                                  'Feld-Eingabe
6700 IF FKEY=1 THEN GOSUB 19010:GOTO 6580                           'Hilfe-Text
6710 IF FKEY=2 OR FKEY=3 THEN GOSUB 11120:VNFLAG=0:GOTO 1970     'DAT$() leeren
6720 IF FKEY=4 THEN GOSUB 13560                           'Zum Hauptmenü?-Frage
6730 IF FKEY>4 THEN 6680
6740 IF CKEY<>0 THEN 6680
6750 IF LEN(OUTTEXT$)=0 THEN N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 12920:GOTO 6670 ELSE MITGLIED=VAL(OUTTEXT$):IF MITGLIED=0 THEN 6690 'Nachricht
6760 IF MITGLIED>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12920:PRINT HOME$;HIGHOFF$:GOTO 6670 'Nachricht
6770 MTGLZAHL=MITGLIED:MTGL$=STR$(MITGLIED)
6780 '
6790 ' *** Lesen des Mitgliedsnamens ******************************************
6800 '
6810 PRINT FNCUR$(21,1);DELLINE$;
6820 GET #2,MITGLIED                                           'DATEN STS lesen
6830 NNAM$=LEFT$(DD1$,25)                                 'NACHNAME RAUSFISCHEN
6840 VNAM$=MID$(DD1$,26,25)                               'VORNAME  RAUSFISCHEN
6850 '
6860 ' ** LEERZEICHEN HINTER NACHNAME ABSCHNEIDEN *****************************
6870 '
6880 FOR I=LEN(NNAM$) TO 1 STEP -1
6890   IF MID$(NNAM$,I,1)<>" " THEN NNAM$=LEFT$(NNAM$,I):I=1
6900 NEXT
6910 NAMM$=NNAM$+" "+VNAM$
6920 GOSUB 14080                                             '1. VN-BLATT lesen
6930 BLATT=1
6940 VP=1:HP=1                                                 'INITIALISIERUNG
6950 F=8
6960 GOSUB 13130                                                   'Basis-Maske
6970 FKEYS$="<F7>=Speichern":GOSUB 13240                   'F-Tasten einblenden
6980 PRINT HIGHOFF$;
6990 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=Vorwärts";TAB(53);"<F9>=Blatt drucken";
7000 PRINT FNCUR$(3,2);"Blatt:"
7010 PRINT FNCUR$(5,2);"Mitglieds-Nr.:"
7020 PRINT FNCUR$(5,24);"Name:"
7030 MSK$="Von:  .  .   Bis:  .  .   Amt:"
7040 FOR KK=7 TO 19 
7050   PRINT FNCUR$(KK,2);MSK$;TAB(43);MSK$                    'LW NO semicolon
7060 NEXT
7070 GOSUB 15290                                                 'Ausgabe Daten
7080 LNG=2-(HP=7)*6                                    'BEGIN OF THE INPUT-LOOP
7090 TYP=2-(HP=7)
7100 Z=VP+13*(VP>13)+6
7110 S=6-39*(VP>13)-(HP<7)*(HP-1)*3+(HP>3)*(HP<7)*4-(HP=7)*26
7120 GOSUB 13060                                             'Nachricht löschen
7130 GOSUB 10370                                                  'Feld-Eingabe
7140 IF HP<7 THEN IF ASCC=ASCRET THEN OUTTEXT$=STRING$(2-LEN(OUTTEXT$),"0")+OUTTEXT$   '+NULLEN
7150 IF CKEY<>0 OR ASCC=ASCRET THEN PRINT FNCUR$(Z,S);OUTTEXT$
7160 IF ASCC=ASCRET THEN IF HP<7 THEN MID$(DAT$(VP),-1+2*HP,2)=OUTTEXT$ ELSE DAT$(VP)=LEFT$(DAT$(VP),12)+OUTTEXT$+SPACE$(8-LEN(OUTTEXT$))
7170 IF ASCC=ASCRET AND HP<7 THEN JA=1:TEST$=OUTTEXT$:ON HP GOSUB 12680,12710,12740,12680,12710,12740:IF JA=0 THEN N$="Ungültiges Datum":GOSUB 12920:GOTO 7130 'Datums-Check 'Nachricht
7180 IF ASCC=ASCRET THEN IF HP=7 THEN HP=1:VP=VP-(VP<26)-(VP=26)*(-25):GOTO 7080 ELSE HP=HP+1:GOTO 7080     'NEUE SCREEN-POSITION
7190 IF CKEY<>0 THEN TEST$=MID$(DAT$(VP),-(HP<7)*2*HP-1-(HP=7)*14,-(HP<7)*2-(HP=7)*8):PRINT FNCUR$(Z,S);TEST$
7200 IF CKEY<>0 THEN IF HP<7 AND NOT TEST$="  " THEN JA=1:ON HP GOSUB 12680,12710,12740,12680,12710,12740:IF JA=0 THEN N$="Ungültiges Datum":GOSUB 12920:GOTO 7130 'Nachricht
7210 IF CKEY=CKEYDN THEN VP=VP-(VP<26)-(VP=26)*(-25):GOTO 7080
7220 IF CKEY=CKEYUP THEN VP=VP+(VP>1)-(VP=1)*25     :GOTO 7080
7230 IF CKEY=CKEYRT THEN HP=HP-(HP<7)-(HP=7)*(-6)   :GOTO 7080
7240 IF CKEY=CKEYLT THEN HP=HP+(HP>1)+(HP=1)*(-6)   :GOTO 7080
7250 IF FKEY=1 THEN GOSUB 19120 :GOTO 6940                          'Hilfe-Text
7260 IF FKEY=2 THEN 6580                                       'ZU MTGL-EINGABE
7270 IF FKEY=3 THEN VNFLAG=0:GOSUB 11120:GOTO 1970               'DAT$() leeren
7280 IF FKEY=4 THEN GOSUB 13560:GOSUB 13300:GOTO 7130     'Zum Hauptmenü?-Frage
7290 IF FKEY=5 THEN GOSUB 14170:GOTO 7080
7300 IF FKEY=6 THEN GOSUB 14310:GOTO 7080
7310 IF FKEY=7 THEN GOSUB 14950:GOTO 7130                 'SPEICHERN DANN INPUT
7320 IF FKEY=9 THEN GOSUB 15530:GOSUB 14500:GOTO 7120
7330 '
7340 ' ############# Mitgliedsliste drucken von 05*************** #############
7350 '
7360 MT$="MITGLIEDSLISTE DRUCKEN":MI$="G-013000":GOSUB 13130       'Basis-Maske
7370 FKEYS$="":GOSUB 13240                                 'F-Tasten einblenden
7380 PRINT FNCUR$(4,28);"1 - Suchen & Drucken"
7390 PRINT FNCUR$(6,28);"2 - Komplett drucken"
7400 PRINT FNCUR$(8,28);"3 - Datenblatt drucken"
7410 FKEYMAX=4:NKEYMAX=3:GOSUB 13400                              'Menü-Eingabe
7420 IF NKEY THEN ON NKEY GOTO 7500,7500,8490
7430 IF FKEY=1 THEN GOSUB 19310:GOTO 7360                           'Hilfe-Text
7440 IF FKEY=2 THEN 1970
7450 IF FKEY=3 THEN 1970
7460 IF FKEY=4 THEN GOSUB 13560:GOTO 7410                 'Zum Hauptmenü?-Frage
7470 '
7480 ' *** Suchen & Drucken, Komplett drucken *********************************
7490 '
7500 IF NKEY=1 THEN MT$="SUCHEN & DRUCKEN":MI$="G-013010"
7510 IF NKEY=2 THEN MT$="KOMPLETT DRUCKEN":MI$="G-013020"
7520 GOSUB 13130                                                   'Basis-Maske
7530 GOSUB 13240                                           'F-Tasten einblenden
7540 PRINT FNCUR$(4,21);"Bitte geben Sie das heutige Datum ein."
7550 PRINT FNCUR$(5,22);"Beenden Sie die Eingabe mit RETURN."
7560 PRINT FNCUR$(7,35);"=>........"
7570 BADINPUT=0
7580 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
7590 Z=7:S=37:LNG=8:TYP=5:MUSS=1
7600 GOSUB 10380                                                'Feld-Eingabe+1
7610 IF FKEY=1 THEN GOSUB 19940:GOTO 7500 'Hilfe-Text 'TODO: Wert geht verloren
7620 IF FKEY=2 THEN 7360
7630 IF FKEY=3 THEN 1970
7640 IF FKEY=4 THEN GOSUB 13560:GOTO 7600                 'Zum Hauptmenü?-Frage
7650 '
7660 POS1=INSTR(OUTTEXT$,".")                               'RETURN verarbeiten
7670 POS2=INSTR(POS1+1,OUTTEXT$,".")
7680 IF POS2=0 THEN 7910
7690 POS3=INSTR(POS2+1,OUTTEXT$,".")
7700 IF POS3<>0 THEN 7910
7710 D1$=LEFT$(OUTTEXT$,POS1-1)
7720 IF LEN(D1$)=0 OR LEN(D1$)>2 THEN 7910
7730 D2$=MID$(OUTTEXT$,POS1+1,POS2-POS1-1)
7740 IF LEN(D2$)=0 OR LEN(D2$)>2 THEN 7910
7750 D3$=RIGHT$(OUTTEXT$,LEN(OUTTEXT$)-POS2)
7760 IF LEN(D3$)=0 OR LEN(D3$)>2 THEN 7910
7770 '
7780 D1=VAL(D1$):IF D1<1 OR D1>31 THEN 7940
7790 D2=VAL(D2$):IF D2<1 OR D2>12 THEN 7940
7800 D3=VAL(D3$) 
7810 ' TODO: Deeper validation
7820 D1$=STR$(D1):D1$=MID$(D1$,2,99):IF D1<10 THEN D1$="0"+D1$
7830 D2$=STR$(D2):D2$=MID$(D2$,2,99):IF D2<10 THEN D2$="0"+D2$
7840 D3$=STR$(D3):D3$=MID$(D3$,2,99):IF D3<10 THEN D3$="0"+D3$
7850 DATUM$=D1$+"."+D2$+"."+D3$
7860 '
7870 PRINT FNCUR$(7,37);HIGHON$;DATUM$;HIGHOFF$
7880 GOTO 7990
7890 '
7900 ' *** Eingabe-Fehlerbehandlung Datum
7910 N$="Datum muss Format TT.MM.JJ haben. Bitte korrigieren.":GOSUB 12920 'Nachricht
7920 GOTO 7950
7930 '
7940 N$="Ungültiges Datum. Bitte korrigieren.":GOSUB 12920           'Nachricht
7950 BADINPUT=1
7960 INTEXT$=OUTTEXT$
7970 GOTO 7590
7980 '
7990 IF NKEY=2 THEN 8130
8000 '
8010 ' *** Suchen nach Nummern und Maske
8020 DRUCKFLAG=1
8030 GOTO 4230                                  'Stammsatz suchen (und drucken)
8040 ' *** Rückkehr aus "Stammsatz suchen"
8050 ' IN: SEARCH (BYNUMS, BYMASK)
8060 ' IN: NUMSUCH1, NUMSUCH2
8070 ' IN: SUCH$()
8080 GOSUB 15850                                         'Drucker bereit?-Frage
8090 IF JA=0 THEN 8450
8100 GOTO 8180
8110 '
8120 ' *** Komplette Mitgliedliste drucken
8130 SEARCH=BYNUMS
8140 NUMSUCH1=1:NUMSUCH2=MTGLMAX
8150 GOSUB 15850                                         'Drucker bereit?-Frage
8160 IF JA=0 THEN 7500
8170 '
8180 N$="Mitgliedsliste wird gedruckt... Abbrechen mit Taste.":GOSUB 12920 'Nachricht
8190 GOSUB 16020                                 'Mitgliedsliste drucken (Kopf)
8200 IF SEARCH=BYMASK THEN 8330
8210 '
8220 ' *** Suchen nach Nummern
8230 FOR MTGLZAHL=NUMSUCH1 TO NUMSUCH2
8240   GOSUB 11890                                             'Stammsatz lesen
8250   IF INSTR(DAT$(5),"ausgetreten")>0 THEN 8270
8260     GOSUB 16160                         'Mitgliedsliste drucken (Mitglied)
8270   GOSUB 15920                                      'Druck abbrechen? Frage
8280   IF JA=1 THEN 8440
8290 NEXT MTGLZAHL
8300 GOTO 8440
8310 ' 
8320 ' *** Suchen nach Maske
8330 MTGLZAHL=0
8340 GOSUB 6240                        'Nächsten Treffer suchen IN/OUT:MTGLZAHL
8350 IF MTGLZAHL=-1 THEN 8440
8360   GOSUB 11890                    'Stammsatz lesen, IN:MTGLZAHL, OUT DAT$()
8370   IF INSTR(DAT$(5),"ausgetreten")>0 THEN 8390
8380     GOSUB 16160                         'Mitgliedsliste drucken (Mitglied)
8390   GOSUB 15920                                      'Druck abbrechen? Frage
8400   IF JA=1 THEN 8440
8410 GOTO 8350
8420 '
8430 ' *** Suchen zuende
8440 GOSUB 13060                                             'Nachricht löschen
8450 IF SEARCH=BYNUMS THEN 4400 ELSE 5000                'TODO: Very hackish...
8460 '
8470 ' *** Datenblatt drucken *************************************************
8480 '
8490 MT$="DATENBLATT DRUCKEN":MI$="G-013030":GOSUB 13130           'Basis-Maske
8500 FKEYS$="":GOSUB 13240                                 'F-Tasten einblenden
8510 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer"
8520 PRINT FNCUR$(5,23)"des zu druckenden Datenblattes ein."
8530 PRINT FNCUR$(7,23)"Beenden Sie die Eingabe mit RETURN."
8540 PRINT FNCUR$(8,23);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
8550 PRINT FNCUR$(10,39)"=>..."
8560 BADINPUT=0
8570 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
8580 Z=10:S=41:LNG=3:TYP=2:MUSS=1
8590 GOSUB 10380                                                'Feld-Eingabe+1
8600 IF FKEY=1 THEN GOSUB 20020:GOTO 8490 'Hilfe-Text 'TODO: Wert geht verloren
8610 IF FKEY=2 THEN 7360
8620 IF FKEY=3 THEN 1970
8630 IF FKEY=4 THEN GOSUB 13560:GOTO 8590                 'Zum Hauptmenü?-Frage
8640 '
8650 M=VAL(OUTTEXT$)                                        'RETURN verarbeiten
8660 IF M>=1 AND M<=MTGLMAX THEN 8710
8670   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 12920         'Nachricht
8680   BADINPUT=1
8690   INTEXT$=OUTTEXT$                                           
8700   GOTO 8580
8710 MTGLZAHL=M
8720 GOSUB 15850                                         'Drucker bereit?-Frage
8730 IF JA=1 THEN 8770
8740   GOSUB 13240                                         'F-Tasten einblenden
8750   INTEXT$=OUTTEXT$
8760   GOTO 8580
8770 N$="Datenblatt drucken...":GOSUB 13000     'Nachricht. F-Tasten ausblenden
8780 GOSUB 11890                                               'Stammsatz lesen
8790 NAMM$ = DAT$(1)+" "+DAT$(2)
8800 GOSUB 16540                          'Stammsatz drucken (Bildschirmformat)
8810 GOSUB 14080                                     'Verwendungsnachweis lesen
8820 GOSUB 15560                                   'Verwendungsnachweis drucken
8830 GOSUB 16810                                     'Mitgliedsvermerke drucken
8840 GOSUB 13060                                             'Nachricht löschen
8850 GOTO 8490
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
10460   GOSUB 13060                                          'Nachricht löschen
10470   PRINT FNCUR$(Z,S+P);
10480   BADINPUT=0
10490 ON TYP GOSUB 12430,12480,12530,12580,12630                    'Typ prüfen
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
11740 '
11750 ' *** Mitgliedsnummer ausgeben
11760 '
11770 ' IN: MTGLZAHL
11780 PRINT FNCUR$(3,31);"Mitgliedsnummer";HIGHON$;MTGLZAHL;HIGHOFF$
11790 RETURN
11800 '
11810 ' *** FIELDen der Stammsatz-Datei ****************************** REVIEWED
11820 '
11830 OPEN "R",#2,"B:MV.DAT",462
11840 FIELD #2,236 AS DD1$,225 AS DD2$
11850 RETURN
11860 '
11870 ' *** Stammsatz von Disk lesen ******************************************
11880 '
11890 GET #2,MTGLZAHL
11900 GOSUB 11120                                                 'Clear DAT$()
11910 I=1
11920 FOR K=1 TO 14
11930   LNG=FELNG(K)
11940   TMP$=MID$(DD1$,I,LNG):GOSUB 12050:DAT$(K)=TMP$
11950   I=I+LNG
11960 NEXT
11970 I=1
11980 FOR K=15 TO FELDMAX
11990   LNG=FELNG(K)
12000   TMP$=MID$(DD2$,I,LNG):GOSUB 12050:DAT$(K)=TMP$
12010   I=I+LNG
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
12190 LSET DD1$=TMP$
12200 TMP$=""
12210 FOR K=15 TO FELDMAX
12220   LNG=FELNG(K)
12230   TMP$=TMP$+DAT$(K)+SPACE$(LNG-LEN(DAT$(K)))
12240 NEXT K
12250 LSET DD2$=TMP$
12260 TMP$=""
12270 PUT #2,MTGLZAHL
12280 RETURN
12290 '
12300 ' *** Ja/Nein-Frage und F-Tasten ausblenden ******************** REVIEWED
12310 '
12320 ' IN:  Q$ - Frage
12330 ' OUT: JA - (1=yes, 0=no)
12340 N$=Q$+" (J=Ja/N=Nein)":GOSUB 12920                             'Nachricht
12350 GOSUB 13500                                          'F-Tasten ausblenden
12360 ANSWER$=INPUT$(1)
12370 IF ANSWER$="J" OR ANSWER$="j" THEN JA=1:RETURN
12380 IF ANSWER$="N" OR ANSWER$="n" THEN JA=0:RETURN
12390 GOTO 12360
12400 '
12410 ' *** Testbereich 1 (Alpha) *********************************************
12420 '
12430 VALID=0: IF (ASCC>57 AND ASCC<127) OR (ASCC>31 AND ASCC<48) THEN VALID=1
12440 RETURN
12450 '
12460 ' *** Testbereich 2 (Numerisch) *****************************************
12470 '
12480 VALID=0: IF ASCC>47 AND ASCC<58 THEN VALID=1
12490 RETURN
12500 '
12510 ' *** Testbereich 3 (Alphanumerisch) ************************************
12520 '
12530 VALID=0: IF ASCC>31 AND ASCC<127 THEN VALID=1
12540 RETURN
12550 '
12560 ' *** Testbereich 4 (Numerisch + "/" und ".") ***************************
12570 '
12580 VALID=0: IF ASCC>45 AND ASCC<58 THEN VALID=1
12590 RETURN
12600 '
12610 ' *** Testbereich 5 (Numerisch + und ".") *******************************
12620 '
12630 VALID=0: IF (ASCC>=48 AND ASCC<=57) OR ASCC=46 THEN VALID=1
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
12770 ' *** Voreinstellungen setzen im ERFASSEN *********************** REVIWED
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
12970 ' *** Nachricht anzeigen und F-Tasten ausblenden *************** REVIEWED
12980 '
12990 ' IN: N$ - Nachricht
13000 GOSUB 12920                                                    'Nachricht
13010 GOSUB 13500                                          'F-Tasten ausblenden
13020 RETURN
13030 '
13040 ' *** Nachricht löschen **************************************** REVIEWED
13050 '
13060 N$="":GOSUB 12920                                              'Nachricht
13070 RETURN
13080 '
13090 ' *** Bildschirmmaske - Basis ********************************** REVIEWED
13100 '
13110 ' IN: MT$ - Menu title
13120 ' IN: MI$ - Menu ID
13130 PRINT CLS$;
13140 PRINT "00**********************";
13150 PRINT FNCUR$(1,(80-LEN(MT$))/2+1);MT$;FNCUR$(1,72);MI$
13160 PRINT STRICH$
13170 PRINT FNCUR$(21,1);
13180 PRINT STRICH$             'quirk in display driver places this in line 22
13190 RETURN
13200 '
13210 ' *** F-Tasten einblenden ************************************** REVIEWED
13220 '
13230 ' IN: FKEYS$ - Additional function key labels 
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
13340 ' *** Eingabe bei Menübildern ********************************** REVIEWED
13350 '
13360 ' IN:  NKEYMAX - max valid numeric key (1,2,3,...)
13370 ' IN:  FKEYMAX - max valid function key (1,2,3,...)
13380 ' OUT: FKEY - number of pressed function key or 0
13390 ' OUT: NKEY - number of pressed numeric key or 0
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
13570 IF JA=1 THEN 20230                                          'Programmende
13580 GOSUB 13240                                          'F-Tasten einblenden
13590 RETURN
13600 '
13610 ' *** 'Zum Hauptmenü?-Frage (einfach) ************************** REVIEWED
13620 '
13630 ' OUT: JA (1=true, 0=false)
13640 Q$="Wollen Sie wirklich ins Hauptmenü?":GOSUB 12340        'Ja/Nein-Frage
13650 GOSUB 13060                                            'Nachricht löschen
13660 RETURN
13670 '
13680 ' *** Rückkehr aus Hilfe-Text? ********************************* REVIEWED
13690 '
13700 N$="Zurück? (J=Ja)":GOSUB 12920                                'Nachricht
13710 GOSUB 12350                                           'skip string concat
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
14450 IF JA=0 THEN 14500
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
15530 Q$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 12340 'Ja/Nein-Frage TODO Back to main code?
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
15820 ' *** Drucker bereit?-Frage
15830 '
15840 ' OUT: JA (1=Ja, 0=Nein)
15850 Q$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 12340 'Ja/Nein-Frage
15860 GOSUB 13060                                            'Nachricht löschen
15870 RETURN
15880 '
15890 ' *** Druck abbrechen? Frage
15900 '
15910 ' OUT: JA (1=true,0=false)
15920 JA=0 
15930 IF INKEY$="" THEN 15970
15940 Q$="Drucken abbrechen?":GOSUB 12340                        'Ja/Nein-Frage
15950 IF JA=1 THEN 15970
15960 N$="Mitgliedsliste wird gedruckt... Abbrechen mit Taste.":GOSUB 12920 'Nachricht
15970 RETURN
15980 '
15990 ' *** Mitgliedsliste drucken (Kopf)
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
16130 ' *** Mitgliedsliste drucken (Mitglied)
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
16340 ' *** Adressaufkleber drucken
16350 '                                                           Ja/Nein-Frage
16360 LIND$=SPACE$(3)
16370 '
16380 LPRINT LIND$;DAT$(7)                                              'Anrede
16390 LPRINT LIND$;LEFT$(DAT$(2),INSTR(DAT$(2)," "));DAT$(1)  'Vorname Nachname
16400 LPRINT
16410 LPRINT LIND$;DAT$(10)                                  'Straße Hausnummer
16420 LPRINT
16430 LPRINT LIND$;DAT$(11);" ";DAT$(12)                      'Postleitzahl Ort
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
16570 PRINT STRICH$
16580 PRINT
16590 PRINT TAB(31);"Mitgliedsnummer";MTGLZAHL
16600 PRINT
16610 RESTORE 16670
16620 FOR K=1 TO 7:READ A,B
16630   IF A<>0 THEN PRINT TAB(XCOL1);FENAM$(A);DAT$(A);
16640   IF B<>0 THEN PRINT TAB(XCOL2);FENAM$(B);DAT$(B);
16650   PRINT
16660 NEXT K
16670 DATA 1,0,2,15,3,16,4,0,5,17,6,18,0,19
16680 FOR K=7 TO 14
16690   PRINT TAB(XCOL1);FENAM$(K);DAT$(K);
16700   PRINT TAB(XCOL2);FENAM$(K+13);DAT$(K+13);
16710 NEXT K
16720 PRINT
16730 PRINT
16740 PRINT
16750 PRINT STRICH$
16760 RETURN
16770 '
16780 ' *** Mitgliedsvermerke drucken
16790 '
16800 ' IN: NAMM$ =
16810 RETURN                                         '*** TMP Print Disable ***
16820 LIND$=SPACE$(9)
16830 SLOT1$="___"
16840 SLOT2$="___ / ___"
16850 SLOT3$="___ / ___ / ___"
16860 '
16870 LPRINT TAB(10);"Mitgliedsnr.: ";SLOT1$;
16880 LPRINT TAB(28);"Name: ";NAMM$;
16890 LPRINT TAB(65);"Datum: ";SLOT3$
16900 LPRINT
16910 LPRINT LIND$;"26*************************** ";SLOT3$
16920 LRPINT
16930 LPRINT LIND$;"27*************************** ";SLOT3$
16940 LRPINT
16950 LPRINT LIND$;"28*************************** ";SLOT3$
16960 LRPINT
16970 LPRINT LIND$;"29*************************** ";SLOT3$
16980 LRPINT
16990 LPRINT LIND$;"30*************************** ";SLOT2$
17000 LRPINT
17010 LPRINT LIND$;"31*************************** ";SLOT3$
17020 LRPINT
17030 LPRINT LIND$;"32*************************** ";SLOT3$
17040 LRPINT
17050 LPRINT LIND$;"33*************************** ";SLOT1$
17060 LRPINT
17070 LPRINT LIND$;"34*************************** ";SLOT3$
17080 LRPINT
17090 LPRINT LIND$;"35*************************** ";SLOT3$
17100 LRPINT
17110 LPRINT LIND$;"36*************************** ";SLOT3$
17120 LRPINT
17130 LPRINT LIND$;"37*************************** ";SLOT3$
17140 LRPINT
17150 LPRINT LIND$;"38*************************** ";SLOT3$
17160 RETURN
17170 '
17180 ' ### Hilfe-Texte #######################################################
17190 '
17200 ' *** Hilfe-Text "Mitgliederverwaltung" G-010000
17210 '
17220 MT$="HILFE":GOSUB 13130                                      'Basis-Maske
17230 PRINT HOME$
17240 PRINT IN$;"Die MITGLIEDERVERWALTUNG bietet folgende Funktionen an :"
17250 PRINT 
17260 PRINT IN$;"1 - STAMMSATZPFLEGE"
17270 PRINT IN$;"    Bearbeiten von Mitgliedsdaten gemäß dem Personalstamm-"
17280 PRINT IN$;"    blatt" 
17290 PRINT
17300 PRINT IN$;"2 - VERWENDUNGSNACHWEIS"
17310 PRINT IN$;"    Bearbeiten von Mitgliedsdaten, die Ämter umd Amtsperioden"
17320 PRINT IN$;"    eines Mitglieds enthalten"
17330 PRINT
17340 PRINT IN$;"3 - MITGLIEDSLISTE DRUCKEN"
17350 PRINT IN$;"    Drucken von Mitgliedslisten nach verschiedenen Gesichts-"
17360 PRINT IN$;"    punkten"
17370 PRINT
17380 PRINT IN$;"Wählen Sie aus, indem Sie die zugehörige Zahlentaste drücken."
17390 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
17400 RETURN
17410 '
17420 ' *** Hilfe-Text "Stammsatzpflege" G-011000
17430 '
17440 MT$="HILFE":GOSUB 13130                                      'Basis-Maske
17450 PRINT HOME$
17460 PRINT IN$;"Die STAMMSATZPFLEGE bietet folgende Funktionen an:"
17470 PRINT
17480 PRINT IN$;"1 - STAMMSATZ ANLEGEN"
17490 PRINT IN$;"    Erfassen der Daten eines neuen Mitglieds"
17500 PRINT
17510 PRINT IN$;"2 - STAMMSATZ ÄNDERN"
17520 PRINT IN$;"    Verändern eines bereits bestehenden Stammsatzes"
17530 PRINT
17540 PRINT IN$;"3 - STAMMSATZ LÖSCHEN"
17550 PRINT IN$;"    Verändern des Mitgliedsstatus auf `ausgetreten`"
17560 PRINT
17570 PRINT IN$;"4 - STAMMSATZ DRUCKEN"
17580 PRINT IN$;"    Drucken eines kompletten Stammsatzes"
17590 PRINT
17600 PRINT IN$;"5 - STAMMSATZ SUCHEN"
17610 PRINT IN$;"    Suchen vom Stammsätzen nach bestimmten Gesichtspunkten"
17620 PRINT
17630 PRINT IN$;"Wählen Sie aus, indem Sie die zugehörige Zahlentaste drücken."
17640 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
17650 RETURN
17660 '
17670 ' *** Hilfe-Text "Stammsatz anlegen" G-011010
17680 '
17690 MT$="HILFE":GOSUB 13130                                      'Basis-Maske
17700 PRINT HOME$
17710 PRINT IN$;"Sie können jetzt Daten eines neuen Mitglieds eingeben, jedoch"
17720 PRINT IN$;"nicht die eines bestehenden Mitglieds ändern."
17730 PRINT
17740 PRINT IN$;"Die Mitgliedsnummer wird laufend vergeben."
17750 PRINT
17760 PRINT IN$;"Während der Dateneingebe können Sie mit der DELETE-Taste"
17770 PRINT IN$;"korrigieren. Ist ein Feld ausgefüllt worden, muß RETURN"
17780 PRINT IN$;"gedrückt werden, da sonst die Eingabe nicht gespeichert wird."
17790 PRINT IN$;"Mit den Cursortasten kann das Feld gewechselt werden."
17800 PRINT
17810 PRINT IN$;"`=>`-Felder müssen ausgefüllt werden."
17820 PRINT
17830 PRINT IN$;"Weitere Funktionstasten:"
17840 PRINT IN$;"<F7>  Diesen Stammsatz speichern"
17850 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
17860 RETURN
17870 '
17880 ' *** Hilfe-Text "Stammsatz ändern (I)" G-011020
17890 '
17900 MT$="HILFE":GOSUB 13130                                      'Basis-Maske
17910 PRINT HOME$
17920 PRINT IN$;"Sie können jetzt einen bestehenden Stammsatz ändern."
17930 PRINT
17940 PRINT IN$;"Geben Sie die Mitgliedsnummer des Stammsatzes ein."
17950 PRINT IN$;"Bei Tippfehlern können Sie mit der DELETE-Taste korrigieren."
17960 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
17970 RETURN
17980 '
17990 ' *** Hilfe-Text "Stammsatz ändern (II)" G-011020
18000 '
18010 SAVEMT$=MT$:MT$="HILFE":GOSUB 13130:MT$=SAVEMT$              'Basis-Maske
18020 PRINT HOME$
18030 PRINT IN$;"Sie können jetzt einen bestehenden Stammsatz ändern."
18040 PRINT
18050 PRINT IN$;"Während der Dateneingabe können Sie mit der DELETE-Taste"
18060 PRINT IN$;"korrigieren. Ist ein Feld geändert worden, muß RETURN"
18070 PRINT IN$;"gedrückt werden, da sonst die Änderung nicht gespeichert wird."
18080 PRINT IN$;"Mit den Cursortasten kann das Feld gewechselt werden."
18090 PRINT
18100 PRINT IN$;"`=>`-Felder müssen ausgefüllt werden."
18110 PRINT
18120 PRINT IN$;"Weitere Funktionstasten:"
18130 PRINT IN$;"<F7>  Diesen Stammsatz speichern"
18140 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
18150 RETURN
18160 '
18170 ' *** Hilfe-Text "Stammsatz löschen" G-011030
18180 '
18190 MT$="HILFE":GOSUB 13130                                      'Basis-Maske
18200 PRINT HOME$
18210 PRINT IN$;"Sie können jetzt den Status eines Mitglieds auf `ausgetreten`"
18220 PRINT IN$;"ändern. Damit wird der Stammsatz beim Druck von Mitglieds-"
18230 PRINT IN$;"listen nicht berüchsichtigt."
18240 PRINT IN$;"Auf keinen Fall werden die Daten WIRKLICH gelöscht."
18250 PRINT
18260 PRINT IN$;"Geben Sie die Mitgliedsnummer des Stammsatzes ein."
18270 PRINT IN$;"Bei Tippfehlern können Sie mit der DELETE-Taste korrigieren."
18280 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
18290 RETURN
18300 ' 
18310 ' *** Hilfe-Text für "Stammsatz drucken" G-011040
18320 '
18330 SAVEMT$=MT$:MT$="HILFE":GOSUB 13130:MT$=SAVEMT$              'Basis-Maske
18340 PRINT HOME$
18350 PRINT IN$;"Sie können jetzt einen kompletten Stammsatz drucken, so, wie"
18360 PRINT IN$;"er auf dem Bildschirm steht."
18370 PRINT
18380 PRINT IN$;"Geben Sie die Mitgliedsnummer des Stammsatzes ein."
18390 PRINT IN$;"Bei Tippfehlern können Sie mit der DELETE-Taste korrigieren."
18400 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
18410 RETURN
18420 '
18430 ' *** Hilfe-Text "Stammsatz suchen (I)" G-011050
18440 '
18450 MT$="HILFE":GOSUB 13130                                      'Basis-Maske
18460 PRINT HOME$
18470 PRINT IN$;"Sie können jetzt Stammsätze suchen."
18480 PRINT
18490 PRINT IN$;"1 - Suchen nach Nummern"
18500 PRINT IN$;"    Geben Sie ein Intervall von Mitgliedsnummern ein."
18510 PRINT IN$;"    Es werden alle Stammsätze ausgegeben, deren Nummern im"
18520 PRINT IN$;"    Intervall liegen."
18530 PRINT
18540 PRINT IN$;"2 - Suchen nach Maske"
18550 PRINT IN$;"    Sie füllen ein Formular aus. Diejenigen Stammsätze, die"
18560 PRINT IN$;"    mit dem Formular übereinstimmen, werden ausgegeben."
18570 PRINT IN$;"    Leere Felder bezeichnen beliebige Stammsatzdaten."
18580 PRINT IN$;"    Ein * zu Beginn des Feldes bedeutet: Das folgende Wort"
18590 PRINT IN$;"    darf sich an beliebiger Stelle des Feldes befinden."
18600 PRINT IN$;"    Ein oder mehrere ? zu Beginn des Feldes stellen Platz-"
18610 PRINT IN$;"    halter für beliebige Zeichen dar."
18620 PRINT
18630 PRINT IN$;"Wählen Sie aus, indem Sie die zugehörige Zahlentaste drücken."
18640 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
18650 RETURN
18660 '
18670 ' *** Hilfe-Text "Stammsatz suchen nach Nummern" G-011051
18680 '
18690 MT$="HILFE":GOSUB 13130                                      'Basis-Maske
18700 PRINT HOME$
18710 PRINT IN$;"Hilfe-Text *Stammsatz suchen nach Nummern* G-011051"
18720 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
18730 RETURN
18740 '
18750 ' *** Hilfe-Text "Stammsatz suchen nach Maske" G-011052
18760 '
18770 MT$="HILFE":GOSUB 13130                                      'Basis-Maske
18780 PRINT HOME$
18790 PRINT IN$;"Hilfe-Text *Stammsatz suchen nach Maske* G-011052"
18800 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
18810 RETURN
18820 '
18830 ' *** Hilfe-Text "Stammsatz suchen (II)"  G-011050
18840 '
18850 MT$="HILFE":GOSUB 13130                                      'Basis-Maske
18860 PRINT HOME$
18870 PRINT IN$;"Hilfe-Text *Stammsatz suchen (II)* G-011050 Keine Suchtreffer"
18880 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
18890 RETURN
18900 '
18910 ' *** Hilfe-Text "Stammsatz suchen (III)" G-011050
18920 '
18930 MT$="HILFE":GOSUB 13130                                      'Basis-Maske
18940 PRINT HOME$
18950 PRINT IN$;"Hilfe-Text *Stammsatz suchen (III)* G-011050 Blättern"
18960 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
18970 RETURN
18980 '
18990 ' *** Hilfe-Text "Verwendungsnachweis (I)" G-012000
19000 '
19010 MT$="HILFE":GOSUB 13130                                      'Basis-Maske
19020 PRINT HOME$
19030 PRINT IN$;"Sie können jetzt einen Verwendungsnachweis ändern."
19040 PRINT
19050 PRINT IN$;"Geben Sie die Mitgliedsnummer des Verwendungsnachweises ein."
19060 PRINT IN$;"Bei Tippfehlern können Sie mit der DELETE-Taste korrigieren."
19070 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
19080 RETURN
19090 '
19100 ' *** Hilfe-Text "Verwendungsnachweis (II)" G-012000
19110 '
19120 SAVEMT$=MT$:MT$="HILFE":GOSUB 13130:MT$=SAVEMT$              'Basis-Maske
19130 PRINT HOME$
19140 PRINT IN$;"Sie können jetzt einen bestehenden Verwendungsnachweis ändern."
19150 PRINT
19160 PRINT IN$;"Während der Dateneingabe können Sie mit der DELETE-Taste"
19170 PRINT IN$;"korrigieren. Ist ein Feld ausgefüllt worden, muß RETURN"
19180 PRINT IN$;"gedrückt werden, da sonst die Änderung nicht gespeichert"
19190 PRINT IN$;"wird. Mit den Cursortasten kann das Feld gewechselt werden."
19200 PRINT
19210 PRINT IN$;"Weitere Funktionstasten:"
19220 PRINT IN$;"<F5>  Zurückblättern (ohne die jetzige Seite zu speichern)"
19230 PRINT IN$;"<F6>  Vorblättern (ohne die jetzige Seite zu speichern)"
19240 PRINT IN$;"<F7>  Aktuelle Seite speichern"
19250 PRINT IN$;"<F9>  Aktuelle Seite drucken"
19260 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
19270 RETURN
19280 '
19290 ' *** Hilfe-Text "Mitgliedsliste drucken" G-013000
19300 '
19310 MT$="HILFE":GOSUB 13130                                      'Basis-Maske
19320 PRINT HOME$
19330 PRINT IN$;"Sie können jetzt eine Mitgliedsliste drucken." 
19340 PRINT
19350 PRINT IN$;"1 - Suchen & Drucken"
19360 PRINT IN$;"    Sie können Stammsätze nach verschiedenen Gesichtspunkten"
19370 PRINT IN$;"    suchen und drucken."
19380 PRINT
19390 PRINT IN$;"2 - Komplett drucken"
19400 PRINT IN$;"    Es wird eine komplette Mitgliedsliste ausgedruckt."
19410 PRINT IN$;"    Ausgetretene Mitglieder werden nicht berücksichtigt."
19420 PRINT
19430 PRINT IN$;"3 - Datenblatt drucken"
19440 PRINT IN$;"    Der Stammsatz und der Verwendungsnachweis werden im"
19450 PRINT IN$;"    Bildschirmformat mit einer Liste von Mitgliedsvermerken"
19460 PRINT IN$;"    ausgedruckt."
19470 PRINT
19480 PRINT IN$;"Wählen Sie aus, indem Sie die zugehörige Zahlentaste drücken."
19490 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
19500 RETURN
19510 '
19520 ' *** Hilfe-Text "Suchen & Drucken (I)" G-013010
19530 '
19540 MT$="HILFE":GOSUB 13130                                      'Basis-Maske
19550 PRINT HOME$
19560 PRINT IN$;"Hilfe-Text *Suchen & Drucken (I)* G-013010" 
19570 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
19580 RETURN
19590 '
19600 ' *** Hilfe-Text "Suchen & Drucken nach Nummern" G-013011
19610 '
19620 MT$="HILFE":GOSUB 13130                                      'Basis-Maske
19630 PRINT HOME$
19640 PRINT IN$;"Hilfe-Text *Suchen & Drucken nach Nummern* G-013011"
19650 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
19660 RETURN
19670 '
19680 ' *** Hilfe-Text "Suchen & Drucken nach Maske" G-013012
19690 '
19700 MT$="HILFE":GOSUB 13130                                      'Basis-Maske
19710 PRINT HOME$
19720 PRINT IN$;"Hilfe-Text *Suchen & Drucken nach Maske* G-013012"
19730 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
19740 RETURN
19750 '
19760 ' *** Hilfe-Text "Suchen & Drucken (II)" G-013010
19770 '
19780 MT$="HILFE":GOSUB 13130                                      'Basis-Maske
19790 PRINT HOME$
19800 PRINT IN$;"Hilfe-Text *Suchen & Drucken (II)* G-013010 Keine Suchtreffer"
19810 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
19820 RETURN
19830 '
19840 ' *** Hilfe-Text "Suchen & Drucken (III)" G-013010
19850 '
19860 MT$="HILFE":GOSUB 13130                                      'Basis-Maske
19870 PRINT HOME$
19880 PRINT IN$;"Hilfe-Text *Suchen & Drucken (III)* G-013010 Blättern"
19890 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
19900 RETURN
19910 '
19920 ' *** Hilfe-Text "Suchen & Drucken (IB), Komplettdruck Datum" G-013010, G-013020
19930 '
19940 SAVEMT$=MT$:MT$="HILFE":GOSUB 13130:MT$=SAVEMT$              'Basis-Maske
19950 PRINT HOME$
19960 PRINT IN$;"Hilfe-Text *Suchen & Drucken (IB)* Komplettdruck Datum G-013010, G-013020"
19970 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
19980 RETURN
19990 '
20000 ' *** Hilfe-Text "Datenblatt drucken" G-013030
20010 '
20020 MT$="HILFE":GOSUB 13130                                      'Basis-Maske
20030 PRINT HOME$
20040 PRINT IN$;"Hilfe-Text *Datenblatt drucken* G-013030
20050 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
20060 RETURN
20070 '
20080 ' *** Programmfehler ******************************************* REVIEWED
20090 '
20100 I$=SPACE$(15)
20110 GOSUB 13130                                                  'Basis-Maske
20120 PRINT HOME$
20130 PRINT I$;"Ein Programmfehler ist aufgetreten."
20140 PRINT   
20150 PRINT I$;"- Notieren Sie folgende Fehlerinformation:"
20160 PRINT I$;"    ";HIGHON$;"ERL";HIGHOFF$;" (Error Line)";HIGHON$;ERL;HIGHOFF$
20170 PRINT I$;"    ";HIGHON$;"ERR";HIGHOFF$;" (Error Code)";HIGHON$;ERR;HIGHOFF$
20180 PRINT I$;"- Drücken Sie am Rechner den Knopf hinten rechts."
20190 PRINT I$;"19**************************"
20200 '
20210 ' *** Programmende ********************************************* REVIEWED
20220 '
20230 PRINT FNCUR$(22,1);HIGHOFF$;CURON$;
20240 RESET
20250 'LW RUN "DISKMENU.EXE"
20260 END                                                           '1799 lines
