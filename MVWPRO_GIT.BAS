1000 ' RELEASE 01***** Neubearbeitung durch 04**********
1010 '
1020 ' LW 31-JAN-2026: Load MSBASIC.COM with /S:560 /F:6
1030 '
1120 ' TIP: Do not let the cursor slip into line 25 -> display will scroll up
1130 ' TIP: Use semicolons with PRINT carefully -> display driver bug
1140 '
1150 ' *** Initialisierung, Dimensionierung ***********************************
1160 '
1170 DEFINT A-Z
1180 '
1190 TRUE=-1
1200 FALSE=0
1210 '
1220 FLDMAX=27                                          'Anzahl Stammsatzfelder
1230 DIM FLDNAM$(FLDMAX)                                       'Name des Feldes
1240 DIM FLDISMAND(FLDMAX)                             'Muss-Feld (TRUE, FALSE)
1250 DIM FLDROW(FLDMAX)                           'Zeile des Feldnamens (1..25)
1260 DIM FLDCOL(FLDMAX)                          'Spalte des Feldnamens (1..80)
1270 DIM FLDLNG(FLDMAX)                                       'Länge des Feldes
1280 DIM FLDTYP(FLDMAX)                                         'Typ des Feldes
1290 '
1300 DIM FLDDAT$(FLDMAX)                                    'Stammsatzfelddaten
1310 DIM FLDSEARCH$(FLDMAX)                                 'Stammsatzsuchdaten
1320 '
1330 SEARCHNUMSMAX=2
1340 DIM SEARCHNUMS$(SEARCHNUMSMAX)
1350 DIM SEARCHNUMSROW(SEARCHNUMSMAX)
1360 SEARCHNUMSROW(1)=4
1370 SEARCHNUMSROW(2)=6
1380 '
1390 BYNUMS=1                                             'Werte von SEARCHMODE
1400 BYMASK=2
1410 '
1600 CKEYUP=1                                    'Werte von CKEY (Cursortasten)
1610 CKEYDN=2
1620 CKEYLT=3
1630 CKEYRT=4
1640 '
1650 ASCBSP=8    'Werte von ASCC (ASCII-Code des ersten eingegebenen Feldwerts)
1660 ASCRET=13
1670 ASCESC=27
1680 ASC0=48
1690 ASC1=49
1700 ASCCUP=200
1710 ASCCDN=201
1720 ASCCLT=202
1730 ASCCRT=203
1740 ASCF1=241
1750 ASCF4=244
1760 ASCF9=249
1770 ASCF9=249
1780 '
1790 HELPIDMAX=23                                     'Anzahl Hilfe-Text-Seiten
1800 DIM HELPFIRSTROW(HELPIDMAX)
1810 DIM HELPLASTROW(HELPIDMAX)
1820 '
1821 SAVMBRNUMEDIT$=""
1822 SAVMBRNUMDELETE$=""
1823 SAVMBRNUMPRINT$=""
1829 ' 
1830 G0100000=1                                           'Mitgliederverwaltung
1840 G0110000=2                                                'Stammsatzpflege
1850 G0110100=3                                              'Stammsatz anlegen
1860 G0110201=4                                           'Stammsatz ändern (1)
1870 G0110202=5                                           'Stammsatz ändern (2)
1880 G0110300=6                                              'Stammsatz löschen
1890 G0110400=7                                              'Stammsatz drucken
1900 G0110501=8                                           'Stammsatz suchen (1)
1910 G0110510=9                                  'Stammsatz suchen nach Nummern
1920 G0110520=10                                   'Stammsatz suchen nach Maske
1930 G0110502=11                            'Stammsatz suchen (2) Keine Treffer
1940 G0110503=12                                 'Stammsatz suchen (3) Blättern
1950 G0120001=13                                       'Verwendungsnachweis (1)
1960 G0120002=14                                       'Verwendungsnachweis (2)
1970 G0130000=15                                        'Mitgliedsliste drucken
1980 G0130101=16                                    'Suchen & Drucken (1) Datum
1990 G0130102=17                                          'Suchen & Drucken (2)
2000 G0130110=18                                 'Suchen & Drucken nach Nummern
2010 G0130120=19                                   'Suchen & Drucken nach Maske
2020 G0130103=20                            'Suchen & Drucken (2) Keine Treffer
2030 G0130200=21                                           'Komplettdruck Datum
2040 G0130300=22                                            'Datenblatt drucken
2050 '
2060 COLSEP=14           'Offset Colum Feldname to Column Wertefeld (Pünktchen)
2070 '
2071 BSP$    =CHR$(8)                                                'Backspace
2072 DEL$    =BSP$+"."+BSP$                                             'Delete
2073 ESC$    =CHR$(27)                                                  'Escape
2074 ROWSEP$ =STRING$(79,"-")                                           'Strich
2075 '
2076 CLS$    =ESC$+"E"                                      'Bildschirm löschen
2077 INVON$  =ESC$+"p"                                              'Invers ein
2078 INVOFF$ =ESC$+"q"                                              'Invers aus
2079 CURON$  =ESC$+"y5"                                             'Cursor ein
2080 CUROFF$ =ESC$+"x5"                                             'Cursor aus
2081 HION$   =ESC$+"("                                           'Helle Schrift
2082 HIOFF$  =ESC$+")"                                          'Dunkle Schrift
2083 DELLINE$=ESC$+"K"                                           'Zeile löschen
2085 DELHIGH$=ESC$+"+"                                'LW Helle Schrift löschen
2086 '
2087 DEF FNCUR$(R,C)=ESC$+"Y"+CHR$(31+R)+CHR$(31+C)       'Cursor positionieren
2088 HOME$=FNCUR$(3,1)                                    'Cursor Home Position
2089 ' 
2090 ON ERROR GOTO 18110                               'Programmfehler abfangen
2091 '
2100 GOSUB 10050    'Tasten deaktivieren, ASCII-Codes für Cursortasten vergeben
2120 '
2130 ' *** Beginn des Mitgliederverwaltungsprogramms **************************
2140 '
2150 MT$="MITGLIEDERVERWALTUNG":MI$="G-010000":GOSUB 13700         'Basis-Maske
2160 Q$="Bitte Daten-Diskette in Laufwerk B einlegen. Bereit?":GOSUB 12590 'Ja/Nein-Frage
2170 IF NOT ISYES THEN 18240                                      'Programmende
2180 '
2190 N$="Einen Moment bitte...":GOSUB 13490                 'Nachricht anzeigen
2200 GOSUB 11510                                                 'MBRMAX lesen
2210 GOSUB 10970                              'Stammsatzfeld-Definitionen lesen
2220 GOSUB 12100                                       'Stammsatz-Datei fielden
2230 GOSUB 14370                           'Verwendungsnachweis-Dateien fielden
2240 GOSUB 17780              'Hilfe-Text-Dateien fielden, Zeilenbereiche lesen
2250 GOSUB 13630                                             'Nachricht löschen
2260 '
2270 ' ###################### Mitgliederverwaltungs-Menü ######################
2280 '
2290 MT$="MITGLIEDERVERWALTUNG":MI$="G-010000":GOSUB 13700         'Basis-Maske
2300 FKEYS$="":GOSUB 13810                                 'F-Tasten einblenden
2310 PRINT FNCUR$(4,28);"1 - Stammsatzpflege"
2320 PRINT FNCUR$(6,28);"2 - Verwendungsnachweis"
2330 PRINT FNCUR$(8,28);"3 - Mitgliedsliste drucken"
2340 FKEYMAX=4:NKEYMAX=3:GOSUB 13970                              'Menü-Eingabe
2350 IF FKEY THEN ON FKEY GOTO 2370,2380,2290,2380
2360 ON NKEY GOTO 2440,7140,7970
2370 HELPID=G0100000:GOSUB 17940:GOTO 2290                     '<F1> Hilfe-Text
2380 GOSUB 14130:GOTO 2340                      '<F2> <F4> Zum Hauptmenü?-Frage
2390 '
2400 ' ########################### Stammsatzpflege ############################
2410 '
2420 ' *** Stammsatzpflege-Menü ***********************************************
2430 '
2440 MT$="STAMMSATZPFLEGE":MI$="G-011000":GOSUB 13700              'Basis-Maske
2450 FKEYS$="":GOSUB 13810                                 'F-Tasten einblenden
2460 PRINT FNCUR$(4,30);"1 - Stammsatz anlegen"
2470 PRINT FNCUR$(6,30);"2 - Stammsatz ändern"
2480 PRINT FNCUR$(8,30);"3 - Stammsatz löschen"
2490 PRINT FNCUR$(10,30);"4 - Stammsatz drucken"
2500 PRINT FNCUR$(12,30);"5 - Stammsatz suchen"
2510 FKEYMAX=4:NKEYMAX=5:GOSUB 13970                              'Menü-Eingabe
2520 IF FKEY THEN ON FKEY GOTO 2540,2290,2290,2550
2530 ON NKEY GOTO 2590,3080,3740,4160,4520
2540 HELPID=G0110000:GOSUB 17940:GOTO 2440                     '<F1> Hilfe-Text
2550 GOSUB 14130:GOTO 2510                           '<F4> Zum Hauptmenü?-Frage
2560 '
2570 ' *** Stammsatz anlegen von 02************ *******************************
2580 '
2590 MBRNUM=MBRMAX+1
2600 F=1
2610 GOSUB 11400                                              'Feldwerte leeren
2620 GOSUB 13360                                     'Voreinstellungen zuweisen
2630 MT$="STAMMSATZ ANLEGEN":MI$="G-011010":GOSUB 13700            'Basis-Maske
2640 FKEYS$="<F7>=Speichern":GOSUB 13810                   'F-Tasten einblenden
2650 GOSUB 12050                                      'Mitgliedsnummer ausgeben
2660 GOSUB 11790          'Feldnamen und Feldwerte mit Feldplatzhalter ausgeben
2670 R=FLDROW(F):C=FLDCOL(F)+COLSEP:LNG=FLDLNG(F):TYP=FLDTYP(F):ISMAND=FLDISMAND(F)
2680 FKEYMASK$="12347":CKEYMASK$="12":TEXTIN$=FLDDAT$(F)
2690 GOSUB 10370                                                  'Feld-Eingabe
2700 '
2710 IF ASCC<>ASCRET THEN 2750                              'RETURN verarbeiten
2720   FLDDAT$(F)=TEXTOUT$
2730   F=F+1:IF F>FLDMAX THEN F=1
2740   GOTO 2670
2750 IF CKEY=0 THEN 2800                              'Cursortasten verarbeiten
2760   PRINT FNCUR$(R,C);HION$;FLDDAT$(F);HIOFF$;STRING$(LNG-LEN(FLDDAT$(F)),".")
2770   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FLDMAX
2780   IF CKEY=CKEYDN THEN F=F+1:IF F>FLDMAX THEN F=1
2790   GOTO 2670
2800 IF FKEY=1 THEN HELPID=G0110100:GOSUB 17940:GOTO 2630           'Hilfe-Text
2810 IF FKEY=2 THEN GOSUB 2910:GOTO 2440              'Exit "Stammsatz anlegen"
2820 IF FKEY=3 THEN GOSUB 2910:GOTO 2290              'Exit "Stammsatz anlegen"
2830 IF FKEY<>4 THEN 2880
2840   GOSUB 14210                              'Zum Hauptmenü?-Frage (einfach)
2850   IF NOT ISYES THEN GOSUB 13810:GOTO 2690             'F-Tasten einblenden
2860   GOSUB 2910                                     'Exit "Stammsatz anlegen"
2870   GOTO 18240                                                 'Programmende
2880 IF FKEY=7 THEN GOSUB 2980:GOTO 2590            'Exit "Stammsatz anlegen" 2
2890 '
2900 ' *** SUB Exit "Stammsatz anlegen"
2910 GOSUB 10920                                           'Tastatur entprellen
2920 Q$="Wollen Sie vorher den Stammsatz speichern?":GOSUB 12590 'Ja/Nein-Frage
2930 IF NOT ISYES THEN MBRNUM=MBRNUM-1:RETURN
2960 '                                                            
2970 ' *** SUB Exit "Stammsatz anlegen" 2
2980 N$="Stammsatz wird gespeichert...":GOSUB 13570 'Nachricht anzeigen, F-Tasten ausblenden
2990 GOSUB 12380                                           'Stammsatz speichern
3000 GOSUB 14480                       'Erste Verwendungsnachweis-Seite anlegen
3010 MBRMAX=MBRNUM:GOSUB 11600                                 'MBRMAX speichen
3030 GOSUB 13630                                             'Nachricht löschen
3040 RETURN
3050 ' 
3060 ' *** Stammsatz ändern von 03************ ********************************
3070 '
3080 MT$="STAMMSATZ ÄNDERN":MI$="G-011020":GOSUB 13700             'Basis-Maske
3090 FKEYS$="":GOSUB 13810                                 'F-Tasten einblenden
3100 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer" 
3110 PRINT FNCUR$(5,24)"des zu ändernden Stammsatzes ein."
3120 PRINT FNCUR$(6,22);"Gültige Mitgliedsnummern sind 1 -";STR$(MBRMAX);"."
3130 PRINT FNCUR$(7,23)"Beenden Sie die Eingabe mit RETURN."
3140 PRINT FNCUR$(9,38)"=>..."
3150 ISINPUTOK=TRUE
3160 FKEYMASK$="1234":CKEYMASK$=""
3170 R=9:C=40:LNG=3:TYP=2:ISMAND=TRUE
3180 TEXTIN$=SAVMBRNUMEDIT$
3181 GOSUB 10380                                                'Feld-Eingabe+1
3182 SAVMBRNUMEDIT$=TEXTOUT$
3190 IF FKEY=1 THEN HELPID=G0110201:GOSUB 17940:GOTO 3080 'Hilfe-Text 'TODO: Wert geht verloren
3200 IF FKEY=2 THEN 2440
3210 IF FKEY=3 THEN 2290
3220 IF FKEY=4 THEN GOSUB 14130:GOTO 3180                 'Zum Hauptmenü?-Frage
3230 '
3240 M=VAL(TEXTOUT$)                                        'RETURN verarbeiten
3250 IF M>=1 AND M<=MBRMAX THEN 3290
3260   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 13490         'Nachricht anzeigen
3270   ISINPUTOK=FALSE
3280   GOTO 3170
3290 MBRNUM=M
3300 N$="Stammsatz wird gelesen...":GOSUB 13570 'Nachricht anzeigen, F-Tasten ausblenden
3310 GOSUB 12160                                               'Stammsatz lesen
3320 GOSUB 13630                                             'Nachricht löschen
3330 '
3340 GOSUB 13700                                                   'Basis-Maske
3350 FKEYS$="<F7>=Speichern":GOSUB 13810                   'F-Tasten einblenden
3360 GOSUB 12050                                      'Mitgliedsnummer ausgeben
3370 GOSUB 11790          'Feldnamen und Feldwerte mit Feldplatzhalter ausgeben
3380 F=1
3390 R=FLDROW(F):C=FLDCOL(F)+COLSEP:LNG=FLDLNG(F):TYP=FLDTYP(F):ISMAND=FLDISMAND(F)
3400 FKEYMASK$="12347":CKEYMASK$="12":TEXTIN$=FLDDAT$(F)
3410 GOSUB 10370                                                  'Feld-Eingabe
3420 '
3430 IF ASCC<>ASCRET THEN 3470                              'RETURN verarbeiten
3440   FLDDAT$(F)=TEXTOUT$
3450   F=F+1:IF F>FLDMAX THEN F=1
3460   GOTO 3390
3470 IF CKEY=0 THEN 3520                              'Cursortasten verarbeiten
3480   PRINT FNCUR$(R,C);HION$;FLDDAT$(F);HIOFF$;STRING$(LNG-LEN(FLDDAT$(F)),".")
3490   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FLDMAX
3500   IF CKEY=CKEYDN THEN F=F+1:IF F>FLDMAX THEN F=1
3510   GOTO 3390
3520 IF FKEY=1 THEN HELPID=G0110202:GOSUB 17940:GOTO 3340           'Hilfe-Text
3530 IF FKEY=2 THEN GOSUB 3630:GOTO 3080               'Exit "Stammsatz ändern"
3540 IF FKEY=3 THEN GOSUB 3630:GOTO 2290               'Exit "Stammsatz ändern"
3550 IF FKEY<>4 THEN 3600 
3560   GOSUB 14210                              'Zum Hauptmenü?-Frage (einfach)
3570   IF NOT ISYES THEN GOSUB 13810:GOTO 3410             'F-Tasten einblenden
3580   GOSUB 3630                                      'Exit "Stammsatz ändern"
3590   GOTO 18240                                                 'Programmende
3600 IF FKEY=7 THEN GOSUB 3670:GOTO 3080             'Exit "Stammsatz ändern" 2
3610 '
3620 ' *** SUB Exit "Stammsatz ändern"
3630 Q$="Wollen Sie vorher den Stammsatz speichern?":GOSUB 12590 'Ja/Nein-Frage
3640 IF NOT ISYES THEN RETURN
3650 '
3660 ' *** SUB Exit "Stammsatz ändern" 2
3670 N$="Stammsatz wird gespeichert...":GOSUB 13570 'Nachricht anzeigen, F-Tasten ausblenden
3680 GOSUB 12380                                           'Stammsatz speichern
3690 GOSUB 13630                                             'Nachricht löschen
3700 RETURN
3710 '
3720 ' *** Stammsatz löschen von 04********** *********************************
3730 '
3740 MT$="STAMMSATZ LÖSCHEN":MI$="G-011030":GOSUB 13700            'Basis-Maske
3750 FKEYS$="":GOSUB 13810                                 'F-Tasten einblenden
3760 PRINT FNCUR$(4,23);"Bitte geben Sie die Mitgliedsnummer"         'Menütext
3770 PRINT FNCUR$(5,24);"des zu löschenden Mitglieds ein."
3780 PRINT FNCUR$(6,22);"Gültige Mitgliedsnummern sind 1 -";STR$(MBRMAX);"."
3790 PRINT FNCUR$(7,15);"(Dabei wird nur der Status des Mitglieds verändert.)"
3800 PRINT FNCUR$(8,23);"Beenden Sie die Eingabe mit RETURN."
3810 PRINT FNCUR$(10,38);"=>..."
3820 ISINPUTOK=TRUE
3830 FKEYMASK$="1234":CKEYMASK$=""
3840 R=10:C=40:LNG=3:TYP=2:ISMAND=TRUE
3850 TEXTIN$=SAVMBRNUMDELETE$
3851 GOSUB 10380                                                'Feld-Eingabe+1
3852 SAVMBRNUMDELETE$=TEXTOUT$
3860 IF FKEY=1 THEN HELPID=G0110300:GOSUB 17940:GOTO 3740 'Hilfe-Text 'TODO: Wert geht verloren
3870 IF FKEY=2 THEN 2440
3880 IF FKEY=3 THEN 2290
3890 IF FKEY=4 THEN GOSUB 14130:GOTO 3850                 'Zum Hauptmenü?-Frage
3900 '
3910 M=VAL(TEXTOUT$)                                        'RETURN verarbeiten
3920 IF M>=1 AND M<=MBRMAX THEN 3960
3930   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 13490         'Nachricht anzeigen
3940   ISINPUTOK=FALSE
3950   GOTO 3840
3960 MBRNUM=M
3970 N$="Stammsatz wird gelesen...":GOSUB 13570 'Nachricht anzeigen, F-Tasten ausblenden
3980 GOSUB 12160                                               'Stammsatz lesen
3990 '
4000 GOSUB 13700                                                   'Basis-Maske
4010 GOSUB 12050                                      'Mitgliedsnummer ausgeben
4020 GOSUB 11790          'Feldnamen und Feldwerte mit Feldplatzhalter ausgeben
4030 Q$="Mitglied endgültig löschen?":GOSUB 12590                'Ja/Nein-Frage
4040 IF NOT ISYES THEN 4120
4050   F=5
4060   FLDDAT$(F)="ausgetreten"
4070   R=FLDROW(F):C=FLDCOL(F)+COLSEP:LNG=FLDLNG(F)
4080   PRINT FNCUR$(R,C);HION$;FLDDAT$(F);HIOFF$;STRING$(LNG-LEN(FLDDAT$(F)),".")               
4090   N$="Stammsatz wird gespeichert...":GOSUB 13570 'Nachricht anzeigen, F-Tasten ausblenden
4100   GOSUB 12380                                         'Stammsatz speichern
4110   GOSUB 13630                                           'Nachricht löschen
4120 GOTO 3740
4130 '
4140 ' *** Stammsatz drucken von 02************ & 04********** ****************
4150 '
4160 MT$="STAMMSATZ DRUCKEN":MI$="G-011040":GOSUB 13700            'Basis-Maske
4170 FKEYS$="":GOSUB 13810                                 'F-Tasten einblenden
4180 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer"
4190 PRINT FNCUR$(5,23)"des auszudruckenden Stammsatzes ein."
4200 PRINT FNCUR$(6,22);"Gültige Mitgliedsnummern sind 1 -";STR$(MBRMAX);"."
4210 PRINT FNCUR$(7,23)"Beenden Sie die Eingabe mit RETURN."
4220 PRINT FNCUR$(9,38);"=>..."
4230 ISINPUTOK=TRUE
4240 FKEYMASK$="1234":CKEYMASK$=""
4250 R=9:C=40:LNG=3:TYP=2:ISMAND=TRUE
4260 TEXTIN$=SAVMBRNUMPRINT$
4261 GOSUB 10380                                                'Feld-Eingabe+1
4262 SAVMBRNUMPRINT$=TEXTOUT$
4270 IF FKEY=1 THEN HELPID=G0110400:GOSUB 17940:GOTO 4160 'Hilfe-Text 'TODO: Wert geht verloren
4280 IF FKEY=2 THEN 2440
4290 IF FKEY=3 THEN 2290
4300 IF FKEY=4 THEN GOSUB 14130:GOTO 4260                 'Zum Hauptmenü?-Frage
4310 '
4320 M=VAL(TEXTOUT$)                                        'RETURN verarbeiten
4330 IF M>=1 AND M<=MBRMAX THEN 4370
4340   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 13490         'Nachricht anzeigen
4350   ISINPUTOK=FALSE
4360   GOTO 4250
4370 MBRNUM=M
4380 N$="Stammsatz wird gelesen...":GOSUB 13570 'Nachricht anzeigen, F-Tasten ausblenden
4390 GOSUB 12160                                               'Stammsatz lesen
4400 GOSUB 13630                                             'Nachricht löschen
4410 '
4420 GOSUB 13700                                                   'Basis-Maske
4430 GOSUB 12050                                      'Mitgliedsnummer ausgeben
4440 GOSUB 11790          'Feldnamen und Feldwerte mit Feldplatzhalter ausgeben
4450 GOSUB 16420                                         'Drucker bereit?-Frage
4460 IF ISYES THEN GOSUB 17110            'Stammsatz drucken (Bildschirmformat)
4480 GOTO 4160
4490 '
4500 ' *** Stammsatz suchen (und drucken) von 05*************** ***************
4510 '
4520 ISPRINT=FALSE
4530 MT$="STAMMSATZ SUCHEN":IF ISPRINT THEN MT$="SUCHEN & DRUCKEN" 'TODO Einsprung Suchen & Drucken
4540 MI$="G-011050":IF ISPRINT THEN MI$="G-013010"
4550 GOSUB 13700                                                   'Basis-Maske
4560 FKEYS$="":GOSUB 13810                                 'F-Tasten einblenden
4570 IF ISPRINT THEN 4610 
4580 PRINT FNCUR$(4,29);"1 - Suchen nach Nummern"
4590 PRINT FNCUR$(6,29);"2 - Suchen nach Maske"
4600 GOTO 4630
4610 PRINT FNCUR$(4,24);"1 - Suchen & drucken nach Nummern"
4620 PRINT FNCUR$(6,24);"2 - Suchen & drucken nach Maske"
4630 NKEYMAX=2:FKEYMAX=4:GOSUB 13970                              'Menü-Eingabe
4640 IF NKEY THEN ON NKEY GOTO 4740,5370
4650 IF FKEY<>1 THEN 4690 
4660   HELPID=G0110501:IF ISPRINT THEN HELPID=G0130102
4670   GOSUB 17940                                                  'Hilfe-Text
4680   GOTO 4530
4690 IF FKEY=2 THEN IF ISPRINT THEN 8100 ELSE 2440    'Absprung F2 Suchen & Drucken oder Suchen
4700 IF FKEY=3 THEN 2290
4710 IF FKEY=4 THEN GOSUB 14130:GOTO 4630                 'Zum Hauptmenü?-Frage
4720 '
4730 ' *** Suchen nach Nummern
4740 TMP$=STR$(1):SEARCHNUMS$(1)=MID$(TMP$,2,99) 'Einsprung nach Ende Suchen & Drucken
4750 TMP$=STR$(MBRMAX):SEARCHNUMS$(2)=MID$(TMP$,2,99)
4760 MT$="STAMMSATZ SUCHEN NACH NUMMERN":MI$="G-011051"
4770 IF ISPRINT THEN MT$="SUCHEN & DRUCKEN NACH NUMMERN":MI$="G-013011"
4780 GOSUB 13700                                                   'Basis-Maske
4790 FKEYS$="<F5>=Suchen":IF ISPRINT THEN FKEYS$="<F5>=Suchen & Drucken"
4800 GOSUB 13810                                           'F-Tasten einblenden
4810 IF ISPRINT THEN 4870
4820 PRINT FNCUR$(4,12);"Ab welcher Mitgliedsnummer soll gesucht werden?     =>..."
4830 PRINT FNCUR$(6,12);"Bis zu welcher Mitgliedsnummer soll gesucht werden? =>..."
4840 PRINT FNCUR$(4,66);HION$;SEARCHNUMS$(1);HIOFF$
4850 PRINT FNCUR$(6,66);HION$;SEARCHNUMS$(2);HIOFF$
4860 GOTO 4910
4870 PRINT FNCUR$(4,6);"Ab welcher Mitgliedsnummer soll gesucht und gedruckt werden?     =>..."
4880 PRINT FNCUR$(6,6);"Bis zu welcher Mitgliedsnummer soll gesucht und gedruckt werden? =>..."
4890 PRINT FNCUR$(4,73);HION$;SEARCHNUMS$(1);HIOFF$
4900 PRINT FNCUR$(6,73);HION$;SEARCHNUMS$(2);HIOFF$
4910 PRINT FNCUR$(8,22);"Gültige Mitgliedsnummern sind 1 -";STR$(MBRMAX);"."
4920 F=1
4930 ISINPUTOK=TRUE
4940 FKEYMASK$="12345":CKEYMASK$="":TEXTIN$=SEARCHNUMS$(F)
4950 R=SEARCHNUMSROW(F):LNG=3:TYP=2:ISMAND=TRUE
4960 C=66:IF ISPRINT THEN C=73
4970 GOSUB 10380                                                'Feld-Eingabe+1
4980 IF FKEY<>1 THEN 5020                                 'F-Tasten verarbeiten
4990   HELPID=G0110510:IF ISPRINT THEN HELPID=G0130110
5000   GOSUB 17940                                                  'Hilfe-Text
5010   GOTO 4760                                    'TODO: Werte gehen verloren
5020 IF FKEY=2 THEN 4530
5030 IF FKEY=3 THEN 2290
5040 IF FKEY=4 THEN GOSUB 14130:GOTO 4970                 'Zum Hauptmenü?-Frage
5050 IF FKEY<>5 THEN 5110
5060   SEARCHMODE=BYNUMS
5070   NUMSUCH1=VAL(SEARCHNUMS$(1))
5080   NUMSUCH2=VAL(SEARCHNUMS$(2))
5090   IF ISPRINT THEN 8410 ELSE 5690    'Absprung Suchen & Drucken oder Suchen
5100 '
5110 M=VAL(TEXTOUT$)                                        'RETURN verarbeiten
5120 IF M>=1 AND M<=MBRMAX THEN 5160
5130   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 13490         'Nachricht anzeigen
5140   GOSUB 5300                                     'Eingabefehler-Behandlung
5150   GOTO 4950
5160 SEARCHNUMS$(F)=TEXTOUT$
5170 NUMSUCH1=VAL(SEARCHNUMS$(1))
5180 NUMSUCH2=VAL(SEARCHNUMS$(2))
5190 IF NUMSUCH1<=NUMSUCH2 THEN 5230
5200   N$="Ungültiger Bereich":GOSUB 13490                  'Nachricht anzeigen
5210   GOSUB 5300                                     'Eingabefehler-Behandlung
5220   GOTO 4950
5230 F=F+1:IF F>SEARCHNUMSMAX THEN F=1
5240 GOSUB 14070                                           'F-Tasten ausblenden
5250 FKEYS$="<F5>=Suchen":IF ISPRINT THEN FKEYS$="<F5>=Suchen & Drucken" 
5260 GOSUB 13810                                           'F-Tasten einblenden
5270 GOTO 4940
5280 '
5290 ' *** SUB Eingabefehler-Behandlung
5300 ISINPUTOK=FALSE
5310 FKEYMASK$="1234"
5320 GOSUB 14070                                           'F-Tasten ausblenden
5330 FKEYS$="":GOSUB 13810                                 'F-Tasten einblenden
5340 RETURN
5350 '
5360 ' *** Suchen nach Maske
5370 GOSUB 11450                                           'FLDSEARCH$() leeren
5380 MT$="STAMMSATZ SUCHEN NACH MASKE":MI$="G-011052" 'Einsprung nach Ende Suchen & Drucken
5390 IF ISPRINT THEN MT$="SUCHEN & DRUCKEN NACH MASKE":MI$="G-013012"
5400 GOSUB 13700                                                   'Basis-Maske
5410 FKEYS$="<F5>=Suchen":IF ISPRINT THEN FKEYS$="<F5>=Suchen & Drucken" 
5420 GOSUB 13810                                           'F-Tasten einblenden
5430 GOSUB 11920      'Feldnamen und Suchfeldwerte und Feldplatzhalter ausgeben
5440 F=1
5450 R=FLDROW(F):C=FLDCOL(F)+COLSEP:LNG=FLDLNG(F):TYP=FLDTYP(F):ISMAND=FLDISMAND(F)
5460 FKEYMASK$="12345":CKEYMASK$="12":TEXTIN$=FLDSEARCH$(F)
5470 GOSUB 10370                                                  'Feld-Eingabe
5480 IF FKEY<>1 THEN 5520                                 'F-Tasten verarbeiten
5490   HELPID=G0110520:IF ISPRINT THEN HELPID=G0130120
5500   GOSUB 17940                                                  'Hilfe-Text
5510   GOTO 5380
5520 IF FKEY=2 THEN 4530
5530 IF FKEY=3 THEN 2290
5540 IF FKEY=4 THEN GOSUB 14130:GOTO 5470                 'Zum Hauptmenü?-Frage
5550 IF FKEY<>5 THEN 5580 
5560   SEARCHMODE=BYMASK                                                  '<F5>
5570   IF ISPRINT THEN 8410 ELSE 5690             'Suchen oder Suchen & Drucken
5580 IF CKEY=0 THEN 5640                              'Cursortasten verarbeiten
5590   PRINT FNCUR$(R,C);HION$;FLDSEARCH$(F);HIOFF$;
5600   PRINT STRING$(LNG-LEN(FLDSEARCH$(F)),".")
5610   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FLDMAX
5620   IF CKEY=CKEYDN THEN F=F+1:IF F>FLDMAX THEN F=1
5630   GOTO 5450
5640 FLDSEARCH$(F)=TEXTOUT$                                 'RETURN verarbeiten
5650 F=F+1:IF F>FLDMAX THEN F=1
5660 GOTO 5450
5670 '
5680 ' *** Suche
5690 IF SEARCHMODE=BYMASK THEN 5760
5700   GOSUB 13700                                                 'Basis-Maske
5710   GOSUB 11400                                             'Leere FLDDAT$()
5720   GOSUB 11790        'Feldnamen und Feldwerte mit Feldplatzhalter ausgeben
5730   MBRNUM=NUMSUCH1                                'Init Suche nach Nummern
5740   GOTO 5920
5750 '
5760 GOSUB 14070                                           'F-Tasten ausblenden
5770 MBRNUM=0                                           'Init Suche nach Maske
5780 GOSUB 6780                                        'Nächsten Treffer suchen
5790 IF MBRNUM<>-1 THEN 5920
5800   N$="Kein Suchtreffer. Mit <F2> neue Suche.":GOSUB 13490       'Nachricht anzeigen
5810   FKEYS$="":GOSUB 13810                               'F-Tasten einblenden
5820   FKEYMASK$="1234":NKEYMASK$="":GOSUB 13970                  'Menü-Eingabe
5830   ON FKEY GOTO 5840,5380,2290,5900
5840   HELPID=G0110502:IF ISPRINT THEN HELPID=G0130103          'F1 verarbeiten
5850   GOSUB 17940                                                  'Hilfe-Text
5860   GOSUB 13700                                                 'Basis-Maske
5870   GOSUB 11920    'Feldnamen und Suchfeldwerte mit Feldplatzhalter ausgeben
5880   GOTO 5800
5890 '
5900   GOSUB 14130:GOTO 5800              'F4 verarbeiten 'Zum Hauptmenü?-Frage
5910 '
5920 GOSUB 6540                                 'Suchtreffer lesen und anzeigen
5930 '
5940 FKEYS$="<F5>=Suchen rückwärts <F6>=Suchen vorwärts <F7>=Adressaufkleber" 
5950 GOSUB 13810                                           'F-Tasten einblenden
5960 NKEYMAX=0:FKEYMAX=7:GOSUB 13970                              'Menü-Eingabe
5970 ON FKEY GOTO 6000,6070,2290,6100,6140,6340,6610
5980 '
5990 ' *** <F1> Hilfe
6000 HELPID=G0110503:GOSUB 17940                                    'Hilfe-Text
6010 GOSUB 13700                                                   'Basis-Maske
6020 GOSUB 12050                                      'Mitgliedsnummer ausgeben
6030 GOSUB 11790          'Feldnamen und Feldwerte mit Feldplatzhalter ausgeben
6040 GOTO 5940
6050 '
6060 ' *** <F2> Zurück
6070 IF SEARCHMODE=BYNUMS THEN 4750 ELSE 5380       'TODO: Werte gehen verloren
6080 '
6090 ' *** <F4> Hauptmenü
6100 GOSUB 14130                                          'Zum Hauptmenü?-Frage
6110 GOTO 5960
6120 '
6130 ' *** <F5> Suchen rückwärts
6140 GOSUB 14070                                           'F-Tasten ausblenden
6150 IF SEARCHMODE=BYMASK THEN 6220
6160 '
6170 MBRNUM=MBRNUM-1                            'Suchen rückwärts nach Nummer
6180 IF MBRNUM>=NUMSUCH1 THEN 6300 
6190 MBRNUM=NUMSUCH1
6200 GOTO 6270
6210 '       
6220 TMP=MBRNUM                                   'Suchen rückwärts nach Maske
6230 GOSUB 6700                                      'Vorherigen Treffer suchen
6240 IF MBRNUM<>-1 THEN 6300
6250 MBRNUM=TMP                                       'Keinen Treffer gefunden
6260 '
6270 N$="Suchbeginn erreicht":GOSUB 13490       'Suchbeginn erreicht 'Nachricht anzeigen
6280 GOTO 5940
6290 '
6300 GOSUB 6540                                 'Suchtreffer lesen und anzeigen
6310 GOTO 5940
6320 '
6330 ' *** <F6> Suchen vorwärts
6340 GOSUB 14070                                           'F-Tasten ausblenden
6350 IF SEARCHMODE=BYMASK THEN 6420
6360 '
6370 MBRNUM=MBRNUM+1                             'Suchen vorwärts nach Nummer
6380 IF MBRNUM<=NUMSUCH2 THEN 6500
6390 MBRNUM=NUMSUCH2
6400 GOTO 6470
6410 '
6420 TMP=MBRNUM                                     'Suche vorwärts nach Maske
6430 GOSUB 6780                                        'Nächsten Treffer suchen
6440 IF MBRNUM<>-1 THEN 6500
6450 MBRNUM=TMP                                       'Keinen Treffer gefunden
6460 '
6470 N$="Suchende erreicht":GOSUB 13490           'Suchende erreicht 'Nachricht anzeigen
6480 GOTO 5940
6490 '
6500 GOSUB 6540                                 'Suchtreffer lesen und anzeigen
6510 GOTO 5940
6520 '
6530 ' *** SUB Suchtreffer lesen und anzeigen
6540 GOSUB 13630                                             'Nachricht löschen
6550 GOSUB 12160                                               'Stammsatz lesen
6560 GOSUB 12050                                      'Mitgliedsnummer ausgeben
6570 GOSUB 11680                        'Feldwerte mit Feldplatzhalter ausgeben
6580 RETURN
6590 '
6600 ' *** <F7> Adressaufkleber drucken
6610 GOSUB 16420                                         'Drucker bereit?-Frage
6620 IF NOT ISYES THEN 6660
6630   N$="Adressaufkleber wird gedruckt...":GOSUB 13490    'Nachricht anzeigen
6640   GOSUB 16930                                     'Adressaufkleber drucken
6650   GOSUB 13630                                           'Nachricht löschen
6660 GOTO 5940                                                    'Neue Eingabe
6670 '
6680 ' *** SUB Vorherigen Treffer suchen
6690 ' IN: MBRNUM OUT: MBRNUM oder -1 wenn kein Treffer
6700 MBRNUM=MBRNUM-1
6710 IF MBRNUM<1 THEN 6840
6720 GOSUB 6880                                                       'Treffer?
6730 IF NOT ISMATCH THEN 6700 
6740 RETURN
6750 '
6760 ' *** SUB Nächsten Treffer suchen
6770 ' IN: MBRNUM OUT: MBRNUM oder -1 wenn kein Treffer
6780 MBRNUM=MBRNUM+1
6790 IF MBRNUM>MBRMAX THEN 6840
6800 GOSUB 6880                                                       'Treffer?
6810 IF NOT ISMATCH THEN 6780 
6820 RETURN
6830 '
6840 MBRNUM=-1
6850 RETURN
6860 '
6870 ' *** SUB Treffer?
6880 N$="Vergleiche Stammsatz"+STR$(MBRNUM)+"...":GOSUB 13490      'Nachricht anzeigen
6890 GOSUB 12160                                               'Stammsatz lesen
6900 FOR I=1 TO FLDMAX
6910   S$=FLDSEARCH$(I)
6920   IF LEN(S$)=0 THEN 7050
6930   F$=LEFT$(S$,1)
6940   IF F$<>"*" THEN 6980
6950     IF LEN(S$)=1 THEN 7050
6960     IF INSTR(FLDDAT$(I),RIGHT$(S$,LEN(S$)-1))>0 THEN 7050
6970     GOTO 7090
6980   IF F$<>"?" THEN 7040
6990     P=2
7000     IF MID$(S$,P,1)="?" THEN P=P+1:GOTO 7000
7010     IF P=LEN(FLDDAT$(I))+1 THEN 7050         
7020     IF INSTR(FLDDAT$(I),RIGHT$(S$,LEN(S$)-P+1))=P THEN 7050
7030     GOTO 7090
7040   IF S$<>FLDDAT$(I) THEN 7090 
7050 NEXT I
7060 ISMATCH=TRUE
7070 RETURN
7080 '
7090 ISMATCH=FALSE
7100 RETURN
7110 '
7120 ' ######################### Verwendungsnachweis ##########################
7130 '
7140 GOSUB 11510                             'Einsprung MVW-Menü 'MBRMAX lesen
7150 VNFLAG=TRUE
7160 '
7170 ' Verwendungsnachweis-Menü ***********************************************
7180 '
7190 MT$="VERWENDUNGSNACHWEIS":MI$="G-012000":GOSUB 13700          'Basis-Maske
7200 F=4                                                                'TODO ?
7210 FKEYS$="":GOSUB 13810                                 'F-Tasten einblenden
7220 PRINT CURON$;
7230 PRINT HIOFF$;
7240 PRINT FNCUR$(4,23);"Bitte geben Sie die Mitgliedsnummer"
7250 PRINT FNCUR$(5,20);"des zu ändernden Verwendungsnachweises ein"
7260 'LW PRINT FNCUR$(6,23);"Gültige Mitgliedsnummern sind 1 -";STR$(MBRMAX);"."
7270 PRINT FNCUR$(6,20);" und beenden Sie die Eingabe mit RETURN."
7280 PRINT FNCUR$(8,38);"=>..."
7290 R=8:C=39:LNG=3:TYP=2
7300 GOSUB 10370                                                  'Feld-Eingabe
7310 IF FKEY=1 THEN HELPID=G0120001:GOSUB 17940:GOTO 7190           'Hilfe-Text
7320 IF FKEY=2 OR FKEY=3 THEN GOSUB 11400:VNFLAG=FALSE:GOTO 2290 'Feldwerte leeren
7330 IF FKEY=4 THEN GOSUB 14130                           'Zum Hauptmenü?-Frage
7340 IF FKEY>4 THEN 7290
7350 IF CKEY<>0 THEN 7290
7360 IF LEN(TEXTOUT$)=0 THEN N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 13490:GOTO 7280 ELSE MITGLIED=VAL(TEXTOUT$):IF MITGLIED=0 THEN 7300 'Nachricht anzeigen
7370 IF MITGLIED>MBRMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 13490:PRINT HOME$;HIOFF$:GOTO 7280 'Nachricht anzeigen
7380 MBRNUM=MITGLIED:MTGL$=STR$(MITGLIED)
7390 '
7400 ' *** Lesen des Mitgliedsnamens ******************************************
7410 '
7420 PRINT FNCUR$(21,1);DELLINE$;
7430 GET #2,MITGLIED                                           'DATEN STS lesen
7440 NNAM$=LEFT$(DD1$,25)                                 'NACHNAME RAUSFISCHEN
7450 VNAM$=MID$(DD1$,26,25)                               'VORNAME  RAUSFISCHEN
7460 '
7470 ' ** LEERZEICHEN HINTER NACHNAME ABSCHNEIDEN *****************************
7480 '
7490 FOR I=LEN(NNAM$) TO 1 STEP -1
7500   IF MID$(NNAM$,I,1)<>" " THEN NNAM$=LEFT$(NNAM$,I):I=1
7510 NEXT I
7520 NACHVORNAME$=NNAM$+" "+VNAM$
7530 GOSUB 14650                                             '1. VN-BLATT lesen
7540 BLATT=1
7550 VP=1:HP=1                                                 'INITIALISIERUNG
7560 F=8
7570 GOSUB 13700                                                   'Basis-Maske
7580 FKEYS$="<F7>=Speichern":GOSUB 13810                   'F-Tasten einblenden
7590 PRINT HIOFF$;
7600 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=Vorwärts";TAB(53);"<F9>=Blatt drucken";
7610 PRINT FNCUR$(3,2);"Blatt:"
7620 PRINT FNCUR$(5,2);"Mitglieds-Nr.:"
7630 PRINT FNCUR$(5,24);"Name:"
7640 MSK$="Von:  .  .   Bis:  .  .   Amt:"
7650 FOR I=7 TO 19 
7660   PRINT FNCUR$(I,2);MSK$;TAB(43);MSK$
7670 NEXT I
7680 GOSUB 15860                                                 'Ausgabe Daten
7690 LNG=2-(HP=7)*6                                    'BEGIN OF THE INPUT-LOOP
7700 TYP=2-(HP=7)
7710 R=VP+13*(VP>13)+6
7720 C=6-39*(VP>13)-(HP<7)*(HP-1)*3+(HP>3)*(HP<7)*4-(HP=7)*26
7730 GOSUB 13630                                             'Nachricht löschen
7740 GOSUB 10370                                                  'Feld-Eingabe
7750 IF HP<7 THEN IF ASCC=ASCRET THEN TEXTOUT$=STRING$(2-LEN(TEXTOUT$),"0")+TEXTOUT$   '+NULLEN
7760 IF CKEY<>0 OR ASCC=ASCRET THEN PRINT FNCUR$(R,C);TEXTOUT$
7770 IF ASCC=ASCRET THEN IF HP<7 THEN MID$(FLDDAT$(VP),-1+2*HP,2)=TEXTOUT$ ELSE FLDDAT$(VP)=LEFT$(FLDDAT$(VP),12)+TEXTOUT$+SPACE$(8-LEN(TEXTOUT$))
7780 IF ASCC=ASCRET AND HP<7 THEN ISYES=TRUE:TEST$=TEXTOUT$:ON HP GOSUB 12890,12920,12950,12890,12920,12950:IF NOT ISYES THEN N$="Ungültiges Datum":GOSUB 13490:GOTO 7740 'Datums-Check 'Nachricht anzeigen
7790 IF ASCC=ASCRET THEN IF HP=7 THEN HP=1:VP=VP-(VP<26)-(VP=26)*(-25):GOTO 7690 ELSE HP=HP+1:GOTO 7690     'NEUE SCREEN-POSITION
7800 IF CKEY<>0 THEN TEST$=MID$(FLDDAT$(VP),-(HP<7)*2*HP-1-(HP=7)*14,-(HP<7)*2-(HP=7)*8):PRINT FNCUR$(R,C);TEST$
7810 IF CKEY<>0 THEN IF HP<7 AND NOT TEST$="  " THEN ISYES=TRUE:ON HP GOSUB 12890,12920,12950,12890,12920,12950:IF NOT ISYES THEN N$="Ungültiges Datum":GOSUB 13490:GOTO 7740 'Nachricht anzeigen
7820 IF CKEY=CKEYDN THEN VP=VP-(VP<26)-(VP=26)*(-25):GOTO 7690
7830 IF CKEY=CKEYUP THEN VP=VP+(VP>1)-(VP=1)*25     :GOTO 7690
7840 IF CKEY=CKEYRT THEN HP=HP-(HP<7)-(HP=7)*(-6)   :GOTO 7690
7850 IF CKEY=CKEYLT THEN HP=HP+(HP>1)+(HP=1)*(-6)   :GOTO 7690
7860 IF FKEY=1 THEN HELPID=G0120002:GOSUB 17940:GOTO 7550           'Hilfe-Text
7870 IF FKEY=2 THEN 7190                                       'ZU MTGL-EINGABE
7880 IF FKEY=3 THEN VNFLAG=FALSE:GOSUB 11400:GOTO 2290        'Feldwerte leeren
7890 IF FKEY=4 THEN GOSUB 14130:GOSUB 13870:GOTO 7740     'Zum Hauptmenü?-Frage
7900 IF FKEY=5 THEN GOSUB 14740:GOTO 7690
7910 IF FKEY=6 THEN GOSUB 14880:GOTO 7690
7920 IF FKEY=7 THEN GOSUB 15520:GOTO 7740                 'SPEICHERN DANN INPUT
7930 IF FKEY=9 THEN GOSUB 16100:GOSUB 15070:GOTO 7730
7940 '
7950 ' ############# Mitgliedsliste drucken von 05*************** #############
7960 '
7970 MT$="MITGLIEDSLISTE DRUCKEN":MI$="G-013000":GOSUB 13700       'Basis-Maske
7980 FKEYS$="":GOSUB 13810                                 'F-Tasten einblenden
7990 PRINT FNCUR$(4,30);"1 - Suchen & Drucken"
8000 PRINT FNCUR$(6,30);"2 - Komplett drucken"
8010 PRINT FNCUR$(8,30);"3 - Datenblatt drucken"
8020 NKEYMAX=3:FKEYMAX=4:GOSUB 13970                              'Menü-Eingabe
8030 IF NKEY THEN ON NKEY GOTO 8100,8100,8860
8040 ON FKEY GOTO 8050,2290,2290,8060
8050 HELPID=G0130000:GOSUB 17940:GOTO 7970                          'Hilfe-Text
8060 GOSUB 14130:GOTO 8020                                'Zum Hauptmenü?-Frage
8070 '
8080 ' *** Suchen & Drucken, Komplett drucken *********************************
8090 '
8100 IF NKEY=1 THEN MT$="SUCHEN & DRUCKEN":MI$="G-013010"
8110 IF NKEY=2 THEN MT$="KOMPLETT DRUCKEN":MI$="G-013020"
8120 GOSUB 13700                                                   'Basis-Maske
8130 GOSUB 13810                                           'F-Tasten einblenden
8140 PRINT FNCUR$(4,22);"Bitte geben Sie das heutige Datum ein."
8150 PRINT FNCUR$(5,23);"Beenden Sie die Eingabe mit RETURN."
8160 PRINT FNCUR$(7,36);"=>........"
8170 ISINPUTOK=TRUE
8180 FKEYMASK$="1234":CKEYMASK$="":TEXTIN$=""
8190 R=7:C=38:LNG=8:TYP=4:ISMAND=TRUE
8200 GOSUB 10380                                                'Feld-Eingabe+1
8210 IF FKEY<>1 THEN 8260       'F-Tasten verarbeiten 'TODO: Wert geht verloren
8220   IF NKEY=1 THEN HELPID=G0130101
8230   IF NKEY=2 THEN HELPID=G0130200
8240   GOSUB 17940                                                  'Hilfe-Text
8250   GOTO 8100
8260 IF FKEY=2 THEN 7970
8270 IF FKEY=3 THEN 2290
8280 IF FKEY=4 THEN GOSUB 14130:GOTO 8200                 'Zum Hauptmenü?-Frage
8290 '
8300 D$=TEXTOUT$:GOSUB 13030                    'Test Datum 'RETURN verarbeiten
8310 IF NOT ISINPUTOK THEN N$=DATUM$:GOSUB 13490:GOTO 8190  'Nachricht anzeigen
8320 PRINT FNCUR$(7,37);HION$;DATUM$;HIOFF$
8330 '
8340 ISPRINT=TRUE 
8350 IF NKEY=2 THEN 8470
8360 '
8370 ' *** Suchen nach Nummern und Maske
8380 GOTO 4530                                  'Stammsatz suchen (und drucken)
8390 ' *** Rückkehr aus "Stammsatz suchen"
8400 ' IN: SEARCHMODE (BYNUMS, BYMASK), NUMSUCH1, NUMSUCH2, FLDSEARCH$()
8410 GOSUB 16420                                         'Drucker bereit?-Frage
8420 IF ISYES THEN 8540
8430 GOSUB 13810                                           'F-Tasten einblenden
8440 IF SEARCHMODE=BYNUMS THEN 4970 ELSE 5470 
8450 '
8460 ' *** Komplette Mitgliedsliste drucken
8470 SEARCHMODE=BYNUMS
8480 ISCOMPLETE=TRUE
8490 NUMSUCH1=1:NUMSUCH2=MBRMAX
8500 GOSUB 16420                                         'Drucker bereit?-Frage
8510 IF NOT ISYES THEN 8200
8520 '
8530 ' *** Drucken
8540 N$="Mitgliedsliste wird gedruckt... Abbrechen mit Taste.":GOSUB 13490 'Nachricht anzeigen
8550 GOSUB 16590                                 'Mitgliedsliste drucken (Kopf)
8560 IF SEARCHMODE=BYMASK THEN 8690
8570 '
8580 ' *** Suchen nach Nummern
8590 FOR MBRNUM=NUMSUCH1 TO NUMSUCH2
8600   GOSUB 12160                                             'Stammsatz lesen
8610   IF INSTR(FLDDAT$(5),"ausgetreten")>0 THEN 8630
8620     GOSUB 16730                         'Mitgliedsliste drucken (Mitglied)
8630   GOSUB 16490                                      'Druck abbrechen?-Frage
8640   IF ISYES THEN 8800
8650 NEXT MBRNUM
8660 GOTO 8800
8670 ' 
8680 ' *** Suchen nach Maske
8690 MBRNUM=0
8700 GOSUB 6780                                        'Nächsten Treffer suchen
8710 IF MBRNUM=-1 THEN 8800
8720   GOSUB 12160                                             'Stammsatz lesen
8730   IF INSTR(FLDDAT$(5),"ausgetreten")>0 THEN 8750
8740     GOSUB 16730                         'Mitgliedsliste drucken (Mitglied)
8750   GOSUB 16490                                      'Druck abbrechen?-Frage
8760   IF ISYES THEN 8800
8770 GOTO 8700
8780 '
8790 ' *** Suchen zuende
8800 GOSUB 13630                                             'Nachricht löschen
8810 IF ISCOMPLETE THEN ISCOMPLETE=FALSE:GOTO 7970       'TODO: Very hackish...
8820 IF SEARCHMODE=BYNUMS THEN 4740 ELSE 5380            'TODO: Very hackish...
8830 '
8840 ' *** Datenblatt drucken *************************************************
8850 '
8860 MT$="DATENBLATT DRUCKEN":MI$="G-013030":GOSUB 13700           'Basis-Maske
8870 FKEYS$="":GOSUB 13810                                 'F-Tasten einblenden
8880 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer"
8890 PRINT FNCUR$(5,23)"des zu druckenden Datenblattes ein."
8900 PRINT FNCUR$(6,22);"Gültige Mitgliedsnummern sind 1 -";STR$(MBRMAX);"."
8910 PRINT FNCUR$(7,23)"Beenden Sie die Eingabe mit RETURN."
8920 PRINT FNCUR$(10,38)"=>..."
8930 ISINPUTOK=TRUE
8940 FKEYMASK$="1234":CKEYMASK$="":TEXTIN$=""
8950 R=10:C=40:LNG=3:TYP=2:ISMAND=TRUE
8960 GOSUB 10380                                                'Feld-Eingabe+1
8970 IF FKEY=1 THEN HELPID=G0130300:GOSUB 17940:GOTO 8860 'Hilfe-Text 'TODO: Wert geht verloren
8980 IF FKEY=2 THEN 7970
8990 IF FKEY=3 THEN 2290
9000 IF FKEY=4 THEN GOSUB 14130:GOTO 8960                 'Zum Hauptmenü?-Frage
9010 '
9020 M=VAL(TEXTOUT$)                                        'RETURN verarbeiten
9030 IF M>=1 AND M<=MBRMAX THEN 9070
9040   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 13490         'Nachricht anzeigen
9050   ISINPUTOK=FALSE
9060   GOTO 8950
9070 MBRNUM=M
9080 GOSUB 16420                                         'Drucker bereit?-Frage
9090 IF ISYES THEN 9130
9100   GOSUB 13810                                         'F-Tasten einblenden
9110   TEXTIN$=TEXTOUT$
9120   GOTO 8950
9130 N$="Datenblatt drucken...":GOSUB 13570     'Nachricht anzeigen, F-Tasten ausblenden
9140 GOSUB 12160                                               'Stammsatz lesen
9150 FULLNAME$ = FLDDAT$(1)+" "+FLDDAT$(2)
9160 GOSUB 17110                          'Stammsatz drucken (Bildschirmformat)
9170 GOSUB 14650                                     'Verwendungsnachweis lesen
9180 GOSUB 16130                                   'Verwendungsnachweis drucken
9190 GOSUB 17380                                     'Mitgliedsvermerke drucken
9200 GOSUB 13630                                             'Nachricht löschen
9210 GOTO 8860
10000 '
10010 ' ### Unterprogramme von 06*****, 07****, 08**** und 09**** #############
10020 '
10030 ' *** Tasten deaktivieren, ASCII Codes für Cursortasten vergeben ********
10040 '
10050 RESTORE 10140
10060 READ KEY
10070 IF KEY<0 THEN 10170
10080 ' Map unshifted logical key code KEY to ASCII code 0
10090   PRINT ESC$;"41";CHR$(KEY);CHR$(0);
10100 ' Map shifted logical key code KEY to ASCII code 0
10110   PRINT ESC$;"42";CHR$(KEY);CHR$(0);
10120 GOTO 10060
10130 '
10140 DATA 11,26,27,32,33,47,48:     'ESC, CLR, DEL CHR, INT, TAB, INS, DEL/EOL
10150 DATA 53,67,68,73,94,97,-1:     'RVS,SCROL, LTRL, UNDL, RPT, CONT, Endflag
10160 ' Map unshifted logical cursor key codes to ASCII codes
10170 PRINT ESC$;"41";CHR$(88);CHR$(ASCCUP);            'Key 88 "Cursor up"   .
10180 PRINT ESC$;"41";CHR$(89);CHR$(ASCCDN);            'Key 89 "Cursor down" .
10190 PRINT ESC$;"41";CHR$(98);CHR$(ASCCLT);            'Key 98 "Cursor left" .
10200 PRINT ESC$;"41";CHR$(99);CHR$(ASCCRT);            'Key 99 "Cursor right".
10210 ' Map unshifted and shifted logical tab key codes to ASCII code "ESC"
10220 PRINT ESC$;"41";CHR$(33);CHR$(ASCESC);            'Key 33 "ESC"         .
10230 PRINT ESC$;"42";CHR$(33);CHR$(ASCESC);            'Key 33 "ESC"         .
10240 RETURN
10250 '
10260 ' *** Feld-Eingabe ******************************************************
10270 '
10280 ' IN: R   - Zeile  OUT: TEXTOUT$ TMP: P
10290 ' IN: C   - Spalte OUT: FKEY     TMP: ISVALID (TRUE, FALSE)
10300 ' IN: LNG - Länge  OUT: CKEY     TMP: ISINPUTOK (TRUE, FALSE)
10310 ' IN: TYP - Typ    OUT: ASCC
10320 ' IN: ISMAND (TRUE, FALSE)
10330 ' IN: TEXTIN$
10340 ' IN: FKEYMASK$
10350 ' IN: CKEYMASK$
10360 ' IN: VNFLAG
10370 ISINPUTOK=TRUE
10380 IF NOT ISINPUTOK THEN TEXTIN$=TEXTOUT$
10390 P=LEN(TEXTIN$)
10400 PRINT FNCUR$(R,C);HION$;TEXTIN$;HIOFF$;STRING$(LNG-P,".")
10410 TEXTOUT$=TEXTIN$
10420 PRINT CURON$;
10430 CKEY=0:FKEY=0
10440 PRINT FNCUR$(R,C+P);
10450 A$=INPUT$(1):ASCC=ASC(A$)
10460 IF ISINPUTOK THEN 10500
10470   GOSUB 13630                                          'Nachricht löschen
10480   PRINT FNCUR$(R,C+P);
10490   ISINPUTOK=TRUE
10500 ON TYP GOSUB 12680,12740,12790,12840                          'Typ prüfen
10510 IF NOT ISVALID THEN 10570
10520   IF P>=LNG THEN 10450                                  'Gültiges Zeichen
10530   PRINT HION$;A$;HIOFF$         
10540   TEXTOUT$=TEXTOUT$+A$
10550   P=P+1
10560   GOTO 10440
10570 IF ASCC<>ASCBSP THEN 10630                         'Backspace verarbeiten
10580   IF P<1 THEN 10450
10590   PRINT DEL$                        
10600   P=P-1
10610   TEXTOUT$=LEFT$(TEXTOUT$,P)
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
10730 N$="Ungültige Eingabe - Bitte wiederholen":GOSUB 13490         'Nachricht anzeigen
10740 ISINPUTOK=FALSE
10750 GOTO 10440
10760 '
10770 IF NOT ISMAND OR LEN(TEXTOUT$)>0 THEN 10810           'RETURN verarbeiten
10780   N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 13490   'Nachricht anzeigen
10790   ISINPUTOK=FALSE 
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
10920 FOR I=1 TO 30:A$=INKEY$:NEXT I
10930 RETURN
10940 '
10950 ' *** Stammsatzfelder-Definitionen lesen ******************************** 
10960 '
10970 RESTORE 11090
10980 FOR I=1 TO FLDMAX
10990   READ NAM$,COL,ROW,LNG,TYP
11000   FLDNAM$(I)=NAM$
11010   IF MID$(NAM$,13,1)="=" THEN FLDISMAND(I)=TRUE ELSE FLDISMAND(I)=FALSE                           
11020   FLDCOL(I)=COL
11030   FLDROW(I)=ROW
11040   FLDLNG(I)=LNG
11050   FLDTYP(I)=TYP 
11060 NEXT I
11070 RETURN
11080 '
11090 DATA "Nachname:     ",08,05,17,1
11100 DATA "Vorname:      ",08,06,17,1
11110 DATA "Feld 3:       ",08,07,17,1
11120 DATA "Feld 4:       ",08,08,17,1
11130 DATA "Status:       ",08,09,17,1
11140 DATA "Amt:          ",08,10,17,3
11150 DATA "Anrede:       ",08,12,17,1
11160 DATA "Feld 8:       ",08,13,17,1
11170 DATA "Feld 9:       ",08,14,17,1
11180 DATA "Straße Hausnr:",08,15,17,3
11190 DATA "Postleitzahl: ",08,16,17,2
11200 DATA "Ort:          ",08,17,17,1
11210 DATA "Telefonnummer:",08,18,17,3
11220 DATA "Feld 14:      ",08,19,15,1
11230 '
11240 DATA "Beruf:        ",43,06,17,1
11250 DATA "Feld 16:      ",43,07,17,1
11260 DATA "Feld 17:      ",43,09,17,1
11270 DATA "Feld 18:      ",43,10,17,1
11280 DATA "Feld 19:      ",43,11,17,1
11290 DATA "Feld 20:      ",43,12,17,1
11300 DATA "Feld 21:      ",43,13,17,1
11310 DATA "Feld 22:      ",43,14,17,1
11320 DATA "Feld 23:      ",43,15,17,1
11330 DATA "Feld 24:      ",43,16,17,1
11340 DATA "Feld 25:      ",43,17,17,1
11350 DATA "Feld 26:      ",43,18,17,1
11360 DATA "Feld 27:      ",43,19,21,1
11370 '
11380 ' *** Feldwerte leeren **************************************************
11390 '
11400 FOR I=1 TO FLDMAX:FLDDAT$(I)="":NEXT I
11410 RETURN
11420 '
11430 ' *** FLDSEARCH$() leeren ***********************************************
11440 '
11450 FOR I=1 TO FLDMAX:FLDSEARCH$(I)="":NEXT I
11460 RETURN
11470 '
11480 ' *** MBRMAX lesen ****************************************************** 
11490 '
11500 ' OUT: MBRMAX
11510 OPEN "I",#1,"B:MTGLZAHL.DAT"
11520   INPUT #1,MBRMAX$
11530   MBRMAX=VAL(MBRMAX$)
11540 CLOSE #1
11550 RETURN
11560 '
11570 ' *** MBRMAX speichern **************************************************
11580 '
11590 ' IN: MBRMAX
11600 OPEN "O",#1,"B:MTGLZAHL.DAT"
11610   PRINT #1,STR$(MBRMAX)
11620 CLOSE #1
11630 RETURN
11640 '
11650 ' *** Maske Feldwerte mit Feldplatzhalter ausgeben **********************
11660 '
11670 ' IN: FLDMAX
11680 FOR I=1 TO FLDMAX
11690   R=FLDROW(I)
11700   C=FLDCOL(I)+COLSEP
11710   PRINT FNCUR$(R,C);HION$;FLDDAT$(I);HIOFF$;
11720   PRINT STRING$(FLDLNG(I)-LEN(FLDDAT$(I)),".")
11730 NEXT I
11740 RETURN
11750 '
11760 ' *** Maske Feldnamen und Feldwerte mit Feldplatzhalter ausgeben ********
11770 '
11780 ' IN: FLDMAX
11790 FOR I=1 TO FLDMAX
11800   R=FLDROW(I)
11810   C=FLDCOL(I)
11820   PRINT FNCUR$(R,C);FLDNAM$(I)
11830   C=C+COLSEP
11840   PRINT FNCUR$(R,C);HION$;FLDDAT$(I);HIOFF$;
11850   PRINT STRING$(FLDLNG(I)-LEN(FLDDAT$(I)),".")               
11860 NEXT I
11870 RETURN
11880 '
11890 ' *** Maske Feldnamen und Suchfeldwerte mit Feldplatzhalter ausgeben ****
11900 '
11910 ' IN: FLDMAX
11920 FOR I=1 TO FLDMAX
11930   R=FLDROW(I)
11940   C=FLDCOL(I)
11950   PRINT FNCUR$(R,C);FLDNAM$(I)
11960   C=C+COLSEP
11970   PRINT FNCUR$(R,C);HION$;FLDSEARCH$(I);HIOFF$;
11980   PRINT STRING$(FLDLNG(I)-LEN(FLDSEARCH$(I)),".")               
11990 NEXT I
12000 RETURN
12010 '
12020 ' *** Mitgliedsnummer ausgeben ******************************************
12030 '
12040 ' IN: MBRNUM
12050 PRINT FNCUR$(3,31);"Mitgliedsnummer";HION$;MBRNUM;HIOFF$;"   "
12060 RETURN
12070 '
12080 ' *** FIELDen der Stammsatz-Datei *************************************** 
12090 '
12100 OPEN "R",#2,"B:MV.DAT",462+27
12110 FIELD #2,236+14 AS XSTAMMSATZDAT1$,225+13 AS XSTAMMSATZDAT2$
12120 RETURN
12130 '
12140 ' *** Stammsatz von Disk lesen ******************************************
12150 '
12160 GET #2,MBRNUM                                              'LW Compress?
12170 P=1
12180 FOR I=1 TO 14
12190   LNG=FLDLNG(I)+1
12200   TMP$=MID$(XSTAMMSATZDAT1$,P,LNG)
12210   L=ASC(LEFT$(TMP$,1))
12220   FLDDAT$(I)=MID$(TMP$,2,L)
12230   P=P+LNG
12240 NEXT I
12250 P=1
12260 FOR I=15 TO FLDMAX
12270   LNG=FLDLNG(I)+1
12280   TMP$=MID$(XSTAMMSATZDAT2$,P,LNG)
12290   L=ASC(LEFT$(TMP$,1))
12300   FLDDAT$(I)=MID$(TMP$,2,L)
12310   P=P+LNG
12320 NEXT I
12330 RETURN
12340 '
12350 ' *** Stammsatz auf Disk speichern **************************************
12360 '
12370 ' IN: MBRNUM
12380 TMP$=""                                                     'LW Compress?
12390 FOR I=1 TO 14
12400   LNG=FLDLNG(I)
12410   L=LEN(FLDDAT$(I))
12420   TMP$=TMP$+CHR$(L)+FLDDAT$(I)+SPACE$(LNG-LEN(FLDDAT$(I)))
12430 NEXT I
12440 LSET XSTAMMSATZDAT1$=TMP$
12450 TMP$=""
12460 FOR I=15 TO FLDMAX
12470   LNG=FLDLNG(I)
12480   L=LEN(FLDDAT$(I))
12490   TMP$=TMP$+CHR$(L)+FLDDAT$(I)+SPACE$(LNG-LEN(FLDDAT$(I)))
12500 NEXT I
12510 LSET XSTAMMSATZDAT2$=TMP$
12520 PUT #2,MBRNUM
12530 RETURN
12540 '
12550 ' *** Ja/Nein-Frage und F-Tasten ausblenden *****************************
12560 '
12570 ' IN:  Q$   - Frage
12580 ' OUT: ISYES - (TRUE, FALSE)
12590 N$=Q$+" (J=Ja/N=Nein)":GOSUB 13490                    'Nachricht anzeigen
12600 GOSUB 14070                                          'F-Tasten ausblenden
12610 ANSWER$=INPUT$(1)
12620 IF ANSWER$="J" OR ANSWER$="j" THEN ISYES=TRUE:RETURN
12630 IF ANSWER$="N" OR ANSWER$="n" THEN ISYES=FALSE:RETURN
12640 GOTO 12610
12650 '
12660 ' *** Testbereich 1 (Alpha) *********************************************
12670 '
12680 ISVALID=FALSE
12690 IF (ASCC>57 AND ASCC<127) OR (ASCC>31 AND ASCC<48) THEN ISVALID=TRUE
12700 RETURN
12710 '
12720 ' *** Testbereich 2 (Numerisch) *****************************************
12730 '
12740 ISVALID=FALSE: IF ASCC>47 AND ASCC<58 THEN ISVALID=TRUE
12750 RETURN
12760 '
12770 ' *** Testbereich 3 (Alphanumerisch) ************************************
12780 '
12790 ISVALID=FALSE: IF ASCC>31 AND ASCC<127 THEN ISVALID=TRUE
12800 RETURN
12810 '
12820 ' *** Testbereich 4 (Numerisch und ".") *********************************
12830 '
12840 ISVALID=FALSE: IF (ASCC>=48 AND ASCC<=57) OR ASCC=46 THEN ISVALID=TRUE
12850 RETURN
12860 '
12870 ' *** DATUMS-TEST (bei VN und Mitgliederliste drucken) ******************
12880 '
12890 IF VAL(TEST$)=0 OR VAL(TEST$)>31 THEN ISYES=FALSE                    'Tag
12900 RETURN
12910 '
12920 IF VAL(TEST$)=0 OR VAL(TEST$)>12 THEN ISYES=FALSE                  'Monat
12930 RETURN
12940 '
12950 IF VAL(TEST$)=0 THEN ISYES=FALSE                                    'Jahr
12960 RETURN
12970 '
12980 ' *** Test Datum
12990 '
13000 ' IN: D$
13010 ' OUT: DATUM$
13020 ' OUT: ISINPUTOK (TRUE, FALSE)
13030 ISINPUTOK=TRUE
13040 POS1=INSTR(D$,".")
13050 POS2=INSTR(POS1+1,D$,".")
13060 IF POS2=0 THEN 13260
13070 POS3=INSTR(POS2+1,D$,".")
13080 IF POS3<>0 THEN 13260
13090 D1$=LEFT$(D$,POS1-1)
13100 IF LEN(D1$)=0 OR LEN(D1$)>2 THEN 13260
13110 D2$=MID$(D$,POS1+1,POS2-POS1-1)
13120 IF LEN(D2$)=0 OR LEN(D2$)>2 THEN 13260
13130 D3$=RIGHT$(D$,LEN(D$)-POS2)
13140 IF LEN(D3$)=0 OR LEN(D3$)>2 THEN 13260
13150 '
13160 D1=VAL(D1$):IF D1<1 OR D1>31 THEN 13290
13170 D2=VAL(D2$):IF D2<1 OR D2>12 THEN 13290
13180 D3=VAL(D3$) 
13190 ' TODO: Genauere Verprobung
13200 D1$=STR$(D1):D1$=MID$(D1$,2,99):IF D1<10 THEN D1$="0"+D1$
13210 D2$=STR$(D2):D2$=MID$(D2$,2,99):IF D2<10 THEN D2$="0"+D2$
13220 D3$=STR$(D3):D3$=MID$(D3$,2,99):IF D3<10 THEN D3$="0"+D3$
13230 DATUM$=D1$+"."+D2$+"."+D3$
13240 GOTO 13320
13250 '
13260 DATUM$="Datum muss Format TT.MM.JJ haben. Bitte Eingabe korrigieren."
13270 GOTO 13300
13280 '
13290 DATUM$="Ungültiges Datum. Bitte Eingabe korrigieren."
13300 ISINPUTOK=FALSE
13310 '
13320 RETURN
13330 '
13340 ' *** Voreinstellungen zuweisen in "Stammsatz anlegen" ******************
13350 '
13360 FLDDAT$(5)="10***"
13370 FLDDAT$(7)=""11**""
13380 FLDDAT$(11)="12**"
13390 FLDDAT$(12)="13******"
13400 FLDDAT$(21)="3"
13410 FLDDAT$(22)="Nein"
13420 FLDDAT$(23)="Nein"
13430 FLDDAT$(25)="Nein"
13440 RETURN
13450 '
13460 ' *** Nachricht anzeigen ************************************************
13470 '
13480 ' IN: N$ - Nachricht
13490 PRINT FNCUR$(21,1);DELLINE$;
13500 IF N$<>"" THEN PRINT INVON$;" ";N$;" ";INVOFF$;
13510 PRINT                              'LW workaround quirk in display driver
13520 RETURN
13530 '
13540 ' *** Nachricht anzeigen, F-Tasten ausblenden *************************** 
13550 '
13560 ' IN: N$ - Nachricht
13570 GOSUB 13490                                           'Nachricht anzeigen
13580 GOSUB 14070                                          'F-Tasten ausblenden
13590 RETURN
13600 '
13610 ' *** Nachricht löschen *************************************************
13620 '
13630 N$="":GOSUB 13490                                     'Nachricht anzeigen
13640 RETURN
13650 '
13660 ' *** Bildschirmmaske - Basis *******************************************
13670 '
13680 ' IN: MT$ - Menü-Titel
13690 ' IN: MI$ - Menu-ID
13700 PRINT CUROFF$;CLS$;
13710 PRINT "00**********************";
13720 PRINT FNCUR$(1,(80-LEN(MT$))/2+1);MT$;FNCUR$(1,72);MI$
13730 PRINT ROWSEP$
13740 PRINT FNCUR$(21,1);
13750 PRINT ROWSEP$             'quirk in display driver places this in line 22
13760 RETURN
13770 '
13780 ' *** F-Tasten einblenden ***********************************************
13790 '
13800 ' IN: FKEYS$ - Zusätzliche F-Tasten 
13810 PRINT FNCUR$(23,1);"<F1>=Hilfe <F2>=Zurück <F3>=Anfang <F4>=Hauptmenü";
13820 IF FKEYS$<>"" THEN PRINT " ";FKEYS$;HOME$ ELSE PRINT             'LW Hack
13830 RETURN
13840 '
13850 ' *** restliche funktionstasten bei Verw.-nachweis ausgeben
13860 '
13870 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=Vorwärts             <F8>=Drucken";
13880 PRINT HOME$
13890 RETURN
13900 '
13910 ' *** Eingabe bei Menübildern *******************************************
13920 '
13930 ' IN:  NKEYMAX - Max gültige Zifferntaste (1,2,3,...)
13940 ' IN:  FKEYMAX - Max gültige F-Taste (1,2,3,...)
13950 ' OUT: NKEY    - Ziffer der gedrückten Zifferntaste oder 0
13960 ' OUT: FKEY    - Ziffer der gedrückten F-Taste oder 0
13970 NKEY=0:FKEY=0
13980 GOSUB 10920                                          'Tastatur entprellen
13990 A$=INPUT$(1):ASCC=ASC(A$)
14000 IF ASCC>=ASC1 AND ASCC<=ASC1+NKEYMAX-1 THEN NKEY=ASCC-ASC1+1:RETURN
14010 IF ASCC>=ASCF1 AND ASCC<=ASCF1+FKEYMAX-1 THEN FKEY=ASCC-ASCF1+1:RETURN
14020 N$="Sie haben eine falsche Taste gedrückt - Bitte neue Auswahl treffen.":GOSUB 13490 'Nachricht anzeigen
14030 GOTO 13990
14040 '
14050 ' *** F-Tasten ausblenden ***********************************************
14060 '
14070 PRINT FNCUR$(23,1);DELLINE$
14080 PRINT FNCUR$(24,1);DELLINE$;
14090 RETURN
14100 '
14110 ' *** 'Zum Hauptmenü?-Frage (Original) **********************************
14120 '
14130 GOSUB 14210                               'Zum Hauptmenü?-Frage (einfach)
14140 IF ISYES THEN 18240                                         'Programmende
14150 GOSUB 13810                                          'F-Tasten einblenden
14160 RETURN
14170 '
14180 ' *** 'Zum Hauptmenü?-Frage (einfach) ***********************************
14190 '
14200 ' OUT: ISYES (TRUE, FALSE)
14210 Q$="Wollen Sie wirklich ins Hauptmenü?":GOSUB 12590        'Ja/Nein-Frage
14220 GOSUB 13630                                            'Nachricht löschen
14230 RETURN
14240 '
14250 ' *** Rückkehr aus Hilfe-Text? ******************************************
14260 '
14270 N$="Zurück? (J=Ja)":GOSUB 13490                       'Nachricht anzeigen
14280 GOSUB 12600                             'Ja/Nein-Frage ohne String-Koncat
14290 IF NOT ISYES THEN 14270
14300 RETURN
14310 '
14320 ' **** VN - Subroutines, permitting one page handling *******************
14330 ' ****         by 04********** 21*******              *******************
14340 '
14350 ' **** OPEN & FIELDING VERWENDUNGSNACHWEIS ******************************
14360 '
14370 OPEN "R",#3,"B:VN.NDX",4
14380 FIELD #3,4 AS XVNPOINT0$
14390 OPEN "R",#4,"B:VN.POI",8
14400 FIELD #4,4 AS XVNPOINT1$, 4 AS XVNPOINT2$
14410 OPEN "R",#5,"B:VN.DAT",560
14420 FIELD #5,240 AS XVNDAT1$,240 AS XVNDAT2$,80 AS XVNDAT3$
14430 RETURN
14440 '
14450 ' *** VERWENDUNGSNACHWEIS FÜR NEUES MITGLIED ANLEGEN ********************
14460 '
14470 ' IN: MBRNUM
14480 GET #3,1                               'letzte Seitennummer lesen dann +1
14490 TMPPOINT=VAL(XVNPOINT0$)+1
14500 LSET XVNDAT1$=""                                 'leeren Record speichern
14510 LSET XVNDAT2$=""
14520 LSET XVNDAT3$=""
14530 PUT #5,TMPPOINT
14540 LSET XVNPOINT0$=STR$(TMPPOINT)         'letzte Seitennummer neu speichern
14550 PUT #3,1
14560 PUT #3,MBRNUM+1
14570 LSET XVNPOINT1$="0000"
14580 LSET XVNPOINT2$="0000"
14590 PUT #4,TMPPOINT
14600 RETURN
14610 '
14620 ' **** ERSTE SEITE FINDEN & LESEN ***************************************
14630 '
14640 ' IN: MBRNUM
14650 GET #3,MBRNUM+1                                             'Index-Datei
14660 CPOINT=VAL(XVNPOINT0$)
14670 GET #5,CPOINT:GOSUB 15700                                    'Daten lesen
14680 BLATT=1
14690 GET #4,CPOINT                                              'Pointer lesen
14700 RETURN
14710 '
14720 ' **** SEITE RÜCKWÄRTS **************************************************
14730 '
14740 TMPPOINT=VAL(XVNPOINT1$)
14750 IF TMPPOINT<>0 THEN 14780
14760 N$="Erste Seite":GOSUB 13490                          'Nachricht anzeigen
14770 RETURN
14780 CPOINT=TMPPOINT
14790 BLATT=BLATT-1
14800 GET #5,CPOINT:GOSUB 15700                                    'Daten lesen
14810 GET #4,CPOINT:GOSUB 15860                             'Neue Pointer lesen
14820 HP=1:VP=1
14830 GOSUB 15070
14840 RETURN
14850 '
14860 ' **** SEITE VORWÄRTS  **************************************************
14870 '
14880 TMPPOINT=VAL(XVNPOINT2$)
14890 IF TMPPOINT=0 THEN 14960
14900 CPOINT=TMPPOINT
14910 BLATT=BLATT+1
14920 GET #5,CPOINT:GOSUB 15700
14930 GET #4,CPOINT:GOSUB 15860
14940 HP=1:VP=1
14950 RETURN
14960 N$="Letzte Seite"
14970 IF INSTR(SPACE$(20),FLDDAT$(26))=0 THEN 15000 
14980 GOSUB 13490                                           'Nachricht anzeigen
14990 RETURN
15000 Q$="Letzte Seite - Neue Seite anlegen?":GOSUB 12590        'Ja/Nein-Frage
15010 GOSUB 13630                                            'Nachricht löschen
15020 IF NOT ISYES THEN 15070
15030 GOSUB 15310
15040 BLATT=BLATT+1
15050 GOSUB 15860
15060 HP=1:VP=1
15070 GOSUB 13810                                          'F-Tasten einblenden
15080 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=vorwärts";
15090 PRINT TAB(54);"<F9>=Blatt drucken";
15100 PRINT HOME$;" "
15110 RETURN
15120 '
15130 ' **** ERSTE SEITE EINES MITGLIEDS ANLEGEN ******************************
15140 '
15150 GET #3,1
15160 LSET XVNPOINT0$=STR$(VAL(XVNPOINT0$)+1)                            'NDX+1
15170 PUT #3,1
15180 LSET XVNPOINT1$="0000"                                     'Page pointers
15190 LSET XVNPOINT2$="0000"
15200 PUT #4,VAL(XVNPOINT0$)
15210 LSET XVNPOINT0$=XVNPOINT0$                                    'First page
15220 PUT #3,MBRNUM+1
15230 LSET XVNDAT1$=""                                              'Datenblatt
15240 LSET XVNDAT2$=""
15250 LSET XVNDAT3$=""
15260 PUT #5,VAL(XVNPOINT0$)
15270 RETURN
15280 '
15290 ' **** ALLGEMEIN EINE NEUE SEITE ANLEGEN ********************************
15300 '
15310 GET #3,1                                                           'NDX+1
15320 LSET XVNPOINT0$=STR$(VAL(XVNPOINT0$)+1)
15330 PUT #3,1:TMPPOINT$=XVNPOINT0$    'Vektoren der bisher letzten Seite verbiegen
15340 LSET XVNPOINT2$=XVNPOINT0$                          'Verbiege Vektoren...
15350 PUT #4,CPOINT                                         '...der alten Seite
15360 LSET XVNPOINT1$=STR$(CPOINT)                          '...der neuen Seite
15370 LSET XVNPOINT2$="0000"                                             '.. ""
15380 CPOINT=VAL(TMPPOINT$)                                      'Nr neue Seite
15390 PUT #4,CPOINT
15400 LSET XVNDAT1$=""                                        'Write empty page
15410 LSET XVNDAT2$=""
15420 LSET XVNDAT3$="" 
15430 PUT #5,CPOINT
15440 DATEN$=SPACE$(20)
15450 FOR I=1 TO 26
15460   FLDDAT$(I)=DATEN$
15470 NEXT I
15480 RETURN
15490 '
15500 ' **** SEITE SPEICHERN **************************************************
15510 '
15520 DATEN$=""
15530 FOR I=1 TO 12
15540   DATEN$=DATEN$+FLDDAT$(I)
15550 NEXT I
15560 LSET XVNDAT1$=DATEN$:DATEN$=""
15570 FOR I=13 TO 24
15580   DATEN$=DATEN$+FLDDAT$(I)
15590 NEXT I
15600 LSET XVNDAT2$=DATEN$:DATEN$=""
15610 FOR I=25 TO 26
15620   DATEN$=DATEN$+FLDDAT$(I)
15630 NEXT I
15640 LSET XVNDAT3$=DATEN$:DATEN$=""
15650 PUT #5,CPOINT
15660 RETURN
15670 '
15680 ' **** HACK ! (ZERSTÜCKELN DER DATEN-STRINGS) ***************************
15690 '
15700 FOR I=1 TO 12
15710   FLDDAT$(I)=MID$(XVNDAT1$,(I-1)*20+1,20)
15720 NEXT I
15730 FOR I=13 TO 24
15740   FLDDAT$(I)=MID$(XVNDAT2$,(I-13)*20+1,20)
15750 NEXT I
15760 FOR I=25 TO 26
15770   FLDDAT$(I)=MID$(XVNDAT3$,(I-25)*20+1,20)
15780 NEXT I
15790 RETURN
15800 '
15810 ' **** UPRO DATENAUSGABE VN  ********************************************
15820 '
15830 ' IN: MBRNUM
15840 ' IN: NACHVORNAME$
15850 ' IN: BLATT
15860 PRINT DELHIGH$;                                          'TODO Necessary?
15870 PRINT HION$
15880 PRINT FNCUR$(5,16);MBRNUM
15890 PRINT FNCUR$(5,29);NACHVORNAME$
15900 PRINT FNCUR$(3,8);BLATT
15910 FOR I=1 TO 26
15920   HOR=6-39*(I>13)
15930   VER=6+I+13*(I>13)
15940   FOR J=1 TO 3                                                 'Von-Datum
15950     PRINT FNCUR$(VER,HOR+(J-1)*3);MID$(FLDDAT$(I),-1+J*2,2)
15960   NEXT
15970   FOR J=1 TO 3                                                 'Bis-Datum
15980     PRINT FNCUR$(VER,HOR+13+(J-1)*3);MID$(FLDDAT$(I),5+J*2,2)
15990   NEXT
16000   PRINT FNCUR$(VER,HOR+26);MID$(FLDDAT$(I),13,8)                     'Amt
16010 NEXT
16020 PRINT HIOFF$
16030 RETURN
16040 '
16050 ' *** Verwendungsnachweis drucken ***************************************
16060 '
16070 ' IN: BLATT
16080 ' IN: MITGLZAHL
16090 ' IN: NACHVORNAME$
16100 Q$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 12590 'Ja/Nein-Frage TODO Back to main code?
16110 IF NOT ISYES THEN 16350                             'LW TODO Remove this!
16120 '
16130 RETURN                                         '*** TMP Print Disable ***
16140 I$=SPACE$(2)
16150 '
16160 PRINT ROWSEP$
16170 PRINT
16180 PRINT I$;"Blatt:";BLATT
16190 PRINT
16200 PRINT I$;"Mitglieds-Nr.:";MBRNUM;TAB(24);"Name:";NACHVORNAME$
16210 PRINT
16220 FOR I=1 TO 13
16230   KK=I:GOSUB 16320
16240   PRINT I$;"Von:";VON$;" Bis:";BIS$;" Amt:";AMT$;
16250   KK=I+13:GOSUB 16320
16260   PRINT " Von:";VON$;" Bis:";BIS$;" Amt:";AMT$
16270 NEXT 
16280 PRINT
16290 PRINT ROWSEP$
16300 RETURN
16310 'Format: VVVVVV BBBBBB AAAAAAAA
16320 VON$=LEFT$(FLDDAT$(KK),2)+"."+MID$(FLDDAT$(KK),3,2)+"."+MID$(FLDDAT$(KK),5,2)
16330 BIS$=MID$(FLDDAT$(KK),7,2)+"."+MID$(FLDDAT$(KK),9,2)+"."+MID$(FLDDAT$(KK),11,2)
16340 AMT$=RIGHT$(FLDDAT$(KK),8)
16350 RETURN
16360 '
16370 ' ### Unterprogramme "Mitgliedsliste drucken" ###########################
16380 '
16390 ' *** Drucker bereit?-Frage *********************************************
16400 '
16410 ' OUT: ISYES (TRUE, FALSE)
16420 Q$="Ist der Drucker eingeschaltet und bereit zur Ausgabe?":GOSUB 12590 'Ja/Nein-Frage
16430 GOSUB 13630                                            'Nachricht löschen
16440 RETURN
16450 '
16460 ' *** Druck abbrechen?-Frage ********************************************
16470 '
16480 ' OUT: ISYES (TRUE, FALSE)
16490 ISYES=FALSE 
16500 IF INKEY$="" THEN 16540
16510 Q$="Drucken abbrechen?":GOSUB 12590                        'Ja/Nein-Frage
16520 IF ISYES THEN 16540
16530 N$="Mitgliedsliste wird gedruckt... Abbrechen mit Taste.":GOSUB 13490 'Nachricht anzeigen
16540 RETURN
16550 '
16560 ' *** Mitgliedsliste drucken (Kopf) *************************************
16570 '
16580 ' IN: DATUM$
16590 PRINT TAB(16);"MITGLIEDSLISTE 25*******************************"
16600 PRINT 
16610 PRINT TAB(32);"Stand:"; DATUM$
16620 PRINT
16630 PRINT TAB(3);"Mitgl."; 
16640 PRINT TAB(10);"Name";
16650 PRINT TAB(36);"Vorname, Anschrift";
16660 PRINT TAB(63);"Status";
16670 PRINT TAB(71);"Amt"
16680 RETURN
16690 '
16700 ' *** Mitgliedsliste drucken (Mitglied) *********************************
16710 '
16720 ' IN:  MBRNUM
16730 AMT$=""
16740 IN=INSTR(FLDDAT$(6),"22**"):IF IN>0 THEN AMT$=MID$(FLDDAT$(6),IN,6):GOTO 16780
16750 IN=INSTR(FLDDAT$(6),"23**"):IF IN>0 THEN AMT$=MID$(FLDDAT$(6),IN,5):GOTO 16780
16760 IN=INSTR(FLDDAT$(6),"24**"):IF IN>0 THEN AMT$=MID$(FLDDAT$(6),IN,3)
16770 '
16780 PRINT
16790 PRINT TAB(5);MBRNUM;
16800 PRINT TAB(10);FLDDAT$(1);                                       'Nachname
16810 PRINT TAB(36);FLDDAT$(2);                                        'Vorname
16820 PRINT TAB(65);"(";LEFT$(FLDDAT$(5),1);")";                        'Status
16830 PRINT TAB(71);AMT$                                                   'Amt
16840 PRINT TAB(36);FLDDAT$(10)                                         'Straße
16850 PRINT TAB(36);FLDDAT$(11);                                  'Postleitzahl
16860 PRINT TAB(41);FLDDAT$(12)                                            'Ort
16870 PRINT TAB(36);"Tel.:";FLDDAT$(13)                                'Telefon
16880 PRINT
16890 RETURN
16900 '
16910 ' *** Adressaufkleber drucken *******************************************
16920 '
16930 I$=SPACE$(3)
16940 '
16950 PRINT I$;FLDDAT$(7)                                               'Anrede
16960 PRINT I$;FLDDAT$(2);" ";FLDDAT$(1)                      'Vorname Nachname
16970 PRINT
16980 PRINT I$;FLDDAT$(10)                                   'Straße Hausnummer
16990 PRINT
17000 PRINT I$;FLDDAT$(11);" ";FLDDAT$(12)                    'Postleitzahl Ort
17010 PRINT
17020 PRINT
17030 PRINT
17040 PRINT
17050 PRINT
17060 PRINT
17070 RETURN
17080 '
17090 ' *** Stammsatz drucken (Bildschirmformat)
17100 '
17110 TMP1=8
17120 TMP2=43
17130 '
17140 PRINT ROWSEP$
17150 PRINT
17160 PRINT TAB(31);"Mitgliedsnummer";MBRNUM
17170 PRINT
17180 RESTORE 17240
17190 FOR I=1 TO 7:READ A,B
17200   IF A<>0 THEN PRINT TAB(TMP1);FLDNAM$(A);FLDDAT$(A);
17210   IF B<>0 THEN PRINT TAB(TMP2);FLDNAM$(B);FLDDAT$(B);
17220   PRINT
17230 NEXT I
17240 DATA 1,0,2,15,3,16,4,0,5,17,6,18,0,19
17250 FOR I=7 TO 14
17260   PRINT TAB(TMP1);FLDNAM$(I);FLDDAT$(I);
17270   PRINT TAB(TMP2);FLDNAM$(I+13);FLDDAT$(I+13);
17280 NEXT I
17290 PRINT
17300 PRINT
17310 PRINT
17320 PRINT ROWSEP$
17330 RETURN
17340 '
17350 ' *** Mitgliedsvermerke drucken
17360 '
17370 ' IN: FULLNAME$
17380 I$=SPACE$(9)
17390 FORM1$="___"
17400 FORM2$="___ / ___"
17410 FORM3$="___ / ___ / ___"
17420 '
17430 PRINT TAB(10);"Mitgliedsnr.: ";FORM1$;
17440 PRINT TAB(28);"Name: ";FULLNAME$;
17450 PRINT TAB(58);"Datum: ";FORM3$
17460 PRINT
17470 PRINT I$;"26*************************** ";FORM3$
17480 PRINT
17490 PRINT I$;"27*************************** ";FORM3$
17500 PRINT
17510 PRINT I$;"28*************************** ";FORM3$
17520 PRINT
17530 PRINT I$;"29*************************** ";FORM3$
17540 PRINT
17550 PRINT I$;"30*************************** ";FORM2$
17560 PRINT
17570 PRINT I$;"31*************************** ";FORM3$
17580 PRINT
17590 PRINT I$;"32*************************** ";FORM3$
17600 PRINT
17610 PRINT I$;"33*************************** ";FORM1$
17620 PRINT
17630 PRINT I$;"34*************************** ";FORM3$
17640 PRINT
17650 PRINT I$;"35*************************** ";FORM3$
17660 PRINT
17670 PRINT I$;"36*************************** ";FORM3$
17680 PRINT
17690 PRINT I$;"37*************************** ";FORM3$
17700 PRINT
17710 PRINT I$;"38*************************** ";FORM3$
17720 RETURN
17730 '
17740 ' ### HILFE-TEXT ########################################################
17750 '
17760 ' *** Fielden der Hilfe-Text-Dateien ************************************
17770 '
17780 OPEN "R",#6,"MVWHELP.NDX",4
17790   FIELD #6,2 AS XFIRSTLINE$,2 AS XLASTLINE$
17800   FOR I=1 TO HELPIDMAX
17810     GET #6,I
17820     HELPFIRSTROW(I)=CVI(XFIRSTLINE$)
17830     HELPLASTROW(I)=CVI(XLASTLINE$)
17840   NEXT I
17850 CLOSE #6
17860 '
17870 OPEN "R",#6,"MVWHELP.DAT",64
17880 FIELD #6,2 AS XINDENT$,2 AS XLENGTH$,60 AS XROWTEXT$
17890 RETURN
17900 '
17910 ' *** Hilfe-Text-Seite anzeigen *****************************************
17920 '
17930 ' IN: HELPID
17940 SAVEMT$=MT$:MT$="HILFE":GOSUB 13700:MT$=SAVEMT$              'Basis-Maske
17950 PRINT HOME$
17960 FIRSTROW=HELPFIRSTROW(HELPID)
17970 LASTROW=HELPLASTROW(HELPID)
17980 FOR I=FIRSTROW TO LASTROW
17990   GET #6,I
18000   INDENT=CVI(XINDENT$)
18010	LENGTH=CVI(XLENGTH$)
18020   PRINT TAB(INDENT);LEFT$(XROWTEXT$,LENGTH)
18030 NEXT I
18040 GOSUB 14270                                      'Rückkehr aus Hilfe-Text
18050 RETURN
18060 '
18070 ' ### PROGRAMMFEHLER, PROGRAMMENDE ######################################
18080 '
18090 ' *** Programmfehler ****************************************************
18100 '
18110 I$=SPACE$(15)
18120 GOSUB 13700                                                  'Basis-Maske
18130 PRINT HOME$
18140 PRINT I$;"Ein Programmfehler ist aufgetreten."
18150 PRINT   
18160 PRINT I$;"- Bitte notieren Sie folgende Fehlerinformation:"
18170 PRINT I$;"  ";HION$;"ERL";HIOFF$;" (Error Line)";HION$;ERL;HIOFF$
18180 PRINT I$;"  ";HION$;"ERR";HIOFF$;" (Error Code)";HION$;ERR;HIOFF$
18190 PRINT I$;"- Drücken Sie am Rechner den Knopf hinten rechts."
18200 PRINT I$;"19**************************"
18210 '
18220 ' *** Programmende ******************************************************
18230 '
18240 PRINT FNCUR$(22,1);HIOFF$;CURON$;
18250 RESET
18260 'LW RUN "DISKMENU.EXE"
18270 END                                                           '1622 lines
