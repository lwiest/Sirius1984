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
1760 ON ERROR GOTO 20150                                 'Enable error trapping
1770 '
1780 GOSUB 10050    'Tasten deaktivieren, ASCII Codes für Cursortasten vergeben
1790 PRINT CUROFF$;
1800 '
1810 ' *** Beginn des Mitgliederverwaltungsprogramms **************************
1820 '
1830 MT$="MITGLIEDERVERWALTUNG":MI$="G-010000":GOSUB 13130         'Basis Maske
1840 Q$="Bitte Daten-Diskette in Laufwerk B einlegen. Bereit?":GOSUB 12280 'Ja/Nein-Frage
1850 IF JA=0 THEN 20280                                           'Programmende
1860 '
1870 N$="Einen Moment bitte...":GOSUB 13000       'Nachricht. F-keys ausblenden
1880 GOSUB 10960                           'Einlesen Stammsatzfelder-Definition
1890 GOSUB 11770                                   'Fielden der Stammsatz-Datei
1900 GOSUB 13800                       'Fielden der Verwendungsnachweis-Dateien
1910 GOSUB 11230                                              'Einlesen MTGLMAX
1920 GOSUB 13060                                             'Nachricht löschen
1930 '
1940 ' ################# Mitgliederverwaltungsmenü (MVW-Menü) #################
1950 '
1960 MT$="MITGLIEDERVERWALTUNG":MI$="G-010000":GOSUB 13130         'Basis Maske
1970 FKEYS$="":GOSUB 13240                           'F-Tastenleiste einblenden
1980 VNFLAG=0                                                              '???
1990 PRINT FNCUR$(4,26);"1 - Stammsatzpflege"                         'Menütext
2000 PRINT FNCUR$(6,26);"2 - Verwendungsnachweis"
2010 PRINT FNCUR$(8,26);"3 - Mitgliedsliste drucken"
2020 FKEYMAX=4:NKEYMAX=3:GOSUB 13400                              'Menü-Eingabe
2030 DRUCKFLAG=0                                                           '???
2040 IF NKEY THEN ON NKEY GOTO 2130,6150,6970
2050 ON FKEY GOTO 2060,2070,1960,2070         'Hilfe, Zurück, Anfang, Hauptmenü
2060 GOSUB 17230:GOTO 1960                                      'F1->Hilfe-Text
2070 GOSUB 13560:GOTO 2020                           'F2->Zurück, F4->Hauptmenü
2080 '
2090 ' ########################### Stammsatzpflege ############################
2100 '
2110 ' *** Stammsatzpflege-Menü ***********************************************
2120 '
2130 MT$="STAMMSATZPFLEGE":MI$="G-011000":GOSUB 13130              'Basis Maske
2140 FKEYS$="":GOSUB 13240                           'F-Tastenleiste einblenden
2150 PRINT FNCUR$(4,29);"1 - Stammsatz anlegen"
2160 PRINT FNCUR$(6,29);"2 - Stammsatz ändern"
2170 PRINT FNCUR$(8,29);"3 - Stammsatz löschen"
2180 PRINT FNCUR$(10,29);"4 - Stammsatz drucken"
2190 PRINT FNCUR$(12,29);"5 - Stammsatz suchen"
2200 FKEYMAX=4:NKEYMAX=5:GOSUB 13400                              'Menü-Eingabe
2210 IF NKEY THEN ON NKEY GOTO 2280,2820,3440,3816,3870
2220 ON FKEY GOTO 2230,1960,1960,2240         'Hilfe, Zurück, Anfang, Hauptmenü
2230 GOSUB 17450:GOTO 2130                                      'F1->Hilfe-Text
2240 GOSUB 13560:GOTO 2200                                       'F4->Hauptmenü
2250 '
2260 ' *** Stammsatz erfassen von 02************ ******************************
2270 '
2280 MTGLZAHL=MTGLMAX+1
2290 F=1
2310 MT$="STAMMSATZ ANLEGEN":MI$="G-011010":GOSUB 13130            'Basis Maske
2320 FKEYS$="<F7>=Speichern":GOSUB 13240             'F-Tastenleiste einblenden
2330 GOSUB 11120                                                 'DAT$() leeren
2340 GOSUB 12790                                           'Defaultwerte setzen
2350 PRINT FNCUR$(3,31);"Mitgliedsnummer";HIGHON$;MTGLZAHL;HIGHOFF$
2360 GOSUB 11650                                          'Felder und Feldwerte
2370 Z=FEROW(F):S=FECOL(F)+XOFF:LNG=FELNG(F):TYP=FETYP(F):MUSS=FEMUSS(F)
2380 FKEYMASK$="12347":CKEYMASK$="12":INTEXT$=DAT$(F)
2390 GOSUB 10370                                                         'Input
2400 IF ASCC<>ASCRET THEN 2440                                   'Handle RETURN
2410   DAT$(F)=OUTTEXT$
2420   F=F+1:IF F>FELDMAX THEN F=1
2430   GOTO 2370
2440 IF CKEY=0 THEN 2490                                    'Handle cursor keys
2450   PRINT FNCUR$(Z,S);HIGHON$;DAT$(F);HIGHOFF$;STRING$(LNG-LEN(DAT$(F)),".")
2460   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FELDMAX
2470   IF CKEY=CKEYDN THEN F=F+1:IF F>FELDMAX THEN F=1
2480   GOTO 2370
2490 IF FKEY=1 THEN GOSUB 17700:GOTO 2310                       'F1->Hilfe-Text
2500 IF FKEY=2 THEN GOSUB 2660:GOTO 2130                       'Exit "Erfassen"
2510 IF FKEY=3 THEN GOSUB 2660:GOTO 1960                       'Exit "Erfassen"
2520 IF FKEY<>4 THEN 2570
2530   GOSUB 13630                                'Rückkehr ins Hauptmenü Frage
2540   IF JA=0 THEN GOSUB 13240:GOTO 2390                    'F-Keys einblenden
2550   GOSUB 2660                                              'Exit "Erfassen"
2560   GOTO 20280                                                 'Programmende
2570 IF FKEY=7 THEN GOSUB 2730:GOTO 2280
2660 GOSUB 10910                                             'Debounce keyboard
2670 Q$="Wollen Sie vorher den Stammsatz speichern?":GOSUB 12280 'Ja/Nein-Frage
2680 IF JA=1 THEN 2730
2690   N$="Stammsatz wird nicht gespeichert.":GOSUB 13000 'Nachricht. F-keys ausblenden
2700   MTGLZAHL=MTGLZAHL-1
2710   GOTO 2770
2720 '                                                            
2730 N$="Stammsatz wird gespeichert...":GOSUB 13000 'Nachricht. F-keys ausblenden
2740 GOSUB 12080                                       'Speichern der Felddaten
2750 GOSUB 13910                                        'Erste VN-Seite anlegen
2760 MTGLMAX=MTGLZAHL:GOSUB 11320                            'Schreiben MTGLMAX
2770 GOSUB 13060                                             'Nachricht löschen
2780 RETURN
2790 ' 
2800 ' *** Stammsatz ändern von 03************ ********************************
2810 '
2820 MT$="STAMMSATZ ÄNDERN":MI$="G-011020":GOSUB 13130             'Basis Maske
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
3050 N$="Stammsatz wird gelesen...":GOSUB 13000   'Nachricht. F-keys ausblenden
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
3330 GOTO 20280                                                   'Programmende
3340 '
3350 Q$="Wollen Sie den Stammsatz speichern?":GOSUB 12280        'Ja/Nein-Frage
3360 IF JA=0 THEN RETURN
3370 N$="Stammsatz wird gespeichert...":GOSUB 13000 'Nachricht. F-keys ausblenden
3380 GOSUB 12080                                               'Daten speichern
3390 GOSUB 13060                                             'Nachricht löschen
3400 RETURN
3410 '
3420 ' *** Stammsatz löschen von 04********** *********************************
3430 '
3440 MT$="STAMMSATZ LÖSCHEN":MI$="G-011030":GOSUB 13130            'Basis Maske
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
3680 N$="Stammsatz wird gelesen...":GOSUB 13000   'Nachricht. F-keys ausblenden
3690 GOSUB 11830                                                   'Daten lesen
3700 GOSUB 13060                                             'Nachricht löschen
3710 '
3720 GOSUB 13130                                                   'Basis Maske
3730 PRINT FNCUR$(3,31);"Mitgliedsnummer";HIGHON$;MTGLZAHL;HIGHOFF$
3740 GOSUB 11650                                 'Felder und Feldwerte ausgeben
3750 Q$="Mitglied endgültig löschen?":GOSUB 12280                'Ja/Nein-Frage
3760 IF JA=0 THEN 3812
3770   DAT$(5)="ausgetreten"
3780   PRINT FNCUR$(FEROW(5),FECOL(5)+XOFF);HIGHON$;DAT$(5);HIGHOFF$;
3790   PRINT STRING$(FELNG(5)-LEN(DAT$(5)),".")                'LW NO semicolon
3800   N$="Stammsatz wird gespeichert...":GOSUB 13000 'Nachricht. F-keys ausblenden
3810   GOSUB 12080                                             'Daten speichern
3811   GOSUB 13060                                           'Nachricht löschen
3812 GOTO 3440
3813 '
3814 ' *** Stammsatz drucken von 02************ & 04********** ****************
3815 '
3816 MT$="STAMMSATZ DRUCKEN":MI$="G-011040":GOSUB 13130            'Basis Maske
3817 FKEYS$="":GOSUB 13240                           'F-Tastenleiste einblenden
3818 PRINT FNCUR$(4,24)"Bitte geben Sie die Mitgliedsnummer"
3819 PRINT FNCUR$(5,23)"des auszudruckenden Stammsatzes ein."
3820 PRINT FNCUR$(6,23);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
3821 PRINT FNCUR$(7,24)"Beenden Sie die Eingabe mit RETURN."
3822 PRINT FNCUR$(9,39);"=>..."
3823 BADINPUT=0
3824 Z=9:S=41:LNG=3:TYP=2:MUSS=1
3825 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
3826 GOSUB 10380                                                  'Input++
3827 IF FKEY=1 THEN GOSUB 18940:GOTO 3816                               'F-Keys
3828 IF FKEY=2 THEN 2130
3829 IF FKEY=3 THEN 1960
3830 IF FKEY=4 THEN GOSUB 13560:GOTO 3826         'Rückkehr ins Hauptmenü-Frage
3831 '
3832 M=VAL(OUTTEXT$)                                             'Handle RETURN
3833   IF M>=1 AND M<=MTGLMAX THEN 3838
3834   N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12920       'Nachricht
3835   BADINPUT=1
3836   INTEXT$=OUTTEXT$                                                    '???
3837   GOTO 3824
3838 MTGLZAHL=M
3839 N$="Stammsatz wird gelesen...":GOSUB 13000   'Nachricht. F-keys ausblenden
3840 GOSUB 11830                                                   'Daten lesen
3841 GOSUB 13060                                             'Nachricht löschen
3842 '
3843 GOSUB 13130                                                   'Basis Maske
3844 PRINT FNCUR$(3,31);"Mitgliedsnummer";HIGHON$;MTGLZAHL;HIGHOFF$
3845 GOSUB 11650                                 'Felder und Feldwerte ausgeben
3846 GOSUB 15860                                  'Drucker eingeschaltet? Frage
3847 IF JA=0 THEN 3849
3848   GOSUB 16550                        'Stammsatz drucken (Bildschirmformat)
3849 GOTO 3816

3867 '
3868 ' *** Stammsatz suchen (und drucken) von 05*************** ***************
3869 '
3870 DRUCKFLAG=0
3880 MT$="STAMMSATZ SUCHEN":IF DRUCKFLAG THEN MT$="SUCHEN & DRUCKEN"
3890 MI$="G-011050":IF DRUCKFLAG THEN MI$="G-013010"
3900 GOSUB 13130                                                   'Basis Maske
3910 FKEYS$="":GOSUB 13240                           'F-Tastenleiste einblenden
3920 TMP$="Suchen":IF DRUCKFLAG THEN TMP$="Drucken" 
3930 PRINT FNCUR$(4,30)"1 - ";TMP$;" nach Nummern"
3940 PRINT FNCUR$(6,30)"2 - ";TMP$;" nach Maske"
3950 FKEYMAX=4:NKEYMAX=2:GOSUB 13400                              'Menü-Eingabe
3960 IF FKEY<>1 THEN 3990 
3970   IF DRUCKFLAG THEN GOSUB 19610 ELSE GOSUB 18370
3980   GOTO 3880
3990 IF FKEY=2 THEN IF DRUCKFLAG THEN 6970 ELSE 2130
4000 IF FKEY=3 THEN 1960
4010 IF FKEY=4 THEN GOSUB 13560:GOTO 3950         'Rückkehr ins Hauptmenü-Frage
4020 ON NKEY GOTO 4060,4630 
4030 '
4040 ' *** Suchen nach Nummern
4050 '
4060 TMP$=STR$(1):NUMSUCH$(1)=RIGHT$(TMP$,LEN(TMP$)-1)
4070 TMP$=STR$(MTGLMAX):NUMSUCH$(2)=RIGHT$(TMP$,LEN(TMP$)-1)
4080 IF DRUCKFLAG=0 THEN MT$="STAMMSATZ SUCHEN NACH NUMMERN":MI$="G-011051"
4090 IF DRUCKFLAG=1 THEN MT$="SUCHEN & DRUCKEN NACH NUMMERN":MI$="G-013011"
4100 GOSUB 13130                                                   'Basis Maske
4110 FKEYS$="<F5>=Suchen":IF DRUCKFLAG THEN FKEYS$="<F5>=Suchen & Drucken"
4120 GOSUB 13240                                     'F-Tastenleiste einblenden
4130 TMP$="gesucht":IF DRUCKFLAG THEN TMP$="gesucht und gedruckt"     
4140 PRINT FNCUR$(4,25);"Gültige Mitgliedsnummern: 1 -";STR$(MTGLMAX)
4150 PRINT FNCUR$(6,13);"Ab welcher Mitgliedsnummer soll ";TMP$;" werden?     =>..."
4160 PRINT FNCUR$(8,13);"Bis zu welcher Mitgliedsnummer soll ";TMP$;" werden? =>..."
4170 PRINT FNCUR$(6,67);HIGHON$;NUMSUCH$(1);HIGHOFF$
4180 PRINT FNCUR$(8,67);HIGHON$;NUMSUCH$(2);HIGHOFF$
4190 F=1
4200 BADINPUT=0
4210 Z=NUMSUCHROW(F):S=67:LNG=3:TYP=2:MUSS=1
4220 FKEYMASK$="12345":CKEYMASK$="":INTEXT$=NUMSUCH$(F)
4230 GOSUB 10380                                                       'Input++
4240 IF FKEY<>1 THEN 4270
4250  IF DRUCKFLAG THEN GOSUB 19670 ELSE GOSUB 18620
4260  GOTO 4080
4270 IF FKEY=2 THEN 3880
4280 IF FKEY=3 THEN 1960
4290 IF FKEY=4 THEN GOSUB 13560:GOTO 4230         'Rückkehr ins Hauptmenü-Frage
4300 IF FKEY<>5 THEN 4350
4310   SEARCH=BYNUMS
4320   NUMSUCH1=VAL(NUMSUCH$(1))
4330   NUMSUCH2=VAL(NUMSUCH$(2))
4340   IF DRUCKFLAG THEN 7760 ELSE 4940                         'Reentry, Suche
4350 M=VAL(OUTTEXT$)                                             'Handle RETURN
4360 IF M>=1 AND M<=MTGLMAX THEN 4400
4370   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 12920         'Nachricht
4380   GOSUB 4540
4390   GOTO 4230
4400 NUMSUCH$(F)=OUTTEXT$
4410 NUMSUCH1=VAL(NUMSUCH$(1))
4420 NUMSUCH2=VAL(NUMSUCH$(2))
4430 IF NUMSUCH1<=NUMSUCH2 THEN 4470
4440   N$="Ungültiger Bereich":GOSUB 12920                           'Nachricht
4450   GOSUB 4540
4460   GOTO 4230
4470 F=F+1:IF F>NUMSUCHMAX THEN F=1
4480 FKEYMASK$="12345"
4490 FKEYS$="<F5>=Suchen":IF DRUCKFLAG THEN FKEYS$="<F5>=Suchen & Drucken" 'TODO Required?
4500 GOSUB 13500                                        'F-Tastenleiste löschen
4510 GOSUB 13240                                     'F-Tastenleiste einblenden
4520 GOTO 4210
4530 '
4540 BADINPUT=1                                  'Finish handling invalid input
4550 INTEXT$=OUTTEXT$
4560 FKEYMASK$="1234"
4570 GOSUB 13500                                        'F-Tastenleiste löschen
4580 FKEYS$="":GOSUB 13240                           'F-Tastenleiste einblenden
4590 RETURN
4600 '
4610 ' *** Suchen nach Maske
4620 '
4630 GOSUB 11170                                                'SUCH$() leeren
4640 IF DRUCKFLAG=0 THEN MT$="STAMMSATZ SUCHEN NACH MASKE":MI$="G-011052"
4650 IF DRUCKFLAG=1 THEN MT$="SUCHEN & DRUCKEN NACH MASKE":MI$="G-013012"
4660 GOSUB 13130                                                   'Basis Maske
4670 FKEYS$="<F5>=Suchen":IF DRUCKFLAG THEN FKEYS$="<F5>=Suchen & Drucken" 
4680 GOSUB 13240                                     'F-Tastenleiste einblenden
4690 GOSUB 11410                       'Feldnamen und Werteplatzhalter ausgeben
4700 F=1
4710 Z=FEROW(F):S=FECOL(F)+XOFF:LNG=FELNG(F):TYP=3
4720 FKEYMASK$="12345":CKEYMASK$="12":INTEXT$=""
4730 GOSUB 10370                                                         'Input
4740 IF FKEY<>1 THEN 4770 
4750   IF DRUCKFLAG THEN GOSUB 19750 ELSE GOSUB 18700
4760   GOTO 4640
4770 IF FKEY=2 THEN 3880
4780 IF FKEY=3 THEN 1960
4790 IF FKEY=4 THEN GOSUB 13560:GOTO 4730         'Rückkehr ins Hauptmenü-Frage
4800 IF FKEY<>5 THEN 4830 
4810   SEARCH=BYMASK                                                      '<F5>
4820   IF DRUCKFLAG THEN 7760 ELSE 4940                         'Reentry, Suche
4830 IF CKEY=0 THEN 4880                                    'Handle cursor keys
4840   PRINT FNCUR$(Z,S);HIGHON$;SUCH$(F);HIGHOFF$;STRING$(LNG-LEN(SUCH$(F)),".")
4850   IF CKEY=CKEYUP THEN F=F-1:IF F<1 THEN F=FELDMAX
4860   IF CKEY=CKEYDN THEN F=F+1:IF F>FELDMAX THEN F=1
4870   GOTO 4710
4880 SUCH$(F)=OUTTEXT$                                           'Handle RETURN
4890 F=F+1:IF F>FELDMAX THEN F=1
4900 GOTO 4710
4910 '
4920 ' *** Suche
4930 '
4940 GOSUB 13130                                                   'Basis Maske
4950 FKEYS$="<F5>=Suchen rückwärts <F6>=Suchen vorwärts <F7>=Adressaufkleber" 
4960 GOSUB 13240                                     'F-Tastenleiste einblenden
4970 PRINT FNCUR$(3,31);"Mitgliedsnummer"
4980 GOSUB 11410                       'Feldnamen und Werteplatzhalter ausgeben
4990 '
5000 IF SEARCH=BYMASK THEN 5050
5010 '
5020 MTGLZAHL=NUMSUCH1                      'Initialisierung Suche nach Nummern
5030 GOTO 5170
5040 '
5050 MTGLZAHL=0                               'Initialisierung Suche nach Maske
5060 GOSUB 8170                                     'next match IN/OUT:MTGLZAHL
5070 IF MTGLZAHL<>-1 THEN 5170
5080   N$="Keine Suchtreffer. Mit <F2> neue Suche.":GOSUB 12920      'Nachricht
5090   FKEYS$="":GOSUB 13240                         'F-Tastenleiste einblenden
5100   FKEYMASK$="1234":NKEYMASK="":GOSUB 13400
5110   IF FKEY<>1 THEN 5140
5120     IF DRUCKFLAG THEN GOSUB 19830 ELSE GOSUB 18770
5130     GOTO 4640
5140   IF FKEY=2 THEN 4640
5150   IF FKEY=3 THEN 1960
5160   IF FKEY=4 THEN GOSUB 13560:GOTO 5100       'Rückkehr ins Hauptmenü-Frage
5170 GOSUB 5620                              'Suchtreffer einlesen und anzeigen
5180 '
5190 FKEMAX=7:NKEYMAX=0:GOSUB 13400      'Menü-Eingabe 'TODO Nachricht löschen?
5200 ON FKEY GOTO 5220,5250,1960,5270,5300,5450,5680
5210 ' *** <F1> Hilfe
5220 IF DRUCKFLAG THEN GOSUB 19910 ELSE GOSUB 18850
5230 GOTO 5190 
5240 ' *** <F2> Zurück
5250 IF SEARCH=BYNUMS THEN 4060 ELSE 4640            'TODO BYNUMS:Initialwerte?
5260 ' *** <F4> Rückkehr ins Hauptmenü
5270 GOSUB 13560                                  'Rückkehr ins Hauptmenü-Frage
5280 GOTO 5190
5290 ' *** <F5> Suchen rückwärts
5300 IF SEARCH=BYMASK THEN 5360
5310 '
5320 MTGLZAHL=MTGLZAHL-1                          'Suchen rückwärts nach Nummer
5330 IF MTGLZAHL<NUMSUCH1 THEN 5420
5340   GOSUB 5620:GOTO 5190                  'Suchtreffer einlesen und anzeigen
5350 '       
5360 TMP=MTGLZAHL                                  'Suchen rückwärts nach Maske
5370 GOSUB 8090                                     'prev match IN/OUT:MTGLZAHL
5380 IF MTGLZAHL=-1 THEN 5410
5390   GOSUB 5620:GOTO 5190                  'Suchtreffer einlesen und anzeigen
5400 '
5410 MTGLZAHL=TMP                                               'No match found
5420 N$="Erster Suchtreffer":GOSUB 12920                             'Nachricht
5430 GOTO 5190
5440 ' *** <F6> Suchen vorwärts
5450 IF SEARCH=BYMASK THEN 5510
5460 '
5470 MTGLZAHL=MTGLZAHL+1                           'Suchen vorwärts nach Nummer
5480 IF MTGLZAHL>NUMSUCH2 THEN 5570
5490   GOSUB 5620:GOTO 5190                  'Suchtreffer einlesen und anzeigen
5500 '
5510 TMP=MTGLZAHL                                    'Suche vorwärts nach Maske
5520 GOSUB 8170                                     'next match IN/OUT:MTGLZAHL
5530 IF MTGLZAHL=-1 THEN 5560
5540   GOSUB 5620:GOTO 5190                  'Suchtreffer einlesen und anzeigen
5550 '
5560 MTGLZAHL=TMP                                               'No match found
5570 N$="Letzter Suchtreffer":GOSUB 12920                            'Nachricht
5580 GOTO 5190
5590 '
5600 ' *** Suchtreffer einlesen und anzeigen
5610 '
5620 GOSUB 13060                            'Nachricht löschen 'TODO notwendig?
5630 GOSUB 11830                                       'Stammsatzdaten einlesen
5640 GOSUB 11530                                            'Feldwerte ausgeben
5650 PRINT FNCUR$(3,46);HIGHON$;STR$(MTGLZAHL);HIGHOFF$
5660 RETURN
5670 ' *** <F7> Adressaufkleber drucken
5680 GOSUB 15860                          'Ist der Drucker eingeschaltet? Frage
5690 IF JA=0 THEN 5740                              'LW TODO F-Keys einblenden?
5700   N$="Adressaufkleber wird gedruckt...":GOSUB 12920             'Nachricht
5710   GOSUB 16370                                     'Adressaufkleber drucken
5720   GOSUB 13060                                           'Nachricht löschen
5730   GOSUB 13240                                           'F-keys einblenden
5740 GOTO 5190                                                    'Neue Eingabe
6120 '
6130 ' ######################### Verwendungsnachweis ##########################
6140 '
6150 GOSUB 11230                                              'Einlesen MTGLMAX
6160 VNFLAG=-1
6170 '
6180 ' Verwendungsnachweis-Menü ***********************************************
6190 '
6200 MT$="VERWENDUNGSNACHWEIS":MI$="G-012000":GOSUB 13130          'Basis Maske
6210 F=4                                                                'TODO ?
6220 FKEYS$="":GOSUB 13240                           'F-Tastenleiste einblenden
6230 PRINT CURON$;
6240 PRINT HIGHOFF$;
6250 PRINT FNCUR$(4,20)"   Bitte geben Sie die Mitgliedsnummer"
6260 PRINT FNCUR$(5,20)"des zu ändernden Verwendungsnachweises ein"
6261 'LW PRINT FNCUR$(6,23);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
6270 PRINT FNCUR$(6,20)" und beenden Sie die Eingabe mit RETURN."
6280 PRINT FNCUR$(8,20)"                 =>..."
6290 Z=8:S=39:LNG=3:TYP=2
6300 GOSUB 10370                                                         'INPUT
6310 IF FKEY=1 THEN GOSUB 19060:GOTO 6200
6320 IF FKEY=2 OR FKEY=3 THEN GOSUB 11120:VNFLAG=0:GOTO 1960     'DAT$() leeren
6330 IF FKEY=4 THEN GOSUB 13560
6340 IF FKEY>4 THEN 6290
6350 IF CKEY<>0 THEN 6290
6360 IF LEN(OUTTEXT$)=0 THEN N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 12920:GOTO 6280 ELSE MITGLIED=VAL(OUTTEXT$):IF MITGLIED=0 THEN 6300 'Nachricht
6370 IF MITGLIED>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12920:PRINT HOME$;HIGHOFF$:GOTO 6280 'Nachricht
6380 MTGLZAHL=MITGLIED:MTGL$=STR$(MITGLIED)
6390 '
6400 ' *** Einlesen des Mitgliedsnamens ***************************************
6410 '
6420 PRINT FNCUR$(21,1);DELLINE$;
6430 GET #2,MITGLIED                                        'EINLESEN DATEN STS
6440 NNAM$=LEFT$(DD1$,25)                                 'NACHNAME RAUSFISCHEN
6450 VNAM$=MID$(DD1$,26,25)                               'VORNAME  RAUSFISCHEN
6460 '
6470 ' ** LEERZEICHEN HINTER NACHNAME ABSCHNEIDEN *****************************
6480 '
6490 FOR I=LEN(NNAM$) TO 1 STEP -1
6500   IF MID$(NNAM$,I,1)<>" " THEN NNAM$=LEFT$(NNAM$,I):I=1
6510 NEXT
6520 NAMM$=NNAM$+" "+VNAM$
6530 GOSUB 14080                                          'EINLESEN 1. VN-BLATT
6540 BLATT=1
6550 VP=1:HP=1                                                 'INITIALISIERUNG
6560 F=8
6570 GOSUB 13130                                                   'Basis Maske
6580 FKEYS$="<F7>=Speichern":GOSUB 13240             'F-Tastenleiste einblenden
6590 PRINT HIGHOFF$;
6600 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=Vorwärts";TAB(53);"<F9>=Blatt drucken";
6610 PRINT FNCUR$(3,2);"Blatt:"
6620 PRINT FNCUR$(5,2);"Mitglieds-Nr.:"
6630 PRINT FNCUR$(5,24);"Name:"
6640 MSK$="Von:  .  .   Bis:  .  .   Amt:"
6650 FOR KK=7 TO 19 
6660   PRINT FNCUR$(KK,2);MSK$;TAB(43);MSK$                    'LW NO semicolon
6670 NEXT
6680 GOSUB 15290                                                 'Ausgabe Daten
6690 LNG=2-(HP=7)*6                                    'BEGIN OF THE INPUT-LOOP
6700 TYP=2-(HP=7)
6710 Z=VP+13*(VP>13)+6
6720 S=6-39*(VP>13)-(HP<7)*(HP-1)*3+(HP>3)*(HP<7)*4-(HP=7)*26
6730 GOSUB 13060                                             'Nachricht löschen
6740 GOSUB 10370                                                       'INPUT+1
6750 IF HP<7 THEN IF ASCC=ASCRET THEN OUTTEXT$=STRING$(2-LEN(OUTTEXT$),"0")+OUTTEXT$   '+NULLEN
6760 IF CKEY<>0 OR ASCC=ASCRET THEN PRINT FNCUR$(Z,S);OUTTEXT$
6770 IF ASCC=ASCRET THEN IF HP<7 THEN MID$(DAT$(VP),-1+2*HP,2)=OUTTEXT$ ELSE DAT$(VP)=LEFT$(DAT$(VP),12)+OUTTEXT$+SPACE$(8-LEN(OUTTEXT$))
6780 IF ASCC=ASCRET AND HP<7 THEN JA=1:TEST$=OUTTEXT$:ON HP GOSUB 12680,12710,12740,12680,12710,12740:IF JA=0 THEN N$="Ungültiges Datum":GOSUB 12920:GOTO 6740 'Datums-Check 'Nachricht
6790 IF ASCC=ASCRET THEN IF HP=7 THEN HP=1:VP=VP-(VP<26)-(VP=26)*(-25):GOTO 6690 ELSE HP=HP+1:GOTO 6690     'NEUE SCREEN-POSITION
6800 IF CKEY<>0 THEN TEST$=MID$(DAT$(VP),-(HP<7)*2*HP-1-(HP=7)*14,-(HP<7)*2-(HP=7)*8):PRINT FNCUR$(Z,S);TEST$
6810 IF CKEY<>0 THEN IF HP<7 AND NOT TEST$="  " THEN JA=1:ON HP GOSUB 12680,12710,12740,12680,12710,12740:IF JA=0 THEN N$="Ungültiges Datum":GOSUB 12920:GOTO 6740 'Nachricht
6820 IF CKEY=CKEYDN THEN VP=VP-(VP<26)-(VP=26)*(-25):GOTO 6690
6830 IF CKEY=CKEYUP THEN VP=VP+(VP>1)-(VP=1)*25     :GOTO 6690
6840 IF CKEY=CKEYRT THEN HP=HP-(HP<7)-(HP=7)*(-6)   :GOTO 6690
6850 IF CKEY=CKEYLT THEN HP=HP+(HP>1)+(HP=1)*(-6)   :GOTO 6690
6860 IF FKEY=1 THEN GOSUB 19170 :GOTO 6550           'HILFE - ZU INIT UND MASKE
6870 IF FKEY=2 THEN 6200                                       'ZU MTGL-EINGABE
6880 IF FKEY=3 THEN VNFLAG=0:GOSUB 11120:GOTO 1960               'DAT$() leeren
6890 IF FKEY=4 THEN GOSUB 13560:GOSUB 13300:GOTO 6740 
6900 IF FKEY=5 THEN GOSUB 14170:GOTO 6690
6910 IF FKEY=6 THEN GOSUB 14310:GOTO 6690
6920 IF FKEY=7 THEN GOSUB 14950:GOTO 6740                 'SPEICHERN DANN INPUT
6930 IF FKEY=9 THEN GOSUB 15530:GOSUB 14500:GOTO 6730
6940 '
6950 ' ############# Mitgliedsliste drucken von 05*************** #############
6960 '
6970 MT$="MITGLIEDSLISTE DRUCKEN":MI$="G-013000":GOSUB 13130       'Basis Maske
6980 FKEYS$="":GOSUB 13240                           'F-Tastenleiste einblenden
6990 PRINT FNCUR$(4,28);"1 - Suchen & Drucken"
7000 PRINT FNCUR$(6,28);"2 - Komplett drucken"
7010 PRINT FNCUR$(8,28);"3 - Datenblatt drucken"
7020 FKEYMAX=4:NKEYMAX=3:GOSUB 13400                              'Menü-Eingabe
7030 IF FKEY=1 THEN GOSUB 19360:GOTO 6970
7040 IF FKEY=2 THEN 1960
7050 IF FKEY=3 THEN 1960
7060 IF FKEY=4 THEN GOSUB 13560:GOTO 7020
7070 ON NKEY GOTO 7110,7110,8460
7080 '
7090 ' *** Suchen & Drucken, Komplett drucken *********************************
7100 '
7110 IF NKEY=1 THEN MT$="SUCHEN & DRUCKEN":MI$="G-013010"
7120 IF NKEY=2 THEN MT$="KOMPLETT DRUCKEN":MI$="G-013020"
7130 GOSUB 13130                                                   'Basis Maske
7140 GOSUB 13240                                     'F-Tastenleiste einblenden
7150 PRINT FNCUR$(4,21);"Bitte geben Sie das heutige Datum ein."
7160 PRINT FNCUR$(5,22);"Beenden Sie die Eingabe mit RETURN."
7170 PRINT FNCUR$(7,35);"=>........"
7180 BADINPUT=0
7190 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
7200 Z=7:S=37:LNG=8:TYP=5:MUSS=1
7210 GOSUB 10380                                                       'Input++
7220 IF FKEY=1 THEN GOSUB 19990:GOTO 7110
7230 IF FKEY=2 THEN 6970                                                'F-Keys
7240 IF FKEY=3 THEN 1960
7250 IF FKEY=4 THEN GOSUB 13560:GOTO 7210         'Rückkehr ins Hauptmenü-Frage
7260 '
7270 POS1=INSTR(OUTTEXT$,".")                                    'Handle RETURN
7280 POS2=INSTR(POS1+1,OUTTEXT$,".")
7290 IF POS2=0 THEN 7530
7300 POS3=INSTR(POS2+1,OUTTEXT$,".")
7310 IF POS3<>0 THEN 7530
7320 D1$=LEFT$(OUTTEXT$,POS1-1)
7330 IF LEN(D1$)=0 OR LEN(D1$)>2 THEN 7530
7340 D2$=MID$(OUTTEXT$,POS1+1,POS2-POS1-1)
7350 IF LEN(D2$)=0 OR LEN(D2$)>2 THEN 7530
7360 D3$=RIGHT$(OUTTEXT$,LEN(OUTTEXT$)-POS2)
7370 IF LEN(D3$)=0 OR LEN(D3$)>2 THEN 7530
7380 '
7390 D1=VAL(D1$):IF D1<1 OR D1>31 THEN 7550
7400 D2=VAL(D2$):IF D2<1 OR D2>12 THEN 7550
7410 D3=VAL(D3$) 
7420 ' TODO: Deeper validation
7430 D1$=STR$(D1):D1$=MID$(D1$,2,99):IF D1<10 THEN D1$="0"+D1$
7440 D2$=STR$(D2):D2$=MID$(D2$,2,99):IF D2<10 THEN D2$="0"+D2$
7450 D3$=STR$(D3):D3$=MID$(D3$,2,99):IF D3<10 THEN D3$="0"+D3$
7460 DATUM$=D1$+"."+D2$+"."+D3$
7470 '
7480 PRINT FNCUR$(7,37);HIGHON$;DATUM$;HIGHOFF$
7490 GOTO 7600
7500 '
7510 ' *** Fehlerbehandlung Datum
7520 '
7530 N$="Datum muss Format TT.MM.JJ haben. Bitte korrigieren.":GOSUB 12920 'Nachricht
7540 GOTO 7560
7550 N$="Ungültiges Datum. Bitte korrigieren.":GOSUB 12920           'Nachricht
7560 BADINPUT=1
7570 INTEXT$=OUTTEXT$                                                      '???
7580 GOTO 7200
7590 '
7600 IF NKEY=1 THEN 7700
7610 '
7620 ' *** Komplette Mitgliedliste drucken
7630   SEARCH=BYNUMS                          'Drucken komplette Mitgliedsliste
7640   NUMSUCH1=1:NUMSUCH2=MTGLMAX
7650   GOSUB 15860                              '"Drucker eingeschaltet?"-Frage
7660   IF JA=0 THEN 7110
7670   GOTO 7780                                           'LW OK-ish target...
7680 '
7690 ' *** Suchen nach Nummern und Maske
7700 DRUCKFLAG=1
7710 GOTO 3880                                  'Stammsatz suchen (und drucken)
7720 ' ---> Reentry <----------------------------------------------------------
7730 ' IN: SEARCH (BYNUMS, BYMASK)
7740 ' IN: NUMSUCH1, NUMSUCH2
7750 ' IN: SUCH$()
7760 GOSUB 15860                                '"Drucker eingeschaltet?"-Frage
7770 IF JA=0 THEN 8050
7780 N$="Mitgliedsliste wird gedruckt... Abbrechen mit Taste.":GOSUB 12920 'Nachricht
7790 GOSUB 15930                                 'Mitgliedsliste drucken (Kopf)
7800 IF SEARCH=BYMASK THEN 7930
7810 '
7820 ' *** Suchen nach Nummern
7830 FOR MTGLZAHL=NUMSUCH1 TO NUMSUCH2
7840   GOSUB 11830                                     'Stammsatzdaten einlesen
7850   IF INSTR(DAT$(5),"ausgetreten")>0 THEN 7870
7860     GOSUB 16070                         'Mitgliedsliste drucken (Mitglied)
7870   GOSUB 16280                                      'Druck abbrechen? Frage
7880   IF JA=1 THEN 8040
7890 NEXT MTGLZAHL
7900 GOTO 8040
7910 ' 
7920 ' *** Suchen nach Maske
7930 MTGLZAHL=0
7940 GOSUB 8170                                     'next match IN/OUT:MTGLZAHL
7950 IF MTGLZAHL=-1 THEN 8040
7960   GOSUB 11830            'Stammsatzdaten einlesen, IN:MTGLZAHL, OUT DAT$()
7970   IF INSTR(DAT$(5),"ausgetreten")>0 THEN 7990
7980     GOSUB 16070                         'Mitgliedsliste drucken (Mitglied)
7990   GOSUB 16280                                      'Druck abbrechen? Frage
8000   IF JA=1 THEN 8040
8010 GOTO 7950
8020 '
8030 ' *** Suchen zuende
8040 GOSUB 13060                                             'Nachricht löschen
8050 IF SEARCH=BYNUMS THEN 4060 ELSE 4630                  'LW Feels hackish...
8060 '
8070 ' *** Subroutine Prev match 
8080 ' IN: MTGLZAHL OUT: MTGLZAHL or -1 if not found
8090 MTGLZAHL=MTGLZAHL-1
8100 IF MTGLZAHL<1 THEN 8230
8110 GOSUB 8270
8120 IF NOT ISMATCH THEN 8090 
8130 RETURN
8140 '
8150 ' *** Subroutine Next match 
8160 ' IN: MTGLZAHL OUT: MTGLZAHL or -1 if not found
8170 MTGLZAHL=MTGLZAHL+1
8180 IF MTGLZAHL>MTGLMAX THEN 8230
8190 GOSUB 8270
8200 IF NOT ISMATCH THEN 8170 
8210 RETURN
8220 '
8230 MTGLZAHL=-1
8240 RETURN
8250 '
8260 ' *** Subroutine Is match? 
8270 GOSUB 11830                                                   'Daten lesen
8280 FOR K=1 TO FELDMAX
8290   IF LEFT$(SUCH$(K),1)<>"*" THEN 8320
8300     IF INSTR(DAT$(K),RIGHT$(SUCH$(K),LEN(SUCH$(K))-1))>0 THEN 8370
8310     GOTO 8410
8320   I=1
8330   IF MID$(SUCH$(K),I,1)="?" THEN I=I+1:GOTO 8330
8340   I=I-1
8350   IF INSTR(DAT$(K),RIGHT$(SUCH$(K),LEN(SUCH$(K))-I))=I+1 THEN 8370
8360   GOTO 8410
8370 NEXT K
8380 ISMATCH=1
8390 RETURN
8400 '
8410 ISMATCH=0
8420 RETURN
8430 '
8440 ' *** Datenblatt drucken *************************************************
8450 '
8460 MT$="DATENBLATT DRUCKEN":MI$="G-013030":GOSUB 13130           'Basis Maske
8470 FKEYS$="":GOSUB 13240                           'F-Tastenleiste einblenden
8480 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer"
8490 PRINT FNCUR$(5,23)"des zu druckenden Datenblattes ein."
8500 PRINT FNCUR$(7,23)"Beenden Sie die Eingabe mit RETURN."
8501 PRINT FNCUR$(8,23);"Gültige Mitgliedsnummern sind 1 -";STR$(MTGLMAX);"."
8510 PRINT FNCUR$(10,39)"=>..."
8520 BADINPUT=0
8530 FKEYMASK$="1234":CKEYMASK$="":INTEXT$=""
8540 Z=10:S=41:LNG=3:TYP=2:MUSS=1
8550 GOSUB 10380                                                       'Input++
8560 IF FKEY=1 THEN GOSUB 20070:GOTO 8460
8570 IF FKEY=2 THEN 6970                                                'F-Keys
8580 IF FKEY=3 THEN 1960
8590 IF FKEY=4 THEN GOSUB 13560:GOTO 8550         'Rückkehr ins Hauptmenü-Frage
8600 '
8610 M=VAL(OUTTEXT$)                                             'Handle RETURN
8620 IF M>=1 AND M<=MTGLMAX THEN 8670
8630   N$="Diese Mitgliedsnummer gibt es nicht.":GOSUB 12920         'Nachricht
8640   BADINPUT=1
8650   INTEXT$=OUTTEXT$                                                    '???
8660   GOTO 8540
8670 MTGLZAHL=M
8680 GOSUB 15860                                '"Drucker eingeschaltet?"-Frage
8690 IF JA=1 THEN 8730
8700   GOSUB 13240                                   'F-Tastenleiste einblenden
8710   INTEXT$=OUTTEXT$
8720   GOTO 8540
8730 N$="Datenblatt drucken...":GOSUB 13000       'Nachricht. F-keys ausblenden
8740 GOSUB 11830                                          'Stammsatzdaten lesen
8750 NAMM$ = DAT$(1)+" "+DAT$(2)
8760 GOSUB 16550                          'Stammsatz drucken (Bildschirmformat)
8770 GOSUB 14080                             'Verwendungsnachweisdaten einlesen
8780 GOSUB 15560                              'Verwendungsnachweisdaten drucken
8790 GOSUB 16820                                     'Mitgliedsvermerke drucken
8800 GOSUB 13060                                             'Nachricht löschen
8810 GOTO 8460
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
10940 ' *** Einlesen Stammsatzfelder-Definition ********************** REVIEWED
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
11200 ' *** MTGLMAX *lesen ******************************************* REVIEWED
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
12370 VALID=0: IF (ASCC>57 AND ASCC<127) OR (ASCC>31 AND ASCC<48) THEN VALID=1
12400 RETURN
12410 '
12420 ' *** Testbereich 2 (Numerisch) *****************************************
12430 '
12440 VALID=0: IF ASCC>47 AND ASCC<58 THEN VALID=1
12460 RETURN
12470 '
12480 ' *** Testbereich 3 (Alphanumerisch) ************************************
12490 '
12500 VALID=0: IF ASCC>31 AND ASCC<127 THEN VALID=1
12520 RETURN
12530 '
12540 ' *** Testbereich 4 (Numerisch + "/" und ".") ***************************
12550 '
12560 VALID=0: IF ASCC>45 AND ASCC<58 THEN VALID=1
12580 RETURN
12590 '
12600 ' *** Testbereich 5 (Numerisch + und ".") *******************************
12610 '
12620 VALID=0: IF (ASCC>=48 AND ASCC<=57) OR ASCC=46 THEN VALID=1
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
13150 PRINT FNCUR$(1,(80-LEN(MT$))/2+1);MT$;FNCUR$(1,72);MI$
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
13570 IF JA=1 THEN 20280                                          'Programmende
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
17210 ' *** Hilfe-Text "Mitgliederverwaltung" G-010000
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
17430 ' *** Hilfe-Text "Stammsatzpflege" G-011000
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
17680 ' *** Hilfe-Text "Stammsatz anlegen" G-011010
17690 '
17700 MT$="HILFE":GOSUB 13130                                      'Basis Maske
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
17880 RETURN
17890 '
17900 ' *** Hilfe-Text "Stammsatz ändern (I)" G-011020
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
18010 ' *** Hilfe-Text "Stammsatz ändern (II)" G-011020
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
18190 RETURN
18200 '
18210 ' *** Hilfe-Text "Stammsatz löschen" G-011030
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
18350 ' *** Hilfe-Text "Stammsatz suchen (I)" G-011050
18360 '
18370 MT$="HILFE":GOSUB 13130                                      'Basis Maske
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
18600 ' *** Hilfe-Text "Stammsatz suchen nach Nummern" G-011051
18610 '
18620 MT$="HILFE":GOSUB 13130                                      'Basis Maske
18630 PRINT HOME$
18640 PRINT IN$;"Hilfe-Text *Stammsatz suchen nach Nummern* G-011051"
18650 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
18660 RETURN
18670 '
18680 ' *** Hilfe-Text "Stammsatz suchen nach Maske" G-011052
18690 '
18700 MT$="HILFE":GOSUB 13130                                      'Basis Maske
18710 PRINT HOME$
18720 PRINT IN$;"Hilfe-Text *Stammsatz suchen nach Maske* G-011052"
18730 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
18740 RETURN
18750 '
18760 ' *** Hilfe-Text "Stammsatz suchen (II)"  G-011050
18770 '
18780 MT$="HILFE":GOSUB 13130                                      'Basis Maske
18790 PRINT HOME$
18800 PRINT IN$;"Hilfe-Text *Stammsatz suchen (II)* G-011050 Keine Suchtreffer"
18810 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
18820 RETURN
18830 '
18840 ' *** Hilfe-Text "Stammsatz suchen (III)" G-011050
18850 '
18860 MT$="HILFE":GOSUB 13130                                      'Basis Maske
18870 PRINT HOME$
18880 PRINT IN$;"Hilfe-Text *Stammsatz suchen (III)* G-011050 Blättern"
18890 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
18900 RETURN
18910 ' 
18920 ' *** Hilfe-Text für "Stammsatz drucken" G-011040
18930 '
18940 SAVEMT$=MT$:MT$="HILFE":GOSUB 13130:MT$=SAVEMT$              'Basis Maske
18950 PRINT HOME$
18960 PRINT IN$;"Sie können jetzt einen kompletten Stammsatz drucken, so, wie"
18970 PRINT IN$;"er auf dem Bildschirm steht."
18980 PRINT
18990 PRINT IN$;"Geben Sie die Mitgliedsnummer des Stammsatzes ein."
19000 PRINT IN$;"Bei Tippfehlern können Sie mit der DELETE-Taste korrigieren."
19010 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
19020 RETURN
19030 '
19040 ' *** Hilfe-Text "Verwendungsnachweis (I)" G-012000
19050 '
19060 MT$="HILFE":GOSUB 13130                                      'Basis Maske
19070 PRINT HOME$
19080 PRINT IN$;"Sie können jetzt einen Verwendungsnachweis ändern."
19090 PRINT
19100 PRINT IN$;"Geben Sie die Mitgliedsnummer des Verwendungsnachweises ein."
19110 PRINT IN$;"Bei Tippfehlern können Sie mit der DELETE-Taste korrigieren."
19120 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
19130 RETURN
19140 '
19150 ' *** Hilfe-Text "Verwendungsnachweis (II)" G-012000
19160 '
19170 SAVEMT$=MT$:MT$="HILFE":GOSUB 13130:MT$=SAVEMT$              'Basis Maske
19180 PRINT HOME$
19190 PRINT IN$;"Sie können jetzt einen bestehenden Verwendungsnachweis ändern."
19200 PRINT
19210 PRINT IN$;"Während der Dateneingabe können Sie mit der DELETE-Taste"
19220 PRINT IN$;"korrigieren. Ist ein Feld ausgefüllt worden, muß RETURN"
19230 PRINT IN$;"gedrückt werden, da sonst die Änderung nicht gespeichert"
19240 PRINT IN$;"wird. Mit den Cursortasten kann das Feld gewechselt werden."
19250 PRINT
19260 PRINT IN$;"Weitere Funktionstasten:"
19270 PRINT IN$;"<F5>  Zurückblättern (ohne die jetzige Seite zu speichern)"
19280 PRINT IN$;"<F6>  Vorblättern (ohne die jetzige Seite zu speichern)"
19290 PRINT IN$;"<F7>  Aktuelle Seite speichern"
19300 PRINT IN$;"<F9>  Aktuelle Seite drucken"
19310 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
19320 RETURN
19330 '
19340 ' *** Hilfe-Text "Mitgliedsliste drucken" G-013000
19350 '
19360 MT$="HILFE":GOSUB 13130                                      'Basis Maske
19370 PRINT HOME$
19380 PRINT IN$;"Sie können jetzt eine Mitgliedsliste drucken." 
19390 PRINT
19400 PRINT IN$;"1 - Suchen & Drucken"
19410 PRINT IN$;"    Sie können Stammsätze nach verschiedenen Gesichtspunkten"
19420 PRINT IN$;"    suchen und drucken."
19430 PRINT
19440 PRINT IN$;"2 - Komplett drucken"
19450 PRINT IN$;"    Es wird eine komplette Mitgliedsliste ausgedruckt."
19460 PRINT IN$;"    Ausgetretene Mitglieder werden nicht berücksichtigt."
19470 PRINT
19480 PRINT IN$;"3 - Datenblatt drucken"
19490 PRINT IN$;"    Der Stammsatz und der Verwendungsnachweis werden im"
19500 PRINT IN$;"    Bildschirmformat mit einer Liste von Mitgliedsvermerken"
19510 PRINT IN$;"    ausgedruckt."
19520 PRINT
19530 PRINT IN$;"Wählen Sie aus, indem Sie die zugehörige Zahlentaste drücken."
19540 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
19550 RETURN
19560 '
19570 ' *** Hilfe-Text "Suchen & Drucken (I)" G-013010
19580 '
19590 MT$="HILFE":GOSUB 13130                                      'Basis Maske
19600 PRINT HOME$
19610 PRINT IN$;"Hilfe-Text *Suchen & Drucken (I)* G-013010" 
19620 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
19630 RETURN
19640 '
19650 ' *** Hilfe-Text "Suchen & Drucken nach Nummern" G-013011
19660 '
19670 MT$="HILFE":GOSUB 13130                                      'Basis Maske
19680 PRINT HOME$
19690 PRINT IN$;"Hilfe-Text *Suchen & Drucken nach Nummern* G-013011"
19700 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
19710 RETURN
19720 '
19730 ' *** Hilfe-Text "Suchen & Drucken nach Maske" G-013012
19740 '
19750 MT$="HILFE":GOSUB 13130                                      'Basis Maske
19760 PRINT HOME$
19770 PRINT IN$;"Hilfe-Text *Suchen & Drucken nach Maske* G-013012"
19780 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
19790 RETURN
19800 '
19810 ' *** Hilfe-Text "Suchen & Drucken (II)" G-013010
19820 '
19830 MT$="HILFE":GOSUB 13130                                      'Basis Maske
19840 PRINT HOME$
19850 PRINT IN$;"Hilfe-Text *Suchen & Drucken (II)* G-013010 Keine Suchtreffer"
19860 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
19870 RETURN
19880 '
19890 ' *** Hilfe-Text "Suchen & Drucken (III)" G-013010
19900 '
19910 MT$="HILFE":GOSUB 13130                                      'Basis Maske
19920 PRINT HOME$
19930 PRINT IN$;"Hilfe-Text *Suchen & Drucken (III)* G-013010 Blättern"
19940 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
19950 RETURN
19960 '
19970 ' *** Hilfe-Text "Suchen & Drucken (IB), Komplettdruck Datum" G-013010, G-013020
19980 '
19990 SAVEMT$=MT$:MT$="HILFE":GOSUB 13130:MT$=SAVEMT$              'Basis Maske
20000 PRINT HOME$
20010 PRINT IN$;"Hilfe-Text *Suchen & Drucken (IB)* Komplettdruck Datum G-013010, G-013020"
20020 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
20030 RETURN
20040 '
20050 ' *** Hilfe-Text "Datenblatt drucken" G-013030
20060 '
20070 MT$="HILFE":GOSUB 13130                                      'Basis Maske
20080 PRINT HOME$
20090 PRINT IN$;"Hilfe-Text *Datenblatt drucken* G-013030
20100 GOSUB 13700                                      'Rückkehr aus Hilfe-Text
20110 RETURN
20120 '
20130 ' *** Programmfehler ******************************************* REVIEWED
20140 '
20150 LIND$=SPACE$(15)
20160 GOSUB 13130                                                  'Basis Maske
20170 PRINT HOME$
20180 PRINT LIND$;"Ein Programmfehler ist aufgetreten."
20190 PRINT   
20200 PRINT LIND$;"- Notieren Sie folgende Fehlerinformation:"
20210 PRINT LIND$;"    ";HIGHON$;"ERL";HIGHOFF$;" (Error Line)";HIGHON$;ERL;HIGHOFF$
20220 PRINT LIND$;"    ";HIGHON$;"ERR";HIGHOFF$;" (Error Code)";HIGHON$;ERR;HIGHOFF$
20230 PRINT LIND$;"- Drücken Sie am Rechner den Knopf hinten rechts."
20240 PRINT LIND$;"19**************************"
20250 '
20260 ' *** Programmende ********************************************* REVIEWED
20270 '
20280 PRINT FNCUR$(22,1);HIGHOFF$;CURON$;
20290 RESET
20300 'LW RUN "DISKMENU.EXE"
20310 END                                                           '1799 lines
