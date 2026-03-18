1000 ' RELEASE 01***** Neubearbeitung durch 04**********
1010 '
1020 ' LW 31-JAN-2026: Load MSBASIC.COM with /S:560 /F:5
1030 '
1040 ' *** Initialisierung, Dimensionierung ***********************************
1050 '
1060 DEFINT A-Z                                              'Integerdefinition
1070 DIM FELD$(27),SUCH$(27),DAT$(27)                    'Arrays dimensionieren
1080 BSP$    =CHR$(8)                                                'Backspace
1090 ESC$    =CHR$(27)                                'Einleitung Escapesequenz
1100 CLS$    =ESC$+"E"                                      'Bildschirm löschen
1110 INVON$  =ESC$+"p"                                              'Invers ein
1120 INVOFF$ =ESC$+"q"                                              'Invers aus
1130 CURSON$ =ESC$+"y5"                                             'Cursor ein
1140 CURSOFF$=ESC$+"x5"                                             'Cursor aus
1150 HIGHON$ =ESC$+"("                                           'Helle Schrift
1160 HIGHOFF$=ESC$+")"                                          'Dunkle Schrift
1170 DELLINE$=ESC$+"K"                                           'Zeile löschen
1180 DELHIGH$=ESC$+"+"                                   'Helle Schrift löschen
1190 STRICH$ =STRING$(79,"-")                                           'Strich
1200 DEF FNCUR$(Z,S)=ESC$+"Y"+CHR$(31+Z)+CHR$(31+S)       'Cursor positionieren
1210 DEF FNROW(F$)=VAL(MID$(F$,17,2))                     'Zeile des Feldnamens
1220 DEF FNCOL(F$)=VAL(MID$(F$,15,2))                    'Spalte des Feldnamens
1230 DEF FNLNG(F$)=VAL(MID$(F$,19,2))                         'Länge des Feldes
1240 DEF FNTYP(F$)=VAL(RIGHT$(F$,1))                     'Gültigkeits-Test Flag
1250 HOME$=FNCUR$(3,1)                                    'Cursor Home Position
1260 IN$=STRING$(10," ")                                 'Indent für Hilfe-Texte
1270 ' 
1280 ON ERROR GOTO 18050                          'Program Error Trap Nachricht
1290 '
1300 GOSUB 10050    'Tasten deaktivieren, ASCII Codes für Cursortasten vergeben
1310 '
1320 ' *** Beginn des Mitgliederverwaltungsprogramms **************************
1330 '
1340 MT$="MITGLIEDERVERWALTUNG":MI$="G-01000":GOSUB 12090          'Basis Maske
1350 Q$="Befindet sich die Daten-Diskette im Laufwerk B?":GOSUB 11310 'Ja/Nein-Frage
1360 IF JA=1 THEN 1420
1370 Q$="Bitte legen Sie sie ein. Fertig?":GOSUB 11310           'Ja/Nein-Frage
1380 IF JA=0 THEN RUN "DISKMENU.EXE"
1390 '
1400 ' ################# Mitgliederverwaltungsmenü (MVW-Menü) #################
1410 '
1420 GOSUB 11010                                   'Fielden der Stammsatz-Datei
1430 GOSUB 12780                       'Fielden der Verwendungsnachweis-Dateien
1440 '
1450 ' Einlesen Stammdatenfelder
1460 OPEN "I",#1,"B:FELD.DAT"                        
1470 INPUT #1,FELDMAX
1480 FOR I=1 TO FELDMAX
1490   INPUT #1,FELD$(I)
1500 NEXT
1510 CLOSE #1
1520 '
1530 MT$="MITGLIEDERVERWALTUNG":MI$="G-01000":GOSUB 12090          'Basis Maske
1540 FKEYS$="":GOSUB 12210                                      'F-Tastenleiste
1550 VNFLAG=0                                                              '???
1560 PRINT FNCUR$(4,26);"1 - Stammsatzpflege"                         'Menütext
1570 PRINT FNCUR$(6,26);"2 - Verwendungsnachweis"
1580 PRINT FNCUR$(8,26);"3 - Mitgliedsliste drucken"
1590 NKEYMAX=3:GOSUB 12370                                         'Menüeingabe
1600 DRUCKFLAG=0                                                           '???
1610 IF NKEY THEN ON NKEY GOTO 1700,4390,5220
1620 ON FKEY GOTO 1630,1640,1530,1640         'Hilfe, Zurück, Anfang, Hauptmenü
1630 GOSUB 15960:GOTO 1530                                      'F1->Hilfe-Text
1640 GOSUB 12480:GOTO 1530                           'F2->Zurück, F4->Hauptmenü
1650 '
1660 DEL$=BSP$+"."+BSP$                 'Zeichen links löschen 'TODO ever used?
1670 '
1680 ' ########################### Stammsatzpflege ############################
1690 '
1700 GOSUB 10570                                              'Einlesen MTGLMAX
1710 IF DRUCKFLAG THEN RETURN                                              '???
1720 '
1730 ' *** Stammsatzpflege-Menü ***********************************************
1740 '
1750 MT$="STAMMSATZPFLEGE":MI$="G-01100":GOSUB 12090               'Basis Maske
1760 FKEYS$="":GOSUB 12210                                      'F-Tastenleiste
1770 PRINT FNCUR$(4,29);"1 - Stammsatz anlegen"
1780 PRINT FNCUR$(6,29);"2 - Stammsatz ändern"
1790 PRINT FNCUR$(8,29);"3 - Stammsatz löschen"
1800 PRINT FNCUR$(10,29);"4 - Stammsatz drucken"
1810 PRINT FNCUR$(12,29);"5 - Stammsatz suchen"
1820 NKEYMAX=5:GOSUB 12370                                         'Menüeingabe
1830 IF NKEY THEN GOSUB 10480:ON NKEY GOTO 1910,2200,2670,4120,2960 'Variablen Init: Read MTGLZAHL, clear DAT$(), SUCH$()
1840 ON FKEY GOTO 1850,1860,1860,1870         'Hilfe, Zurück, Anfang, Hauptmenü
1850 GOSUB 16180:GOTO 1750                                      'F1->Hilfe-Text
1860 GOSUB 12610:GOTO 1530                      'F2,F3->Clear DAT$(),->MVW-Menü  
1870 GOSUB 12480:GOTO 1750                                       'F4->Hauptmenü
1880 '
1890 ' *** Stammsatz erfassen von 02************ ******************************
1900 '
1910 MT$="STAMMSATZ ANLEGEN":MI$="G-01101":PRINT CURSON$:DATENAUSG=1:F=1
1920 MTGLZAHL=MTGLMAX
1930 MTGLZAHL=MTGLZAHL+1:MTGL$=STR$(MTGLZAHL)
1940 GOSUB 12090                                                   'Basis Maske
1950 FKEYS$="<F7>=Abspeichern":GOSUB 12210                        'F-Tastenleiste
1960 PRINT FNCUR$(3,30);"Mitgliedsnummer";FNCUR$(5,41);"Beruf:":GOSUB 10670  'Feldmaske
1970 GOSUB 11890                                                      'Defaults
1980 IF DATENAUSG=1 THEN GOSUB 10910:DATENAUSG=0             'wenn vorher HILFE
1990 PRINT FNCUR$(21,1);DELLINE$                               'Cursorparameter
2000 PRINT HIGHON$;CURSON$;FNCUR$(3,46);MTGL$
2010 Z=FNROW(FELD$(F)):S=FNCOL(FELD$(F))+14
2020 LNG=FNLNG(FELD$(F)):TYP=FNTYP(FELD$(F))
2030 PRINT FNCUR$(21,1);DELLINE$
2040 GOSUB 10240                                                         'INPUT
2050 IF AASC=13 THEN IF MUSS$="=" THEN IF LEN(ANT$)<1 THEN N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 12020:GOTO 2040 'Nachricht
2060 IF AASC=13 THEN DAT$(F)=ANT$                                       'Return
2070 IF AASC=13 THEN IF F<FELDMAX THEN F=F+1:GOTO 2010 ELSE N$="Letztes Feld":GOSUB 12020:GOTO 2010          'ELSE zu IF F< 'Nachricht
2080 IF HOCH<>0 THEN PRINT FNCUR$(Z,S);DAT$(F)+STRING$(LNG-LEN(DAT$(F)),"."):F=F+(HOCH=-1)*(-(F>1)+(F=1)*25)+(HOCH=1)*((F<27)-(F=27)*26):GOTO 2010
2090 IF ZURUCK=1 THEN GOSUB 16430:GOTO 1940                             'F-Keys
2100 IF ZURUCK=3 THEN GOSUB 11780:PRINT HIGHOFF$:GOSUB 12610:GOTO 1530
2110 IF ZURUCK=2 THEN GOSUB 11780:PRINT HIGHOFF$:GOTO 1750
2120 IF ZURUCK=4 THEN GOSUB 11780:PRINT HIGHOFF$:GOSUB 12480:GOTO 2040
2130 IF ZURUCK=7 THEN GOSUB 11170:GOSUB 10810:F=1:MTGLZAHL=MTGLZAHL+1:MTGL$=STR$(MTGLZAHL):GOSUB 12610:GOSUB 11890:GOSUB 10910:GOTO 2000
2140 N$="Ungültige Eingabe - Bitte wiederholen":GOSUB 12020          'Nachricht
2150 GOSUB 10260                                       'INPUT (2 Zeilen später)
2160 GOTO 2050                                                      'Auswertung 
2170 ' 
2180 ' *** Stammsatz ändern von 03************ ********************************
2190 '
2200 MT$="STAMMSATZ ÄNDERN":MI$="G-01102":GOSUB 12090              'Basis Maske
2210 F=1:PRINT CURSON$
2220 FKEYS$="":GOSUB 12210                                      'F-Tastenleiste
2230 PRINT HIGHOFF$;
2240 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer"
2250 PRINT FNCUR$(5,23)"des zu verändernden Stammsatzes ein,"
2260 PRINT FNCUR$(6,22)"und beenden Sie die Eingabe mit RETURN"
2270 PRINT FNCUR$(8,37)"=>..."
2280 LNG=3:TYP=2:Z=8:S=39
2290 GOSUB 10240                                                         'INPUT
2300 IF ZURUCK=1 THEN GOSUB 16660:GOTO 2200                             'F-Keys
2310 IF ZURUCK=2 THEN 1750
2320 IF ZURUCK=3 THEN GOSUB 12610:GOTO 1530
2330 IF ZURUCK=4 THEN GOSUB 12480
2340 IF HOCH<>0 OR ZURUCK<>0 THEN GOTO 2280                             'Errors
2350 IF LEN(ANT$)<1 THEN N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 12020:GOTO 2280 ELSE MITGLIED=VAL(ANT$) 'Nachricht
2360 IF MITGLIED=0 THEN MITGLIED=MTGLMAX+2
2370 IF MITGLIED>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12020:PRINT HIGHOFF$;FNCUR$(8,39);"...";DELHIGH$:ANT$="":GOTO 2290 'Nachricht
2380 MTGLZAHL=MITGLIED:MTGL$=STR$(MITGLIED)
2390 FOR K=4 TO 8:PRINT FNCUR$(K,1);DELLINE$:NEXT:PRINT FNCUR$(21,1);DELLINE$
2400 GOSUB 11070                                                   'Daten lesen
2410 PRINT HIGHOFF$;FNCUR$(3,30);"Mitgliedsnummer";FNCUR$(5,41);"Beruf:"
2420 GOSUB 10670
2430 GOSUB 10910
2440 PRINT FNCUR$(3,46);MTGLZAHL
2450 FKEYS$="<F7>=Abspeichern":GOSUB 12210                      'F-Tastenleiste
2460 S=FNCOL(FELD$(F))+14:Z=FNROW(FELD$(F)):N$="":GOSUB 12020        'Nachricht
2470 LNG=FNLNG(FELD$(F)):TYP=FNTYP(FELD$(F))
2480 GOSUB 10240                                                         'INPUT
2490 IF ZURUCK=1 THEN GOSUB 16770:GOTO 2410                 'Hilfe-Text  F-Keys
2500 IF ZURUCK=2 THEN GOSUB 12680: GOTO 2200                   'Eing.-Mtgl.-Nr.
2510 IF ZURUCK=3 THEN GOSUB 12680:GOSUB 12610: GOTO 1530              'Mvw-Menü
2520 IF ZURUCK=4 THEN GOSUB 12480:PRINT HIGHOFF$;FNCUR$(24,1);"<F7>=Abspeichern";FNCUR$(1,1);HIGHON$:GOTO 2480
2530 IF ZURUCK=7 THEN 2600                                     'Daten Speichern
2540 IF HOCH=-1 THEN PRINT FNCUR$(Z,S);DAT$(F);STRING$(LNG-LEN(DAT$(F)),"."):IF F>1  THEN F=F-1:GOTO 2460  ELSE F=26:GOTO 2460 'Cursor hoch
2550 IF HOCH=1  THEN PRINT FNCUR$(Z,S);DAT$(F);STRING$(LNG-LEN(DAT$(F)),"."):IF F<27 THEN F=F+1:GOTO 2460  ELSE F= 1:GOTO 2460 'Cursor runter
2560 IF AASC=13 AND LEN(ANT$)<1 AND MUSS$="=" THEN N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 12020:GOTO 2480 'Nachricht
2570 IF AASC=13 THEN DAT$(F)=ANT$:PRINT HIGHON$;FNCUR$(Z,S);DAT$(F);STRING$(LNG-LEN(DAT$(F)),".")                    'Return
2580 IF AASC=13 THEN IF F<FELDMAX THEN F=F+1:GOTO 2460 ELSE F=1:GOTO 2460
2590 N$="Ungültige Eingabe - Bitte wiederholen":GOSUB 12020:FOR KK=1 TO 25000:NEXT:GOTO 2460 'Nachricht
2600 GOSUB 11180                                               'Daten speichern
2610 Q$="Wollen Sie noch einen weiteren Datensatz ändern?":GOSUB 11310 'Ja/Nein-Frage
2620 IF JA=1 THEN 2200
2630 GOTO 1750                                                    'Zum STS-Menü
2640 '
2650 ' *** Stammsatz löschen von 04********** *********************************
2660 '
2670 MT$="STAMMSATZ LÖSCHEN":MI$="G-01103":GOSUB 12090             'Basis Maske
2680 FKEYS$="":GOSUB 12210                                      'F-Tastenleiste
2690 PRINT FNCUR$(4,23);"Bitte geben Sie die Mitgliedsnummer"         'Menütext
2700 PRINT FNCUR$(5,25);"des zu löschenden Mitglieds ein."
2710 PRINT FNCUR$(6,15);"(Dabei wird nur der Status des Mitglieds verändert.)"
2720 PRINT FNCUR$(7,23);"Beenden Sie die Eingabe mit RETURN"
2730 PRINT FNCUR$(9,37);"=>..."
2740 LNG=3:TYP=2:Z=9:S=39
2750 PRINT HIGHON$:GOSUB 10250:PRINT HIGHOFF$                            'INPUT
2760 IF ZURUCK=2 THEN 1750                                              'F-Keys
2770 IF ZURUCK=3 THEN GOSUB 12610: GOTO 1530
2780 IF ZURUCK=1 THEN GOSUB 16970:GOTO 2670
2790 IF ZURUCK=4 THEN GOSUB 12480:GOTO 2690
2800 IF ZURUCK>4 THEN HOCH=1
2810 IF HOCH<>0 THEN N$="Ungültige Eingabe - Bitte wiederholen":GOSUB 12020:GOTO 2730 'Nachricht
2820 MTGLZAHL=VAL(ANT$):IF MTGLZAHL=0 THEN MTGLZAHL=MTGLMAX+1           'Errors
2830 IF MTGLZAHL>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12020:GOTO 2730 'Nachricht
2840 GOSUB 11070                                                'Daten einlesen
2850 GOSUB 12090                                                   'Basis Maske
2860 PRINT FNCUR$(3,30);"Mitgliedsnummer";FNCUR$(3,46);MTGLZAHL;FNCUR$(5,41);"Beruf:":GOSUB 10670:GOSUB 10910 'Feldmaske
2870 Q$="Endgültig löschen?":GOSUB 11310                         'Ja/Nein-Frage 
2880 IF JA=0 THEN 2910                                               'Löschen ?
2890 DAT$(5)="ausgetreten     ":PRINT FNCUR$(9,16);HIGHON$;DAT$(5);HIGHOFF$
2900 GOSUB 11180                                             'Daten abspeichern
2910 Q$="Soll noch jemand gelöscht werden?":GOSUB 11310          'Ja/Nein-Frage
2920 IF JA=1 THEN 2670 ELSE 1750
2930 '
2940 ' *** Stammsatz suchen von 05*************** *****************************
2950 '
2960 MI$="G-01105"
2970 NUMSUCH1=0:NUMSUCH2=0:MTGL$=""
2980 PRINT HIGHOFF$
2990 IF DRUCKFLAG THEN MT$="SUCHEN & DRUCKEN" ELSE MT$="STAMMSATZ SUCHEN"
3000 GOSUB 12090                                                   'Basis Maske
3010 FKEYS$="":GOSUB 12210                                      'F-Tastenleiste
3020 PRINT CURSOFF$;
3030 IF DRUCKFLAG THEN A$=" Drucken " ELSE A$=" Suchen "
3040 PRINT FNCUR$(6,30)"1 -";A$;"nach Nummern"
3050 PRINT FNCUR$(8,30)"2 -";A$;"nach Maske  ":GOSUB 12610
3060 NKEYMAX=2:GOSUB 12370                                         'Menüeingabe
3070 IF FKEY=1 THEN GOSUB 17110:GOTO 2980                               'F-Keys
3080 IF FKEY=2 THEN IF DRUCKFLAG THEN GOSUB 12610:GOTO 5220 ELSE 1750
3090 IF FKEY=3 THEN GOSUB 12610: GOTO 1530
3100 IF FKEY=4 THEN GOSUB 12480:GOTO 3060
3110 IF NKEY=1 THEN 3310
3120 F=1                                                     'Suchen nach Maske
3130 FOR I=1 TO 27:SUCH$(I)="":DAT$(I)="":NEXT
3140 FKEYS$="<F5>=Suchen ":GOSUB 4030
3150 IF DRUCKFLAG THEN FKEYS$="<F5>=Suchen & Drucken"
3160 GOSUB 12210                                                'F-Tastenleiste
3170 Z=FNROW(FELD$(F)):S=FNCOL(FELD$(F))+14                    'Cursorparameter
3180 LNG=FNLNG(FELD$(F)):TYP=3
3190 PRINT FNCUR$(Z,S)
3200 ANT$="":GOSUB 10240                                                 'INPUT
3210 IF ZURUCK=5 THEN MTGLZAHL=1:GOTO 3620                              'F-Keys
3220 IF ZURUCK=1 THEN GOSUB 17110:GOSUB 4030:GOSUB 10910:GOTO 3150
3230 IF ZURUCK=4 THEN GOSUB 12480:GOTO 3170
3240 IF ZURUCK=2 THEN 2960
3250 IF ZURUCK=3 THEN GOSUB 12610: GOTO 1530
3260 IF ANT$<>"" OR AASC=13 THEN SUCH$(F)=ANT$:DAT$(F)=ANT$
3270 IF HOCH=-1 THEN IF F>1 THEN F=F-1 ELSE F=26                   'Cursor hoch
3280 IF HOCH=1 OR AASC=13 THEN IF F<27 THEN F=F+1 ELSE F=1       'Cursor runter
3290 N$="":GOSUB 12020                                               'Nachricht
3300 GOTO 3170
3310 PRINT CLS$:PRINT:PRINT                                'Suchen nach Nummern
3320 Z=7:S=1:IF DRUCKFLAG THEN DRUCK$="gedruckt" ELSE DRUCK$="gesucht "
3330 TYP=2:LNG=3:PRINT CURSON$;                                       'Menütext
3340 GOSUB 12090                                                   'Basis Maske
3350 PRINT FNCUR$(4,5);"Maximale Mitgliedsnummer:";MTGLMAX
3360 FKEYS$="":GOSUB 12210                                        'F-Tastenleiste
3370 PRINT HOME$;:PRINT FNCUR$(Z,S);                    'Beginn bei Nr...
3380 PRINT HIGHOFF$;"Ab welcher Mitgliedsnummer soll ";DRUCK$;" werden      =>...":S=56
3390 GOSUB 10250                                                         'INPUT
3400 IF ZURUCK=1 THEN GOSUB 17110:GOTO 3310                             'F-Keys
3410 IF ZURUCK=2 THEN 2960
3420 IF ZURUCK=3 THEN GOSUB 12610: GOTO 1530
3430 IF ZURUCK=4 THEN GOSUB 12480:S=1:GOTO 3370
3440 IF ANT$="" THEN S=1:GOTO 3370
3450 NUMSUCH1=VAL(ANT$)
3460 IF NUMSUCH1>MTGLMAX OR NUMSUCH1<1 THEN 3590
3470 Z=9:S=1
3480 N$="":GOSUB 12020                                               'Nachricht
3490 PRINT HOME$:PRINT FNCUR$(Z,S);                       'Ende bei Nr...
3500 PRINT HIGHOFF$;"Bis zu welcher Mitgliedsnummer soll ";DRUCK$;" werden  =>...":S=56
3510 GOSUB 10250                                                         'INPUT
3520 IF ZURUCK=1 THEN GOSUB 17110:GOTO 3310                             'F-Keys
3530 IF ZURUCK=2 THEN 2960
3540 IF ZURUCK=3 THEN GOSUB 12610: GOTO 1530
3550 IF ZURUCK=4 THEN GOSUB 12480:S=1:GOTO 3490
3560 IF ANT$="" THEN S=1:GOTO 3490
3570 NUMSUCH2=VAL(ANT$)
3580 IF NUMSUCH2<=MTGLMAX AND NUMSUCH1<=NUMSUCH2 THEN 3610
3590 N$="Unlogische Eingabe - Bitte neu eingeben":GOSUB 12020 'Errors 'Nachricht
3600 Z=6:S=1:PRINT FNCUR$(9,1);DELLINE$;FNCUR$(Z,S);:Z=7:GOTO 3370
3610 MTGLZAHL=NUMSUCH1                                               'Feldmaske
3620 PRINT CURSOFF$
3630 IF DRUCKFLAG THEN GOSUB 15030
3640 CC$="  <F7>=Adressaufkleber"
3650 IF NUMSUCH1<>0 THEN FKEYS$="<F5>=Suchen rückwärts  <F6>=vorwärts"+CC$ ELSE FKEYS$="<F6>=Suchen vorwärts"+CC$
3660 GOSUB 4030
3670 PRINT FNCUR$(3,30);"Mitgliedsnummer"
3680 IF NUMSUCH1=0  AND MTGLZAHL<=MTGLMAX  THEN 3750
3690 IF NUMSUCH1<>0 AND MTGLZAHL<=NUMSUCH2 THEN 3750
3700 IF DRUCKFLAG THEN GOSUB 12610:GOTO 2960
3710 PRINT HIGHON$;CURSOFF$;:MTGLZAHL=MTGLZAHL-1
3720 N$="Daten nicht vorhanden. Mit <F2> neue Suchmöglichkeit.":GOSUB 12020 'Nachricht
3730 PRINT HIGHOFF$;
3740 GOTO 3900
3750 GOSUB 11070                                                   'Daten lesen
3760 IF NUMSUCH1<>0 THEN 3860
3770 FOR K=1 TO FELDMAX                                      'Daten durchsuchen
3780   IF LEFT$(SUCH$(K),1)<>"*" THEN 3810
3790   IF INSTR(DAT$(K),RIGHT$(SUCH$(K),LEN(SUCH$(K))-1))=0 THEN MTGLZAHL=MTGLZAHL+1:GOTO 3680
3800   GOTO 3850
3810   I=1
3820   IF MID$(SUCH$(K),I,1)="?" THEN I=I+1:GOTO 3820
3830   I=I-1
3840   IF INSTR(DAT$(K),RIGHT$(SUCH$(K),LEN(SUCH$(K))-I))<>I+1 THEN MTGLZAHL=MTGLZAHL+1:GOTO 3680
3850 NEXT
3860 GOSUB 10910                                                     'Feldmaske
3870 MTGL$=STR$(MTGLZAHL) 
3880 PRINT CURSOFF$;FNCUR$(3,46);MTGL$
3890 IF DRUCKFLAG THEN GOSUB 14800:IF BRKFLAG THEN BRKFLAG=0:N$="":GOSUB 12020:GOTO 2960 ELSE N$="":GOSUB 12020:PRINT CURSOFF$:MTGLZAHL=MTGLZAHL+1:GOTO 3680 'Nachricht 'Nachricht
3900 ZURUCK=ASC(INPUT$(1))-240
3910 N$="":GOSUB 12020                                               'Nachricht
3920 IF ZURUCK=1 THEN GOSUB 17110:GOSUB 4030:PRINT FNCUR$(3,30);"Mitgliedsnummer":GOTO 3860
3930 IF ZURUCK=2 THEN IF NUMSUCH1<>0 THEN 3310 ELSE 3120
3940 IF ZURUCK=3 THEN GOSUB 12610:GOTO 1530
3950 IF ZURUCK=7 THEN GOSUB 15160:GOSUB 12210:N$="":GOSUB 12020:GOTO 3860 'F-Tastenleiste 'Nachricht
3960 IF ZURUCK=4 THEN GOSUB 12480:GOTO 3860
3970 IF ZURUCK=5 AND NUMSUCH1<>0 THEN IF MTGLZAHL>NUMSUCH1 THEN MTGLZAHL=MTGLZAHL-1:GOTO 3750
3980 IF ZURUCK=6 THEN 4010
3990 N$="Falsche Eingabe":GOSUB 12020                                'Nachricht
4000 GOTO 3900
4010 MTGLZAHL=MTGLZAHL+1:PRINT FNCUR$(21,1);DELLINE$;HOME$;
4020 GOTO 3680
4030 GOSUB 12090                                                   'Basis Maske
4040 IF DRUCKFLAG THEN MT$="SUCHEN & DRUCKEN" ELSE MT$="STAMMSATZ SUCHEN"
4050 GOSUB 10670
4060 IF NOT DRUCKFLAG THEN GOSUB 12210                          'F-Tastenleiste
4070 PRINT FNCUR$(5,41);"BERUF:"
4080 RETURN
4090 '
4100 ' *** Stammsatz ausdrucken von 02************ & 04********** *************
4110 '
4120 MT$="STAMMSATZ AUSDRUCKEN":MI$="G-01104":GOSUB 12090          'Basis Maske
4130 FKEYS$="":GOSUB 12210                                      'F-Tastenleiste
4140 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer"
4150 PRINT FNCUR$(5,21)"des auszudruckenden Stammsatzes ein."
4160 PRINT FNCUR$(6,23)"Beenden Sie die Eingabe mit RETURN"
4170 PRINT FNCUR$(8,37);"=>..."
4180 LNG=3:TYP=2:Z=8:S=39
4190 PRINT HIGHON$:GOSUB 10250:PRINT HIGHOFF$                            'INPUT
4200 IF ZURUCK=1 THEN GOSUB 17370:GOTO 4120                             'F-Keys
4210 IF ZURUCK=2 THEN 1750
4220 IF ZURUCK=3 THEN PRINT HIGHOFF$:GOSUB 12610: GOTO 1530
4230 IF ZURUCK=4 THEN GOSUB 12480:GOTO 4170
4240 IF ZURUCK>4 THEN HOCH=1
4250 IF HOCH<>0 THEN 4170
4260 MTGLZAHL=VAL(ANT$):IF MTGLZAHL=0 THEN MTGLZAHL=MTGLMAX+1           'Errors
4270 IF MTGLZAHL>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12020:GOTO 4170 'Nachricht
4280 GOSUB 11070                                                   'Daten lesen
4290 GOSUB 12090                                                   'Basis Maske
4300 PRINT FNCUR$(3,30);"Mitgliedsnummer";HIGHON$;FNCUR$(3,46);MTGLZAHL;HIGHOFF$;FNCUR$(5,41);"Beruf:":GOSUB 10670:GOSUB 10910
4310 Q$="Wollen Sie diese Daten ausdrucken lassen?":GOSUB 11310  'Ja/Nein-Frage
4320 IF JA=0 THEN 4120                                            'Ausdrucken ?
4330 Q$="Ist der Drucker bereit zur Ausgabe?":GOSUB 11310        'Ja/Nein-Frage
4340 IF JA=0 THEN N$="Bitte schalten Sie den Drucker ein.":GOSUB 12020:FOR DEL=0 TO 1000:NEXT:GOTO 4310     'Drucker bereit ? 'Nachricht
4350 GOSUB 15360:GOTO 4120                                             'Drucken
4360 '
4370 ' ######################### Verwendungsnachweis ##########################
4380 '
4390 GOSUB 10570                                              'Einlesen MTGLMAX
4400 DEL$=BSP$+" "+BSP$                                        'TODO ever used?
4410 VNFLAG=-1
4420 '
4430 ' Verwendungsnachweis-Menü ***********************************************
4440 '
4450 MT$="VERWENDUNGSNACHWEIS":MI$="G-01200":GOSUB 12090           'Basis Maske
4460 F=4                                                                'TODO ?
4470 FKEYS$="":GOSUB 12210                                      'F-Tastenleiste
4480 PRINT CURSON$;
4490 PRINT HIGHOFF$;
4500 PRINT FNCUR$(4,20)"   Bitte geben Sie die Mitgliedsnummer"
4510 PRINT FNCUR$(5,20)"des zu ändernden Verwendungsnachweises ein"
4520 PRINT FNCUR$(6,20)" und beenden Sie die Eingabe mit RETURN."
4530 PRINT FNCUR$(8,20)"                 =>..."
4540 Z=8:S=39:LNG=3:TYP=2
4550 GOSUB 10240                                                         'INPUT
4560 IF ZURUCK=1 THEN GOSUB 17500:GOTO 4450
4570 IF ZURUCK=2 OR ZURUCK=3 THEN GOSUB 12610:VNFLAG=0:GOTO 1530
4580 IF ZURUCK=4 THEN GOSUB 12480
4590 IF ZURUCK>4 THEN 4540
4600 IF HOCH<>0 THEN GOTO 4540
4610 IF LEN(ANT$)=0 THEN N$="Muss-Feld: Sie müssen etwas eingeben!":GOSUB 12020:GOTO 4530 ELSE MITGLIED=VAL(ANT$):IF MITGLIED=0 THEN 4550 'Nachricht
4620 IF MITGLIED>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12020:PRINT HOME$;HIGHOFF$:GOTO 4530 'Nachricht
4630 MTGLZAHL=MITGLIED:MTGL$=STR$(MITGLIED)
4640 '
4650 ' *** Einlesen des Mitgliedsnamens ***************************************
4660 '
4670 PRINT FNCUR$(21,1);DELLINE$;
4680 GET #2,MITGLIED                                        'EINLESEN DATEN STS
4690 NNAM$=LEFT$(DD1$,25)                                 'NACHNAME RAUSFISCHEN
4700 VNAM$=MID$(DD1$,26,25)                               'VORNAME  RAUSFISCHEN
4710 '
4720 ' ** LEERZEICHEN HINTER NACHNAME ABSCHNEIDEN *****************************
4730 '
4740 FOR I=LEN(NNAM$) TO 1 STEP -1
4750   IF MID$(NNAM$,I,1)<>" " THEN NNAM$=LEFT$(NNAM$,I):I=1
4760 NEXT
4770 NAMM$=NNAM$+" "+VNAM$
4780 GOSUB 13050                                          'EINLESEN 1. VN-BLATT
4790 BLATT=1
4800 VP=1:HP=1                                                 'INITIALISIERUNG
4810 F=8
4820 GOSUB 12090                                                   'Basis Maske
4830 FKEYS$="<F7>=Abspeichern":GOSUB 12210                      'F-Tastenleiste
4840 PRINT HIGHOFF$;
4850 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=Vorwärts";TAB(53);"<F9>=Blatt drucken";
4860 PRINT FNCUR$(5,2);"Mitglieds-Nr.:"
4870 PRINT FNCUR$(3,2); "Blatt:"
4880 PRINT FNCUR$(5,24);"Name:"
4890 MSK$="Von:  .  .   Bis:  .  .   Amt:"
4900 FOR KK=7 TO 19 
4910   PRINT FNCUR$(KK,2);MSK$;TAB(43);MSK$;
4920 NEXT
4930 GOSUB 14290                                                 'Ausgabe Daten
4940 LNG=2-(HP=7)*6                                    'BEGIN OF THE INPUT-LOOP
4950 TYP=2-(HP=7)
4960 Z=VP+13*(VP>13)+6
4970 S=6-39*(VP>13)-(HP<7)*(HP-1)*3+(HP>3)*(HP<7)*4-(HP=7)*26
4980 N$="":GOSUB 12020                         'ERASE MESSAGE-LINE   'Nachricht
4990 GOSUB 10250                                                         'INPUT
5000 IF HP<7 THEN IF AASC=13 THEN ANT$=STRING$(2-LEN(ANT$),"0")+ANT$   '+NULLEN
5010 IF HOCH<>0 OR AASC=13 THEN PRINT FNCUR$(Z,S);ANT$
5020 IF AASC=13 THEN IF HP<7 THEN MID$(DAT$(VP),-1+2*HP,2)=ANT$ ELSE DAT$(VP)=LEFT$(DAT$(VP),12)+ANT$+SPACE$(8-LEN(ANT$))
5030 IF AASC=13 AND HP<7 THEN JA=1:TEST$=ANT$:ON HP GOSUB 11690,11710,11730,11690,11710,11730:IF JA=0 THEN N$="Ungültiges Datum":GOSUB 12020:GOTO 4990 'Datums-Check 'Nachricht
5040 IF AASC=13 THEN IF HP=7 THEN HP=1:VP=VP-(VP<26)-(VP=26)*(-25):GOTO 4940 ELSE HP=HP+1:GOTO 4940     'NEUE SCREEN-POSITION
5050 IF HOCH<>0 THEN TEST$=MID$(DAT$(VP),-(HP<7)*2*HP-1-(HP=7)*14,-(HP<7)*2-(HP=7)*8):PRINT FNCUR$(Z,S);TEST$
5060 IF HOCH<>0 THEN IF HP<7 AND NOT TEST$="  " THEN JA=1:ON HP GOSUB 11690,11710,11730,11690,11710,11730:IF JA=0 THEN N$="Ungültiges Datum":GOSUB 12020:GOTO 4990 'Nachricht
5070 IF HOCH=1  THEN VP=VP-(VP<26)-(VP=26)*(-25):GOTO 4940
5080 IF HOCH=-1 THEN VP=VP+(VP>1)-(VP=1)*25     :GOTO 4940
5090 IF HOCH=2  THEN HP=HP-(HP<7)-(HP=7)*(-6)   :GOTO 4940
5100 IF HOCH=-2 THEN HP=HP+(HP>1)+(HP=1)*(-6)   :GOTO 4940
5110 IF ZURUCK=1 THEN GOSUB 17610 :GOTO 4800         'HILFE - ZU INIT UND MASKE
5120 IF ZURUCK=2 THEN GOTO 4450                                'ZU MTGL-EINGABE
5130 IF ZURUCK=3 THEN VNFLAG=0:GOSUB 12610:GOTO 1530
5140 IF ZURUCK=4 THEN GOSUB 12480:GOSUB 12280:GOTO 4990 
5150 IF ZURUCK=5 THEN GOSUB 13140:GOTO 4940
5160 IF ZURUCK=6 THEN GOSUB 13300:GOTO 4940
5170 IF ZURUCK=7 THEN GOSUB 13950:GOTO 4990             'ABSPEICHERN DANN INPUT
5180 IF ZURUCK=9 THEN GOSUB 14520:GOSUB 13500:GOTO 4980
5190 '
5200 ' ############# Mitgliedsliste drucken von 05*************** #############
5210 '
5220 DRUCKFLAG=-1
5230 MTGL$=""
5240 DATUM$="  .  .  "
5250 ' GOSUB 1680                                               'TODO Sad hack!
5260 DEL$=BSP$+"."+BSP$
5270 GOSUB 10570                                              'Einlesen MTGLMAX
5280 GOSUB 10480          'Variablen Init: Read MTGLZAHL, clear DAT$(), SUCH$()
5290 '
5300 PRINT CLS$;
5310 PRINT HIGHOFF$;
5320 PRINT CURSOFF$;
5330 MT$="MITGLIEDSLISTE DRUCKEN":MI$="G-01300":GOSUB 12090        'Basis Maske
5340 FKEYS$="":GOSUB 12210                                      'F-Tastenleiste
5350 PRINT FNCUR$(4,28);"1 - Suchen & Ausdrucken"
5360 PRINT FNCUR$(6,28);"2 - Komplett ausdrucken"
5370 PRINT FNCUR$(8,28);"3 - Datenblatt drucken"
5380 NKEYMAX=3:GOSUB 12370                                         'Menüeingabe
5390 IF FKEY=1 THEN GOSUB 17810:GOTO 5300
5400 IF FKEY=2 OR FKEY=3 THEN GOSUB 12610:DRUCKFLAG=0:GOTO 1530
5410 IF FKEY=4 THEN GOSUB 12480:GOTO 5380
5420 IF NKEY=1 THEN 5480
5430 IF NKEY=2 THEN DRUCKFLAG=0:GOTO 5480
5440 IF NKEY=3 THEN 5890
5450 '
5460 ' *** Suchen & Ausdrucken, Komplett ausdrucken ***************************
5470 '
5480 GOSUB 12090                                                   'Basis Maske
5490 GOSUB 12210                                                'F-Tastenleiste
5500 PRINT FNCUR$(7,13);"Geben Sie bitte das heutige Datum ein :   .  .  ":Z=7:LNG=2:TYP=2
5510 PRINT HIGHON$;CURSON$
5520 FOR K=0 TO 2
5530   S=53+K*3
5540   VNFLAG=-1:GOSUB 10250:VNFLAG=0    'in order to activate right-left shift
5550   IF HOCH<>0 THEN JA=1:TEST$=MID$(DATUM$,K*3+1,2):ON K+1 GOSUB 11690,11710,11730:IF JA=0 THEN N$="Ungültiges Datum":GOSUB 12020:GOTO 5530 'Nachricht
5560   IF ZURUCK=1 THEN GOSUB 17810:GOTO 5480
5570   IF ZURUCK=2 THEN 5220
5580   IF ZURUCK=3 THEN GOSUB 12610:DRUCKFLAG=0:GOTO 1530
5590   IF ZURUCK=4 THEN GOSUB 12480:GOTO 5530
5600   IF HOCH=-2 THEN IF K=0 THEN 5530 ELSE K=K-1:GOTO 5530
5610   IF HOCH=+2 THEN IF K=2 THEN K=0:GOTO 5530 ELSE K=K+1:GOTO 5530
5620   IF AASC<>13 THEN 5530
5630   ANT$=STRING$(2-LEN(ANT$),"0")+ANT$:PRINT FNCUR$(Z,S);ANT$
5640   TEST$=ANT$:JA=1:ON K+1 GOSUB 11690,11710,11730:IF JA=0 THEN N$="Ungültige Datumseingabe":GOSUB 12020:GOTO 5530 'Nachricht
5650   MID$(DATUM$,K*3+1,2)=ANT$
5660 NEXT
5670 PRINT HIGHOFF$;
5680 PRINT CURSOFF$
5690 Q$="Bitte den Drucker bereit machen. Fertig?":GOSUB 11310   'Ja/Nein-Frage
5700 IF JA=0 THEN N$="":GOSUB 12020:GOTO 5480                        'Nachricht
5710 '
5720 ' *** Ausdrucken Anfang **************************************************
5730 '
5740 IF DRUCKFLAG THEN 5780
5750 PRINT FNCUR$(7,1);DELLINE$;
5760 PRINT CURSOFF$
5770 N$="Mitgliedsliste wird gedruckt. Abbrechen mit Taste.":GOSUB 12020 'Nachricht
5780 IF DRUCKFLAG THEN 2970
5790 PRINT FNCUR$(23,1);DELLINE$;
5800 GOSUB 15030
5810 FOR MTGLZAHL=1 TO MTGLMAX
5820   GOSUB 11070:IF INSTR(DAT$(5),"ausgetreten")>0 THEN 5840
5830   GOSUB 14800:IF BRKFLAG THEN BRKFLAG=0:N$="Drucken abgebrochen":GOSUB 12020:PRINT CURSOFF$:GOTO 5850 ELSE GOSUB 12020:PRINT CURSOFF$ 'Nachricht
5840 NEXT MTGLZAHL
5850 GOSUB 12610:DRUCKFLAG=0:GOTO 5220
5860 '
5870 ' *** Datenblatt drucken *************************************************
5880 '
5890 MT$="DATENBLATT DRUCKEN":MI$="G-01303":GOSUB 12090            'Basis Maske
5900 FKEYS$="":GOSUB 12210                                      'F-Tastenleiste
5910 PRINT FNCUR$(4,23)"Bitte geben Sie die Mitgliedsnummer"
5920 PRINT FNCUR$(5,24)"des zu druckenden Datenblattes ein."
5930 PRINT FNCUR$(7,23)"Beenden Sie die Eingabe mit RETURN"
5940 PRINT FNCUR$(9,37)"=>..."
5950 LNG=3:TYP=2:Z=9:S=39
5960 PRINT HIGHON$:GOSUB 10250:PRINT HIGHOFF$                            'INPUT
5970 IF ZURUCK=2 THEN 5220                                              'F-Keys
5980 IF ZURUCK=3 THEN GOSUB 12610: GOTO 1530
5990 IF ZURUCK=1 THEN GOSUB 17810:PRINT CURSON$:MT$="DATENBLATT DRUCKEN":GOTO 5890
6000 IF ZURUCK=4 THEN GOSUB 12480:GOTO 5890
6010 IF ZURUCK>4 THEN HOCH=1 
6020 IF HOCH<>0 THEN N$="Ungültige Eingabe - Bitte wiederholen":GOSUB 12020:GOTO 5940 'Nachricht
6030 MTGLZAHL=VAL(ANT$):IF MTGLZAHL=0 THEN MTGLZAHL=MTGLMAX+1           'Errors
6040 IF MTGLZAHL>MTGLMAX THEN N$="Diese Mitgliedsnummer existiert nicht.":GOSUB 12020:GOTO 5940 'Nachricht
6050 Q$="Ist der Drucker bereit zur Ausgabe?":GOSUB 11310        'Ja/Nein-Frage
6060 IF JA=0 THEN N$="Bitte schalten Sie den Drucker ein.":GOSUB 12020:FOR DEL=0 TO 1000:NEXT:GOTO 5890     'Drucker bereit ? 'Nachricht
6070 GOSUB 11070                                            'S.S.Daten einlesen
6080 NNAM$ = LEFT$(DD1$,25) : NNAM$ = LEFT$(NNAM$,INSTR(NNAM$," "))
6090 NAMM$ = NNAM$ + MID$(DD1$,26,25)
6100 GOSUB 15360                                                   'S.S.drucken
6110 GOSUB 13050                                            'V.N Daten einlesen
6120 GOSUB 14540                                             'V.N Daten drucken
6130 GOSUB 15620                                             'Mitgliedsvermerke
6140 GOTO 5890
10000 '
10010 ' ######## Unterprogramme von 06*****, 07****, 08**** und 09**** ########
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
10160 PRINT ESC$;"41";CHR$(88);CHR$(200);   'Key 88 "Cursor up"    -> ASCII 200
10170 PRINT ESC$;"41";CHR$(89);CHR$(201);   'Key 89 "Cursor down"  -> ASCII 201
10180 PRINT ESC$;"41";CHR$(98);CHR$(202);   'Key 98 "Cursor right" -> ASCII 202
10190 PRINT ESC$;"41";CHR$(99);CHR$(203);   'Key 99 "Cursor left"  -> ASCII 203
10200 RETURN
10210 '
10220 ' *** Eingaberoutine (INPUT) ********************************************
10230 '
10240 MUSS$=MID$(FELD$(F),13,1)
10250 PRINT HIGHON$:ANT$="":I=1
10260 HOCH=0:ZURUCK=0:PRINT HOME$;" ":PRINT FNCUR$(Z,S+I-1);
10270 A$=INPUT$(1):AASC=ASC(A$)
10280 ON TYP GOSUB 11440,11510,11570,11630                      'Bereichs check
10290 IF DRIN=1 THEN IF I<=LNG THEN PRINT A$;:ANT$=LEFT$(ANT$,I-1)+A$:I=I+1:GOTO 10270 ELSE GOTO 10270
10300 IF AASC=8 THEN IF I>1 THEN PRINT DEL$;:I=I-1:ANT$=LEFT$(ANT$,I-1):GOTO 10260 ELSE GOTO 10270                'Delete
10310 IF VNFLAG THEN IF AASC=13 THEN PRINT HIGHOFF$;STRING$(LNG-I+1," ");HIGHON$:RETURN 'BEI VN RETURN OHNE PUNKTE
10320 IF AASC=13  THEN PRINT HIGHOFF$;STRING$(LNG-I+1,"."):RETURN       'Return
10330 IF AASC=200 THEN HOCH=-1:GOSUB 10420:RETURN                  'Cursor hoch
10340 IF AASC=201 THEN HOCH=+1:GOSUB 10420:RETURN                'Cursor runter
10350 IF VNFLAG AND AASC=202 THEN HOCH=-2:GOSUB 10420:RETURN
10360 IF VNFLAG AND AASC=203 THEN HOCH=2 :GOSUB 10420:RETURN
10370 IF AASC>240 AND AASC<250 THEN ZURUCK=AASC-240:RETURN              'F-Keys
10380 N$="Ungültige Eingabe - Bitte wiederholen":GOSUB 12020:GOTO 10260 'Nachricht
10390 '
10400 ' *** Cursor hoch/runter Repeat Blockierung *****************************
10410 '
10420 A$=INKEY$
10430 IF A$>CHR$(199) THEN 10420                    'Blocks any cursor movement
10440 RETURN
10450 '
10460 ' *** Disk I/O Variablen Initalisierung *********************************
10470 '
10480 GOSUB 10570:MTGLZAHL=MTGLMAX                            'Einlesen MTGLMAX 
10490 FOR K=1 TO FELDMAX                                 'Clear DAT$(), SUCH$()
10500   DAT$(K)=""
10510   SUCH$(K)=""
10520 NEXT K
10530 RETURN
10540 '
10550 ' *** Einlesen MTGLMAX **************************************************
10560 '
10570 OPEN "I",#1,"B:MTGLZAHL.DAT"
10580   INPUT #1,MTGLMAX$
10590   MTGLMAX=VAL(MTGLMAX$)
10600 CLOSE #1
10610 RETURN 
10620 '
10630 ' *** Maske Feldnamen und Feldwerte-Platzhalter (Pünktchen) *************
10640 '
10650 ' IN: FELDMAX
10660 ' IN: MTGLZAHL
10670 PRINT HIGHOFF$;
10680 PRINT CURSOFF$;
10690 FOR K=1 TO FELDMAX
10700   S=FNCOL(FELD$(K))
10710   Z=FNROW(FELD$(K))
10720   PRINT FNCUR$(Z,S);MID$(FELD$(K),1,14);STRING$(FNLNG(FELD$(K)),".")
10730 NEXT
10740 PRINT CURSON$;
10750 MTGL$=STR$(MTGLZAHL)                                              'TODO ?
10760 RETURN
10770 '
10780 ' *** Maske Feldwerte-Platzhalter (Pünktchen) ***************************
10790 '
10800 ' IN: FELDMAX
10810 FOR K=1 TO FELDMAX
10820   S=FNCOL(FELD$(K))+14
10830   Z=FNROW(FELD$(K))
10840   PRINT FNCUR$(Z,S);STRING$(FNLNG(FELD$(K)),".")
10850 NEXT
10860 RETURN
10870 '
10880 ' *** Maske Feldwerte ***************************************************
10890 '
10900 ' IN: FELDMAX
10910 PRINT HIGHON$
10920 FOR K=1 TO FELDMAX
10930   S=FNCOL(FELD$(K))+14
10940   Z=FNROW(FELD$(K))
10950   PRINT FNCUR$(Z,S);DAT$(K)
10960 NEXT
10970 RETURN
10980 '
10990 ' *** Fielden der Stammsatz Random X.S.-Datei ************************OK*
11000 '
11010 OPEN "R",#2,"B:MV.DAT",462
11020 FIELD #2,236 AS DD1$,225 AS DD2$
11030 RETURN
11040 '
11050 ' *** Einlesen der Felddaten von Disk ***********************************
11060 '
11070 GET #2,MTGLZAHL
11080 FOR LL=1 TO FELDMAX:DAT$(LL)="":NEXT
11090 HLANG=1
11100 FOR K=1 TO 14:LNG=FNLNG(FELD$(K)):DAT$(K)=MID$(DD1$,HLANG,LNG):HLANG=LNG+HLANG:NEXT
11110 HLANG=1
11120 FOR K=15 TO 27:LNG=FNLNG(FELD$(K)):DAT$(K)=MID$(DD2$,HLANG,LNG):HLANG=LNG+HLANG:NEXT
11130 RETURN
11140 '
11150 ' *** Abspeichern der Felddaten auf Disk ********************************
11160 '
11170 GOSUB 12880       'NEUES VN-BLATT ANLEGEN   DARF NUR STS-ERF. ANSPRINGEN!
11180 FOR K=1 TO 27:LNG=FNLNG(FELD$(K)):DAT$(K)=DAT$(K)+STRING$(LNG-LEN(DAT$(K))," "):NEXT
11190 HH$=""
11200 FOR K=1 TO 14:HH$=HH$+DAT$(K):NEXT:LSET DD1$=HH$
11210 HH$="":FOR K=15 TO 27:HH$=HH$+DAT$(K):NEXT:LSET DD2$=HH$:HH$=""
11220 PRINT FNCUR$(21,1);
11230 PUT #2,MTGLZAHL
11240 N$="Die Daten sind auf Diskette abgespeichert.":GOSUB 12020 'Nachricht
11250 RETURN
11260 '
11270 ' *** Ja/Nein - Frage ***************************************************
11280 '
11290 ' IN:  Q$ - Frage
11300 ' OUT: JA 1 = true, 0 = false
11310 N$=Q$+" (J=Ja/N=Nein)":GOSUB 12020                             'Nachricht
11320 PRINT HOME$;" ";
11330 PRINT FNCUR$(23,1);DELLINE$;FNCUR$(24,1);DELLINE$;HOME$
11340 PRINT CURSOFF$;
11350 ANSWER$=INPUT$(1)
11360 PRINT CURSON$;BSP$;
11370 'IF ANSWER$="J" OR ANSWER$="j" THEN JA=1:GOSUB 14060:RETURN 'F-Tastenleiste
11380 IF ANSWER$="J" OR ANSWER$="j" THEN JA=1:RETURN            'F-Tastenleiste
11390 IF ANSWER$="N" OR ANSWER$="n" THEN JA=0:RETURN
11400 GOTO 11340
11410 '
11420 ' *** Testbereich 1 (Alpha) *********************************************
11430 '
11440 DRIN=0
11450 IF AASC>57 THEN IF AASC<127 THEN DRIN=1:RETURN
11460 IF AASC>31 THEN IF AASC<48 THEN DRIN=1
11470 RETURN
11480 '
11490 ' *** Testbereich 2 (Numerisch) *****************************************
11500 '
11510 DRIN=0
11520 IF AASC>47 AND AASC<58 THEN DRIN=1
11530 RETURN
11540 '
11550 ' *** Testbereich 3 (Alphanumerisch) ************************************
11560 '
11570 DRIN=0
11580 IF AASC>31 THEN IF AASC<127 THEN DRIN=1
11590 RETURN
11600 '
11610 ' *** Testbereich 4 (Numerisch + "/" und ".") ***************************
11620 '
11630 DRIN=0
11640 IF AASC>45 THEN IF AASC<58 THEN DRIN=1
11650 RETURN
11660 '
11670 ' *** DATUMS-TEST (bei VN und M-Liste drucken) **************************
11680 '
11690 IF VAL(TEST$)=0 OR VAL(TEST$)>31 THEN JA=0                           'TAG
11700 RETURN
11710 IF VAL(TEST$)=0 OR VAL(TEST$)>12 THEN JA=0                         'MONAT
11720 RETURN
11730 IF VAL(TEST$)=0 THEN JA=0                                           'JAHR
11740 RETURN
11750 '
11760 ' *** EXIT für ERFASSEN *************************************************
11770 '
11780 K=1:FOR II=1 TO 30:A$=INKEY$:NEXT:A$=""
11790 PRINT FNCUR$(21,1);DELLINE$
11800 IF INSTR(SPACE$(25),DAT$(K))<>0 THEN K=K+1:IF K<28 THEN 11800 ELSE MTGLZAHL=MTGLZAHL-1:MTGL$=STR$(MTGLZAHL):GOTO 11830
11810 Q$="Wollen Sie diese Daten abspeichern?":GOSUB 11310       'Ja/Nein-Frage
11820 IF JA=1 THEN GOSUB 11170:N$="Daten werden abgespeichert.":GOSUB 12020 ELSE N$="Daten werden nicht abgespeichert.":GOSUB 12020:M=MTGLZAHL-1:MTGL$=STR$(M) 'Nachricht 'Nachricht
11830 OPEN "O",#1,"B:MTGLZAHL.DAT":PRINT #1,MTGL$:CLOSE #1
11840 MTGLMAX=MTGLZAHL:MTGL$=STR$(MTGLZAHL)
11850 RETURN
11860 '
11870 ' *** Voreinstellungen im ERFASSEN  (Defaults) **************************
11880 '
11890 DAT$(5)="10***"
11900 DAT$(7)=""11**""
11910 DAT$(11)="12**"
11920 DAT$(12)="13******"
11930 DAT$(21)="3"
11940 DAT$(22)="Nein"
11950 DAT$(23)="Nein"
11960 DAT$(25)="Nein"
11970 RETURN
11980 '
11990 ' *** Nachricht anzeigen *********************************************OK*
12000 '
12010 ' IN: N$ - Nachricht
12020 PRINT FNCUR$(21,2);DELLINE$;INVON$;" ";N$;" ";INVOFF$
12030 RETURN
12040 '
12050 ' *** Bildschirmmaske - Basis ****************************************OK*
12060 '
12070 ' IN: MT$ - Menu title
12080 ' IN: MI$ - Menu ID
12090 PRINT CLS$;
12100 PRINT HIGHOFF$;
12110 PRINT "00**********************";
12120 PRINT FNCUR$(1,(80-LEN(MT$))/2);MT$;FNCUR$(1,72);MI$
12130 PRINT STRICH$
12140 PRINT FNCUR$(21,1);
12150 PRINT STRICH$
12160 RETURN
12170 '
12180 ' *** Funktionstasten Leiste (KEY LABELS) *******************************
12190 '
12200 ' IN: FKEYS$ - Additional function key labels 
12210 PRINT HIGHOFF$;
12220 PRINT FNCUR$(23,1);"<F1>=Hilfe <F2>=Zurück <F3>=Anfang <F4>=Hauptmenü ";
12230 PRINT FKEYS$
12240 RETURN
12250 '
12260 ' *** restliche funktionstasten bei Verw.-nachweis ausgeben
12270 '
12280 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=Vorwärts             <F8>=Drucken";
12290 PRINT HOME$;" "
12300 RETURN
12310 '
12320 ' *** Eingabe bei Menübildern *******************************************
12330 '
12340 ' IN:  NKEYMAX - max valid numeric key
12350 ' OUT: FKEY - number of pressed function key or 0
12360 ' OUT: NKEY - number of pressed numeric key or 0
12370 PRINT CURSOFF$
12380 NKEY=0:FKEY=0
12390 FOR I=1 TO 30:A$=INKEY$:NEXT                              'Key debouncing
12400 A$=INPUT$(1)
12410 IF A$>"0" AND ASC(A$)<48+NKEYMAX+1 THEN NKEY=VAL(A$):PRINT CURSON$:RETURN
12420 IF A$>CHR$(240) AND A$<CHR$(245) THEN FKEY=ASC(A$)-240:PRINT CURSON$:RETURN
12430 N$="Sie haben eine falsche Taste gedrückt - Bitte neue Auswahl treffen.":GOSUB 12020 'Nachricht
12440 GOTO 12400
12450 '
12460 ' *** Rückkehr ins Hauptmenü ? ******************************************
12470 '
12480 Q$="Sie wollen wirklich ins Hauptmenü?":GOSUB 11310        'Ja/Nein-Frage
12490 IF JA AND MT$="STAMMSATZ ÄNDERN" THEN GOSUB 12680
12500 IF JA=1 THEN CLOSE:RUN "DISKMENU.EXE" ELSE PRINT HOME$:PRINT FNCUR$(21,1);DELLINE$:GOSUB 12210:RETURN
12510 '
12520 ' *** Rückkehr aus Hilfe-Text? ***************************************OK*
12530 '
12540 N$="Zurück? (J=Ja)":GOSUB 12020                                'Nachricht
12550 GOSUB 11320                                          'TODO why not *****?
12560 IF JA=0 THEN 12540
12570 RETURN
12580 '
12590 ' *** Leerlesen von DAT$() (Ersetzt ERASE-Befehl) ***********************
12600 '
12610 FOR II=1 TO 27
12620   DAT$(II)=""
12630 NEXT
12640 RETURN
12650 '
12660 ' *** UPRO FÜR ÄNDERN-EXIT **********************************************
12670 '
12680 Q$="Wollen Sie die Daten abspeichern?":GOSUB 11310         'Ja/Nein-Frage
12690 IF JA THEN GOSUB 11180
12700 RETURN
12710 '
12720 ' **** VN - Subroutines, permitting one page handling *******************
12730 ' ****         by 04********** 21*******              *******************
12740 '
12750 '
12760 ' **** OPEN & FIELDING VERWENDUNGSNACHWEIS ******************************
12770 '
12780 OPEN "R",#3,"B:VN.NDX",4
12790 FIELD #3,4 AS POINT0$
12800 OPEN "R",#4,"B:VN.POI",8
12810 FIELD #4,4 AS POINT1$, 4 AS POINT2$
12820 OPEN "R",#5,"B:VN.DAT",560
12830 FIELD #5,240 AS DDD1$,240 AS DDD2$,80 AS DDD3$
12840 RETURN
12850 '
12860 ' *** VERWENDUNGSNACHWEIS FÜR NEUES MITGLIED ANLEGEN ********************
12870 '
12880 GET #3,1                            'letzte Seitennummer einlesen dann +1
12890 TMPOINT=VAL(POINT0$)+1
12900 LSET DDD1$=""                                    'leeren Record speichern
12910 LSET DDD2$=""
12920 LSET DDD3$=""
12930 PUT #5,TMPOINT
12940 LSET POINT0$=STR$(TMPOINT)             'letzte Seitennummer neu speichern
12950 PUT #3,1
12960 PUT #3,MTGLZAHL+1
12970 LSET POINT1$="0000"
12980 LSET POINT2$="0000"
12990 PUT #4,TMPOINT
13000 RETURN
13010 '
13020 ' **** ERSTE SEITE FINDEN & LESEN ***************************************
13030 '
13040 ' IN: MTGLZAHL
13050 GET #3,MTGLZAHL+1                                            'Index_Datei
13060 CPOINT=VAL(POINT0$)
13070 GET #5,CPOINT:GOSUB 14130                                       'Einlesen
13080 BLATT=1
13090 GET #4,CPOINT                                              'Pointer lesen
13100 RETURN
13110 '
13120 ' **** SEITE RÜCKWÄRTS **************************************************
13130 '
13140 TMPOINT=VAL(POINT1$)
13150 IF TMPOINT<>0 THEN 13190
13160 N$="Erste Seite":GOSUB 12020                                   'Nachricht
13170 FOR KK=1 TO 9999:NEXT                                              'Delay
13180 RETURN
13190 CPOINT=TMPOINT
13200 BLATT=BLATT-1
13210 GET #5,CPOINT:GOSUB 14130   
13220 GET #4,CPOINT:GOSUB 14290                             'Neue Pointer lesen
13230 HP=1:VP=1
13240 GOSUB 13500
13250 FOR KK=1 TO 100:NEXT                                               'Delay
13260 RETURN
13270 '
13280 ' **** SEITE VORWÄRTS  **************************************************
13290 '
13300 TMPOINT=VAL(POINT2$)
13310 IF TMPOINT=0 THEN 13380
13320 CPOINT=TMPOINT
13330 BLATT=BLATT+1
13340 GET #5,CPOINT:GOSUB 14130
13350 GET #4,CPOINT:GOSUB 14290
13360 HP=1:VP=1
13370 RETURN
13380 N$="Letzte Seite"
13390 IF INSTR(SPACE$(20),DAT$(26))=0 THEN 13430 
13400 GOSUB 12020                                                    'Nachricht
13410 FOR KK=1 TO 20000:NEXT                                             'Delay
13420 RETURN
13430 Q$="Letzte Seite - Neue Seite anlegen?":GOSUB 11310        'Ja/Nein-Frage
13440 N$="":GOSUB 12020                                              'Nachricht
13450 IF JA=0 THEN 13500
13460 GOSUB 13740
13470 BLATT=BLATT+1
13480 GOSUB 14290
13490 HP=1:VP=1
13500 GOSUB 12210
13510 PRINT FNCUR$(24,1);"<F5>=Blättern rückwärts <F6>=vorwärts";
13520 PRINT TAB(54);"<F9>=Blatt drucken";
13530 PRINT HOME$;" "
13540 RETURN
13550 '
13560 ' **** ERSTE SEITE EINES MITGLIEDS ANLEGEN ******************************
13570 '
13580 GET #3,1
13590 LSET POINT0$=STR$(VAL(POINT0$)+1)                                  'NDX+1
13600 PUT #3,1
13610 LSET POINT1$="0000"                                        'Page pointers
13620 LSET POINT2$="0000"
13630 PUT #4,VAL(POINT0$)
13640 LSET POINT0$=POINT0$                                          'First page
13650 PUT #3,MTGLZAHL+1
13660 LSET DDD1$=""                                                 'Datenblatt
13670 LSET DDD2$=""
13680 LSET DDD3$=""
13690 PUT #5,VAL(POINT0$)
13700 RETURN
13710 '
13720 ' **** ALLGEMEIN EINE NEUE SEITE ANLEGEN ********************************
13730 '
13740 GET #3,1                                                           'NDX+1
13750 LSET POINT0$=STR$(VAL(POINT0$)+1)
13760 PUT #3,1:TMPOINT$=POINT0$    'Vektoren der bisher letzten Seite verbiegen
13770 LSET POINT2$=POINT0$                                'Verbiege Vektoren...
13780 PUT #4,CPOINT                                         '...der alten Seite
13790 LSET POINT1$=STR$(CPOINT)                             '...der neuen Seite
13800 LSET POINT2$="0000"                                                '.. ""
13810 CPOINT=VAL(TMPOINT$)                                       'Nr neue Seite
13820 PUT #4,CPOINT
13830 LSET DDD1$=""                                           'Write empty page
13840 LSET DDD2$=""
13850 LSET DDD3$="" 
13860 PUT #5,CPOINT
13870 DATEN$=SPACE$(20)
13880 FOR II=1 TO 26
13890   DAT$(II)=DATEN$
13900 NEXT
13910 RETURN
13920 '
13930 ' **** SEITE ABSPEICHERN ************************************************
13940 '
13950 DATEN$=""
13960 FOR II=1 TO 12
13970   DATEN$=DATEN$+DAT$(II)
13980 NEXT
13990 LSET DDD1$=DATEN$:DATEN$=""
14000 FOR II=13 TO 24
14010   DATEN$=DATEN$+DAT$(II)
14020 NEXT
14030 LSET DDD2$=DATEN$:DATEN$=""
14040 FOR II=25 TO 26
14050   DATEN$=DATEN$+DAT$(II)
14060 NEXT 
14070 LSET DDD3$=DATEN$:DATEN$=""
14080 PUT #5,CPOINT
14090 RETURN
14100 '
14110 ' **** HACK ! (ZERSTÜCKELN DER DATEN-STRINGS) ***************************
14120 '
14130 FOR II=1 TO 12
14140   DAT$(II)=MID$(DDD1$,(II-1)*20+1,20)
14150 NEXT
14160 FOR II=13 TO 24
14170   DAT$(II)=MID$(DDD2$,(II-13)*20+1,20)
14180 NEXT 
14190 FOR II=25 TO 26
14200   DAT$(II)=MID$(DDD3$,(II-25)*20+1,20)
14210 NEXT 
14220 RETURN
14230 '
14240 ' **** UPRO DATENAUSGABE VN  ********************************************
14250 '
14260 ' IN: MTGLZAHL
14270 ' IN: NAMM$
14280 ' IN: BLATT
14290 PRINT DELHIGH$;HIGHON$
14300 PRINT FNCUR$(5,16);MTGLZAHL
14310 PRINT FNCUR$(5,29);NAMM$
14320 PRINT FNCUR$(3,8);BLATT
14330 FOR K=1 TO 26
14340   HOR=6-39*(K>13)
14350   VER=6+K+13*(K>13)
14360   FOR J=1 TO 3
14370     PRINT FNCUR$(VER,HOR+(J-1)*3);MID$(DAT$(K),-1+J*2,2)
14380   NEXT
14390   FOR J=1 TO 3
14400     PRINT FNCUR$(VER,HOR+13+(J-1)*3);MID$(DAT$(K),5+J*2,2)
14410   NEXT
14420   PRINT FNCUR$(VER,HOR+26);MID$(DAT$(K),13,8)
14430 NEXT
14440 PRINT HIGHOFF$
14450 RETURN
14460 '
14470 ' *** vn drucken ********************************************************
14480 '
14490 ' IN: BLATT
14500 ' IN: MITGLZAHL
14510 ' IN: NAMM$
14520 Q$="Ist der Drucker an?":GOSUB 11310                       'Ja/Nein-Frage
14530 IF JA=0 THEN 14730
14540 LPRINT STRICH$
14550 LPRINT
14560 LPRINT TAB(2);"Blatt:";BLATT
14570 LPRINT
14580 LPRINT TAB(2);"Mitglieds-Nr.:";MTGLZAHL;TAB(24);"Name:";NAMM$
14590 LPRINT
14600 FOR II=1 TO 13
14610   LOZ=II:GOSUB 14700
14620   LPRINT TAB(2);"Von:";LODAT1$;" Bis:";LODAT2$;" Amt:";LODAT3$;
14630   LOZ=II+13:GOSUB 14700
14640   LPRINT " Von:";LODAT1$;" Bis:";LODAT2$;" Amt:";LODAT3$
14650 NEXT 
14660 LPRINT
14670 LPRINT STRICH$
14680 RETURN
14690 '
14700 LODAT1$=LEFT$(DAT$(LOZ),2)+"."+MID$(DAT$(LOZ),3,2)+"."+MID$(DAT$(LOZ),5,2)
14710 LODAT2$=MID$(DAT$(LOZ),7,2)+"."+MID$(DAT$(LOZ),9,2)+"."+MID$(DAT$(LOZ),11,2)
14720 LODAT3$=RIGHT$(DAT$(LOZ),8)
14730 RETURN
14740 '
14750 ' *** upros MITGLIEDSLISTE DRUCKEN **************************************
14760 '
14770 ' *** MITGLIED DRUCKEN **************************************************
14780 '
14790 ' IN: MTGLZAHL
14800 LPRINT
14810 LPRINT TAB(5);MTGLZAHL;
14820 LPRINT TAB(10);DAT$(1);                                        'Nachnamen
14830 LPRINT TAB(36);DAT$( 2);                                        'Vornamen
14840 LPRINT TAB(65);"(";LEFT$(DAT$( 5),1);")";                         'Status
14850 AMT$=""
14860 IN=INSTR(DAT$(6),"22**"):IF IN<>0 THEN AMT$=MID$(DAT$(6),IN,6):GOTO 14890
14870 IN=INSTR(DAT$(6),"23**"):IF IN<>0 THEN AMT$=MID$(DAT$(6),IN,5):GOTO 14890
14880 IN=INSTR(DAT$(6),"24**"):IF IN<>0 THEN AMT$=MID$(DAT$(6),IN,3)
14890 LPRINT TAB(71);AMT$
14900 LPRINT TAB(36);DAT$(10)                                           'Straße
14910 LPRINT TAB(36);DAT$(11);                                    'Postleitzahl
14920 LPRINT TAB(41);DAT$(12)                                              'Ort
14930 LPRINT TAB(36);"Tel.:";DAT$(13)                                  'Telefon
14940 LPRINT
14950 IF INKEY$="" THEN 14980
14960 Q$="Drucken abbrechen ?":GOSUB 11310                       'Ja/Nein-Frage
14970 IF JA=1 THEN BRKFLAG=-1 ELSE BRKFLAG=0
14980 RETURN
14990 '
15000 ' *** KOPF DER MITGLIEDSLISTE *******************************************
15010 '
15020 ' IN: DATUM$
15030 LPRINT TAB(16);"MITGLIEDSLISTE 25*******************************"
15040 LPRINT 
15050 LPRINT TAB(32);"Stand:";DATUM$
15060 LPRINT
15070 LPRINT TAB(3);"Mitgl."; 
15080 LPRINT TAB(10);"Name";
15090 LPRINT TAB(36);"Vorname, Anschrift";
15100 LPRINT TAB(63);"Status";
15110 LPRINT TAB(71);"Amt"
15120 RETURN
15130 '
15140 ' *** UPRO ADRESSAUFKLEBER DRUCKEN **************************************
15150 '
15160 N$="Ist der Drucker bereit und positioniert? (J/N)":GOSUB 12020 'Nachricht
15170 GOSUB 11320                                           'TODO Why not 12520
15180 IF JA=0 THEN RETURN
15190 LPRINT "   ";DAT$(7)
15200 SPAC1=INSTR(DAT$(2)," ")
15210 LPRINT "   ";LEFT$(DAT$(2),SPAC1);DAT$(1)
15220 LPRINT
15230 LPRINT "   ";DAT$(10)
15240 LPRINT
15250 LPRINT "   ";DAT$(11);" "DAT$(12)
15260 LPRINT
15270 LPRINT
15280 LPRINT
15290 LPRINT
15300 LPRINT
15310 LPRINT
15320 RETURN
15330 '
15340 ' *** Stammsatz ausdrucken **********************************************
15350 '
15360 RESTORE 15470
15370 LPRINT STRICH$
15380 LPRINT
15390 LPRINT TAB(30);"Mitgliedsnummer  ";MTGLZAHL
15400 LPRINT
15410 LPRINT TAB(2);LEFT$(FELD$(1),14);DAT$(1);TAB(41);"Beruf:"
15420 FOR I=1 TO 6:READ A,B
15430   LPRINT TAB(2);LEFT$(FELD$(A),14);DAT$(A);
15440   IF B=0 THEN LPRINT:GOTO 15460
15450   LPRINT TAB(41);LEFT$(FELD$(B),14);DAT$(B);
15460 NEXT
15470 DATA 2,15,3,16,4,0,5,17,6,18,0,19
15480 FOR I=7 TO 14
15490   LPRINT TAB(2);LEFT$(FELD$(I),14);DAT$(I);
15500   LPRINT TAB(41);LEFT$(FELD$(I+13),14);DAT$(I+13);
15510 NEXT
15520 LPRINT
15530 LPRINT
15540 LPRINT
15550 LPRINT STRICH$
15560 RETURN
15570 '
15580 ' *** Mitgliedsvermerke *************************************************
15590 '
15600 ' IN: NAMM$ =
15610 LIN$=" ___ / ___ / ___" 
15620 LPRINT TAB(10);"Mitgliedsnr.:";TAB(28);"Name:";NAMM$
15630 LPRINT TAB(65);"Datum:"
15640 LPRINT
15650 LPRINT TAB(9);"26***************************";LIN$
15660 LRPINT
15670 LPRINT TAB(9);"27***************************";LIN$
15680 LRPINT
15690 LPRINT TAB(9);"28***************************";LIN$
15700 LRPINT
15710 LPRINT TAB(9);"29***************************";LIN$
15720 LRPINT
15730 LPRINT TAB(9);"30***************************";LEFT$(LIN$,10)
15740 LRPINT
15750 LPRINT TAB(9);"31***************************";LIN$
15760 LRPINT
15770 LPRINT TAB(9);"32***************************";LIN$
15780 LRPINT
15790 LPRINT TAB(9);"33***************************";LEFT$(LIN$,4)
15800 LRPINT
15810 LPRINT TAB(9);"34***************************";LIN$
15820 LRPINT
15830 LPRINT TAB(9);"35***************************";LIN$
15840 LRPINT
15850 LPRINT TAB(9);"36***************************";LIN$
15860 LRPINT
15870 LPRINT TAB(9);"37***************************";LIN$
15880 LRPINT
15890 LPRINT TAB(9);"38***************************";LIN$
15900 RETURN
15910 '
15920 ' ############################# Hilfe-Texte #############################
15930 '
15940 ' *** Hilfe-Text "Mitgliederverwaltung" ********************************* 
15950 '
15960 MT$="HILFE":GOSUB 12090                                      'Basis Maske
15970 PRINT HOME$
15980 PRINT IN$;"Die MITGLIEDERVERWALTUNG bietet folgende Funktionen an :"
15990 PRINT 
16000 PRINT IN$;"1 - STAMMSATZPFLEGE"
16010 PRINT IN$;"    Bearbeiten von Mitgliedsdaten gemäß dem Personalstamm-"
16020 PRINT IN$;"    blatt." 
16030 PRINT
16040 PRINT IN$;"2 - VERWENDUNGSNACHWEIS"
16050 PRINT IN$;"    Bearbeiten von Mitgliedsdaten, die Ämter umd Amtsperioden"
16060 PRINT IN$;"    eines Mitglieds enthalten."
16070 PRINT
16080 PRINT IN$;"3 - MITGLIEDSLISTE DRUCKEN"
16090 PRINT IN$;"    Ausdruck von Mitgliedslisten nach verschiedenen Gesichts-"
16100 PRINT IN$;"    punkten."
16110 PRINT
16120 PRINT IN$;"Wählen Sie aus, indem Sie die zugehörige Zahlentaste drücken."
16130 GOSUB 12540                                      'Rückkehr aus Hilfe-Text
16140 RETURN
16150 '
16160 ' *** Hilfe-Text "Stammsatzpflege" ************************************** 
16170 '
16180 MT$="HILFE":GOSUB 12090                                      'Basis Maske
16190 PRINT HOME$
16200 PRINT IN$;"Die STAMMSATZPFLEGE bietet folgende Funktionen an:"
16210 PRINT
16220 PRINT IN$;"1 - STAMMSATZ ANLEGEN"
16230 PRINT IN$;"    Erfassen der Daten eines neuen Mitglieds."
16240 PRINT
16250 PRINT IN$;"2 - STAMMSATZ ÄNDERN"
16260 PRINT IN$;"    Verändern eines bereits bestehenden Stammsatzes."
16270 PRINT
16280 PRINT IN$;"3 - STAMMSATZ LÖSCHEN"
16290 PRINT IN$;"    Verändern des Mitgliedsstatus auf `ausgetreten`."
16300 PRINT
16310 PRINT IN$;"4 - STAMMSATZ DRUCKEN"
16320 PRINT IN$;"    Ausdrucken eines kompletten Stammsatzes."
16330 PRINT
16340 PRINT IN$;"5 - STAMMSATZ SUCHEN"
16350 PRINT IN$;"    Suchen vom Stammsätzen nach bestimmten Gesichtspunkten."
16360 PRINT
16370 PRINT IN$;"Wählen Sie aus, indem Sie die zugehörige Zahlentaste drücken."
16380 GOSUB 12540                                      'Rückkehr aus Hilfe-Text
16390 RETURN
16400 '
16410 ' *** Hilfe-Text "Stammsatz anlegen" ************************************
16420 '
16430 MT$="HILFE":GOSUB 12090                                      'Basis Maske
16440 PRINT HOME$
16450 PRINT IN$;"Sie können jetzt Daten eines neuen Mitglieds eingeben, jedoch"
16460 PRINT IN$;"nicht die eines bestehenden Mitglieds ändern."
16470 PRINT
16480 PRINT IN$;"Die Mitgliedsnummer wird laufend vergeben."
16490 PRINT
16500 PRINT IN$;"Während der Dateneingebe können Sie mit der DELETE-Taste"
16510 PRINT IN$;"korrigieren. Ist ein Feld ausgefüllt worden, muß RETURN"
16520 PRINT IN$;"gedrückt werden, da sonst die Eingabe nicht gespeichert wird."
16530 PRINT IN$;"Mit den Cursortasten kann das Feld gewechselt werden."
16540 PRINT
16550 PRINT IN$;"`=>`-Felder müssen ausgefüllt werden."
16560 PRINT
16570 PRINT IN$;"Weitere Funktionstasten:"
16580 PRINT IN$;"<F7>  Diesen Stammsatz speichern."
16590 GOSUB 12540                                      'Rückkehr aus Hilfe-Text
16600 MT$="STAMMSATZ ANLEGEN"
16610 DATENAUSG=1                                                          '???
16620 RETURN
16630 '
16640 ' *** Hilfe-Text "Stammsatz ändern (I)" *********************************
16650 '
16660 MT$="HILFE":GOSUB 12090                                      'Basis Maske
16670 PRINT HOME$
16680 PRINT IN$;"Sie können jetzt einen bestehenden Stammsatz ändern."
16690 PRINT
16700 PRINT IN$;"Geben Sie die Mitgliedsnummer des Stammsatzes ein."
16710 PRINT IN$;"Bei Tippfehlern können Sie mit der DELETE-Taste korrigieren."
16720 GOSUB 12540                                      'Rückkehr aus Hilfe-Text
16730 RETURN
16740 '
16750 ' *** Hilfe-Text "Stammsatz ändern (II)" ********************************
16760 '
16770 MT$="HILFE":GOSUB 12090                                      'Basis Maske
16780 PRINT HOME$
16790 PRINT IN$;"Sie können jetzt einen bestehenden Stammsatz ändern."
16800 PRINT
16810 PRINT IN$;"Während der Dateneingabe können Sie mit der DELETE-Taste"
16820 PRINT IN$;"korrigieren. Ist ein Feld geändert worden, muß RETURN"
16830 PRINT IN$;"gedrückt werden, da sonst die Änderung nicht gespeichert wird."
16840 PRINT IN$;"Mit den Cursortasten kann das Feld gewechselt werden."
16850 PRINT
16860 PRINT IN$;"`=>`-Felder müssen ausgefüllt werden."
16870 PRINT
16880 PRINT IN$;"Weitere Funktionstasten:"
16890 PRINT IN$;"<F7>  Diesen Stammsatz speichern.
16900 GOSUB 12540                                      'Rückkehr aus Hilfe-Text
16910 MT$="STAMMDATEN ÄNDERN":GOSUB 12090                          'Basis Maske
16920 GOSUB 12210                                               'F-Tastenleiste
16930 RETURN
16940 '
16950 ' *** Hilfe-Text "Stammsatz löschen" ************************************
16960 '
16970 MT$="HILFE":GOSUB 12090                                      'Basis Maske
16980 PRINT HOME$
16990 PRINT IN$;"Sie können jetzt den Status eines Mitglieds auf `ausgetreten`"
17000 PRINT IN$;"ändern. Damit wird der Stammsatz beim Ausdruck von Mitglieds-"
17010 PRINT IN$;"listen nicht berüchsichtigt."
17020 PRINT IN$;"Auf keinen Fall werden die Daten WIRKLICH gelöscht."
17030 PRINT
17040 PRINT IN$;"Geben Sie die Mitgliedsnummer des Stammsatzes ein."
17050 PRINT IN$;"Bei Tippfehlern können Sie mit der DELETE-Taste korrigieren."
17060 GOSUB 12540                                      'Rückkehr aus Hilfe-Text
17070 RETURN
17080 '
17090 ' *** Hilfe-Text "Stammsatz suchen" *************************************
17100 '
17110 IF DRUCKFLAG THEN 17810                                              '???
17120 MT$="HILFE":GOSUB 12090                                      'Basis Maske
17130 PRINT HOME$
17140 PRINT IN$;"Sie können jetzt Stammsätze suchen."
17150 PRINT
17160 PRINT IN$;"1 - Suchen nach Nummern"
17170 PRINT IN$;"    Geben Sie ein Intervall von Mitgliedsnummern ein."
17180 PRINT IN$;"    Es werden alle Stammsätze ausgegeben, deren Nummern im"
17190 PRINT IN$;"    Intervall liegen."
17200 PRINT
17210 PRINT IN$;"2 - Suchen nach Maske"
17220 PRINT IN$;"    Sie füllen ein Formular aus. Diejenigen Stammsätze, die"
17230 PRINT IN$;"    mit dem Formular übereinstimmen, werden ausgegeben."
17240 PRINT IN$;"    Leere Felder bezeichnen beliebige Stammsatzdaten."
17250 PRINT IN$;"    Ein * zu Beginn des Feldes bedeutet: Das folgende Wort"
17260 PRINT IN$;"    darf sich an beliebiger Stelle des Feldes befinden."
17270 PRINT IN$;"    Ein oder mehrere ? zu Beginn des Feldes stellen Platz-"
17280 PRINT IN$;"    halter für beliebige Zeichen dar."
17290 PRINT
17300 PRINT IN$;"Wählen Sie aus, indem Sie die zugehörige Zahlentaste drücken."
17310 GOSUB 12540                                      'Rückkehr aus Hilfe-Text
17320 MT$="STAMMSATZ SUCHEN"
17330 RETURN
17340 ' 
17350 ' *** Hilfe-Text für "Stammsatz drucken" ********************************
17360 '
17370 MT$="HILFE":GOSUB 12090                                      'Basis Maske
17380 PRINT HOME$
17390 PRINT IN$;"Sie können jetzt einen kompletten Stammsatz, wie er auf dem"
17400 PRINT IN$;"Bildschirm steht, ausdrucken."
17410 PRINT
17420 PRINT IN$;"Geben Sie die Mitgliedsnummer des Stammsatzes ein."
17430 PRINT IN$;"Bei Tippfehlern können Sie mit der DELETE-Taste korrigieren."
17440 GOSUB 12540                                      'Rückkehr aus Hilfe-Text
17450 MT$="STAMMSATZ DRUCKEN"
17460 RETURN
17470 '
17480 ' *** Hilfe-Text "Verwendungsnachweis (I)" ******************************
17490 '
17500 MT$="HILFE":GOSUB 12090                                      'Basis Maske
17510 PRINT HOME$
17520 PRINT IN$;"Sie können jetzt einen Verwendungsnachweis ändern."
17530 PRINT
17540 PRINT IN$;"Geben Sie die Mitgliedsnummer des Verwendungsnachweises ein."
17550 PRINT IN$;"Bei Tippfehlern können Sie mit der DELETE-Taste korrigieren."
17560 GOSUB 12540                                      'Rückkehr aus Hilfe-Text
17570 RETURN
17580 '
17590 ' *** Hilfe-Text "Verwendungsnachweis (II)" *****************************
17600 '
17610 MT$="HILFE":GOSUB 12090                                      'Basis Maske
17620 PRINT HOME$
17630 PRINT IN$;"Sie können jetzt einen bestehenden Verwendungsnachweis ändern."
17640 PRINT
17650 PRINT IN$;"Während der Dateneingabe können Sie mit der DELETE-Taste"
17660 PRINT IN$;"korrigieren. Ist ein Feld ausgefüllt worden, muß RETURN"
17670 PRINT IN$;"gedrückt werden, da sonst die Änderung nicht gespeichert"
17680 PRINT IN$;"wird. Mit den Cursortasten kann das Feld gewechselt werden."
17690 PRINT
17700 PRINT IN$;"Weitere Funktionstasten:"
17710 PRINT IN$;"<F5>  Zurückblättern (ohne die jetzige Seite zu speichern)"
17720 PRINT IN$;"<F6>  Vorblättern (ohne die jetzige Seite zu speichern)"
17730 PRINT IN$;"<F7>  Die aktuelle Seite speichern."
17740 PRINT IN$;"<F9>  Die aktuelle Seite ausdrucken."
17750 GOSUB 12540                                      'Rückkehr aus Hilfe-Text
17760 MT$="VERWENDUNGSNACHWEIS"
17770 RETURN
17780 '
17790 ' *** Hilfe-Text "Mitgliedsliste drucken" *******************************
17800 '
17810 MT$="HILFE":GOSUB 12090                                      'Basis Maske
17820 PRINT HOME$
17830 PRINT IN$;"Sie können jetzt eine Mitgliedsliste drucken." 
17840 PRINT
17850 PRINT IN$;"1 - Suchen & Drucken"
17860 PRINT IN$;"    Sie können Stammsätze nach verschiedenen Gesichtspunkten"
17870 PRINT IN$;"    suchen und drucken."
17880 PRINT
17890 PRINT IN$;"2 - Komplett drucken"
17900 PRINT IN$;"    Es wird eine komplette Mitgliedsliste ausgedruckt."
17910 PRINT IN$;"    Ausgetretene Mitglieder werden nicht berücksichtigt."
17920 PRINT
17930 PRINT IN$;"3 - Datenblatt drucken"
17940 PRINT IN$;"    Der Stammsatz und der Verwendungsnachweis werden im"
17950 PRINT IN$;"    Bildschirmformat mit einer Liste von Mitgliedsvermerken"
17960 PRINT IN$;"    ausgedruckt."
17970 PRINT
17980 PRINT IN$;"Wählen Sie aus, indem Sie die zugehörige Zahlentaste drücken."
17990 GOSUB 12540                                      'Rückkehr aus Hilfe-Text
18000 MT$="MITGLIEDSLISTE AUSDRUCKEN"
18010 RETURN
18020 '
18030 ' *** Program Error Trap Nachricht **************************************
18040 '
18050 GOSUB 12090                                                  'Basis Maske
18060 PRINT HOME$
18070 PRINT IN$;"14*********************"
18080 PRINT
18090 PRINT IN$;"ein Programmfehler ist aufgetreten."
18100 PRINT   
18110 PRINT IN$;"- Notieren Sie folgende Fehlerinformation:"
18120 PRINT IN$;"    ERL (Error Line):";ERL
18130 PRINT IN$;"    ERR (Error Code):";ERR
18140 PRINT IN$;"- Drücken Sie am Rechner auf das Knöpfchen hinten rechts."
18150 PRINT IN$;"19**************************"
18160 PRINT
18170 PRINT IN$;"                                           20***************"
18180 PRINT
18190 PRINT CURSON$
18200 RESET
18210 END
