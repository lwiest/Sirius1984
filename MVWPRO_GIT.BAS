10 ' RELEASE 01***** Neubearbeitung durch 04**********
11 '
12 ' LW 31-JAN-2026: Load MSBASIC.COM with /S:560 /F:5
97 '
98 '    *** Initialisierung, Dimensionierung *********************************
99 '
100 DEFINT A-Z                                    'Integerdefinition
110 DIM FELD$(27),SUCH$(27),DAT$(27)              'Arrays dimensionieren
120 ES$   =CHR$(27)                               'Einleitung Escapesequenz
130 DEF FNP$(Z,S)= ES$+"Y"+CHR$(31+Z)+CHR$(31+S)  'Cursor positionieren
140 DEF FNZ(F$)  = VAL(MID$(F$,17,2))             'Zeile des Feldnamens
150 DEF FNS(F$)  = VAL(MID$(F$,15,2))             'Spalte des Feldnamens
160 DEF FNL(F$)  = VAL(MID$(F$,19,2))             'Länge des Feldes
170 DEF FNB(F$)  = VAL(RIGHT$(F$,1))              'Gültigkeits-Test Flag
180 CL$   =ES$+"E"                                'Bildschirm löschen
190 INON$ =ES$+"p"                                'Invers ein
200 INOF$ =ES$+"q"                                'Invers aus
210 CUON$ =ES$+"y5"                               'Cursor ein
220 CUOF$ =ES$+"x5"                               'Cursor aus
230 DLI$  =ES$+"K"                                'Zeile löschen
270 HIGHI$=ES$+"("                                'Helle Schrift 
280 LOWI$ =ES$+")"                                'Dunkle Schrift
290 HIGHL$=ES$+"+"                                'Helle Schrift löschen
320 S$    =STRING$(79,"-")                        'Strich               
330 ' 
340 ON ERROR GOTO 41500
345 '
350 RESTORE 390:KEY=0:WHILE KEY>=0                   'Tastatur umbelegen
360 PRINT ES$"41"CHR$(KEY)CHR$(0);                   'Umbelegen UNSHIFT
370 PRINT ES$"42"CHR$(KEY)CHR$(0);                   'Umbelegen SHIFT 
380 READ KEY:WEND
390 DATA 11,26:   'ESC          CLR                  'Tastencodes
400 DATA 27,32:   'DEL CHR      INT
410 DATA 33,47:   'TABULATOR    INS
420 DATA 48,53:   'DEL/EOL      RVS
430 DATA 67,68:   'SCROL        LTRL
440 DATA 73,94:   'UNDL         RPT
450 DATA 97,-1:   'CONT         Endflag
460 '
480 PRINT ES$"41"CHR$(88)CHR$(200);                  'Cursor rauf
490 PRINT ES$"41"CHR$(89)CHR$(201);                  'Cursor runter
500 PRINT ES$"41"CHR$(98)CHR$(202);                  'Cursor links
510 PRINT ES$"41"CHR$(99)CHR$(203);                  'Cursor rechts
997 '
998 '   *** Beginn des Mitgliederverwaltungsprogramms ************************
999 '
1000 B$="MITGLIEDERVERWALTUNG":P$="G-01000":GOSUB 41330
1010 A$=" Befindet sich die Daten-Diskette im Laufwerk B ?":GOSUB 41089:
     IF JA=1 THEN 1028       'B-Disk eingelegt ?
1020 A$=" Bitte legen Sie sie ein. Fertig ?":GOSUB 41089:
     IF JA=0 THEN RUN "DISKMENU.EXE"
1023 '
1024 '   *** Mitgliederverwaltungsmenü (MVW-Menü) ****************************
1025 '
1028 GOSUB 40934:GOSUB 50000
1029 OPEN "I",#2,"B:FELD.DAT":INPUT #2,FELDMAX:
     FOR K=1 TO FELDMAX:INPUT #2,FELD$(K):NEXT:CLOSE #2
1030 B$="MITGLIEDERVERWALTUNG":P$="G-01000":GOSUB 41330:KOM$="":GOSUB 41370
1035 VNFLAG=0
1040 PRINT TAB(26)"1 - Stammsatzpflege       ":PRINT 'Menütext
1050 PRINT TAB(26)"2 - Verwendungsnachweis   ":PRINT
1060 PRINT TAB(26)"3 - Mitgliedsliste drucken"
1070 FK=51:PRINT CUOF$:GOSUB 41400:PRINT CUON$       'Key check
1075 DRUCKFLAG=0                '      (???????)
1080 IF N THEN ON N GOTO 1530,10000,20000
1090           ON F GOTO 1100,1200,1030,1200
1100 GOSUB 40340:GOTO 1030                           'Help TEXT
1200 GOSUB 41450:GOTO 1030                           'Zurück & Hauptmenü
1498 ' *** Stammsatzpflege vorbereiten **************************************
1500 '
1520 DEL$  =CHR$(8)+"."+CHR$(8)                    'Zeichen links löschen
1530 OPEN "I",#2,"B:MTGLZAHL.DAT":INPUT #2,MTGL$
1540 MTGLMAX=VAL(MTGL$):CLOSE #2
1545 IF DRUCKFLAG THEN RETURN  '       (???????)
1547 '
1548 '   *** Stammsatzpflegemenü (STS-Menü) ***********************************
1549 '
1550 B$="STAMMSATZPFLEGE":P$="G-01100":GOSUB 41330:KOM$="":GOSUB 41370
1560 PRINT TAB(29)"1 - Stammsatz anlegen":PRINT      'Menütext
1570 PRINT TAB(29)"2 - Stammsatz ändern ":PRINT
1580 PRINT TAB(29)"3 - Stammsatz löschen":PRINT
1590 PRINT TAB(29)"4 - Stammsatz drucken":PRINT
1600 PRINT TAB(29)"5 - Stammsatz suchen "
1610 FK=53:PRINT CUOF$:GOSUB 41400:PRINT CUON$       'Key check
1620 IF N THEN GOSUB 40140:ON N GOTO 2000,2500,3510,5500,4000
1630                       ON F GOTO 1640,1660,1660,1850
1640 GOSUB 40370:GOTO 1550                           'Help TEXT
1660 GOSUB 41490:GOTO 1030
1850 GOSUB 41450:GOTO 1550                           'Hauptmenü
1997 '
1998 ' *** Stammsatz erfassen von 02************ ****************************
1999 '
2000 B$="STAMMSATZ ANLEGEN":P$="G-01101":PRINT CUON$:DATENAUSG=1:F=1
2010 MTGLZAHL=VAL(MTGL$)
2020 MTGLZAHL=MTGLZAHL+1:MTGL$=STR$(MTGLZAHL)
2030 GOSUB 41330:KOM$="<F7>=Abspeichern":GOSUB 41370:PRINT FNP$(3,30);
      "Mitgliedsnummer";FNP$(5,41);"Beruf:":GOSUB 40180  'Feldmaske
2035 GOSUB 41312                                          'Defaults
2040 IF DATENAUSG=1 THEN GOSUB 40330:DATENAUSG=0          'wenn vorher HILFE
2050 PRINT FNP$(21,1);DLI$                               'Cursorparameter
2060 PRINT HIGHI$;CUON$;FNP$(3,46);MTGL$
2070 Z=FNZ(FELD$(F)):S=FNS(FELD$(F))+14
2080 LANG=FNL(FELD$(F)):BEREICH=FNB(FELD$(F))
2090 PRINT FNP$(21,1);DLI$
2100 GOSUB 40000                                          'INPUT
2110 IF AASC=13 THEN IF MUSS$="=" THEN IF LEN(ANT$)<1 THEN                    
     A$=" Muß-Feld : Es muß etwas eingegeben werden ! ":GOSUB 41320:GOTO 2100
2120 IF AASC=13 THEN DAT$(F)=ANT$                         'Return
2130 IF AASC=13 THEN IF F<FELDMAX THEN F=F+1:GOTO 2070 ELSE  
     A$=" Letztes Feld ! ":GOSUB 41320:GOTO 2070          'ELSE zu IF F<
2145 IF HOCH<>0 THEN PRINT FNP$(Z,S);DAT$(F)+STRING$(LANG-LEN(DAT$(F)),"."):
     F=F+(HOCH=-1)*(-(F>1)+(F=1)*25)+(HOCH=1)*((F<27)-(F=27)*26):GOTO 2070
2160 IF ZURUCK=1 THEN GOSUB 40410:GOTO 2030               'F-Keys
2170 IF ZURUCK=3 THEN GOSUB 41240:PRINT LOWI$:GOSUB 41490:GOTO 1030
2180 IF ZURUCK=2 THEN GOSUB 41240:PRINT LOWI$:GOTO 1550
2190 IF ZURUCK=4 THEN GOSUB 41240:PRINT LOWI$:GOSUB 41450:GOTO 2100
2200 IF ZURUCK=7 THEN GOSUB 41009:GOSUB 40260:F=1:MTGLZAHL=MTGLZAHL+1:
        MTGL$=STR$(MTGLZAHL):GOSUB 41490:GOSUB 41312:GOSUB 40330:GOTO 2060
2210 A$=" Ungültige Eingabe - Bitte wiederholen ":GOSUB 41320:GOSUB 40020:
     GOTO 2110
2497 ' 
2498 ' *** Stammsatz ändern von 03************ ****************************
2499 '
2500 B$="STAMMSATZ ÄNDERN":P$="G-01102":F=1:PRINT CUON$
2510 GOSUB 41330:KOM$="":GOSUB 41370                     'Menütext
2520 PRINT LOWI$
2530 PRINT FNP$(4,23)"Bitte geben Sie die Mitgliedsnummer"
2540 PRINT FNP$(5,23)"des zu verändernden Stammsatzes ein,"
2550 PRINT FNP$(6,22)"und beenden Sie die Eingabe mit RETURN"
2560 PRINT FNP$(8,37)"=>..."
2570 LANG=3:BEREICH=2:Z=8:S=39
2580 GOSUB 40000                                           'INPUT
2590 IF ZURUCK=1 THEN GOSUB 40584:GOTO 2500                 'F-Keys
2600 IF ZURUCK=2 THEN 1550
2602 IF ZURUCK=3 THEN GOSUB 41490:GOTO 1030
2610 IF ZURUCK=4 THEN GOSUB 41450
2620 IF HOCH<>0 OR ZURUCK<>0 THEN GOTO 2570                'Errors
2630 IF LEN(ANT$)<1 THEN A$=" Muß-Feld : Sie müssen etwas eingeben.":
                         GOSUB 41320:GOTO 2570 ELSE MITGLIED=VAL(ANT$)
2635 IF MITGLIED=0 THEN MITGLIED=MTGLMAX+2
2640 IF MITGLIED>MTGLMAX THEN A$=" Diese Mitgliedsnummer existiert nicht ":
     GOSUB 41320:PRINT LOWI$;FNP$(8,39);"...";HIGHL$:ANT$="":GOTO 2580
2650 MTGLZAHL=MITGLIED:MTGL$=STR$(MITGLIED)
2655 FOR K=4 TO 8:PRINT FNP$(K,1);DLI$:NEXT:PRINT FNP$(21,1);DLI$
2660 GOSUB 40940                                          'Daten lesen
2670 PRINT LOWI$;FNP$(3,30);"Mitgliedsnummer";FNP$(5,41);"Beruf:"
2680 GOSUB 40180
2690 GOSUB 40330
2700 PRINT FNP$(3,46);MTGLZAHL
2710 KOM$="<F7>=Abspeichern"                                'Key labels
2730 GOSUB 41370
2740 S=FNS(FELD$(F))+14:Z=FNZ(FELD$(F)):A$="":GOSUB 41320
2750 LANG=FNL(FELD$(F)):BEREICH=FNB(FELD$(F))
2760 GOSUB 40000                                          'INPUT
2770 IF ZURUCK=1 THEN GOSUB 40598:GOTO 2670          'Hilfe       F-Keys 
2780 IF ZURUCK=2 THEN GOSUB 42000: GOTO 2500     'Eing.-Mtgl.-Nr.
2790 IF ZURUCK=3 THEN GOSUB 42000:GOSUB 41490: GOTO 1030  'Mvw-Menü
2800 IF ZURUCK=4 THEN GOSUB 41450:PRINT LOWI$;FNP$(24,1);"<F7>=Abspeichern"
                 ;FNP$(1,1);HIGHI$:GOTO 2760
2820 IF ZURUCK=7 THEN 3010                    'Daten Speichern
2830 IF HOCH=-1 THEN PRINT FNP$(Z,S);DAT$(F);STRING$(LANG-LEN(DAT$(F)),"."):
        IF F>1  THEN F=F-1:GOTO 2740  ELSE F=26:GOTO 2740 'Cursor hoch
2840 IF HOCH=1  THEN PRINT FNP$(Z,S);DAT$(F);STRING$(LANG-LEN(DAT$(F)),"."):
        IF F<27 THEN F=F+1:GOTO 2740  ELSE F= 1:GOTO 2740 'Cursor runter
2850 IF AASC=13 AND LEN(ANT$)<1 AND MUSS$="=" THEN
     A$=" Muß-Feld : Sie müssen etwas eingeben.":GOSUB 41320:GOTO 2760
2860 IF AASC=13 THEN DAT$(F)=ANT$:PRINT HIGHI$;FNP$(Z,S);DAT$(F);
        STRING$(LANG-LEN(DAT$(F)),".")                    'Return
2870 IF AASC=13 THEN IF F<FELDMAX THEN F=F+1:GOTO 2740
                                  ELSE F=1:GOTO 2740
2880 A$=" Ungültige Eingabe - Bitte wiederholen ":GOSUB 41320:
     FOR KK=1 TO 25000:NEXT:GOTO 2740
3010 GOSUB 41010                                          'Daten speichern
3020 A$=" Wollen Sie noch einen weiteren Datensatz ändern ?":GOSUB 41089
3040 IF JA=1 THEN 2500
3050 GOTO 1550                                            'Zum STS-Menü
3497 '
3498 ' *** Stammsatz löschen von 04********** *******************************
3499 '
3510 B$="STAMMSATZ LÖSCHEN":P$="G-01103":GOSUB 41330:KOM$="":GOSUB 41370
3520 PRINT FNP$(4,23);"Bitte geben Sie die Mitgliedsnummer" 'Menütext
3530 PRINT FNP$(5,25);"des zu löschenden Mitglieds ein."
3535 PRINT FNP$(6,15);"(Dabei wird nur der Status des Mitglieds verändert.)"
3540 PRINT FNP$(7,23);"Beenden Sie die Eingabe mit RETURN"
3550 PRINT FNP$(9,37);"=>..."
3560 LANG=3:BEREICH=2:Z=9:S=39
3570 PRINT HIGHI$:GOSUB 40010:PRINT LOWI$                  'INPUT
3580 IF ZURUCK=2 THEN 1550                                 'F-Keys
3590 IF ZURUCK=3 THEN GOSUB 41490: GOTO 1030
3600 IF ZURUCK=1 THEN GOSUB 40630:GOTO 3510
3610 IF ZURUCK=4 THEN GOSUB 41450:GOTO 3520
3620 IF ZURUCK>4 THEN HOCH=1 
3630 IF HOCH<>0 THEN A$=" Ungültige Eingabe - Bitte wiederholen.":GOSUB 41320:
        GOTO 3550
3640 MTGLZAHL=VAL(ANT$):IF MTGLZAHL=0 THEN MTGLZAHL=MTGLMAX+1  'Errors
3650 IF MTGLZAHL>MTGLMAX THEN A$=" Diese Mitgliedsnummer existiert nicht ":
     GOSUB 41320:GOTO 3550
3660 GOSUB 40940                                           'Daten einlesen
3670 GOSUB 41330:PRINT FNP$(3,30);"Mitgliedsnummer";FNP$(3,46);
     MTGLZAHL;FNP$(5,41);"Beruf:":GOSUB 40180:GOSUB 40330 'Feldmaske
3680 A$=" Endgültig löschen ?":GOSUB 41089 
3690 IF JA=0 THEN 3720                                     'Löschen ?
3700 DAT$(5)="ausgetreten     ":PRINT FNP$(9,16);HIGHI$;DAT$(5);LOWI$
3710 GOSUB 41010                                           'Daten abspeichern
3720 A$=" Soll noch jemand gelöscht werden ?":GOSUB 41089  'Nochmal ?
3730 IF JA=1 THEN 3510
             ELSE GOTO 1550
3998 ' *** Stammsatz suchen von 05*************** ***************************
4000 P$="G-01105"
4005 NUMSUCH1=0:NUMSUCH2=0:MTGL$=""
4010 PRINT LOWI$
4015 IF DRUCKFLAG THEN B$="SUCHEN & DRUCKEN" ELSE B$="STAMMSATZ SUCHEN"
4020 KOM$="":GOSUB 41330:GOSUB 41370
4030 PRINT CUOF$                        
4035 IF DRUCKFLAG THEN A$=" Drucken " ELSE A$=" Suchen "
4040 PRINT FNP$(6,30)"1 -";A$;"nach Nummern"
4050 PRINT FNP$(8,30)"2 -";A$;"nach Maske  ":GOSUB 41490
4060 FK=50:PRINT CUOF$:GOSUB 41400:PRINT CUON$
4070 IF F=1 THEN GOSUB 40650:GOTO 4010                    'F-Keys
4080 IF F=2 THEN IF DRUCKFLAG THEN GOSUB 41490:GOTO 20000 ELSE 1550
4090 IF F=3 THEN GOSUB 41490: GOTO 1030
4100 IF F=4 THEN GOSUB 41450:GOTO 4060
4110 IF N=1 THEN 4280
4130 F=1                                                   'Suchen nach Maske
4135 FOR I=1 TO 27:SUCH$(I)="":DAT$(I)="":NEXT
4140 KOM$="<F5>=Suchen ":GOSUB 5100
4145 IF DRUCKFLAG THEN KOM$="<F5>=Suchen & Drucken"
4147 GOSUB 41370
4150 Z=FNZ(FELD$(F)):S=FNS(FELD$(F))+14                    'Cursorparameter
4160 LANG=FNL(FELD$(F)):BEREICH=3
4170 PRINT FNP$(Z,S)
4180 ANT$="":GOSUB 40000                                   'INPUT
4200 IF ZURUCK=5 THEN MTGLZAHL=1:GOTO 4560                 'F-Keys
4210 IF ZURUCK=1 THEN GOSUB 40650:GOSUB 5100:GOSUB 40330:GOTO 4145
4220 IF ZURUCK=4 THEN GOSUB 41450:GOTO 4150
4230 IF ZURUCK=2 THEN 4000
4240 IF ZURUCK=3 THEN GOSUB 41490: GOTO 1030
4245 IF ANT$<>"" OR AASC=13 THEN SUCH$(F)=ANT$:DAT$(F)=ANT$
4250 IF HOCH=-1 THEN IF F>1 THEN F=F-1 ELSE F=26           'Cursor hoch
4260 IF HOCH=1 OR AASC=13 THEN IF F<27 THEN F=F+1 ELSE F=1 'Cursor runter
4265 A$="":GOSUB 41320
4270 GOTO 4150
4280 PRINT CL$:PRINT:PRINT                                 'Suchen nach Nummern
4290 Z=7:S=1:IF DRUCKFLAG THEN DRUCK$="gedruckt" ELSE DRUCK$="gesucht "
4300 BEREICH=2:LANG=3:PRINT CUON$;                         'Menütext
4310 GOSUB 41330:PRINT FNP$(4,5);"Maximale Mitgliedsnummer:";MTGLMAX:KOM$="":GOSUB 41370
4320 PRINT FNP$(3,1);:PRINT FNP$(Z,S);                    'Beginn bei Nr...
4330 PRINT LOWI$;"Ab welcher Mitgliedsnummer soll ";DRUCK$;" werden      =>..."
     :S=56
4340 GOSUB 40010                                           'INPUT
4350 IF ZURUCK=1 THEN GOSUB 40650:GOTO 4280                 'F-Keys
4360 IF ZURUCK=2 THEN 4000
4370 IF ZURUCK=3 THEN GOSUB 41490: GOTO 1030
4380 IF ZURUCK=4 THEN GOSUB 41450:S=1:GOTO 4320
4390 IF ANT$="" THEN S=1:GOTO 4320
4400 NUMSUCH1=VAL(ANT$)
4410 IF NUMSUCH1>MTGLMAX OR NUMSUCH1<1 THEN 4530
4420 Z=9:S=1
4425 A$="":GOSUB 41320
4430 PRINT FNP$(3,1):PRINT FNP$(Z,S);                    'Ende bei Nr...
4440 PRINT LOWI$;"Bis zu welcher Mitgliedsnummer soll ";DRUCK$;" werden  =>..."
     :S=56
4450 GOSUB 40010                                           'INPUT
4460 IF ZURUCK=1 THEN GOSUB 40650:GOTO 4280                 'F-Keys
4470 IF ZURUCK=2 THEN 4000
4480 IF ZURUCK=3 THEN GOSUB 41490: GOTO 1030
4490 IF ZURUCK=4 THEN GOSUB 41450:S=1:GOTO 4430
4500 IF ANT$="" THEN S=1:GOTO 4430
4510 NUMSUCH2=VAL(ANT$)
4520 IF NUMSUCH2<=MTGLMAX AND NUMSUCH1<=NUMSUCH2 THEN 4550
4530 A$=" Unlogische Eingabe - Bitte neu eingeben.":GOSUB 41320 'Errors
4540 Z=6:S=1:PRINT FNP$(9,1);DLI$;FNP$(Z,S);:Z=7:GOTO 4320
4550 MTGLZAHL=NUMSUCH1                                     'Feldmaske
4560 PRINT CUOF$
4565 IF DRUCKFLAG THEN GOSUB 55210
4567 CC$="  <F7>=Adressaufkleber"
4570 IF NUMSUCH1<>0 THEN KOM$="<F5>=Suchen rückwärts  <F6>=vorwärts"+CC$
                    ELSE KOM$="<F6>=Suchen vorwärts"+CC$
4572 GOSUB 5100
4575 PRINT FNP$(3,30);"Mitgliedsnummer"
4580 IF NUMSUCH1=0  AND MTGLZAHL<=MTGLMAX  THEN 4680
4590 IF NUMSUCH1<>0 AND MTGLZAHL<=NUMSUCH2 THEN 4680
4595 IF DRUCKFLAG THEN GOSUB 41490:GOTO 4000
4610 PRINT HIGHI$;CUOF$;:MTGLZAHL=MTGLZAHL-1
4620 A$=" Daten nicht vorhanden. Mit <F2> neue Suchmöglichkeit.":GOSUB 41320
4630 PRINT LOWI$;
4640 GOTO 4820
4680 GOSUB 40940                                           'Daten lesen
4690 IF NUMSUCH1<>0 THEN 4790
4700 FOR K=1 TO FELDMAX                                    'Daten durchsuchen
4710    IF LEFT$(SUCH$(K),1)<>"*" THEN 4740
4720    IF INSTR(DAT$(K),RIGHT$(SUCH$(K),LEN(SUCH$(K))-1))=0
                                THEN MTGLZAHL=MTGLZAHL+1:GOTO 4580
4730    GOTO 4780
4740    I=1
4750    IF MID$(SUCH$(K),I,1)="?" THEN I=I+1:GOTO 4750
4760    I=I-1
4770    IF INSTR(DAT$(K),RIGHT$(SUCH$(K),LEN(SUCH$(K))-I))<>I+1
                                THEN MTGLZAHL=MTGLZAHL+1:GOTO 4580
4780 NEXT
4790 GOSUB 40330                                          'Feldmaske
4800 MTGL$=STR$(MTGLZAHL) 
4810 PRINT CUOF$;FNP$(3,46);MTGL$
4815 IF DRUCKFLAG THEN GOSUB 55030:IF BRKFLAG THEN BRKFLAG=0:A$="":GOSUB
     41320:GOTO 4000
      ELSE A$="":GOSUB 41320:PRINT CUOF$:MTGLZAHL=MTGLZAHL+1:GOTO 4580
4820 ZURUCK=ASC(INPUT$(1))-240
4821 A$="":GOSUB 41320
4822 IF ZURUCK=1 THEN GOSUB 40650:GOSUB 5100:PRINT FNP$(3,30);
                      "Mitgliedsnummer":GOTO 4790
4824 IF ZURUCK=2 THEN IF NUMSUCH1<>0 THEN 4280 ELSE 4130
4826 IF ZURUCK=3 THEN GOSUB 41490:GOTO 1030
4827 IF ZURUCK=7 THEN GOSUB 55300:GOSUB 41370:A$="":GOSUB 41320:GOTO 4790
4828 IF ZURUCK=4 THEN GOSUB 41450:GOTO 4790
4830 IF ZURUCK=5 AND NUMSUCH1<>0 THEN IF MTGLZAHL>NUMSUCH1 THEN MTGLZAHL=
     MTGLZAHL-1:GOTO 4680
4832 IF ZURUCK=6 THEN 4850
4834 A$=" Falsche Eingabe ":GOSUB 41320
4836 GOTO 4820
4850 MTGLZAHL=MTGLZAHL+1:PRINT FNP$(21,1);DLI$;FNP$(3,1);
4860 GOTO 4580
5100 GOSUB 41330                                           'Maske darstellen
5110 IF DRUCKFLAG THEN B$="SUCHEN & DRUCKEN" ELSE B$="STAMMSATZ SUCHEN"
5120 GOSUB 40180
5125 IF NOT DRUCKFLAG THEN GOSUB 41370
5130 PRINT FNP$(5,41);"BERUF:"
5140 RETURN
5497 '
5498 ' *** Stammsatz ausdrucken von 02************ & 04********** ***********
5499 '
5500 B$="STAMMSATZ AUSDRUCKEN":P$="G-01104":GOSUB 41330:KOM$="":GOSUB 41370
5520 PRINT FNP$(4,23)"Bitte geben Sie die Mitgliedsnummer"
5530 PRINT FNP$(5,21)"des auszudruckenden Stammsatzes ein."
5535 PRINT FNP$(6,23)"Beenden Sie die Eingabe mit RETURN"
5540 PRINT FNP$(8,37);"=>..."
5550 LANG=3:BEREICH=2:Z=8:S=39
5560 PRINT HIGHI$:GOSUB 40010:PRINT LOWI$                  'INPUT
5570 IF ZURUCK=1 THEN GOSUB 40684:GOTO 5500                'F-Keys
5580 IF ZURUCK=2 THEN 1550
5590 IF ZURUCK=3 THEN PRINT LOWI$:GOSUB 41490: GOTO 1030
5600 IF ZURUCK=4 THEN GOSUB 41450:GOTO 5540
5610 IF ZURUCK>4 THEN HOCH=1
5620 IF HOCH<>0 THEN 5540
5630 MTGLZAHL=VAL(ANT$):IF MTGLZAHL=0 THEN MTGLZAHL=MTGLMAX+1 'Errors
5640 IF MTGLZAHL>MTGLMAX THEN A$=" Diese Mitgliedsnummer existiert nicht.":
      GOSUB 41320:GOTO 5540
5650 GOSUB 40940                                           'Daten lesen
5660 GOSUB 41330:PRINT FNP$(3,30);"Mitgliedsnummer";HIGHI$;
      FNP$(3,46);MTGLZAHL;LOWI$;FNP$(5,41);"Beruf:":GOSUB 40180:GOSUB 40330
5670 A$=" Wollen Sie diese Daten ausdrucken lassen ?":GOSUB 41089:
     IF JA=0 THEN 5500            'Ausdrucken ?
5680 A$=" Ist der Drucker bereit zur Ausgabe ?":GOSUB 41089:
     IF JA=0 THEN A$=" Bitte schalten Sie den Drucker ein.":
     GOSUB 41320:FOR DEL=0 TO 1000:NEXT:GOTO 5670     'Drucker bereit ?
5690 GOSUB 55600:GOTO 5500                              'Drucken
9997 '
9998 ' Verwendungsnachweis vorbereiten **************************************
9999 '
10000 OPEN "I",#2,"B:MTGLZAHL.DAT":INPUT #2,MTGL$:MTGLMAX=VAL(MTGL$):CLOSE #2
10015 DEL$=CHR$(8)+" "+CHR$(8):VNFLAG=-1
10017 '
10018 ' Verwendungsnachweis Menü ********************************************
10019 '
10020 B$="VERWENDUNGSNACHWEIS ":P$="G-01200":F=4:PRINT CUON$
10030 GOSUB 41330:KOM$="":GOSUB 41370:PRINT LOWI$
10040 PRINT FNP$(4,23)"Bitte geben Sie die Mitgliedsnummer"
10050 PRINT FNP$(5,20)"des zu ändernden Verwendungsnachweises ein"
10060 PRINT FNP$(6,22)"und beenden Sie die Eingabe mit RETURN"
10070 PRINT FNP$(8,37)"=>..."
10080 LANG=3:BEREICH=2:Z=8:S=39
10090 GOSUB 40000
10100 IF ZURUCK=1 THEN GOSUB 40700:GOTO 10020
10110 IF ZURUCK=2 OR ZURUCK=3 THEN GOSUB 41490:VNFLAG=0:GOTO 1030
10120 IF ZURUCK=4 THEN GOSUB 41450
10125 IF ZURUCK>4 THEN 10080
10130 IF HOCH<>0 THEN GOTO 10080
10140 IF LEN(ANT$)=0 THEN A$=" Muß-Feld : Sie müssen etwas eingeben.":
      GOSUB 41320:GOTO 10070 ELSE MITGLIED=VAL(ANT$):IF MITGLIED=0 THEN 10090
10150 IF MITGLIED>MTGLMAX THEN A$=" Diese Mitgliedsnummer existiert nicht ":
      GOSUB 41320:PRINT FNP$(3,1);LOWI$:GOTO 10070
10160 MTGLZAHL=MITGLIED:MTGL$=STR$(MITGLIED)
10198 ' *** Einlesen des Mitgliedsnamens **********
10199 PRINT FNP$(21,1);DLI$;
10200 GET #1,MITGLIED                                  'EINLESEN DATEN STS
10220 NNAM$=LEFT$(DD1$,25)                             'NACHNAME RAUSFISCHEN
10230 VNAM$=MID$(DD1$,26,25)                           'VORNAME  RAUSFISCHEN
10318 ' ** LEERZEICHEN HINTER NACHNAME ABSCHNEIDEN ***
10320    FOR I=LEN(NNAM$) TO 1 STEP -1
10330       IF MID$(NNAM$,I,1)<>" " THEN NNAM$=LEFT$(NNAM$,I):I=1
10340    NEXT
10350 NAMM$=NNAM$+" "+VNAM$
10360 GOSUB 50070                               'EINLESEN 1. VN-BLATT
10390 BLATT=1
10400 VP=1:HP=1                                 'INITIALISIERUNG
10510 F=8
10530 GOSUB 41330                               'MASKE OBEN UNTEN
10540 KOM$="<F7>=Abspeichern"
10550 GOSUB 41370                               'F-TASTEN BELEGUNG
10555 PRINT LOWI$
10560 PRINT FNP$(24,1);"<F5>=Blättern rückwärts <F6>=Vorwärts";TAB(54);
      "<F9>=Blatt drucken";
10569 PRINT FNP$(3,1)
10570 PRINT FNP$(3,1);FNP$(5,2);"Mitglieds-Nr.:"
10580 PRINT FNP$(3,2); "Blatt:"
10590 PRINT FNP$(5,24);"Name:"
10600 MSK$="Von:  .  .   Bis:  .  .   Amt:"
10602 FOR KK=7 TO 19 
10604   PRINT FNP$(KK,2)MSK$;TAB(43)MSK$
10606 NEXT
10610 GOSUB 52100                              'Ausgabe Daten
10620 LANG=2-(HP=7)*6                           'BEGIN OF THE INPUT-LOOP
10630 BEREICH=2-(HP=7)
10640 Z=VP+13*(VP>13)+6
10650 S=6-39*(VP>13)-(HP<7)*(HP-1)*3+(HP>3)*(HP<7)*4-(HP=7)*26
10655 A$="":GOSUB 41320                         'ERASE MESSAGE-LINE
10660 GOSUB 40010                               'INPUT
10663 IF HP<7 THEN IF AASC=13 THEN ANT$=STRING$(2-LEN(ANT$),"0")+ANT$ '+NULLEN
10664 IF HOCH<>0 OR AASC=13 THEN PRINT FNP$(Z,S);ANT$
10665 IF AASC=13 THEN IF HP<7 THEN MID$(DAT$(VP),-1+2*HP,2)=ANT$
                      ELSE DAT$(VP)=LEFT$(DAT$(VP),12)+ANT$+SPACE$(8-LEN(ANT$))
10666 IF AASC=13 AND HP<7 THEN JA=1:TEST$=ANT$:
      ON HP GOSUB 41232,41234,41236,41232,41234,41236:IF JA=0 THEN      
      A$=" Ungültiges Datum ":GOSUB 41320:GOTO 10660 'Datums-Check
10670 IF AASC=13 THEN IF HP=7 THEN HP=1:VP=VP-(VP<26)-(VP=26)*(-25):GOTO 10620
                     ELSE HP=HP+1:GOTO 10620     'NEUE SCREEN-POSITION
10680 IF HOCH<>0 THEN TEST$=MID$(DAT$(VP),-(HP<7)*2*HP-1-(HP=7)*14,
-(HP<7)*2-(HP=7)*8):PRINT FNP$(Z,S);TEST$
10690 IF HOCH<>0 THEN IF HP<7 AND NOT TEST$="  "
  THEN JA=1:ON HP GOSUB 41232,41234,41236,41232,41234,41236:IF JA=0 
THEN A$=" Ungültiges Datum ":GOSUB 41320:GOTO 10660
10770 IF HOCH=1  THEN VP=VP-(VP<26)-(VP=26)*(-25):GOTO 10620
10780 IF HOCH=-1 THEN VP=VP+(VP>1)-(VP=1)*25     :GOTO 10620
10790 IF HOCH=2  THEN HP=HP-(HP<7)-(HP=7)*(-6)   :GOTO 10620
10800 IF HOCH=-2 THEN HP=HP+(HP>1)+(HP=1)*(-6)   :GOTO 10620
10810 IF ZURUCK=1 THEN GOSUB 40716 :GOTO 10400  'HILFE - ZU INIT UND MASKE
10820 IF ZURUCK=2 THEN GOTO 10020                'ZU MTGL-EINGABE
10830 IF ZURUCK=3 THEN VNFLAG=0:GOSUB 41490:GOTO 1030
10840 IF ZURUCK=4 THEN GOSUB 41450:GOSUB 41395:GOTO 10660 
10850 IF ZURUCK=5 THEN GOSUB 50090:GOTO 10620
10860 IF ZURUCK=6 THEN GOSUB 50130:GOTO 10620
10870 IF ZURUCK=7 THEN GOSUB 50600:GOTO 10660    'ABSPEICHERN DANN INPUT
10880 IF ZURUCK=9 THEN GOSUB 52550:GOSUB 50162:GOTO 10655
19997 '
19998 ' *** Mitgliedsliste drucken von 05*************** *****************
19999 '
20000 DRUCKFLAG=-1:MTGL$="":DATUM$="  .  .  ":GOSUB 1500:GOSUB 40140
20010 PRINT CL$;LOWI$;CUOF$;:B$="MITGLIEDSLISTE DRUCKEN":P$="G-01300"
20020 KOM$="":GOSUB 41330:GOSUB 41370
20030 PRINT FNP$( 6,30);"1 - Suchen & Ausdrucken
20040 PRINT FNP$( 8,30);"2 - Komplett ausdrucken"
20045 PRINT FNP$(10,30);"3 - Datenblatt drucken"
20050 FK=51:PRINT CUOF$:GOSUB 41400:PRINT CUON$
20060 IF F=1 THEN GOSUB 40746:GOTO 20010
20070 IF F=2 OR F=3 THEN GOSUB 41490:DRUCKFLAG=0:GOTO 1030
20080 IF F=4 THEN GOSUB 41450:GOTO 20050
20090 IF N=1 THEN 20120
20100 IF N=2 THEN DRUCKFLAG=0:GOTO 20120
20110 IF N=3 THEN 23510
20120 GOSUB 41330:GOSUB 41370
20130 PRINT FNP$(7,13);"Geben Sie bitte das heutige Datum ein :   .  .  "
       :Z=7:LANG=2:BEREICH=2
20135 PRINT HIGHI$;CUON$
20140 FOR K=0 TO 2
20150   S=53+K*3
20160   VNFLAG=-1:GOSUB 40010:VNFLAG=0  'in order to activate right-left shift
20161 IF HOCH<>0 THEN JA=1:TEST$=MID$(DATUM$,K*3+1,2):
      ON K+1 GOSUB 41232,41234,41236:IF JA=0 THEN A$=" Ungültiges Datum ":
      GOSUB 41320:GOTO 20150
20162   IF ZURUCK=1 THEN GOSUB 40746:GOTO 20120
20164   IF ZURUCK=2 THEN 20000
20166   IF ZURUCK=3 THEN GOSUB 41490:DRUCKFLAG=0:GOTO 1030
20168   IF ZURUCK=4 THEN GOSUB 41450:GOTO 20150
20169   IF HOCH=-2 THEN IF K=0 THEN 20150
                               ELSE K=K-1:GOTO 20150
20170   IF HOCH=+2 THEN IF K=2 THEN K=0:GOTO 20150
                               ELSE K=K+1:GOTO 20150
20176   IF AASC<>13 THEN 20150
20180   ANT$=STRING$(2-LEN(ANT$),"0")+ANT$:PRINT FNP$(Z,S);ANT$
20185   TEST$=ANT$:JA=1:ON K+1 GOSUB 41232,41234,41236:IF JA=0 THEN
        A$=" Ungültige Datumseingabe ":GOSUB 41320:GOTO 20150
20190   MID$(DATUM$,K*3+1,2)=ANT$
20200 NEXT
20205 PRINT LOWI$;CUOF$
20210 A$=" Bitte den Drucker bereit machen. Fertig ?":GOSUB 41089:
      IF JA=0 THEN A$="":GOSUB 41320:GOTO 20120
20230 'Ausdrucken Anfang ***
20235 IF NOT DRUCKFLAG THEN PRINT FNP$(7,1);DLI$CUOF$:
      A$=" Ok. Mitgliedsliste wird gedruckt. Abbrechen mit Taste.":GOSUB 41320
20250 IF DRUCKFLAG THEN 4005
20255 PRINT FNP$(23,1);DLI$;:GOSUB 55210
20260 FOR MTGLZAHL=1 TO MTGLMAX
20270   GOSUB 40940:IF INSTR(DAT$(5),"ausgetreten")>0 THEN 20280
20274   GOSUB 55030:IF BRKFLAG THEN BRKFLAG=0:A$=" Drucken abgebrochen.":
        GOSUB 41320:PRINT CUOF$:GOTO 20290 ELSE GOSUB 41320:PRINT CUOF$
20280 NEXT
20290 GOSUB 41490:DRUCKFLAG=0:GOTO 20000
20599 ' Datenblatt drucken
23510 B$="DATENBLATT DRUCKEN":P$="G-01303":GOSUB 41330:KOM$="":GOSUB 41370
23520 PRINT FNP$(4,23)"Bitte geben Sie die Mitgliedsnummer"
23530 PRINT FNP$(5,24)"des zu druckenden Datenblattes ein."
23540 PRINT FNP$(7,23)"Beenden Sie die Eingabe mit RETURN"
23550 PRINT FNP$(9,37)"=>..."
23560 LANG=3:BEREICH=2:Z=9:S=39
23570 PRINT HIGHI$:GOSUB 40010:PRINT LOWI$                  'INPUT
23580 IF ZURUCK=2 THEN 20000                                'F-Keys
23590 IF ZURUCK=3 THEN GOSUB 41490: GOTO 1030
23600 IF ZURUCK=1 THEN GOSUB 40746:PRINT CUON$:B$="DATENBLATT DRUCKEN":
                       GOTO 23510
23610 IF ZURUCK=4 THEN GOSUB 41450:GOTO 23510
23620 IF ZURUCK>4 THEN HOCH=1 
23630 IF HOCH<>0 THEN A$=" Ungültige Eingabe - Bitte wiederholen.":GOSUB 41320:
        GOTO 23550
23640 MTGLZAHL=VAL(ANT$):IF MTGLZAHL=0 THEN MTGLZAHL=MTGLMAX+1  'Errors
23650 IF MTGLZAHL>MTGLMAX THEN A$=" Diese Mitgliedsnummer existiert nicht ":
     GOSUB 41320:GOTO 23550
23655 A$=" Ist der Drucker bereit zur Ausgabe ?":GOSUB 41089:IF JA=0 THEN 
      A$=" Bitte schalten Sie den Drucker ein.":GOSUB 41320:
      FOR DEL=0 TO 1000:NEXT:GOTO 23510     'Drucker bereit ?
23660 GOSUB 40940                                           'S.S.Daten einlesen
23661 NNAM$ = LEFT$(DD1$,25) : NNAM$ = LEFT$(NNAM$,INSTR(NNAM$," "))
23662 NAMM$ = NNAM$ + MID$(DD1$,26,25)
23670 GOSUB 55600                                           'S.S.drucken
23680 GOSUB 50070                                           'V.N Daten einlesen
23690 GOSUB 52570                                           'V.N Daten drucken
23700 GOSUB 56010                                           'Mitgliedsvermerke
23800 GOTO 23510
39996 ' *** Unterprogramme von 06*****, 07****, 08**** und 09**** ***********
39998 ' *** Eingaberoutine (INPUT) ******************************************
40000 MUSS$=MID$(FELD$(F),13,1)
40010 PRINT HIGHI$:ANT$="":I=1
40020 HOCH=0:ZURUCK=0:PRINT FNP$(3,1);" ":PRINT FNP$(Z,S+I-1);
40030 A$=INPUT$(1):AASC=ASC(A$)
40050 ON BEREICH GOSUB 41140,41180,41200,41220             'Bereichs check
40060 IF DRIN=1 THEN IF I<=LANG THEN PRINT A$;:ANT$=LEFT$(ANT$,I-1)+A$:
      I=I+1:GOTO 40030          ELSE GOTO 40030
40070 IF AASC=8 THEN IF I>1 THEN PRINT DEL$;:I=I-1:ANT$=LEFT$(ANT$,I-1):       
      GOTO 40020            ELSE GOTO 40030                'Delete
40075 IF VNFLAG THEN IF AASC=13 THEN PRINT LOWI$;STRING$(LANG-I+1," ");HIGHI$:
      RETURN 'BEI VN RETURN OHNE PUNKTE
40080 IF AASC=13  THEN PRINT LOWI$;STRING$(LANG-I+1,"."):RETURN 'Return
40090 IF AASC=200 THEN HOCH=-1:GOSUB 40130:RETURN          'Cursor hoch
40100 IF AASC=201 THEN HOCH=+1:GOSUB 40130:RETURN          'Cursor runter
40102 IF VNFLAG AND AASC=202 THEN HOCH=-2:GOSUB 40130:RETURN
40104 IF VNFLAG AND AASC=203 THEN HOCH=2 :GOSUB 40130:RETURN
40110 IF AASC>240 AND AASC<250 THEN ZURUCK=AASC-240:RETURN 'F-Keys
40120 A$=" Ungültige Eingabe - Bitte wiederholen ":GOSUB 41320:
      GOTO 40020
40128 ' *** Cursor hoch/runter Repeat Blockierung ***************************
40130   A$=INKEY$:IF A$>CHR$(199) THEN 40130 ELSE RETURN
40138 ' *** Disk I/O Variablen Initalisierung *******************************
40140   OPEN "I",#2,"B:MTGLZAHL.DAT"
40150   INPUT #2,MTGL$:MTGLZAHL=VAL(MTGL$):CLOSE #2
40160   FOR K=1 TO FELDMAX:DAT$(K)="":SUCH$(K)="":NEXT
40170 RETURN
40178 ' *** Feldnamen Maske *************************************************
40180   PRINT LOWI$CUOF$;
40190   FOR K=1 TO FELDMAX
40200     S=FNS(FELD$(K))
40210     Z=FNZ(FELD$(K))
40220     PRINT FNP$(Z,S);MID$(FELD$(K),1,14);STRING$(FNL(FELD$(K)),46)
40230   NEXT
40240   MTGL$=STR$(MTGLZAHL):PRINT CUON$;
40250 RETURN
40258 ' *** Pünktchen der Felder ********************************************
40260   PRINT FNP$(3,1);" "
40270   FOR K=1 TO FELDMAX
40280     S=FNS(FELD$(K))+14
40290     Z=FNZ(FELD$(K))
40300     PRINT FNP$(Z,S);STRING$(FNL(FELD$(K)),46)
40310   NEXT
40320 RETURN
40328 ' *** Felddaten ausgeben **********************************************
40330   PRINT HIGHI$
40331   FOR K=1 TO FELDMAX
40332     LANG=FNL(FELD$(K))
40333     E1=FNS(FELD$(K))+14
40334     E2=FNZ(FELD$(K))
40335     PRINT FNP$(E2,E1);DAT$(K)
40336   NEXT
40337 RETURN
40338 ' Help-Text 'Mitgliederverwaltung' 
40339 '
40340 B$="HILFE":GOSUB 41330:PRINT FNP$(3,1)
40342 PRINT "       Die MITGLIEDERVERWALTUNG bietet folgende Funktionen an :"
40344 PRINT 
40346 PRINT "        1 - STAMMSATZPFLEGE"
40348 PRINT "            Bearbeiten von Mitgliedsdaten gemäß dem Personal-"
40350 PRINT "            stammblatt." 
40351 PRINT
40352 PRINT "        2 - VERWENDUNGSNACHWEIS"
40354 PRINT "            Bearbeiten von Mitgliedsdaten, die Ämter umd Amts-"
40356 PRINT "            perioden eines Mitglieds enthalten."
40357 PRINT
40358 PRINT "        3 - MITGLIEDSLISTE DRUCKEN"
40360 PRINT "            Ausdruck von Mitgliedslisten nach verschiedenen"
40361 PRINT
40362 PRINT "            Gesichtspunkten."
40364 PRINT "    Wählen Sie aus, indem Sie die zugehörige Zahlentaste drücken."
40366 GOSUB 41480:RETURN
40367 '
40368 ' Help-Text 'Stammsatzpflege' 
40369 '
40370 B$="HILFE":GOSUB 41330:PRINT FNP$(3,1)
40374 PRINT "     Die STAMMSATZPFLEGE bietet folgende Funktionen an:"
40375 PRINT
40376 PRINT "      1 - STAMMSATZ ANLEGEN"
40378 PRINT "          Erfassen der Daten eines neuen Mitglieds."
40379 PRINT
40380 PRINT "      2 - STAMMSATZ ÄNDERN"
40382 PRINT "          Verändern eines bereits bestehenden Stammsatzes."
40383 PRINT
40384 PRINT "      3 - STAMMSATZ LÖSCHEN"
40386 PRINT "          Verändern des Mitgliedsstatus auf `ausgetreten'."
40387 PRINT
40388 PRINT "      4 - STAMMSATZ DRUCKEN"
40390 PRINT "          Ausdrucken eines kompletten Stammsatzes."
40391 PRINT
40392 PRINT "      5 - STAMMSATZ SUCHEN"
40394 PRINT "          Suchen vom Stammsätzen nach bestimmten Gesichtspunkten."
40396 GOSUB 41480:RETURN
40407 '
40408 ' Help-Text 'Stammsatz anlegen'
40409 '
40410 B$="HILFE":GOSUB 41330:PRINT FNP$(3,1)
40430 PRINT "   Sie können jetzt Daten eines neuen Mitglieds eingeben,"
40440 PRINT "   jedoch nicht die eines bestehenden Mitglieds ändern."
40442 PRINT
40460 PRINT "   Die Mitgliedsnummer wird laufend vergeben."
40462 PRINT
40480 PRINT "   Während der Dateneingebe können Sie mit der DELETE-Taste "
40490 PRINT "   korrigieren. Ist ein Feld ausgefüllt worden, muß RETURN"
40500 PRINT "   gedrückt werden, da sonst die Eingabe nicht gespeichert"
40510 PRINT "   wird. Mit den Cursortasten kann das Feld gewechselt werden."
40520 PRINT
40530 PRINT "   ` => '-Felder sollten ausgefüllt werden."
40532 PRINT
40540 PRINT "   Weitere Funktionstasten :"
40550 PRINT "   <F7>  Diesen Stammsatz abspeichern."
40580 GOSUB 41480:B$="STAMMSATZ ANLEGEN":DATENAUSG=1:RETURN
40581 '
40582 ' Help-Text 'Stammsatz ändern (I)'
40583 '
40584 B$="HILFE":GOSUB 41330:PRINT FNP$(3,1);
40586 PRINT "      Sie können jetzt einen bestehenden Stammsatz ändern."
40587 PRINT
40590 PRINT "      Geben Sie die Mitgliedsnummer des betreffenen Stammsatzes"
40592 PRINT "      ein. Bei Tippfehlern können Sie mit DELETE-Taste korri-"
40593 PRINT "      gieren."
40594 GOSUB 41480:RETURN   
40595 '
40596 ' Help-Text 'Stammsatz ändern (II)
40597 '
40598 B$="HILFE":GOSUB 41330:PRINT FNP$(3,1);
40600 PRINT "    Sie können jetzt einen bestehenden Stammsatz ändern."
40602 PRINT
40604 PRINT "    Während der Dateneingabe können Sie mit der DELETE-Taste"
40606 PRINT "    korrigieren. Ist ein Feld ausgefüllt, muß RETURN gedrückt"
40608 PRINT "    werden, da sonst die Änderung nicht gespeichert wird."
40610 PRINT "      Mit den Cursortasten kann das Feld zum nächsten oberen,"
40612 PRINT "    bzw. unteren Feld gewechselt werden."
40614 PRINT
40616 PRINT "    ` => '-Felder sollten ausgefüllt werden."
40618 PRINT
40620 PRINT "    Weitere Funktionstasten :"
40622 PRINT
40624 PRINT "    <F7>  Diesen Stammsatz speichern.
40626 GOSUB 41480:B$="STAMMDATEN ÄNDERN":GOSUB 41330:GOSUB 41370:RETURN
40627 '
40628 ' Help-Text 'Stammsatz löschen'
40629 '
40630 B$="HILFE":GOSUB 41330:PRINT FNP$(3,1)
40632 PRINT "    Sie können jetzt den Status eines Mitglieds auf `ausgetreten'"
40634 PRINT "    ändern. Damit wird der Stammsatz beim Ausdruck von Mitglieds-"
40636 PRINT "    listen nicht berüchsichtigt."
40638 PRINT "    Auf keinen Fall werden die Daten WIRKLICH gelöscht."
40640 PRINT
40642 PRINT "    Geben Sie die Mitgliedsnummer des betreffenden Stammsatzes "
40644 PRINT "    ein. Bei Tippfehlern können Sie mit der DELETE-Taste"
40645 PRINT "    korrigieren."
40646 GOSUB 41480:RETURN
40647 '
40648 ' Help-Text 'Stammsatz suchen'
40649 '
40650 IF DRUCKFLAG THEN 40746
40652 B$="HILFE":GOSUB 41330:PRINT FNP$(3,1)
40654 PRINT "    Sie können jetzt Stammsätze suchen."
40655 PRINT
40656 PRINT "    1 - Suchen nach Nummern"
40658 PRINT "        Geben Sie ein Intervall von Mitgliedsnummern ein."
40660 PRINT "        Es werden alle Stammsätze ausgegeben, deren Nummern"
40662 PRINT "        im Intervall liegt."
40663 PRINT
40664 PRINT "    2 - Suchen nach Maske"
40666 PRINT "        Sie füllen ein Formular aus. Diejenigen Stammsätze, die"
40668 PRINT "        mit dem Formular übereinstimmen, werden ausgegeben."
40670 PRINT "        Leere Felder bezeichnen beliebige Stammsatzdaten."
40672 PRINT "        Ein * zu Beginn des Feldes bedeutet : Das folgende Wort"
40674 PRINT "        darf sich an beliebiger Stelle des Feldes befinden."
40676 PRINT "        Ein oder mehrere ? zu Beginn des Feldes stellen Platz-"
40678 PRINT "        halter für beliebige Zeichen dar."
40679 PRINT
40680 GOSUB 41480:B$="STAMMSATZ SUCHEN":RETURN
40681 ' 
40682 ' Help-Text für 'Stammsatz drucken'
40683 '
40684 B$="HILFE":GOSUB 41330:PRINT FNP$(5,1)
40686 PRINT "   Sie können jetzt einen kompletten Stammsatz, wie er auf"
40688 PRINT "   dem Bildschirm steht, ausdrucken."
40690 PRINT
40692 PRINT "   Geben Sie die Mitgliedsnummer ein. Bei Tippfehlern können"
40694 PRINT "   Sie mit der DELETE-Taste korrigieren."
40696 B$="STAMMSATZ DRUCKEN":GOSUB 41480:RETURN
40697 '
40698 ' Help-Text 'Verwendungsnachweis (I)'
40699 '
40700 B$="HILFE":GOSUB 41330:PRINT FNP$(6,1)
40702 PRINT "    Sie können jetzt einen Verwendungsnachweis ändern."
40704 PRINT
40706 PRINT "    Geben Sie die Mitgliedsnummer des betreffenden "
40708 PRINT "    Verwendungsnachweises ein. Bei Tippfehlern können"
40710 PRINT "    Sie mit der DELETE-Taste korrigieren."
40712 GOSUB 41480:RETURN
40713 '
40714 ' Help-Text 'Verwendungsnachweis (II)'
40715 '
40716 B$="HILFE":GOSUB 41330:PRINT FNP$(3,1)
40718 PRINT "    Sie können jetzt einen bestehenden Verwendungsnachweis ändern."
40720 PRINT
40722 PRINT "    Während der Dateneingabe können Sie mit der DELETE-Taste"
40724 PRINT "    korrigieren. Ist ein Feld ausgefüllt worden, muß RETURN"
40726 PRINT "    gedrückt werden, da sonst die Änderung nicht gespeichert"
40728 PRINT "    wird. Mit den Cursortasten kann das Feld gewechselt werden."
40730 PRINT
40732 PRINT "    Weitere Funktionstasten :"
40734 PRINT "    <F5>  Zurückblättern (ohne die jetzige Seite zu speichern)"
40736 PRINT "    <F6>  Vorblättern    (ohne die jetzige Seite zu speichern)"
40738 PRINT "    <F7>  Die aktuelle Seite speichern"
40740 PRINT "    <F9>  Die aktuelle Seite ausdrucken"
40742 GOSUB 41480:B$="VERWENDUNGSNACHWEIS":RETURN
40743 '
40744 ' Help-Text 'Mitgliedsliste drucken'
40745 '
40746 B$="HILFE":GOSUB 41330:PRINT FNP$(4,1)
40748 PRINT "    Sie können jetzt eine Mitgliedsliste drucken lassen." 
40750 PRINT
40752 PRINT "    1 - Suchen & Drucken"
40754 PRINT "        Sie können Stammsätze nach verschiedenen Gesichtspunkten"
40756 PRINT "        suchen und drucken lassen."
40758 PRINT
40760 PRINT "    2 - Komplett drucken"
40762 PRINT "        Es wird eine komplette Mitgliedsliste ausgedruckt."
40764 PRINT "        Ausgetretene Mitglieder werden nicht berücksichtigt."
40766 PRINT
40768 PRINT "    3 - Datenblatt drucken"
40770 PRINT "        Der Stammsatz und der Verwendungsnachweis werden im"
40772 PRINT "        Bildschirmformat mit einer Liste von Mitgliedsver-"
40774 PRINT "        merken ausgedruckt."
40776 GOSUB 41480:B$="MITGLIEDSLISTE AUSDRUCKEN":RETURN
40778 '
40932 ' *** Fielden der Stammsatz Random X.S.-Datei *************************
40934   OPEN "R",#1,"B:MV.DAT",462
40935   FIELD #1,236 AS DD1$,225 AS DD2$
40936 RETURN
40938 ' *** Einlesen der Felddaten von Disk *********************************
40940   GET #1,MTGLZAHL
40950   FOR LL=1 TO FELDMAX:DAT$(LL)="":NEXT
40960   HLANG=1
40970   FOR K=1 TO 14:LANG=FNL(FELD$(K)):DAT$(K)=MID$(DD1$,HLANG,LANG):
         HLANG=LANG+HLANG:NEXT
40980   HLANG=1
40990   FOR K=15 TO 27:LANG=FNL(FELD$(K)):DAT$(K)=MID$(DD2$,HLANG,LANG):      
        HLANG=LANG+HLANG:NEXT
41000 RETURN
41008 ' *** Abspeichern der Felddaten auf Disk ******************************
41009   GOSUB 50060  'NEUES VN-BLATT ANLEGEN   DARF NUR STS-ERF. ANSPRINGEN!
41010   FOR K=1 TO 27:LANG=FNL(FELD$(K)):
        DAT$(K)=DAT$(K)+STRING$(LANG-LEN(DAT$(K))," "):NEXT
41020   HH$=""
41030   FOR K=1 TO 14:HH$=HH$+DAT$(K):NEXT:LSET DD1$=HH$
41040   HH$="":FOR K=15 TO 27:HH$=HH$+DAT$(K):NEXT:LSET DD2$=HH$:HH$=""
41050   PRINT FNP$(21,1);
41060   PUT #1,MTGLZAHL
41070   A$=" Die Daten sind auf Diskette abgespeichert.":GOSUB 41320
41080 RETURN
41088 ' *** Ja/Nein - Frage *************************************************
41089   A$=A$+" (J=Ja/N=Nein) ":GOSUB 41320
41090   PRINT FNP$(3,1);" ";FNP$(23,1);DLI$;FNP$(24,1);DLI$;FNP$(3,1)
41100   PRINT CUOF$;:ANSWER$=INPUT$(1):PRINT CUON$CHR$(8);
41110   IF ANSWER$="J" OR ANSWER$="j" THEN JA=1:GOSUB 41370:RETURN
41120   IF ANSWER$="N" OR ANSWER$="n" THEN JA=0:            RETURN
41130   GOTO 41100
41138 ' *** Testbereich 1 (Alpha) *******************************************
41140   DRIN=0
41150   IF AASC>57 THEN IF AASC<127        THEN DRIN=1:RETURN
41160   IF AASC>31 THEN IF AASC<48         THEN DRIN=1
41170 RETURN
41178 ' *** Testbereich 2 (Numerisch) ***************************************
41180   DRIN=0:IF AASC>47 AND AASC<58      THEN DRIN=1
41190 RETURN
41198 ' *** Testbereich 3 (Alphanumerisch) **********************************
41200   DRIN=0:IF AASC>31 THEN IF AASC<127 THEN DRIN=1
41210 RETURN
41218 ' *** Testbereich 4 (Numerisch + "/" und ".") *************************
41220   DRIN=0:IF AASC>45 THEN IF AASC<58  THEN DRIN=1
41230 RETURN
41231 ' *** DATUMS-TEST (bei VN und M-Liste drucken) ***
41232   IF VAL(TEST$)=0 OR VAL(TEST$)>31 THEN JA=0 'TAG
41233 RETURN
41234   IF VAL(TEST$)=0 OR VAL(TEST$)>12 THEN JA=0 'MONAT
41235 RETURN
41236   IF VAL(TEST$)=0 THEN JA=0 'JAHR
41237 RETURN
41238 ' *** EXIT für ERFASSEN ***********************************************
41240   K=1:FOR II=1 TO 30:A$=INKEY$:NEXT:A$=""
41250   PRINT FNP$(21,1);DLI$
41260   IF INSTR(SPACE$(25),DAT$(K))<>0 THEN K=K+1:IF K<28 THEN 41260 ELSE
        MTGLZAHL=MTGLZAHL-1:MTGL$=STR$(MTGLZAHL):GOTO 41290
41270   A$=" Wollen Sie diese Daten abspeichern ?":GOSUB 41089
41280   IF JA=1 THEN GOSUB 41009:A$=" Daten werden abgespeichert.":GOSUB 41320
                ELSE A$=" Nicht abgespeichert.":GOSUB 41320:M=MTGLZAHL-1:
                 MTGL$=STR$(M)
41290   OPEN "O",#2,"B:MTGLZAHL.DAT":PRINT #2,MTGL$:CLOSE #2
41300   MTGLMAX=MTGLZAHL:MTGL$=STR$(MTGLZAHL)
41310 RETURN
41311 ' *** Voreinstellungen im ERFASSEN  (Defaults) **********************
41312   DAT$( 5)="10***" : DAT$(21)="3"
41313   DAT$( 7)=""11**""  : DAT$(22)="Nein"
41314   DAT$(11)="12**"  : DAT$(23)="Nein"
41315   DAT$(12)="13******" : DAT$(25)="Nein"
41316 RETURN
41318 ' *** Nachricht anzeigen *********************************************
41320   PRINT FNP$(3,1);" ":PRINT FNP$(21,2);DLI$;INON$;A$;INOF$
41325 RETURN
41329 ' *** Bildschirmmaske - Basis ****************************************
41330   PRINT CL$;LOWI$;"00**********************"
41340   PRINT FNP$(1,(80-LEN(B$))/2);B$;FNP$(1,72);P$
41350   PRINT S$;FNP$(21,1);S$
41360 RETURN
41368 ' *** Funktionstasten Leiste (KEY LABELS) *****************************
41370   PRINT LOWI$;FNP$(23,1);
        "<F1>=Hilfe <F2>=Zurück <F3>=Anfang <F4>=Hauptmenü ";
41380   PRINT KOM$;: PRINT FNP$(3,1);" "
41390 RETURN
41394 ' *** restliche funktionstasten bei Verw.-nachweis ausgeben
41395   PRINT FNP$(24,1);"<F5>=Blättern rückwärts <F6>=Vorwärts            "+
        " <F8>=Drucken";FNP$(3,1);" "
41396 RETURN
41398 ' *** Eingabe bei Menübildern *****************************************
41400   N=0:F=0:FOR II=1 TO 30:A$=INKEY$:NEXT
41410   A$=INPUT$(1)
41420   IF A$>"0"  AND ASC(A$)<FK+1 THEN N=VAL(A$)    :RETURN
41430   IF A$>="q" AND A$<"u"       THEN F=ASC(A$)-240:RETURN
41440   A$=" Sie haben eine falsche Taste gedrückt - Bitte neue Auswahl"+
           " treffen "        :GOSUB 41320:GOTO 41410
41448 ' *** Rückkehr ins Hauptmenü ? ****************************************
41450   A$=" Sie wollen wirklich ins Hauptmenü ?":GOSUB 41089
41465   IF JA AND B$="STAMMSATZ ÄNDERN" THEN GOSUB 42000
41470   IF JA=1 THEN CLOSE:RUN "DISKMENU.EXE"
        ELSE PRINT FNP$(3,1):PRINT FNP$(21,1);DLI$:GOSUB 41370:RETURN
41478 ' *** Rückkehr aus HELP-Text ? ****************************************
41480   A$=" Zurück ? (J=Ja) ":GOSUB 41320:GOSUB 41090:IF JA=1 THEN RETURN
                                                               ELSE 41480
41488 ' *** Leerlesen von DAT$() (Ersetzt ERASE-Befehl) *********************
41490   FOR II=1 TO 27:DAT$(II)="":NEXT:RETURN
41498 ' *** Disk I/O Error Trap Nachricht ***********************************
41500 'LW PRINT CL$:PRINT FNP$(13,1);S$
41510 'LW PRINT
41520 'LW PRINT
41530 'LW GOSUB 41330:IF ERR<50 THEN PRINT FNP$(5,1);"Programmfehler."
41540 'LW PRINT "   14*********************"
41550 'LW PRINT
41560 'LW PRINT "   15************************************************************"
41570 'LW PRINT "   16*************************************************"
41580 'LW PRINT "   
41590 'LW PRINT "   17**********************"
41600 'LW PRINT
41610 'LW PRINT "         18********************************************"
41620 'LW PRINT
41630 'LW PRINT "         19**************************"
41640 'LW PRINT
41650 'LW PRINT "                                      20***************"
41660 'LW PRINT
41670 'LW PRINT S$;CUON$
41671 PRINT "ERL (Error Line) :";ERL
41672 PRINT "ERR (Error Code) :";ERR
41680 'LW RESET
41690 'LW GOTO 41690
41693 'LW END
41694 STOP
41999 ' *** UPRO FÜR ÄNDERN-EXIT ***************************
42000   A$=" Wollen Sie die Daten abspeichern ?":GOSUB 41089
42030   IF JA THEN GOSUB 41010
42040 RETURN
49980 ' **** VN - Subroutines, permitting one page handling ***
49990 ' ****         by 04********** 21*******              ***
49999 ' **** OPEN & FIELDING VERWENDUNGSNACHWEIS *********************
50000   OPEN "R",#5,"B:VN.POI",8                                    '#5
50010   FIELD #5,4 AS POINT1$, 4 AS POINT2$
50020   OPEN "R",#3,"B:VN.NDX",4                                    '#3
50030   FIELD #3,4 AS POINT0$
50040   OPEN "R",#4,"B:VN.DAT",560                                  '#4
50050   FIELD #4,240 AS DDD1$, 240 AS DDD2$, 80 AS DDD3$
50055 RETURN
50059 ' *** VERWENDUNGSNACHWEIS FÜR NEUES MITGLIED ANLEGEN ***********
50060   GET #3,1: TMPOINT=VAL(POINT0$)+1 'letzte Seitennummer einlesen dann +1
50063   LSET DDD1$="":LSET DDD2$="":LSET DDD3$="":PUT #4,TMPOINT  
                       'leeren Record speichern
50064   LSET POINT0$=STR$(TMPOINT):PUT #3,1 'letzte Seitennummer neu speichern
50066   PUT #3,MTGLZAHL+1
50067   LSET POINT1$="0000":LSET POINT2$="0000":PUT #5,TMPOINT
50068 RETURN
50069 ' **** ERSTE SEITE FINDEN & LESEN ******************************
50070   GET #3,MTGLZAHL+1:CPOINT=VAL(POINT0$)                    'INDEX-DATEI
50080   GET #4,CPOINT   :GOSUB 50760:BLATT=1                     'EINLESEN
50081   GET #5,CPOINT                         'Pointer lesen
50085 RETURN
50089 ' **** SEITE RÜCKWÄRTS *****************************************
50090   TMPOINT=VAL(POINT1$)
50100   IF TMPOINT=0 THEN A$=" Erste Seite ":GOSUB 41320:FOR KK=1 TO 9999:NEXT:
        RETURN
50110   CPOINT=TMPOINT:BLATT=BLATT-1
50120   GET #4,CPOINT:GOSUB 50760   
50123   GET #5,CPOINT                    'Neue Pointer lesen
50124   GOSUB 52100:VP=1:HP=1:GOSUB 50162:FOR KK=1 TO 100:NEXT
50125 RETURN
50129 ' **** SEITE VORWÄRTS  *****************************************
50130   TMPOINT=VAL(POINT2$)
50140   IF TMPOINT<>0 THEN CPOINT=TMPOINT:BLATT=BLATT+1:GET #4,CPOINT:
        GOSUB 50760:GET #5,CPOINT:GOSUB 52100:VP=1:HP=1:RETURN
50145   A$=" Letzte Seite "
50150   IF INSTR(SPACE$(20),DAT$(26))<>0 THEN GOSUB 41320:FOR KK=1 TO 20000:
        NEXT:RETURN ELSE
        A$=" Letzte Seite - Neue Seite anlegen ?":GOSUB 41089:A$="":GOSUB 41320
50160   IF JA THEN GOSUB 50310:BLATT=BLATT+1:GOSUB 52100:HP=1:VP=1
50162   GOSUB 41370:PRINT FNP$(24,1);"<F5>=Blättern rückwärts <F6>=vorwärts";
50164   PRINT TAB(54);"<F9>=Blatt drucken";FNP$(3,1);" "
50165 RETURN
50169 ' **** ERSTE SEITE EINES MITGLIEDS ANLEGEN *********************
50170   GET #3,1
50180   LSET POINT0$=STR$(VAL(POINT0$)+1)                       'NDX+1
50190   PUT #3,1
50200   LSET POINT1$="0000"                                     'PAGE POINTERS
50210   LSET POINT2$="0000"
50220   PUT #5,VAL(POINT0$)
50230   LSET POINT0$=POINT0$                                    'FIRST PAGE
50240   PUT #3,MTGLZAHL+1
50250   LSET DDD1$=""                                           'DATENBLATT
50260   LSET DDD2$=""
50270   LSET DDD3$=""
50280   PUT #4,VAL(POINT0$)
50290 RETURN
50309 ' **** ALLGEMEIN EINE NEUE SEITE ANLEGEN ***********************
50310   GET #3,1                                                  'NDX+1
50320   LSET POINT0$=STR$(VAL(POINT0$)+1)
50330   PUT #3,1:TMPOINT$=POINT0$
50339        'VEKTOREN DER BISHER LETZTEN SEITE "VERBIEGEN"
50370   LSET POINT2$=POINT0$                             'VERBIEGE VEKTOREN
50390   PUT #5,CPOINT                                    '.. DER ALTEN SEITE
50400   LSET POINT1$=STR$(CPOINT)                        '..DER NEUEN SEITE
50410   LSET POINT2$="0000"                              '.. ""
50415   CPOINT=VAL(TMPOINT$)                             'NR NEUE SEITE-
50420   PUT #5,CPOINT
50430   LSET DDD1$=""                                    'WRITE EMPTY PAGE
50440   LSET DDD2$=""
50450   LSET DDD3$="" 
50460   PUT #4,CPOINT
50463   DATEN$=SPACE$(20):FOR II=1 TO 26:DAT$(II)=DATEN$:NEXT
50465 RETURN
50599 ' **** SEITE ABSPEICHERN ***************************************
50600   DATEN$=""
50610   FOR II=1 TO 12
50620       DATEN$=DATEN$+DAT$(II)
50630   NEXT
50635   LSET DDD1$=DATEN$:DATEN$=""
50640   FOR II=13 TO 24
50650      DATEN$=DATEN$+DAT$(II)
50660   NEXT
50665   LSET DDD2$=DATEN$:DATEN$=""
50670   FOR II=25 TO 26
50680      DATEN$=DATEN$+DAT$(II)
50690   NEXT 
50720   LSET DDD3$=DATEN$:DATEN$=""
50730   PUT #4,CPOINT
50740 RETURN
50759 ' **** HACK ! (ZERSTÜCKELN DER DATEN-STRINGS) ******************
50760   FOR II=1 TO 12
50770      DAT$(II)=MID$(DDD1$,(II-1)*20+1,20)
50780   NEXT
50790   FOR II=13 TO 24
50800      DAT$(II)=MID$(DDD2$,(II-13)*20+1,20)
50810   NEXT 
50820   FOR II=25 TO 26
50830      DAT$(II)=MID$(DDD3$,(II-25)*20+1,20)
50840   NEXT 
50850 RETURN
52098 ' **** UPRO DATENAUSGABE VN  **********
52100   PRINT HIGHL$
52120   PRINT HIGHI$
52130   PRINT FNP$(5,16);MTGLZAHL
52140   PRINT FNP$ (5,29);NAMM$
52150   PRINT FNP$ (3,8);BLATT
52160   FOR K= 1 TO 26
52170     HOR=6-39*(K>13)
52180     VER=6+K+13*(K>13)
52190     FOR J=1 TO 3
52191       PRINT FNP$(VER,HOR+(J-1)*3);MID$(DAT$(K),-1+J*2,2)
52192     NEXT
52200     FOR J=1 TO 3
52201       PRINT FNP$(VER,HOR+13+(J-1)*3);MID$(DAT$(K),5+J*2,2)
52202     NEXT
52210     PRINT FNP$(VER,HOR+26);MID$(DAT$(K),13,8)
52220   NEXT
52230   PRINT LOWI$
52240 RETURN
52540 'vn drucken
52550   A$=" Ist der Drucker an ?":GOSUB 41089
52560   IF JA=0 THEN RETURN
52570   LPRINT S$
52580   LPRINT
52590   LPRINT TAB(2);"Blatt:";BLATT
52600   LPRINT
52610   LPRINT TAB(2);"Mitglieds-Nr.:";MTGLZAHL;TAB(24);"Name:";NAMM$
52620   LPRINT
52630   FOR II=1 TO 13
52640      LOZ=II:GOSUB 52730
52650      LPRINT TAB(2);"Von:";LODAT1$;" Bis:";LODAT2$;" Amt:";LODAT3$;
52660      LOZ=II+13:GOSUB 52730
52670      LPRINT " Von:";LODAT1$;" Bis:";LODAT2$;" Amt:";LODAT3$
52680   NEXT 
52690   LPRINT
52700   LPRINT S$
52720 RETURN
52730   LODAT1$=LEFT$(DAT$(LOZ),2)+"."+MID$(DAT$(LOZ),3,2)+"."
                +MID$(DAT$(LOZ),5,2)
52740   LODAT2$=MID$(DAT$(LOZ),7,2)+"."+MID$(DAT$(LOZ),9,2)+"."
                +MID$(DAT$(LOZ),11,2)
52750   LODAT3$=RIGHT$(DAT$(LOZ),8)
52760 RETURN
54900 ' *** upros MITGLIEDSLISTE DRUCKEN *******************
54990 ' *** MITGLIED DRUCKEN
55030   LPRINT
55040   LPRINT TAB(5);MTGLZAHL;
55050   LPRINT TAB(10);DAT$( 1); 'NACHNAMEN
55060   LPRINT TAB(36);DAT$( 2); 'VORNAMEN
55070   LPRINT TAB(65);"(";LEFT$(DAT$( 5),1);")";  'STATUS
55080   AMT$=""
55090   IN=INSTR(DAT$(6),"22**"):
           IF IN<>0 THEN AMT$=MID$(DAT$(6),IN,6):GOTO 55110
55100   IN=INSTR(DAT$(6),"23**"):
           IF IN<>0 THEN AMT$=MID$(DAT$(6),IN,5):GOTO 55110
55105   IN=INSTR(DAT$(6),"24**") :IF IN<>0 THEN AMT$=MID$(DAT$(6),IN,3)
55110   LPRINT TAB(71);AMT$
55120   LPRINT TAB(36);DAT$(10)  'STRAßE
55130   LPRINT TAB(36);DAT$(11); 'PLZ
55140   LPRINT TAB(41);DAT$(12)  'ORT
55150   LPRINT TAB(36);"Tel.:";DAT$(13)  'TELEFON
55160   LPRINT
55165   IF INKEY$<>"" THEN A$=" Drucken abbrechen ?":GOSUB 41089:
           IF JA=1 THEN BRKFLAG=-1  ELSE BRKFLAG=0
55170 RETURN
55190 ' *** KOPF DER MITGLIEDSLISTE
55210   LPRINT TAB(16);"MITGLIEDSLISTE 25*******************************"
55220   LPRINT 
55230   LPRINT TAB(32);"Stand:";DATUM$
55240   LPRINT
55250   LPRINT TAB(3);"Mitgl.";             TAB(10);"Name";
               TAB(36);"Vorname, Anschrift";TAB(63);"Status";
               TAB(71);"Amt" 
55260 RETURN
55290 ' **** UPRO ADRESSAUFKLEBER DRUCKEN 
55300   A$=" Ist der Drucker bereit und positioniert (J/N) ":GOSUB 41320
55310   GOSUB 41090
55320   IF JA=0 THEN RETURN
55330   LPRINT "   ";DAT$(7)
55340   SPAC1=INSTR(DAT$(2)," ")
55350   LPRINT "   ";LEFT$(DAT$(2),SPAC1);DAT$(1)
55360   LPRINT
55370   LPRINT "   ";DAT$(10)
55380   LPRINT
55390   LPRINT "   ";DAT$(11);" "DAT$(12)
55400   LPRINT:LPRINT:LPRINT
55405   LPRINT:LPRINT:LPRINT
55410 RETURN
55500 ' Stammsatz ausdrucken
55600   RESTORE 55800
55700   LPRINT S$
55710   LPRINT
55720   LPRINT TAB(30)"Mitgliedsnummer  ";MTGLZAHL
55730   LPRINT
55740   LPRINT TAB( 2)LEFT$(FELD$(1),14);DAT$(1);TAB(41)"Beruf:"
55750   FOR I=1 TO 6:READ A,B
55760     LPRINT TAB(2);LEFT$(FELD$(A),14);DAT$(A);
55770     IF B=0 THEN LPRINT:GOTO 55790
55780     LPRINT TAB(41);LEFT$(FELD$(B),14);DAT$(B);
55790   NEXT
55800   DATA 2,15,3,16,4,0,5,17,6,18,0,19
55810   FOR I=7 TO 14
55820     LPRINT TAB(2) ;LEFT$(FELD$(I),14);   DAT$(I);
                 TAB(41);LEFT$(FELD$(I+13),14);DAT$(I+13);
55830   NEXT
55840   LPRINT:LPRINT:LPRINT:LPRINT S$
55900 RETURN
55999 ' Mitgliedsvermerke
56010   LPRINT TAB(10);"Mitgliedsnr.:";TAB(28);"Name:";NAMM$
56020   LPRINT TAB(65);"Datum:":LPRINT
56030   SPC$=STRING$(9," "):LIN$=" ___ / ___ / ___"
56040   LPRINT SPC$;"26***************************";LIN$:LPRINT
56050   LPRINT SPC$;"27***************************";LIN$:LPRINT
56060   LPRINT SPC$;"28***************************";LIN$:LPRINT 
56070   LPRINT SPC$;"29***************************";LIN$:LPRINT
56080   LPRINT SPC$;"30***************************";LEFT$(LIN$,10):LPRINT
56090   LPRINT SPC$;"31***************************";LIN$:LPRINT
56100   LPRINT SPC$;"32***************************";LIN$:LPRINT
56110   LPRINT SPC$;"33***************************";LEFT$(LIN$,4):LPRINT
56120   LPRINT SPC$;"34***************************";LIN$:LPRINT
56130   LPRINT SPC$;"35***************************";LIN$:LPRINT
56140   LPRINT SPC$;"36***************************";LIN$:LPRINT
56150   LPRINT SPC$;"37***************************";LIN$:LPRINT
56160   LPRINT SPC$;"38***************************";LIN$
56170 RETURN
