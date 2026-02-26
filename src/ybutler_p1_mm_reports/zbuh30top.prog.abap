*&---------------------------------------------------------------------*
*& Include BUH30TOP                         Report RMCBUH30            *
*&                                                                     *
*&---------------------------------------------------------------------*

report   rmcbuh30   message-id lc.
ENHANCEMENT-POINT BUH30TOP_G4 SPOTS ES_RMCBUH30 STATIC.
ENHANCEMENT-POINT BUH30TOP_G5 SPOTS ES_RMCBUH30.
ENHANCEMENT-POINT BUH30TOP_G6 SPOTS ES_RMCBUH30 STATIC.
ENHANCEMENT-POINT BUH30TOP_G7 SPOTS ES_RMCBUH30.

*DDIC-Tabellen

tables: rmcbi,
        rmcb0,
        t001w,
        t023,
        t024,
        t024d,
        t024e,
        t024w,
        t134,
        t156m,
*       T156S,
        v156s,
        t156c,
        marc,
        mara,
        tvko,
        tvkwz,
        t438a,
*       v134w.
        t134m.

data: ft_datum like sy-datum.

data: tlines like sy-tabix,
      tlines1 like sy-tabix,
      tlines2 like sy-tabix,
      tlines3 like sy-tabix,
      tlines4 like sy-tabix,
      zahl type p,                     " Pruefziffer
      seitenstatus(4),
      zeil,
      eins(1) value '1',
      zwei(1) value '2',
      drei(1) value '3',
      true(1) value 'X',
      false   value ' ',
      aobj,                            " Kennung für Analyseobjekt
      datumab like sy-datlo,           " Datum für FB Verbrauch_BCO
      strat,                           " Kennung fuer Segment-Strategie
      hitspace,                        " Schalter alle Materialien
      xalager(1),                      " Alle lagerhaltigen Materialien
      anztage like rmcb0-btage,        " Hilfsvariable, unbenutzt
      hit_wahl like bco_werte-matkl,   " Hitlisteneingrenzung
      dreizlg(1),                      " Dreizeilige Listausgabe
      bdf_analyse.
* Interne Tabellen

data: begin of wertetab occurs 500.
        include structure bco_werte.
data: end of wertetab.

data: begin of verbrauch occurs 100.
        include structure iverb.
data: end of verbrauch.

data: begin of verbr_mat occurs 100.
        include structure bco_werte.
data: end of verbr_mat.

data: begin of xv134w occurs 100.
        include structure v134w.
data: end of xv134w.

data: begin of xt134m occurs 100.
        include structure t134m.
data: end of xt134m.

data: begin of xt156s occurs 100.
*       INCLUDE STRUCTURE T156S.
        include structure v156s.
data: end of xt156s.

data: begin of xt156m occurs 100.
        include structure t156m.
data: end of xt156m.

data: begin of xt156c occurs 100.
        include structure t156c.
data: end of xt156c.

data: begin of mjahr occurs 1.
        include structure mcbmjahr.
data: end of mjahr.

data: begin of flagtab occurs 4,
      flag,
      end   of flagtab.

data: begin of textetab occurs 1.
        include structure bco_texte.
data: end of textetab.

DATA: berid_range LIKE berid_range OCCURS 0 WITH HEADER LINE.

*--------------------------------------------------------------------*
* Aufbereitung Selektionsbildschirm                                   *
*---------------------------------------------------------------------*

*-------- Analyse-Objekt ---------------------------------------------*
selection-screen begin of block objekte with frame title text-100.

parameters omandt like rmcb0-omand.    " gesamter Mandant
select-options vkorg for tvko-vkorg.
select-options ekorg for t024e-ekorg.
select-options werke for t001w-werks.

selection-screen end of block objekte.

*-------- Analysezeitraum
selection-screen begin of block zeitraum with frame title text-117.

*-------- Umschlagshäufigkeit im Zeitraum
selection-screen begin of line.
*selection-screen comment  1(31) text-136.                      "744242
selection-screen comment  1(31) text-136 for field vondatum.    "744242
selection-screen position 33.
parameters: vondatum like rmcb0-vonda.
*selection-screen comment  52(4) text-120.                      "744242
selection-screen comment  52(4) text-120 for field bisdatum.    "744242
selection-screen position 58.
parameters: bisdatum like rmcb0-bisda default sy-datlo.
selection-screen end of line.

parameters verbr like mcbeleg-verbr.

selection-screen end of block zeitraum.

*-------- Analysebereich
selection-screen begin of block bereich with frame title text-114.
select-options material for marc-matnr matchcode object mat1
                                       memory id mat.
*-------- Materialien mit Löschvormerkung
selection-screen begin of line.
parameters xlvorm like mcbeleg-loesch.
*selection-screen comment 3(40) text-103.                       "744242
selection-screen comment 3(40) text-103 for field xlvorm.       "744242
selection-screen end of line.


*-------- Warengruppe
select-options matkl for mara-matkl.

*-------- Materialart
select-options mtart for mara-mtart.

*-------- ABC-Kz.
select-options maabc for marc-maabc.

*-------- Einkäufergruppe
select-options ekgrp for marc-ekgrp.

*-------- Dispomerkmal
select-options dismm for marc-dismm.

*-------- Disponent
select-options dispo for marc-dispo.

*-------- Dispobereich
PARAMETERS FLGDISPO AS CHECKBOX DEFAULT 'X'.
SELECT-OPTIONS berid FOR berid_range-berid_low.

selection-screen end of block bereich.

*-------- Eingrenzung Hitliste
selection-screen begin of block hitliste with frame title text-113.

selection-screen begin of line.
parameters: wahl0 like rmcb0-hlbw1 default 'X' radiobutton group hit.
*selection-screen comment  3(20) text-139.                      "744242
selection-screen comment  3(20) text-139 for field wahl0.       "744242
selection-screen end of line.

*-------- Bereich Umschlagshäufigkeit
selection-screen begin of line.
parameters: wahl1 like rmcb0-hlbw1 radiobutton group hit.
*selection-screen comment  3(25) text-140.                      "744242
selection-screen comment  3(25) text-140 for field wahl1.       "744242
*selection-screen comment  45(4) text-119.                      "744242
selection-screen comment  45(4) text-119 for field uhvon.       "744242
selection-screen position 55.
parameters: uhvon like rmcb0-uhvon default '1'.
*selection-screen comment  67(4) text-120.                      "744242
selection-screen comment  67(4) text-120 for field uhbis.       "744242
parameters: uhbis like rmcb0-uhbis.
selection-screen end of line.

*-------- Anzahl Materialien mit größter UH
selection-screen begin of line.
parameters: wahl3 like rmcb0-hlbw2 radiobutton group hit.
*selection-screen comment  3(49) text-141.                      "744242
selection-screen comment  3(49) text-141 for field wahl3.       "744242
selection-screen position 55.
parameters: ammax like rmcb0-anmat default '5'.
selection-screen end of line.

*-------- Anzahl Materialien mit kleinster UH
selection-screen begin of line.
parameters: wahl2 like rmcb0-hlbw3 radiobutton group hit.
*selection-screen comment  3(49) text-142.                      "744242
selection-screen comment  3(49) text-142 for field wahl2.       "744242
selection-screen position 55.
parameters: ammin like rmcb0-anmat default '5'.
selection-screen end of line.

selection-screen end of block hitliste.
