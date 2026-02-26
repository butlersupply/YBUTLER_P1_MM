*----------------------------------------------------------------------*
*   INCLUDE RMCBINCK                                                   *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  SELECT_OBJEKTE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_OBJEKTE.

  CLEAR AOBJ.
  IF OMANDT NE SPACE.
    AOBJ = 'M'.
  ENDIF.
  READ TABLE VKORG INDEX 1.
  IF SY-SUBRC EQ 0.
    IF NOT AOBJ IS INITIAL.
      MESSAGE E200.
*   Bitte nur ein Analyse-Objekt wählen
    ENDIF.
    AOBJ = 'V'.
  ENDIF.
  READ TABLE EKORG INDEX 1.
  IF SY-SUBRC EQ 0.
    IF NOT AOBJ IS INITIAL.
      MESSAGE E200.
*   Bitte nur ein Analyse-Objekt wählen
    ENDIF.
    AOBJ = 'E'.
  ENDIF.
  READ TABLE WERKE INDEX 1.
  IF SY-SUBRC EQ 0.
    IF NOT AOBJ IS INITIAL.
      MESSAGE E200.
*   Bitte nur ein Analyse-Objekt wählen
    ENDIF.
    AOBJ = 'W'.
    DREIZLG = TRUE.
  ENDIF.

  IF AOBJ IS INITIAL.
    MESSAGE E201.
*   Bitte ein Analyse-Objekt wählen
  ENDIF.

* Berechtigungsprüfung

  DATA: FLG_RESTRICT,
        FLG_AVAUTHOR,                  "Berechtigung f. mind. ein Objekt
        FLG_NOAUTHOR,                  "keine Berechtigung
        FLG_SELECT.

  CLEAR: FLG_RESTRICT,
         FLG_AVAUTHOR,
         FLG_NOAUTHOR,
         FLG_SELECT.

  IF AOBJ EQ 'W' OR AOBJ EQ 'M'.
    DATA: HLP_WERKS LIKE T001W OCCURS 0 WITH HEADER LINE.

*  Prüfung, ob Berechtigung für die Werke existiert
    SELECT * FROM T001W INTO TABLE HLP_WERKS
                        WHERE WERKS IN WERKE.

    IF AOBJ EQ 'W'.  .
      FLG_SELECT = 'X'.
    ENDIF.

    LOOP AT HLP_WERKS.
      AUTHORITY-CHECK OBJECT 'M_BCO_WERK'
               ID 'WERKS' FIELD HLP_WERKS-WERKS.
      IF SY-SUBRC <> 0.
        CLEAR WERKE.
        FLG_NOAUTHOR = 'X'.
        IF FLG_SELECT EQ 'X'.
          WERKE-SIGN   = 'E'.
          WERKE-OPTION = 'EQ'.
          WERKE-LOW    = HLP_WERKS-WERKS.
          COLLECT WERKE.
          FLG_RESTRICT = 'X'.
        ENDIF.
      ELSE.
        FLG_AVAUTHOR      = 'X'.
        IF FLG_SELECT EQ ' '.
          WERKE-SIGN   = 'I'.
          WERKE-OPTION = 'EQ'.
          WERKE-LOW    = HLP_WERKS-WERKS.
          COLLECT WERKE.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF FLG_SELECT = ' '.
      CLEAR WERKE.
      REFRESH WERKE.
      IF FLG_NOAUTHOR = 'X'.
        MESSAGE E040.
*   Sie haben nicht die Berechtigung für eine Auswertung über alle Werke
      ENDIF.
    ENDIF.

  ELSEIF AOBJ = 'V'.
    DATA: HLP_VKORG LIKE TVKO OCCURS 0 WITH HEADER LINE.

*  Prüfung, ob Berechtigung für die Werke existiert
    SELECT * FROM TVKO INTO TABLE HLP_VKORG
      WHERE VKORG IN VKORG.

    LOOP AT HLP_VKORG.
      AUTHORITY-CHECK OBJECT 'M_BCO_VKOR'
               ID 'VKORG' FIELD HLP_VKORG-VKORG.
      IF SY-SUBRC <> 0.
        CLEAR VKORG.
        VKORG-SIGN   = 'E'.
        VKORG-OPTION = 'EQ'.
        VKORG-LOW    = HLP_VKORG-VKORG.
        COLLECT VKORG.
        FLG_RESTRICT = 'X'.
      ENDIF.
    ENDLOOP.
  ELSEIF AOBJ = 'E'.
    DATA: HLP_EKORG LIKE T024E OCCURS 0 WITH HEADER LINE.

*  Prüfung, ob Berechtigung für die Werke existiert
    SELECT * FROM T024E INTO TABLE HLP_EKORG
      WHERE EKORG IN EKORG.

    LOOP AT HLP_EKORG.
      AUTHORITY-CHECK OBJECT 'M_BCO_EKOR'
               ID 'EKORG' FIELD HLP_EKORG-EKORG.
      IF SY-SUBRC <> 0.
        CLEAR EKORG.
        EKORG-SIGN   = 'E'.
        EKORG-OPTION = 'EQ'.
        EKORG-LOW    = HLP_EKORG-EKORG.
        COLLECT EKORG.
        FLG_RESTRICT = 'X'.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF FLG_RESTRICT = 'X'.
    MESSAGE I041.
  ENDIF.

ENDFORM.                               " SELECT_OBJEKTE


*&---------------------------------------------------------------------*
*&      Form  SELECT_ZEITRAUM
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_ZEITRAUM.

*-------- Zeitraum
  IF ANZTAGE > 36500.
     ANZTAGE = 36500.
  ENDIF.

  IF ANZTAGE > 0.
    BISDATUM = SY-DATLO.
    VONDATUM = SY-DATLO - ANZTAGE.
  ENDIF.

  IF VONDATUM EQ 0 OR
     VONDATUM EQ '00000000' OR
     BISDATUM EQ 0 OR
     BISDATUM EQ '00000000'.
    MESSAGE E205.
  ENDIF.
  IF VONDATUM > BISDATUM.
    MESSAGE E205.
  ENDIF.
  IF BDF_ANALYSE = FALSE.
*--Zeitraum in Zukunft nur bei Bedarfsanalyse
    IF BISDATUM > SY-DATLO.
      MESSAGE E206.
    ENDIF.
  ELSE.
*   if vondatum < sy-datlo.
*     message e251.
*   endif.
  ENDIF.
ENDFORM.                               "SELECT_ZEITRAUM.

*&---------------------------------------------------------------------*
*&      Form  SELECT_BEREICH
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_BEREICH.

DATA:      DRILLDOWN_FCODES LIKE MCS04.
TABLES:    DD01T.

*-------- Warengruppe
  IF MATKL-LOW NE SPACE AND MATKL-HIGH NE SPACE.
    SELECT * FROM T023
      WHERE MATKL IN MATKL.
      EXIT.
    ENDSELECT.
    IF SY-SUBRC > 0.
      MESSAGE E222 WITH MATKL-LOW.
    ENDIF.
  ENDIF.

*   Berechtigungsprüfung Warengruppe
  SELECT SINGLE * FROM DD01T WHERE DOMNAME    EQ 'MATKL'
                             AND   DDLANGUAGE EQ SY-LANGU
                             AND   AS4LOCAL   EQ 'A'.
  IF SY-SUBRC EQ 0.
    DRILLDOWN_FCODES-SCRTEXT_L = DD01T-DDTEXT.
  ELSE.
    DRILLDOWN_FCODES-SCRTEXT_L = 'MATKL'.
  ENDIF.

  PERFORM AU_CHECK_MATKL IN PROGRAM RMCREPAU TABLES MATKL
        USING  'S039'                " Berechtigung wie für S039
               DRILLDOWN_FCODES
               'X'                   " Fehlermeldung bei fehlenden Ber.
               IF FOUND.
*-------- Materialart
  IF MTART-LOW NE SPACE AND MTART-HIGH NE SPACE.
    SELECT * FROM T134
      WHERE MTART IN MTART.
      EXIT.
    ENDSELECT.
    IF SY-SUBRC > 0.
      MESSAGE E207 WITH MTART-LOW.
    ENDIF.
  ENDIF.

*   Berechtigungsprüfung Materialart
  SELECT SINGLE * FROM DD01T WHERE DOMNAME    EQ 'MTART'
                             AND   DDLANGUAGE EQ SY-LANGU
                             AND   AS4LOCAL   EQ 'A'.
  IF SY-SUBRC EQ 0.
    DRILLDOWN_FCODES-SCRTEXT_L = DD01T-DDTEXT.
  ELSE.
    DRILLDOWN_FCODES-SCRTEXT_L = 'MTART'.
  ENDIF.

  PERFORM AU_CHECK_MTART IN PROGRAM RMCREPAU TABLES MTART
        USING  'S039'                " Berechtigung wie für S039
               DRILLDOWN_FCODES
               'X'                   " Fehlermeldung bei fehlenden Ber.
               IF FOUND.

*-------- ABC-Kennzeichen
  DESCRIBE TABLE MAABC LINES TLINES.
  IF TLINES <> 0 AND AOBJ NE 'W'.
    MESSAGE E227.
  ENDIF.

*-------- Einkäufergruppe
  DESCRIBE TABLE EKGRP LINES TLINES.
  IF TLINES <> 0 AND AOBJ NE 'W'.
    MESSAGE E228.
  ENDIF.
  IF TLINES <> 0.
    SELECT * FROM T024
      WHERE EKGRP IN EKGRP.
      EXIT.
    ENDSELECT.
    IF SY-SUBRC > 0.
      MESSAGE E224 WITH EKGRP-LOW.
    ENDIF.
  ENDIF.

  IF AOBJ EQ 'W' OR AOBJ EQ 'M'.
*   Berechtigungsprüfung Einkäufergruppe
    SELECT SINGLE * FROM DD01T WHERE DOMNAME    EQ 'EKGRP'
                               AND   DDLANGUAGE EQ SY-LANGU
                               AND   AS4LOCAL   EQ 'A'.
    IF SY-SUBRC EQ 0.
      DRILLDOWN_FCODES-SCRTEXT_L = DD01T-DDTEXT.
    ELSE.
      DRILLDOWN_FCODES-SCRTEXT_L = 'EKGRP'.
    ENDIF.

    PERFORM AU_CHECK_EKGRP IN PROGRAM RMCREPAU TABLES EKGRP
        USING  'S039'                " Berechtigung wie für S039
               DRILLDOWN_FCODES
               'X'                   " Fehlermeldung bei fehlenden Ber.
               IF FOUND.
  ENDIF.

*-------- Dispomerkmal
  DESCRIBE TABLE DISMM LINES TLINES.
  IF TLINES <> 0 AND AOBJ NE 'W'.
    MESSAGE E229.
  ENDIF.
  IF TLINES <> 0.
    SELECT * FROM T438A
      WHERE DISMM IN DISMM.
      EXIT.
    ENDSELECT.
    IF SY-SUBRC > 0.
      MESSAGE E225 WITH DISMM-LOW.
    ENDIF.
  ENDIF.

*-------- Disponent
  DESCRIBE TABLE DISPO LINES TLINES.
  IF TLINES <> 0 AND AOBJ NE 'W'.
    MESSAGE E230.
  ENDIF.
  IF TLINES <> 0.
    SELECT * FROM T024D
      WHERE WERKS IN WERKE
      AND DISPO IN DISPO.
      EXIT.
    ENDSELECT.
    IF SY-SUBRC > 0.
      MESSAGE E226 WITH WERKE-LOW DISPO-LOW.
    ENDIF.
  ENDIF.

  IF AOBJ EQ 'W' OR AOBJ EQ 'M'.
*   Berechtigungsprüfung Disponent
    SELECT SINGLE * FROM DD01T WHERE DOMNAME    EQ 'DISPO'
                               AND   DDLANGUAGE EQ SY-LANGU
                               AND   AS4LOCAL   EQ 'A'.
    IF SY-SUBRC EQ 0.
      DRILLDOWN_FCODES-SCRTEXT_L = DD01T-DDTEXT.
    ELSE.
      DRILLDOWN_FCODES-SCRTEXT_L = 'DISPO'.
    ENDIF.

    PERFORM AU_CHECK_DISPO IN PROGRAM RMCREPAU TABLES DISPO
        USING  'S039'                " Berechtigung wie für S039
               DRILLDOWN_FCODES
               'X'                   " Fehlermeldung bei fehlenden Ber.
               IF FOUND.
  ENDIF.

ENDFORM.                               " SELECT_BEREICH

*&---------------------------------------------------------------------*
*&      Form  SELECT_HITLISTE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM SELECT_HITLISTE.

*-------- Anzahl Materialien geringste RW
  IF WAHL2 = TRUE.
    HIT_WAHL = 'WAHL2'.
    IF AMMIN EQ SPACE.
      HITSPACE = 'X'.
    ELSE.
      IF AMMIN LE 0.
        MESSAGE E233.
      ENDIF.
    ENDIF.
  ENDIF.

*-------- Anzahl Materialien größte RW
  IF WAHL3 = TRUE.
    HIT_WAHL = 'WAHL3'.
    IF AMMAX EQ SPACE.
      HITSPACE = 'X'.
    ELSE.
      IF AMMAX LE 0.
        MESSAGE E233.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                               " SELECT_HITLISTE

*&---------------------------------------------------------------------*
*&      Form  TEXTE_EINLESEN
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM TEXTE_EINLESEN.

  CLEAR TEXTETAB.
  REFRESH TEXTETAB.

  TEXTETAB-TXT00 = TEXT-150.

  CASE AOBJ.
    WHEN 'E'.
      TEXTETAB-TXT21 = TEXT-151.
    WHEN 'V'.
      TEXTETAB-TXT21 = TEXT-152.
    WHEN 'W'.
      TEXTETAB-TXT21 = TEXT-161.
    WHEN 'M'.
      TEXTETAB-TXT21 = TEXT-153.
  ENDCASE.

  TEXTETAB-TXT22 = ' '.
  TEXTETAB-TXT23 = TEXT-162.
  TEXTETAB-TXT24 = TEXT-163.
  TEXTETAB-TXT31 = TEXT-171.
  TEXTETAB-TXT32 = TEXT-172.
  TEXTETAB-TXT33 = TEXT-173.
  TEXTETAB-TXT34 = TEXT-174.
  TEXTETAB-TXT41 = TEXT-181.
  TEXTETAB-TXT42 = TEXT-182.
  TEXTETAB-TXT43 = TEXT-183.
  TEXTETAB-TXT51 = TEXT-191.
  TEXTETAB-TXT52 = TEXT-192.
  TEXTETAB-TXT53 = TEXT-193.
  TEXTETAB-TXT54 = TEXT-194.
  TEXTETAB-TXT55 = TEXT-195.
  TEXTETAB-TXT56 = TEXT-196.
  TEXTETAB-TXT116 = TEXT-140.
  APPEND TEXTETAB.

ENDFORM.                               " TEXTE_EINLESEN


*&---------------------------------------------------------------------*
*&      Form  KEINE_LISTE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM KEINE_LISTE.

  MESSAGE S254.
  SKIP TO LINE 10.
  WRITE: 27 '******************************'.
  WRITE: /28 TEXT-130.
  WRITE: /27 '******************************'.

ENDFORM.                               " KEINE_LISTE
