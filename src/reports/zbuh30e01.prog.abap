*----------------------------------------------------------------------*
*   INCLUDE BUH30E01                                                   *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&   Event AT SELECTION-SCREEN ON BLOCK
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*---------------------------------------------------------------------*
* Plausibilitätscheck der Eingabedaten                                *
*---------------------------------------------------------------------*

*-------- Auswahl
AT SELECTION-SCREEN ON BLOCK OBJEKTE.
  PERFORM SELECT_OBJEKTE.
  PERFORM TEXTE_EINLESEN.

*-------- Analysezeitraum ----------------------------------------
AT SELECTION-SCREEN ON BLOCK ZEITRAUM.
  PERFORM SELECT_ZEITRAUM.

*-------- Analyse-Bereich --------------------------------------------*
AT SELECTION-SCREEN ON BLOCK BEREICH.
  PERFORM SELECT_BEREICH.

*-------- Abprüfen Eingrenzungen Hitliste
AT SELECTION-SCREEN ON BLOCK HITLISTE.

  CLEAR HIT_WAHL.

  IF WAHL0 = TRUE.
    HIT_WAHL = 'WAHL0'.
  ELSEIF WAHL1 = TRUE.
    HIT_WAHL = 'WAHL1'.

    IF UHVON EQ SPACE AND UHBIS EQ SPACE.
      HITSPACE = 'X'.
    ELSE.
      IF UHVON < 0 OR UHBIS < 0.
        MESSAGE E232.
      ENDIF.
      IF UHVON NE SPACE AND
         UHBIS NE SPACE.
        IF UHBIS < UHVON.
          MESSAGE E248.
        ENDIF.
      ELSE.
        IF UHVON NE SPACE.
          UHBIS = '999'.
        ELSE.
          UHVON = 0.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
    PERFORM SELECT_HITLISTE.
  ENDIF.

*  Füllen der Rangesstruktur MJAHR

  mjahr-sign = 'I'.
  mjahr-option = 'BT'.
  mjahr-low = vondatum(4).
  if sy-datum < bisdatum.
    mjahr-high = bisdatum(4).
  else.
    ft_datum = sy-datum.
    mjahr-high = ft_datum(4).
  endif.
  append mjahr.


