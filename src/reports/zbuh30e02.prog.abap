*----------------------------------------------------------------------*
*   INCLUDE Buh30E02                                                   *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&   Event START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
*-------- Start mit einzeiliger Ausgabe
  ZEIL = EINS.

*--Steuertabellen einlesen
  PERFORM FUELLEN_TABELLEN.

*--Einlesen der Materialien und Werke zum gewählten Analysebereich
  CLEAR WERTETAB.
  REFRESH WERTETAB.

  CALL FUNCTION 'AUFBAU_WERTETABELLE'
       EXPORTING
            FLG_AOBJ     = AOBJ
            ALAGER       = XALAGER
            LVORM        = XLVORM
       TABLES
            WERTETABELLE = WERTETAB
            VKORG        = VKORG
            EKORG        = EKORG
            MATERIAL     = MATERIAL
            WERK         = WERKE
            MTART        = MTART
            MATKL        = MATKL
            EKGRP        = EKGRP
            DISMM        = DISMM
            DISPO        = DISPO
            MAABC        = MAABC
       EXCEPTIONS
            NOT_FOUND    = 01.

  IF SY-SUBRC = 01.
    PERFORM KEINE_LISTE.
  ELSE.


    CALL FUNCTION 'LESEN_MATERIALKURZTEXTE'
         TABLES
              WERTETABELLE = WERTETAB.



    CALL FUNCTION 'BERECHNEN_BESTAND'
         EXPORTING
              FLG_VERBR    = VERBR
              FLG_AKT      = ' '
              FLG_MIT      = 'X'
              FLG_MIN      = ' '
              DATUM_AB     = VONDATUM
              DATUM_BIS    = BISDATUM
         TABLES
              WERTETABELLE = WERTETAB
*             xv134w       = xv134w
              X134M        = XT134M
              XT156C       = XT156C
              XT156M       = XT156M
              XT156S       = XT156S
              MJAHR        = MJAHR
              GESVERBRAUCH = VERBR_MAT.


    CALL FUNCTION 'VERBRAUCH_BCO'
         EXPORTING
              FLG_VERBR    = VERBR
              FLG_KZ       = 'U'
              FLG_PROGN    = ' '
              VONDATUM     = VONDATUM
              BISDATUM     = BISDATUM
              FLG_DISPOVERBR = FLGDISPO
         TABLES
              WERTETABELLE = WERTETAB
              GESVERBRAUCH = VERBR_MAT
              berid_range  = berid_range.
*             MJAHR        = MJAHR.



    CALL FUNCTION 'BERECHNUNG'
         EXPORTING
              FLG_KZ       = 'U'
         TABLES
              WERTETABELLE = WERTETAB.


    IF NOT AOBJ = 'W'.
      CALL FUNCTION 'WERTETABELLE_KUMULIERT'
           EXPORTING
                FLG_AOBJ     = AOBJ
                FLG_RECHNG   = 'U'
           TABLES
                WERTETABELLE = WERTETAB.
    ENDIF.

export wertetab TO MEMORY ID 'ITAB'.

*    CALL FUNCTION 'LISTE_KENNZAHLEN'
*         EXPORTING
*              FLG_BETRAG       = ' '
*              LIST_AUFBAU      = ZEIL
*              SEITEN_STATUS    = SEITENSTATUS
*              STEUER_LEISTE    = RMCB0
*              CONSUMPTION      = 'X'
*              REQUIREMENT      = ' '
*              DEC              = 2
*              DREIZEILIG       = DREIZLG
*              KENNZ_1          = 'K_MENGE'
*              KENNZ_2          = ' '
*              KENNZ_3          = 'MIT_MENGE'
*              KENNZ_3_1        = 'MEINS'
*              KENNZ_4          = 'K_K_MENGE'
*              KENNZ_4_1        = 'MEINS'
*              U_GRENZE         = UHVON
*              O_GRENZE         = UHBIS
*              MAX_GRENZE       = AMMAX
*              MIN_GRENZE       = AMMIN
*              HIT              = HIT_WAHL
*              BATCH_ABC_UPDATE = ' '
*         TABLES
*              WERTE            = WERTETAB
*              TEXTE            = TEXTETAB.

  ENDIF.
