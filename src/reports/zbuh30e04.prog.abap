*----------------------------------------------------------------------*
*   INCLUDE Buh30E04                                                   *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&   Event at selection-screen output.
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  CASE SY-UCOMM.
    WHEN 'SELP'.
       CLEAR SY-PFKEY.
       LOOP AT SCREEN.
            SCREEN-INPUT = '0'.
            MODIFY SCREEN.
       ENDLOOP.
  ENDCASE.


