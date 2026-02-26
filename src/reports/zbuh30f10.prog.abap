*----------------------------------------------------------------------*
*   INCLUDE Buh30F10                                                   *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SHOW_SELECTION-SCREEN
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form show_selection-screen.
  call screen 1000 starting at 20 10.



endform.                               " SHOW_SELECTION-SCREEN


*&---------------------------------------------------------------------*
*&      Form  FUELLEN_TABELLEN
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form fuellen_tabellen.

* Einlesen der Tabellen v134w, T156c, T156m, T156s
* clear xv134w. refresh xv134w.
  clear xt134m. refresh xt134m.
  clear xt156s. refresh xt156s.
  clear xt156m. refresh xt156m.
  clear xt156c. refresh xt156c.

* select * from v134w into table xv134w order by mandt werks mtart.
  select * from t134m into table xt134m order by primary key.
* SELECT * FROM T156S INTO TABLE XT156S ORDER BY PRIMARY KEY.
  select * from v156s into table xt156s order by
     mandt bwart wertu mengu sobkz kzbew kzzug kzvbr.
  select * from t156m into table xt156m order by primary key.
  select * from t156c into table xt156c order by primary key.

*--Füllen der Rangesstruktur MJAHR


endform.                               " FUELLEN_TABELLEN
