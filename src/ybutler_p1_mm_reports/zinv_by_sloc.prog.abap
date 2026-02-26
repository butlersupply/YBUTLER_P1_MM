*&---------------------------------------------------------------------*
*& Report  ZINV_BY_SLOC
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZINV_BY_SLOC message-id zspx.
************************************************************************
*----------------------------------------------------------------------*
* Change history
*-----------------------------------------------------------------------
* Date       | Transport   | RICEF | UserID   | Description
*--------------------------------------------------------------------------
* 06/27/2007 | PEDK907956  | R28   |RGRIFT    | Original code development
*--------------------------------------------------------------------------


tables: mara,
        marc,
        mard,
        mska,
        v_mmim_ln,
        t001w,
        t001l,
        t023,
        t024d,
        makt,
        mbew,
        ebew.

* Materials to be processed
types: begin of ty_mat,
         matnr like mara-matnr,
         werks like marc-werks,
         xchar like marc-xchar,
         mtart like mara-mtart,
         matkl like mara-matkl,
         meins like mara-meins, " uom
         trame like marc-trame, " in transit qty
       end of ty_mat.

*--- Material data from MARA, MARC, MARD, MAKT, MSKA
types: begin of t_data,
  werks type mard-werks,
  lgort type mard-lgort,
  matnr type mard-matnr,
  maktx type makt-maktx,
  dispo type marc-dispo,
       end of t_data.

* working table for the entries of all stock tables
data: begin of collector occurs 0,
       matnr like mara-matnr,
       werks like t001w-werks,
*       lgort LIKE mard-lgort,
     end of collector.

* working table for the entries of V_MMIM_LN
data: begin of collector_ln occurs 0,
       matnr like mara-matnr,
       werks like t001w-werks,
       lgort like mard-lgort,
       sobkz like mkol-sobkz,
       labst like mard-labst,
       insme like mard-insme,
       speme like mard-speme,
       einme like mard-einme,
       trame like marc-trame,
     end of collector_ln.

* working table for the entries of V_MMIM_EN
data: begin of collector_en occurs 0,
       matnr like mara-matnr,
       werks like t001w-werks,
       lgort like mard-lgort,
       sobkz like mkol-sobkz,
       vbeln like  mska-vbeln,
       posnr like  mska-posnr,
*       labst LIKE mard-labst,
       kalab like mska-kalab,
*       insme LIKE mard-insme,
       kains like mska-kains,
*       speme LIKE mard-speme,
       kaspe like mska-kaspe,
*       einme LIKE mard-einme,
       kaein like mska-kaein,
     end of collector_en.

* working table for the entries of V_MMIM_KN
data: begin of collector_kn occurs 0,
       matnr like mara-matnr,
       werks like t001w-werks,
       lgort like mard-lgort,
       sobkz like mkol-sobkz,
       lifnr like lfa1-lifnr,
       labst like mard-labst,
       insme like mard-insme,
       speme like mard-speme,
       einme like mard-einme,
     end of collector_kn.

* output ALV table
types: begin of t_output,
         matnr     type mara-matnr,   "Material
         werks     type t001w-werks,  "Plant
         lgort     type mard-lgort,   "Storage Loc
         maktx     type makt-maktx,   "Description
         sobkz(1)  type c,            "Special Stock Indicator
         spstk(20) type c,            " Spec Stock number (derived)
         uprc      type mbew-stprs,   " Unit Price (calculated)
         labst     type p decimals 2, " Unrestr Stock Quantity
         val1      type p decimals 2, " Unrestr Stock Value
         insme     type p decimals 2, " Qual Insp Stock Quantity
         val2      type p decimals 2, " Qual Insp Stock Value
         speme     type p decimals 2, " Blocked Stock Quantity
         val3      type p decimals 2, " Blocked Stock Value
         einme     type p decimals 2, " Restricted Stock Quantity
         val4      type p decimals 2, " Restricted Stock Value
         trame     type p decimals 2, " Stock in Transit Quantity
         val5      type p decimals 2, " Stock in Transit Value
         tqty      type p decimals 2, " Total quantity
         tval      type p decimals 2, " Total value
       end of t_output.

data: t_mat     type ty_mat occurs 0 with header line.
data: t_nobatch type ty_mat occurs 0 with header line.
*DATA: it_data   TYPE TABLE OF tab_data.

data: i_output  type standard table of t_output initial size 0,
      wa_output type t_output.

*----------------------------------------------------------------------*
*       CLASS znew_gc_event_handler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class znew_gc_event_handler definition.

  public section.
    methods:
      on_link_click
        for event link_click of cl_salv_events_table
      importing
        row          " row clicked on
        column  .     " column clicked on

endclass.               "ZNEW_GC_EVENT_HANDLER


*----------------------------------------------------------------------*
*       CLASS znew_gc_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class znew_gc_event_handler implementation.

  method on_link_click.    " Method that fires when user link clicks

*   " Declare local variables
*****    perform d0100_event_hotspot_click using row
*****                                            column.

  endmethod.               " on_link_click

endclass.               " ZNEW_GC_EVENT_HANDLER

selection-screen begin of block a1 with frame title text-001.
select-options:
      s_werks for mard-werks obligatory,
      s_lgort for mard-lgort,
      s_matnr for mard-matnr,
      s_dispo for marc-dispo,
      s_matkl for mara-matkl.
selection-screen end   of block a1.

* Start of code by SALILV - PEDK908170 on 07/17/2007
selection-screen begin of block layo with frame title text-003.
parameters : p_layout  type  slis_vari.
selection-screen end of block layo.
* End of code by SALILV - PEDK908170 on 07/17/2007

at selection-screen.
  perform validate.

* Start of code by SALILV - PEDK908170 on 07/17/2007

at selection-screen on value-request for p_layout.
  perform variant_search changing p_layout.
* End of code by SALILV - PEDK908170 on 07/17/2007

start-of-selection.

*--- Select the data from the tables
  perform get_materials.
* Start of code by SALILV - PEDK908170
  perform process_data.
* Start of comments by SALILV
*  READ TABLE collector INDEX 1 TRANSPORTING NO FIELDS.
*  IF sy-subrc = 0.
*    PERFORM process_data.
*  ENDIF.
*End of comments by SALILV
* End of code by SALILV - PEDK908170
*--- call the OO ALV object to report data
  if not i_output[] is initial.
    perform display_new_grid.
  else.
    message s000 with 'No data found based on selection criteria.'.
  endif.

end-of-selection.

*&---------------------------------------------------------------------*
*&      Form  get_materials
*&---------------------------------------------------------------------*
*     Gets the data from the tables
*----------------------------------------------------------------------*
form get_materials.

*--- get all requested materials to the MARC level
  select mara~matnr werks xchar matkl
       into corresponding fields of table t_mat
       from mara inner join marc
       on mara~matnr = marc~matnr
       where marc~matnr in s_matnr
         and marc~werks in s_werks
         and mara~matkl in s_matkl
* Start of code by SALILV - PEDK908168 on 07/17/2007
         and marc~dispo in s_dispo.
* End of code by SALILV - PEDK908168 on 07/17/2007


*--- process the selection and keep only non batch managed materials
  loop at t_mat.
    if t_mat-xchar is initial.
      append t_mat to t_nobatch.
    endif.
  endloop.

*--- now get the materials down to the MARD level. This internal table
*--- will drive the selection from the views for the report.
  clear collector.
  read table t_nobatch index 1 transporting no fields.
  if sy-subrc = 0.
* Start of code by SALILV - PEDK908168 on 07/17/2007
*--Start of comment by SALILV
****    SELECT matnr werks lgort
****        FROM mard
****      INTO CORRESPONDING FIELDS OF TABLE collector
****        FOR ALL ENTRIES IN t_nobatch
****        WHERE matnr = t_nobatch-matnr
****          AND werks = t_nobatch-werks
****          AND lgort IN s_lgort.
*--End of comment by SALILV

    select matnr
           werks
           from marc
           into table collector
           for all entries in t_nobatch
           where matnr = t_nobatch-matnr
           and   werks = t_nobatch-werks
           and   ( trame gt 0 or
                   umlmc gt 0 ).

    delete adjacent duplicates from collector comparing matnr werks.
* End of code by SALILV - PEDK908168 on 07/17/2007

* now get the first view VV_MMIM_LN (stock)
    select matnr werks lgort
        labst insme einme speme
        from v_mmim_ln
        appending corresponding fields of table collector_ln
        for all entries in t_nobatch
        where matnr = t_nobatch-matnr
          and werks = t_nobatch-werks
          and lgort in s_lgort
          and ( labst gt 0
          or  insme gt 0
          or  einme gt 0
          or  speme gt 0 ).

* now get the first view VV_MMIM_EN (sales order)
*----Start of Code Changes done by SALILV - PEDK908170
*----Selecting the data from table MSKA instead of view V_MMIM_EN as discussed with Vinod Tejwani
    select matnr werks lgort sobkz vbeln posnr
        kalab kains kaspe kaein
        from mska
        appending corresponding fields of table collector_en
        for all entries in t_nobatch
        where matnr = t_nobatch-matnr
          and werks = t_nobatch-werks
          and lgort in s_lgort
          and ( kalab gt 0
          or    kains gt 0
          or    kaspe gt 0
          or    kaein gt 0 ).
*----End of Code Changes done by SALILV - PEDK908170


* now get the first view VV_MMIM_KN (Vendor consignment)
    select matnr werks lgort lifnr
        labst insme einme speme sobkz
        from v_mmim_kn
        appending corresponding fields of table collector_kn
        for all entries in t_nobatch
        where matnr = t_nobatch-matnr
          and werks = t_nobatch-werks
          and lgort in s_lgort
          and ( labst gt 0
          or  insme gt 0
          or  einme gt 0
          or  speme gt 0 ).

    sort collector    by matnr werks ."lgort."  sobkz.
    sort collector_ln by matnr werks lgort sobkz.
    sort collector_en by matnr werks lgort sobkz.
    sort collector_kn by matnr werks lgort sobkz.
  endif.

endform.                    " get_materials

*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form process_data.

  data: lv_bwkey type mbew-bwkey.
  clear i_output.
  refresh i_output.

*--- check for V_MMIM_LN Stock records.
  loop at collector_ln.

*--- stock record found
    clear wa_output.

*      MOVE-CORRESPONDING collector TO wa_output.
    move-corresponding collector_ln to wa_output.

*--- get material text
    clear makt.
    select single * from makt
      where matnr = collector_ln-matnr
        and spras = sy-langu.
    if sy-subrc = 0.
      wa_output-maktx = makt-maktx.
    endif.
*--- calc unit price
    "move collector_ln-werks to lv_bwkey.
    SELECT SINGLE bwkey FROM t001w INTO lv_bwkey WHERE werks = collector_ln-werks.

    select single * from mbew
      where matnr = collector_ln-matnr
        and bwkey = lv_bwkey.
    if sy-subrc = 0.
      wa_output-uprc = mbew-salk3 / mbew-lbkum.
    endif.
*--- calc value
    wa_output-val1 = wa_output-uprc * collector_ln-labst.
    wa_output-val2 = wa_output-uprc * collector_ln-insme.
    wa_output-val3 = wa_output-uprc * collector_ln-speme.
    wa_output-val4 = wa_output-uprc * collector_ln-einme.

    wa_output-tqty = wa_output-labst + wa_output-insme +
                     wa_output-speme + wa_output-einme.
    wa_output-tval = wa_output-val1 + wa_output-val2 +
                     wa_output-val3 + wa_output-val4.

    append wa_output to i_output.
  endloop.
*--- check for V_MMIM_EN Sales Order records
  loop at collector_en.

    clear wa_output.
*--- stock record found
*      MOVE-CORRESPONDING collector TO wa_output.
    move-corresponding collector_en to wa_output.
*----Start of code addition by SALILV - PEDK908170
*----As discussed with Vinod Tejwani
    wa_output-labst = collector_en-kalab.
    wa_output-insme = collector_en-kains.
    wa_output-speme = collector_en-kaspe.
    wa_output-einme = collector_en-kaein.
*----End of code addition by SALILV - PEDK908170
*--- get material text
    clear makt.
    select single * from makt
      where matnr = collector_en-matnr
        and spras = sy-langu.
    if sy-subrc = 0.
      wa_output-maktx = makt-maktx.
    endif.
*--- capture sales order and item
    concatenate collector_en-vbeln '/' collector_en-posnr
    into wa_output-spstk separated by ' '.
*--- calc unit price
    move collector_en-werks to lv_bwkey.
    select single * from ebew
      where matnr = collector_en-matnr
        and bwkey = lv_bwkey
        and bwtar = ' '
        and sobkz = 'E'
        and vbeln = collector_en-vbeln
        and posnr = collector_en-posnr.
    if sy-subrc = 0.
      wa_output-uprc = ebew-salk3 / ebew-lbkum.
    endif.
*--- calc value
*----Start of code addition by SALILV - PEDK908170
*----As discussed with Vinod Tejwani
    wa_output-val1 = wa_output-uprc * collector_en-kalab.
    wa_output-val2 = wa_output-uprc * collector_en-kains.
    wa_output-val3 = wa_output-uprc * collector_en-kaspe.
    wa_output-val4 = wa_output-uprc * collector_en-kaein.
*------start comments
*      wa_output-val1 = wa_output-uprc * collector_en-labst.
*      wa_output-val2 = wa_output-uprc * collector_en-insme.
*      wa_output-val3 = wa_output-uprc * collector_en-speme.
*      wa_output-val4 = wa_output-uprc * collector_en-einme.
*------end comments
*----End of code addition by SALILV - PEDK908170

    wa_output-tqty = wa_output-labst + wa_output-insme +
                     wa_output-speme + wa_output-einme.

    wa_output-tval = wa_output-val1 + wa_output-val2 +
                     wa_output-val3 + wa_output-val4.

    append wa_output to i_output.

  endloop.
*--- check for V_MMIM_KN Vendor Consignment records.
  loop at  collector_kn.

    clear wa_output.
*--- vendor record found
*      MOVE-CORRESPONDING collector TO wa_output.
    move-corresponding collector_kn to wa_output.
*--- get material text
    clear makt.
    select single * from makt
      where matnr = collector_kn-matnr
        and spras = sy-langu.
    if sy-subrc = 0.
      wa_output-maktx = makt-maktx.
    endif.
    wa_output-spstk = collector_kn-lifnr.
*--- Unit proce
    wa_output-uprc = 0.
*--- calc value
    wa_output-val1 = wa_output-uprc * collector_kn-labst.
    wa_output-val2 = wa_output-uprc * collector_kn-insme.
    wa_output-val3 = wa_output-uprc * collector_kn-speme.
    wa_output-val4 = wa_output-uprc * collector_kn-einme.

    wa_output-tqty = wa_output-labst + wa_output-insme +
                     wa_output-speme + wa_output-einme.
    wa_output-tval = wa_output-val1 + wa_output-val2 +
                     wa_output-val3 + wa_output-val4.

    append wa_output to i_output.
  endloop.

*--- Check MARC for an in transit record. If found build a new record.
  loop at collector.
    clear marc.
    select single * from marc
      where matnr = collector-matnr
        and werks = collector-werks
        and ( trame gt 0 or
              umlmc gt 0 ).
    if sy-subrc = 0
       and ( marc-trame > 0 or marc-umlmc gt 0 ). "found and contains value
* in transit amount

      clear wa_output.

      move collector-matnr to wa_output-matnr.
      move collector-werks to wa_output-werks.
      wa_output-trame = marc-trame + marc-umlmc.
*--- get material text
      clear makt.
      select single * from makt
        where matnr = collector-matnr
          and spras = sy-langu.
      if sy-subrc = 0.
        wa_output-maktx = makt-maktx.
      endif.
      add wa_output-trame to wa_output-tqty.

*--- calc unit price
      "move collector-werks to lv_bwkey.
      SELECT SINGLE bwkey FROM t001w INTO lv_bwkey WHERE werks = collector_ln-werks.
      select single * from mbew
        where matnr = collector-matnr
          and bwkey = lv_bwkey.
      if sy-subrc = 0.
        wa_output-uprc = mbew-salk3 / mbew-lbkum.
      endif.

      wa_output-val5 = wa_output-uprc * wa_output-trame.
      move wa_output-trame to wa_output-tqty.
      add wa_output-val5 to wa_output-tval.
      append wa_output to i_output.
    endif.
  endloop.

  sort i_output by matnr werks lgort.

*-----Start of Comment by SALILV - PEDK908170
****  LOOP AT collector.
****
*****--- check for V_MMIM_LN Stock records.
****    LOOP AT collector_ln WHERE matnr = collector-matnr
****                           AND werks = collector-werks
****                           AND lgort = collector-lgort.
****
*****--- stock record found
****      CLEAR wa_output.
****
*****      MOVE-CORRESPONDING collector TO wa_output.
****      MOVE-CORRESPONDING collector_ln TO wa_output.
****
*****--- get material text
****      CLEAR makt.
****      SELECT SINGLE * FROM makt
****        WHERE matnr = collector_ln-matnr
****          AND spras = sy-langu.
****      IF sy-subrc = 0.
****        wa_output-maktx = makt-maktx.
****      ENDIF.
*****--- calc unit price
****      MOVE collector_ln-werks TO lv_bwkey.
****      SELECT SINGLE * FROM mbew
****        WHERE matnr = collector_ln-matnr
****          AND bwkey = lv_bwkey.
****      IF sy-subrc = 0.
****        wa_output-uprc = mbew-salk3 / mbew-lbkum.
****      ENDIF.
*****--- calc value
****      wa_output-val1 = wa_output-uprc * collector_ln-labst.
****      wa_output-val2 = wa_output-uprc * collector_ln-insme.
****      wa_output-val3 = wa_output-uprc * collector_ln-speme.
****      wa_output-val4 = wa_output-uprc * collector_ln-einme.
****
****      wa_output-tqty = wa_output-labst + wa_output-insme +
****                       wa_output-speme + wa_output-einme.
****      wa_output-tval = wa_output-val1 + wa_output-val2 +
****                       wa_output-val3 + wa_output-val4.
****
****      APPEND wa_output TO i_output.
****
****    ENDLOOP.
****
*****--- check for V_MMIM_EN Sales Order records
****    LOOP AT collector_en WHERE matnr = collector-matnr
****                           AND werks = collector-werks
****                           AND lgort = collector-lgort.
****
****      CLEAR wa_output.
*****--- stock record found
*****      MOVE-CORRESPONDING collector TO wa_output.
****      MOVE-CORRESPONDING collector_en TO wa_output.
****
*****--- get material text
****      CLEAR makt.
****      SELECT SINGLE * FROM makt
****        WHERE matnr = collector_en-matnr
****          AND spras = sy-langu.
****      IF sy-subrc = 0.
****        wa_output-maktx = makt-maktx.
****      ENDIF.
*****--- capture sales order and item
****      CONCATENATE collector_en-vbeln '/' collector_en-posnr
****      INTO wa_output-spstk SEPARATED BY ' '.
*****--- calc unit price
****      MOVE collector_en-werks TO lv_bwkey.
****      SELECT SINGLE * FROM ebew
****        WHERE matnr = collector_en-matnr
****          AND bwkey = lv_bwkey
****          AND bwtar = ' '
****          AND sobkz = 'E'
****          AND vbeln = collector_en-vbeln
****          AND posnr = collector_en-posnr.
****      IF sy-subrc = 0.
****        wa_output-uprc = ebew-salk3 / ebew-lbkum.
****      ENDIF.
*****--- calc value
****
****      wa_output-val1 = wa_output-uprc * collector_en-labst.
****      wa_output-val2 = wa_output-uprc * collector_en-insme.
****      wa_output-val3 = wa_output-uprc * collector_en-speme.
****      wa_output-val4 = wa_output-uprc * collector_en-einme.
****
****      wa_output-tqty = wa_output-labst + wa_output-insme +
****                       wa_output-speme + wa_output-einme.
****      wa_output-tval = wa_output-val1 + wa_output-val2 +
****                       wa_output-val3 + wa_output-val4.
****
****      APPEND wa_output TO i_output.
****    ENDLOOP.
****
*****--- check for V_MMIM_KN Vendor Consignment records.
****    LOOP AT collector_kn WHERE matnr = collector-matnr
****                           AND werks = collector-werks
****                           AND lgort = collector-lgort.
****
****      CLEAR wa_output.
*****--- vendor record found
*****      MOVE-CORRESPONDING collector TO wa_output.
****      MOVE-CORRESPONDING collector_kn TO wa_output.
*****--- get material text
****      CLEAR makt.
****      SELECT SINGLE * FROM makt
****        WHERE matnr = collector_kn-matnr
****          AND spras = sy-langu.
****      IF sy-subrc = 0.
****        wa_output-maktx = makt-maktx.
****      ENDIF.
****      wa_output-spstk = collector_kn-lifnr.
*****--- Unit proce
****      wa_output-uprc = 0.
*****--- calc value
****      wa_output-val1 = wa_output-uprc * collector_kn-labst.
****      wa_output-val2 = wa_output-uprc * collector_kn-insme.
****      wa_output-val3 = wa_output-uprc * collector_kn-speme.
****      wa_output-val4 = wa_output-uprc * collector_kn-einme.
****
****      wa_output-tqty = wa_output-labst + wa_output-insme +
****                       wa_output-speme + wa_output-einme.
****      wa_output-tval = wa_output-val1 + wa_output-val2 +
****                       wa_output-val3 + wa_output-val4.
****
****      APPEND wa_output TO i_output.
****    ENDLOOP.
****
*****--- Check MARC for an in transit record. If found build a new record.
****    CLEAR marc.
****    SELECT SINGLE * FROM marc
****      WHERE matnr = collector-matnr
****        AND werks = collector-werks.
****    IF sy-subrc = 0
****       AND marc-trame > 0. "found and contains value
***** in transit amount
****
****      CLEAR wa_output.
****      CLEAR i_output.
****
****      MOVE collector-matnr TO wa_output-matnr.
****      MOVE collector-werks TO wa_output-werks.
****      wa_output-trame = marc-trame.
*****--- get material text
****      CLEAR makt.
****      SELECT SINGLE * FROM makt
****        WHERE matnr = collector-matnr
****          AND spras = sy-langu.
****      IF sy-subrc = 0.
****        wa_output-maktx = makt-maktx.
****      ENDIF.
****      ADD wa_output-trame TO wa_output-tqty.
****
*****--- calc unit price
****      MOVE collector-werks TO lv_bwkey.
****      SELECT SINGLE * FROM mbew
****        WHERE matnr = collector-matnr
****          AND bwkey = lv_bwkey.
****      IF sy-subrc = 0.
****        wa_output-uprc = mbew-salk3 / mbew-lbkum.
****      ENDIF.
****
****      wa_output-val5 = wa_output-uprc * wa_output-trame.
****      MOVE wa_output-trame TO wa_output-tqty.
****      ADD wa_output-val5 TO wa_output-tval.
****      APPEND wa_output TO i_output.
****
****    ENDIF.
****
****  ENDLOOP.
****
****  SORT i_output BY matnr werks lgort.
*-----End of Comment by SALILV - PEDK908170

endform.                    "process_data

*&---------------------------------------------------------------------*
*&      Form  display_new_grid
*&---------------------------------------------------------------------*
* This routine displays the ALV grid
*----------------------------------------------------------------------*
form display_new_grid .

  data: gr_alv        type ref to cl_salv_table,
        lr_display    type ref to cl_salv_display_settings,
        lr_columns    type ref to cl_salv_columns_table,
        lr_column     type ref to cl_salv_column_table,
        lr_functions  type ref to cl_salv_functions_list,
        lr_sorts      type ref to cl_salv_sorts,
        lr_print  type ref to cl_salv_print,
        lr_column_f type ref to cl_salv_columns,
        gr_error    type ref to cx_salv_not_found ,
        gr_msg               type string,
      lr_event             type ref to cl_salv_events_table,
     lr_handler           type ref to znew_gc_event_handler  .

  data: key type salv_s_layout_key.
  data: lr_layout type ref to cl_salv_layout.

*--- Check to make sure the internal table has data.
  if lines( i_output ) > 0.

    try.
        " Create ALV instance - use CALL METHOD since this is a static method
        call method cl_salv_table=>factory
          importing
            r_salv_table = gr_alv
          changing
            t_table      = i_output.

        lr_display = gr_alv->get_display_settings( ).
        lr_display->set_list_header( text-ttl ).

*        " Get functions object and then set all the functions to be allowed
        lr_functions = gr_alv->get_functions( ).
        lr_functions->set_all( ).

        data: lv_field type lvc_fname.
*        " Get column settings object and then optimize the column widths to the data
        lr_columns = gr_alv->get_columns( ).
        lr_columns->set_optimize( ).

*--- set column header for custom field Special Stock Number
        try.
            lr_column ?= lr_columns->get_column( 'SOBKZ' ).
            lr_column->set_output_length('5').
            lr_column->set_long_text( 'Sp St' ).
          catch cx_salv_not_found into gr_error.
            gr_msg = gr_error->get_text( ).
            message gr_msg type 'I'.
        endtry.


*--- set column header for custom field Special Stock Number
        try.
            lr_column ?= lr_columns->get_column( 'SPSTK' ).
            lr_column->set_output_length('20').
            lr_column->set_long_text( 'Special Stock Number' ).
          catch cx_salv_not_found into gr_error.
            gr_msg = gr_error->get_text( ).
            message gr_msg type 'I'.
        endtry.

*--- set column header for custom field Unit Price
        try.
            lr_column ?= lr_columns->get_column( 'UPRC' ).
            lr_column->set_long_text( 'Unit Price' ).
            lr_column->set_medium_text( 'Unit Price' ).
            lr_column->set_short_text( 'Unit Price' ).
          catch cx_salv_not_found into gr_error.
            gr_msg = gr_error->get_text( ).
            message gr_msg type 'I'.
        endtry.

*--- set column header for custom field Unrestricted Stock Quantity
        try.
            lr_column ?= lr_columns->get_column( 'LABST' ).
            lr_column->set_output_length('12').
            lr_column->set_long_text( 'Unrestricted' ).
          catch cx_salv_not_found into gr_error.
            gr_msg = gr_error->get_text( ).
            message gr_msg type 'I'.
        endtry.

*--- set column header for custom field Unrestricted Stock Value
        try.
            lr_column ?= lr_columns->get_column( 'VAL1' ).
            lr_column->set_output_length('18').
            lr_column->set_long_text( 'Value Unrestricted' ).
          catch cx_salv_not_found into gr_error.
            gr_msg = gr_error->get_text( ).
            message gr_msg type 'I'.
        endtry.

*--- set column header for custom field Quality Inspection Stock
        try.
            lr_column ?= lr_columns->get_column( 'INSME' ).
            lr_column->set_output_length('16').
            lr_column->set_long_text( 'In Quality Insp.' ).
          catch cx_salv_not_found into gr_error.
            gr_msg = gr_error->get_text( ).
            message gr_msg type 'I'.
        endtry.

*--- set column header for custom field Quality Inspection Stock Value
        try.
            lr_column ?= lr_columns->get_column( 'VAL2' ).
            lr_column->set_output_length('18').
            lr_column->set_long_text( 'Value in QualInsp.' ).
          catch cx_salv_not_found into gr_error.
            gr_msg = gr_error->get_text( ).
            message gr_msg type 'I'.
        endtry.

*--- set column header for custom field Blocked Stock
        try.
            lr_column ?= lr_columns->get_column( 'SPEME' ).
            lr_column->set_output_length('7').
            lr_column->set_long_text( 'Blocked' ).
          catch cx_salv_not_found into gr_error.
            gr_msg = gr_error->get_text( ).
            message gr_msg type 'I'.
        endtry.

*--- set column header for custom field Blocked Stock Value
        try.
            lr_column ?= lr_columns->get_column( 'VAL3' ).
            lr_column->set_output_length('18').
            lr_column->set_long_text( 'Value BlockedStock' ).
          catch cx_salv_not_found into gr_error.
            gr_msg = gr_error->get_text( ).
            message gr_msg type 'I'.
        endtry.

*--- set column header for custom field Restricted Stock
        try.
            lr_column ?= lr_columns->get_column( 'EINME' ).
            lr_column->set_output_length('14').
            lr_column->set_long_text( 'Restricted-Use' ).
          catch cx_salv_not_found into gr_error.
            gr_msg = gr_error->get_text( ).
            message gr_msg type 'I'.
        endtry.

*--- set column header for custom field Restricted Stock Value
        try.
            lr_column ?= lr_columns->get_column( 'VAL4' ).
            lr_column->set_output_length('16').
            lr_column->set_long_text( 'Value Restricted' ).
          catch cx_salv_not_found into gr_error.
            gr_msg = gr_error->get_text( ).
            message gr_msg type 'I'.
        endtry.

*--- set column header for custom field Stock in Transit
        try.
            lr_column ?= lr_columns->get_column( 'TRAME' ).
            lr_column->set_output_length('15').
            lr_column->set_long_text( 'Transit/Transf.' ).
          catch cx_salv_not_found into gr_error.
            gr_msg = gr_error->get_text( ).
            message gr_msg type 'I'.
        endtry.

*--- set column header for custom field Stock in Transit Value
        try.
            lr_column ?= lr_columns->get_column( 'VAL5' ).
            lr_column->set_output_length('18').
            lr_column->set_long_text( 'Val. in Trans./Tfr' ).
          catch cx_salv_not_found into gr_error.
            gr_msg = gr_error->get_text( ).
            message gr_msg type 'I'.
        endtry.

*--- set column header for custom field Stock Total Quantity
        try.
            lr_column ?= lr_columns->get_column( 'TQTY' ).
            lr_column->set_output_length('14').
            lr_column->set_long_text( 'Total Quantity' ).
          catch cx_salv_not_found into gr_error.
            gr_msg = gr_error->get_text( ).
            message gr_msg type 'I'.
        endtry.

*--- set column header for custom field Stock Total Value
        try.
            lr_column ?= lr_columns->get_column( 'TVAL' ).
            lr_column->set_output_length('11').
            lr_column->set_long_text( 'Total Value' ).
          catch cx_salv_not_found into gr_error.
            gr_msg = gr_error->get_text( ).
            message gr_msg type 'I'.
        endtry.

        lr_event = gr_alv->get_event( ).
        create object lr_handler.
        set handler: lr_handler->on_link_click for lr_event.

*... Set Sort
        lr_sorts = gr_alv->get_sorts( ).

*        " Optimize the column widths for printing.
        lr_print = gr_alv->get_print( ).
        lr_print->set_print_parameters_enabled( value = 'X' ).
        lr_print->set_column_optimization( value = 'X' ).

*--- This code is to get the layout,save the layout and display the layout
        lr_layout = gr_alv->get_layout( ).
        key-report = sy-repid.
        lr_layout->set_key( key ).
        lr_layout->set_save_restriction( cl_salv_layout=>restrict_none ).

        data: init_layout type slis_vari.
        init_layout = p_layout.
        lr_layout->set_initial_layout( init_layout ).

*--- display report
        gr_alv->display( ).

      catch cx_salv_msg.
        write: 'Error displaying grid! - cx_salv_msg'.

      catch cx_salv_not_found.
        write: 'Error displaying grid! - cx_salv_not_found'.

      catch cx_salv_data_error.
        write: 'Error displaying grid! - cx_salv_data_error'.

      catch cx_salv_existing.
        write: 'Error displaying grid! - cx_salv_existing'.

    endtry.

  endif.

endform.                    " display_new_grid

*&---------------------------------------------------------------------*
*&      Form  validate
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

form validate .

*--- Validate plant entered
  if not s_werks[] is initial.
    select werks from t001w
      into t001w-werks
      up to 1 rows where werks in s_werks.
    endselect.
    if sy-subrc <> 0.
      message e000 with 'Invalid plant'.
    endif.
  endif.

*--- Validate storage location
  if not s_lgort[] is initial.
    select lgort from t001l
      into t001l-lgort
      up to 1 rows
      where lgort in s_lgort.
    endselect.
    if sy-subrc <> 0.
      message e000 with 'Invalid storage location'.
    endif.
  endif.

*--- Validate material entered
  if not s_matnr[] is initial.
    select matnr from mara
      into mara-matnr
      up to 1 rows
      where matnr in s_matnr.
    endselect.
    if sy-subrc <> 0.
      message e000 with 'Invalid material number'.
    endif.
  endif.

*--- Validate MRP controller
  if not s_dispo[] is initial.
    select dispo from t024d
      into t024d-dispo
      up to 1 rows
      where dispo in s_dispo.
    endselect.
    if sy-subrc <> 0.
      message e000 with 'Invalid MRP Controller'.
    endif.
  endif.

*--- Validate material group entered
  if not  s_matkl[] is initial.
    select matkl from t023
      into t023-matkl
      up to 1 rows
      where matkl in s_matkl.
    endselect.
    if sy-subrc <> 0.
      message e000 with 'Invalid material group'.
    endif.
  endif.

endform.                    " validate

*---------------------------------------------------------------------*
*      Form  variant_search
*---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LAYOUT  text
*----------------------------------------------------------------------*
form variant_search  changing p_layout type slis_vari.

  data : lv_exit      type c,
         ls_variant   type disvariant.

  ls_variant-report = syst-repid.


  call function 'REUSE_ALV_VARIANT_F4'
    exporting
      is_variant    = ls_variant
      i_save        = 'A'
    importing
      e_exit        = lv_exit
      es_variant    = ls_variant
    exceptions
      not_found     = 1
      program_error = 2
      others        = 3.

  if sy-subrc eq 0 and lv_exit is initial.
    p_layout = ls_variant-variant.
  endif.
endform.                    " variant_search
