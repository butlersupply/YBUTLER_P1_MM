*&---------------------------------------------------------------------*
*& Report  ZMM_BTL_AUTO_AB
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zmm_btl_auto_ab LINE-SIZE 195
                        LINE-COUNT 58
                        NO STANDARD PAGE HEADING.

"*******************
"    declaration
"*******************
INCLUDE zmm_btl_auto_ab_top                     .    " global Data

TABLES:  mkpf,   " Movement Header
         mseg,   " Movement Detail
         mara,   " Matl Master - General Data
         marc,   " Material Master - Plant Data
         mbew,   " Matl Master - Accounting
         makt.   " Matl Desc

SELECTION-SCREEN BEGIN OF BLOCK crt WITH FRAME TITLE text-crt.
SELECT-OPTIONS:
         s_matnr FOR mseg-matnr,  " matl
         s_werks FOR mseg-werks,  " plant
         s_lgort FOR marc-lgort OBLIGATORY NO-EXTENSION NO INTERVALS,  " storage location
         s_matkl FOR mara-matkl,  " matl group
         s_ekgrp FOR marc-ekgrp,  " purchasing group
         s_mmsta FOR marc-mmsta,  " branch-spc material status
         s_maabc FOR marc-maabc,  " abc_class
         s_bklas FOR mbew-bklas.  " valclass
SELECTION-SCREEN END OF BLOCK crt.

SELECTION-SCREEN BEGIN OF BLOCK mov WITH FRAME TITLE text-mov.
SELECT-OPTIONS:
         s_budat FOR mkpf-budat.  " posting date
SELECTION-SCREEN END OF BLOCK mov.

SELECTION-SCREEN BEGIN OF BLOCK smv WITH FRAME TITLE text-smv.
PARAMETERS: c3 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK smv.

SELECTION-SCREEN BEGIN OF BLOCK cf1 WITH FRAME TITLE text-cf1.
SELECT-OPTIONS:
         s_minday FOR marc-plifz DEFAULT '60'    OBLIGATORY NO-EXTENSION NO INTERVALS,
         s_cycday FOR marc-plifz DEFAULT '4'     OBLIGATORY NO-EXTENSION NO INTERVALS.
SELECT-OPTIONS:
         s_ldhits FOR marc-plifz DEFAULT '5'     OBLIGATORY NO-EXTENSION NO INTERVALS,
         s_ldpct  FOR marc-plifz DEFAULT '75'    OBLIGATORY NO-EXTENSION NO INTERVALS.
SELECT-OPTIONS:
         s_htpct  FOR marc-plifz DEFAULT '75'    OBLIGATORY NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN END OF BLOCK cf1.

TYPES: BEGIN OF str_s,
  matnr TYPE matnr,
  werks TYPE werks_d,
  maabc TYPE maabc,      " ABC Class
  minbe TYPE minbe,      " Reorder Point
  mabst TYPE mabst,      " Max Stock Level
  eisbe TYPE eisbe,      " Safety Stock
  lgrad TYPE lgrad,      " Service Level %
  sobsl TYPE sobsl,      " Special Procurement Key
  plifz TYPE plifz,      " Planned Delivery Time
  mmsta TYPE mmsta,      " Branch-sp.matl status (Frozen Indicator)
  peinh TYPE peinh,      " Average Cost UM
  verpr TYPE verpr,      " Average Cost
  mstae TYPE mstae,      " X-Branch Matl Status (Obsolete Indicator).

  ohb     TYPE labst,      " Unrestricted Stock (OHB - doesn not include sales order stock)
  on_ord  TYPE labst,      " Back ordered (all inbound PO's, STO's, STO deliveries, and in-transit)
  alloc   TYPE labst,      " Allocated (all outbound sales orders, deliveries, STO's, STO deliveries)
  in_pick TYPE labst,      " The amount allocated that is actuall on deliveries.
  atp_p   TYPE labst,      " ATP Quantity (this number does not include any unsaved sales orders)
  atp_s   TYPE labst,      " ATP Quantity (this number does not include any unsaved sales orders)
  bkord_p TYPE labst,      " The amount that cannot be satisfied by the ohb.
  bkord_s TYPE labst,      " The amount that cannot be satisfied by the ohb (not including on-order).
  sos     type labst,      " Sales order stock
END OF str_s.

TYPES: BEGIN OF str_m,
  matnr   TYPE matnr,           " Material Number
  werks   TYPE werks_d,         " Branch
  mblnr   TYPE mblnr,           " Number of Material Document
  mjahr	  TYPE mjahr,           " Material Document Year
  budat   TYPE budat,           " Posting Date in the Document
  bwart   TYPE bwart,           " Movement Type (Inventory Management)
  menge   TYPE rescconsumption, " Consumption
  meins   TYPE meins,           " Base Unit of Measure
  shkzg   TYPE shkzg,           " Debit/Credit Indicator
  xauto	  TYPE xauto,           " Line item automatically created
  xblnr   TYPE xblnr,           " Delivery number reference from the movement type table
  vgbel   TYPE vgbel,           " Sales order number reference from delivery detail table (used to calculate hits)
  zbucket TYPE rescconsumption,
  zflag	  TYPE i,
END OF str_m.

TYPES: BEGIN OF str_global_constants,
  cycle_days             TYPE i,
  min_hit_days           TYPE i,
  storage_loc            TYPE bapicm61v-lgort,
END OF str_global_constants.

DATA:    cg TYPE str_global_constants.

DATA:    gt_s  TYPE STANDARD TABLE OF str_s,
         gt_m  TYPE STANDARD TABLE OF str_m.

DATA:    g_sample_days            TYPE i.

"********************************
"    initialization variables
"********************************
cg-min_hit_days = s_minday-low.
cg-storage_loc  = s_lgort-low.

*******************************************************************************
START-OF-SELECTION.
*******************************************************************************
  PERFORM get_material CHANGING gt_s.
  IF gt_s[] IS INITIAL.
    MESSAGE s000(z002).
    WRITE / '   no data found.  Doing EXIT.'.
    EXIT.
  ENDIF.

*******************************************************************************
END-OF-SELECTION.
*******************************************************************************
  PERFORM do_work.

*&---------------------------------------------------------------------*
*&      Form  do_work
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM do_work.
  DATA: l_last_matnr       TYPE matnr,
        l_sample_count     TYPE i.

  FIELD-SYMBOLS:
         <s>      TYPE str_s,
         <m>      TYPE str_m,
         <m_work> TYPE str_m.



  PERFORM drop_above_rop USING cg-storage_loc CHANGING gt_s.



  LOOP AT gt_s ASSIGNING <s>.




    """ IS THIS ITEM OBSOLETE AT THE COMPANY LEVEL? """
    IF <s>-mstae <> ' '.
      <s>-mmsta = <s>-mstae.  " If the Company level is set, then set the plant to the same.
    ENDIF.









    "on my material select, I should only look at materials below their rop (marc-minbe)


    "need to calc the lead time here...
    "need to get inventory level and amount on order



    """ GET THE MOVEMENTS FOR THIS MATERIAL """
    PERFORM get_movements USING gt_s <s>   CHANGING gt_m.
    IF NOT gt_m[] IS INITIAL.
      PERFORM set_sign_of_movement         CHANGING gt_m.
      PERFORM drop_reversals               CHANGING gt_m.
      PERFORM combine_sales_orders         CHANGING gt_m.
      IF c3 = 'X'.
        PERFORM combine_daily_consumption  CHANGING gt_m.
      ENDIF.

      """ SEE IF I HAVE ENOUGH HITS TO CONTINUE """
      DESCRIBE TABLE gt_m LINES l_sample_count.
      IF l_sample_count > 0.
        SORT gt_m ASCENDING BY budat.
        READ TABLE gt_m INDEX 1 ASSIGNING <m_work>.
        g_sample_days = sy-datum - <m_work>-budat.

        """ IF WE HAVE NOT HAD A HIT IN AT LEAST THIS NUMBER OF DAYS THEN DO NOTHING """
        IF g_sample_days > cg-min_hit_days.




        ENDIF.
      ENDIF.
    ENDIF.



  ENDLOOP.

  WRITE / '...done'.
ENDFORM.                    "do_work

*&---------------------------------------------------------------------*
*&      Form  get_material
*&---------------------------------------------------------------------*
*       Get the list of material to be processed based on the users
*       selection.
*----------------------------------------------------------------------*
FORM get_material CHANGING pt_s LIKE gt_s.
  CLEAR pt_s.

  SELECT a~matnr a~werks a~maabc a~minbe a~mabst a~eisbe a~lgrad a~sobsl
    a~plifz a~mmsta b~peinh b~verpr c~mstae
         FROM marc AS a
         JOIN mbew AS b
           ON a~matnr = b~matnr
         JOIN mara AS c
           ON a~matnr = c~matnr
         APPENDING CORRESPONDING FIELDS OF TABLE pt_s
         WHERE a~matnr IN s_matnr
           AND a~werks IN s_werks
           AND a~mmsta IN s_mmsta
           AND a~maabc IN s_maabc
           AND a~ekgrp IN s_ekgrp
           AND b~bklas IN s_bklas
           AND c~matkl IN s_matkl.

  SORT pt_s ASCENDING BY matnr werks.
ENDFORM.                    "get_selection

*&---------------------------------------------------------------------*
*&      Form  get_movements
*&---------------------------------------------------------------------*
*       Get the movements for one material/plant.
*----------------------------------------------------------------------*
FORM get_movements
        USING    pt_s LIKE gt_s
                 p_s  TYPE str_s
        CHANGING pt_m LIKE gt_m.

  "****************************
  "    initialize variables
  "****************************
  CLEAR pt_m.

  "**********************
  "    function logic
  "**********************
  SELECT a~matnr " Material Number
         a~werks " Branch
         a~mblnr " Material Document
         b~mjahr " Material Year
         b~budat " Posting Date
         b~xblnr " Reference (Delivery Number)
         a~bwart " Goods Movement Type
         a~menge " Quantity
         a~meins " UOM
         a~shkzg " Debit/Credit Key
         a~xauto " STO Statistical Flag
       FROM mseg AS a
       JOIN mkpf AS b
         ON a~mblnr = b~mblnr   " Material Document Number
        AND a~mjahr = b~mjahr   " Material Document Year
       APPENDING CORRESPONDING FIELDS OF TABLE pt_m
       WHERE a~matnr = p_s-matnr
         AND a~werks = p_s-werks
         AND a~bwart IN ('601','602')  " Sales order PGI and it's reversal.
         AND b~budat IN s_budat.
ENDFORM.                    "get_movements

*&---------------------------------------------------------------------*
*&      Form  drop_above_rop
*&---------------------------------------------------------------------*
*       Drop all material rows where the available is at or above the
*       the ROP
*----------------------------------------------------------------------*
FORM drop_above_rop
        USING    value(p_storage_loc) TYPE bapicm61v-lgort
        CHANGING pt_s                 LIKE gt_s.

  "********************
  "    declaration
  "********************
  TYPES: BEGIN OF str_sos,
    MATNR type MATNR,        " Material
    WERKS type WERKS,        " Banch
    VBELN type VBELN,        " Order number
    POSNR type POSNR,        " Order line number
    KALAB type LABST,        " Unrestricted Stock
  END OF str_sos.

  DATA:  sos TYPE STANDARD TABLE OF str_sos.

  DATA:  zmrp_list          TYPE  bapi_mrp_list,
         zmrp_control_param TYPE  bapi_mrp_control_param,
         zmrp_stock_detail  TYPE  bapi_mrp_stock_detail,
         zreturn            TYPE  bapiret2,
         zmrp_items         LIKE TABLE OF bapi_mrp_items,
         zmrp_ind_lines     LIKE TABLE OF bapi_mrp_ind_lines,
         zmrp_total_lines   LIKE TABLE OF bapi_mrp_total_lines,
         zextensionout      LIKE TABLE OF bapiparex.

  FIELD-SYMBOLS: <s>        TYPE str_s,
                 <sos_work> TYPE str_sos.

  "****************************
  "    initialize variables
  "****************************
  SELECT VBELN POSNR KALAB           " Read all sales order stock into this itab.
         FROM mska
         APPENDING CORRESPONDING FIELDS OF TABLE sos
         WHERE matnr IN s_matnr
           AND werks IN s_werks
           AND LGORT IN s_lgort.

  "**********************
  "    function logic
  "**********************
  LOOP AT pt_s ASSIGNING <s>.

    """ READ THE STOCK REQUIREMENTS LIST FOR THE CURRENT MATERIAL """
    CALL FUNCTION 'BAPI_MATERIAL_STOCK_REQ_LIST'
      EXPORTING
        material          = <s>-matnr
        plant             = <s>-werks
       GET_ITEM_DETAILS  = 'X'
       GET_IND_LINES     = 'X'
       GET_TOTAL_LINES   = 'X'
      IMPORTING
        mrp_list          = zmrp_list
        mrp_control_param = zmrp_control_param
        mrp_stock_detail  = zmrp_stock_detail
        return            = zreturn
      TABLES
        mrp_items         = zmrp_items
        mrp_ind_lines     = zmrp_ind_lines
        mrp_total_lines   = zmrp_total_lines
        extensionout      = zextensionout.

    """ STORE RESULTS OF BAPI CALL """
    <s>-ohb     = zmrp_stock_detail-unrestricted_stck.
    <s>-on_ord  = zmrp_stock_detail-fixed_recpt.             " + zmrp_stock_detail-stck_in_transit.
    <s>-alloc   = zmrp_stock_detail-fixed_issues.
    <s>-in_pick = zmrp_stock_detail-delivery.                " Amount on deliveries (both sales and STO's)
    <s>-atp_p   = <s>-ohb + <s>-on_ord - <s>-alloc.          " Purchasing friendly
    <s>-atp_s   = <s>-ohb - <s>-alloc.                       " Sales friendly
    <s>-bkord_p = ( <s>-ohb + <s>-on_ord - <s>-alloc ) * -1. " Purchasing friendly
    <s>-bkord_s = ( <s>-ohb - <s>-alloc ) * -1.              " Sales friendly

    """ LOAD SALES ORDER STOCK VALUES """
    LOOP AT sos ASSIGNING <sos_work> WHERE matnr = <s>-matnr AND werks = <s>-werks.
      <s>-sos = <s>-sos + <sos_work>-KALAB.
    endloop.









    " The amount of sales order stock from sales orders
    " The amount of sales order stock on Purchase orders


    " Drop ships don't show up at all in the md04


    """ Back ordered cannot be less than zero """
    IF <s>-bkord_p < 0.
      <s>-bkord_p = 0.
    ENDIF.
    IF <s>-bkord_s < 0.
      <s>-bkord_s = 0.
    ENDIF.

    """ ATP CANNOT BE LESS THAN ZERO """
    IF <s>-atp_p < 0.
      <s>-atp_p   = 0.
    ENDIF.
    IF <s>-atp_s < 0.
      <s>-atp_s   = 0.
    ENDIF.

  ENDLOOP.


  "talk to Jill about the cycle count AND the ab-report programs.  Are they using the correct calcs?




  "DELETE pt_s WHERE zflag = 1.
ENDFORM.                    "drop_above_rop












*&---------------------------------------------------------------------*
*&      Form  set_sign_of_movement
*&---------------------------------------------------------------------*
*       Adjust sign based on debit/credit.  Note that 'H' is a negitive
*       inventory movement but a positive consumption.
*----------------------------------------------------------------------*
FORM set_sign_of_movement CHANGING pt_m LIKE gt_m.
  FIELD-SYMBOLS: <m> TYPE str_m.

  LOOP AT pt_m ASSIGNING <m>.
    CASE <m>-shkzg.
      WHEN 'S'.
        MULTIPLY <m>-menge BY -1.
      WHEN 'H'.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    "set_sign_of_movement

*&---------------------------------------------------------------------*
*&      Form  drop_reversals
*&---------------------------------------------------------------------*
*       Drop the reversal and the matching origional.
*----------------------------------------------------------------------*
FORM drop_reversals CHANGING pt_m LIKE gt_m.
  FIELD-SYMBOLS: <m>      TYPE str_m,
                 <m_work> TYPE str_m.

  SORT pt_m BY mblnr mjahr.    " Sort by doc number and year.

  LOOP AT pt_m ASSIGNING <m>.
    IF <m>-bwart = '602' OR <m>-bwart = '642' .
      CLEAR mkpf.

*     ***  FIND THE HEADER REC OF THE ORIGIONAL ***
      SELECT SINGLE a~mblnr a~mjahr       " doc number and year of the document that was reversed.
             INTO (mkpf-mblnr,mkpf-mjahr)
             FROM mkpf AS a
             JOIN mseg AS b
             ON   a~mblnr = b~smbln AND   " Head doc joined ON detail reference doc number
                  a~mjahr = b~sjahr       " Head doc joined ON detail reference doc year
             WHERE b~mblnr = <m>-mblnr    " Detail doc  = output doc number
               AND b~mjahr = <m>-mjahr    " Detail year = output doc year
               AND b~matnr = <m>-matnr    " Detail matl = output matl
               AND NOT b~smbln = space.   " Reference not = space.

*     *** MARK THE MATCHING ORIGIONAL FOR DELETION ***
      LOOP AT pt_m ASSIGNING <m_work> WHERE mblnr = mkpf-mblnr AND mjahr = mkpf-mjahr.
        <m_work>-zflag = 1.
      ENDLOOP.

*     *** ALSO MARK THE REVERSAL FOR DELETION ***
      <m>-zflag = 1.
    ENDIF.
  ENDLOOP.

  DELETE pt_m WHERE zflag = 1.
ENDFORM.                    "drop_reversals

*&---------------------------------------------------------------------*
*&      Form  combine_sales_orders
*&---------------------------------------------------------------------*
*       Combine movements for the same sales order number into one hit.
*----------------------------------------------------------------------*
FORM combine_sales_orders CHANGING pt_m LIKE gt_m.
  DATA: l_i          TYPE i.
  DATA: l_last_vgbel TYPE vgbel.

  FIELD-SYMBOLS: <m>      TYPE str_m,
                 <m_work> TYPE str_m.

  """ READ THE MATCHING SALES ORDER NUMBER. NOTE THAT THIS CODE WORKS """
  """ FOR STO DELIVERIES ALSO (IT RETURNS THE STO NUMBER).  I ALREADY """
  """ KNOW THE STO NUMBER FROM MY MATERIAL MOVEMENT SELECT STATEMENT  """
  """ SO I COULD DO SOMETHING WITH THAT TO MAKE THE PROGRAM FASTER    """
  """ IF PERFORMANCE PROGLEMS COME UP.                                """
  LOOP AT pt_m ASSIGNING <m>.
    SELECT SINGLE vgbel FROM lips INTO <m>-vgbel WHERE vbeln = <m>-xblnr.
  ENDLOOP.

  SORT pt_m BY vgbel budat.   " Sort by sales order number and date.

  LOOP AT pt_m ASSIGNING <m>.
    IF sy-tabix > 1.
      IF <m>-vgbel = l_last_vgbel.
        READ TABLE pt_m INDEX l_i ASSIGNING <m_work>.
        IF sy-subrc = 0.
          <m_work>-menge = <m_work>-menge + <m>-menge.
          <m>-zflag = 1.
        ENDIF.
      ELSE.
        l_i = sy-tabix.
        l_last_vgbel = <m>-vgbel.
      ENDIF.
    ELSE.
      l_i = 1.
      l_last_vgbel = <m>-vgbel.
    ENDIF.
  ENDLOOP.

  DELETE pt_m WHERE zflag = 1.
ENDFORM.                  "select_Data

*&---------------------------------------------------------------------*
*&      Form  combine_daily_consumption
*&---------------------------------------------------------------------*
*       You could have more than one order per day.  Combine the daily
*       activity into one day buckets.
*----------------------------------------------------------------------*
FORM combine_daily_consumption CHANGING pt_m LIKE gt_m.
  DATA: l_i          TYPE i.
  DATA: l_last_budat TYPE budat.

  FIELD-SYMBOLS: <m>      TYPE str_m,
                 <m_work> TYPE str_m.

  SORT pt_m BY budat.   " Sort by date.

  LOOP AT pt_m ASSIGNING <m>.
    IF sy-tabix > 1.
      IF <m>-budat = l_last_budat.
        READ TABLE pt_m INDEX l_i ASSIGNING <m_work>.
        IF sy-subrc = 0.
          <m_work>-menge = <m_work>-menge + <m>-menge.
          <m>-zflag = 1.
        ENDIF.
      ELSE.
        l_i = sy-tabix.
        l_last_budat = <m>-budat.
      ENDIF.
    ELSE.
      l_i = 1.
      l_last_budat = <m>-budat.
    ENDIF.
  ENDLOOP.

  DELETE pt_m WHERE zflag = 1.
ENDFORM.                  "select_Data
