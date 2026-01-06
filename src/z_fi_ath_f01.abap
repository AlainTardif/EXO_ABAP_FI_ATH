*&---------------------------------------------------------------------*
*& Include Z_FI_ATH_F01 - Sous-routines
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form INIT_DEFAULT_VALUES
*&---------------------------------------------------------------------*
FORM init_default_values.
  gv_gjahr = sy-datum(4).
ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
FORM get_data.
  DATA: lt_ebeln_with_client TYPE STANDARD TABLE OF ty_bkpf,
        lv_gjahr             TYPE gjahr.

  " Extraire l'exercice de la date
  lv_gjahr = p_bldat(4).

  " 1. Sélection des entêtes BKPF
  SELECT bukrs belnr gjahr bldat awtyp awkey waers usnam
    FROM bkpf
    INTO TABLE @gt_bkpf
    WHERE bukrs = @p_bukrs
      AND belnr IN @s_belnr
      AND gjahr >= @lv_gjahr
      AND bldat >= @p_bldat
      AND awtyp IN ('VBRK', 'BKPF').

  IF gt_bkpf IS INITIAL.
    MESSAGE 'Aucune pièce FI trouvée' TYPE 'S' DISPLAY LIKE 'W'.
    RETURN.
  ENDIF.

  " 2. Sélection des postes BSEG
  SELECT bukrs belnr gjahr buzei bschl koart wrbtr
    FROM bseg
    INTO TABLE @gt_bseg
    FOR ALL ENTRIES IN @gt_bkpf
    WHERE bukrs = @gt_bkpf-bukrs
      AND belnr = @gt_bkpf-belnr
      AND gjahr = @gt_bkpf-gjahr.

  " 3. Filtrer les pièces ayant au moins un poste client (KOART = 'D')
  LOOP AT gt_bkpf INTO DATA(ls_bkpf).
    LOOP AT gt_bseg INTO DATA(ls_bseg)
      WHERE bukrs = ls_bkpf-bukrs
        AND belnr = ls_bkpf-belnr
        AND gjahr = ls_bkpf-gjahr
        AND koart = 'D'.
      APPEND ls_bkpf TO lt_ebeln_with_client.
      EXIT.
    ENDLOOP.
  ENDLOOP.

  " Garder uniquement les pièces avec poste client
  gt_bkpf = lt_ebeln_with_client.

  IF gt_bkpf IS INITIAL.
    MESSAGE 'Aucune pièce avec poste client trouvée' TYPE 'S' DISPLAY LIKE 'W'.
    RETURN.
  ENDIF.

  " 4. Récupérer les données des pièces de référence
  PERFORM get_reference_data.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_REFERENCE_DATA
*&---------------------------------------------------------------------*
FORM get_reference_data.
  DATA: lt_vbeln TYPE STANDARD TABLE OF vbeln_vf,
        lt_ref_keys TYPE STANDARD TABLE OF ty_bkpf,
        lv_ref_belnr TYPE belnr_d,
        lv_ref_bukrs TYPE bukrs,
        lv_ref_gjahr TYPE gjahr.

  " Collecter les références factures (VBRK)
  LOOP AT gt_bkpf INTO DATA(ls_bkpf) WHERE awtyp = 'VBRK'.
    APPEND ls_bkpf-awkey(10) TO lt_vbeln.
  ENDLOOP.

  SORT lt_vbeln.
  DELETE ADJACENT DUPLICATES FROM lt_vbeln.

  IF lt_vbeln IS NOT INITIAL.
    SELECT vbeln ernam
      FROM vbrk
      INTO TABLE @gt_vbrk
      FOR ALL ENTRIES IN @lt_vbeln
      WHERE vbeln = @lt_vbeln-table_line.
  ENDIF.

  " Collecter les références pièces FI (BKPF)
  LOOP AT gt_bkpf INTO ls_bkpf WHERE awtyp = 'BKPF'.
    " AWKEY = BELNR(10) + BUKRS(4) + GJAHR(4)
    lv_ref_belnr = ls_bkpf-awkey(10).
    lv_ref_bukrs = ls_bkpf-awkey+10(4).
    lv_ref_gjahr = ls_bkpf-awkey+14(4).

    DATA(ls_ref_key) = VALUE ty_bkpf(
      bukrs = lv_ref_bukrs
      belnr = lv_ref_belnr
      gjahr = lv_ref_gjahr ).
    APPEND ls_ref_key TO lt_ref_keys.
  ENDLOOP.

  SORT lt_ref_keys BY bukrs belnr gjahr.
  DELETE ADJACENT DUPLICATES FROM lt_ref_keys COMPARING bukrs belnr gjahr.

  IF lt_ref_keys IS NOT INITIAL.
    SELECT bukrs belnr gjahr bldat awtyp awkey waers usnam
      FROM bkpf
      INTO TABLE @gt_bkpf_ref
      FOR ALL ENTRIES IN @lt_ref_keys
      WHERE bukrs = @lt_ref_keys-bukrs
        AND belnr = @lt_ref_keys-belnr
        AND gjahr = @lt_ref_keys-gjahr.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form PROCESS_DATA
*&---------------------------------------------------------------------*
FORM process_data.
  DATA: ls_output TYPE ty_output,
        ls_color  TYPE lvc_s_scol,
        lv_ref_belnr TYPE belnr_d,
        lv_ref_bukrs TYPE bukrs,
        lv_ref_gjahr TYPE gjahr.

  CLEAR gt_output.

  " Parcourir les entêtes
  LOOP AT gt_bkpf INTO DATA(ls_bkpf).
    " Créer une ligne par poste
    LOOP AT gt_bseg INTO DATA(ls_bseg)
      WHERE bukrs = ls_bkpf-bukrs
        AND belnr = ls_bkpf-belnr
        AND gjahr = ls_bkpf-gjahr.

      CLEAR: ls_output.

      " Données entête (colonnes bleues)
      ls_output-bukrs = ls_bkpf-bukrs.
      ls_output-belnr = ls_bkpf-belnr.
      ls_output-gjahr = ls_bkpf-gjahr.
      ls_output-bldat = ls_bkpf-bldat.
      ls_output-awtyp = ls_bkpf-awtyp.
      ls_output-waers = ls_bkpf-waers.

      " Données poste
      ls_output-buzei = ls_bseg-buzei.
      ls_output-bschl = ls_bseg-bschl.
      ls_output-koart = ls_bseg-koart.
      ls_output-wrbtr = ls_bseg-wrbtr.

      " Traitement référence selon AWTYP
      IF ls_bkpf-awtyp = 'BKPF'.
        " Référence FI : AWKEY = BELNR(10) + BUKRS(4) + GJAHR(4)
        ls_output-ref_belnr = ls_bkpf-awkey(10).
        ls_output-ref_bukrs = ls_bkpf-awkey+10(4).
        ls_output-ref_gjahr = ls_bkpf-awkey+14(4).
        " Récupérer le créateur de la pièce de référence
        READ TABLE gt_bkpf_ref INTO DATA(ls_bkpf_ref)
          WITH KEY bukrs = ls_output-ref_bukrs
                   belnr = ls_output-ref_belnr
                   gjahr = ls_output-ref_gjahr.
        IF sy-subrc = 0.
          ls_output-ref_ernam = ls_bkpf_ref-usnam.
        ENDIF.
      ELSEIF ls_bkpf-awtyp = 'VBRK'.
        " Référence facture
        ls_output-ref_vbeln = ls_bkpf-awkey(10).
        " Récupérer le créateur de la facture
        READ TABLE gt_vbrk INTO DATA(ls_vbrk)
          WITH KEY vbeln = ls_output-ref_vbeln.
        IF sy-subrc = 0.
          ls_output-ref_ernam = ls_vbrk-ernam.
        ENDIF.
      ENDIF.

      " Couleurs colonnes
      PERFORM set_row_colors CHANGING ls_output-color.

      APPEND ls_output TO gt_output.
    ENDLOOP.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form SET_ROW_COLORS
*&---------------------------------------------------------------------*
FORM set_row_colors CHANGING ct_color TYPE lvc_t_scol.
  DATA: ls_color TYPE lvc_s_scol.

  CLEAR ct_color.

  " Colonnes bleues (entête)
  ls_color-color-col = 1. " Bleu
  ls_color-color-int = 0.
  ls_color-color-inv = 0.

  ls_color-fname = 'BUKRS'. APPEND ls_color TO ct_color.
  ls_color-fname = 'BELNR'. APPEND ls_color TO ct_color.
  ls_color-fname = 'GJAHR'. APPEND ls_color TO ct_color.
  ls_color-fname = 'BLDAT'. APPEND ls_color TO ct_color.
  ls_color-fname = 'AWTYP'. APPEND ls_color TO ct_color.

  " Colonnes oranges/rouges (référence et postes)
  ls_color-color-col = 7. " Orange
  ls_color-fname = 'REF_BUKRS'. APPEND ls_color TO ct_color.
  ls_color-fname = 'REF_BELNR'. APPEND ls_color TO ct_color.
  ls_color-fname = 'REF_GJAHR'. APPEND ls_color TO ct_color.
  ls_color-fname = 'REF_VBELN'. APPEND ls_color TO ct_color.
  ls_color-fname = 'REF_ERNAM'. APPEND ls_color TO ct_color.
  ls_color-fname = 'BUZEI'. APPEND ls_color TO ct_color.
  ls_color-fname = 'BSCHL'. APPEND ls_color TO ct_color.
  ls_color-fname = 'KOART'. APPEND ls_color TO ct_color.
  ls_color-fname = 'WAERS'. APPEND ls_color TO ct_color.
  ls_color-fname = 'WRBTR'. APPEND ls_color TO ct_color.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV
*&---------------------------------------------------------------------*
FORM display_alv.
  DATA: lo_columns TYPE REF TO cl_salv_columns_table,
        lo_column  TYPE REF TO cl_salv_column,
        lo_events  TYPE REF TO cl_salv_events_table.

  IF gt_output IS INITIAL.
    MESSAGE 'Aucune donnée à afficher' TYPE 'S' DISPLAY LIKE 'W'.
    RETURN.
  ENDIF.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = go_alv
        CHANGING
          t_table      = gt_output ).

      " Colonnes
      lo_columns = go_alv->get_columns( ).
      lo_columns->set_optimize( abap_true ).
      lo_columns->set_color_column( 'COLOR' ).

      " Renommer les colonnes
      PERFORM set_column_texts USING lo_columns.

      " Activer la toolbar et ajouter le bouton Export
      go_alv->get_functions( )->set_all( abap_true ).

      " Afficher
      go_alv->display( ).

    CATCH cx_salv_msg cx_salv_not_found.
      MESSAGE 'Erreur affichage ALV' TYPE 'E'.
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form SET_COLUMN_TEXTS
*&---------------------------------------------------------------------*
FORM set_column_texts USING io_columns TYPE REF TO cl_salv_columns_table.
  DATA: lo_column TYPE REF TO cl_salv_column.

  TRY.
      lo_column = io_columns->get_column( 'BUKRS' ).
      lo_column->set_short_text( 'Co.' ).
      lo_column->set_medium_text( 'Company Code' ).
      lo_column->set_long_text( 'Company Code' ).

      lo_column = io_columns->get_column( 'BELNR' ).
      lo_column->set_short_text( 'Document' ).
      lo_column->set_medium_text( 'Document Number' ).
      lo_column->set_long_text( 'Document Number' ).

      lo_column = io_columns->get_column( 'GJAHR' ).
      lo_column->set_short_text( 'Fis.' ).
      lo_column->set_medium_text( 'Fiscal Year' ).
      lo_column->set_long_text( 'Fiscal Year' ).

      lo_column = io_columns->get_column( 'BLDAT' ).
      lo_column->set_short_text( 'Doc. Date' ).
      lo_column->set_medium_text( 'Document Date' ).
      lo_column->set_long_text( 'Document Date' ).

      lo_column = io_columns->get_column( 'AWTYP' ).
      lo_column->set_short_text( 'Ref.' ).
      lo_column->set_medium_text( 'Ref. Transactn' ).
      lo_column->set_long_text( 'Ref. Transaction' ).

      lo_column = io_columns->get_column( 'REF_BUKRS' ).
      lo_column->set_short_text( 'CoC' ).
      lo_column->set_medium_text( 'Ref. Company' ).
      lo_column->set_long_text( 'Reference Company Code' ).

      lo_column = io_columns->get_column( 'REF_BELNR' ).
      lo_column->set_short_text( 'Document' ).
      lo_column->set_medium_text( 'Ref. Document' ).
      lo_column->set_long_text( 'Reference Document Number' ).

      lo_column = io_columns->get_column( 'REF_GJAHR' ).
      lo_column->set_short_text( 'Fis.' ).
      lo_column->set_medium_text( 'Ref. Fiscal Year' ).
      lo_column->set_long_text( 'Reference Fiscal Year' ).

      lo_column = io_columns->get_column( 'REF_VBELN' ).
      lo_column->set_short_text( 'Billing' ).
      lo_column->set_medium_text( 'Billing Doc.' ).
      lo_column->set_long_text( 'Billing Document' ).

      lo_column = io_columns->get_column( 'REF_ERNAM' ).
      lo_column->set_short_text( 'User' ).
      lo_column->set_medium_text( 'User name' ).
      lo_column->set_long_text( 'User name' ).

      lo_column = io_columns->get_column( 'BUZEI' ).
      lo_column->set_short_text( 'It.' ).
      lo_column->set_medium_text( 'Line item' ).
      lo_column->set_long_text( 'Line item' ).

      lo_column = io_columns->get_column( 'BSCHL' ).
      lo_column->set_short_text( 'P.' ).
      lo_column->set_medium_text( 'Posting key' ).
      lo_column->set_long_text( 'Posting key' ).

      lo_column = io_columns->get_column( 'KOART' ).
      lo_column->set_short_text( 'A' ).
      lo_column->set_medium_text( 'Account Type' ).
      lo_column->set_long_text( 'Account Type' ).

      lo_column = io_columns->get_column( 'WAERS' ).
      lo_column->set_short_text( 'Curr.' ).
      lo_column->set_medium_text( 'Currency' ).
      lo_column->set_long_text( 'Currency' ).

      lo_column = io_columns->get_column( 'WRBTR' ).
      lo_column->set_short_text( 'Amount' ).
      lo_column->set_medium_text( 'Amount' ).
      lo_column->set_long_text( 'Amount' ).

      " Cacher la colonne COLOR
      lo_column = io_columns->get_column( 'COLOR' ).
      lo_column->set_visible( abap_false ).

    CATCH cx_salv_not_found.
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form EXPORT_TO_CSV
*&---------------------------------------------------------------------*
FORM export_to_csv.
  DATA: lt_csv    TYPE STANDARD TABLE OF string,
        lv_line   TYPE string,
        lv_sep    TYPE c VALUE ';'.

  CHECK p_file IS NOT INITIAL.
  CHECK gt_output IS NOT INITIAL.

  " En-tête CSV
  lv_l