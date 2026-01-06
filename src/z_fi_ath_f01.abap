*&---------------------------------------------------------------------*
*& Include Z_FI_ATH_F01 - Sous-routines (syntaxe moderne)
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
  DATA: lv_gjahr             TYPE gjahr,
        lt_ebeln_with_client TYPE STANDARD TABLE OF ty_bkpf.

  lv_gjahr = p_bldat(4).

  " 1. Sélection des entêtes BKPF
  SELECT bukrs, belnr, gjahr, bldat, awtyp, awkey, waers, usnam
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
  SELECT bukrs, belnr, gjahr, buzei, bschl, koart, wrbtr
    FROM bseg
    INTO TABLE @gt_bseg
    FOR ALL ENTRIES IN @gt_bkpf
    WHERE bukrs = @gt_bkpf-bukrs
      AND belnr = @gt_bkpf-belnr
      AND gjahr = @gt_bkpf-gjahr.

  " 3. Filtrer les pièces avec poste client
  LOOP AT gt_bkpf INTO DATA(ls_bkpf).
    LOOP AT gt_bseg TRANSPORTING NO FIELDS
      WHERE bukrs = ls_bkpf-bukrs
        AND belnr = ls_bkpf-belnr
        AND gjahr = ls_bkpf-gjahr
        AND koart = 'D'.
      APPEND ls_bkpf TO lt_ebeln_with_client.
      EXIT.
    ENDLOOP.
  ENDLOOP.

  gt_bkpf = lt_ebeln_with_client.

  IF gt_bkpf IS INITIAL.
    MESSAGE 'Aucune pièce avec poste client trouvée' TYPE 'S' DISPLAY LIKE 'W'.
    RETURN.
  ENDIF.

  PERFORM get_reference_data.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_REFERENCE_DATA
*&---------------------------------------------------------------------*
FORM get_reference_data.
  DATA: lt_vbeln    TYPE STANDARD TABLE OF vbeln_vf,
        lt_ref_keys TYPE STANDARD TABLE OF ty_bkpf,
        ls_ref_key  TYPE ty_bkpf.

  " Collecter les références factures (VBRK)
  LOOP AT gt_bkpf INTO DATA(ls_bkpf) WHERE awtyp = 'VBRK'.
    APPEND CONV vbeln_vf( ls_bkpf-awkey(10) ) TO lt_vbeln.
  ENDLOOP.

  SORT lt_vbeln.
  DELETE ADJACENT DUPLICATES FROM lt_vbeln.

  IF lt_vbeln IS NOT INITIAL.
    SELECT vbeln, ernam
      FROM vbrk
      INTO TABLE @gt_vbrk
      FOR ALL ENTRIES IN @lt_vbeln
      WHERE vbeln = @lt_vbeln-table_line.
  ENDIF.

  " Collecter les références pièces FI (BKPF)
  LOOP AT gt_bkpf INTO ls_bkpf WHERE awtyp = 'BKPF'.
    ls_ref_key = VALUE #( bukrs = ls_bkpf-awkey+10(4)
                          belnr = ls_bkpf-awkey(10)
                          gjahr = ls_bkpf-awkey+14(4) ).
    APPEND ls_ref_key TO lt_ref_keys.
  ENDLOOP.

  SORT lt_ref_keys BY bukrs belnr gjahr.
  DELETE ADJACENT DUPLICATES FROM lt_ref_keys COMPARING bukrs belnr gjahr.

  IF lt_ref_keys IS NOT INITIAL.
    SELECT bukrs, belnr, gjahr, bldat, awtyp, awkey, waers, usnam
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
  CLEAR gt_output.

  LOOP AT gt_bkpf INTO DATA(ls_bkpf).
    LOOP AT gt_bseg INTO DATA(ls_bseg)
      WHERE bukrs = ls_bkpf-bukrs
        AND belnr = ls_bkpf-belnr
        AND gjahr = ls_bkpf-gjahr.

      " Construction directe avec VALUE #
      DATA(ls_output) = VALUE ty_output(
        bukrs = ls_bkpf-bukrs
        belnr = ls_bkpf-belnr
        gjahr = ls_bkpf-gjahr
        bldat = ls_bkpf-bldat
        awtyp = ls_bkpf-awtyp
        waers = ls_bkpf-waers
        buzei = ls_bseg-buzei
        bschl = ls_bseg-bschl
        koart = ls_bseg-koart
        wrbtr = ls_bseg-wrbtr ).

      " Traitement référence selon AWTYP
      CASE ls_bkpf-awtyp.
        WHEN 'BKPF'.
          ls_output-ref_belnr = ls_bkpf-awkey(10).
          ls_output-ref_bukrs = ls_bkpf-awkey+10(4).
          ls_output-ref_gjahr = ls_bkpf-awkey+14(4).
          " Récupérer créateur avec OPTIONAL
          ls_output-ref_ernam = VALUE #( gt_bkpf_ref[ bukrs = ls_output-ref_bukrs
                                                       belnr = ls_output-ref_belnr
                                                       gjahr = ls_output-ref_gjahr ]-usnam OPTIONAL ).
        WHEN 'VBRK'.
          ls_output-ref_vbeln = ls_bkpf-awkey(10).
          " Récupérer créateur avec OPTIONAL
          ls_output-ref_ernam = VALUE #( gt_vbrk[ vbeln = ls_output-ref_vbeln ]-ernam OPTIONAL ).
      ENDCASE.

      " Couleurs
      ls_output-color = VALUE lvc_t_scol(
        ( fname = 'BUKRS' color = VALUE #( col = 1 int = 1 ) )
        ( fname = 'BELNR' color = VALUE #( col = 1 int = 1 ) )
        ( fname = 'GJAHR' color = VALUE #( col = 1 int = 1 ) )
        ( fname = 'BLDAT' color = VALUE #( col = 1 int = 1 ) )
        ( fname = 'AWTYP' color = VALUE #( col = 1 int = 1 ) )
        ( fname = 'REF_BUKRS' color = VALUE #( col = 7 int = 1 ) )
        ( fname = 'REF_BELNR' color = VALUE #( col = 7 int = 1 ) )
        ( fname = 'REF_GJAHR' color = VALUE #( col = 7 int = 1 ) )
        ( fname = 'REF_VBELN' color = VALUE #( col = 7 int = 1 ) )
        ( fname = 'REF_ERNAM' color = VALUE #( col = 7 int = 1 ) )
        ( fname = 'BUZEI' color = VALUE #( col = 7 int = 1 ) )
        ( fname = 'BSCHL' color = VALUE #( col = 7 int = 1 ) )
        ( fname = 'KOART' color = VALUE #( col = 7 int = 1 ) )
        ( fname = 'WAERS' color = VALUE #( col = 7 int = 1 ) )
        ( fname = 'WRBTR' color = VALUE #( col = 7 int = 1 ) ) ).

      APPEND ls_output TO gt_output.
    ENDLOOP.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV
*&---------------------------------------------------------------------*
FORM display_alv.
  IF gt_output IS INITIAL.
    MESSAGE 'Aucune donnée à afficher' TYPE 'S' DISPLAY LIKE 'W'.
    RETURN.
  ENDIF.

  TRY.
      cl_salv_table=>factory(
        IMPORTING r_salv_table = go_alv
        CHANGING  t_table      = gt_output ).

      go_alv->get_functions( )->set_all( abap_true ).

      DATA(lo_columns) = go_alv->get_columns( ).
      lo_columns->set_optimize( abap_true ).
      lo_columns->set_color_column( 'COLOR' ).

      PERFORM set_column_texts USING lo_columns.

      go_alv->display( ).

    CATCH cx_salv_msg cx_salv_not_found cx_salv_data_error.
      MESSAGE 'Erreur affichage ALV' TYPE 'E'.
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form SET_COLUMN_TEXTS
*&---------------------------------------------------------------------*
FORM set_column_texts USING io_columns TYPE REF TO cl_salv_columns_table.
  TRY.
      io_columns->get_column( 'BUKRS' )->set_short_text( 'Co.' ).
      io_columns->get_column( 'BELNR' )->set_short_text( 'Document' ).
      io_columns->get_column( 'GJAHR' )->set_short_text( 'Fis.' ).
      io_columns->get_column( 'BLDAT' )->set_short_text( 'Doc. Date' ).
      io_columns->get_column( 'AWTYP' )->set_short_text( 'Ref.' ).
      io_columns->get_column( 'REF_BUKRS' )->set_short_text( 'CoC' ).
      io_columns->get_column( 'REF_BELNR' )->set_short_text( 'Document' ).
      io_columns->get_column( 'REF_GJAHR' )->set_short_text( 'Fis.' ).
      io_columns->get_column( 'REF_VBELN' )->set_short_text( 'Billing' ).
      io_columns->get_column( 'REF_ERNAM' )->set_short_text( 'User' ).
      io_columns->get_column( 'BUZEI' )->set_short_text( 'It.' ).
      io_columns->get_column( 'BSCHL' )->set_short_text( 'P.' ).
      io_columns->get_column( 'KOART' )->set_short_text( 'A' ).
      io_columns->get_column( 'WAERS' )->set_short_text( 'Curr.' ).
      io_columns->get_column( 'WRBTR' )->set_short_text( 'Amount' ).
      io_columns->get_column( 'COLOR' )->set_visible( abap_false ).
    CATCH cx_salv_not_found.
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form EXPORT_TO_CSV
*&---------------------------------------------------------------------*
FORM export_to_csv.
  IF p_file IS INITIAL.
    MESSAGE 'Veuillez spécifier un fichier' TYPE 'I'.
    RETURN.
  ENDIF.

  CHECK gt_output IS NOT INITIAL.

  DATA(lt_csv) = VALUE string_table(
    ( |Company Code;Document Number;Fiscal Year;Document Date;Ref.;| &&
      |CoC;Document;Fis.;Billing Doc.;User name;It.;P.;A;Curr.;Amount| ) ).

  LOOP AT gt_output INTO DATA(ls).
    APPEND |{ ls-bukrs };{ ls-belnr };{ ls-gjahr };{ ls-bldat };{ ls-awtyp };| &&
           |{ ls-ref_bukrs };{ ls-ref_belnr };{ ls-ref_gjahr };{ ls-ref_vbeln };| &&
           |{ ls-ref_ernam };{ ls-buzei };{ ls-bschl };{ ls-koart };{ ls-waers };| &&
           |{ ls-wrbtr }| TO lt_csv.
  ENDLOOP.

  cl_gui_frontend_services=>gui_download(
    EXPORTING filename = p_file filetype = 'ASC'
    CHANGING  data_tab = lt_csv
    EXCEPTIONS OTHERS   = 1 ).

  IF sy-subrc = 0.
    MESSAGE 'Fichier exporté avec succès' TYPE 'S'.
  ELSE.
    MESSAGE 'Erreur export' TYPE 'E'.
  ENDIF.
ENDFORM.