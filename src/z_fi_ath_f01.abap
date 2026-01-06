*&---------------------------------------------------------------------*
*& Include Z_FI_ATH_F01 - Sous-routines (syntaxe moderne)
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Include Z_FI_ATH_F01 - Sous-routines
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Classe locale pour les événements ALV Grid
*&---------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,
      on_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_toolbar.
    DATA: ls_button TYPE stb_button.

    " Ajouter un séparateur
    CLEAR ls_button.
    ls_button-butn_type = 3. " Séparateur
    APPEND ls_button TO e_object->mt_toolbar.

    " Ajouter le bouton Export
    CLEAR ls_button.
    ls_button-function  = 'EXPORT'.
    ls_button-icon      = icon_export.
    ls_button-text      = 'Export'.
    ls_button-quickinfo = 'Exporter en CSV'.
    APPEND ls_button TO e_object->mt_toolbar.
  ENDMETHOD.

  METHOD on_user_command.
    CASE e_ucomm.
      WHEN 'EXPORT'.
        PERFORM export_to_csv.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
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
    APPEND ls_bkpf-awkey(10) TO lt_vbeln.
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

  CALL SCREEN 100.
ENDFORM.

*&---------------------------------------------------------------------*
*& Module PBO_0100
*&---------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.
  SET PF-STATUS 'STATUS_100'.
  SET TITLEBAR 'TITLE_100'.

  IF go_container IS INITIAL.
    CREATE OBJECT go_container
      EXPORTING
        container_name = 'CC_ALV'.

    CREATE OBJECT go_grid
      EXPORTING
        i_parent = go_container.

    PERFORM display_grid.
  ENDIF.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module PAI_0100
*&---------------------------------------------------------------------*
MODULE pai_0100 INPUT.
  gv_ok_code = sy-ucomm.
  CLEAR sy-ucomm.

  CASE gv_ok_code.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXPORT'.
      PERFORM export_to_csv.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form DISPLAY_GRID
*&---------------------------------------------------------------------*
FORM display_grid.
  DATA: lt_fieldcat TYPE lvc_t_fcat,
        ls_fieldcat TYPE lvc_s_fcat,
        ls_layout   TYPE lvc_s_layo,
        lt_exclude  TYPE ui_functions.

  " Layout avec couleurs
  ls_layout-zebra      = abap_true.
  ls_layout-cwidth_opt = abap_true.
  ls_layout-ctab_fname = 'COLOR'.

  " Construire le fieldcatalog manuellement
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'BUKRS'.
  ls_fieldcat-scrtext_s = 'Co.'.
  ls_fieldcat-coltext   = 'Company Code'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'BELNR'.
  ls_fieldcat-scrtext_s = 'Document'.
  ls_fieldcat-coltext   = 'Document Number'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'GJAHR'.
  ls_fieldcat-scrtext_s = 'Fis.'.
  ls_fieldcat-coltext   = 'Fiscal Year'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'BLDAT'.
  ls_fieldcat-scrtext_s = 'Doc. Date'.
  ls_fieldcat-coltext   = 'Document Date'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'AWTYP'.
  ls_fieldcat-scrtext_s = 'Ref.'.
  ls_fieldcat-coltext   = 'Ref. Transactn'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'REF_BUKRS'.
  ls_fieldcat-scrtext_s = 'CoC'.
  ls_fieldcat-coltext   = 'Ref. Company'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'REF_BELNR'.
  ls_fieldcat-scrtext_s = 'Document'.
  ls_fieldcat-coltext   = 'Ref. Document'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'REF_GJAHR'.
  ls_fieldcat-scrtext_s = 'Fis.'.
  ls_fieldcat-coltext   = 'Ref. Fiscal Year'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'REF_VBELN'.
  ls_fieldcat-scrtext_s = 'Billing'.
  ls_fieldcat-coltext   = 'Billing Doc.'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'REF_ERNAM'.
  ls_fieldcat-scrtext_s = 'User'.
  ls_fieldcat-coltext   = 'User name'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'BUZEI'.
  ls_fieldcat-scrtext_s = 'It.'.
  ls_fieldcat-coltext   = 'Line item'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'BSCHL'.
  ls_fieldcat-scrtext_s = 'P.'.
  ls_fieldcat-coltext   = 'Posting key'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'KOART'.
  ls_fieldcat-scrtext_s = 'A'.
  ls_fieldcat-coltext   = 'Account Type'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'WAERS'.
  ls_fieldcat-scrtext_s = 'Curr.'.
  ls_fieldcat-coltext   = 'Currency'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'WRBTR'.
  ls_fieldcat-scrtext_s = 'Amount'.
  ls_fieldcat-coltext   = 'Amount'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'COLOR'.
  ls_fieldcat-tech      = abap_true.
  APPEND ls_fieldcat TO lt_fieldcat.

  " Enregistrer la classe d'événements pour la toolbar
  SET HANDLER lcl_event_handler=>on_toolbar FOR go_grid.
  SET HANDLER lcl_event_handler=>on_user_command FOR go_grid.

  " Afficher la grille
  go_grid->set_table_for_first_display(
    EXPORTING
      is_layout       = ls_layout
    CHANGING
      it_outtab       = gt_output
      it_fieldcatalog = lt_fieldcat ).
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