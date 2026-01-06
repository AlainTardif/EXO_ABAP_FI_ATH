*&---------------------------------------------------------------------*
*& Include Z_FI_ATH_F01 - Sous-routines
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form INIT_DEFAULT_VALUES
*&---------------------------------------------------------------------*
FORM init_default_values.
  " Date par défaut : 01.01.année en cours
  gv_gjahr = sy-datum(4).
  CONCATENATE gv_gjahr '0101' INTO p_bldat.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
FORM get_data.
  " 1. Sélection des entêtes BKPF
  SELECT * FROM bkpf
    INTO TABLE @gt_bkpf
    WHERE bukrs = @p_bukrs
      AND belnr IN @s_belnr
      AND bldat >= @p_bldat
      AND awtyp IN ('VBRK', 'BKPF').

  IF gt_bkpf IS INITIAL.
    MESSAGE 'Aucune pièce FI trouvée' TYPE 'S' DISPLAY LIKE 'W'.
    RETURN.
  ENDIF.

  " 2. Sélection des postes BSEG
  SELECT * FROM bseg
    INTO TABLE @gt_bseg
    FOR ALL ENTRIES IN @gt_bkpf
    WHERE bukrs = @gt_bkpf-bukrs
      AND belnr = @gt_bkpf-belnr
      AND gjahr = @gt_bkpf-gjahr.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form PROCESS_DATA
*&---------------------------------------------------------------------*
FORM process_data.
  DATA: ls_output TYPE ty_output,
        ls_color  TYPE lvc_s_scol,
        lv_has_client TYPE abap_bool.

  " Parcourir les entêtes
  LOOP AT gt_bkpf INTO DATA(ls_bkpf).
    " Vérifier si la pièce a un poste client
    lv_has_client = abap_false.
    LOOP AT gt_bseg INTO DATA(ls_bseg_check)
      WHERE bukrs = ls_bkpf-bukrs
        AND belnr = ls_bkpf-belnr
        AND gjahr = ls_bkpf-gjahr
        AND koart = 'D'.
      lv_has_client = abap_true.
      EXIT.
    ENDLOOP.

    CHECK lv_has_client = abap_true.

    " Créer une ligne par poste
    LOOP AT gt_bseg INTO DATA(ls_bseg)
      WHERE bukrs = ls_bkpf-bukrs
        AND belnr = ls_bkpf-belnr
        AND gjahr = ls_bkpf-gjahr.

      CLEAR: ls_output, ls_color.

      " Données entête
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
        " Couleur bleue pour colonnes référence FI
        ls_color-fname = 'REF_BELNR'.
        ls_color-color-col = 1. " Bleu
        APPEND ls_color TO ls_output-color.
      ELSEIF ls_bkpf-awtyp = 'VBRK'.
        " Référence facture
        ls_output-ref_vbeln = ls_bkpf-awkey(10).
        " Couleur rouge pour colonne facture
        ls_color-fname = 'REF_VBELN'.
        ls_color-color-col = 6. " Rouge
        APPEND ls_color TO ls_output-color.
      ENDIF.

      APPEND ls_output TO gt_output.
    ENDLOOP.
  ENDLOOP.

  " Récupérer les créateurs des pièces de référence
  PERFORM get_reference_creators.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_REFERENCE_CREATORS
*&---------------------------------------------------------------------*
FORM get_reference_creators.
  " À compléter : récupérer ERNAM depuis BKPF ou VBRK
ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV
*&---------------------------------------------------------------------*
FORM display_alv.
  DATA: lo_alv     TYPE REF TO cl_salv_table,
        lo_columns TYPE REF TO cl_salv_columns_table.

  IF gt_output IS INITIAL.
    MESSAGE 'Aucune donnée à afficher' TYPE 'S' DISPLAY LIKE 'W'.
    RETURN.
  ENDIF.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = lo_alv
        CHANGING
          t_table      = gt_output ).

      lo_columns = lo_alv->get_columns( ).
      lo_columns->set_optimize( abap_true ).
      lo_columns->set_color_column( 'COLOR' ).

      lo_alv->display( ).

    CATCH cx_salv_msg.
      MESSAGE 'Erreur affichage ALV' TYPE 'E'.
  ENDTRY.
ENDFORM.