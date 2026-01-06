*&---------------------------------------------------------------------*
*& Include Z_FI_ATH_F01 - Sous-routines (syntaxe moderne)
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Classe locale pour les événements ALV Grid
*& Je définis une classe pour gérer les événements de la toolbar ALV
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

    " J'ajoute un séparateur avant mon bouton personnalisé
    CLEAR ls_button.
    ls_button-butn_type = 3.
    APPEND ls_button TO e_object->mt_toolbar.

    " J'ajoute le bouton Export dans la toolbar
    CLEAR ls_button.
    ls_button-function  = 'EXPORT'.
    ls_button-icon      = icon_export.
    ls_button-text      = 'Export'.
    ls_button-quickinfo = 'Exporter en CSV'.
    APPEND ls_button TO e_object->mt_toolbar.
  ENDMETHOD.

  METHOD on_user_command.
    " Je gère le clic sur le bouton Export
    CASE e_ucomm.
      WHEN 'EXPORT'.
        PERFORM export_to_csv.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Form INIT_DEFAULT_VALUES
*& J'initialise les valeurs par défaut de l'écran de sélection
*&---------------------------------------------------------------------*
FORM init_default_values.
  " Je récupère l'année en cours pour la date par défaut
  gv_gjahr = sy-datum(4).
ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_DATA
*& Je récupère les données des tables BKPF et BSEG
*&---------------------------------------------------------------------*
FORM get_data.
  DATA: lv_gjahr             TYPE gjahr,
        lt_ebeln_with_client TYPE STANDARD TABLE OF ty_bkpf.

  " J'extrais l'exercice de la date de sélection
  lv_gjahr = p_bldat(4).

  " 1. Je sélectionne les entêtes BKPF avec les critères de sélection
  "    Je ne garde que les pièces référençant une facture (VBRK) ou une pièce FI (BKPF)
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

  " 2. Je sélectionne les postes BSEG des pièces récupérées
  SELECT bukrs, belnr, gjahr, buzei, bschl, koart, wrbtr
    FROM bseg
    INTO TABLE @gt_bseg
    FOR ALL ENTRIES IN @gt_bkpf
    WHERE bukrs = @gt_bkpf-bukrs
      AND belnr = @gt_bkpf-belnr
      AND gjahr = @gt_bkpf-gjahr.

  " 3. Je filtre les pièces ayant au moins un poste client (KOART = 'D')
  "    J'utilise TRANSPORTING NO FIELDS car je veux juste vérifier l'existence
  LOOP AT gt_bkpf INTO DATA(ls_bkpf).
    LOOP AT gt_bseg TRANSPORTING NO FIELDS
      WHERE bukrs = ls_bkpf-bukrs
        AND belnr = ls_bkpf-belnr
        AND gjahr = ls_bkpf-gjahr
        AND koart = 'D'.
      " Je garde cette pièce car elle a un poste client
      APPEND ls_bkpf TO lt_ebeln_with_client.
      EXIT.
    ENDLOOP.
  ENDLOOP.

  " Je remplace la table par les pièces filtrées
  gt_bkpf = lt_ebeln_with_client.

  IF gt_bkpf IS INITIAL.
    MESSAGE 'Aucune pièce avec poste client trouvée' TYPE 'S' DISPLAY LIKE 'W'.
    RETURN.
  ENDIF.

  " 4. Je récupère les données des pièces de référence
  PERFORM get_reference_data.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_REFERENCE_DATA
*& Je récupère les données des pièces de référence (VBRK ou BKPF)
*&---------------------------------------------------------------------*
FORM get_reference_data.
  DATA: lt_vbeln    TYPE STANDARD TABLE OF vbeln_vf,
        lt_ref_keys TYPE STANDARD TABLE OF ty_bkpf,
        ls_ref_key  TYPE ty_bkpf.

  " Je collecte les numéros de factures (VBRK) à récupérer
  LOOP AT gt_bkpf INTO DATA(ls_bkpf) WHERE awtyp = 'VBRK'.
    APPEND ls_bkpf-awkey(10) TO lt_vbeln.
  ENDLOOP.

  " J'élimine les doublons pour optimiser le SELECT
  SORT lt_vbeln.
  DELETE ADJACENT DUPLICATES FROM lt_vbeln.

  " Je récupère les créateurs des factures depuis VBRK
  IF lt_vbeln IS NOT INITIAL.
    SELECT vbeln, ernam
      FROM vbrk
      INTO TABLE @gt_vbrk
      FOR ALL ENTRIES IN @lt_vbeln
      WHERE vbeln = @lt_vbeln-table_line.
  ENDIF.

  " Je collecte les clés des pièces FI de référence
  " AWKEY = BELNR(10) + BUKRS(4) + GJAHR(4) pour AWTYP = 'BKPF'
  LOOP AT gt_bkpf INTO ls_bkpf WHERE awtyp = 'BKPF'.
    " Je découpe AWKEY pour extraire les composants de la clé
    ls_ref_key = VALUE #( bukrs = ls_bkpf-awkey+10(4)
                          belnr = ls_bkpf-awkey(10)
                          gjahr = ls_bkpf-awkey+14(4) ).
    APPEND ls_ref_key TO lt_ref_keys.
  ENDLOOP.

  " J'élimine les doublons
  SORT lt_ref_keys BY bukrs belnr gjahr.
  DELETE ADJACENT DUPLICATES FROM lt_ref_keys COMPARING bukrs belnr gjahr.

  " Je récupère les données des pièces FI de référence
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
*& Je construis la table de sortie pour l'ALV
*&---------------------------------------------------------------------*
FORM process_data.
  CLEAR gt_output.

  " Je parcours les entêtes BKPF
  LOOP AT gt_bkpf INTO DATA(ls_bkpf).
    " Je crée une ligne de sortie pour chaque poste de la pièce
    LOOP AT gt_bseg INTO DATA(ls_bseg)
      WHERE bukrs = ls_bkpf-bukrs
        AND belnr = ls_bkpf-belnr
        AND gjahr = ls_bkpf-gjahr.

      " Je construis la ligne avec VALUE # (syntaxe moderne)
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

      " Je traite la référence selon le type AWTYP
      CASE ls_bkpf-awtyp.
        WHEN 'BKPF'.
          " Référence FI : je découpe AWKEY pour extraire les infos
          ls_output-ref_belnr = ls_bkpf-awkey(10).
          ls_output-ref_bukrs = ls_bkpf-awkey+10(4).
          ls_output-ref_gjahr = ls_bkpf-awkey+14(4).
          " Je récupère le créateur avec OPTIONAL pour éviter le dump si non trouvé
          ls_output-ref_ernam = VALUE #( gt_bkpf_ref[ bukrs = ls_output-ref_bukrs
                                                       belnr = ls_output-ref_belnr
                                                       gjahr = ls_output-ref_gjahr ]-usnam OPTIONAL ).
        WHEN 'VBRK'.
          " Référence facture : je récupère le numéro et le créateur
          ls_output-ref_vbeln = ls_bkpf-awkey(10).
          ls_output-ref_ernam = VALUE #( gt_vbrk[ vbeln = ls_output-ref_vbeln ]-ernam OPTIONAL ).
      ENDCASE.

      " Je définis les couleurs des colonnes
      " Bleu (col = 1) pour l'entête, Orange (col = 7) pour les références et postes
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
*& J'affiche l'ALV Grid via l'écran 100
*&---------------------------------------------------------------------*
FORM display_alv.
  IF gt_output IS INITIAL.
    MESSAGE 'Aucune donnée à afficher' TYPE 'S' DISPLAY LIKE 'W'.
    RETURN.
  ENDIF.

  " J'appelle l'écran Dynpro 100 qui contient le container ALV
  CALL SCREEN 100.
ENDFORM.

*&---------------------------------------------------------------------*
*& Module PBO_0100
*& Je prépare l'affichage de l'écran 100 (Process Before Output)
*&---------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.
  " Je définis le status et le titre de l'écran
  SET PF-STATUS 'STATUS_100'.
  SET TITLEBAR 'TITLE_100'.

  " Je crée le container et la grille ALV une seule fois
  IF go_container IS INITIAL.
    " Je crée le container lié au Custom Control 'CC_ALV' du Dynpro
    CREATE OBJECT go_container
      EXPORTING
        container_name = 'CC_ALV'.

    " Je crée la grille ALV dans le container
    CREATE OBJECT go_grid
      EXPORTING
        i_parent = go_container.

    " J'affiche les données dans la grille
    PERFORM display_grid.
  ENDIF.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module PAI_0100
*& Je gère les actions utilisateur sur l'écran 100 (Process After Input)
*&---------------------------------------------------------------------*
MODULE pai_0100 INPUT.
  " Je récupère le code fonction déclenché
  gv_ok_code = sy-ucomm.
  CLEAR sy-ucomm.

  " Je traite l'action utilisateur
  CASE gv_ok_code.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      " Je quitte l'écran et retourne à l'écran précédent
      LEAVE TO SCREEN 0.
    WHEN 'EXPORT'.
      " Je déclenche l'export CSV
      PERFORM export_to_csv.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form DISPLAY_GRID
*& Je configure et affiche la grille ALV avec le fieldcatalog
*&---------------------------------------------------------------------*
FORM display_grid.
  DATA: lt_fieldcat TYPE lvc_t_fcat,
        ls_fieldcat TYPE lvc_s_fcat,
        ls_layout   TYPE lvc_s_layo.

  " Je configure le layout de l'ALV
  ls_layout-zebra      = abap_true.  " Alternance de couleurs lignes
  ls_layout-cwidth_opt = abap_true.  " Largeur optimale des colonnes
  ls_layout-ctab_fname = 'COLOR'.    " Champ contenant les couleurs

  " Je construis le fieldcatalog manuellement car ty_output n'est pas DDIC
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

  " Je masque la colonne COLOR (technique)
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'COLOR'.
  ls_fieldcat-tech      = abap_true.
  APPEND ls_fieldcat TO lt_fieldcat.

  " J'enregistre les handlers pour les événements toolbar et user_command
  SET HANDLER lcl_event_handler=>on_toolbar FOR go_grid.
  SET HANDLER lcl_event_handler=>on_user_command FOR go_grid.

  " J'affiche la grille avec les données et le fieldcatalog
  go_grid->set_table_for_first_display(
    EXPORTING
      is_layout       = ls_layout
    CHANGING
      it_outtab       = gt_output
      it_fieldcatalog = lt_fieldcat ).
ENDFORM.

*&---------------------------------------------------------------------*
*& Form EXPORT_TO_CSV
*& J'exporte les données de l'ALV vers un fichier CSV
*&---------------------------------------------------------------------*
FORM export_to_csv.
  " Je vérifie qu'un fichier a été spécifié
  IF p_file IS INITIAL.
    MESSAGE 'Veuillez spécifier un fichier' TYPE 'I'.
    RETURN.
  ENDIF.

  CHECK gt_output IS NOT INITIAL.

  " Je construis le fichier CSV avec l'en-tête
  DATA(lt_csv) = VALUE string_table(
    ( |Company Code;Document Number;Fiscal Year;Document Date;Ref.;| &&
      |CoC;Document;Fis.;Billing Doc.;User name;It.;P.;A;Curr.;Amount| ) ).

  " J'ajoute chaque ligne de données avec les string templates
  LOOP AT gt_output INTO DATA(ls).
    APPEND |{ ls-bukrs };{ ls-belnr };{ ls-gjahr };{ ls-bldat };{ ls-awtyp };| &&
           |{ ls-ref_bukrs };{ ls-ref_belnr };{ ls-ref_gjahr };{ ls-ref_vbeln };| &&
           |{ ls-ref_ernam };{ ls-buzei };{ ls-bschl };{ ls-koart };{ ls-waers };| &&
           |{ ls-wrbtr }| TO lt_csv.
  ENDLOOP.

  " Je télécharge le fichier sur le poste client
  cl_gui_frontend_services=>gui_download(
    EXPORTING filename = p_file filetype = 'ASC'
    CHANGING  data_tab = lt_csv
    EXCEPTIONS OTHERS   = 1 ).

  " J'affiche le message de résultat
  IF sy-subrc = 0.
    MESSAGE 'Fichier exporté avec succès' TYPE 'S'.
  ELSE.
    MESSAGE 'Erreur export' TYPE 'E'.
  ENDIF.
ENDFORM.