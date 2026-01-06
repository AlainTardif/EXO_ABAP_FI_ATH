*&---------------------------------------------------------------------*
*& Include Z_FI_ATH_SCR - Écran de sélection
*& Je définis ici les paramètres de l'écran de sélection
*&---------------------------------------------------------------------*

" Je déclare des variables locales pour les SELECT-OPTIONS
" J'utilise des variables locales pour éviter le conflit avec les search helps DDIC
DATA: gv_bukrs TYPE bukrs,
      gv_belnr TYPE belnr_d.

" Je crée le bloc de sélection principal
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-001.
  " Société : obligatoire, valeur unique
  PARAMETERS:     p_bukrs TYPE c LENGTH 4 OBLIGATORY.
  " Numéro de pièce : sélection multiple optionnelle
  SELECT-OPTIONS: s_belnr FOR gv_belnr.
  " Date de début : par défaut 01.01.2020
  PARAMETERS:     p_bldat TYPE bldat DEFAULT '20200101'.
SELECTION-SCREEN END OF BLOCK b01.

" Je crée le bloc pour l'export CSV
SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-002.
  " Chemin du fichier CSV pour l'export
  PARAMETERS: p_file TYPE string LOWER CASE.
SELECTION-SCREEN END OF BLOCK b02.

" Je gère l'aide à la recherche pour le fichier (F4)
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  DATA: lv_filename TYPE string,
        lv_path     TYPE string,
        lv_fullpath TYPE string.

  " J'ouvre la boîte de dialogue pour choisir l'emplacement du fichier
  cl_gui_frontend_services=>file_save_dialog(
    EXPORTING
      default_extension = 'CSV'
      default_file_name = 'export_fi'
      file_filter       = 'Fichiers CSV (*.csv)|*.csv'
    CHANGING
      filename          = lv_filename
      path              = lv_path
      fullpath          = lv_fullpath
    EXCEPTIONS
      OTHERS            = 1 ).

  " Je récupère le chemin complet si l'utilisateur a validé
  IF sy-subrc = 0.
    p_file = lv_fullpath.
  ENDIF.