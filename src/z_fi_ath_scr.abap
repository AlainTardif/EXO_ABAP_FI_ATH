*&---------------------------------------------------------------------*
*& Include Z_FI_ATH_SCR - Écran de sélection
*&---------------------------------------------------------------------*

DATA: gv_bukrs TYPE bukrs,
      gv_belnr TYPE belnr_d.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-001.
  PARAMETERS:     p_bukrs TYPE c LENGTH 4 OBLIGATORY.
  SELECT-OPTIONS: s_belnr FOR gv_belnr.
  PARAMETERS:     p_bldat TYPE bldat DEFAULT '20180101'.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_file TYPE string LOWER CASE.
SELECTION-SCREEN END OF BLOCK b02.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  DATA: lv_filename TYPE string,
        lv_path     TYPE string,
        lv_fullpath TYPE string.

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

  IF sy-subrc = 0.
    p_file = lv_fullpath.
  ENDIF.