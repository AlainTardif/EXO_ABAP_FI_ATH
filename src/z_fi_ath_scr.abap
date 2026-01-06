*&---------------------------------------------------------------------*
*& Include Z_FI_ATH_SCR - Écran de sélection
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-001.
  PARAMETERS:     p_bukrs TYPE bukrs OBLIGATORY.
  SELECT-OPTIONS: s_belnr FOR bkpf-belnr.
  PARAMETERS:     p_bldat TYPE bldat.
SELECTION-SCREEN END OF BLOCK b01.