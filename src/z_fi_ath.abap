*&---------------------------------------------------------------------*
*& Report Z_FI_ATH
*&---------------------------------------------------------------------*
*& Description : Programme FI - Pièces comptables avec références
*& Auteur      : ATH
*& Date        : 2025
*&---------------------------------------------------------------------*
REPORT z_fi_ath.

INCLUDE z_fi_ath_top.  " Déclarations globales
INCLUDE z_fi_ath_scr.  " Écran de sélection
INCLUDE z_fi_ath_f01.  " Sous-routines

INITIALIZATION.
  PERFORM init_default_values.

START-OF-SELECTION.
  PERFORM get_data.
  PERFORM process_data.
  PERFORM display_alv.