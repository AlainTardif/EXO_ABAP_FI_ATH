*&---------------------------------------------------------------------*
*& Report Z_FI_ATH
*&---------------------------------------------------------------------*
REPORT z_fi_ath.

INCLUDE z_fi_ath_top.
INCLUDE z_fi_ath_scr.
INCLUDE z_fi_ath_f01.

INITIALIZATION.
  PERFORM init_default_values.

START-OF-SELECTION.
  PERFORM get_data.
  PERFORM process_data.
  PERFORM display_alv.