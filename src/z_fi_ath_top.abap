*&---------------------------------------------------------------------*
*& Include Z_FI_ATH_TOP - Déclarations globales
*&---------------------------------------------------------------------*
TYPE-POOLS: icon.
" Types pour l'ALV
TYPES: BEGIN OF ty_output,
         bukrs      TYPE bukrs,
         belnr      TYPE belnr_d,
         gjahr      TYPE gjahr,
         bldat      TYPE bldat,
         awtyp      TYPE awtyp,
         ref_bukrs  TYPE bukrs,
         ref_belnr  TYPE belnr_d,
         ref_gjahr  TYPE gjahr,
         ref_vbeln  TYPE vbeln_vf,
         ref_ernam  TYPE ernam,
         buzei      TYPE buzei,
         bschl      TYPE bschl,
         koart      TYPE koart,
         waers      TYPE waers,
         wrbtr      TYPE wrbtr,
         color      TYPE lvc_t_scol,
       END OF ty_output.

TYPES: ty_t_output TYPE STANDARD TABLE OF ty_output WITH DEFAULT KEY.

TYPES: BEGIN OF ty_bkpf,
         bukrs TYPE bukrs,
         belnr TYPE belnr_d,
         gjahr TYPE gjahr,
         bldat TYPE bldat,
         awtyp TYPE awtyp,
         awkey TYPE awkey,
         waers TYPE waers,
         usnam TYPE usnam,
       END OF ty_bkpf.

TYPES: BEGIN OF ty_bseg,
         bukrs TYPE bukrs,
         belnr TYPE belnr_d,
         gjahr TYPE gjahr,
         buzei TYPE buzei,
         bschl TYPE bschl,
         koart TYPE koart,
         wrbtr TYPE wrbtr,
       END OF ty_bseg.

TYPES: BEGIN OF ty_vbrk,
         vbeln TYPE vbeln_vf,
         ernam TYPE ernam,
       END OF ty_vbrk.

DATA: gt_bkpf     TYPE STANDARD TABLE OF ty_bkpf,
      gt_bseg     TYPE STANDARD TABLE OF ty_bseg,
      gt_vbrk     TYPE STANDARD TABLE OF ty_vbrk,
      gt_bkpf_ref TYPE STANDARD TABLE OF ty_bkpf,
      gt_output   TYPE ty_t_output.

DATA: gv_gjahr TYPE gjahr.

DATA: go_alv TYPE REF TO cl_salv_table.

" ALV Grid
DATA: go_container TYPE REF TO cl_gui_custom_container,
      go_grid      TYPE REF TO cl_gui_alv_grid.

DATA: gv_ok_code TYPE sy-ucomm.