*&---------------------------------------------------------------------*
*& Include Z_FI_ATH_TOP - Déclarations globales
*&---------------------------------------------------------------------*

TABLES: bkpf.

" Types pour l'ALV
TYPES: BEGIN OF ty_output,
         bukrs      TYPE bukrs,        " Company Code
         belnr      TYPE belnr_d,      " Document Number
         gjahr      TYPE gjahr,        " Fiscal Year
         bldat      TYPE bldat,        " Document Date
         awtyp      TYPE awtyp,        " Ref. Transaction
         ref_bukrs  TYPE bukrs,        " Reference Company Code
         ref_belnr  TYPE belnr_d,      " Reference Document Number
         ref_gjahr  TYPE gjahr,        " Reference Fiscal Year
         ref_vbeln  TYPE vbeln_vf,     " Reference Facture
         ref_ernam  TYPE ernam,        " Créateur pièce réf
         buzei      TYPE buzei,        " Line item
         bschl      TYPE bschl,        " Posting key
         koart      TYPE koart,        " Account Type
         waers      TYPE waers,        " Currency
         wrbtr      TYPE wrbtr,        " Amount
         color      TYPE lvc_t_scol,   " Couleurs ALV
       END OF ty_output.

TYPES: ty_t_output TYPE STANDARD TABLE OF ty_output WITH DEFAULT KEY.

" Tables internes
DATA: gt_bkpf   TYPE STANDARD TABLE OF bkpf,
      gt_bseg   TYPE STANDARD TABLE OF bseg,
      gt_vbrk   TYPE STANDARD TABLE OF vbrk,
      gt_output TYPE ty_t_output.

" Variables globales
DATA: gv_gjahr TYPE gjahr.