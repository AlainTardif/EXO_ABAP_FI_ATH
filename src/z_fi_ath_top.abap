*&---------------------------------------------------------------------*
*& Include Z_FI_ATH_TOP - Déclarations globales
*&---------------------------------------------------------------------*

" Types pour l'ALV
TYPES: BEGIN OF ty_output,
         bukrs      TYPE bukrs,        " Company Code
         belnr      TYPE belnr_d,      " Document Number
         gjahr      TYPE gjahr,        " Fiscal Year
         bldat      TYPE bldat,        " Document Date
         awtyp      TYPE awtyp,        " Ref. Transactn
         ref_bukrs  TYPE bukrs,        " Reference company code (CoC)
         ref_belnr  TYPE belnr_d,      " Reference Document Number
         ref_gjahr  TYPE gjahr,        " Reference Fiscal Year
         ref_vbeln  TYPE vbeln_vf,     " Billing Doc. (Reference Facture)
         ref_ernam  TYPE ernam,        " User name (Créateur pièce réf)
         buzei      TYPE buzei,        " Line item (It.)
         bschl      TYPE bschl,        " Posting key (P.)
         koart      TYPE koart,        " Account Type (A)
         waers      TYPE waers,        " Currency (Curr.)
         wrbtr      TYPE wrbtr,        " Amount
         color      TYPE lvc_t_scol,   " Couleurs ALV
       END OF ty_output.

TYPES: ty_t_output TYPE STANDARD TABLE OF ty_output WITH DEFAULT KEY.

" Structure pour BKPF
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

" Structure pour BSEG
TYPES: BEGIN OF ty_bseg,
         bukrs TYPE bukrs,
         belnr TYPE belnr_d,
         gjahr TYPE gjahr,
         buzei TYPE buzei,
         bschl TYPE bschl,
         koart TYPE koart,
         wrbtr TYPE wrbtr,
       END OF ty_bseg.

" Structure pour VBRK
TYPES: BEGIN OF ty_vbrk,
         vbeln TYPE vbeln_vf,
         ernam TYPE ernam,
       END OF ty_vbrk.

" Tables internes
DATA: gt_bkpf     TYPE STANDARD TABLE OF ty_bkpf,
      gt_bseg     TYPE STANDARD TABLE OF ty_bseg,
      gt_vbrk     TYPE STANDARD TABLE OF ty_vbrk,
      gt_bkpf_ref TYPE STANDARD TABLE OF ty_bkpf,
      gt_output   TYPE ty_t_output.

" Variables globales
DATA: gv_gjahr TYPE gjahr.

" ALV
DATA: go_alv TYPE REF TO cl_salv_table.

*&---------------------------------------------------------------------*
*& Classe locale pour gérer les événements ALV
*&---------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS: on_user_command FOR EVENT added_function OF cl_salv_events
      IMPORTING e_salv_function.
ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_user_command.
    IF e_salv_function = 'EXPORT'.
      PERFORM export_to_csv.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

DATA: go_event_handler TYPE REF TO lcl_event_handler.