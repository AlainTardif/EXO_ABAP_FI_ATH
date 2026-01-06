*&---------------------------------------------------------------------*
*& Include Z_FI_ATH_TOP - Déclarations globales
*& Je déclare ici tous les types, tables et variables globales du programme
*&---------------------------------------------------------------------*

" Je charge le type-pool pour les icônes de la toolbar ALV
TYPE-POOLS: icon.

" Je définis le type de structure pour l'affichage ALV
TYPES: BEGIN OF ty_output,
         bukrs      TYPE bukrs,        " Société
         belnr      TYPE belnr_d,      " Numéro de pièce
         gjahr      TYPE gjahr,        " Exercice
         bldat      TYPE bldat,        " Date pièce
         awtyp      TYPE awtyp,        " Type référence (VBRK/BKPF)
         ref_bukrs  TYPE bukrs,        " Société référence
         ref_belnr  TYPE belnr_d,      " Numéro document référence
         ref_gjahr  TYPE gjahr,        " Exercice référence
         ref_vbeln  TYPE vbeln_vf,     " Numéro facture (si VBRK)
         ref_ernam  TYPE ernam,        " Créateur pièce référence
         buzei      TYPE buzei,        " Numéro de poste
         bschl      TYPE bschl,        " Clé comptable
         koart      TYPE koart,        " Type de compte
         waers      TYPE waers,        " Devise
         wrbtr      TYPE wrbtr,        " Montant
         color      TYPE lvc_t_scol,   " Couleurs des colonnes ALV
       END OF ty_output.

" Je définis le type table pour la sortie ALV
TYPES: ty_t_output TYPE STANDARD TABLE OF ty_output WITH DEFAULT KEY.

" Je définis la structure pour les entêtes BKPF
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

" Je définis la structure pour les postes BSEG
TYPES: BEGIN OF ty_bseg,
         bukrs TYPE bukrs,
         belnr TYPE belnr_d,
         gjahr TYPE gjahr,
         buzei TYPE buzei,
         bschl TYPE bschl,
         koart TYPE koart,
         wrbtr TYPE wrbtr,
       END OF ty_bseg.

" Je définis la structure pour les factures VBRK
TYPES: BEGIN OF ty_vbrk,
         vbeln TYPE vbeln_vf,
         ernam TYPE ernam,
       END OF ty_vbrk.

" Je déclare les tables internes pour stocker les données
DATA: gt_bkpf     TYPE STANDARD TABLE OF ty_bkpf,      " Entêtes pièces FI
      gt_bseg     TYPE STANDARD TABLE OF ty_bseg,      " Postes pièces FI
      gt_vbrk     TYPE STANDARD TABLE OF ty_vbrk,      " Factures SD
      gt_bkpf_ref TYPE STANDARD TABLE OF ty_bkpf,      " Pièces FI de référence
      gt_output   TYPE ty_t_output.                    " Table de sortie ALV

" Je déclare la variable pour l'exercice
DATA: gv_gjahr TYPE gjahr.

" Je déclare les objets pour l'ALV Grid
DATA: go_alv       TYPE REF TO cl_salv_table,           " ALV SALV (non utilisé)
      go_container TYPE REF TO cl_gui_custom_container, " Container pour l'ALV
      go_grid      TYPE REF TO cl_gui_alv_grid.         " Grille ALV

" Je déclare la variable pour gérer les actions utilisateur
DATA: gv_ok_code TYPE sy-ucomm.