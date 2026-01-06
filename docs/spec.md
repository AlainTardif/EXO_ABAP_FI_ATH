# Spécifications EXO ABAP FI

## Tables utilisées
- BKPF : Entêtes pièces FI
- BSEG : Postes pièces FI
- VBRK : Entêtes factures SD

## Règles métier
1. Sélectionner BKPF où AWTYP = 'VBRK' ou 'BKPF'
2. Garder uniquement les pièces avec poste client (KOART = 'D')
3. Afficher tous les postes des pièces retenues
4. Décoder AWKEY selon AWTYP

## Couleurs ALV
- Bleu : Colonnes référence FI
- Rouge : Colonne référence facture