#!/bin/bash

# Vérifie qu'un nom de dossier est fourni
if [ -z "$1" ]; then
    echo "Usage: $0 <nom_dossier>"
    exit 1
fi

# retire nom de l'extension (le .txt)
DIR="${1%.*}"

# supprimer le dossier s'il existe déjà
if [ -d "$DIR" ]; then
    rm -rf "$DIR"
fi

# Crée le dossier s'il n'existe pas
mkdir -p "$DIR"

# Évite les erreurs si aucun fichier ne correspond
shopt -s nullglob

for file in ford_iter*.gv.txt; do
    base="${file%.gv.txt}"
    svg="$DIR/$base.svg"

    dot -Tsvg "$file" > "$svg"
done

dot -Tsvg result.gv.txt > "$DIR/result.svg"

# Supprime tous les fichiers .gv.txt
rm -f ford_iter*.gv.txt
rm -f result.gv.txt

# ouvre le dossier cible
caja $DIR
