#!/bin/sh
# Copyright (c) 2026-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later
set -eu
export PATH=/usr/bin:/bin:/usr/sbin:/sbin LC_ALL=C
metadata=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd -P)
root=$(dirname -- "$metadata")
manifest=$metadata/manifest.tsv
identity=$metadata/identity
fail() { printf 'Nelson uninstaller: %s\n' "$*" >&2; exit 1; }
hash_file() { /usr/bin/shasum -a 256 "$1" | /usr/bin/awk '{print $1}'; }
[ -f "$manifest" ] && [ ! -L "$manifest" ] || fail 'Missing installation manifest.'
[ -f "$identity" ] && [ ! -L "$identity" ] || fail 'Missing installation identity.'
[ "$(/usr/bin/sed -n '1p' "$identity")" = 'NELSON_APPLICATION_INSTALL_1' ] || fail 'Unsupported installation identity.'
[ "$(/usr/bin/sed -n '2p' "$identity")" = "$(hash_file "$manifest")" ] || fail 'Installation manifest changed.'
[ "$(/usr/bin/sed -n '3p' "$identity")" = "$(hash_file "$0")" ] || fail 'Application uninstaller changed.'
preserved=0
while IFS="$(printf '\t')" read -r kind digest mode relative; do
    [ "$kind" = F ] || continue
    target=$root/$relative
    if [ -f "$target" ] && [ ! -L "$target" ] &&
       [ "$(hash_file "$target")" = "$digest" ] &&
       [ "$(/usr/bin/stat -f '%Lp' "$target")" = "$mode" ]; then
        /bin/rm -- "$target"
    elif [ -e "$target" ] || [ -L "$target" ]; then
        printf 'Preserved modified path: %s\n' "$relative"
        preserved=$((preserved + 1))
    fi
done < "$manifest"
/usr/bin/awk -F '\t' '$1 == "D" { print length($4) "\t" $4 }' "$manifest" |
    /usr/bin/sort -rn | while IFS="$(printf '\t')" read -r length relative; do
        /bin/rmdir -- "$root/$relative" 2>/dev/null || true
    done
if [ "$preserved" -eq 0 ]; then
    /bin/rm -- "$manifest" "$identity" "$0"
    /bin/rmdir -- "$metadata" "$root" 2>/dev/null || true
fi
printf 'Removed unchanged application files. Preserved modified files: %s.\n' "$preserved"
