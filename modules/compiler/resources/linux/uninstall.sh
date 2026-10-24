# Copyright (c) 2026-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later

[[ $# == 0 ]] || fail 'The uninstaller takes no arguments.'
[[ ! -L ${BASH_SOURCE[0]} && ! -L $(dirname -- "${BASH_SOURCE[0]}") ]] || fail 'The uninstaller cannot be accessed through a symbolic link.'
self=$(readlink -f -- "${BASH_SOURCE[0]}") || fail 'Cannot locate the uninstaller.'
metadata=$(dirname -- "$self")
[[ ${metadata##*/} == .nelson-install && ! -L $metadata ]] || fail 'Invalid uninstaller location.'
root=$(dirname -- "$metadata")
load_manifest "$metadata/manifest.tsv"
preserved=0
for index in "${!manifest_paths[@]}"; do
    [[ ${manifest_kinds[$index]} != D ]] || continue
    target="$root/${manifest_paths[$index]}"
    if matches_entry "$root" "$index"; then
        rm -- "$target" || fail "Cannot remove ${manifest_paths[$index]}."
    elif [[ -e $target || -L $target ]] || ! safe_parents "$root" "${manifest_paths[$index]}"; then
        printf 'Preserved modified or linked path: %s\n' "${manifest_paths[$index]}"
        (( ++preserved ))
    fi
done
for (( index=${#manifest_paths[@]}-1; index>=0; --index )); do
    [[ ${manifest_kinds[$index]} == D ]] || continue
    target="$root/${manifest_paths[$index]}"
    if safe_parents "$root" "${manifest_paths[$index]}" && [[ -d $target && ! -L $target ]]; then
        rmdir -- "$target" 2>/dev/null || true
    fi
done
if (( preserved == 0 )); then
    [[ $(hash_file "$metadata/manifest.tsv") == "$manifest_hash" ]] || fail 'Installation manifest changed during removal.'
    rm -- "$metadata/manifest.tsv" "$self" || fail 'Cannot remove installation metadata.'
    rmdir -- "$metadata" "$root" 2>/dev/null || true
fi
printf 'Removed unchanged application files. Preserved modified files: %d.\n' "$preserved"
