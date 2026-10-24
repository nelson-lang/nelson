# Copyright (c) 2026-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later

fail() {
    printf 'Nelson installer: %s\n' "$*" >&2
    exit 1
}

hash_file() {
    local output
    output=$(sha256sum < "$1") || fail "Cannot hash $1."
    printf '%s' "${output%% *}"
}

hash_link() {
    local target output
    target=$(readlink -- "$1") || return 1
    safe_relative "$target" || return 1
    output=$(printf '%s' "$target" | sha256sum) || return 1
    printf '%s' "${output%% *}"
}

safe_relative() {
    [[ -n $1 && $1 != /* && $1 != */ && $1 != *//* && $1 != *\\* &&
        $1 != *[[:cntrl:]]* && /$1/ != */../* && /$1/ != */./* ]]
}

safe_parents() {
    local cursor=$1 part
    local -a parts
    IFS=/ read -r -a parts <<< "$2"
    [[ -d $cursor && ! -L $cursor ]] || return 1
    unset "parts[$((${#parts[@]} - 1))]"
    for part in "${parts[@]}"; do
        cursor+=/$part
        [[ ! -L $cursor && ( ! -e $cursor || -d $cursor ) ]] || return 1
    done
}

load_manifest() {
    local filename=$1 kind hash mode relative count=0
    local -A seen=()
    [[ -f $filename && ! -L $filename ]] || fail 'Missing installation manifest.'
    [[ $(hash_file "$filename") == "$manifest_hash" ]] || fail 'Installation manifest was changed.'
    manifest_kinds=(); manifest_hashes=(); manifest_modes=(); manifest_paths=()
    while IFS=$'\t' read -r kind hash mode relative; do
        (( ++count <= 200000 )) || fail 'Installation manifest is too large.'
        safe_relative "$relative" || fail 'Invalid installation path.'
        [[ $relative != .nelson-install && $relative != .nelson-install/* &&
            ! ${seen[$relative]+present} ]] || fail 'Conflicting installation paths.'
        seen[$relative]=1
        case $kind in
            F) [[ $hash =~ ^[0-9a-f]{64}$ && ( $mode == 644 || $mode == 755 ) ]] || fail 'Invalid file record.' ;;
            L) [[ $hash =~ ^[0-9a-f]{64}$ && $mode == - ]] || fail 'Invalid shortcut record.' ;;
            D) [[ $hash == - && $mode == 755 ]] || fail 'Invalid directory record.' ;;
            *) fail 'Invalid installation record.' ;;
        esac
        manifest_kinds+=("$kind"); manifest_hashes+=("$hash")
        manifest_modes+=("$mode"); manifest_paths+=("$relative")
    done < "$filename"
    (( count > 0 )) || fail 'Empty installation manifest.'
}

matches_entry() {
    local root=$1 index=$2 target="$1/${manifest_paths[$2]}"
    safe_parents "$root" "${manifest_paths[$index]}" || return 1
    case ${manifest_kinds[$index]} in
        F) [[ -f $target && ! -L $target ]] &&
            [[ $(hash_file "$target") == "${manifest_hashes[$index]}" &&
                $(stat -c '%a' -- "$target") == "${manifest_modes[$index]}" ]] ;;
        L) [[ -L $target ]] && [[ $(hash_link "$target") == "${manifest_hashes[$index]}" ]] ;;
        D) [[ -d $target && ! -L $target ]] ;;
    esac
}

verify_tree() {
    local index
    for index in "${!manifest_paths[@]}"; do
        matches_entry "$1" "$index" || fail "Missing or changed installed path: ${manifest_paths[$index]}."
    done
}
