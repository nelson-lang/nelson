#!/bin/sh
# Copyright (c) 2026-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later
set -eu
export PATH=/usr/bin:/bin:/usr/sbin:/sbin LC_ALL=C
umask 077

fail()
{
    printf 'Nelson installer: %s\n' "$*" >&2
    exit 1
}

hash_file()
{
    /usr/bin/shasum -a 256 "$1" | /usr/bin/awk '{print $1}'
}

safe_relative()
{
    case "$1" in
        ''|/*|*/|*//*|*\\*|*../*|../*|*/..|.|./*|*/./*) return 1 ;;
    esac
    return 0
}

actual_destination()
{
    target=${3%/}
    [ -n "$target" ] || target=/
    case "$2" in
        /*) printf '%s%s' "$target" "$2" ;;
        *) fail 'The package installation location is not absolute.' ;;
    esac
}

manifest_contains()
{
    /usr/bin/awk -F '\t' -v wanted="$2" '$4 == wanted { found = 1 } END { exit !found }' "$1"
}

verify_manifest()
{
    root=$1
    manifest=$2
    while IFS="$(printf '\t')" read -r kind digest mode relative; do
        safe_relative "$relative" || fail "Invalid installed path: $relative"
        target=$root/$relative
        case "$kind" in
            F)
                [ -f "$target" ] && [ ! -L "$target" ] || fail "Missing installed file: $relative"
                [ "$(hash_file "$target")" = "$digest" ] || fail "Changed installed file: $relative"
                [ "$(/usr/bin/stat -f '%Lp' "$target")" = "$mode" ] || fail "Changed installed mode: $relative"
                ;;
            D)
                [ -d "$target" ] && [ ! -L "$target" ] || fail "Missing installed directory: $relative"
                ;;
            *) fail 'Invalid installation manifest.' ;;
        esac
    done < "$manifest"
}
