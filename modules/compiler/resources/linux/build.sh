#!/bin/bash
# Copyright (c) 2026-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later
set -euo pipefail
export PATH=/usr/bin:/bin LC_ALL=C TZ=UTC
unset TAR_OPTIONS GZIP GZIP_OPT POSIXLY_CORRECT CDPATH
umask 077
[[ $# == 8 && $(uname -s) == Linux ]] || { echo 'Expected a Linux installer staging request.' >&2; exit 1; }
resources=$(cd -P -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
source "$resources/common.sh"
(( BASH_VERSINFO[0] >= 4 )) || fail 'Linux installer generation requires Bash 4 or later.'
payload=$(realpath -- "$1")
stage=$(realpath -- "$2")
name=$3
default_destination=$4
shortcut=$5
application_name=$6
runtime_relative=$7
runtime_engine=$8
[[ $payload == "$stage/payload" && -d $payload && ! -L $1 ]] || fail 'Invalid installer staging directory.'
[[ $(tar --version) == *'GNU tar'* ]] || fail 'Linux installer generation requires GNU tar.'
[[ -n $name && $name != */* && $name != *[[:cntrl:]]* ]] || fail 'Invalid installer name.'
[[ ! -e $payload/.nelson-install && ! -L $payload/.nelson-install ]] || fail 'Application inputs collide with .nelson-install.'
link_name=''
if [[ -n $shortcut ]]; then
    safe_relative "$shortcut" || fail 'Invalid shortcut target.'
    [[ -e $payload/$shortcut && ! -L $payload/$shortcut ]] || fail 'Missing shortcut target.'
    link_name="Launch $application_name"
    safe_relative "$link_name" || fail 'Invalid shortcut name.'
    [[ ! -e $payload/$link_name && ! -L $payload/$link_name ]] || fail 'Application inputs collide with the shortcut.'
    ln -s -- "$shortcut" "$payload/$link_name"
fi
cd -- "$payload"
find . -mindepth 1 -printf '%P\0' > "$stage/paths.nul"
sort -z -o "$stage/paths.nul" "$stage/paths.nul"
count=0
while IFS= read -r -d '' relative; do
    (( ++count <= 200000 )) || fail 'Installer file count limit exceeded.'
    safe_relative "$relative" || fail 'Unsupported installer payload path.'
    if [[ -L $relative ]]; then
        [[ -n $link_name && $relative == "$link_name" ]] || fail 'Symbolic links are not allowed in installer inputs.'
        digest=$(hash_link "$relative") || fail 'Cannot hash shortcut target.'
        printf 'L\t%s\t-\t%s\n' "$digest" "$relative"
    elif [[ -d $relative ]]; then
        chmod 755 -- "$relative"
        printf 'D\t-\t755\t%s\n' "$relative"
    elif [[ -f $relative ]]; then
        mode=644
        if [[ -x $relative ]]; then mode=755; fi
        chmod "$mode" -- "$relative"
        digest=$(hash_file "$relative")
        printf 'F\t%s\t%s\t%s\n' "$digest" "$mode" "$relative"
    else
        fail 'Only ordinary files and directories can be installed.'
    fi
done < "$stage/paths.nul" > "$stage/manifest.tsv"
manifest_hash=$(hash_file "$stage/manifest.tsv")
mkdir .nelson-install
cp -- "$stage/manifest.tsv" .nelson-install/manifest.tsv
write_uninstaller() {
    {
        printf '#!/bin/bash\nset -euo pipefail\nexport PATH=/usr/bin:/bin LC_ALL=C\nunset CDPATH\n'
        printf 'readonly manifest_hash=%q\n' "$1"
        cat -- "$resources/common.sh" "$resources/uninstall.sh"
    } > "$2"
    chmod 700 -- "$2"
}
write_uninstaller "$manifest_hash" .nelson-install/uninstall.sh
uninstall_hash=$(hash_file .nelson-install/uninstall.sh)
shared_manifest_hash='' shared_uninstall_hash='' runtime_helper_hash='' runtime_inventory_hash=''
if [[ -n $runtime_relative ]]; then
    safe_relative "$runtime_relative" && [[ $runtime_engine =~ ^[0-9a-f]{64}$ &&
        -d $payload/$runtime_relative && ! -L $payload/$runtime_relative ]] || fail 'Invalid shared runtime layout.'
    kit=.nelson-install/shared
    mv -- "$stage/runtime-package" "$kit"
    [[ ! -e $kit/payload && ! -L $kit/payload ]] || fail 'Duplicate runtime payload.'
    while IFS=$'\t' read -r kind digest mode relative; do
        if [[ $relative != "$runtime_relative" && $relative != "$runtime_relative/"* ]]; then
            printf '%s\t%s\t%s\t%s\n' "$kind" "$digest" "$mode" "$relative"
        fi
    done < "$stage/manifest.tsv" > "$kit/manifest.tsv"
    shared_manifest_hash=$(hash_file "$kit/manifest.tsv")
    write_uninstaller "$shared_manifest_hash" "$kit/uninstall.sh"
    shared_uninstall_hash=$(hash_file "$kit/uninstall.sh")
    runtime_helper_hash=$(hash_file "$kit/runtime-installer")
    runtime_inventory_hash=$(hash_file "$kit/runtime-package.json")
    chmod 755 -- "$kit" "$kit/runtime-installer"
    chmod 600 -- "$kit/manifest.tsv"
    chmod 644 -- "$kit/runtime-package.json"
fi
chmod 755 "$payload"
tar --create --format=posix --pax-option=exthdr.name=%d/PaxHeaders/%f,delete=atime,delete=ctime --sort=name \
    --mtime=@946684800 --owner=0 --group=0 --numeric-owner \
    --file="$stage/payload.tar" --directory="$payload" .
gzip -n -9 < "$stage/payload.tar" > "$stage/payload.tar.gz"
payload_hash=$(hash_file "$stage/payload.tar.gz")
mkdir "$stage/publish"
installer="$stage/publish/$name.install"
{
    printf '#!/bin/bash\nset -euo pipefail\nexport PATH=/usr/bin:/bin LC_ALL=C\numask 077\n'
    printf 'unset TAR_OPTIONS GZIP GZIP_OPT POSIXLY_CORRECT CDPATH\n'
    printf 'readonly default_destination=%q\n' "$default_destination"
    printf 'readonly installer_kind=application\n'
    printf 'readonly application_name=%q\n' "$application_name"
    printf 'readonly architecture=%q\n' "$(uname -m)"
    printf 'readonly payload_hash=%q\n' "$payload_hash"
    printf 'readonly private_manifest_hash=%q\n' "$manifest_hash"
    printf 'readonly private_uninstall_hash=%q\n' "$uninstall_hash"
    printf 'readonly shared_manifest_hash=%q\n' "$shared_manifest_hash"
    printf 'readonly shared_uninstall_hash=%q\n' "$shared_uninstall_hash"
    printf 'readonly runtime_relative=%q\nreadonly runtime_engine=%q\n' "$runtime_relative" "$runtime_engine"
    printf 'readonly runtime_helper_hash=%q\nreadonly runtime_inventory_hash=%q\n' "$runtime_helper_hash" "$runtime_inventory_hash"
    cat -- "$resources/common.sh" "$resources/runtime-location.sh" "$resources/options.sh" \
        "$resources/shared-runtime.sh" "$resources/install.sh"
    printf '\nexit 0\n__NELSON_PAYLOAD__\n'
    cat -- "$stage/payload.tar.gz"
} > "$installer"
chmod 755 -- "$installer"
printf 'Created Linux installer: %s\n' "$name.install"
