#!/bin/bash
# Copyright (c) 2026-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later
set -euo pipefail
export PATH=/usr/bin:/bin LC_ALL=C TZ=UTC
unset TAR_OPTIONS GZIP GZIP_OPT POSIXLY_CORRECT CDPATH
umask 077
[[ $# == 4 && $(uname -s) == Linux ]] || exit 1
resources=$(cd -P -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
source "$resources/common.sh"
package=$(realpath -- "$1"); stage=$(realpath -- "$2"); name=$3; engine=$4
[[ $package == "$stage/package" && -d $package && ! -L $1 ]] || fail 'Invalid runtime package staging path.'
[[ $name != */* && -n $name && $name != *[[:cntrl:]]* && $engine =~ ^[0-9a-f]{64}$ ]] || fail 'Invalid runtime package identity.'
[[ $(tar --version) == *'GNU tar'* ]] || fail 'GNU tar is required.'
find "$package" -type d -exec chmod 755 -- {} +
tar --create --format=posix --pax-option=exthdr.name=%d/PaxHeaders/%f,delete=atime,delete=ctime --sort=name \
    --mtime=@946684800 --owner=0 --group=0 --numeric-owner --file="$stage/runtime.tar" --directory="$package" .
gzip -n -9 < "$stage/runtime.tar" > "$stage/runtime.tar.gz"
payload_hash=$(hash_file "$stage/runtime.tar.gz")
mkdir "$stage/publish"
installer="$stage/publish/$name.install"
{
    printf '#!/bin/bash\nset -euo pipefail\nexport PATH=/usr/bin:/bin LC_ALL=C\numask 077\n'
    printf 'unset TAR_OPTIONS GZIP GZIP_OPT POSIXLY_CORRECT CDPATH\n'
    printf 'readonly installer_kind=runtime\nreadonly engine=%q\n' "$engine"
    printf 'readonly architecture=%q\nreadonly payload_hash=%q\n' "$(uname -m)" "$payload_hash"
    cat -- "$resources/common.sh" "$resources/runtime-location.sh" "$resources/options.sh" "$resources/runtime.sh"
    printf '\nexit 0\n__NELSON_PAYLOAD__\n'
    cat -- "$stage/runtime.tar.gz"
} > "$installer"
chmod 755 -- "$installer"
