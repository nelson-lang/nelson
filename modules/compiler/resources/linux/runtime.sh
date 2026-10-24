# Copyright (c) 2026-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later

perform_install() (
    local destination=$selected_destination config data stage='' payload_line
    [[ $(uname -s) == Linux && $(uname -m) == "$architecture" ]] || fail 'Runtime installer and target architectures differ.'
    config=$(runtime_configuration)
    if (( EUID == 0 )); then
        data=/usr/local/lib
    else
        data=${XDG_DATA_HOME:-}
        if [[ $data != /* ]]; then
            [[ ${HOME:-} == /* ]] || fail 'HOME or XDG_DATA_HOME must identify an absolute data directory.'
            data=$HOME/.local/share
        fi
    fi
    destination=${destination:-$data/nelson/runtimes/$engine}
    [[ $destination != *:* && $destination != *';'* && $destination != *'$'* && $destination != *[[:cntrl:]]* ]] || fail 'Unsupported runtime installation path.'
    cleanup() { if [[ -n $stage && ! -L $stage ]]; then rm -rf -- "$stage"; fi; }
    trap cleanup EXIT
    trap 'exit 130' INT
    trap 'exit 143' TERM
    stage=$(mktemp -d -- "${TMPDIR:-/tmp}/nelson-runtime-install.XXXXXXXX") || fail 'Cannot create private runtime staging directory.'
    payload_line=$(awk '$0 == "__NELSON_PAYLOAD__" { print NR + 1; exit }' "$self")
    [[ $payload_line =~ ^[0-9]+$ ]] || fail 'Missing runtime installer payload.'
    tail -n "+$payload_line" -- "$self" > "$stage/runtime.tar.gz"
    [[ $(hash_file "$stage/runtime.tar.gz") == "$payload_hash" ]] || fail 'Runtime installer payload checksum differs.'
    mkdir "$stage/package"
    tar --extract --gzip --file="$stage/runtime.tar.gz" --directory="$stage/package" \
        --no-same-owner --same-permissions --delay-directory-restore
    "$stage/package/runtime-installer" --install "$stage/package" "$destination" \
        "$config/nelson/runtimes/glnxa64/$engine.root"
)

self=$(readlink -f -- "${BASH_SOURCE[0]}") || fail 'Cannot locate the runtime installer.'
(( BASH_VERSINFO[0] >= 4 )) || fail 'Bash 4 or later is required.'
declare -A option_seen=()
agreed=no
selected_destination=''
output_file=''
parse_options "$@"
if [[ -n $output_file ]]; then
    (set -C; : > "$output_file") 2>/dev/null || fail 'The output log must be a new writable file.'
    perform_install 2>&1 | tee -a -- "$output_file"
else
    perform_install
fi
