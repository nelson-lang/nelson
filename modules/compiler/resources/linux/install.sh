# Copyright (c) 2026-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later

perform_install() (
    local destination=${selected_destination:-$default_destination} parent base stage=''
    local payload_line metadata installed=no shared_root='' registration=''
    select_layout
    cleanup() {
        if [[ -n $stage && $stage == "$parent/.$base.nelson-install."* && ! -L $stage ]]; then
            rm -rf -- "$stage"
        fi
    }
    trap cleanup EXIT
    trap 'exit 130' INT
    trap 'exit 143' TERM
    [[ $(uname -s) == Linux && $(uname -m) == "$architecture" ]] || fail 'Installer and target architectures differ.'
    while [[ $destination != / ]]; do
        case $destination in
            */) destination=${destination%/} ;;
            */.) destination=${destination%/.} ;;
            *) break ;;
        esac
    done
    destination=${destination:-/}
    [[ ! -L $destination ]] || fail 'The installation folder cannot be a symbolic link.'
    destination=$(realpath -m -- "$destination") || fail 'Cannot resolve the installation folder.'
    [[ $destination != / ]] || fail 'Cannot install into the filesystem root.'
    if [[ -n $selected_runtime ]]; then validate_runtime_location "$destination"; fi
    parent=$(dirname -- "$destination"); base=${destination##*/}
    [[ -d $parent && -w $parent ]] || fail 'The installation parent directory must exist and be writable.'
    stage=$(mktemp -d -- "$parent/.$base.nelson-install.XXXXXXXX") || fail 'Cannot create a private installation directory.'
    payload_line=$(awk '$0 == "__NELSON_PAYLOAD__" { print NR + 1; exit }' "$self")
    [[ $payload_line =~ ^[0-9]+$ ]] || fail 'Missing installer payload.'
    tail -n "+$payload_line" -- "$self" > "$stage/payload.tar.gz"
    [[ $(hash_file "$stage/payload.tar.gz") == "$payload_hash" ]] || fail 'Installer payload checksum differs.'
    if [[ -e $destination || -L $destination ]]; then
        metadata="$destination/.nelson-install"
        [[ -d $destination && ! -L $destination && -d $metadata && ! -L $metadata ]] || fail 'Installation folder already exists and is not a recognized installation.'
        load_manifest "$metadata/manifest.tsv"
        [[ -f $metadata/uninstall.sh && ! -L $metadata/uninstall.sh &&
            $(hash_file "$metadata/uninstall.sh") == "$uninstall_hash" ]] || fail 'Installed uninstaller was changed.'
        verify_tree "$destination"
        if [[ -z $selected_runtime ]]; then
            printf 'Already installed and verified: %s\n' "$destination"
            exit 0
        fi
        [[ ! -e $destination/$runtime_relative && ! -L $destination/$runtime_relative ]] ||
            fail 'A private runtime conflicts with the shared installation.'
        installed=yes
    fi
    mkdir "$stage/content"
    tar --extract --gzip --file="$stage/payload.tar.gz" --directory="$stage/content" \
        --no-same-owner --same-permissions --delay-directory-restore
    metadata="$stage/content/.nelson-install"
    [[ -d $metadata && ! -L $metadata ]] || fail 'Missing installation metadata.'
    if [[ -n $selected_runtime ]]; then
        # The native helper verifies runtime files; the shared manifest verifies application files.
        prepare_shared_runtime "$stage/content" "$stage/runtime-package"
    else
        load_manifest "$metadata/manifest.tsv"
        [[ -f $metadata/uninstall.sh && ! -L $metadata/uninstall.sh &&
            $(hash_file "$metadata/uninstall.sh") == "$uninstall_hash" ]] || fail 'Invalid packaged uninstaller.'
        verify_tree "$stage/content"
        if [[ -n $runtime_relative ]]; then rm -rf -- "$metadata/shared"; fi
    fi
    if [[ $installed == yes ]]; then
        printf 'Already installed and verified: %s\n' "$destination"
        exit 0
    fi
    chmod 755 "$stage/content"
    # coreutils >= 9.2 makes "mv --no-clobber" exit nonzero when it refuses an
    # existing target; guard it so set -e does not abort before the explicit,
    # more informative check below (older coreutils exited zero and skipped).
    mv --no-clobber -T -- "$stage/content" "$destination" 2>/dev/null || true
    [[ ! -d $stage/content ]] || fail 'The installation folder was created by another process.'
    printf 'Installed %s in %s\n' "$application_name" "$destination"
    printf 'Uninstaller: %s/.nelson-install/uninstall.sh\n' "$destination"
)

self=$(readlink -f -- "${BASH_SOURCE[0]}") || fail 'Cannot locate the installer.'
(( BASH_VERSINFO[0] >= 4 )) || fail 'The installer requires Bash 4 or later.'
declare -A option_seen=()
agreed=no
selected_destination=''
selected_runtime=''
output_file=''
parse_options "$@"
if [[ -n $output_file ]]; then
    (set -C; : > "$output_file") 2>/dev/null || fail 'The output log must be a new writable file.'
    perform_install 2>&1 | tee -a -- "$output_file"
else
    perform_install
fi
