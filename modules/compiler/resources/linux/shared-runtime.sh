# Copyright (c) 2026-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later

select_layout() {
    manifest_hash=$private_manifest_hash
    uninstall_hash=$private_uninstall_hash
    if [[ -n $selected_runtime ]]; then
        manifest_hash=$shared_manifest_hash
        uninstall_hash=$shared_uninstall_hash
    fi
}

validate_runtime_location() {
    local root=$selected_runtime
    while [[ $root == */ && $root != / ]]; do root=${root%/}; done
    [[ $root != *:* && $root != *';'* && $root != *'$'* && $root != *[[:cntrl:]]* &&
        /${root#/}/ != */./* && /${root#/}/ != */../* ]] || fail 'Unsupported runtime installation path.'
    shared_root=$(realpath -ms -- "$root/$runtime_engine") || fail 'Cannot resolve the runtime folder.'
    [[ $shared_root != "$1" && $shared_root != "$1/"* && $1 != "$shared_root/"* ]] ||
        fail 'The application and shared runtime folders cannot overlap.'
    registration="$(runtime_configuration)/nelson/runtimes/glnxa64/$runtime_engine.root"
}

prepare_shared_runtime() {
    local content=$1 package=$2 metadata="$1/.nelson-install" filename expected
    mv -- "$metadata/shared" "$package"
    for filename in manifest.tsv uninstall.sh runtime-installer runtime-package.json; do
        case $filename in
            manifest.tsv) expected=$shared_manifest_hash ;;
            uninstall.sh) expected=$shared_uninstall_hash ;;
            runtime-installer) expected=$runtime_helper_hash ;;
            runtime-package.json) expected=$runtime_inventory_hash ;;
        esac
        [[ -f $package/$filename && ! -L $package/$filename &&
            $(hash_file "$package/$filename") == "$expected" ]] || fail 'Invalid shared runtime package.'
    done
    mv -- "$content/$runtime_relative" "$package/payload"
    mv -- "$package/manifest.tsv" "$metadata/manifest.tsv"
    mv -- "$package/uninstall.sh" "$metadata/uninstall.sh"
    select_layout
    load_manifest "$metadata/manifest.tsv"
    verify_tree "$content"
    "$package/runtime-installer" --install "$package" "$shared_root" "$registration"
    printf 'Shared runtime uninstaller: %s/.nelson-runtime/uninstall\n' "$shared_root"
}
