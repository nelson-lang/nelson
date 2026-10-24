# Copyright (c) 2026-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later

runtime_configuration() {
    local config=${XDG_CONFIG_HOME:-}
    if (( EUID == 0 )); then
        config=/etc/xdg
    elif [[ $config != /* ]]; then
        [[ ${HOME:-} == /* ]] || fail 'HOME or XDG_CONFIG_HOME must identify an absolute configuration directory.'
        config=$HOME/.config
    fi
    printf '%s' "$config"
}
