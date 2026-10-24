# Copyright (c) 2026-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later

set_option() {
    local key=$1 value=$2
    [[ ${#value} -le 32768 && $value != *[[:cntrl:]]* ]] || fail 'Invalid installation option value.'
    case $key in
        agreeToLicense) [[ $value == yes ]] || fail 'License agreement is required.'; agreed=yes; return ;;
        runtimeFolder)
            [[ $installer_kind == application && -n ${runtime_relative:-} ]] ||
                fail 'runtimeFolder requires an application installer with shared-runtime support.' ;;
        applicationFolder) [[ $installer_kind == application ]] || fail 'Runtime installers do not accept applicationFolder.' ;;
        destinationFolder|outputFile) ;;
        *) fail "Unsupported installation option: $key." ;;
    esac
    [[ ! ${option_seen[$key]+present} ]] || fail "Duplicate installation option: $key."
    option_seen[$key]=1
    case $key in
        applicationFolder|destinationFolder)
            [[ -z $selected_destination ]] || fail 'applicationFolder and destinationFolder cannot be combined.'
            [[ $key != destinationFolder || -z ${selected_runtime:-} ]] || fail 'runtimeFolder and destinationFolder cannot be combined.'
            [[ $value == /* ]] || fail 'The installation folder must be absolute.'
            selected_destination=$value ;;
        runtimeFolder)
            [[ ! ${option_seen[destinationFolder]+present} ]] || fail 'runtimeFolder and destinationFolder cannot be combined.'
            [[ $value == /* ]] || fail 'The runtime folder must be absolute.'
            selected_runtime=$value ;;
        outputFile) [[ -n $value ]] || fail 'An output log filename is required.'; output_file=$value ;;
    esac
}

read_options() {
    local filename=$1 line key value first=yes line_number=0
    [[ -f $filename && $(stat -Lc '%s' -- "$filename") -le 1048576 ]] || fail 'Expected an installer control file no larger than 1 MiB.'
    if IFS= read -r -d '' line < "$filename"; then fail 'Control files cannot contain NUL bytes.'; fi
    while IFS= read -r line || [[ -n $line ]]; do
        (( ++line_number ))
        if (( line_number == 1 )); then line=${line#$'\xef\xbb\xbf'}; fi
        line=${line%$'\r'}
        [[ -n $line && $line != \#* ]] || continue
        [[ $line == *=* ]] || fail 'Control file entries must use key=value.'
        key=${line%%=*}; value=${line#*=}
        if [[ $first == yes && $agreed != yes ]]; then
            [[ $key == agreeToLicense && $value == yes ]] || fail 'agreeToLicense=yes must be the first control file option.'
        fi
        first=no
        set_option "$key" "$value"
    done < "$filename"
}

parse_options() {
    local input_option=-inputFile
    if [[ $installer_kind == runtime ]]; then input_option=-inputfile; fi
    if [[ $# == 2 && $1 == "$input_option" ]]; then
        read_options "$2"
    else
        [[ $# -ge 2 && $1 == -agreeToLicense && $2 == yes ]] ||
            fail "Use -agreeToLicense yes first or $input_option FILE. Interactive installation is not available."
        agreed=yes
        shift 2
        while (( $# > 0 )); do
            [[ $# -ge 2 && $1 == -* ]] || fail 'Installation options require a value.'
            if [[ $1 == "$input_option" ]]; then
                read_options "$2"
            else
                set_option "${1#-}" "$2"
            fi
            shift 2
        done
    fi
    [[ $agreed == yes ]] || fail 'License agreement is required.'
}
