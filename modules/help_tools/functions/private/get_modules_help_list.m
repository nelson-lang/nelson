%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function modules_help_list = get_modules_help_list()
    modules_help_list = get_builtin_help_modules();
    st_packages = nmm('list');
    external_modules_names = fieldnames(st_packages);
    modules_help_list = [modules_help_list; string(external_modules_names)];
    modules_help_list = filter_requested_modules(modules_help_list);
    modules_help_list = modules_help_list(has_help_directory(modules_help_list));
end
%=============================================================================
function modules_help_list = filter_requested_modules(modules_help_list)
    requested = getenv('NELSON_HELP_MODULES');
    if isempty(requested)
        return
    end
    wanted = string(strtrim(strsplit(requested, {',', ';'})));
    wanted = wanted(wanted ~= "");
    if isempty(wanted)
        return
    end
    keep = false(size(modules_help_list));
    for k = 1:length(modules_help_list)
        keep(k) = any(strcmp(modules_help_list(k), wanted));
    end
    modules_help_list = modules_help_list(keep);
end
%=============================================================================
function tf = has_help_directory(modules_help_list)
    tf = false(size(modules_help_list));
    for k = 1:length(modules_help_list)
        module_path = get_help_module_path(modules_help_list{k});
        tf(k) = ~isempty(module_path) && isdir([module_path, '/help']);
    end
end
%=============================================================================
function module_path = get_help_module_path(module_name)
    module_path = '';
    if ismodule(module_name)
        module_path = modulepath(module_name);
    else
        path = [nelsonroot(), '/modules/', module_name, '/etc/startup.m'];
        if isfile(path)
            module_path = [nelsonroot(), '/modules/', module_name, '/'];
        end
    end
end
%=============================================================================
