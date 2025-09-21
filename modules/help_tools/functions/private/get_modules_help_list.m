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
    run([nelsonroot() '/modules/' 'modules.m']);
    funcList = @(x) x{1};
    modules_help_list = string(cellfun(funcList, modules_list, 'UniformOutput', false));
    st_packages = nmm('list');
    external_modules_names = fieldnames(st_packages);
    modules_help_list = [modules_help_list; string(external_modules_names)];
end