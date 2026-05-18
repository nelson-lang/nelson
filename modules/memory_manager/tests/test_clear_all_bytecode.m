%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
leaked_by_named_clear = 1;
clear('leaked_by_named_clear');
assert_isequal(exist('leaked_by_named_clear', 'var'), 0);
%=============================================================================
true = 1;
false = 0;
modules_list = {'memory_manager'};
module_state = {'memory_manager', true};
clear('all');
assert_isequal(exist('true', 'var'), 0);
assert_isequal(exist('false', 'var'), 0);
assert_isequal(exist('modules_list', 'var'), 0);
assert_isequal(exist('module_state', 'var'), 0);
%=============================================================================
clear_alias = @clear;
alias_workspace_value = 1;
clear_alias('all');
assert_isequal(exist('clear_alias', 'var'), 0);
assert_isequal(exist('alias_workspace_value', 'var'), 0);
%=============================================================================
