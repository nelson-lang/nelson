%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
path_test = [modulepath('interpreter', 'tests'), '/nested_scope'];
addpath(path_test);
%=============================================================================
% The input argument of a nested function is local to it: even though the parent
% function has a variable with the same name (f), calling the nested function
% must not overwrite the parent's variable.
[parentAfter, innerResult, sharedAfter] = nested_param_shadow();
%=============================================================================
% The parent 'f' keeps its own value (a char row vector), untouched by the
% nested function's numeric parameter 'f'.
assert_isequal(parentAfter, 'PARENT_STRING');
%=============================================================================
% The nested function computed with its local parameter.
assert_isequal(innerResult, 43);
%=============================================================================
% A variable that is not a parameter of the nested function (shared) is genuinely
% shared and its update inside the nested function is visible to the parent.
assert_isequal(sharedAfter, 11);
%=============================================================================
