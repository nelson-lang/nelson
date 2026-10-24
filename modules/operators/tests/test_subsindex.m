%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% subsindex and subsasgn are two distinct builtins of the operators module
assert_isequal(exist('subsindex', 'builtin'), 5);
assert_isequal(exist('subsasgn', 'builtin'), 5);
%=============================================================================
R = subsasgn([1 2 3], substruct('()', {2}), 5);
assert_isequal(R, [1 5 3]);
%=============================================================================
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
b = ClassdefOperatorFolderBase(2);
d = ClassdefOperatorFolderDerived(3);
%=============================================================================
% own method: its result is returned unchanged (zero-based)
assert_isequal(subsindex(b), 1);
%=============================================================================
% inherited method
assert_isequal(subsindex(d), 2);
%=============================================================================
% value without subsindex method
msg = sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'subsindex');
assert_checkerror('subsindex(5)', msg, 'Nelson:UndefinedFunction:incorrectArgumentType');
assert_checkerror('subsindex(struct(''a'', 1))', msg, 'Nelson:UndefinedFunction:incorrectArgumentType');
assert_checkerror('builtin(''subsindex'', 5)', msg, 'Nelson:UndefinedFunction:incorrectArgumentType');
%=============================================================================
% builtin() bypasses the class method: no built-in conversion of an object
assert_checkerror('builtin(''subsindex'', b)', msg, 'Nelson:UndefinedFunction:incorrectArgumentType');
%=============================================================================
rmpath(path_test);
%=============================================================================
