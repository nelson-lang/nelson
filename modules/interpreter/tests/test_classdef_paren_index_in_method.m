%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Parenthesis indexing on an object of the defining class, performed inside a
% method of that class (here inside subsref itself), uses built-in indexing and
% does not re-enter the overloaded subsref.
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
o = ClassdefParenInMethod(7);
%=============================================================================
% o(1) enters subsref; inside, f(1) must use the built-in subset (no recursion).
assert_isequal(o(1), 7);
%=============================================================================
% Dot access still routes through the method's builtin('subsref', ...).
assert_isequal(o.v, 7);
%=============================================================================
