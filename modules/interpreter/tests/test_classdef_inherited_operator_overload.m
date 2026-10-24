%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% operator overloads inherited from a superclass are dispatched for a value class
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
a = ClassdefOperatorInherit(3);
b = ClassdefOperatorInherit(4);
assert_isequal(a + b, 7);
assert_isequal(a - b, -1);
assert_isequal(a * b, 12);
assert_isequal(a == b, false);
assert_isequal(a == a, true);
assert_isequal(plus(a, b), 7);
assert_isequal(mtimes(a, b), 12);
%=============================================================================
