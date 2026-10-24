%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% a no-argument method of a value class can be called via dot without parens
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
v = ClassdefNoArgMethodDot([4 5 6]);
% assignment RHS context
m = v.doubled;
assert_isequal(m, [8 10 12]);
% with parentheses still works
assert_isequal(v.doubled(), [8 10 12]);
% function-argument context
assert_isequal(sum(v.doubled), 30);
assert_isequal(max(v.doubled), 12);
%=============================================================================
