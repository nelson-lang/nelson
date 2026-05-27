%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = [5 7 1];
B = [3 1 1];
assert_isequal(union(A, B), [1 3 5 7]);
assert_isequal(intersect(A, B), 1);
assert_isequal(setdiff(A, B), [5 7]);
assert_isequal(setxor(A, B), [3 5 7]);
%=============================================================================
