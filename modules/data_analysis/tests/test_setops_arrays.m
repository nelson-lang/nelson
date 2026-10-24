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
asserts.isequal(union(A, B), [1 3 5 7]);
asserts.isequal(intersect(A, B), 1);
asserts.isequal(setdiff(A, B), [5 7]);
asserts.isequal(setxor(A, B), [3 5 7]);
%=============================================================================
