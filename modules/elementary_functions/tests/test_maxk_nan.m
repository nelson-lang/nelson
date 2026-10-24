%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% NaN are considered smaller than any value: they come out last
asserts.isequal(maxk([1 5 3 NaN], 2), [5 3]);
[v, i] = maxk([1 5 3 NaN], 2);
asserts.isequal(v, [5 3]);
asserts.isequal(i, [2 3]);
asserts.isequal(maxk([NaN NaN 1], 2), [1 NaN]);
asserts.isequal(maxk([NaN 1 2], 3), [2 1 NaN]);
%=============================================================================
% matrices, along columns and along dim 2
M = [16 2 3 13; 5 11 NaN 8; 9 7 6 12];
asserts.isequal(maxk(M, 2), [16 11 6 13; 9 7 3 12]);
asserts.isequal(maxk(M, 2, 2), [16 13; 11 8; 12 9]);
%=============================================================================
% no NaN: unchanged behavior
asserts.isequal(maxk([1 5 3], 2), [5 3]);
asserts.isequal(mink([1 5 3 NaN], 2), [1 3]);
%=============================================================================
