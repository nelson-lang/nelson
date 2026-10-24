%=============================================================================
% Copyright (c) 2020-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = [5, 1, 4, 3];
k = 2;
[val, idx] = maxk(A, k);
asserts.isequal(val, [5 4]);
asserts.isequal(idx, [1 3]);
%=============================================================================
A = [5; 1; 4; 3];
[val, idx] = maxk(A, 2);
asserts.isequal(val, [5; 4]);
asserts.isequal(idx, [1; 3]);
%=============================================================================
A = [1 4 2; 7 3 5]; % 2x3
[val, idx] = maxk(A, 1);
asserts.isequal(val, [7 4 5]);
asserts.isequal(idx, [2 1 2]);
%=============================================================================
[val, idx] = maxk(A, 2, 2);
asserts.isequal(val, [4 2; 7 5]);
asserts.isequal(idx, [2 3; 1 3]);
%=============================================================================
% k larger than the array size returns all elements sorted (reference-implementation semantics)
[val, idx] = maxk([1, 2, 3], 5);
asserts.isequal(val, [3 2 1]);
asserts.isequal(idx, [3 2 1]);
% NaN values are placed last
asserts.isequal(maxk([1 5 3 NaN], 2), [5 3]);
%=============================================================================
[maxValues, maxIndices] = maxk([5 1 4 3], 2);
asserts.isequal(maxValues, [5 4]);
asserts.isequal(maxIndices, [1 3]);
asserts.isequal(maxk([1, 2, 3], 5), [3 2 1]);
%=============================================================================
