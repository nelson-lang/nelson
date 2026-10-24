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
[val, idx] = mink(A, k);
asserts.isequal(val, [1 3]);
asserts.isequal(idx, [2 4]);
%=============================================================================
A = [5; 1; 4; 3];
[val, idx] = mink(A, 2);
asserts.isequal(val, [1; 3]);
asserts.isequal(idx, [2; 4]);
%=============================================================================
A = [1 4 2; 7 3 5]; % 2x3
[val, idx] = mink(A, 1);
asserts.isequal(val, [1 3 2]);
asserts.isequal(idx, [1 2 1]);
%=============================================================================
[val, idx] = mink(A, 2, 2);
asserts.isequal(val, [1 2; 3 5]);
asserts.isequal(idx, [1 3; 2 3]);
%=============================================================================
% k larger than the array size returns all elements sorted (reference-implementation semantics)
[val, idx] = mink([1, 2, 3], 5);
asserts.isequal(val, [1 2 3]);
asserts.isequal(idx, [1 2 3]);
%=============================================================================
[minValues, minIndices] = mink([5 1 4 3], 2);
asserts.isequal(minValues, [1 3]);
asserts.isequal(minIndices, [2 4]);
asserts.isequal(mink([1, 2, 3], 5), [1 2 3]);
%=============================================================================
