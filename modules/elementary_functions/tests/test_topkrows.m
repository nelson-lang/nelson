%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = [3 1; 1 2; 2 3; 1 1];
asserts.isequal(topkrows(A, 2), [3 1; 2 3]);
asserts.isequal(topkrows(A, 2, 1, 'ascend'), [1 2; 1 1]);
[B, I] = topkrows(A, 2);
asserts.isequal(I', [1 3]);
asserts.isequal(size(topkrows(A, 10)), [4 2]);
%=============================================================================
% Works on a table (single output; the index output needs table sortrows).
T = table([3; 1; 2], 'VariableNames', {'v'});
T2 = topkrows(T, 2);
asserts.isequal(T2.v, [3; 2]);
%=============================================================================
