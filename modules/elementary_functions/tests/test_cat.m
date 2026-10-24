%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
asserts.isequal(cat(1, zeros(0, 2), [1 2]), [1 2]);
asserts.isequal(size(cat(3, [1 2], [3 4])), [1 2 2]);
%=============================================================================
% N-D concatenation (dim >= 3): native builtin, permute-based
A = [1 2; 3 4];
B = [5 6; 7 8];
C3 = cat(3, A, B);
asserts.isequal(size(C3), [2 2 2]);
asserts.isequal(C3(:, :, 1), A);
asserts.isequal(C3(:, :, 2), B);
asserts.isequal(cat(3, A, B), reshape([A, B], 2, 2, 2));
asserts.isequal(size(cat(4, A, B, A)), [2 2 1 3]);
asserts.isequal(size(cat(3, A, B, A + 10)), [2 2 3]);
%=============================================================================
% class promotion, complex, and empty handling along dim 3
asserts.isequal(class(cat(3, int8(A), B)), 'int8');
asserts.isequal(class(cat(3, single(A), B)), 'single');
Z = cat(3, A + 2i * B, B - 1i * A);
asserts.isequal(Z(:, :, 1), A + 2i * B);
asserts.isequal(cat(3, [], A, []), A);
asserts.isequal(cat(3, A), A);
asserts.isequal(isempty(cat(3)), true);
%=============================================================================
% non-square N-D operands
P = reshape(1:24, 2, 3, 4);
Q = reshape(25:48, 2, 3, 4);
R = cat(3, P, Q);
asserts.isequal(size(R), [2 3 8]);
asserts.isequal(R(:, :, 1:4), P);
asserts.isequal(R(:, :, 5:8), Q);
asserts.isequal(size(cat(1, P, Q)), [4 3 4]);
%=============================================================================
