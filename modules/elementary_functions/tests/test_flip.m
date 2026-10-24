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
A = 'no word, no bond, row on.';
B = flip(A);
asserts.isequal(B, '.no wor ,dnob on ,drow on');
%=============================================================================
A = eye(2, 3);
B = flip(A, 1);
REF = [0 1 0; 1 0 0];
asserts.isequal(B, REF);
%=============================================================================
B = flip(A, 2);
REF = [0 0 1; 0 1 0];
asserts.isequal(B, REF);
%=============================================================================
B = flip(A, 3);
REF = [1 0 0; 0 1 0];
asserts.isequal(B, REF);
%=============================================================================
A = [];
A(1:2, 1:2, 1) = [5 6; 7 8];
A(1:2, 1:2, 2) = [1 2; 3 4];
B = flip(A, 1);
REF = [];
REF(1:2, 1:2, 1) = [7 8; 5 6];
REF(1:2, 1:2, 2) = [3 4; 1 2];
asserts.isequal(B, REF);
%=============================================================================
A = [];
A(1:2, 1:2, 1) = [5 6; 7 8];
A(1:2, 1:2, 2) = [1 2; 3 4];
B = flip(A, 2);
REF = [];
REF(1:2, 1:2, 1) = [6 5; 8 7];
REF(1:2, 1:2, 2) = [2 1; 4 3];
asserts.isequal(B, REF);
%=============================================================================
A = [];
A(1:2, 1:2, 1) = [5 6; 7 8];
A(1:2, 1:2, 2) = [1 2; 3 4];
B = flip(A, 3);
REF = [];
REF(1:2, 1:2, 1) = [1 2; 3 4];
REF(1:2, 1:2, 2) = [5 6; 7 8];
asserts.isequal(B, REF);
%=============================================================================
A = [];
A(1:2, 1:2, 1) = [5 6; 7 8];
A(1:2, 1:2, 2) = [1 2; 3 4];
B = flip(A);
REF = flip(A, 1);
asserts.isequal(B, REF);
%=============================================================================
asserts.isequal(flip([1 2 3]), [3 2 1]);
%=============================================================================
% N-D flip along every dimension (fast builtin path) matches index reversal
A = reshape(1:60, [3 4 5]);
asserts.isequal(flip(A, 1), A(3:-1:1, :, :));
asserts.isequal(flip(A, 2), A(:, 4:-1:1, :));
asserts.isequal(flip(A, 3), A(:, :, 5:-1:1));
% flipping a singleton / out-of-range dimension is the identity
asserts.isequal(flip(A, 4), A);
%=============================================================================
% class is preserved across supported types
asserts.isequal(class(flip(single(A), 3)), 'single');
asserts.isequal(class(flip(int16(A), 2)), 'int16');
asserts.isequal(flip(int8([1 2 3; 4 5 6]), 2), int8([3 2 1; 6 5 4]));
asserts.isequal(flip('abc', 2), 'cba');
Z = reshape((1:8) + 1i * (8:-1:1), 2, 2, 2);
asserts.isequal(flip(Z, 3), Z(:, :, 2:-1:1));
%=============================================================================
% generic (cell) path still works through the fallback
C = {1, 'ab', [3 4]; 5, 6, 7};
asserts.isequal(flip(C, 2), C(:, 3:-1:1));
%=============================================================================
