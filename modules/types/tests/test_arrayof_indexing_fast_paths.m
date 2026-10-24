%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
v = 1:10;
asserts.isequal(v(3:6), 3:6);
asserts.isequal(v([1 3 10]), [1 3 10]);
asserts.istrue(isempty(v([])));
%=============================================================================
A = reshape(1:12, [3 4]);
asserts.isequal(A(:, 2:3), [4 7; 5 8; 6 9]);
asserts.isequal(A([1 3], [2 4]), [4 10; 6 12]);
%=============================================================================
B = A;
B(:, 2:3) = 0;
asserts.isequal(A, reshape(1:12, [3 4]));
asserts.isequal(B, [1 0 0 10; 2 0 0 11; 3 0 0 12]);
%=============================================================================
C = reshape(1:24, [4 3 2]);
D = C(:, 2, :);
asserts.isequal(size(D), [4 1 2]);
asserts.isequal(D, reshape([5 6 7 8 17 18 19 20], [4 1 2]));
E = C(:, :, 2);
asserts.isequal(size(E), [4 3]);
asserts.isequal(E, reshape(13:24, [4 3]));
F = C(2:3, :, 2);
asserts.isequal(F, [14 18 22; 15 19 23]);
G = zeros(4, 3, 2);
G(:, [1 3], :) = C(:, [1 3], :);
asserts.isequal(G(:, 1, :), C(:, 1, :));
asserts.isequal(G(:, 2, :), zeros(4, 1, 2));
asserts.isequal(G(:, 3, :), C(:, 3, :));
%=============================================================================
CA = {1, 2; 3, 4};
asserts.isequal(CA{2, 1}, 3);
CB = CA;
CB{1, 2} = 20;
asserts.isequal(CA{1, 2}, 2);
asserts.isequal(CB{1, 2}, 20);
%=============================================================================
H = reshape(1:12, [3 4]);
H(:, 2) = [];
asserts.isequal(H, [1 7 10; 2 8 11; 3 9 12]);
I = reshape(1:24, [4 3 2]);
I(:, 2, :) = [];
asserts.isequal(size(I), [4 2 2]);
asserts.isequal(I(:, 1, :), C(:, 1, :));
asserts.isequal(I(:, 2, :), C(:, 3, :));
%=============================================================================
S = sparse([1 0; 0 2]);
T = S(:, 2);
asserts.istrue(issparse(T));
asserts.isequal(full(T), [0; 2]);
S2 = S;
S2(1, 2) = 3;
asserts.istrue(issparse(S2));
asserts.isequal(full(S), [1 0; 0 2]);
asserts.isequal(full(S2), [1 3; 0 2]);
S3 = sparse([1 0 2; 0 3 0]);
S3(:, 2) = [];
asserts.istrue(issparse(S3));
asserts.isequal(full(S3), [1 2; 0 0]);
%=============================================================================
