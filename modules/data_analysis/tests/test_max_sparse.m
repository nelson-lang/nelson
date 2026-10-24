%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Two-argument (element-wise) max with sparse operands.
%=============================================================================
A = sparse([1 1 2], [2 3 4], [1 4 1], 4, 4);
B = A';
FA = full(A);
FB = full(B);
%=============================================================================
% max(A, A') : both sparse, same size -> sparse (adjacency symmetrization idiom)
R = max(A, B);
asserts.istrue(issparse(R));
asserts.isequal(full(R), max(FA, FB));
%=============================================================================
% max(A, 0) : keeps only the positive entries, stays sparse
R = max(A, 0);
asserts.istrue(issparse(R));
asserts.isequal(full(R), max(FA, 0));
%=============================================================================
% max(A, 2) : positive scalar fills the implicit zeros -> full result
R = max(A, 2);
asserts.isfalse(issparse(R));
asserts.isequal(R, max(FA, 2));
%=============================================================================
% max(A, -1) : negative scalar preserves the zeros -> stays sparse
R = max(A, -1);
asserts.istrue(issparse(R));
asserts.isequal(full(R), max(FA, -1));
%=============================================================================
% scalar as first argument commutes
R = max(0, A);
asserts.istrue(issparse(R));
asserts.isequal(full(R), max(0, FA));
%=============================================================================
% max(A, full(B)) : sparse vs full of equal size -> full result
R = max(A, FB);
asserts.isfalse(issparse(R));
asserts.isequal(R, max(FA, FB));
%=============================================================================
% max(full(A), B) : full vs sparse of equal size -> full result
R = max(FA, B);
asserts.isfalse(issparse(R));
asserts.isequal(R, max(FA, FB));
%=============================================================================
% comparison against the dense equivalent, keeping the result sparse
C = sparse([1 2 3 4], [1 2 3 4], [-3 5 -1 2], 4, 4);
FC = full(C);
R = max(C, C');
asserts.istrue(issparse(R));
asserts.isequal(full(R), max(FC, FC'));
%=============================================================================
% 'omitnan' flag form on two sparse arrays
R = max(A, B, 'omitnan');
asserts.isequal(full(R), max(FA, FB));
%=============================================================================
% NaN handling: explicit stored NaN, default omitnan and includenan
N = sparse([1 2 3], [1 2 3], [NaN 4 -2], 3, 3);
FN = full(N);
R = max(N, N');
asserts.isequal(full(R), max(FN, FN'));
R = max(N, N', 'includenan');
asserts.isequal(full(R), max(FN, FN', 'includenan'));
%=============================================================================
% single precision sparse keeps its class
S = sparse(single([0 2; -1 0]));
R = max(S, S');
asserts.isequal(class(R), 'single');
asserts.isequal(full(R), max(full(S), full(S)'));
%=============================================================================
% logical sparse
L1 = sparse(logical([1 0 1 0]));
L2 = sparse(logical([0 0 1 1]));
R = max(L1, L2);
asserts.isequal(full(R), max(full(L1), full(L2)));
%=============================================================================
