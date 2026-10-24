%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Two-argument (element-wise) min with sparse operands.
%=============================================================================
A = sparse([1 1 2], [2 3 4], [1 4 1], 4, 4);
B = A';
FA = full(A);
FB = full(B);
%=============================================================================
% min(A, A') : both sparse, same size -> sparse
R = min(A, B);
asserts.istrue(issparse(R));
asserts.isequal(full(R), min(FA, FB));
%=============================================================================
% min(A, 0) : keeps only the negative entries, stays sparse
R = min(A, 0);
asserts.istrue(issparse(R));
asserts.isequal(full(R), min(FA, 0));
%=============================================================================
% min(A, -2) : negative scalar fills the implicit zeros -> full result
R = min(A, -2);
asserts.isfalse(issparse(R));
asserts.isequal(R, min(FA, -2));
%=============================================================================
% min(A, 2) : positive scalar preserves the zeros -> stays sparse
R = min(A, 2);
asserts.istrue(issparse(R));
asserts.isequal(full(R), min(FA, 2));
%=============================================================================
% scalar as first argument commutes
R = min(0, A);
asserts.istrue(issparse(R));
asserts.isequal(full(R), min(0, FA));
%=============================================================================
% min(A, full(B)) : sparse vs full of equal size -> full result
R = min(A, FB);
asserts.isfalse(issparse(R));
asserts.isequal(R, min(FA, FB));
%=============================================================================
% comparison against the dense equivalent, keeping the result sparse
C = sparse([1 2 3 4], [1 2 3 4], [-3 5 -1 2], 4, 4);
FC = full(C);
R = min(C, C');
asserts.istrue(issparse(R));
asserts.isequal(full(R), min(FC, FC'));
%=============================================================================
% 'omitnan' flag form on two sparse arrays
R = min(A, B, 'omitnan');
asserts.isequal(full(R), min(FA, FB));
%=============================================================================
% NaN handling: explicit stored NaN, default omitnan and includenan
N = sparse([1 2 3], [1 2 3], [NaN 4 -2], 3, 3);
FN = full(N);
R = min(N, N');
asserts.isequal(full(R), min(FN, FN'));
R = min(N, N', 'includenan');
asserts.isequal(full(R), min(FN, FN', 'includenan'));
%=============================================================================
% single precision sparse keeps its class
S = sparse(single([0 2; -1 0]));
R = min(S, S');
asserts.isequal(class(R), 'single');
asserts.isequal(full(R), min(full(S), full(S)'));
%=============================================================================
% logical sparse
L1 = sparse(logical([1 0 1 0]));
L2 = sparse(logical([0 0 1 1]));
R = min(L1, L2);
asserts.isequal(full(R), min(full(L1), full(L2)));
%=============================================================================
