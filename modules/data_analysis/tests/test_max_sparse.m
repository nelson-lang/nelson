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
% max(A, 2) : positive scalar fills the implicit zeros, result still sparse
R = max(A, 2);
asserts.istrue(issparse(R));
asserts.isequal(full(R), max(FA, 2));
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
% max(A, full(B)) : sparse vs full of equal size -> sparse result
R = max(A, FB);
asserts.istrue(issparse(R));
asserts.isequal(full(R), max(FA, FB));
%=============================================================================
% max(full(A), B) : full vs sparse of equal size -> sparse result
R = max(FA, B);
asserts.istrue(issparse(R));
asserts.isequal(full(R), max(FA, FB));
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
% any sparse operand gives a sparse result
R = max(sparse(66), [65 67]);
asserts.istrue(issparse(R));
asserts.isequal(full(R), [66 67]);
R = max([1 3], sparse(2));
asserts.istrue(issparse(R));
asserts.isequal(full(R), [2 3]);
R = max(sparse(2), 3);
asserts.istrue(issparse(R));
asserts.isequal(full(R), 3);
R = max(sparse([1 0]), [0; 2]);
asserts.istrue(issparse(R));
asserts.isequal(full(R), [1 0; 2 2]);
R = max(A, [1 2 3 4]);
asserts.istrue(issparse(R));
asserts.isequal(full(R), max(FA, [1 2 3 4]));
R = max(A, NaN, 'includenan');
asserts.istrue(issparse(R));
asserts.isequal(full(R), NaN(4, 4));
R = max(sparse(true), [false true]);
asserts.istrue(issparse(R));
asserts.isequal(full(R), [true true]);
R = max(single([1 2]), sparse(66));
asserts.istrue(issparse(R));
asserts.isequal(full(R), single([66 66]));
R = max(sparse(1i), [1 2]);
asserts.istrue(issparse(R));
asserts.isequal(full(R), [1i 2]);
assert_checkerror('max(A, zeros(4, 4, 2))', 'Cannot make n-dimensional arrays sparse.');
%=============================================================================
% sparse against a scalar matches the dense result for every flag and side
S = sparse([0 NaN 0; -3 0 4]);
flags = {'omitnan', 'includenan'};
scalars = [0 2 -2 NaN];
for f = 1:numel(flags)
  for k = 1:numel(scalars)
    R = max(S, scalars(k), flags{f});
    asserts.istrue(issparse(R));
    asserts.isequal(full(R), max(full(S), scalars(k), flags{f}));
    R = max(scalars(k), S, flags{f});
    asserts.istrue(issparse(R));
    asserts.isequal(full(R), max(scalars(k), full(S), flags{f}));
  end
end
%=============================================================================
