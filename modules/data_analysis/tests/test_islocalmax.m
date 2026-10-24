%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('islocalmax'), -2);
asserts.isequal(nargout('islocalmax'), 2);
%=============================================================================
% strict peaks, equal heights do not stop the prominence line
[TF, P] = islocalmax([0 2 1 2 0]);
asserts.isequal(TF, [false true false true false]);
asserts.isequal(P, [0 2 0 2 0]);
[TF, P] = islocalmax([0 3 1 2 1 3 0]);
asserts.isequal(TF, [false true false true false true false]);
asserts.isequal(P, [0 3 0 1 0 3 0]);
[TF, P] = islocalmax([0 5 1 3 1 4 0]);
asserts.isequal(TF, [false true false true false true false]);
asserts.isequal(P, [0 5 0 2 0 3 0]);
%=============================================================================
% flat regions: every element of the plateau carries the prominence
[TF, P] = islocalmax([0 2 2 2 0 1 0]);
asserts.isequal(TF, [false false true false false true false]);
asserts.isequal(P, [0 2 2 2 0 1 0]);
A = [0 2 2 2 2 0 1 0];
[TF, P] = islocalmax(A);
asserts.isequal(TF, [false false true false false false true false]);
asserts.isequal(P, [0 2 2 2 2 0 1 0]);
asserts.isequal(islocalmax(A, 'FlatSelection', 'first'), [false true false false false false true false]);
asserts.isequal(islocalmax(A, 'FlatSelection', 'last'), [false false false false true false true false]);
asserts.isequal(islocalmax(A, 'FlatSelection', 'all'), [false true true true true false true false]);
asserts.isequal(islocalmax(A, 'flatselection', "FIRST"), islocalmax(A, 'FlatSelection', 'first'));
asserts.isequal(islocalmax(A, 'Flat', 'l'), islocalmax(A, 'FlatSelection', 'last'));
asserts.isequal(islocalmax([5 5 5 5]), false(1, 4));
%=============================================================================
% endpoints are never extrema
asserts.isequal(islocalmax([3 1 2]), [false false false]);
asserts.isequal(islocalmax([1 2]), [false false]);
asserts.isequal(islocalmax(7), false);
asserts.isequal(islocalmax([2 2 1 3 3]), false(1, 5));
%=============================================================================
% NaN values are ignored
[TF, P] = islocalmax([0 2 NaN 2 0 1 0]);
asserts.isequal(TF, [false true false false false true false]);
asserts.isequal(P, [0 2 0 2 0 1 0]);
[TF, P] = islocalmax([NaN 1 NaN 3 NaN 1 NaN]);
asserts.isequal(TF, [false false false true false false false]);
asserts.isequal(P, [0 0 0 2 0 0 0]);
asserts.isequal(islocalmax([3 NaN NaN NaN 1]), false(1, 5));
asserts.isequal(islocalmax(NaN(1, 5)), false(1, 5));
%=============================================================================
% +Inf is always a local maximum, even at an endpoint or repeated
[TF, P] = islocalmax([1 Inf 1]);
asserts.isequal(TF, [false true false]);
asserts.isequal(P, [0 Inf 0]);
[TF, P] = islocalmax([Inf 1 2 1 Inf]);
asserts.isequal(TF, [true false true false true]);
asserts.isequal(P, [Inf 0 1 0 Inf]);
[TF, P] = islocalmax([1 Inf Inf 1 3 1]);
asserts.isequal(TF, [false true true false true false]);
asserts.isequal(P, [0 Inf Inf 0 2 0]);
[TF, P] = islocalmax([1 2 3 Inf Inf Inf 3 2 1]);
asserts.isequal(TF, [false false false true true true false false false]);
asserts.isequal(P, [0 0 0 Inf Inf Inf 0 0 0]);
[TF, P] = islocalmax([-Inf 1 -Inf]);
asserts.isequal(TF, [false true false]);
asserts.isequal(P, [0 Inf 0]);
asserts.isequal(islocalmax([Inf 1]), [false false]);
%=============================================================================
% classes: prominence of integers is unsigned
[TF, P] = islocalmax(int8([0 100 -100 120 -128 5 0]));
asserts.isequal(TF, [false true false true false true false]);
asserts.isequal(P, uint8([0 100 0 220 0 5 0]));
[TF, P] = islocalmax(uint8([0 200 1 255 0 5 0]));
asserts.isequal(TF, [false true false true false true false]);
asserts.isequal(P, uint8([0 199 0 255 0 5 0]));
[TF, P] = islocalmax([int64(0), intmax('int64'), intmin('int64'), int64(5), int64(0)]);
asserts.isequal(TF, [false true false true false]);
asserts.isequal(P, [uint64(0), uint64(intmax('int64')), uint64(0), uint64(5), uint64(0)]);
[TF, P] = islocalmax([uint64(0), intmax('uint64'), uint64(0), uint64(7), uint64(3)]);
asserts.isequal(TF, [false true false true false]);
asserts.isequal(P, [uint64(0), intmax('uint64'), uint64(0), uint64(4), uint64(0)]);
[TF, P] = islocalmax(single([0 2 1 3 0]));
asserts.isequal(TF, [false true false true false]);
asserts.isequal(P, single([0 1 0 3 0]));
[TF, P] = islocalmax(logical([0 1 0 1 1 0]));
asserts.isequal(TF, [false true false true false false]);
asserts.isequal(P, logical([0 1 0 1 1 0]));
%=============================================================================
% empty and scalar inputs
[TF, P] = islocalmax([]);
asserts.isequal(TF, false(0, 0));
asserts.isequal(P, zeros(0, 0));
asserts.isequal(islocalmax(zeros(1, 0)), false(1, 0));
asserts.isequal(islocalmax(zeros(0, 3)), false(0, 3));
asserts.isequal(islocalmax(zeros(3, 0)), false(3, 0));
%=============================================================================
% matrices and N-D arrays
A = [1 2 1; 3 1 3; 1 2 1];
[TF, P] = islocalmax(A);
asserts.isequal(TF, logical([0 0 0; 1 0 1; 0 0 0]));
asserts.isequal(P, [0 0 0; 2 0 2; 0 0 0]);
[TF, P] = islocalmax(A, 2);
asserts.isequal(TF, logical([0 1 0; 0 0 0; 0 1 0]));
asserts.isequal(P, [0 1 0; 0 0 0; 0 1 0]);
asserts.isequal(islocalmax(A, 3), false(3, 3));
asserts.isequal(islocalmax([1 3 1], 4), false(1, 3));
A3 = reshape([1 3 1 2 5 2 0 4 0 1 1 1 3 0 3 2 2 9], 3, 2, 3);
[TF, P] = islocalmax(A3);
asserts.isequal(TF(:)', logical([0 1 0 0 1 0 0 1 0 0 0 0 0 0 0 0 0 0]));
asserts.isequal(P(:)', [0 2 0 0 3 0 0 4 0 0 0 0 0 0 0 0 0 0]);
asserts.isequal(islocalmax(A3, 2), false(3, 2, 3));
[TF, P] = islocalmax(A3, 3);
asserts.isequal(TF(:)', logical([0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0]));
asserts.isequal(P(:)', [0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0]);
%=============================================================================
% sparse input gives sparse outputs
[TF, P] = islocalmax(sparse([0 2 0 3 0]));
asserts.istrue(issparse(TF));
asserts.istrue(issparse(P));
asserts.isequal(full(TF), [false true false true false]);
asserts.isequal(full(P), [0 2 0 3 0]);
%=============================================================================
% documented examples
x = 1:100;
A = (1 - cos(2 * pi * 0.01 * x)) .* sin(2 * pi * 0.15 * x);
asserts.isequal(find(islocalmax(A)), [2 9 15 22 28 35 42 48 55 62 68 75 82 88 95]);
A = 25 * diag(ones(5, 1)) + [0 1 0 2 0; 1 0 2 0 1; 0 2 0 1 0; 1 0 2 0 1; 0 1 0 2 0];
asserts.isequal(islocalmax(A, 2), logical([0 0 0 1 0; 0 1 0 0 0; 0 0 1 0 0; 0 0 0 1 0; 0 1 0 0 0]));
x = 0:0.1:5;
A = min(0.75, sin(pi * x));
asserts.isequal(find(islocalmax(A, 'FlatSelection', 'first')), [4 24 44]);
asserts.isequal(find(islocalmax(A, 'FlatSelection', 'all')), [4:8, 24:28, 44:48]);
A = peaks(100);
A = A(50, :);
[TF1, P] = islocalmax(A);
asserts.isapprox(P(TF1), [1.770303986244544 3.5547737767075871], 1e-12);
asserts.isequal(find(islocalmax(A, 'MinProminence', 2)), 72);
%=============================================================================
% large vector: one extremum per plateau of the staircase
A = repmat([0 1 1 2 2 2 1 0], 1, 1e4);
TF = islocalmax(A);
asserts.isequal(nnz(TF), 1e4);
asserts.isequal(find(TF, 2), [5 13]);
%=============================================================================
