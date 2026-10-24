%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('islocalmin'), -2);
asserts.isequal(nargout('islocalmin'), 2);
%=============================================================================
[TF, P] = islocalmin([0 2 1 2 0]);
asserts.isequal(TF, [false false true false false]);
asserts.isequal(P, [0 0 1 0 0]);
[TF, P] = islocalmin([0 5 1 3 1 4 0]);
asserts.isequal(TF, [false false true false true false false]);
asserts.isequal(P, [0 0 3 0 3 0 0]);
asserts.isequal(islocalmin([0 5 1 3 1 4 0], 'MinSeparation', 4), [false false true false false false false]);
asserts.isequal(islocalmin([0 9 1 3 0 4 0 3 1 9 0], 'MaxNumExtrema', 3), [false false true false true false true false false false false]);
%=============================================================================
% flat regions
A = [5 2 2 2 2 5 4 5];
[TF, P] = islocalmin(A);
asserts.isequal(TF, [false false true false false false true false]);
asserts.isequal(P, [0 3 3 3 3 0 1 0]);
asserts.isequal(islocalmin(A, 'FlatSelection', 'first'), [false true false false false false true false]);
asserts.isequal(islocalmin(A, 'FlatSelection', 'last'), [false false false false true false true false]);
asserts.isequal(islocalmin(A, 'FlatSelection', 'all'), [false true true true true false true false]);
%=============================================================================
% -Inf is always a local minimum
[TF, P] = islocalmin([-Inf 1 -Inf]);
asserts.isequal(TF, [true false true]);
asserts.isequal(P, [Inf 0 Inf]);
[TF, P] = islocalmin([1 -Inf 2 -Inf 1]);
asserts.isequal(TF, [false true false true false]);
asserts.isequal(P, [0 Inf 0 Inf 0]);
[TF, P] = islocalmin([Inf 1 2 1 Inf]);
asserts.isequal(TF, [false true false true false]);
asserts.isequal(P, [0 Inf 0 Inf 0]);
%=============================================================================
% NaN values are ignored
asserts.isequal(islocalmin([NaN 1 NaN 3 NaN 1 NaN]), false(1, 7));
[TF, P] = islocalmin([1 NaN 3 NaN 1 NaN 3 NaN 1], 'MinSeparation', 3);
asserts.isequal(TF, [false false false false true false false false false]);
asserts.isequal(P, [0 0 0 0 2 0 0 0 0]);
%=============================================================================
% classes
[TF, P] = islocalmin(int8([0 100 -100 120 -128 5 0]));
asserts.isequal(TF, [false false true false true false false]);
asserts.isequal(P, uint8([0 0 200 0 133 0 0]));
[TF, P] = islocalmin(uint8([0 200 1 255 0 5 0]));
asserts.isequal(TF, [false false true false true false false]);
asserts.isequal(P, uint8([0 0 199 0 5 0 0]));
[TF, P] = islocalmin([int64(0), intmax('int64'), intmin('int64'), int64(5), int64(0)]);
asserts.isequal(TF, [false false true false false]);
asserts.isequal(P, [uint64(0), uint64(0), uint64(intmax('int64')) + 6, uint64(0), uint64(0)]);
[TF, P] = islocalmin([int64(5), intmin('int64'), int64(5), int64(-3), int64(4)]);
asserts.isequal(TF, [false true false true false]);
asserts.isequal(P, [uint64(0), uint64(intmax('int64')) + 6, uint64(0), uint64(7), uint64(0)]);
[TF, P] = islocalmin([uint64(0), intmax('uint64'), uint64(0), uint64(7), uint64(3)]);
asserts.isequal(TF, [false false true false false]);
asserts.isequal(P, uint64([0 0 7 0 0]));
[~, P] = islocalmin(int8([1 -128 2]), 'ProminenceWindow', 1);
asserts.isequal(P, uint8([0 127 0]));
[~, P] = islocalmin([int64(1), intmin('int64'), int64(2)], 'ProminenceWindow', 1);
asserts.isequal(P, [uint64(0), uint64(intmax('int64')), uint64(0)]);
[TF, P] = islocalmin(single([0 1e-8 0 3 NaN 3 0]));
asserts.isequal(TF, [false false true false false false false]);
asserts.isequal(P, single([0 0 1e-8 0 0 0 0]));
[TF, P] = islocalmin(logical([0 1 0 1 1 0]));
asserts.isequal(TF, [false false true false false false]);
asserts.isequal(P, logical([0 0 1 0 0 0]));
%=============================================================================
% matrices, sparse and tables
A = [1 2 1; 3 1 3; 1 2 1];
[TF, P] = islocalmin(A);
asserts.isequal(TF, logical([0 0 0; 0 1 0; 0 0 0]));
asserts.isequal(P, [0 0 0; 0 1 0; 0 0 0]);
[TF, P] = islocalmin(A, 2);
asserts.isequal(TF, logical([0 0 0; 0 1 0; 0 0 0]));
asserts.isequal(P, [0 0 0; 0 2 0; 0 0 0]);
[TF, P] = islocalmin(sparse([1 0 2 0 1]));
asserts.istrue(issparse(TF));
asserts.isequal(full(TF), [false true false true false]);
asserts.isequal(full(P), [0 1 0 1 0]);
[TF, P] = islocalmin(table([1; 3; 1; 2; 1], [5; 1; 5; 0; 5]));
asserts.isequal(TF, logical([0 0; 0 1; 1 0; 0 1; 0 0]));
asserts.isequal(P.Var1, [0; 0; 1; 0; 0]);
asserts.isequal(P.Var2, [0; 4; 0; 5; 0]);
%=============================================================================
% documented example
x = 0:0.1:5;
A = max(-0.75, -sin(pi * x));
asserts.isequal(find(islocalmin(A, 'FlatSelection', 'first')), [4 24 44]);
A = -peaks(100);
A = A(50, :);
[TF, P] = islocalmin(A);
asserts.isapprox(P(TF), [1.770303986244544 3.5547737767075871], 1e-12);
asserts.isequal(find(islocalmin(A, 'MinProminence', 2)), 72);
%=============================================================================
assert_checkerror('islocalmin({1, 2})', 'First argument must be a numeric or logical array, a table, or a timetable.', 'Nelson:isLocalExtrema:FirstInputInvalid');
assert_checkerror('islocalmin([1 2 1], ''MaxNumExtrema'', 0)', '''MaxNumExtrema'' value must be a positive integer.', 'Nelson:isLocalExtrema:MaxNumInvalid');
%=============================================================================
