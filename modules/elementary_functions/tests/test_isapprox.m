%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
% <--WEIGHT 4-->
asserts.checkerror('isapprox()', _('Not enough input arguments.'));
asserts.checkerror('isapprox(1)', _('Not enough input arguments.'));
asserts.checkerror('isapprox([1, 1])', _('Not enough input arguments.'));
%=============================================================================
asserts.isequal(nargin('isapprox'), -2);
asserts.isequal(nargout('isapprox'), 1);
%=============================================================================
% element-wise result with implicit expansion
asserts.isequal(isapprox(1, 1), true);
asserts.isequal(isapprox([1 2], [1 3]), [true false]);
asserts.isequal(isapprox(1, [1 2; 1 3]), [true false; true false]);
asserts.isequal(isapprox([1; 2], [1 2]), [true false; false true]);
asserts.isequal(isapprox([], []), logical([]));
asserts.isequal(isapprox([], 1), logical([]));
asserts.isequal(size(isapprox(zeros(0, 3), 1)), [0 3]);
asserts.isequal(size(isapprox(ones(2, 1, 2), ones(1, 3))), [2 3 2]);
asserts.checkerror('isapprox([1 2], [1 2 3])', _('Arrays have incompatible sizes for this operation.'));
%=============================================================================
% special values
asserts.isequal(isapprox([NaN 1], [NaN 1]), [false true]);
asserts.isequal(isapprox([Inf -Inf Inf], [Inf -Inf -Inf]), [true true false]);
asserts.isfalse(isapprox(Inf, 1e308));
asserts.isfalse(isapprox(Inf, 1e308, 'RelativeTolerance', 0.5));
asserts.isfalse(isapprox(realmax, -realmax, 'RelativeTolerance', 0.99));
asserts.istrue(isapprox(complex(1, Inf), complex(1, Inf)));
asserts.isfalse(isapprox(complex(Inf, 1), complex(Inf, 2)));
asserts.isfalse(isapprox(complex(Inf, NaN), complex(Inf, NaN)));
%=============================================================================
% default tolerances (verytight)
asserts.isfalse(isapprox(1, 1 + 1e-13));
asserts.isfalse(isapprox(1, 1 + 1e-11));
asserts.istrue(isapprox(0, 1e-300));
asserts.istrue(isapprox(0, 1e-15));
asserts.isfalse(isapprox(0, 1.0000000000000002e-15));
asserts.isfalse(isapprox(single(1), single(1 + 1e-6)));
asserts.isfalse(isapprox(single(1), single(1 + 1e-4)));
asserts.istrue(isapprox(single(0), single(5e-7)));
asserts.isfalse(isapprox(single(0), single(5e-7) + eps(single(5e-7))));
asserts.istrue(isapprox(pi, single(pi)));
asserts.isfalse(isapprox(pi, double(single(pi))));
%=============================================================================
% tolerance levels
asserts.istrue(isapprox(1, 1 + 1e-14, 'tight'));
asserts.istrue(isapprox(1, 1 + 1e-9, 'loose'));
asserts.isfalse(isapprox(1, 1.01, 'loose'));
asserts.istrue(isapprox(1, 1.0009, 'veryloose'));
asserts.isfalse(isapprox(1, 1.0011, 'veryloose'));
asserts.istrue(isapprox(0, 1e-12, 'tight'));
asserts.isfalse(isapprox(0, 1.0000000001e-12, 'tight'));
asserts.istrue(isapprox(0, 1e-8, 'loose'));
asserts.isfalse(isapprox(0, 1.0000000001e-8, 'loose'));
asserts.istrue(isapprox(0, 1e-3, 'veryloose'));
asserts.isfalse(isapprox(0, 1.0000000001e-3, 'veryloose'));
asserts.istrue(isapprox(single(0), 1e-6, 'tight'));
asserts.isfalse(isapprox(single(0), 1.0000000001e-6, 'tight'));
asserts.istrue(isapprox(single(0), 1e-4, 'loose'));
asserts.isfalse(isapprox(single(0), 1.0000000001e-4, 'loose'));
asserts.istrue(isapprox(single(0), 1e-2, 'veryloose'));
asserts.isfalse(isapprox(single(0), 1.0000000001e-2, 'veryloose'));
asserts.istrue(isapprox(1, 1 + 1e-9, 'LOOSE'));
asserts.istrue(isapprox(1, 1 + 1e-9, "loose"));
asserts.istrue(isapprox(1, 1 + 1e-9, 'lo'));
asserts.isfalse(isapprox(1, 2, 't'));
%=============================================================================
% named tolerances
asserts.istrue(isapprox(1, 1.5, 'AbsoluteTolerance', 0.6));
asserts.istrue(isapprox(100, 101, 'RelativeTolerance', 0.02));
asserts.isfalse(isapprox(1e6, 1e6 * (1 + 5e-16), 'AbsoluteTolerance', 0));
asserts.isfalse(isapprox(0, 5e-16, 'RelativeTolerance', 0));
asserts.istrue(isapprox(1, 1 + 1e-9, 'AbsoluteTolerance', 0, 'RelativeTolerance', 1e-8));
asserts.istrue(isapprox(1, 2, 'AbsoluteTolerance', 0.5, 'AbsoluteTolerance', 2));
asserts.istrue(isapprox(1, 2, 'absolutetolerance', 2));
asserts.istrue(isapprox(1, 2, 'Abs', 2));
asserts.istrue(isapprox(1, 2, 'A', 1));
asserts.istrue(isapprox(1, 2, 'R', 0.6));
asserts.istrue(isapprox(1, 2, "AbsoluteTolerance", 2));
asserts.istrue(isapprox(1, 2, 'AbsoluteTolerance', single(2)));
asserts.istrue(isapprox(1, 2, 'RelativeTolerance', 0.99));
asserts.isfalse(isapprox(1, 3, 'RelativeTolerance', 0.5));
asserts.istrue(isapprox(1, 1 + 2^-40, 'AbsoluteTolerance', 2^-40, 'RelativeTolerance', 0));
asserts.isfalse(isapprox(1, 1 + 2^-40, 'AbsoluteTolerance', 2^-40 * (1 - 2^-50), 'RelativeTolerance', 0));
asserts.isfalse(isapprox(1, 1 + eps, 'AbsoluteTolerance', 0, 'RelativeTolerance', eps / 2));
asserts.istrue(isapprox(1, 1, 'AbsoluteTolerance', 0, 'RelativeTolerance', 0));
asserts.istrue(isapprox(0, -0, 'AbsoluteTolerance', 0, 'RelativeTolerance', 0));
asserts.isfalse(isapprox(single(0), single(1e-3), 'AbsoluteTolerance', 1e-3, 'RelativeTolerance', 0));
asserts.istrue(isapprox(single(0), single(1e-3), 'AbsoluteTolerance', single(1e-3), 'RelativeTolerance', 0));
asserts.isfalse(isapprox(single(1), 1 + 1e-12, 'AbsoluteTolerance', 0, 'RelativeTolerance', 1e-14));
%=============================================================================
% tolerance arrays expand like the inputs
asserts.isequal(isapprox(1, 2, 'AbsoluteTolerance', [1 2]), [true true]);
asserts.isequal(isapprox([1 2], [1 2.5], 'AbsoluteTolerance', [0.1; 1]), [true false; true true]);
asserts.isequal(isapprox([1 2], [1 2.5], 'RelativeTolerance', [0; 0.5]), [true false; true true]);
asserts.istrue(isapprox(1, 1, 'AbsoluteTolerance', sparse(1)));
msg = _('Arrays have incompatible sizes for this operation.');
asserts.checkerror('isapprox([1 2], [1 2.5], ''AbsoluteTolerance'', [0.1 1 2])', msg);
%=============================================================================
% integers, mixed classes and complex values
asserts.isfalse(isapprox(int8(3), int8(4)));
asserts.istrue(isapprox(int8(3), int16(3)));
asserts.istrue(isapprox(int8(3), single(3)));
asserts.isfalse(isapprox(int8(3), 3.0000001));
asserts.istrue(isapprox(int8(100), int8(101), 'RelativeTolerance', 0.01));
asserts.isfalse(isapprox(int8(100), int8(102), 'RelativeTolerance', 0.01));
asserts.isfalse(isapprox(uint8(3), uint8(5), 'AbsoluteTolerance', 1.9));
asserts.istrue(isapprox(uint8(5), uint8(3), 'AbsoluteTolerance', 2));
asserts.istrue(isapprox(int32(5), int32(6), 'AbsoluteTolerance', 1));
asserts.istrue(isapprox(int8(100), single(100.00004)));
asserts.isfalse(isapprox(int8(100), 100.00004));
asserts.istrue(isapprox(single(1), 1 + 4e-7));
asserts.isfalse(isapprox(single(1), 1 + 1e-6));
asserts.istrue(isapprox(int64(2^53), int64(2^53)));
asserts.isfalse(isapprox(-int64(2^53), 1));
asserts.isfalse(isapprox(2^60, int64(1)));
asserts.isequal(size(isapprox(int64(zeros(1, 0)), 1)), [1 0]);
msg = _('int64 and uint64 values above flintmax cannot be compared exactly.');
asserts.checkerror('isapprox(int64(2^53) + 1, int64(2^53) + 1)', msg);
asserts.checkerror('isapprox(-int64(2^53) - 1, 1)', msg);
asserts.checkerror('isapprox(uint64(2^53) + 1, 1)', msg);
asserts.checkerror('isapprox(int64(2^62), 1, ''Foo'', 1)', msg);
asserts.istrue(isapprox(1 + 1i, 1 + 1i + 1e-16));
asserts.istrue(isapprox(single(1) + 1i, 1 + 1i));
asserts.isfalse(isapprox(1i, 1i * (1 + 1e-15), 'AbsoluteTolerance', 0));
R = isapprox(3 + 4i, 3 + 4i + [0.5 0.5i 0.4+0.3i 0.4+0.31i], 'AbsoluteTolerance', 0.5, 'RelativeTolerance', 0);
asserts.isequal(R, [true true true false]);
R = isapprox(100, 100 + [1 2 3] * (1 + 1i), 'AbsoluteTolerance', 0, 'RelativeTolerance', 0.0142);
asserts.isequal(R, [true false false]);
%=============================================================================
% sparse result only when both inputs are sparse
R = isapprox(sparse([1 0 2]), sparse([1 0 2.1]), 'AbsoluteTolerance', 0.2);
asserts.istrue(issparse(R));
asserts.isequal(full(R), [true true true]);
R = isapprox(sparse([1 0 2]), [1 0 3]);
asserts.isfalse(issparse(R));
asserts.isequal(R, [true true false]);
R = isapprox(sparse([1 0 2]), sparse([1; 0]));
asserts.istrue(issparse(R));
asserts.isequal(full(R), [true false false; false true false]);
asserts.isfalse(issparse(isapprox(2, sparse([1 0 2]))));
%=============================================================================
% input errors
msg = _('The first two inputs must be numeric arrays.');
asserts.checkerror('isapprox(1, ''A'')', msg);
asserts.checkerror('isapprox(''a'', 97)', msg);
asserts.checkerror('isapprox(true, 1)', msg);
asserts.checkerror('isapprox("a", "a")', msg);
msg = _('Tolerance level expected: ''verytight'', ''tight'', ''loose'' or ''veryloose''.');
asserts.checkerror('isapprox(1, 2, 1e-3)', msg);
asserts.checkerror('isapprox(1, 2, ''v'')', msg);
asserts.checkerror('isapprox(1, 2, ''very'')', msg);
asserts.checkerror('isapprox(1, 2, '''')', msg);
asserts.checkerror('isapprox(1, 2, ''AbsoluteTolerance'')', msg);
asserts.checkerror('isapprox(1, 2, ["loose" "tight"])', msg);
asserts.checkerror('isapprox(1, 2, {''loose''})', msg);
msg = _('Parameter name expected: ''AbsoluteTolerance'' or ''RelativeTolerance''.');
asserts.checkerror('isapprox(1, 2, ''Foo'', 1)', msg);
asserts.checkerror('isapprox(1, 2, ''loose'', ''tight'')', msg);
asserts.checkerror('isapprox(1, 2, 3, 4)', msg);
asserts.checkerror('isapprox(1, 2, '''', 1)', msg);
msg = _('A tolerance level cannot be combined with AbsoluteTolerance or RelativeTolerance.');
asserts.checkerror('isapprox(1, 2, ''loose'', ''AbsoluteTolerance'', 1, ''RelativeTolerance'', 0)', msg);
asserts.checkerror('isapprox(1, 2, ''loose'', 3, 4)', msg);
asserts.checkerror('isapprox(1, 2, ''t'', ''Abs'', 1)', msg);
msg = _('Each parameter name must be followed by its value.');
asserts.checkerror('isapprox(1, 2, 3, 4, 5, 6, 7)', msg);
asserts.checkerror('isapprox(1, 2, ''Foo'', 1, 2)', msg);
asserts.isfalse(isapprox(1, 2, 'Abs', 1, 'Rel', 0, 'Abs', 0));
msg = _('AbsoluteTolerance must be a nonnegative finite real double or single value.');
asserts.checkerror('isapprox(1, 2, ''AbsoluteTolerance'', -1)', msg);
asserts.checkerror('isapprox(1, 2, ''AbsoluteTolerance'', NaN)', msg);
asserts.checkerror('isapprox(1, 2, ''AbsoluteTolerance'', Inf)', msg);
asserts.checkerror('isapprox(1, 2, ''AbsoluteTolerance'', int8(2))', msg);
asserts.checkerror('isapprox(1, 2, ''AbsoluteTolerance'', ''a'')', msg);
asserts.checkerror('isapprox(1, 2, ''AbsoluteTolerance'', true)', msg);
asserts.checkerror('isapprox(1, 2, ''AbsoluteTolerance'', 1 + 2i)', msg);
asserts.checkerror('isapprox(1, 2, ''AbsoluteTolerance'', [])', msg);
asserts.checkerror('isapprox(1, 1, ''AbsoluteTolerance'', [0 NaN])', msg);
msg = _('RelativeTolerance must be a nonnegative finite real double or single value smaller than 1.');
asserts.checkerror('isapprox(1, 2, ''RelativeTolerance'', 1)', msg);
asserts.checkerror('isapprox(1, 2, ''RelativeTolerance'', 2)', msg);
asserts.checkerror('isapprox(1, 1, ''RelativeTolerance'', [0.5 1])', msg);
%=============================================================================
