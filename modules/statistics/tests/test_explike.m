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
%=============================================================================
asserts.isequal(nargin('explike'), 4);
asserts.isequal(nargout('explike'), 2);
%=============================================================================
x = [0.5 1 2 3 5 8];
[NLOGL, AVAR] = explike(3.25, x);
asserts.isapprox(NLOGL, 13.07192997804988, 1e-12);
asserts.isapprox(AVAR, 1.760416666666667, 1e-12);
%=============================================================================
censor = [0 0 0 1 0 1];
freq = [1 2 1 1 2 1];
[NLOGL, AVAR] = explike(4.125, x, censor, freq);
asserts.isapprox(NLOGL, 14.68421430053805, 1e-12);
asserts.isapprox(AVAR, 2.673883928571429, 1e-12);
%=============================================================================
[NLOGL, AVAR] = explike(single(2), single([1 2 3]));
asserts.isequal(class(NLOGL), 'double');
asserts.isequal(class(AVAR), 'double');
asserts.isapprox(NLOGL, 5.079441541679836, 1e-12);
asserts.isapprox(AVAR, 1.333333333333333, 1e-12);
%=============================================================================
[NLOGL, AVAR] = explike(0, x);
asserts.istrue(isinf(NLOGL));
asserts.istrue(isnan(AVAR));
[NLOGL, AVAR] = explike(NaN, x);
asserts.istrue(isnan(NLOGL));
asserts.istrue(isnan(AVAR));
[NLOGL, AVAR] = explike(1, [1 NaN 3]);
asserts.istrue(isnan(NLOGL));
asserts.istrue(isnan(AVAR));
%=============================================================================
asserts.checkerror('explike(1)', _('Not enough input arguments.'));
asserts.checkerror('explike({1}, [1 2 3])', sprintf(_('%s must be real numeric.'), 'MU'));
asserts.checkerror('explike(1, {1 2 3})', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('explike(1, [])', sprintf(_('%s must be nonempty.'), 'X'));
asserts.checkerror('explike(1, [-1 1 2])', sprintf(_('%s must contain nonnegative finite values.'), 'X'));
asserts.checkerror('explike(1, [1 Inf 2])', sprintf(_('%s must contain nonnegative finite values.'), 'X'));
asserts.checkerror('explike(1, [1 2 3], [0 2 0])', _('CENSORING must contain 0 or 1 values.'));
asserts.checkerror('explike(1, [1 2 3], [], [1 -1 1])', sprintf(_('%s must contain nonnegative finite values.'), 'FREQ'));
asserts.checkerror('explike(1, [1 2 3], ones(2, 2))', _('Size mismatch on input arguments'));
%=============================================================================
