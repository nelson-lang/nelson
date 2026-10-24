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
% <--REUSE PROCESS-->
%=============================================================================
asserts.isequal(nargin('binolike'), 3);
asserts.isequal(nargout('binolike'), 2);
%=============================================================================
x = [0 2 5 8 10];
n = [10 10 10 10 10];
[NLOGL, AVAR] = binolike(0.4, x, n);
asserts.isapprox(NLOGL, 22.53515482395158, 1e-12);
asserts.isapprox(AVAR, 0.0048, 1e-12);
%=============================================================================
[NLOGL, AVAR] = binolike(single(0.5), single([1 2 3]), single([5 5 5]));
asserts.isequal(class(NLOGL), 'double');
asserts.isequal(class(AVAR), 'double');
asserts.isapprox(NLOGL, 4.182599609976989, 1e-12);
asserts.isapprox(AVAR, 0.01666666666666667, 1e-12);
%=============================================================================
[NLOGL, AVAR] = binolike(0, [0 0 0], [2 3 4]);
asserts.isequal(NLOGL, 0);
asserts.isequal(AVAR, 0);
[NLOGL, AVAR] = binolike(1, [2 3 4], [2 3 4]);
asserts.isequal(NLOGL, 0);
asserts.isequal(AVAR, 0);
[NLOGL, AVAR] = binolike(0, [0 1], [2 2]);
asserts.istrue(isinf(NLOGL));
asserts.istrue(isnan(AVAR));
[NLOGL, AVAR] = binolike(1, [1 1], [1 2]);
asserts.istrue(isinf(NLOGL));
asserts.istrue(isnan(AVAR));
[NLOGL, AVAR] = binolike(-0.1, x, n);
asserts.istrue(isinf(NLOGL));
asserts.istrue(isnan(AVAR));
[NLOGL, AVAR] = binolike(1.1, x, n);
asserts.istrue(isinf(NLOGL));
asserts.istrue(isnan(AVAR));
[NLOGL, AVAR] = binolike(NaN, x, n);
asserts.istrue(isnan(NLOGL));
asserts.istrue(isnan(AVAR));
[NLOGL, AVAR] = binolike(0.5, [1 NaN 3], [5 5 5]);
asserts.istrue(isnan(NLOGL));
asserts.istrue(isnan(AVAR));
%=============================================================================
asserts.checkerror('binolike(0.5, [1 2])', _('Not enough input arguments.'));
asserts.checkerror('binolike({0.5}, [1 2], [5 5])', _('P must be a real numeric scalar.'));
asserts.checkerror('binolike(0.5, {1 2}, [5 5])', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('binolike(0.5, [1 2], {5 5})', sprintf(_('%s must be real numeric.'), 'N'));
asserts.checkerror('binolike(0.5, [], [])', sprintf(_('%s must be nonempty.'), 'X and N'));
asserts.checkerror('binolike(0.5, ones(2), ones(3))', _('Size mismatch on input arguments'));
asserts.checkerror('binolike(0.5, [-1 1], [5 5])', sprintf(_('%s must contain nonnegative integer finite values.'), 'X'));
asserts.checkerror('binolike(0.5, [1.5 2], [5 5])', sprintf(_('%s must contain nonnegative integer finite values.'), 'X'));
asserts.checkerror('binolike(0.5, [1 Inf], [5 5])', sprintf(_('%s must contain nonnegative integer finite values.'), 'X'));
asserts.checkerror('binolike(0.5, [1 2], [-5 5])', sprintf(_('%s must contain nonnegative integer finite values.'), 'N'));
asserts.checkerror('binolike(0.5, [1 2], [5.5 5])', sprintf(_('%s must contain nonnegative integer finite values.'), 'N'));
asserts.checkerror('binolike(0.5, [1 2], [Inf 5])', sprintf(_('%s must contain nonnegative integer finite values.'), 'N'));
asserts.checkerror('binolike(0.5, [6 2], [5 5])', _('X must not exceed N.'));
%=============================================================================
