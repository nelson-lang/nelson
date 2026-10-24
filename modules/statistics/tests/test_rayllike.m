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
asserts.isequal(nargin('rayllike'), 4);
asserts.isequal(nargout('rayllike'), 2);
%=============================================================================
x = [0.5 1 2 3 5 8];
[NLOGL, AVAR] = rayllike(2.933285984466340, x);
asserts.isapprox(NLOGL, 14.12598774953680, 1e-12);
asserts.isapprox(AVAR, 0.3585069147814494, 1e-12);
%=============================================================================
censor = [0 0 0 1 0 1];
freq = [1 2 1 1 2 1];
[NLOGL, AVAR] = rayllike(3.281894777919203, x, censor, freq);
asserts.isapprox(NLOGL, 17.04217535673503, 1e-12);
asserts.isapprox(AVAR, 0.4487846850896110, 1e-12);
%=============================================================================
[NLOGL, AVAR] = rayllike(single(2), single([1 2 3]));
asserts.isequal(class(NLOGL), 'double');
asserts.isequal(class(AVAR), 'double');
asserts.isapprox(NLOGL, 4.117123614131617, 1e-12);
asserts.isapprox(AVAR, 0.8888905697715792, 1e-12);
%=============================================================================
[NLOGL, AVAR] = rayllike(0, x);
asserts.istrue(isinf(NLOGL));
asserts.istrue(isnan(AVAR));
[NLOGL, AVAR] = rayllike(NaN, x);
asserts.istrue(isnan(NLOGL));
asserts.istrue(isnan(AVAR));
[NLOGL, AVAR] = rayllike(1, [1 NaN 3]);
asserts.istrue(isnan(NLOGL));
asserts.istrue(isnan(AVAR));
%=============================================================================
asserts.checkerror('rayllike(1)', _('Not enough input arguments.'));
asserts.checkerror('rayllike({1}, [1 2 3])', sprintf(_('%s must be real numeric.'), 'B'));
asserts.checkerror('rayllike(1, {1 2 3})', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('rayllike(1, [])', sprintf(_('%s must be nonempty.'), 'X'));
asserts.checkerror('rayllike(1, [-1 1 2])', sprintf(_('%s must contain nonnegative finite values.'), 'X'));
asserts.checkerror('rayllike(1, [1 Inf 2])', sprintf(_('%s must contain nonnegative finite values.'), 'X'));
asserts.checkerror('rayllike(1, [1 2 3], [0 2 0])', _('CENSORING must contain 0 or 1 values.'));
asserts.checkerror('rayllike(1, [1 2 3], [], [1 -1 1])', sprintf(_('%s must contain nonnegative finite values.'), 'FREQ'));
asserts.checkerror('rayllike(1, [1 2 3], ones(2, 2))', _('Size mismatch on input arguments'));
%=============================================================================
