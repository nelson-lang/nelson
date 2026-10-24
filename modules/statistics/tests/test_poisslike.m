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
asserts.isequal(nargin('poisslike'), 2);
asserts.isequal(nargout('poisslike'), 2);
%=============================================================================
x = [0 1 2 3 5 8];
[NLOGL, AVAR] = poisslike(3, x);
asserts.isapprox(NLOGL, 15.00336781062121, 1e-12);
asserts.isapprox(AVAR, 0.5, 1e-12);
%=============================================================================
[NLOGL, AVAR] = poisslike(single(2), single([1 2 3]));
asserts.isequal(class(NLOGL), 'double');
asserts.isequal(class(AVAR), 'double');
asserts.isapprox(NLOGL, 4.326023566428328, 1e-12);
asserts.isapprox(AVAR, 0.6666666666666666, 1e-12);
%=============================================================================
[NLOGL, AVAR] = poisslike(0, [0 0 0]);
asserts.isequal(NLOGL, 0);
asserts.isequal(AVAR, 0);
[NLOGL, AVAR] = poisslike(0, [0 1]);
asserts.istrue(isinf(NLOGL));
asserts.istrue(isnan(AVAR));
[NLOGL, AVAR] = poisslike(NaN, x);
asserts.istrue(isnan(NLOGL));
asserts.istrue(isnan(AVAR));
[NLOGL, AVAR] = poisslike(1, [1 NaN 3]);
asserts.istrue(isnan(NLOGL));
asserts.istrue(isnan(AVAR));
%=============================================================================
asserts.checkerror('poisslike(1)', _('Not enough input arguments.'));
asserts.checkerror('poisslike({1}, [1 2 3])', sprintf(_('%s must be real numeric.'), 'LAMBDA'));
asserts.checkerror('poisslike(1, {1 2 3})', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('poisslike(1, [])', sprintf(_('%s must be nonempty.'), 'X'));
asserts.checkerror('poisslike(1, [-1 1 2])', sprintf(_('%s must contain nonnegative integer finite values.'), 'X'));
asserts.checkerror('poisslike(1, [1.5 2 3])', sprintf(_('%s must contain nonnegative integer finite values.'), 'X'));
asserts.checkerror('poisslike(1, [1 Inf 2])', sprintf(_('%s must contain nonnegative integer finite values.'), 'X'));
%=============================================================================