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
asserts.isequal(nargin('geolike'), 2);
asserts.isequal(nargout('geolike'), 2);
%=============================================================================
x = [0 1 2 3 5 8];
[NLOGL, AVAR] = geolike(0.25, x);
asserts.isapprox(NLOGL, 13.78372554330318, 1e-12);
asserts.isapprox(AVAR, 0.007705479452054795, 1e-12);
%=============================================================================
[NLOGL, AVAR] = geolike(single(0.5), single([0 1 2]));
asserts.isequal(class(NLOGL), 'double');
asserts.isequal(class(AVAR), 'double');
asserts.isapprox(NLOGL, 4.158883083359671, 1e-12);
asserts.isapprox(AVAR, 0.04166666666666666, 1e-12);
%=============================================================================
[NLOGL, AVAR] = geolike(1, [0 0 0]);
asserts.isequal(NLOGL, 0);
asserts.isequal(AVAR, 0);
[NLOGL, AVAR] = geolike(1, [0 1]);
asserts.istrue(isinf(NLOGL));
asserts.istrue(isnan(AVAR));
[NLOGL, AVAR] = geolike(NaN, x);
asserts.istrue(isnan(NLOGL));
asserts.istrue(isnan(AVAR));
[NLOGL, AVAR] = geolike(0.5, [1 NaN 3]);
asserts.istrue(isnan(NLOGL));
asserts.istrue(isnan(AVAR));
%=============================================================================
asserts.checkerror('geolike(0.5)', _('Not enough input arguments.'));
asserts.checkerror('geolike({0.5}, [1 2 3])', _('P must be a real numeric scalar.'));
asserts.checkerror('geolike(0.5, {1 2 3})', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('geolike(0.5, [])', sprintf(_('%s must be nonempty.'), 'X'));
asserts.checkerror('geolike(0.5, [-1 1 2])', sprintf(_('%s must contain nonnegative integer finite values.'), 'X'));
asserts.checkerror('geolike(0.5, [1.5 2 3])', sprintf(_('%s must contain nonnegative integer finite values.'), 'X'));
asserts.checkerror('geolike(0.5, [1 Inf 2])', sprintf(_('%s must contain nonnegative integer finite values.'), 'X'));
%=============================================================================