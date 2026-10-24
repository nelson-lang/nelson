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
asserts.isequal(nargin('unidlike'), 2);
asserts.isequal(nargout('unidlike'), 2);
%=============================================================================
x = [1 2 4 5 5];
[NLOGL, AVAR] = unidlike(5, x);
asserts.isapprox(NLOGL, 8.047189562170502, 1e-12);
asserts.istrue(isnan(AVAR));
%=============================================================================
[NLOGL, AVAR] = unidlike(single(6), single([1 2 6]));
asserts.isequal(class(NLOGL), 'double');
asserts.isequal(class(AVAR), 'double');
asserts.isapprox(NLOGL, 5.375278407684165, 1e-12);
asserts.istrue(isnan(AVAR));
%=============================================================================
[NLOGL, AVAR] = unidlike(3, [1 NaN 2]);
asserts.istrue(isnan(NLOGL));
asserts.istrue(isnan(AVAR));
[NLOGL, AVAR] = unidlike(3, [1 4]);
asserts.istrue(isinf(NLOGL));
asserts.istrue(isnan(AVAR));
[NLOGL, AVAR] = unidlike(0, [1 2]);
asserts.istrue(isinf(NLOGL));
asserts.istrue(isnan(AVAR));
[NLOGL, AVAR] = unidlike(NaN, [1 2]);
asserts.istrue(isnan(NLOGL));
asserts.istrue(isnan(AVAR));
%=============================================================================
asserts.checkerror('unidlike(5)', _('Not enough input arguments.'));
asserts.checkerror('unidlike({5}, [1 2 3])', sprintf(_('%s must be real numeric.'), 'N'));
asserts.checkerror('unidlike(5, {1 2 3})', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('unidlike(5, [])', sprintf(_('%s must be nonempty.'), 'X'));
asserts.checkerror('unidlike(5, [0 1 2])', _('X must contain positive integer finite values.'));
asserts.checkerror('unidlike(5, [-1 1 2])', _('X must contain positive integer finite values.'));
asserts.checkerror('unidlike(5, [1.5 2 3])', _('X must contain positive integer finite values.'));
asserts.checkerror('unidlike(5, [1 Inf 2])', _('X must contain positive integer finite values.'));
%=============================================================================
