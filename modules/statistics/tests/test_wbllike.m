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
asserts.isequal(nargin('wbllike'), 4);
asserts.isequal(nargout('wbllike'), 2);
%=============================================================================
x = [0.5 1 2 3 5 8];
params = [3.499523978509226 1.254115126547431];
[NLOGL, AVAR] = wbllike(params, x);
asserts.isapprox(NLOGL, 12.85047491609707, 1e-4);
asserts.isapprox(AVAR, [1.448408251955458 0.1575651189798849;
                       0.1575651189798849 0.1647866320570422], 1e-4);
%=============================================================================
censor = [0 0 0 1 0 1];
freq = [1 2 1 1 2 1];
params = [4.254789570500730 1.060085049217854];
[NLOGL, AVAR] = wbllike(params, x, censor, freq);
asserts.isapprox(NLOGL, 14.66680592544580, 1e-4);
asserts.isapprox(AVAR, [2.686931885272295 0.01615211738513232;
                       0.01615211738513232 0.1267278547640187], 1e-4);
%=============================================================================
[NLOGL, AVAR] = wbllike(single([2 3]), single([1 2 3]));
asserts.isequal(class(NLOGL), 'double');
asserts.isequal(class(AVAR), 'double');
asserts.isapprox(NLOGL, 3.858968820579068, 1e-4);
asserts.isapprox(AVAR, [0.2690474274029275 0.7583276652839760;
                       0.7583276652839760 3.191971278203054], 1e-4);
%=============================================================================
[NLOGL, AVAR] = wbllike([0 1], x);
asserts.istrue(isinf(NLOGL));
asserts.istrue(all(isnan(AVAR(:))));
[NLOGL, AVAR] = wbllike([NaN 1], x);
asserts.istrue(isnan(NLOGL));
asserts.istrue(all(isnan(AVAR(:))));
[NLOGL, AVAR] = wbllike([1 1], [1 NaN 3]);
asserts.istrue(isnan(NLOGL));
asserts.istrue(all(isnan(AVAR(:))));
%=============================================================================
asserts.checkerror('wbllike([1 2])', _('Not enough input arguments.'));
asserts.checkerror('wbllike({1 2}, [1 2 3])', sprintf(_('%s must be real numeric.'), 'PARAMS'));
asserts.checkerror('wbllike([1 2 3], [1 2 3])', sprintf(_('PARAMS must be a %s vector.'), 'two-element'));
asserts.checkerror('wbllike([1 2], {1 2 3})', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('wbllike([1 2], [])', sprintf(_('%s must be nonempty.'), 'X'));
asserts.checkerror('wbllike([1 2], [0 1 2])', _('X must contain positive finite values.'));
asserts.checkerror('wbllike([1 2], [1 Inf 2])', _('X must contain positive finite values.'));
asserts.checkerror('wbllike([1 2], [1 2 3], [0 2 0])', _('CENSORING must contain 0 or 1 values.'));
asserts.checkerror('wbllike([1 2], [1 2 3], [], [1 -1 1])', sprintf(_('%s must contain nonnegative finite values.'), 'FREQ'));
asserts.checkerror('wbllike([1 2], [1 2 3], ones(2, 2))', _('Size mismatch on input arguments'));
%=============================================================================
