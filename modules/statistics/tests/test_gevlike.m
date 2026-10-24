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
asserts.isequal(nargin('gevlike'), 4);
asserts.isequal(nargout('gevlike'), 2);
%=============================================================================
x = [-1.2 -0.4 0.1 0.8 1.5 2.8 4.0];
phat = gevfit(x);
[NLOGL, AVAR] = gevlike(phat, x);
asserts.isapprox(NLOGL, 1.336060645571893e+01, 1e-4);
asserts.isapprox(AVAR, [2.234375455238936e-01 -1.485550041781628e-01 -1.724719755612775e-01;
                      -1.485550041781628e-01 2.702991093058385e-01 1.663147082978266e-01;
                      -1.724719755612775e-01 1.663147082978266e-01 4.491159902513904e-01], 1e-4);
%=============================================================================
censor = [0 0 0 1 0 1 0];
freq = [1 2 1 1 2 1 1];
phat = gevfit(x, 0.05, censor, freq);
[NLOGL, AVAR] = gevlike(phat, x, censor, freq);
asserts.isapprox(NLOGL, 1.506430018753870e+01, 1e-4);
asserts.isapprox(AVAR, [3.812005285494094e-01 2.521479271701395e-02 -1.705756118047981e-01;
                       2.521479271701395e-02 2.916605338966706e-01 2.077539099933857e-01;
                      -1.705756118047981e-01 2.077539099933857e-01 3.783152810464121e-01], 1e-4);
%=============================================================================
[NLOGL, AVAR] = gevlike([0.2 1 0], x);
asserts.isapprox(NLOGL, 1.390499671500907e+01, 1e-4);
asserts.isapprox(AVAR, [1.905080245044483e-01 -6.629319086741366e-02 -2.411532712390118e-01;
                      -6.629319086741366e-02 9.867966136843241e-02 1.624193978759958e-01;
                      -2.411532712390118e-01 1.624193978759958e-01 4.806145145526154e-01], 1e-4);
%=============================================================================
[NLOGL, AVAR] = gevlike(single([0 1 0]), single([-1 0 1]));
asserts.isequal(class(NLOGL), 'double');
asserts.isequal(class(AVAR), 'double');
asserts.isapprox(NLOGL, 4.086161269630487e+00, 1e-4);
asserts.istrue(all(isnan(AVAR(:))));
%=============================================================================
[NLOGL, AVAR] = gevlike([0 0 0], x);
asserts.istrue(isinf(NLOGL));
asserts.istrue(all(isnan(AVAR(:))));
[NLOGL, AVAR] = gevlike([NaN 1 0], x);
asserts.istrue(isnan(NLOGL));
asserts.istrue(all(isnan(AVAR(:))));
[NLOGL, AVAR] = gevlike([0.2 1 0], [1 NaN 3]);
asserts.istrue(isnan(NLOGL));
asserts.istrue(all(isnan(AVAR(:))));
[NLOGL, AVAR] = gevlike([0.2 1 0], -10);
asserts.istrue(isinf(NLOGL));
asserts.istrue(all(isnan(AVAR(:))));
%=============================================================================
asserts.checkerror('gevlike([0 1 0])', _('Not enough input arguments.'));
asserts.checkerror('gevlike([0 1], x)', sprintf(_('PARAMS must be a %s vector.'), 'three-element'));
asserts.checkerror('gevlike({0 1 0}, x)', sprintf(_('%s must be real numeric.'), 'PARAMS'));
asserts.checkerror('gevlike([0 1 0], {})', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('gevlike([0 1 0], [])', sprintf(_('%s must be nonempty.'), 'X'));
asserts.checkerror('gevlike([0 1 0], [1 Inf 2])', sprintf(_('%s must contain finite values.'), 'X'));
asserts.checkerror('gevlike([0 1 0], x, [0 2 0 0 0 0 0])', _('CENSORING must contain 0 or 1 values.'));
asserts.checkerror('gevlike([0 1 0], x, [], [1 -1 1 1 1 1 1])', sprintf(_('%s must contain nonnegative finite values.'), 'FREQ'));
asserts.checkerror('gevlike([0 1 0], x, ones(2, 2))', _('Size mismatch on input arguments'));
%=============================================================================
