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
asserts.isequal(nargin('lognlike'), 4);
asserts.isequal(nargout('lognlike'), 2);
%=============================================================================
x = [0.5 1 2 3 5 8];
params = [0.7979152904636743 0.9365014364963760];
[NLOGL, AVAR] = lognlike(params, x);
asserts.isapprox(NLOGL, 12.90749760280882, 1e-6);
asserts.isapprox(AVAR, [0.1461729287094217 -0.00000005066012583813165;
                       -0.00000005066012583813165 0.07308656356899881], 1e-6);
%=============================================================================
censor = [0 0 0 1 0 1];
freq = [1 2 1 1 2 1];
params = [0.9808752654800201 1.123582749374891];
[NLOGL, AVAR] = lognlike(params, x, censor, freq);
asserts.isapprox(NLOGL, 14.23694973737748, 1e-6);
asserts.isapprox(AVAR, [0.1741283149770794 0.02367443566992538;
                       0.02367443566992538 0.1158765986318134], 1e-6);
%=============================================================================
[NLOGL, AVAR] = lognlike(single([0.5 1.2]), single([1 2 3]));
asserts.isequal(class(NLOGL), 'double');
asserts.isequal(class(AVAR), 'double');
asserts.isapprox(NLOGL, 5.319721266138667, 1e-6);
asserts.istrue(all(isnan(AVAR(:))));
%=============================================================================
[NLOGL, AVAR] = lognlike([0 0], x);
asserts.istrue(isinf(NLOGL));
asserts.istrue(all(isnan(AVAR(:))));
[NLOGL, AVAR] = lognlike([NaN 1], x);
asserts.istrue(isnan(NLOGL));
asserts.istrue(all(isnan(AVAR(:))));
[NLOGL, AVAR] = lognlike([1 1], [1 NaN 3]);
asserts.istrue(isnan(NLOGL));
asserts.istrue(all(isnan(AVAR(:))));
%=============================================================================
asserts.checkerror('lognlike([1 2])', _('Not enough input arguments.'));
asserts.checkerror('lognlike({1 2}, [1 2 3])', sprintf(_('%s must be real numeric.'), 'PARAMS'));
asserts.checkerror('lognlike([1 2 3], [1 2 3])', sprintf(_('PARAMS must be a %s vector.'), 'two-element'));
asserts.checkerror('lognlike([1 2], {1 2 3})', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('lognlike([1 2], [])', sprintf(_('%s must be nonempty.'), 'X'));
asserts.checkerror('lognlike([1 2], [0 1 2])', _('X must contain positive finite values.'));
asserts.checkerror('lognlike([1 2], [1 Inf 2])', _('X must contain positive finite values.'));
asserts.checkerror('lognlike([1 2], [1 2 3], [0 2 0])', _('CENSORING must contain 0 or 1 values.'));
asserts.checkerror('lognlike([1 2], [1 2 3], [], [1 -1 1])', sprintf(_('%s must contain nonnegative finite values.'), 'FREQ'));
asserts.checkerror('lognlike([1 2], [1 2 3], ones(2, 2))', _('Size mismatch on input arguments'));
%=============================================================================
