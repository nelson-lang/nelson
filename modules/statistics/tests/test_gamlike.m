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
asserts.isequal(nargin('gamlike'), 4);
asserts.isequal(nargout('gamlike'), 2);
%=============================================================================
x = [0.5 1 2 3 5 8];
params = [1.457461695639626 2.229904229883514];
[NLOGL, AVAR] = gamlike(params, x);
asserts.isapprox(NLOGL, 12.83875287654343, 1e-4);
asserts.isapprox(AVAR, [0.5842678900554542 -0.8939240873659653;
                       -0.8939240873659653 1.936317244873048], 1e-4);
%=============================================================================
censor = [0 0 0 1 0 1];
freq = [1 2 1 1 2 1];
params = [1.164302008282069 3.543364968242789];
[NLOGL, AVAR] = gamlike(params, x, censor, freq);
asserts.isapprox(NLOGL, 14.63762457131714, 1e-4);
asserts.isapprox(AVAR, [0.3442145196619660 -1.240714592816863;
                       -1.240714592816863 6.231792452555158], 1e-4);
%=============================================================================
[NLOGL, AVAR] = gamlike(single([2 3]), single([1 2 3]));
asserts.isequal(class(NLOGL), 'double');
asserts.isequal(class(AVAR), 'double');
asserts.isapprox(NLOGL, 6.799914262780604, 1e-4);
asserts.istrue(all(isnan(AVAR(:))));
%=============================================================================
[NLOGL, AVAR] = gamlike([0 1], x);
asserts.istrue(isinf(NLOGL));
asserts.istrue(all(isnan(AVAR(:))));
[NLOGL, AVAR] = gamlike([NaN 1], x);
asserts.istrue(isnan(NLOGL));
asserts.istrue(all(isnan(AVAR(:))));
[NLOGL, AVAR] = gamlike([1 1], [1 NaN 3]);
asserts.istrue(isnan(NLOGL));
asserts.istrue(all(isnan(AVAR(:))));
%=============================================================================
asserts.checkerror('gamlike([1 2])', _('Not enough input arguments.'));
asserts.checkerror('gamlike({1 2}, [1 2 3])', sprintf(_('%s must be real numeric.'), 'PARAMS'));
asserts.checkerror('gamlike([1 2 3], [1 2 3])', sprintf(_('PARAMS must be a %s vector.'), 'two-element'));
asserts.checkerror('gamlike([1 2], {1 2 3})', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('gamlike([1 2], [])', sprintf(_('%s must be nonempty.'), 'X'));
asserts.checkerror('gamlike([1 2], [0 1 2])', _('X must contain positive finite values.'));
asserts.checkerror('gamlike([1 2], [1 Inf 2])', _('X must contain positive finite values.'));
asserts.checkerror('gamlike([1 2], [1 2 3], [0 2 0])', _('CENSORING must contain 0 or 1 values.'));
asserts.checkerror('gamlike([1 2], [1 2 3], [], [1 -1 1])', sprintf(_('%s must contain nonnegative finite values.'), 'FREQ'));
asserts.checkerror('gamlike([1 2], [1 2 3], ones(2, 2))', _('Size mismatch on input arguments'));
%=============================================================================
