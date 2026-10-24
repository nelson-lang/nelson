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
asserts.isequal(nargin('nbinlike'), 4);
asserts.isequal(nargout('nbinlike'), 2);
%=============================================================================
x = [0 1 2 4 6 9 12 15];
params = [4 0.45];
[NLOGL, AVAR] = nbinlike(params, x);
asserts.isapprox(NLOGL, 24.95287568128001, 1e-4);
asserts.isapprox(AVAR, [6.261785373514261 0.3478683827184349;
                       0.3478683827184349 0.02245046711416182], 1e-4);
%=============================================================================
censor = [0 0 0 0 1 0 1 0];
freq = [1 2 1 1 1 2 1 1];
[PHAT] = nbinfit(x, 0.05, censor, freq);
[NLOGL, AVAR] = nbinlike(PHAT, x, censor, freq);
asserts.istrue(isfinite(NLOGL));
asserts.istrue(all(isfinite(AVAR(:))));
%=============================================================================
[NLOGL, AVAR] = nbinlike(single([3 0.5]), single([0 1 2 3]));
asserts.isequal(class(NLOGL), 'double');
asserts.isequal(class(AVAR), 'double');
asserts.isapprox(NLOGL, 7.283692399188805, 1e-4);
asserts.istrue(all(isnan(AVAR(:))));
%=============================================================================
[NLOGL, AVAR] = nbinlike([1 1], [0 0 0]);
asserts.isequal(NLOGL, 0);
asserts.istrue(all(isnan(AVAR(:))));
[NLOGL, AVAR] = nbinlike([1 1], [0 1]);
asserts.istrue(isinf(NLOGL));
asserts.istrue(all(isnan(AVAR(:))));
[NLOGL, AVAR] = nbinlike([1 0], [0 1]);
asserts.istrue(isinf(NLOGL));
asserts.istrue(all(isnan(AVAR(:))));
[NLOGL, AVAR] = nbinlike([0 0.5], x);
asserts.istrue(isinf(NLOGL));
asserts.istrue(all(isnan(AVAR(:))));
[NLOGL, AVAR] = nbinlike([NaN 0.5], x);
asserts.istrue(isnan(NLOGL));
asserts.istrue(all(isnan(AVAR(:))));
[NLOGL, AVAR] = nbinlike([1 0.5], [1 NaN 3]);
asserts.istrue(isnan(NLOGL));
asserts.istrue(all(isnan(AVAR(:))));
%=============================================================================
asserts.checkerror('nbinlike([1 0.5])', _('Not enough input arguments.'));
asserts.checkerror('nbinlike({1 0.5}, [1 2 3])', sprintf(_('%s must be real numeric.'), 'PARAMS'));
asserts.checkerror('nbinlike([1 0.5 2], [1 2 3])', sprintf(_('PARAMS must be a %s vector.'), 'two-element'));
asserts.checkerror('nbinlike([1 0.5], {1 2 3})', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('nbinlike([1 0.5], [])', sprintf(_('%s must be nonempty.'), 'X'));
asserts.checkerror('nbinlike([1 0.5], [-1 1 2])', sprintf(_('%s must contain nonnegative integer finite values.'), 'X'));
asserts.checkerror('nbinlike([1 0.5], [1.5 2 3])', sprintf(_('%s must contain nonnegative integer finite values.'), 'X'));
asserts.checkerror('nbinlike([1 0.5], [1 Inf 2])', sprintf(_('%s must contain nonnegative integer finite values.'), 'X'));
asserts.checkerror('nbinlike([1 0.5], [1 2 3], [0 2 0])', _('CENSORING must contain 0 or 1 values.'));
asserts.checkerror('nbinlike([1 0.5], [1 2 3], [], [1 -1 1])', sprintf(_('%s must contain nonnegative finite values.'), 'FREQ'));
asserts.checkerror('nbinlike([1 0.5], [1 2 3], ones(2, 2))', _('Size mismatch on input arguments'));
%=============================================================================
