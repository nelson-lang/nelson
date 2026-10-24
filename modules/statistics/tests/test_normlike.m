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
asserts.isequal(nargin('normlike'), 4);
asserts.isequal(nargout('normlike'), 2);
%=============================================================================
x = [-2 -1 0 1 3 5];
[NLOGL, AVAR] = normlike([1 2], x);
asserts.isapprox(NLOGL, 13.92251428258771, 1e-4);
asserts.isapprox(AVAR, [0.6666626640726055 -0.0000006072969573079265;
                       -0.0000006072969573079265 0.2051279078638130], 1e-4);
%=============================================================================
censor = [0 0 0 1 0 1];
freq = [1 2 1 1 2 1];
[NLOGL, AVAR] = normlike([0 2], x, censor, freq);
asserts.isapprox(NLOGL, 18.93007432146002, 1e-4);
asserts.isapprox(AVAR, [0.7398396257759413 -0.1922004611090463;
                       -0.1922004611090463 0.1706618587720619], 1e-4);
%=============================================================================
[NLOGL, AVAR] = normlike(single([1 2]), single([1 2 3]));
asserts.isequal(class(NLOGL), 'double');
asserts.isequal(class(AVAR), 'double');
asserts.isapprox(NLOGL, 5.461257141293855, 1e-4);
asserts.istrue(all(isnan(AVAR(:))));
%=============================================================================
[NLOGL, AVAR] = normlike([0 0], x);
asserts.istrue(isinf(NLOGL));
asserts.istrue(all(isnan(AVAR(:))));
[NLOGL, AVAR] = normlike([NaN 1], x);
asserts.istrue(isnan(NLOGL));
asserts.istrue(all(isnan(AVAR(:))));
[NLOGL, AVAR] = normlike([0 1], [1 NaN 3]);
asserts.istrue(isnan(NLOGL));
asserts.istrue(all(isnan(AVAR(:))));
%=============================================================================
asserts.checkerror('normlike([1 2])', _('Not enough input arguments.'));
asserts.checkerror('normlike({1 2}, [1 2 3])', sprintf(_('PARAMS must be a %s vector.'), 'two-element real numeric'));
asserts.checkerror('normlike([1 2 3], [1 2 3])', sprintf(_('PARAMS must be a %s vector.'), 'two-element real numeric'));
asserts.checkerror('normlike([1 2], {1 2 3})', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('normlike([1 2], [])', sprintf(_('%s must be nonempty.'), 'X'));
asserts.checkerror('normlike([1 2], [1 Inf 2])', sprintf(_('%s must contain finite values.'), 'X'));
asserts.checkerror('normlike([1 2], [1 2 3], [0 2 0])', _('CENSORING must contain 0 or 1 values.'));
asserts.checkerror('normlike([1 2], [1 2 3], [], [1 -1 1])', sprintf(_('%s must contain nonnegative finite values.'), 'FREQ'));
asserts.checkerror('normlike([1 2], [1 2 3], ones(2, 2))', _('Size mismatch on input arguments'));
%=============================================================================