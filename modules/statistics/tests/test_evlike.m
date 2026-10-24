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
asserts.isequal(nargin('evlike'), 4);
asserts.isequal(nargout('evlike'), 2);
%=============================================================================
x = [-2 -1 0 1 2 3];
phat = evfit(x);
[NLOGL, AVAR] = evlike(phat, x);
asserts.isapprox(NLOGL, 11.89084989663238, 1e-6);
asserts.isapprox(AVAR, [0.4297237072037848 -0.08368181782691925;
                       -0.08368181782691925 0.1874994996359633], 1e-6);
%=============================================================================
censor = [0 0 0 1 0 1];
freq = [1 2 1 1 2 1];
phat = evfit(x, 0.05, censor, freq);
[NLOGL, AVAR] = evlike(phat, x, censor, freq);
asserts.isapprox(NLOGL, 15.25575255481226, 1e-6);
asserts.isapprox(AVAR, [0.5383601976248241 -0.01485230627169684;
                       -0.01485230627169684 0.3450296736904299], 1e-6);
%=============================================================================
[NLOGL, AVAR] = evlike(single([0 1]), single([0 1 2]));
asserts.isequal(class(NLOGL), 'double');
asserts.isequal(class(AVAR), 'double');
asserts.isapprox(NLOGL, 8.107337927389695, 1e-6);
%=============================================================================
[NLOGL, AVAR] = evlike([0 0], x);
asserts.istrue(isinf(NLOGL));
asserts.istrue(all(isnan(AVAR(:))));
[NLOGL, AVAR] = evlike([NaN 1], x);
asserts.istrue(isnan(NLOGL));
asserts.istrue(all(isnan(AVAR(:))));
[NLOGL, AVAR] = evlike([0 1], [1 NaN 3]);
asserts.istrue(isnan(NLOGL));
asserts.istrue(all(isnan(AVAR(:))));
%=============================================================================
asserts.checkerror('evlike([0 1])', _('Not enough input arguments.'));
asserts.checkerror('evlike([0 1 2], x)', sprintf(_('PARAMS must be a %s vector.'), 'two-element'));
asserts.checkerror('evlike({0 1}, x)', sprintf(_('%s must be real numeric.'), 'PARAMS'));
asserts.checkerror('evlike([0 1], {})', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('evlike([0 1], [])', sprintf(_('%s must be nonempty.'), 'X'));
asserts.checkerror('evlike([0 1], x, [0 2 0 0 0 0])', _('CENSORING must contain 0 or 1 values.'));
asserts.checkerror('evlike([0 1], x, [], [1 -1 1 1 1 1])', sprintf(_('%s must contain nonnegative finite values.'), 'FREQ'));
asserts.checkerror('evlike([0 1], x, ones(2, 2))', _('Size mismatch on input arguments'));
%=============================================================================
