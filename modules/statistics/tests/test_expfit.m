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
asserts.isequal(nargin('expfit'), 5);
asserts.isequal(nargout('expfit'), 2);
%=============================================================================
x = [0.5 1 2 3 5 8];
[PHAT, PCI] = expfit(x);
asserts.isapprox(PHAT, 3.25, 1e-12);
asserts.isapprox(PCI, [1.460097260500232; 7.234107128165739], 1e-12);
%=============================================================================
[PHAT, PCI] = expfit(x, 0.01);
asserts.isapprox(PHAT, 3.25, 1e-12);
asserts.isapprox(PCI, [1.135504546639229; 9.302032326741445], 1e-12);
%=============================================================================
censor = [0 0 0 1 0 1];
freq = [1 2 1 1 2 1];
[PHAT, PCI] = expfit(x, 0.05, censor, freq);
asserts.isapprox(PHAT, 4.25, 1e-12);
asserts.isapprox(PCI, [1.909357956038766; 9.459986244524428], 1e-12);
%=============================================================================
opts = statset('MaxIter', 100, 'TolX', 1e-8);
[PHAT2, PCI2] = expfit(x, [], [], [], opts);
asserts.isapprox(PHAT2, 3.25, 1e-12);
asserts.isapprox(PCI2, [1.460097260500232; 7.234107128165739], 1e-12);
%=============================================================================
xm = [0.5 1; 1 2; 2 3; 3 4; 5 7];
[PHATM, PCIM] = expfit(xm);
asserts.isequal(size(PHATM), [2 1]);
asserts.isapprox(PHATM, [2.3; 3.4], 1e-12);
asserts.isequal(size(PCIM), [2 2]);
asserts.isapprox(PCIM, [0.9573239705065707 1.415174565096670;
                       5.525820059849520 8.168603566734074], 1e-12);
%=============================================================================
[PHAT, PCI] = expfit(single(x));
asserts.isequal(class(PHAT), 'double');
asserts.isequal(class(PCI), 'double');
asserts.isapprox(PHAT, 3.25, 1e-12);
%=============================================================================
[PHAT, PCI] = expfit([1 NaN 3]);
asserts.istrue(isnan(PHAT));
asserts.istrue(all(isnan(PCI(:))));
[PHAT, PCI] = expfit([1 2 3], [], [1 1 1], []);
asserts.istrue(isnan(PHAT));
asserts.istrue(all(isnan(PCI(:))));
[PHAT, PCI] = expfit([1 2 3], [], [], [0 0 0]);
asserts.istrue(isnan(PHAT));
asserts.istrue(all(isnan(PCI(:))));
[PHAT, PCI] = expfit([0 1 2]);
asserts.isapprox(PHAT, 1, 1e-12);
asserts.istrue(all(isfinite(PCI(:))));
%=============================================================================
asserts.checkerror('expfit()', _('Not enough input arguments.'));
asserts.checkerror('expfit([])', _('X must be a nonempty vector or matrix.'));
asserts.checkerror('expfit(ones(1, 1, 2))', _('X must be a nonempty vector or matrix.'));
asserts.checkerror('expfit({1 2 3})', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('expfit([-1 1 2])', sprintf(_('%s must contain nonnegative finite values.'), 'X'));
asserts.checkerror('expfit([1 Inf 2])', sprintf(_('%s must contain nonnegative finite values.'), 'X'));
asserts.checkerror('expfit([1 2 3], {0.05})', sprintf(_('%s must be numeric.'), 'ALPHA'));
asserts.checkerror('expfit([1 2 3], -0.1)', sprintf(_('%s must be a scalar in the range [0,1].'), 'ALPHA'));
asserts.checkerror('expfit([1 2 3], 1.1)', sprintf(_('%s must be a scalar in the range [0,1].'), 'ALPHA'));
asserts.checkerror('expfit([1 2 3], [], [0 2 0])', _('CENSORING must contain 0 or 1 values.'));
asserts.checkerror('expfit([1 2 3], [], [], [1 -1 1])', sprintf(_('%s must contain nonnegative finite values.'), 'FREQ'));
asserts.checkerror('expfit([1 2 3], [], ones(2, 2))', _('Size mismatch on input arguments'));
asserts.checkerror('expfit([1 2 3], [], [], [], 1)', sprintf(_('%s must be a scalar structure.'), 'OPTIONS'));
asserts.checkerror('expfit([1 2 3], [], [], [], statset(''MaxIter'', 0))', sprintf(_('%s must be a positive integer scalar.'), 'OPTIONS MaxIter'));
asserts.checkerror('expfit([1 2 3], [], [], [], statset(''TolX'', 0))', sprintf(_('%s must be a positive scalar.'), 'OPTIONS TolX'));
%=============================================================================
