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
asserts.isequal(nargin('raylfit'), 4);
asserts.isequal(nargout('raylfit'), 2);
%=============================================================================
x = [0.5 1 2 3 5 8];
[PHAT, PCI] = raylfit(x);
asserts.isapprox(PHAT, 2.933285984466340, 1e-12);
asserts.isapprox(PCI, [1.966091053118864; 4.376280871131397], 1e-12);
%=============================================================================
[PHAT, PCI] = raylfit(x, 0.01);
asserts.isapprox(PHAT, 2.933285984466340, 1e-12);
asserts.isapprox(PCI, [1.733832641651132; 4.962512793895091], 1e-12);
%=============================================================================
censor = [0 0 0 1 0 1];
freq = [1 2 1 1 2 1];
[PHAT, PCI] = raylfit(x, 0.05, censor, freq);
asserts.isapprox(PHAT, 3.281894777919203, 1e-12);
asserts.isapprox(PCI, [2.199754069559662; 4.896380682904884], 1e-12);
%=============================================================================
xm = [0.5 1; 1 2; 2 3; 3 4; 5 7];
[PHATM, PCIM] = raylfit(xm);
asserts.isequal(size(PHATM), [2 1]);
asserts.isapprox(PHATM, [1.981161275615895; 2.810693864511039], 1e-12);
asserts.isequal(size(PCIM), [2 2]);
asserts.isapprox(PCIM, [1.278160734110911 1.813339417764860;
                       3.070818790823075 4.356603028978224], 1e-12);
%=============================================================================
[PHAT, PCI] = raylfit(single(x));
asserts.isequal(class(PHAT), 'double');
asserts.isequal(class(PCI), 'double');
asserts.isapprox(PHAT, 2.933285984466340, 1e-12);
%=============================================================================
[PHAT, PCI] = raylfit([1 NaN 3]);
asserts.istrue(isnan(PHAT));
asserts.istrue(all(isnan(PCI(:))));
[PHAT, PCI] = raylfit([1 2 3], [], [1 1 1], []);
asserts.istrue(isnan(PHAT));
asserts.istrue(all(isnan(PCI(:))));
[PHAT, PCI] = raylfit([1 2 3], [], [], [0 0 0]);
asserts.istrue(isnan(PHAT));
asserts.istrue(all(isnan(PCI(:))));
%=============================================================================
asserts.checkerror('raylfit()', _('Not enough input arguments.'));
asserts.checkerror('raylfit([])', _('X must be a nonempty vector or matrix.'));
asserts.checkerror('raylfit(ones(1, 1, 2))', _('X must be a nonempty vector or matrix.'));
asserts.checkerror('raylfit({1 2 3})', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('raylfit([-1 1 2])', sprintf(_('%s must contain nonnegative finite values.'), 'X'));
asserts.checkerror('raylfit([1 Inf 2])', sprintf(_('%s must contain nonnegative finite values.'), 'X'));
asserts.checkerror('raylfit([1 2 3], {0.05})', sprintf(_('%s must be numeric.'), 'ALPHA'));
asserts.checkerror('raylfit([1 2 3], -0.1)', sprintf(_('%s must be a scalar in the range [0,1].'), 'ALPHA'));
asserts.checkerror('raylfit([1 2 3], 1.1)', sprintf(_('%s must be a scalar in the range [0,1].'), 'ALPHA'));
asserts.checkerror('raylfit([1 2 3], [], [0 2 0])', _('CENSORING must contain 0 or 1 values.'));
asserts.checkerror('raylfit([1 2 3], [], [], [1 -1 1])', sprintf(_('%s must contain nonnegative finite values.'), 'FREQ'));
asserts.checkerror('raylfit([1 2 3], [], ones(2, 2))', _('Size mismatch on input arguments'));
%=============================================================================
