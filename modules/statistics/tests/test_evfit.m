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
asserts.isequal(nargin('evfit'), 5);
asserts.isequal(nargout('evfit'), 2);
%=============================================================================
x = [-2 -1 0 1 2 3];
[PHAT, PCI] = evfit(x);
asserts.isapprox(PHAT, [1.349567930826397 1.534358808399359], 1e-6);
asserts.isapprox(PCI, [0.06474657041823351 0.8824882691601001;
                      2.634389291234561 2.667748722771514], 1e-6);
%=============================================================================
[PHAT, PCI] = evfit(x, 0.01);
asserts.isapprox(PHAT, [1.349567930826397 1.534358808399359], 1e-6);
asserts.isapprox(PCI, [-0.3389735613240346 0.7416977780690303;
                      3.038109422976829 3.174145888695907], 1e-6);
%=============================================================================
censor = [0 0 0 1 0 1];
freq = [1 2 1 1 2 1];
[PHAT, PCI] = evfit(x, 0.05, censor, freq);
asserts.isapprox(PHAT, [1.718877479707116 1.796197414106813], 1e-6);
asserts.isapprox(PCI, [0.2807924567647526 0.9462246089071871;
                      3.156962502649479 3.409682141082915], 1e-6);
%=============================================================================
opts = statset('MaxIter', 120, 'TolX', 1e-6);
[PHAT2, PCI2] = evfit(x, [], [], [], opts);
asserts.isapprox(PHAT2, [1.349567930826397 1.534358808399359], 1e-6);
asserts.isapprox(PCI2, [0.06474657041823351 0.8824882691601001;
                       2.634389291234561 2.667748722771514], 1e-6);
%=============================================================================
xm = [-2 0; -1 1; 0 2; 1 3; 2 4];
[PHATM, PCIM] = evfit(xm);
asserts.isequal(size(PHATM), [2 2]);
asserts.isapprox(PHATM, [0.7033921760217647 1.266653211238571;
                        2.703392172603819 1.266653218924798], 1e-6);
asserts.isequal(size(PCIM), [2 2 2]);
asserts.isapprox(PCIM(:, :, 1), [-0.4583975042679940 0.6899382169880370;
                                1.865181856311523 2.325440623575143], 1e-6);
asserts.isapprox(PCIM(:, :, 2), [1.541602485964279 0.6899382204434013;
                                3.865181859243359 2.325440640151012], 1e-6);
%=============================================================================
[PHAT, PCI] = evfit(single(x));
asserts.isequal(class(PHAT), 'double');
asserts.isequal(class(PCI), 'double');
asserts.isapprox(PHAT, [1.349567930826397 1.534358808399359], 1e-6);
%=============================================================================
[PHAT, PCI] = evfit([1 NaN 3]);
asserts.istrue(all(isnan(PHAT)));
asserts.istrue(all(isnan(PCI(:))));
[PHAT, PCI] = evfit([1 2 3], [], [1 1 1], []);
asserts.istrue(all(isnan(PHAT)));
asserts.istrue(all(isnan(PCI(:))));
[PHAT, PCI] = evfit([1 2 3], [], [], [0 0 0]);
asserts.istrue(all(isnan(PHAT)));
asserts.istrue(all(isnan(PCI(:))));
%=============================================================================
asserts.checkerror('evfit()', _('Not enough input arguments.'));
asserts.checkerror('evfit([])', _('X must be a nonempty vector or matrix.'));
asserts.checkerror('evfit(ones(1, 1, 2))', _('X must be a nonempty vector or matrix.'));
asserts.checkerror('evfit({1 2 3})', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('evfit([1 2 3], {0.05})', sprintf(_('%s must be numeric.'), 'ALPHA'));
asserts.checkerror('evfit([1 2 3], -0.1)', sprintf(_('%s must be a scalar in the range [0,1].'), 'ALPHA'));
asserts.checkerror('evfit([1 2 3], 1.1)', sprintf(_('%s must be a scalar in the range [0,1].'), 'ALPHA'));
asserts.checkerror('evfit([1 2 3], [], [0 2 0])', _('CENSORING must contain 0 or 1 values.'));
asserts.checkerror('evfit([1 2 3], [], [], [1 -1 1])', sprintf(_('%s must contain nonnegative finite values.'), 'FREQ'));
asserts.checkerror('evfit([1 2 3], [], ones(2, 2))', _('Size mismatch on input arguments'));
asserts.checkerror('evfit([1 2 3], [], [], [], 1)', sprintf(_('%s must be a scalar structure.'), 'OPTIONS'));
asserts.checkerror('evfit([1 2 3], [], [], [], statset(''MaxIter'', 0))', sprintf(_('%s must be a positive integer scalar.'), 'OPTIONS MaxIter'));
asserts.checkerror('evfit([1 2 3], [], [], [], statset(''TolX'', 0))', sprintf(_('%s must be a positive scalar.'), 'OPTIONS TolX'));
%=============================================================================
