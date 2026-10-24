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
asserts.isequal(nargin('binofit'), 3);
asserts.isequal(nargout('binofit'), 2);
%=============================================================================
x = [0 2 5 8 10];
n = [10 10 10 10 10];
[PHAT, PCI] = binofit(x, n);
asserts.isapprox(PHAT, [0 0.2 0.5 0.8 1], 1e-12);
asserts.isapprox(PCI, [0 0.3084971078187607;
                      0.02521072632683338 0.5560954623076413;
                      0.1870860284473985 0.8129139715526013;
                      0.4439045376923585 0.9747892736731667;
                      0.6915028921812392 1], 1e-12);
%=============================================================================
[PHAT, PCI] = binofit(x, n, 0.01);
asserts.isapprox(PHAT, [0 0.2 0.5 0.8 1], 1e-12);
asserts.isapprox(PCI, [0 0.4112959813475248;
                      0.01085050937974049 0.6482011663048362;
                      0.1283105539350832 0.8716894460649163;
                      0.3517988336951632 0.9891494906202594;
                      0.5887040186524746 1], 1e-12);
%=============================================================================
xm = [0 5; 2 6; 4 7];
nm = [10 10; 10 12; 10 14];
[PHATM, PCIM] = binofit(xm, nm);
asserts.isequal(size(PHATM), [3 2]);
asserts.isapprox(PHATM, [0 0.5;
                        0.2 0.5;
                        0.4 0.5], 1e-12);
asserts.isequal(size(PCIM), [6 2]);
asserts.isapprox(PCIM, [0 0.3084971078187607;
                       0.02521072632683338 0.5560954623076413;
                       0.1215522581198272 0.7376219233930548;
                       0.1870860284473985 0.8129139715526013;
                       0.2109446382392967 0.7890553617607030;
                       0.2303605414480623 0.7696394585519373], 1e-12);
%=============================================================================
[PHAT, PCI] = binofit(single(x), single(n));
asserts.isequal(class(PHAT), 'double');
asserts.isequal(class(PCI), 'double');
asserts.isapprox(PHAT, [0 0.2 0.5 0.8 1], 1e-12);
%=============================================================================
[PHAT, PCI] = binofit([1 NaN 3], [5 5 5]);
asserts.isapprox(PHAT([1 3]), [0.2 0.6], 1e-12);
asserts.istrue(isnan(PHAT(2)));
asserts.isapprox(PCI([1 3], :), [0.005050763379468057 0.7164179361180893;
                                0.1466327996346731 0.9472550494736831], 1e-12);
asserts.istrue(all(isnan(PCI(2, :))));
%=============================================================================
[PHAT, PCI] = binofit([0 5], [5 5]);
asserts.isequal(PHAT, [0 1]);
asserts.isequal(PCI(1, 1), 0);
asserts.isequal(PCI(2, 2), 1);
asserts.isapprox(PCI(1, 2), 0.5218237501049812, 1e-12);
asserts.isapprox(PCI(2, 1), 0.4781762498950185, 1e-12);
%=============================================================================
asserts.checkerror('binofit()', _('Not enough input arguments.'));
asserts.checkerror('binofit([], [])', sprintf(_('%s must be nonempty.'), 'X and N'));
asserts.checkerror('binofit([], [])', sprintf(_('%s must be nonempty.'), 'X and N'), 'Nelson:validators:mustBeNonempty');
asserts.checkerror('binofit({1 2}, 10)', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('binofit([1 2], {10 10})', sprintf(_('%s must be real numeric.'), 'N'));
asserts.checkerror('binofit(ones(2), ones(3))', _('Size mismatch on input arguments'));
asserts.checkerror('binofit([-1 1], [5 5])', sprintf(_('%s must contain nonnegative integer finite values.'), 'X'));
asserts.checkerror('binofit([1.5 2], [5 5])', sprintf(_('%s must contain nonnegative integer finite values.'), 'X'));
asserts.checkerror('binofit([1 Inf], [5 5])', sprintf(_('%s must contain nonnegative integer finite values.'), 'X'));
asserts.checkerror('binofit([1 2], [-5 5])', sprintf(_('%s must contain nonnegative integer finite values.'), 'N'));
asserts.checkerror('binofit([1 2], [5.5 5])', sprintf(_('%s must contain nonnegative integer finite values.'), 'N'));
asserts.checkerror('binofit([1 2], [Inf 5])', sprintf(_('%s must contain nonnegative integer finite values.'), 'N'));
asserts.checkerror('binofit([6 2], [5 5])', _('X must not exceed N.'));
asserts.checkerror('binofit([1 2], [5 5], {0.05})', sprintf(_('%s must be numeric.'), 'ALPHA'));
asserts.checkerror('binofit([1 2], [5 5], -0.1)', sprintf(_('%s must be a scalar in the range [0,1].'), 'ALPHA'));
asserts.checkerror('binofit([1 2], [5 5], 1.1)', sprintf(_('%s must be a scalar in the range [0,1].'), 'ALPHA'));
%=============================================================================
