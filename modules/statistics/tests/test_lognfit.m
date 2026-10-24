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
asserts.isequal(nargin('lognfit'), 5);
asserts.isequal(nargout('lognfit'), 2);
%=============================================================================
x = [0.5 1 2 3 5 8];
[PHAT, PCI] = lognfit(x);
asserts.isapprox(PHAT, [0.7979152904636743 0.9365014364963760], 1e-4);
asserts.isapprox(PCI, [0.04857071929664269 0.4066344491341202;
                      1.547259861630706 1.466368423858632], 1e-4);
%=============================================================================
[PHAT, PCI] = lognfit(x, 0.01);
asserts.isapprox(PHAT, [0.7979152904636743 0.9365014364963760], 1e-4);
asserts.isapprox(PCI, [-0.1868904100963166 0.2401381748259536;
                      1.782720991023665 1.632864698166798], 1e-4);
%=============================================================================
censor = [0 0 0 1 0 1];
freq = [1 2 1 1 2 1];
[PHAT, PCI] = lognfit(x, 0.05, censor, freq);
asserts.isapprox(PHAT, [0.9808752654800201 1.123582749374891], 1e-4);
asserts.isapprox(PCI, [0.1630080687412662 0.4563982307249053;
                      1.798742462218774 1.790767268024877], 1e-4);
%=============================================================================
opts = statset('MaxIter', 120, 'TolX', 1e-4);
[PHAT2, PCI2] = lognfit(x, [], [], [], opts);
asserts.isapprox(PHAT2, [0.7979152904636743 0.9365014364963760], 1e-4);
asserts.isapprox(PCI2, [0.04857071929664269 0.4066344491341202;
                       1.547259861630706 1.466368423858632], 1e-4);
%=============================================================================
xm = [0.5 1; 1 2; 2 3; 3 4; 5 7];
[PHATM, PCIM] = lognfit(xm);
asserts.isequal(size(PHATM), [2 2]);
asserts.isapprox(PHATM, [0.5416100402204420 0.8113492718048468;
                        1.024792795880652 0.6549471682747616], 1e-4);
asserts.isequal(size(PCIM), [2 2 2]);
asserts.isapprox(PCIM(:, :, 1), [-0.1695563869617602 0.3084790444798517;
                                1.252776467402644 1.314219499129842], 1e-4);
asserts.isapprox(PCIM(:, :, 2), [0.4507159303557678 0.2490134626659516;
                                1.598869661405536 1.060880873883572], 1e-4);
%=============================================================================
[PHAT, PCI] = lognfit(single(x));
asserts.isequal(class(PHAT), 'double');
asserts.isequal(class(PCI), 'double');
asserts.isapprox(PHAT, [0.7979152904636743 0.9365014364963760], 1e-4);
%=============================================================================
[PHAT, PCI] = lognfit([1 NaN 3]);
asserts.istrue(all(isnan(PHAT)));
asserts.istrue(all(isnan(PCI(:))));
[PHAT, PCI] = lognfit([1 2 3], [], [1 1 1], []);
asserts.istrue(all(isnan(PHAT)));
asserts.istrue(all(isnan(PCI(:))));
[PHAT, PCI] = lognfit([1 2 3], [], [], [0 0 0]);
asserts.istrue(all(isnan(PHAT)));
asserts.istrue(all(isnan(PCI(:))));
%=============================================================================
asserts.checkerror('lognfit()', _('Not enough input arguments.'));
asserts.checkerror('lognfit([])', _('X must be a nonempty vector or matrix.'));
asserts.checkerror('lognfit(ones(1, 1, 2))', _('X must be a nonempty vector or matrix.'));
asserts.checkerror('lognfit({1 2 3})', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('lognfit([0 1 2])', _('X must contain positive finite values.'));
asserts.checkerror('lognfit([1 Inf 2])', _('X must contain positive finite values.'));
asserts.checkerror('lognfit([1 2 3], {0.05})', sprintf(_('%s must be numeric.'), 'ALPHA'));
asserts.checkerror('lognfit([1 2 3], -0.1)', sprintf(_('%s must be a scalar in the range [0,1].'), 'ALPHA'));
asserts.checkerror('lognfit([1 2 3], 1.1)', sprintf(_('%s must be a scalar in the range [0,1].'), 'ALPHA'));
asserts.checkerror('lognfit([1 2 3], [], [0 2 0])', _('CENSORING must contain 0 or 1 values.'));
asserts.checkerror('lognfit([1 2 3], [], [], [1 -1 1])', sprintf(_('%s must contain nonnegative finite values.'), 'FREQ'));
asserts.checkerror('lognfit([1 2 3], [], ones(2, 2))', _('Size mismatch on input arguments'));
asserts.checkerror('lognfit([1 2 3], [], [], [], 1)', sprintf(_('%s must be a scalar structure.'), 'OPTIONS'));
asserts.checkerror('lognfit([1 2 3], [], [], [], statset(''MaxIter'', 0))', sprintf(_('%s must be a positive integer scalar.'), 'OPTIONS MaxIter'));
asserts.checkerror('lognfit([1 2 3], [], [], [], statset(''TolX'', 0))', sprintf(_('%s must be a positive scalar.'), 'OPTIONS TolX'));
%=============================================================================
