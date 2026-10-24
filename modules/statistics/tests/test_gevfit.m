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
asserts.isequal(nargin('gevfit'), 5);
asserts.isequal(nargout('gevfit'), 2);
%=============================================================================
x = [-1.2 -0.4 0.1 0.8 1.5 2.8 4.0];
[PHAT, PCI] = gevfit(x);
asserts.isapprox(PHAT, [-5.678401856490126e-02 1.421941033901271e+00 3.208905845945853e-01], 1e-2, 1e-1);
asserts.isapprox(PCI, [-9.832429383918991e-01 6.944765037036665e-01 -9.926011672889139e-01;
                       8.696749012620967e-01 2.911425070696084e+00 1.634382336478085e+00], 1e-2, 2e-1);
%=============================================================================
[PHAT, PCI] = gevfit(x, 0.01);
asserts.isapprox(PHAT, [-5.678401856490126e-02 1.421941033901271e+00 3.208905845945853e-01], 1e-2, 1e-1);
asserts.isapprox(PCI, [-1.274357429631765e+00 5.544519035384995e-01 -1.405330188418883e+00;
                       1.160789392501963e+00 3.646693772694066e+00 2.047111357608054e+00], 1e-2, 2e-1);
%=============================================================================
censor = [0 0 0 1 0 1 0];
freq = [1 2 1 1 2 1 1];
[PHAT, PCI] = gevfit(x, 0.05, censor, freq);
asserts.isapprox(PHAT, [3.876774538305412e-01 1.463665505004482e+00 1.965436373813094e-01], 1e-2, 2e-1);
asserts.isapprox(PCI, [-8.224325134242509e-01 7.101820793380070e-01 -1.008978064364088e+00;
                       1.597787421085333e+00 3.016573879950586e+00 1.402065339126707e+00], 1e-2, 2e-1);
%=============================================================================
opts = statset('MaxIter', 160, 'TolX', 1e-4);
[PHAT2, PCI2] = gevfit(x, [], [], [], opts);
asserts.isapprox(PHAT2, [-5.678401856490126e-02 1.421941033901271e+00 3.208905845945853e-01], 1e-2, 1e-1);
asserts.isapprox(PCI2, [-9.832429383918991e-01 6.944765037036665e-01 -9.926011672889139e-01;
                        8.696749012620967e-01 2.911425070696084e+00 1.634382336478085e+00], 1e-2, 2e-1);
%=============================================================================
xm = [-1 0; -0.2 0.5; 0.4 1.2; 1.5 2.0; 3.0 3.5];
[PHATM, PCIM] = gevfit(xm);
asserts.isequal(size(PHATM), [2 3]);
asserts.isapprox(PHATM, [6.680645149811258e-03 1.120607950362313e+00 7.457523826066395e-02;
                        1.714359843496142e-01 8.802555220999863e-01 7.790516150005073e-01], 1e-2, 2e-1);
asserts.isequal(size(PCIM), [2 3 2]);
asserts.isapprox(PCIM(:, :, 1), [-1.184255615490922e+00 4.610763941317437e-01 -1.183984717388003e+00;
                                 1.197616905790545e+00 2.723544719264927e+00 1.333135193909331e+00], 1e-2, 2e-1);
asserts.isapprox(PCIM(:, :, 2), [-1.189197611119902e+00 3.313326942794296e-01 -2.586879371102864e-01;
                                 1.532069579819130e+00 2.338585348097431e+00 1.816791167111301e+00], 1e-2, 2e-1);
%=============================================================================
[PHAT, PCI] = gevfit(single(x));
asserts.isequal(class(PHAT), 'double');
asserts.isequal(class(PCI), 'double');
asserts.isapprox(PHAT, [-5.678401856490126e-02 1.421941033901271e+00 3.208905845945853e-01], 1e-7);
%=============================================================================
[PHAT, PCI] = gevfit([1 NaN 3]);
asserts.istrue(all(isnan(PHAT)));
asserts.istrue(all(isnan(PCI(:))));
[PHAT, PCI] = gevfit([1 2 3], [], [1 1 1], []);
asserts.istrue(all(isnan(PHAT)));
asserts.istrue(all(isnan(PCI(:))));
[PHAT, PCI] = gevfit([1 2 3], [], [], [0 0 0]);
asserts.istrue(all(isnan(PHAT)));
asserts.istrue(all(isnan(PCI(:))));
%=============================================================================
asserts.checkerror('gevfit()', _('Not enough input arguments.'));
asserts.checkerror('gevfit([])', _('X must be a nonempty vector or matrix.'));
asserts.checkerror('gevfit(ones(1, 1, 2))', _('X must be a nonempty vector or matrix.'));
asserts.checkerror('gevfit({1 2 3})', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('gevfit([1 Inf 2])', sprintf(_('%s must contain finite values.'), 'X'));
asserts.checkerror('gevfit([1 2 3], {0.05})', sprintf(_('%s must be numeric.'), 'ALPHA'));
asserts.checkerror('gevfit([1 2 3], -0.1)', sprintf(_('%s must be a scalar in the range [0,1].'), 'ALPHA'));
asserts.checkerror('gevfit([1 2 3], 1.1)', sprintf(_('%s must be a scalar in the range [0,1].'), 'ALPHA'));
asserts.checkerror('gevfit([1 2 3], [], [0 2 0])', _('CENSORING must contain 0 or 1 values.'));
asserts.checkerror('gevfit([1 2 3], [], [], [1 -1 1])', sprintf(_('%s must contain nonnegative finite values.'), 'FREQ'));
asserts.checkerror('gevfit([1 2 3], [], ones(2, 2))', _('Size mismatch on input arguments'));
asserts.checkerror('gevfit([1 2 3], [], [], [], 1)', sprintf(_('%s must be a scalar structure.'), 'OPTIONS'));
asserts.checkerror('gevfit([1 2 3], [], [], [], statset(''MaxIter'', 0))', sprintf(_('%s must be a positive integer scalar.'), 'OPTIONS MaxIter'));
asserts.checkerror('gevfit([1 2 3], [], [], [], statset(''TolX'', 0))', sprintf(_('%s must be a positive scalar.'), 'OPTIONS TolX'));
%=============================================================================
