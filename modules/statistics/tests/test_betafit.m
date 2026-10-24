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
asserts.isequal(nargin('betafit'), 2);
asserts.isequal(nargout('betafit'), 2);
%=============================================================================
x = [0.12 0.2 0.35 0.5 0.7 0.85];
[PHAT, PCI] = betafit(x);
asserts.isapprox(PHAT, [1.454428030863046 1.726016528287111], 1e-12);
asserts.isapprox(PCI, [0.5067427948095659 0.5856340607364172;
                      4.174427182048263 5.087021496280666], 1e-12);
%=============================================================================
[PHAT, PCI] = betafit(x, 0.01);
asserts.isapprox(PHAT, [1.454428030863046 1.726016528287111], 1e-12);
asserts.isapprox(PCI, [0.3638341935188416 0.4169887190901191;
                      5.814079420357204 7.144397245136134], 1e-12);
%=============================================================================
xm = [0.1 0.2; 0.2 0.35; 0.4 0.55; 0.65 0.75; 0.85 0.9];
[PHATM, PCIM] = betafit(xm);
asserts.isequal(size(PHATM), [2 2]);
asserts.isapprox(PHATM, [1.210569072748261 1.516000289787297;
                        1.811987026844903 1.457046571880519], 1e-12);
asserts.isequal(size(PCIM), [2 2 2]);
asserts.isapprox(PCIM(:, :, 1), [0.3866968576775351 0.5517338703873901;
                                3.789732062205271 5.950870812315589], 1e-12);
asserts.isapprox(PCIM(:, :, 2), [0.4639236934118748 0.4600587444659575;
                                4.953954521556974 4.614594849388551], 1e-12);
%=============================================================================
[PHAT, PCI] = betafit(single(x));
asserts.isequal(class(PHAT), 'double');
asserts.isequal(class(PCI), 'double');
asserts.isapprox(PHAT, [1.454428030863046 1.726016528287111], 1e-6);
%=============================================================================
[PHAT, PCI] = betafit([0.4 NaN 0.6]);
asserts.istrue(all(isnan(PHAT)));
asserts.istrue(all(isnan(PCI(:))));
%=============================================================================
asserts.checkerror('betafit()', _('Not enough input arguments.'));
asserts.checkerror('betafit([])', _('X must be a nonempty vector or matrix.'));
asserts.checkerror('betafit([])', _('X must be a nonempty vector or matrix.'), 'Nelson:statistics:xMustBeNonemptyVectorOrMatrix');
asserts.checkerror('betafit(ones(1, 1, 2))', _('X must be a nonempty vector or matrix.'));
asserts.checkerror('betafit({0.2 0.5})', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('betafit([0 0.2 0.5])', _('X must contain finite values in the open interval (0,1).'));
asserts.checkerror('betafit([0.2 0.5 1])', _('X must contain finite values in the open interval (0,1).'));
asserts.checkerror('betafit([0.2 Inf 0.5])', _('X must contain finite values in the open interval (0,1).'));
asserts.checkerror('betafit([0.2 0.5], {0.05})', sprintf(_('%s must be numeric.'), 'ALPHA'));
asserts.checkerror('betafit([0.2 0.5], -0.1)', sprintf(_('%s must be a scalar in the range [0,1].'), 'ALPHA'));
asserts.checkerror('betafit([0.2 0.5], 1.1)', sprintf(_('%s must be a scalar in the range [0,1].'), 'ALPHA'));
%=============================================================================