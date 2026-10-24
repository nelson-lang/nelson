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
asserts.isequal(nargin('zscore'), 3);
asserts.isequal(nargout('zscore'), 3);
%=============================================================================
X = [1 2 3; 4 5 6];
[Z, mu, sigma] = zscore(X, 0, 1);
asserts.isapprox(Z, [-0.707106781186548 -0.707106781186548 -0.707106781186548; ...
                    0.707106781186548 0.707106781186548 0.707106781186548], 1e-12);
asserts.isequal(mu, [2.5 3.5 4.5]);
asserts.isapprox(sigma, [2.121320343559642 2.121320343559642 2.121320343559642], 1e-12);
%=============================================================================
[Z2, mu2, sigma2] = zscore(X, 1, 2);
asserts.isapprox(Z2, [-1.224744871391589 0 1.224744871391589; ...
                     -1.224744871391589 0 1.224744871391589], 1e-12);
asserts.isequal(mu2, [2; 5]);
asserts.isapprox(sigma2, [0.816496580927726; 0.816496580927726], 1e-12);
%=============================================================================
B = reshape(1:24, [3 4 2]);
[Z3, mu3, sigma3] = zscore(B, 0, [1 2]);
asserts.isequal(size(Z3), [3 4 2]);
asserts.isequal(mu3(:), [6.5; 18.5]);
asserts.isapprox(sigma3(:), [3.605551275463989; 3.605551275463989], 1e-12);
asserts.isapprox(Z3(:, :, 1), [-1.525425539619380 -0.693375245281536 0.138675049056307 0.970725343394151; ...
                              -1.248075441506766 -0.416025147168922 0.416025147168922 1.248075441506766; ...
                              -0.970725343394151 -0.138675049056307 0.693375245281536 1.525425539619380], 1e-12);
%=============================================================================
[Z4, mu4, sigma4] = zscore([5 5 5], 0, 2);
asserts.isequal(Z4, [0 0 0]);
asserts.isequal(mu4, 5);
asserts.isequal(sigma4, 0);
[Z5, mu5, sigma5] = zscore([1 NaN 3], 0, 2);
asserts.istrue(all(isnan(Z5)));
asserts.istrue(isnan(mu5));
asserts.istrue(isnan(sigma5));
%=============================================================================
[Z6, mu6, sigma6] = zscore(int32([1 2 3]), 0, 2);
asserts.isequal(class(Z6), 'double');
asserts.isapprox(Z6, [-1 0 1], 1e-12);
asserts.isequal(mu6, 2);
asserts.isequal(sigma6, 1);
%=============================================================================
asserts.checkerror('zscore()', _('Not enough input arguments.'));
asserts.checkerror('zscore({1})', sprintf(_('%s must be a real numeric array.'), 'X'));
asserts.checkerror('zscore([1 2], 2)', sprintf(_('%s must be 0 or 1.'), 'FLAG'));
asserts.checkerror('zscore([1 2], 2)', sprintf(_('%s must be 0 or 1.'), 'FLAG'), 'Nelson:statistics:invalidFlag');
asserts.checkerror('zscore([1 2], 0, 0)', _('Dimension argument must be a positive integer scalar, vector, or ''all''.'));
asserts.checkerror('zscore([1 2], 0, ''bad'')', _('Dimension argument must be a positive integer scalar, vector, or ''all''.'));
%=============================================================================
