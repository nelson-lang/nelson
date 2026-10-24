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
asserts.isequal(nargin('pca'), -2);
asserts.isequal(nargout('pca'), 6);
%=============================================================================
X = [1 2; 3 4; 5 8; 7 11];
[coeff, score, latent, tsquared, explained, mu] = pca(X);
asserts.isapprox(coeff, [0.538200080736440 0.842817105364675; ...
  0.842817105364675 -0.538200080736440], 1e-12);
asserts.isapprox(score, [-5.196572940009188 -0.241100972964155; ...
  -2.434538567806959 0.368133076292315; 2.013130015124621 -0.099033035924095; ...
  5.617981492691527 -0.027999067404065], 1e-12);
asserts.isapprox(latent, [22.848585622998481; 0.068081043668182], 1e-12);
asserts.isapprox(tsquared, [2.035714285714277; 2.25; 0.321428571428572; ...
  1.392857142857145], 1e-12);
asserts.isapprox(explained, [99.702919082175200; 0.297080917824795], 1e-12);
asserts.isequal(mu, [4 6.25]);
%=============================================================================
[coeff, score, latent, tsquared, explained, mu] = pca(X, 'NumComponents', 1);
asserts.isapprox(coeff, [0.538200080736440; 0.842817105364675], 1e-12);
asserts.isequal(size(score), [4 1]);
asserts.isapprox(score, [-5.196572940009188; -2.434538567806959; ...
  2.013130015124621; 5.617981492691527], 1e-12);
asserts.isapprox(latent, 22.848585622998481, 1e-12);
asserts.isequal(size(tsquared), [4 1]);
asserts.isapprox(tsquared, [1.181883673957228; 0.259402403979601; ...
  0.177371699266863; 1.381342222796309], 1e-12);
asserts.isequal(explained, 100);
asserts.isequal(mu, [4 6.25]);
%=============================================================================
[coeff, score, latent, tsquared, explained, mu] = pca([X; NaN 1]);
asserts.isapprox(coeff, [0.538200080736440 0.842817105364675; ...
  0.842817105364675 -0.538200080736440], 1e-12);
asserts.istrue(all(isnan(score(5, :))));
asserts.istrue(isnan(tsquared(5)));
asserts.isapprox(latent, [22.848585622998481; 0.068081043668182], 1e-12);
asserts.isapprox(explained, [99.702919082175200; 0.297080917824795], 1e-12);
asserts.isequal(mu, [4 6.25]);
%=============================================================================
[coeff, ~, latent] = pca(X, 'Centered', false);
asserts.isapprox(coeff, [0.538851601250530 0.842400707401021; ...
  0.842400707401021 -0.538851601250530], 1e-12);
asserts.isapprox(latent, [72.198925832933696; 0.051074167066319], 1e-12);
%=============================================================================
[coeff, ~, latent, tsquared, explained] = pca([1 2 3; 4 5 6], 'Economy', false);
asserts.isapprox(coeff(:, 1), [1; 1; 1] / sqrt(3), 1e-12);
asserts.isapprox(coeff' * coeff, eye(3), 1e-12);
asserts.isapprox(latent, [13.500000000000004; 0; 0], 1e-12);
asserts.isapprox(tsquared, [0.5; 0.5], 1e-12);
asserts.isapprox(explained, [100; 0; 0], 1e-12);
%=============================================================================
[coeff, ~, latent] = pca(X, 'Weights', [1 2 3 4]);
asserts.isapprox(coeff, [0.526795342179064 0.849992157292315; ...
  0.849992157292315 -0.526795342179064], 1e-12);
asserts.isapprox(latent, [19.101994192959829; 0.044672473706846], 1e-12);
%=============================================================================
[coeff, ~, latent] = pca(X, 'VariableWeights', 'variance');
asserts.isapprox(coeff, [0.842075358320684 0.842075358320684; ...
  0.539359889970594 -0.539359889970594], 1e-12);
asserts.isapprox(latent, [1.992794553591540; 0.007205446408460], 1e-12);
%=============================================================================
asserts.checkerror('pca()', _('Not enough input arguments.'));
asserts.checkerror('pca({1})', sprintf(_('%s must be a real numeric matrix.'), 'X'));
asserts.checkerror('pca([1 2], ''NumComponents'', 3)', _('NumComponents must be an integer scalar between 0 and the number of variables.'));
asserts.checkerror('pca([1 NaN], ''Rows'', ''all'')', _('Raw data contains NaN while Rows is all.'));
asserts.checkerror('pca([1 2], ''Algorithm'', ''als'')', sprintf(_('Unsupported %s.'), 'algorithm'));
asserts.checkerror('pca([1 2], ''Bad'', 1)', _('Unknown option.'));
%=============================================================================
