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
asserts.isequal(nargin('gmdistribution'), 3);
asserts.isequal(nargout('gmdistribution'), 1);
asserts.isequal(nargin('fitgmdist'), -3);
asserts.isequal(nargout('fitgmdist'), 1);
%=============================================================================
gm0 = gmdistribution([0; 10], cat(3, 1, 4), [0.25 0.75]);
asserts.isequal(gm0.NumComponents, 2);
asserts.isequal(gm0.NumVariables, 1);
asserts.isequal(gm0.ComponentProportion, [0.25 0.75]);
Y0 = pdf(gm0, [0; 10; 5]);
asserts.isapprox(Y0, [0.099736127620176; 0.149603355150537; 0.006573484364967], 1e-12);
P0 = posterior(gm0, [0; 10; 5]);
asserts.isapprox(P0, [0.999994410051490 0.000005589948510; ...
  4.33777511966791e-14 0.999999999999957; 0.000056542292953 0.999943457707047], 1e-12);
%=============================================================================
X = [0; 1; 10; 11];
gm = fitgmdist(X, 2, 'Start', [1; 1; 2; 2], 'MaxIter', 20, 'TolFun', 1e-8);
asserts.isapprox(gm.mu, [0.5; 10.5], 1e-12);
asserts.isapprox(gm.Sigma, cat(3, 0.250001, 0.250001), 1e-12);
asserts.isapprox(gm.ComponentProportion, [0.5 0.5], 1e-12);
asserts.isapprox(gm.NegativeLogLikelihood, 5.675754132834690, 1e-12);
asserts.isapprox(gm.AIC, 21.351508265669381, 1e-12);
asserts.isapprox(gm.BIC, 18.282980071268835, 1e-12);
asserts.istrue(gm.Converged);
asserts.isequal(gm.NumComponents, 2);
asserts.isequal(gm.NumVariables, 1);
[idx, nlogL, P, logpdf] = cluster(gm, X);
asserts.isequal(idx, [1; 1; 2; 2]);
asserts.isapprox(nlogL, 5.675754132834690, 1e-12);
asserts.isapprox(P, [1 0; 1 0; 0 1; 0 1], 1e-12);
asserts.isapprox(logpdf, [-1.418938533208673; -1.418938533208673; -1.418938533208673; -1.418938533208673], 1e-12);
asserts.isapprox(pdf(gm, X), [0.241970724518176; 0.241970724518176; 0.241970724518176; 0.241970724518176], 1e-12);
%=============================================================================
X2 = [0 0; 0 1; 10 10; 10 11];
gm2 = fitgmdist(X2, 2, 'Start', [1; 1; 2; 2], 'CovarianceType', 'diagonal', ...
  'SharedCovariance', true, 'MaxIter', 10);
asserts.isapprox(gm2.mu, [0 0.5; 10 10.5], 1e-12);
asserts.isequal(gm2.CovarianceType, 'diagonal');
asserts.istrue(gm2.SharedCovariance);
[idx2, ~, P2] = cluster(gm2, X2);
asserts.isequal(idx2, [1; 1; 2; 2]);
asserts.isapprox(P2, [1 0; 1 0; 0 1; 0 1], 1e-12);
%=============================================================================
opts = statset('fitgmdist');
asserts.isequal(opts.Display, 'off');
asserts.isequal(opts.MaxIter, 100);
asserts.isequal(opts.TolFun, 1e-6);
gm3 = fitgmdist(X, 2, 'Start', [1; 1; 2; 2], 'Options', statset('fitgmdist', 'MaxIter', 5));
asserts.isequal(size(gm3.mu), [2 1]);
%=============================================================================
rng(42);
R = random(gm, 5);
asserts.isequal(size(R), [5 1]);
rng(42);
R2 = random(gm, 5);
asserts.isequal(R, R2);
%=============================================================================
asserts.checkerror('gmdistribution([0; 1], -ones(1, 1, 2))', _('SIGMA must contain positive definite covariance matrices.'));
asserts.checkerror('fitgmdist()', _('Not enough input arguments.'));
asserts.checkerror('fitgmdist([], 1)', sprintf(_('%s must contain finite values.'), 'X'));
asserts.checkerror('fitgmdist([1; 2], 3)', _('K must not exceed the number of rows in X.'));
asserts.checkerror('fitgmdist([1; 2], 0)', sprintf(_('%s must be a positive integer scalar.'), 'K'));
asserts.checkerror('fitgmdist([1; 2], 1, ''Start'')', _('Name-value arguments must be paired.'));
asserts.checkerror('fitgmdist([1; 2], 1, ''Unknown'', 1)', _('Invalid name-value argument name.'));
asserts.checkerror('fitgmdist([1; 2], 1, ''Start'', ''bad'')', sprintf(_('Invalid %s value.'), 'Start'));
asserts.checkerror('fitgmdist([1; 2], 1, ''CovarianceType'', ''bad'')', _('CovarianceType must be full or diagonal.'));
asserts.checkerror('fitgmdist([1; 2], 1, ''SharedCovariance'', 2)', sprintf(_('%s must be true or false.'), 'SharedCovariance'));
asserts.checkerror('random(gm, -1)', sprintf(_('%s must be a nonnegative integer scalar.'), 'N'));
%=============================================================================
