%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--REUSE PROCESS-->
%=============================================================================
asserts.isequal(nargin('lasso'), -3);
asserts.isequal(nargout('lasso'), 2);
%=============================================================================
X = [1 0 2; 2 1 0; 3 1 1; 4 2 3; 5 3 5; 6 5 8; 7 8 13; 8 13 21];
y = X * [0; 2; -1] + 0.5;
[B, FitInfo] = lasso(X, y, 'Lambda', [0 0.1 0.5]);
REF = [0.003221749676649 0.183820374299024 0.086502291255244; ...
       1.989012466606983 0.270772093896595 0.216806009769157; ...
       -0.994292575247762 0 0];
asserts.isapprox(B, REF, 1e-12);
asserts.isequal(size(B(:, 1:2)), [3 2]);
asserts.isequal(FitInfo.Lambda(1:2), [0 0.1]);
asserts.isequal(FitInfo.DF(1:2), [3 2]);
asserts.isapprox(FitInfo.Intercept, [0.493014012717696 0.180873428330936 ...
                                    0.841414899053629], 1e-12);
asserts.isapprox(FitInfo.MSE, [0.000031482672177 0.858167203801974 ...
                              1.144402923827055], 1e-12);
asserts.isequal(FitInfo.DF, [3 2 2]);
asserts.isequal(FitInfo.Alpha, 1);
asserts.isequal(FitInfo.Lambda, [0 0.1 0.5]);
%=============================================================================
[B2, FitInfo2] = lasso(X, y, 'Lambda', [0.1 0.5], 'Alpha', 0.5, 'Standardize', false);
REF = [0.079690172928500 0.086107916664589; ...
       1.414808909644195 0.403448314589393; ...
       -0.666121806835798 -0.048230428358845];
asserts.isapprox(B2, REF, 1e-12);
asserts.isapprox(FitInfo2.Intercept, [0.343364439826608 0.392816665205448], 1e-12);
%=============================================================================
[B3, FitInfo3] = lasso(X, y, 'Lambda', 0.1, 'Intercept', false);
asserts.isapprox(B3, [0.174339654240931; 1.376741671768426; ...
                     -0.663301729644353], 1e-12);
asserts.isequal(FitInfo3.Intercept, 0);
%=============================================================================
[B4, FitInfo4] = lasso(X, y, 'NumLambda', 4, 'LambdaRatio', 0);
asserts.isapprox(FitInfo4.Lambda, [1.508811046288377 0.015088110462884 ...
                                  0.000150881104629 0], 1e-12);
asserts.isequal(size(B4), [3 4]);
%=============================================================================
[Bw, FitInfoW] = lasso(X, y, 'Lambda', [0.1 0.5], 'Weights', [1 1 1 1 2 2 2 2]);
REF = [0.106186357511525 0.003794111446580; ...
       0.312334647767217 0.260244413710441; ...
       0 0];
asserts.isapprox(Bw, REF, 1e-12);
asserts.isapprox(FitInfoW.Intercept, [0.337641472726502 1.135800953355391], 1e-12);
%=============================================================================
[Bempty, FitInfoEmpty] = lasso(X, y, 'Lambda', []);
asserts.isequal(size(Bempty), [3 100]);
asserts.isequal(size(FitInfoEmpty.Lambda), [1 100]);
%=============================================================================
asserts.checkerror('lasso()', _('Not enough input arguments.'));
asserts.checkerror('lasso({1}, [1])', sprintf(_('%s must be a real numeric matrix.'), 'X'));
asserts.checkerror('lasso([1], {1})', sprintf(_('%s must be a real numeric vector.'), 'Y'));
asserts.checkerror('lasso([1; 2], [1])', sprintf(_('%s must have the same number of rows.'), 'X and Y'));
asserts.checkerror('lasso([1; 2], [1; 2], ''Lambda'', -1)', sprintf(_('%s must be a nonnegative numeric vector.'), 'LAMBDA'));
asserts.checkerror('lasso([1; 2], [1; 2], ''Alpha'', 2)', _('ALPHA must be in the range (0,1].'));
asserts.checkerror('lasso([1; 2], [1; 2], ''Standardize'', 2)', sprintf(_('%s must be a logical scalar.'), 'STANDARDIZE'));
asserts.checkerror('lasso([1; 2], [1; 2], ''Weights'', [1 -1])', _('WEIGHTS must contain nonnegative values with at least one positive value.'));
asserts.checkerror('lasso([1; 2], [1; 2], ''CV'', 5)', _('Invalid name-value argument name.'));
%=============================================================================
