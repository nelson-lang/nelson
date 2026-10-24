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
asserts.isequal(nargin('mdscale'), -3);
asserts.isequal(nargout('mdscale'), 3);
%=============================================================================
X = [0 0; 1 0; 0 2; 2 2];
Dv = pdist(X);
D = squareform(Dv);
[Y, stress, disparities] = mdscale(Dv, 2);
Yexpected = [-1.238218880541527 0.171213328542168; ...
  -0.753450348148581 -0.703429152704649; ...
  0.511066081952108 1.140750393328058; ...
  1.480603146737999 -0.608534569165577];
asserts.isapprox(Y, Yexpected, 1e-12);
asserts.isapprox(stress, 0, 1e-6, 1e-3);
asserts.isapprox(disparities, D, 1e-12);
asserts.isapprox(pdist(Y), Dv, 1e-12);
%=============================================================================
[Ymetric, stressMetric, disparitiesMetric] = mdscale(D, 2, 'Criterion', 'metricstress');
asserts.isapprox(Ymetric, Yexpected, 1e-12);
asserts.isapprox(stressMetric, 0, 1e-6, 1e-3);
asserts.isapprox(disparitiesMetric, D, 1e-12);
%=============================================================================
criteria = {'strain', 'sammon', 'sstress', 'metricsstress'};
for k = 1:length(criteria)
  [Yc, sc, dc] = mdscale(D, 2, 'Criterion', criteria{k});
  asserts.isequal(size(Yc), [4 2]);
  asserts.isapprox(sc, 0, 1e-6, 1e-3);
  asserts.isapprox(dc, D, 1e-12);
end
%=============================================================================
Start = [0 0; 1 0; 0 1; 1 1];
[Ys, ss] = mdscale(D, [], 'Start', Start, 'Criterion', 'metricstress');
asserts.isequal(size(Ys), [4 2]);
asserts.isapprox(ss, 0, 1e-6, 1e-3);
%=============================================================================
S = 1 - D .^ 2 / 10;
S(1:size(S, 1) + 1:end) = 1;
[Ysim, stressSim] = mdscale(S, 2, 'Criterion', 'metricstress');
asserts.isapprox(Ysim, Yexpected / sqrt(10), 1e-12);
asserts.isapprox(stressSim, 0, 1e-6, 1e-3);
%=============================================================================
U = triu(D);
[Yu, stressU] = mdscale(U, 2, 'Criterion', 'metricstress');
asserts.isapprox(Yu, Yexpected, 1e-12);
asserts.isapprox(stressU, 0, 1e-6, 1e-3);
%=============================================================================
W = ones(4);
W(1, 2) = 0;
W(2, 1) = 0;
[Yw, sw, dw] = mdscale(D, 2, 'Weights', W, 'Start', 'random', ...
  'Options', statset('MaxIter', 5));
asserts.isequal(size(Yw), [4 2]);
asserts.istrue(isfinite(sw));
asserts.isequal(size(dw), [4 4]);
%=============================================================================
Dnan = D;
Dnan(1, 2) = NaN;
Dnan(2, 1) = NaN;
[Ynan, snan] = mdscale(Dnan, 2, 'Start', 'random', ...
  'Options', statset('MaxIter', 5));
asserts.isequal(size(Ynan), [4 2]);
asserts.istrue(isfinite(snan));
%=============================================================================
[Yr, sr] = mdscale(D, 2, 'Start', 'random', 'Replicates', 2, ...
  'Options', statset('MaxIter', 5, 'TolFun', 1e-6, 'TolX', 1e-4));
asserts.isequal(size(Yr), [4 2]);
asserts.istrue(isfinite(sr));
%=============================================================================
asserts.checkerror('mdscale()', _('Not enough input arguments.'));
asserts.checkerror('mdscale({1}, 2)', sprintf(_('%s must be a real numeric vector or matrix.'), 'D'));
asserts.checkerror('mdscale([0 Inf; Inf 0], 2)', _('D must contain nonnegative finite dissimilarities.'));
asserts.checkerror('mdscale([0 -1; -1 0], 2)', _('D must contain nonnegative finite dissimilarities.'));
asserts.checkerror('mdscale([0 1 2; 1 0 3], 2)', _('D must be square or a valid distance vector.'));
asserts.checkerror('mdscale([0 1; 2 0], 2)', _('D must be symmetric or upper triangular.'));
asserts.checkerror('mdscale(D, 0)', _('p must be an integer scalar between 1 and the number of points.'));
asserts.checkerror('mdscale(D, 5)', _('p must be an integer scalar between 1 and the number of points.'));
asserts.checkerror('mdscale(D, 2, ''Criterion'', ''bad'')', sprintf(_('Invalid %s value.'), 'Criterion'));
asserts.checkerror('mdscale(D, 2, ''Weights'', [1 2])', _('Weights must have the same size as D.'));
asserts.checkerror('mdscale(D, 2, ''Start'', ''bad'')', sprintf(_('%s has an invalid value.'), 'Start'));
asserts.checkerror('mdscale(D, 2, ''Replicates'', -1)', sprintf(_('%s must be a nonnegative integer scalar.'), 'Replicates'));
asserts.checkerror('mdscale(D, 2, ''Options'', 1)', sprintf(_('%s must be a scalar structure.'), 'Options'));
%=============================================================================
