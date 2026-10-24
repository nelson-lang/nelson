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
asserts.isequal(nargin('relieff'), -4);
asserts.isequal(nargout('relieff'), 2);
%=============================================================================
X = [0 0; 0 1; 1 0; 1 1; 3 0; 3 1];
y = [1; 1; 1; 1; 2; 2];
[idx, weights] = relieff(X, y, 1, 'Method', 'classification');
asserts.isequal(idx, [1 2]);
asserts.isapprox(weights, [0.555555555555555 -0.333333333333333], 1e-12);
%=============================================================================
[idx, weights] = relieff(X, {'a'; 'a'; 'a'; 'a'; 'b'; 'b'}, 1);
asserts.isequal(idx, [1 2]);
asserts.isapprox(weights, [0.555555555555555 -0.333333333333333], 1e-12);
%=============================================================================
[idx, weights] = relieff(X + 1, {'a'; 'a'; 'a'; 'a'; 'b'; 'b'}, 1, ...
  'CategoricalX', 'on', 'Prior', 'uniform');
asserts.isequal(idx, [1 2]);
asserts.isapprox(weights, [0.666666666666667 -0.666666666666667], 1e-12);
%=============================================================================
yr = [0; 0; 1; 1; 3; 3];
[idx, weights] = relieff(X, yr, 2);
asserts.isequal(idx, [1 2]);
asserts.isapprox(weights, [0.177146728908471 -0.130227486052146], 1e-12);
%=============================================================================
[idx, weights] = relieff(X, yr, 2, 'CategoricalX', [2], ...
  'Sigma', 1, 'Updates', [1 2 3]);
asserts.isequal(idx, [1 2]);
asserts.isapprox(weights, [-0.031235056790279 -0.260454972104293], 1e-12);
%=============================================================================
[idx, weights] = relieff(X, y, 1, 'Method', 'classification', ...
  'Prior', [0.5 0.5], 'Updates', 3);
asserts.isequal(size(idx), [1 2]);
asserts.isequal(size(weights), [1 2]);
%=============================================================================
asserts.checkerror('relieff()', _('Not enough input arguments.'));
asserts.checkerror('relieff({1}, y, 1)', sprintf(_('%s must be a real numeric matrix.'), 'X'));
asserts.checkerror('relieff([NaN 1], 1, 1)', sprintf(_('%s must contain finite values.'), 'X'));
asserts.checkerror('relieff(X, [1; 2], 1)', _('y must be a vector with one element per observation.'));
asserts.checkerror('relieff(X, y, 0)', sprintf(_('%s must be a positive integer scalar.'), 'k'));
asserts.checkerror('relieff(X, y, 1, ''Method'', ''bad'')', sprintf(_('Invalid %s value.'), 'Method'));
asserts.checkerror('relieff(X, {''a'';''b'';''c'';''d'';''e'';''f''}, 1, ''Method'', ''regression'')', _('Regression method requires a numeric response.'));
asserts.checkerror('relieff(X, y, 1, ''Updates'', [0 1])', _('Updates must contain valid observation indices.'));
asserts.checkerror('relieff(X, y, 1, ''CategoricalX'', [4])', sprintf(_('%s contains invalid predictor indices.'), 'CategoricalX'));
asserts.checkerror('relieff(X, y, 1, ''Method'', ''classification'', ''Prior'', [1 -1])', sprintf(_('%s must contain nonnegative finite values with positive sum.'), 'Prior'));
asserts.checkerror('relieff(X, yr, 1, ''Sigma'', 0)', sprintf(_('%s must be a positive scalar.'), 'Sigma'));
asserts.checkerror('relieff(X, y, 1, ''Unknown'', 1)', _('Unknown option.'));
%=============================================================================
