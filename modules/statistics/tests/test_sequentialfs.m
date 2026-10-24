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
asserts.isequal(nargin('sequentialfs'), -2);
asserts.isequal(nargout('sequentialfs'), 2);
%=============================================================================
X = [1 0 0; 2 0 1; 3 1 0; 4 1 1; 5 2 0; 6 2 1];
y = X(:, 1);
fun = @(Xt, yt, Xv, yv) sum((yv - Xv(:, 1)) .^ 2);
[tf, history] = sequentialfs(fun, X, y, 'CV', 'resubstitution', 'NFeatures', 1);
asserts.isequal(tf, [true false false]);
asserts.isequal(history.In, [false false false; true false false]);
asserts.isequal(history.Crit, [Inf 0]);
%=============================================================================
[tf, history] = sequentialfs(fun, X, y, 'CV', 3, 'NFeatures', 1);
asserts.isequal(tf, [true false false]);
asserts.isequal(history.In, [false false false; true false false]);
asserts.isequal(history.Crit, [Inf 0]);
%=============================================================================
filterFun = @(Z) - sum(var(Z));
[tf, history] = sequentialfs(filterFun, X, 'CV', 'none', 'NFeatures', 2);
asserts.isequal(tf, [true true false]);
asserts.isequal(history.In, [false false false; true false false; true true false]);
asserts.isapprox(history.Crit, [Inf -3.5 -4.3], 1e-12);
%=============================================================================
backFun = @(Z) sum(var(Z));
[tf, history] = sequentialfs(backFun, X, 'CV', 'none', ...
  'Direction', 'backward', 'NFeatures', 1, 'NullModel', true);
asserts.isequal(tf, [false false true]);
asserts.isequal(history.In, [true true true; false true true; false false true]);
asserts.isapprox(history.Crit, [4.6 1.1 0.3], 1e-12);
%=============================================================================
[tf, history] = sequentialfs(filterFun, X, 'CV', 'none', ...
  'KeepIn', 2, 'KeepOut', 3, 'NFeatures', 2);
asserts.isequal(tf, [true true false]);
asserts.istrue(all(history.In(:, 2)));
asserts.isfalse(any(history.In(:, 3)));
%=============================================================================
asserts.checkerror('sequentialfs()', _('Not enough input arguments.'));
asserts.checkerror('sequentialfs(1, X)', sprintf(_('%s must be a function handle.'), 'FUN'));
asserts.checkerror('sequentialfs(filterFun, {})', sprintf(_('%s must be a real numeric matrix.'), 'First data input'));
asserts.checkerror('sequentialfs(filterFun, [], ''CV'', ''none'')', sprintf(_('%s must be nonempty.'), 'First data input'));
asserts.checkerror('sequentialfs(filterFun, X, y(1:2), ''CV'', ''none'')', sprintf(_('%s must have the same number of rows.'), 'All data inputs'));
asserts.checkerror('sequentialfs(filterFun, X, ''Direction'', ''bad'')', sprintf(_('Invalid %s value.'), 'Direction'));
asserts.checkerror('sequentialfs(filterFun, X, ''KeepIn'', 4)', sprintf(_('%s contains invalid feature indices.'), 'KeepIn'));
asserts.checkerror('sequentialfs(filterFun, X, ''KeepIn'', 1, ''KeepOut'', 1)', _('KeepIn and KeepOut must not overlap.'));
asserts.checkerror('sequentialfs(filterFun, X, ''NFeatures'', 4)', _('NFeatures must be an integer scalar between 0 and the number of features.'));
asserts.checkerror('sequentialfs(filterFun, X, ''CV'', ''bad'')', sprintf(_('%s has an invalid value.'), 'CV'));
asserts.checkerror('sequentialfs(filterFun, X, ''Options'', 1)', sprintf(_('%s must be a scalar structure.'), 'Options'));
%=============================================================================
