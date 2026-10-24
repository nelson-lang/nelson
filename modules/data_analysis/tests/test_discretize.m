%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
X = [1 2 3 4 5 NaN];
Y = discretize(X, [0 2 4 6]);
asserts.isequal(Y, [1 2 2 3 3 NaN]);
%=============================================================================
Y = discretize([0 2 4 6], [0 2 4 6], 'IncludedEdge', 'right');
asserts.isequal(Y, [1 1 2 3]);
%=============================================================================
Y = discretize([5 15 25 NaN], [0 10 20 30], [100 200 300]);
asserts.isequal(Y, [100 200 300 NaN]);
%=============================================================================
X = [90 95 5 100 65 10 45 70 99 98];
edges = 0:25:100;
values = edges(2:end);
Y = discretize(X, edges, values);
asserts.isequal(Y, [100 100 25 100 75 25 50 75 100 100]);
%=============================================================================
X = 1:2:11;
edges = [1 3 4 7 10 11];
Y = discretize(X, edges, 'IncludedEdge', 'right');
asserts.isequal(Y, [1 1 3 3 4 5]);
Z = discretize(X, edges);
asserts.isequal(Z, [1 2 3 4 4 5]);
%=============================================================================
Y = discretize([5 15 25 NaN], [0 10 20 30], 'categorical', {'small', 'medium', 'large'});
asserts.istrue(iscategorical(Y));
asserts.isequal(categories(Y), {'small'; 'medium'; 'large'});
asserts.isequal(cellstr(Y), {'small', 'medium', 'large', '<undefined>'});
%=============================================================================
Y = discretize([5 15 25], [0 10 20 30], 'categorical', ["small" "medium" "large"], ...
  IncludedEdge= "left");
asserts.isequal(cellstr(Y), {'small', 'medium', 'large'});
%=============================================================================
X = [-3.5; -2.5; -1.5; -0.5; 0.5; 1.5; 2.5; 3.5];
edges = -3:3;
Y = discretize(X, edges, 'categorical', ...
  {'-3sigma', '-2sigma', '-sigma', 'sigma', '2sigma', '3sigma'});
asserts.istrue(iscategorical(Y));
asserts.isequal(categories(Y), {'-3sigma'; '-2sigma'; '-sigma'; 'sigma'; '2sigma'; '3sigma'});
asserts.isequal(cellstr(Y), {'<undefined>'; '-3sigma'; '-2sigma'; '-sigma'; ...
  'sigma'; '2sigma'; '3sigma'; '<undefined>'});
asserts.isequal(isundefined(Y), [true; false; false; false; false; false; false; true]);
asserts.isequal(nnz(Y == '-sigma' | Y == 'sigma'), 2);
%=============================================================================
rng(1);
X = randn(1000, 1);
edges = std(X) * (-3:3);
Y = discretize(X, edges, 'categorical', ...
  {'-3sigma', '-2sigma', '-sigma', 'sigma', '2sigma', '3sigma'});
ratio = nnz(Y == '-sigma' | Y == 'sigma') / numel(Y);
asserts.istrue(abs(ratio - 0.695) < eps);
asserts.istrue(any(isundefined(Y)));
%=============================================================================
X = datetime(2016, 1, [298; 331; 47; 334; 231; 36; 102; 200; 350; 353]);
Y = discretize(X, 'month', 'categorical');
asserts.istrue(iscategorical(Y));
asserts.isequal(cellstr(Y), {'Oct-2016'; 'Nov-2016'; 'Feb-2016'; 'Nov-2016'; ...
  'Aug-2016'; 'Feb-2016'; 'Apr-2016'; 'Jul-2016'; 'Dec-2016'; 'Dec-2016'});
asserts.isequal(size(Y), [10 1]);
%=============================================================================
X = [datetime(2016, 1, 31), NaT, datetime(2016, 3, 1)];
Y = discretize(X, 'month', 'categorical');
asserts.isequal(cellstr(Y), {'Jan-2016', '<undefined>', 'Mar-2016'});
asserts.isequal(categories(Y), {'Jan-2016'; 'Feb-2016'; 'Mar-2016'});
%=============================================================================
X = minutes([30; 90; 150; 45; 15; 75; 30; 20; 210; 150]);
Y = discretize(X, 'hour', 'categorical', 'm');
asserts.istrue(iscategorical(Y));
asserts.isequal(cellstr(Y), {'[0 min, 60 min)'; '[60 min, 120 min)'; ...
  '[120 min, 180 min)'; '[0 min, 60 min)'; '[0 min, 60 min)'; ...
  '[60 min, 120 min)'; '[0 min, 60 min)'; '[0 min, 60 min)'; ...
  '[180 min, 240 min]'; '[120 min, 180 min)'});
%=============================================================================
Y = discretize(X, 'hour', 'categorical', 'hh:mm:ss');
asserts.isequal(cellstr(Y), {'[00:00:00, 01:00:00)'; '[01:00:00, 02:00:00)'; ...
  '[02:00:00, 03:00:00)'; '[00:00:00, 01:00:00)'; '[00:00:00, 01:00:00)'; ...
  '[01:00:00, 02:00:00)'; '[00:00:00, 01:00:00)'; '[00:00:00, 01:00:00)'; ...
  '[03:00:00, 04:00:00]'; '[02:00:00, 03:00:00)'});
%=============================================================================
asserts.checkerror('discretize([1 2], [0 1 1])', _('Edges must be a strictly increasing vector.'));
asserts.checkerror('discretize([1 2], [0 1 2], [10 20 30])', ...
  _('Number of values must match the number of bins.'));
%=============================================================================
G = discretize([1 2 3 4 5 NaN], [0 2 4 6]);
asserts.isequal(G, [1 2 2 3 3 NaN]);
H = discretize([5 15 25 NaN], [0 10 20 30], [100 200 300]);
asserts.isequal(H, [100 200 300 NaN]);
K = discretize([5 15 25 NaN], [0 10 20 30], 'categorical', {'small', 'medium', 'large'});
asserts.istrue(iscategorical(K));
asserts.isequal(categories(K), {'small'; 'medium'; 'large'});
asserts.isequal(cellstr(K), {'small', 'medium', 'large', '<undefined>'});
asserts.checkerror('discretize([1 2], [0 1], [10 20])', _('Number of values must match the number of bins.'));
%=============================================================================
% A scalar second argument is a number of bins: edges are picked automatically
% and match the histcounts bin picker.
asserts.isequal(discretize([1 5 10], 3), [1 2 3]);
%=============================================================================
[Y, E] = discretize([1 5 10 7 3], 4);
asserts.isequal(Y, [1 3 4 3 2]);
asserts.isapprox(E, [0 2.5 5 7.5 10], 1e-10);
[~, HE] = histcounts([1 5 10 7 3], 4);
asserts.isequal(E, HE);
%=============================================================================
% NaN values keep their NaN bin index when a number of bins is requested.
asserts.isequal(discretize([1 5 NaN 10], 3), [1 2 NaN 3]);
%=============================================================================
% The second output returns the edges, including for explicit edge vectors.
[Y, E] = discretize([1 2 3 4 5], [0 2 4 6]);
asserts.isequal(Y, [1 2 2 3 3]);
asserts.isequal(E, [0 2 4 6]);
%=============================================================================
% A number of bins combined with output values.
[Y, E] = discretize([1 5 10], 3, [10 20 30]);
asserts.isequal(Y, [10 20 30]);
asserts.isapprox(E, [0 4 8 12], 1e-10);
%=============================================================================
% Constant data with a requested number of bins.
[~, E] = discretize([3 3 3], 4);
asserts.isapprox(E, [2.5 2.75 3 3.25 3.5], 1e-12);
%=============================================================================
asserts.checkerror('discretize([1 2 3], 0)', sprintf(_('%s must be a positive integer scalar.'), 'Number of bins'));
asserts.checkerror('discretize([1 2 3], 2.5)', sprintf(_('%s must be a positive integer scalar.'), 'Number of bins'));
%=============================================================================
