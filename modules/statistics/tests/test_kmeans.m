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
asserts.isequal(nargin('kmeans'), -1);
asserts.isequal(nargout('kmeans'), -1);
%=============================================================================
X = [0 0; 0 1; 5 5; 5 6];
[idx, C, sumd, D] = kmeans(X, 2, 'Start', [0 0; 5 5]);
asserts.isequal(idx, [1; 1; 2; 2]);
asserts.isapprox(C, [0 0.5; 5 5.5], 1e-12);
asserts.isapprox(sumd, [0.5; 0.5], 1e-12);
REF_D = [0.25 55.25; 0.25 45.25; 45.25 0.25; 55.25 0.25];
asserts.isapprox(D, REF_D, 1e-12);
%=============================================================================
[idx, C, sumd] = kmeans(X, 2, 'Start', [0 0; 5 5], 'Distance', 'cityblock');
asserts.isequal(idx, [1; 1; 2; 2]);
asserts.isapprox(C, [0 0.5; 5 5.5], 1e-12);
asserts.isapprox(sumd, [1; 1], 1e-12);
X = [0; 100; 101; 102; 200; 201; 202];
[idx, C] = kmeans(X, 2, 'Start', [0; 200], 'Distance', 'cityblock', 'OnlinePhase', 'off');
asserts.isequal(idx, [1; 1; 1; 1; 2; 2; 2]);
asserts.isapprox(C, [100.5; 201], 1e-12);
%=============================================================================
X = [1 0; 2 0; 0 1; 0 2];
[idx, C] = kmeans(X, 2, 'Start', [1 0; 0 1], 'Distance', 'cosine');
asserts.isequal(idx, [1; 1; 2; 2]);
asserts.isapprox(C, [1 0; 0 1], 1e-12);
[idx, C] = kmeans([0 0; 0 1; 1 1], 2, 'Start', [0 0; 1 1], 'Distance', 'hamming');
asserts.isequal(idx, [1; 1; 2]);
asserts.isequal(C, [0 1; 1 1]);
idx = kmeans(X, 2, 'Start', [1 0; 0 1], 'Distance', 'hamming');
asserts.isequal(idx, [1; 1; 2; 2]);
%=============================================================================
X = [1 2; 2 3; 10 9; 11 10];
[idx, C] = kmeans(X, 2, 'Start', [1 2; 10 9], 'Distance', 'correlation');
asserts.isequal(idx, [1; 1; 2; 2]);
asserts.isapprox(sum(C, 2), [0; 0], 1e-12);
%=============================================================================
X = [0 0; 0 1; 5 5; 5 6];
rng(10);
[idx1, C1] = kmeans(X, 2, 'Start', 'plus', 'Replicates', 2);
rng(10);
[idx2, C2] = kmeans(X, 2, 'Start', 'plus', 'Replicates', 2);
asserts.isequal(idx1, idx2);
asserts.isequal(C1, C2);
%=============================================================================
rng(11);
[idx, C] = kmeans(X, 2, 'Start', 'sample');
asserts.isequal(size(idx), [4 1]);
asserts.isequal(size(C), [2 2]);
rng(12);
[idx, C] = kmeans(X, 2, 'Start', 'uniform');
asserts.isequal(size(idx), [4 1]);
asserts.isequal(size(C), [2 2]);
rng(13);
[idx, C] = kmeans(X, 2, 'Start', 'cluster');
asserts.isequal(size(idx), [4 1]);
asserts.isequal(size(C), [2 2]);
%=============================================================================
values = [0.113 0.114 0.115 0.116 0.117 0.118 0.119 0.120 0.121 0.122 ...
  0.123 0.124 0.125 0.126 0.127 0.128 0.129 0.130 0.131 0.132 ...
  0.133 0.134 0.135 0.136 0.137 0.138 0.139 0.140 0.141 0.142 ...
  0.143 0.144 0.145 0.146 0.147 0.148 0.149 0.150 0.151 0.152 ...
  0.153 0.154 0.155 0.156 0.157 0.158 0.159 0.160 0.162 0.163 0.174];
counts = [4 7 19 31 24 16 22 18 22 22 14 14 11 11 12 15 6 8 6 6 ...
  10 6 5 7 13 8 13 17 22 12 9 19 21 16 15 18 13 20 7 12 7 7 9 4 ...
  6 1 4 1 1 1 2];
K = zeros(sum(counts), 2);
row = 1;
for j = 1:numel(values)
  count = counts(j);
  K(row:row + count - 1, :) = values(j) * ones(count, 2);
  row = row + count;
end
m = 6;
for i = 1:10
  [idx, centroid] = kmeans(K, m);
  asserts.isequal(size(idx), [594 1]);
  asserts.isequal(size(centroid), [6 2]);
end
%=============================================================================
S = zeros(2, 2, 2);
S(:, :, 1) = [0 0; 5 5];
S(:, :, 2) = [5 5; 0 0];
[idx, C] = kmeans(X, [], 'Start', S);
asserts.isequal(size(idx), [4 1]);
asserts.isequal(size(C), [2 2]);
%=============================================================================
opts = statset('Display', 'off', 'MaxIter', 5);
[idx, C] = kmeans(X, 2, 'Start', [0 0; 5 5], 'Options', opts);
asserts.isequal(idx, [1; 1; 2; 2]);
asserts.isapprox(C, [0 0.5; 5 5.5], 1e-12);
opts = statset('kmeans');
asserts.isequal(opts.Display, 'off');
asserts.isequal(opts.MaxIter, 100);
opts2 = statset(opts, 'MaxIter', 7);
asserts.isequal(opts2.MaxIter, 7);
opts3 = statset(opts, statset('TolX', 1e-8));
asserts.isequal(opts3.Display, 'off');
asserts.isequal(opts3.TolX, 1e-8);
opts = statset('UseParallel', true, 'UseSubstreams', true, 'Streams', {});
[idx, C] = kmeans(X, 2, 'Start', [0 0; 5 5], 'Options', opts);
asserts.isequal(idx, [1; 1; 2; 2]);
asserts.isapprox(C, [0 0.5; 5 5.5], 1e-12);
s = RandStream('mt19937ar', 'Seed', 12);
opts = statset('kmeans', 'Streams', s);
stateBeforeKmeans = s.State;
[idx0, C0] = kmeans(X, 2, 'Replicates', 2, 'Options', opts, 'MaxIter', 5);
stateAfterKmeans = s.State;
reset(s, 12);
[idx1, C1] = kmeans(X, 2, 'Replicates', 2, 'Options', opts, 'MaxIter', 5);
asserts.isequal(idx0, idx1);
asserts.isequal(C0, C1);
asserts.isfalse(isequal(stateBeforeKmeans, stateAfterKmeans));
Xbig = [X; X + 0.1; X + 0.2; X + 0.3];
opts = statset('UseParallel', false);
[idx0, C0, sumd0, D0] = kmeans(Xbig, 2, 'Start', [0 0; 5 5], 'Options', opts);
opts = statset('UseParallel', true);
[idx1, C1, sumd1, D1] = kmeans(Xbig, 2, 'Start', [0 0; 5 5], 'Options', opts);
asserts.isequal(idx0, idx1);
asserts.isequal(C0, C1);
asserts.isequal(sumd0, sumd1);
asserts.isequal(D0, D1);
%=============================================================================
Xnan = [0 0; 0 1; NaN NaN; 5 5; 5 6];
[idx, C, sumd, D] = kmeans(Xnan, 2, 'Start', [0 0; 5 5]);
asserts.isequal(idx(1:2), [1; 1]);
asserts.istrue(isnan(idx(3)));
asserts.isequal(idx(4:5), [2; 2]);
asserts.isapprox(C, [0 0.5; 5 5.5], 1e-12);
asserts.istrue(all(isnan(D(3, :))));
asserts.isapprox(sumd, [0.5; 0.5], 1e-12);
T = table([0; 0; 5; 5], [0; 1; 5; 6], 'VariableNames', {'A', 'B'});
[idx, C] = kmeans(T, 2, 'Start', [0 0; 5 5]);
asserts.isequal(idx, [1; 1; 2; 2]);
asserts.isapprox(C, [0 0.5; 5 5.5], 1e-12);
%=============================================================================
lastwarn('');
kmeans(X, 2, 'Start', [0 0; 5 5], 'MaxIter', 1);
msg = lastwarn();
asserts.isequal(msg, _('Failed to converge in 1 iterations.'));
lastwarn('');
%=============================================================================
txt = evalc('kmeans(X, 2, ''Start'', [0 0; 5 5], ''Display'', ''iter'', ''MaxIter'', 2);');
asserts.istrue(contains(txt, 'phase 1'));
asserts.istrue(contains(txt, 'points reassigned'));
asserts.istrue(contains(txt, 'Best total sum of distances'));
txt = evalc('kmeans(X, 2, ''Start'', [0 0; 5 5], ''Display'', ''iter'', ''OnlinePhase'', ''off'');');
asserts.isfalse(contains(txt, 'phase 2'));
txt = evalc('kmeans(X, 2, ''Start'', [0 0; 5 5], ''Display'', ''final'');');
asserts.istrue(contains(txt, 'iterations'));
asserts.istrue(contains(txt, 'Best total sum of distances'));
%=============================================================================
X = [0; 1; 10; 11];
[idx, C] = kmeans(X, 3, 'Start', [0; 10; 100], 'EmptyAction', 'singleton');
asserts.isequal(numel(unique(idx)), 3);
asserts.istrue(all(isfinite(C)));
%=============================================================================
X = [4 6; 5 1; 4 3; 3 4; 4 9; 9 3; 9 5; 5 10];
start = X(1:3, :);
lastwarn('');
[idx0, C0, sumd0] = kmeans(X, 3, 'Start', start, 'MaxIter', 1, 'OnlinePhase', 'off');
lastwarn('');
[idx1, C1, sumd1] = kmeans(X, 3, 'Start', start, 'MaxIter', 1, 'OnlinePhase', 'on');
lastwarn('');
asserts.istrue(sum(sumd1) < sum(sumd0));
asserts.istrue(~isequal(idx0, idx1) || ~isequal(C0, C1));
%=============================================================================
asserts.checkerror('kmeans()', _('Not enough input arguments.'));
asserts.checkerror('kmeans([1 2], 0)', sprintf(_('%s must be a positive integer scalar.'), 'K'));
asserts.checkerror('kmeans([1 2], 1, ''Distance'')', _('Name-value arguments must be paired.'));
asserts.checkerror('kmeans([1 2], 1, ''Distance'', ''unknown'')', sprintf(_('Invalid %s value.'), 'Distance'));
asserts.checkerror('kmeans([1 2], 1, ''Distance'', ''c'')', sprintf(_('Ambiguous %s value.'), 'Distance'));
asserts.checkerror('kmeans([1 2], 1, ''Start'', ''bad'')', sprintf(_('Invalid %s value.'), 'Start'));
asserts.checkerror('kmeans([1 2], 1, ''Replicates'', 1.5)', sprintf(_('%s must be a positive integer scalar.'), 'Replicates'));
asserts.checkerror('kmeans([0; 1], 3, ''Start'', ''sample'')', _('K must not exceed the number of rows in X.'));
asserts.checkerror('kmeans([0; 10; 11], 3, ''Start'', [0; 10; 100], ''EmptyAction'', ''error'')', _('Empty cluster created.'));
asserts.checkerror('kmeans([NaN; NaN], 1)', _('X must have at least one row.'));
asserts.checkerror('kmeans([Inf; 1], 1)', sprintf(_('%s must not contain Inf values.'), 'X'));
T = table([1; 2], {'a'; 'b'}, 'VariableNames', {'A', 'B'});
asserts.checkerror('kmeans(T, 1)', sprintf(_('%s must be numeric or logical.'), 'Table variables'));
%=============================================================================
