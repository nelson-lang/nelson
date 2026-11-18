%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% The Elkan and Hamerly acceleration engines must return exactly the same
% results as the legacy assignment path. The 'Engine' name-value selects the
% engine so the three can be compared on the same inputs.
%=============================================================================
engines = {'elkan', 'hamerly'};
%=============================================================================
% Deterministic start, full outputs.
X = [0 0; 0 1; 5 5; 5 6];
[idxL, CL, sumdL, DL] = kmeans(X, 2, 'Start', [0 0; 5 5], 'Engine', 'legacy');
for e = 1:numel(engines)
  [idxE, CE, sumdE, DE] = kmeans(X, 2, 'Start', [0 0; 5 5], 'Engine', engines{e});
  asserts.isequal(idxE, idxL);
  asserts.isequal(CE, CL);
  asserts.isequal(sumdE, sumdL);
  asserts.isequal(DE, DL);
end
asserts.isequal(idxL, [1; 1; 2; 2]);
%=============================================================================
% Larger clustered data over several configurations.
rng(0);
n = 400;
X = [rand(n, 5); rand(n, 5) + 2; rand(n, 5) + 4; rand(n, 5) + 6; rand(n, 5) + 8];
configs = { ...
  {5, {'Start', 'plus', 'Replicates', 3}}, ...
  {6, {'Start', 'sample'}}, ...
  {6, {'Start', 'sample', 'OnlinePhase', 'off'}}, ...
  {5, {'Start', 'plus', 'Options', statset('UseParallel', true)}} };
for c = 1:numel(configs)
  m = configs{c}{1};
  args = configs{c}{2};
  rng(20 + c);
  [idxL, CL, sumdL, DL] = kmeans(X, m, args{:}, 'Engine', 'legacy');
  for e = 1:numel(engines)
    rng(20 + c);
    [idxE, CE, sumdE, DE] = kmeans(X, m, args{:}, 'Engine', engines{e});
    asserts.isequal(idxE, idxL);
    asserts.isequal(CE, CL);
    asserts.isequal(sumdE, sumdL);
    asserts.isequal(DE, DL);
  end
end
%=============================================================================
% Singleton empty-action repair path.
Xs = [0; 1; 10; 11];
[idxL, CL] = kmeans(Xs, 3, 'Start', [0; 10; 100], 'EmptyAction', 'singleton', 'Engine', 'legacy');
for e = 1:numel(engines)
  [idxE, CE] = kmeans(Xs, 3, 'Start', [0; 10; 100], 'EmptyAction', 'singleton', 'Engine', engines{e});
  asserts.isequal(idxE, idxL);
  asserts.isequal(CE, CL);
end
%=============================================================================
% Rows with NaN are dropped identically.
Xnan = [0 0; 0 1; NaN NaN; 5 5; 5 6];
[idxL, CL, sumdL, DL] = kmeans(Xnan, 2, 'Start', [0 0; 5 5], 'Engine', 'legacy');
for e = 1:numel(engines)
  [idxE, CE, sumdE, DE] = kmeans(Xnan, 2, 'Start', [0 0; 5 5], 'Engine', engines{e});
  asserts.isequal(isnan(idxE), isnan(idxL));
  asserts.isequal(idxE(~isnan(idxE)), idxL(~isnan(idxL)));
  asserts.isequal(CE, CL);
  asserts.isequal(sumdE, sumdL);
  asserts.isequal(isnan(DE), isnan(DL));
end
%=============================================================================
% Medium clustered data, repeated with random starts.
values = [0.113 0.114 0.115 0.116 0.117 0.118 0.119 0.120 0.121 0.122 ...
  0.123 0.124 0.125 0.126 0.127 0.128 0.129 0.130 0.131 0.132 ...
  0.133 0.134 0.135 0.136 0.137 0.138 0.139 0.140 0.141 0.142];
counts = [4 7 19 31 24 16 22 18 22 22 14 14 11 11 12 15 6 8 6 6 ...
  10 6 5 7 13 8 13 17 22 12];
K = zeros(sum(counts), 2);
row = 1;
for j = 1:numel(values)
  count = counts(j);
  K(row:row + count - 1, :) = values(j) * ones(count, 2);
  row = row + count;
end
for i = 1:5
  rng(100 + i);
  [idxL, CL, sumdL] = kmeans(K, 6, 'Engine', 'legacy');
  for e = 1:numel(engines)
    rng(100 + i);
    [idxE, CE, sumdE] = kmeans(K, 6, 'Engine', engines{e});
    asserts.isequal(idxE, idxL);
    asserts.isequal(CE, CL);
    asserts.isequal(sumdE, sumdL);
  end
end
%=============================================================================
% Higher dimension clustered data.
rng(42);
Xhd = [rand(300, 40); rand(300, 40) + 5; rand(300, 40) + 10; rand(300, 40) + 15];
rng(43);
[idxL, CL, sumdL, DL] = kmeans(Xhd, 4, 'Start', 'plus', 'Engine', 'legacy');
for e = 1:numel(engines)
  rng(43);
  [idxE, CE, sumdE, DE] = kmeans(Xhd, 4, 'Start', 'plus', 'Engine', engines{e});
  asserts.isequal(idxE, idxL);
  asserts.isequal(CE, CL);
  asserts.isequal(sumdE, sumdL);
  asserts.isequal(DE, DL);
end
%=============================================================================
% The auto default must match an explicit legacy run.
rng(44);
[idxA, CA] = kmeans(Xhd, 4, 'Start', 'sample');
rng(44);
[idxL, CL] = kmeans(Xhd, 4, 'Start', 'sample', 'Engine', 'legacy');
asserts.isequal(idxA, idxL);
asserts.isequal(CA, CL);
%=============================================================================
% Invalid engine name is rejected.
asserts.checkerror('kmeans([1 2; 3 4], 2, ''Engine'', ''bad'')', sprintf(_('Invalid %s value.'), 'Engine'));
%=============================================================================
