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
asserts.isequal(nargin('evalclusters'), -1);
asserts.isequal(nargout('evalclusters'), 1);
%=============================================================================
X = [0; 1; 10; 11];
C = [1 1 1; 1 1 2; 1 2 3; 1 2 3];
eva = evalclusters(X, C, 'CalinskiHarabasz');
asserts.isequal(eva.CriterionName, 'CalinskiHarabasz');
asserts.isequal(eva.NumObservations, 4);
asserts.isequal(eva.InspectedK, [1 2 3]);
asserts.istrue(isnan(eva.CriterionValues(1)));
asserts.isapprox(eva.CriterionValues(2:3), [200 100.5], 1e-12);
asserts.isequal(eva.OptimalK, 2);
asserts.isequal(eva.OptimalY, [1; 1; 2; 2]);
%=============================================================================
eva = evalclusters(X, C, 'DaviesBouldin');
asserts.istrue(isnan(eva.CriterionValues(1)));
asserts.isapprox(eva.CriterionValues(2:3), [0.1 0.050960735171261], 1e-12);
asserts.isequal(eva.OptimalK, 3);
asserts.isequal(eva.OptimalY, [1; 2; 3; 3]);
%=============================================================================
eva = evalclusters(X, C, 'silhouette');
asserts.istrue(isnan(eva.CriterionValues(1)));
asserts.isapprox(eva.CriterionValues(2:3), [0.989950251243719 0.494413580246914], 1e-12);
asserts.isequal(eva.OptimalK, 2);
asserts.isequal(eva.ClusterPriors, 'empirical');
%=============================================================================
eva = evalclusters(X, C, 'silhouette', 'ClusterPriors', 'equal');
asserts.isapprox(eva.CriterionValues(2:3), [0.989950251243719 0.329609053497942], 1e-12);
%=============================================================================
rng(10);
eva = evalclusters(X, 'kmeans', 'CalinskiHarabasz', 'KList', 1:3);
asserts.istrue(isnan(eva.CriterionValues(1)));
asserts.isapprox(eva.CriterionValues(2:3), [200 100.5], 1e-12);
asserts.isequal(eva.OptimalK, 2);
%=============================================================================
eva = evalclusters(X, 'linkage', 'silhouette', 'KList', 1:3);
asserts.istrue(isnan(eva.CriterionValues(1)));
asserts.isapprox(eva.CriterionValues(2:3), [0.989950251243719 0.494413580246914], 1e-12);
asserts.isequal(eva.OptimalK, 2);
%=============================================================================
f = @(DATA, K) clusterdata(DATA, 'MaxClust', K);
eva = evalclusters(X, f, 'CalinskiHarabasz', 'KList', 1:3);
asserts.istrue(isnan(eva.CriterionValues(1)));
asserts.isapprox(eva.CriterionValues(2:3), [200 100.5], 1e-12);
asserts.isequal(eva.OptimalK, 2);
%=============================================================================
scores = @(DATA, K)[double(DATA <= 1), double(DATA > 1)];
eva = evalclusters(X, scores, 'CalinskiHarabasz', 'KList', 2);
asserts.isapprox(eva.CriterionValues, 200, 1e-12);
asserts.isequal(eva.OptimalY, [1; 1; 2; 2]);
%=============================================================================
asserts.checkerror('evalclusters()', _('Not enough input arguments.'));
asserts.checkerror('evalclusters([], C, ''silhouette'')', sprintf(_('%s must be a nonempty numeric matrix.'), 'X'));
asserts.checkerror('evalclusters(X, C, ''gap'')', _('Criterion must be silhouette, CalinskiHarabasz, or DaviesBouldin.'));
asserts.checkerror('evalclusters(X, C, ''silhouette'', ''KList'')', _('Name-value arguments must be paired.'));
asserts.checkerror('evalclusters(X, C, ''silhouette'', '''', 1)', _('Invalid name-value argument name.'));
asserts.checkerror('evalclusters(X, C, ''silhouette'', ''ClusterPriors'', ''bad'')', _('ClusterPriors must be empirical or equal.'));
asserts.checkerror('evalclusters(X, ''kmeans'', ''silhouette'')', _('KList must be specified for clustering algorithms.'));
asserts.checkerror('evalclusters(X, ''bad'', ''silhouette'', ''KList'', 2)', _('Invalid clustering algorithm.'));
asserts.checkerror('evalclusters(X, C, ''silhouette'', ''KList'', 0)', _('KList must be a vector of positive integers.'));
asserts.checkerror('evalclusters(X, [1 1; 1 1], ''silhouette'')', sprintf(_('%s must have one row per observation.'), 'CLUST'));
badfun = @(DATA, K) ones(size(DATA, 1), 1);
asserts.checkerror('evalclusters(X, badfun, ''silhouette'', ''KList'', 2)', _('Clustering output must contain K clusters.'));
%=============================================================================
