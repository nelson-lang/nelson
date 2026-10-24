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
asserts.isequal(nargin('spectralcluster'), -3);
asserts.isequal(nargout('spectralcluster'), 3);
%=============================================================================
rng(4);
X = [0 0; 0 1; 1 0; 10 10; 10 11; 11 10];
[idx, V, D] = spectralcluster(X, 2, 'NumNeighbors', 5, 'KernelScale', 2, ...
  'LaplacianNormalization', 'symmetric');
asserts.isequal(canonicalLabels(idx), [1; 1; 1; 2; 2; 2]);
asserts.isequal(size(V), [6 2]);
asserts.isapprox(D, [2.88033387577171e-16; 1.43064798877632e-10], 1e-12, 1e-12);
% The two smallest eigenvalues form a near-degenerate pair (both ~0 because the
% graph splits into two well-separated clusters, so eigenvalue 0 has multiplicity
% two). The embedding therefore spans a 2-D eigenspace whose basis is defined only
% up to an orthonormal rotation, and the individual eigenvector components are not
% reproducible across LAPACK backends (their sensitivity is eps / gap ~ 1e-6, which
% exceeds a 1e-6 component tolerance). The embedding projector V*V' is invariant to
% that rotation (and to column signs): for any orthogonal Q, (V*Q)*(V*Q)' = V*V'.
% Its block structure is the meaningful, portable quantity: each cluster's rows are
% collinear unit vectors (block of ones) and orthogonal to the other cluster's rows.
embeddingProjector = [ones(3, 3), zeros(3, 3); zeros(3, 3), ones(3, 3)];
asserts.isapprox(V * V', embeddingProjector, 1e-6, 1e-6);
%=============================================================================
S = exp(-squareform(pdist(X)) .^ 2 / 8);
[idx2, V2, D2] = spectralcluster(S, 2, 'Distance', 'precomputed', ...
  'LaplacianNormalization', 'symmetric');
asserts.isequal(canonicalLabels(idx2), [1; 1; 1; 2; 2; 2]);
% The precomputed-similarity path builds the same graph as the euclidean path, so
% it must yield the same embedding subspace. Compare the rotation-invariant
% projector rather than the (basis-dependent) eigenvector components.
asserts.isapprox(V2 * V2', V * V', 1e-6, 1e-6);
asserts.isapprox(D2, D, 1e-12, 1e-12);
%=============================================================================
rng(4);
[idx3, ~, D3] = spectralcluster(X, 2, 'SimilarityGraph', 'epsilon', 'Radius', 2, ...
  'KernelScale', 'auto', 'ClusterMethod', 'kmedoids');
asserts.isequal(canonicalLabels(idx3), [1; 1; 1; 2; 2; 2]);
asserts.isapprox(D3, [-1.58906300684610e-07; -1.58906300684610e-07], 1e-12, 1e-12);
%=============================================================================
Xnan = [X; NaN 1];
[idxn, Vn] = spectralcluster(Xnan, 2, 'NumNeighbors', 5, 'KernelScale', 2);
asserts.isequal(canonicalLabels(idxn(1:6)), [1; 1; 1; 2; 2; 2]);
asserts.istrue(isnan(idxn(7)));
asserts.istrue(all(isnan(Vn(7, :))));
%=============================================================================
eva = evalclusters(X, 'spectralcluster', 'CalinskiHarabasz', 'KList', [2 3]);
asserts.isequal(eva.InspectedK, [2 3]);
asserts.isequal(size(eva.ClusterLabels), [6 2]);
%=============================================================================
asserts.checkerror('spectralcluster()', _('Not enough input arguments.'));
asserts.checkerror('spectralcluster({1}, 1)', sprintf(_('%s must be a real numeric matrix.'), 'X'));
asserts.checkerror('spectralcluster([1; 2], 0)', sprintf(_('%s must be a positive integer scalar.'), 'K'));
asserts.checkerror('spectralcluster([1; 2], 3)', _('K must not exceed the number of observations.'));
asserts.checkerror('spectralcluster([1; 2], 1, ''Distance'')', _('Name-value arguments must be paired.'));
asserts.checkerror('spectralcluster([1; 2], 1, ''Unknown'', 1)', _('Invalid name-value argument name.'));
asserts.checkerror('spectralcluster([1; 2], 1, ''Distance'', 1)', sprintf(_('%s must be a character vector or string scalar.'), 'Distance'));
asserts.checkerror('spectralcluster([1; 2], 1, ''Distance'', ''bad'')', sprintf(_('Invalid %s value.'), 'Distance'));
asserts.checkerror('spectralcluster([1; 2], 1, ''SimilarityGraph'', ''bad'')', _('SimilarityGraph must be knn or epsilon.'));
asserts.checkerror('spectralcluster([1; 2], 1, ''SimilarityGraph'', ''epsilon'')', _('Radius must be specified for epsilon similarity graph.'));
asserts.checkerror('spectralcluster([1; 2], 1, ''KernelScale'', ''bad'')', _('KernelScale must be auto or a positive scalar.'));
asserts.checkerror('spectralcluster([1 2 3; 4 5 6], 1, ''Distance'', ''precomputed'')', sprintf(_('%s must be square.'), 'Similarity matrix'));
asserts.checkerror('spectralcluster([0 1; 2 0], 1, ''Distance'', ''precomputed'')', sprintf(_('%s must be symmetric.'), 'Similarity matrix'));
%=============================================================================
function labels = canonicalLabels(labels)
  groups = unique(labels(~isnan(labels)), 'stable');
  out = labels;
  for k = 1:length(groups)
    out(labels == groups(k)) = k;
  end
  labels = out;
end
%=============================================================================
