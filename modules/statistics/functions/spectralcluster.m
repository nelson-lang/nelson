%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [idx, V, D] = spectralcluster(X, k, varargin)
  narginchk(2, Inf);
  nargoutchk(0, 3);
  k = positiveInteger(k, sprintf(_('%s must be a positive integer scalar.'), 'K'));
  if rem(length(varargin), 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  opts = parseOptions(varargin);
  [validX, validRows, outputRows] = prepareInput(X, opts);
  if k > size(validX, 1)
    error('Nelson:statistics:kExceedNumberObservations', 'K must not exceed the number of observations.');
  end
  S = similarityMatrix(validX, opts);
  [embedding, values] = spectralEmbedding(S, k, opts.LaplacianNormalization);
  labels = clusterEmbedding(embedding, k, opts.ClusterMethod);
  idx = NaN(outputRows, 1);
  idx(validRows) = labels;
  V = NaN(outputRows, k);
  V(validRows, :) = embedding;
  D = values(:);
end
%=============================================================================
function opts = parseOptions(args)
  opts = struct('Distance', 'euclidean', 'DistanceParameter', [], 'SimilarityGraph', 'knn', ...
    'NumNeighbors', [], 'KNNGraphType', 'complete', 'Radius', [], 'KernelScale', [], ...
    'LaplacianNormalization', 'randomwalk', 'ClusterMethod', 'kmeans');
  for pos = 1:2:length(args)
    name = optionName(args{pos});
    value = args{pos + 1};
    switch name
      case 'distance'
        opts.Distance = distanceValue(value);
      case 'p'
        opts.DistanceParameter = positiveScalar(value, sprintf(_('%s must be a positive scalar.'), 'P'));
      case {'cov', 'scale'}
        opts.DistanceParameter = value;
      case 'similaritygraph'
        opts.SimilarityGraph = similarityGraphValue(value);
      case 'numneighbors'
        opts.NumNeighbors = positiveInteger(value, sprintf(_('%s must be a positive integer scalar.'), 'NumNeighbors'));
      case 'knngraphtype'
        opts.KNNGraphType = knnGraphTypeValue(value);
      case 'radius'
        opts.Radius = positiveScalar(value, sprintf(_('%s must be a positive scalar.'), 'Radius'));
      case 'kernelscale'
        opts.KernelScale = kernelScaleValue(value);
      case 'laplaciannormalization'
        opts.LaplacianNormalization = laplacianValue(value);
      case 'clustermethod'
        opts.ClusterMethod = clusterMethodValue(value);
    end
  end
end
%=============================================================================
function [validX, validRows, outputRows] = prepareInput(X, opts)
  if ~(isnumeric(X) && isreal(X) && ~issparse(X) && ndims(X) == 2)
    error('Nelson:statistics:xRealNumericMatrix', '%s must be a real numeric matrix.', 'X');
  end
  X = double(X);
  outputRows = size(X, 1);
  if strcmp(opts.Distance, 'precomputed')
    validateSimilarityInput(X);
    validRows = (1:outputRows)';
    validX = X;
  else
    finiteRows = (double(isfinite(X)) * ones(size(X, 2), 1)) == size(X, 2);
    validRows = find(finiteRows);
    validX = X(validRows, :);
  end
  if size(validX, 1) < 2
    error('Nelson:statistics:xContainLeastTwoValid', 'X must contain at least two valid observations.');
  end
end
%=============================================================================
function validateSimilarityInput(S)
  if size(S, 1) ~= size(S, 2)
    error('Nelson:expectedSquare', '%s must be square.', 'Similarity matrix');
  end
  if any(~isfinite(S(:))) || any(S(:) < 0)
    error('Nelson:statistics:mustBeFiniteNonnegative', '%s must contain finite nonnegative values.', 'Similarity matrix');
  end
  if ~isequal(S, S')
    error('Nelson:statistics:similarityMatrixSymmetric', '%s must be symmetric.', 'Similarity matrix');
  end
end
%=============================================================================
function S = similarityMatrix(X, opts)
  if strcmp(opts.Distance, 'precomputed')
    S = X;
    S(1:size(S, 1) + 1:end) = 0;
    return
  end
  D = pairwiseDistances(X, opts);
  scale = resolveKernelScale(D, opts.KernelScale);
  S = exp(-(D .^ 2) / (2 * scale ^ 2));
  S(1:size(S, 1) + 1:end) = 0;
  mask = graphMask(D, opts);
  S = S .* mask;
  S = max(S, S');
end
%=============================================================================
function D = pairwiseDistances(X, opts)
  if isempty(opts.DistanceParameter)
    D = pdist2(X, X, opts.Distance);
  else
    D = pdist2(X, X, opts.Distance, opts.DistanceParameter);
  end
end
%=============================================================================
function scale = resolveKernelScale(D, value)
  if ischar(value) || isStringScalar(value)
    text = lower(char(value));
    if ~strncmp('auto', text, length(text))
      error('Nelson:statistics:kernelScaleAutoPositiveScalar', 'KernelScale must be auto or a positive scalar.');
    end
    finiteValues = D(isfinite(D) & D > 0);
    scale = median(finiteValues(:));
  elseif isempty(value)
    finiteValues = D(isfinite(D) & D > 0);
    scale = median(finiteValues(:));
  else
    scale = positiveScalar(value, _('KernelScale must be auto or a positive scalar.'));
  end
  if isempty(scale) || ~(scale > 0)
    scale = 1;
  end
end
%=============================================================================
function mask = graphMask(D, opts)
  n = size(D, 1);
  switch opts.SimilarityGraph
    case 'epsilon'
      if isempty(opts.Radius)
        error('Nelson:statistics:radiusSpecifiedEpsilonSimilarityGraph', 'Radius must be specified for epsilon similarity graph.');
      end
      mask = double(D <= opts.Radius);
      mask(1:n + 1:end) = 0;
    otherwise
      neighbors = opts.NumNeighbors;
      if isempty(neighbors)
        neighbors = min(max(ceil(log2(n)), 1), n - 1);
      end
      neighbors = min(neighbors, n - 1);
      mask = knnMask(D, neighbors, opts.KNNGraphType);
  end
end
%=============================================================================
function mask = knnMask(D, neighbors, graphType)
  n = size(D, 1);
  mask = zeros(n, n);
  for r = 1:n
    distances = D(r, :);
    distances(r) = Inf;
    [~, order] = sort(distances);
    mask(r, order(1:neighbors)) = 1;
  end
  if strcmp(graphType, 'mutual')
    mask = double(mask & mask');
  else
    mask = double(mask | mask');
  end
end
%=============================================================================
function [V, D] = spectralEmbedding(S, k, normalization)
  degree = S * ones(size(S, 2), 1);
  L = diag(degree) - S;
  switch normalization
    case 'symmetric'
      invSqrt = zeros(size(degree));
      invSqrt(degree > 0) = 1 ./ sqrt(degree(degree > 0));
      L = diag(invSqrt) * L * diag(invSqrt);
    case 'randomwalk'
      invDegree = zeros(size(degree));
      invDegree(degree > 0) = 1 ./ degree(degree > 0);
      L = diag(invDegree) * L;
  end
  [vectors, valuesMatrix] = eig((L + L') / 2);
  [values, order] = sort(real(diag(valuesMatrix)));
  V = real(vectors(:, order(1:k)));
  D = values(1:k);
  if strcmp(normalization, 'symmetric')
    V = normalizeRows(V);
  end
end
%=============================================================================
function V = normalizeRows(V)
  norms = sqrt((V .* V) * ones(size(V, 2), 1));
  norms(norms == 0) = 1;
  V = V ./ repmat(norms, 1, size(V, 2));
end
%=============================================================================
function labels = clusterEmbedding(V, k, method)
  switch method
    case 'kmedoids'
      labels = kmedoids(V, k, 'Replicates', 3);
    otherwise
      labels = kmeans(V, k, 'Replicates', 5, 'EmptyAction', 'singleton');
  end
end
%=============================================================================
function name = optionName(value)
  if ~(ischar(value) || isStringScalar(value))
    error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
  end
  text = lower(char(value));
  names = {'distance', 'p', 'cov', 'scale', 'similaritygraph', 'numneighbors', ...
    'knngraphtype', 'radius', 'kernelscale', 'laplaciannormalization', 'clustermethod'};
  matches = names(strncmp(names, text, length(text)));
  if isempty(matches)
    error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
  end
  if length(matches) > 1
    error('Nelson:statistics:ambiguousNameValue', 'Ambiguous name-value argument name.');
  end
  name = matches{1};
end
%=============================================================================
function value = distanceValue(value)
  if ~(ischar(value) || isStringScalar(value))
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Distance');
  end
  text = lower(char(value));
  names = {'euclidean', 'precomputed', 'cityblock', 'cosine', 'correlation', ...
    'hamming', 'jaccard', 'minkowski', 'seuclidean', 'mahalanobis', 'chebychev', 'chebyshev'};
  matches = names(strncmp(names, text, length(text)));
  if isempty(matches)
    error('Nelson:statistics:invalidDistance', 'Invalid %s value.', 'Distance');
  end
  if length(matches) > 1
    error('Nelson:statistics:ambiguousValue', 'Ambiguous %s value.', 'Distance');
  end
  value = matches{1};
end
%=============================================================================
function value = similarityGraphValue(value)
  value = textChoice(value, {'knn', 'epsilon'}, 'SimilarityGraph');
end
%=============================================================================
function value = knnGraphTypeValue(value)
  value = textChoice(value, {'complete', 'mutual'}, 'KNNGraphType');
end
%=============================================================================
function value = laplacianValue(value)
  value = textChoice(value, {'randomwalk', 'symmetric', 'none'}, 'LaplacianNormalization');
end
%=============================================================================
function value = clusterMethodValue(value)
  value = textChoice(value, {'kmeans', 'kmedoids'}, 'ClusterMethod');
end
%=============================================================================
function value = textChoice(value, names, label)
  if numel(names) == 2
    choicesText = strjoin(names, ' or ');
  else
    choicesText = [strjoin(names(1:end - 1), ', '), ', or ', names{end}];
  end
  if ~(ischar(value) || isStringScalar(value))
    error('Nelson:statistics:invalidChoiceType', '%s must be %s.', label, choicesText);
  end
  text = lower(char(value));
  matches = names(strncmp(names, text, length(text)));
  if isempty(matches)
    error('Nelson:statistics:invalidChoiceValue', '%s must be %s.', label, choicesText);
  end
  if length(matches) > 1
    error('Nelson:statistics:ambiguousNameValueArgumentValue', 'Ambiguous name-value argument value.');
  end
  value = matches{1};
end
%=============================================================================
function value = kernelScaleValue(value)
  if ischar(value) || isStringScalar(value)
    text = lower(char(value));
    if strncmp('auto', text, length(text))
      value = 'auto';
      return
    end
  end
  value = positiveScalar(value, _('KernelScale must be auto or a positive scalar.'));
end
%=============================================================================
function value = positiveInteger(value, message)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) ...
      && floor(value) == value && value > 0)
    error('Nelson:statistics:mustBePositiveInteger', message);
  end
  value = double(value);
end
%=============================================================================
function value = positiveScalar(value, message)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) && value > 0)
    error('Nelson:statistics:mustBePositive', message);
  end
  value = double(value);
end
%=============================================================================
