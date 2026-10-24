%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function eva = evalclusters(varargin)
  narginchk(3, Inf);
  nargoutchk(0, 1);

  X = varargin{1};
  clust = varargin{2};
  criterion = criterionName(varargin{3});
  if ~isnumeric(X) || ndims(X) ~= 2 || isempty(X)
    error('Nelson:statistics:xNonemptyNumericMatrix', '%s must be a nonempty numeric matrix.', 'X');
  end
  if rem(nargin - 3, 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end

  opts = parseOptions(varargin(4:end));
  labels = clusteringSolutions(X, clust, opts);
  values = evaluateSolutions(X, labels, criterion, opts);
  inspectedK = clusterCounts(labels);
  [optimalK, optimalColumn] = chooseOptimal(inspectedK, values, criterion);

  eva = struct();
  eva.CriterionName = criterion;
  eva.NumObservations = size(X, 1);
  eva.InspectedK = inspectedK;
  eva.CriterionValues = values;
  eva.OptimalK = optimalK;
  eva.OptimalY = labels(:, optimalColumn);
  eva.ClusterLabels = labels;
  eva.Distance = opts.Distance;
  if strcmp(criterion, 'silhouette')
    eva.ClusterPriors = opts.ClusterPriors;
  end

  if nargout == 0
    disp(eva);
    clear eva;
  end
end
%=============================================================================
function opts = parseOptions(args)
  opts = struct('KList', [], 'Distance', 'sqeuclidean', 'ClusterPriors', 'empirical');
  for k = 1:2:length(args)
    name = optionName(args{k});
    value = args{k + 1};
    switch name
      case 'klist'
        opts.KList = positiveIntegerVector(value, 'KList must be a vector of positive integers.');
      case 'distance'
        opts.Distance = value;
      case 'clusterpriors'
        opts.ClusterPriors = clusterPriorsValue(value);
      otherwise
        error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
    end
  end
end
%=============================================================================
function name = optionName(value)
  if ~(ischar(value) || isStringScalar(value))
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Name');
  end
  text = lower(char(value));
  if isempty(text)
    error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
  end
  names = {'klist', 'distance', 'clusterpriors'};
  matches = {};
  for k = 1:length(names)
    if strncmp(names{k}, text, length(text))
      matches{end + 1} = names{k};
    end
  end
  if isempty(matches)
    error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
  end
  if length(matches) > 1
    error('Nelson:statistics:ambiguousNameValue', 'Ambiguous name-value argument name.');
  end
  name = matches{1};
end
%=============================================================================
function criterion = criterionName(value)
  if ~(ischar(value) || isStringScalar(value))
    error('Nelson:statistics:criterionSilhouetteCalinskiHarabaszDaviesBouldin', 'Criterion must be silhouette, CalinskiHarabasz, or DaviesBouldin.');
  end
  text = lower(char(value));
  names = {'silhouette', 'calinskiharabasz', 'daviesbouldin'};
  canonical = {'silhouette', 'CalinskiHarabasz', 'DaviesBouldin'};
  matches = [];
  for k = 1:length(names)
    if ~isempty(text) && strncmp(names{k}, text, length(text))
      matches(end + 1) = k;
    end
  end
  if isempty(matches)
    error('Nelson:statistics:criterionSilhouetteCalinskiHarabaszDaviesBouldin', 'Criterion must be silhouette, CalinskiHarabasz, or DaviesBouldin.');
  end
  if length(matches) > 1
    error('Nelson:statistics:ambiguousValue', 'Ambiguous %s value.', 'criterion');
  end
  criterion = canonical{matches(1)};
end
%=============================================================================
function value = clusterPriorsValue(value)
  if ~(ischar(value) || isStringScalar(value))
    error('Nelson:statistics:clusterPriorsEmpiricalEqual', 'ClusterPriors must be empirical or equal.');
  end
  text = lower(char(value));
  names = {'empirical', 'equal'};
  matches = {};
  for k = 1:length(names)
    if ~isempty(text) && strncmp(names{k}, text, length(text))
      matches{end + 1} = names{k};
    end
  end
  if isempty(matches)
    error('Nelson:statistics:clusterPriorsEmpiricalEqual', 'ClusterPriors must be empirical or equal.');
  end
  if length(matches) > 1
    error('Nelson:statistics:ambiguousValue', 'Ambiguous %s value.', 'ClusterPriors');
  end
  value = matches{1};
end
%=============================================================================
function values = positiveIntegerVector(value, message)
  if ~isnumeric(value) || ~isvector(value) || isempty(value) || any(value(:) < 1) || any(value(:) ~= fix(value(:)))
    error('Nelson:statistics:mustBePositiveInteger', message);
  end
  values = value(:)';
end
%=============================================================================
function labels = clusteringSolutions(X, clust, opts)
  if isnumeric(clust)
    labels = matrixSolutions(X, clust);
    return
  end
  if isempty(opts.KList)
    error('Nelson:statistics:kListSpecifiedClusteringAlgorithms', 'KList must be specified for clustering algorithms.');
  end
  labels = zeros(size(X, 1), length(opts.KList));
  for k = 1:length(opts.KList)
    labels(:, k) = runClustering(X, clust, opts.KList(k), opts);
  end
end
%=============================================================================
function labels = matrixSolutions(X, clust)
  if ndims(clust) ~= 2 || size(clust, 1) ~= size(X, 1) || isempty(clust)
    error('Nelson:statistics:CLUSTOneRowObservation', '%s must have one row per observation.', 'CLUST');
  end
  labels = clust;
end
%=============================================================================
function labels = runClustering(X, clust, k, opts)
  if ischar(clust) || isStringScalar(clust)
    name = lower(char(clust));
    if isempty(name)
      error('Nelson:statistics:invalidClusteringAlgorithm', 'Invalid clustering algorithm.');
    end
    if strncmp('kmeans', name, length(name))
      labels = kmeans(X, k, 'EmptyAction', 'singleton', 'Replicates', 5, 'Distance', opts.Distance);
    elseif strncmp('linkage', name, length(name))
      labels = clusterdata(X, 'MaxClust', k, 'Linkage', linkageMethod(opts.Distance), 'Distance', linkageDistance(opts.Distance));
    elseif strncmp('spectralcluster', name, length(name))
      labels = spectralcluster(X, k, 'Distance', spectralDistance(opts.Distance));
    else
      error('Nelson:statistics:invalidClusteringAlgorithm', 'Invalid clustering algorithm.');
    end
  elseif isfunction_handle(clust)
    labels = feval(clust, X, k);
  else
    error('Nelson:statistics:invalidClusteringAlgorithm', 'Invalid clustering algorithm.');
  end
  labels = labelsFromScores(labels, k, size(X, 1));
end
%=============================================================================
function method = linkageMethod(distance)
  if ischar(distance) || isStringScalar(distance)
    text = lower(char(distance));
    if strncmp('sqeuclidean', text, length(text)) || strncmp('euclidean', text, length(text))
      method = 'ward';
    else
      method = 'average';
    end
  else
    method = 'average';
  end
end
%=============================================================================
function distance = linkageDistance(distance)
  if ischar(distance) || isStringScalar(distance)
    text = lower(char(distance));
    if strncmp('sqeuclidean', text, length(text))
      distance = 'euclidean';
    end
  end
end
%=============================================================================
function distance = spectralDistance(distance)
  if ischar(distance) || isStringScalar(distance)
    text = lower(char(distance));
    if strncmp('sqeuclidean', text, length(text))
      distance = 'euclidean';
    end
  end
end
%=============================================================================
function labels = labelsFromScores(labels, k, n)
  if ~isnumeric(labels) || ndims(labels) ~= 2 || size(labels, 1) ~= n
    error('Nelson:statistics:clusteringOutputOneRowObservation', '%s must have one row per observation.', 'Clustering output');
  end
  if size(labels, 2) > 1
    [~, labels] = max(labels, [], 2);
  else
    labels = labels(:);
  end
  if length(unique(labels)) ~= k
    error('Nelson:statistics:clusteringOutputContainKClusters', 'Clustering output must contain K clusters.');
  end
end
%=============================================================================
function values = evaluateSolutions(X, labels, criterion, opts)
  values = zeros(1, size(labels, 2));
  for k = 1:size(labels, 2)
    current = labels(:, k);
    switch criterion
      case 'silhouette'
        values(k) = silhouetteCriterion(X, current, opts.Distance, opts.ClusterPriors);
      case 'CalinskiHarabasz'
        values(k) = calinskiHarabaszCriterion(X, current);
      case 'DaviesBouldin'
        values(k) = daviesBouldinCriterion(X, current);
    end
  end
end
%=============================================================================
function values = clusterCounts(labels)
  values = zeros(1, size(labels, 2));
  for k = 1:size(labels, 2)
    values(k) = length(unique(labels(:, k)));
  end
end
%=============================================================================
function value = silhouetteCriterion(X, labels, distance, priors)
  if length(unique(labels)) < 2
    value = NaN;
    return
  end
  s = silhouette(X, labels, distance);
  if strcmp(priors, 'empirical')
    value = mean(s);
  else
    groups = unique(labels);
    clusterValues = zeros(length(groups), 1);
    for k = 1:length(groups)
      clusterValues(k) = mean(s(labels == groups(k)));
    end
    value = mean(clusterValues);
  end
end
%=============================================================================
function value = calinskiHarabaszCriterion(X, labels)
  groups = unique(labels);
  n = size(X, 1);
  k = length(groups);
  if k < 2 || k >= n
    value = NaN;
    return
  end
  grandMean = mean(X, 1);
  within = 0;
  between = 0;
  for i = 1:k
    rows = labels == groups(i);
    Xi = X(rows, :);
    centroid = mean(Xi, 1);
    centered = Xi - centroid;
    within = within + sum(centered(:) .^ 2);
    delta = centroid - grandMean;
    between = between + size(Xi, 1) * sum(delta(:) .^ 2);
  end
  value = (between / (k - 1)) / (within / (n - k));
end
%=============================================================================
function value = daviesBouldinCriterion(X, labels)
  groups = unique(labels);
  k = length(groups);
  if k < 2
    value = NaN;
    return
  end
  centroids = zeros(k, size(X, 2));
  scatter = zeros(k, 1);
  for i = 1:k
    Xi = X(labels == groups(i), :);
    centroids(i, :) = mean(Xi, 1);
    scatter(i) = meanEuclideanDistance(Xi, centroids(i, :));
  end
  maxRatio = zeros(k, 1);
  for i = 1:k
    best = -Inf;
    for j = 1:k
      if i ~= j
        separation = euclideanDistance(centroids(i, :), centroids(j, :));
        if separation == 0
          ratio = Inf;
        else
          ratio = (scatter(i) + scatter(j)) / separation;
        end
        best = max(best, ratio);
      end
    end
    maxRatio(i) = best;
  end
  value = mean(maxRatio);
end
%=============================================================================
function value = meanEuclideanDistance(X, center)
  delta = X - center;
  value = mean(sqrt(sum(delta .^ 2, 2)));
end
%=============================================================================
function value = euclideanDistance(a, b)
  delta = a - b;
  value = sqrt(sum(delta(:) .^ 2));
end
%=============================================================================
function [optimalK, optimalColumn] = chooseOptimal(inspectedK, values, criterion)
  valid = find(~isnan(values));
  if isempty(valid)
    optimalK = NaN;
    optimalColumn = 1;
    return
  end
  if strcmp(criterion, 'DaviesBouldin')
    bestValue = min(values(valid));
  else
    bestValue = max(values(valid));
  end
  candidates = valid(values(valid) == bestValue);
  [optimalK, order] = min(inspectedK(candidates));
  optimalColumn = candidates(order);
end
%=============================================================================
