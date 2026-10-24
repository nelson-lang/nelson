%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [Y, stress, disparities] = mdscale(D, p, varargin)
  narginchk(2, Inf);
  nargoutchk(0, 3);
  [D, opts] = parseInputsLocal(D, p, convertStringToCharArgs(varargin));
  [bestY, bestStress, bestDisparities] = runReplicatesLocal(D, opts);
  Y = bestY;
  stress = bestStress;
  disparities = bestDisparities;
end
%=============================================================================
function [D, opts] = parseInputsLocal(D, p, args)
  D = parseDissimilaritiesLocal(D);
  opts = defaultOptionsLocal(size(D, 1));
  opts = parseNameValuesLocal(opts, args);
  opts.p = parseDimensionLocal(p, opts.Start, size(D, 1));
  opts.Weights = parseWeightsLocal(opts.Weights, D);
  opts.Replicates = parseReplicatesLocal(opts.Replicates, opts.Start);
  opts.Options = parseOptionsLocal(opts.Options);
  D = similarityToDissimilarityLocal(D);
  if any(isnan(D(:)))
    opts.Weights(isnan(D)) = 0;
    D(isnan(D)) = 0;
  end
  validateStartLocal(opts.Start, size(D, 1), opts.p, opts.Replicates);
  zeroWeights = opts.Weights == 0;
  zeroWeights(1:size(D, 1) + 1:end) = false;
  if isTextScalarLocal(opts.Start) && strcmpi(char(opts.Start), 'cmdscale') ...
      && any(zeroWeights(:))
    error('Nelson:statistics:startCmdscaleValidZeroWeights', 'Start cmdscale is not valid with zero weights.');
  end
end
%=============================================================================
function opts = defaultOptionsLocal(n)
  opts = struct('Criterion', 'stress', 'Weights', ones(n), ...
    'Start', 'cmdscale', 'Replicates', [], ...
    'Options', statset('Display', 'off', 'MaxIter', 200, ...
    'TolFun', 1e-4, 'TolX', 1e-4), 'p', []);
end
%=============================================================================
function opts = parseNameValuesLocal(opts, args)
  if rem(length(args), 2) ~= 0
    error('Nelson:statistics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  for k = 1:2:length(args)
    if ~isTextScalarLocal(args{k})
      error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Name');
    end
    name = lower(char(args{k}));
    switch name
      case 'criterion'
        opts.Criterion = parseCriterionLocal(args{k + 1});
      case 'weights'
        opts.Weights = args{k + 1};
      case 'start'
        opts.Start = args{k + 1};
      case 'replicates'
        opts.Replicates = args{k + 1};
      case 'options'
        opts.Options = args{k + 1};
      otherwise
        error('Nelson:statistics:unknownOption', 'Unknown option.');
    end
  end
end
%=============================================================================
function value = parseCriterionLocal(value)
  choices = {'stress', 'sstress', 'metricstress', 'metricsstress', 'sammon', 'strain'};
  if ~isTextScalarLocal(value)
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Criterion');
  end
  value = lower(char(value));
  matches = choices(strncmp(choices, value, length(value)));
  if isempty(matches)
    error('Nelson:statistics:invalidCriterionValue', 'Invalid %s value.', 'Criterion');
  end
  if length(matches) > 1
    error('Nelson:statistics:ambiguousValue', 'Ambiguous %s value.', 'Criterion');
  end
  value = matches{1};
end
%=============================================================================
function D = parseDissimilaritiesLocal(D)
  if ~(isnumeric(D) && isreal(D) && ~issparse(D) && ndims(D) == 2)
    error('Nelson:statistics:dRealNumericVectorMatrix', '%s must be a real numeric vector or matrix.', 'D');
  end
  D = double(D);
  if any(isinf(D(:))) || any(D(:) < 0)
    error('Nelson:statistics:dContainNonnegativeFiniteDissimilarities', 'D must contain nonnegative finite dissimilarities.');
  end
  if isvector(D) && ~(size(D, 1) == size(D, 2))
    D = vectorToSquareLocal(D(:)', _('D must be square or a valid distance vector.'));
  elseif size(D, 1) ~= size(D, 2)
    error('Nelson:statistics:dSquareValidDistanceVector', 'D must be square or a valid distance vector.');
  end
  validateDMatrixLocal(D);
end
%=============================================================================
function D = vectorToSquareLocal(v, message)
  m = length(v);
  n = (1 + sqrt(1 + 8 * m)) / 2;
  if floor(n) ~= n
    error('Nelson:statistics:sizeMismatch', message);
  end
  D = zeros(n, n);
  idx = 1;
  for j = 2:n
    for i = 1:j - 1
      D(i, j) = v(idx);
      D(j, i) = v(idx);
      idx = idx + 1;
    end
  end
end
%=============================================================================
function validateDMatrixLocal(D)
  if isempty(D)
    error('Nelson:statistics:dContainLeastOnePoint', 'D must contain at least one point.');
  end
  if isSimilarityLocal(D) || isUpperTriangularLocal(D)
    return
  end
  if ~isSymmetricWithNaNLocal(D)
    error('Nelson:statistics:dSymmetricUpperTriangular', 'D must be symmetric or upper triangular.');
  end
  diagonal = diag(D);
  if any(~isnan(diagonal) & abs(diagonal) > 100 * eps)
    error('Nelson:statistics:dZerosDiagonal', 'D must have zeros on the diagonal.');
  end
end
%=============================================================================
function D = similarityToDissimilarityLocal(D)
  if isSimilarityLocal(D)
    D = sqrt(max(0, 1 - D));
    D(1:size(D, 1) + 1:end) = 0;
  elseif isUpperTriangularLocal(D)
    D = D + D';
  end
end
%=============================================================================
function tf = isSimilarityLocal(D)
  diagonal = diag(D);
  offDiagonal = D - diag(diagonal);
  finiteOff = offDiagonal(~isnan(offDiagonal));
  tf = all(abs(diagonal - 1) <= 100 * eps) && isSymmetricWithNaNLocal(D) ...
    && all(finiteOff(:) >= 0) && all(finiteOff(:) <= 1 + 100 * eps);
end
%=============================================================================
function tf = isUpperTriangularLocal(D)
  diagonal = diag(D);
  lower = tril(D, -1);
  upper = triu(D, 1);
  tf = all(abs(diagonal(~isnan(diagonal))) <= 100 * eps) ...
    && all(abs(lower(~isnan(lower))) <= 100 * eps) ...
    && any(upper(~isnan(upper)) > 0);
end
%=============================================================================
function tf = isSymmetricWithNaNLocal(D)
  tolerance = 100 * eps(max(1, max(abs(D(~isnan(D))))));
  difference = abs(D - D');
  missing = isnan(D) & isnan(D');
  tf = all(difference(~missing) <= tolerance | isnan(difference(~missing)));
end
%=============================================================================
function p = parseDimensionLocal(p, start, n)
  if isempty(p)
    if isnumeric(start) && ndims(start) >= 2
      p = size(start, 2);
      return
    end
    error('Nelson:statistics:pSpecifiedUnlessStartNumeric', 'p must be specified unless Start is a numeric matrix.');
  end
  if ~(isnumeric(p) && isreal(p) && isscalar(p) && isfinite(p) ...
      && floor(p) == p && p >= 1 && p <= n)
    error('Nelson:statistics:pIntegerScalarBetween1', 'p must be an integer scalar between 1 and the number of points.');
  end
  p = double(p);
end
%=============================================================================
function W = parseWeightsLocal(W, D)
  if ~(isnumeric(W) && isreal(W) && ~issparse(W) && ndims(W) == 2)
    error('Nelson:statistics:weightsRealNumericVectorMatrix', '%s must be a real numeric vector or matrix.', 'Weights');
  end
  W = double(W);
  if isvector(W) && ~(size(W, 1) == size(W, 2))
    W = vectorToSquareLocal(W(:)', _('Weights must have the same size as D.'));
  elseif ~isequal(size(W), size(D))
    error('Nelson:statistics:weightsSameSizeD', 'Weights must have the same size as D.');
  end
  if any(~isfinite(W(:))) || any(W(:) < 0)
    error('Nelson:statistics:weightsContainNonnegativeFiniteValues', '%s must contain nonnegative finite values.', 'Weights');
  end
  if ~isSymmetricWithNaNLocal(W)
    error('Nelson:statistics:weightsSymmetric', '%s must be symmetric.', 'Weights');
  end
  W(1:size(W, 1) + 1:end) = 0;
end
%=============================================================================
function replicates = parseReplicatesLocal(replicates, start)
  if isempty(replicates)
    if isnumeric(start) && ndims(start) == 3
      replicates = size(start, 3);
    else
      replicates = 1;
    end
    return
  end
  if ~(isnumeric(replicates) && isreal(replicates) && isscalar(replicates) ...
      && isfinite(replicates) && floor(replicates) == replicates && replicates >= 0)
    error('Nelson:statistics:replicatesNonnegativeIntegerScalar', '%s must be a nonnegative integer scalar.', 'Replicates');
  end
  replicates = double(replicates);
end
%=============================================================================
function options = parseOptionsLocal(options)
  if ~(isstruct(options) && isscalar(options))
    error('Nelson:statistics:invalidOptions', '%s must be a scalar structure.', 'Options');
  end
  displayValue = statget(options, 'Display', 'off');
  if ~isTextScalarLocal(displayValue)
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Options Display');
  end
  options.Display = lower(char(displayValue));
  options.MaxIter = statget(options, 'MaxIter', 200);
  options.TolFun = statget(options, 'TolFun', 1e-4);
  options.TolX = statget(options, 'TolX', 1e-4);
  if ~any(strcmp(options.Display, {'off', 'final', 'iter'}))
    error('Nelson:statistics:optionsDisplayInvalidValue', '%s has an invalid value.', 'Options Display');
  end
  if ~isPositiveIntegerLocal(options.MaxIter)
    error('Nelson:statistics:optionsMaxIterPositiveIntegerScalar', '%s must be a positive integer scalar.', 'Options MaxIter');
  end
  if ~isPositiveScalarLocal(options.TolFun) || ~isPositiveScalarLocal(options.TolX)
    error('Nelson:statistics:optionsTolerancesPositiveScalarValues', 'Options tolerances must be %s scalar values.', 'positive');
  end
end
%=============================================================================
function validateStartLocal(start, n, p, replicates)
  if isTextScalarLocal(start)
    value = lower(char(start));
    if ~any(strcmp(value, {'cmdscale', 'random'}))
      error('Nelson:statistics:startInvalidValue', '%s has an invalid value.', 'Start');
    end
    return
  end
  if ~(isnumeric(start) && isreal(start) && ~issparse(start) && (ndims(start) == 2 || ndims(start) == 3))
    error('Nelson:statistics:startCharacterVectorStringScalar', 'Start must be a character vector, string scalar, matrix, or 3-D array.');
  end
  if size(start, 1) ~= n || size(start, 2) ~= p || any(~isfinite(start(:)))
    error('Nelson:statistics:startCompatibleFiniteNumericValues', 'Start must have compatible finite numeric values.');
  end
  if ndims(start) == 3 && replicates ~= size(start, 3)
    error('Nelson:statistics:replicatesMatchNumberStartPages', 'Replicates must match the number of Start pages.');
  end
end
%=============================================================================
function [bestY, bestStress, bestDisparities] = runReplicatesLocal(D, opts)
  bestStress = Inf;
  bestY = zeros(size(D, 1), opts.p);
  bestDisparities = D;
  if opts.Replicates == 0
    return
  end
  for r = 1:opts.Replicates
    startY = startConfigurationLocal(D, opts, r);
    [Y, stress, disparities, iterations] = fitOneLocal(D, opts, startY);
    if stress < bestStress
      bestStress = stress;
      bestY = Y;
      bestDisparities = disparities;
    end
    displayProgressLocal(opts.Options.Display, 'final', iterations, stress);
  end
end
%=============================================================================
function Y = startConfigurationLocal(D, opts, replicate)
  n = size(D, 1);
  if isnumeric(opts.Start)
    if ndims(opts.Start) == 3
      Y = opts.Start(:, :, replicate);
    else
      Y = opts.Start;
    end
  elseif strcmpi(char(opts.Start), 'random')
    scale = max(D(:));
    if scale == 0
      scale = 1;
    end
    Y = randn(n, opts.p) * scale / sqrt(opts.p);
  else
    Y = cmdscale(D, opts.p);
    if size(Y, 2) < opts.p
      Y(:, end + 1:opts.p) = 0;
    end
  end
  Y = centerConfigurationLocal(Y);
end
%=============================================================================
function [Y, stress, disparities, iterations] = fitOneLocal(D, opts, Y)
  W = opts.Weights;
  disparities = initialDisparitiesLocal(D, opts.Criterion, W);
  stress = stressLocal(D, Y, W, disparities, opts.Criterion);
  for iterations = 1:opts.Options.MaxIter
    distances = distancesLocal(Y);
    if isNonmetricLocal(opts.Criterion)
      disparities = disparitiesLocal(D, distances, W);
    end
    Ynew = smacofStepLocal(Y, W, disparities);
    Ynew = centerConfigurationLocal(Ynew);
    newStress = stressLocal(D, Ynew, W, disparities, opts.Criterion);
    displayProgressLocal(opts.Options.Display, 'iter', iterations, newStress);
    if newStress > stress
      break
    end
    if shouldStopLocal(Y, Ynew, stress, newStress, opts.Options)
      Y = Ynew;
      stress = newStress;
      break
    end
    Y = Ynew;
    stress = newStress;
  end
end
%=============================================================================
function disparities = initialDisparitiesLocal(D, criterion, W)
  if isNonmetricLocal(criterion)
    distances = D;
    disparities = disparitiesLocal(D, distances, W);
  else
    disparities = D;
  end
end
%=============================================================================
function tf = isNonmetricLocal(criterion)
  tf = strcmp(criterion, 'stress') || strcmp(criterion, 'sstress');
end
%=============================================================================
function distances = distancesLocal(Y)
  n = size(Y, 1);
  gram = sum(Y .^ 2, 2);
  D2 = repmat(gram, 1, n) + repmat(gram', n, 1) - 2 * (Y * Y');
  distances = sqrt(max(D2, 0));
  distances(1:n + 1:end) = 0;
end
%=============================================================================
function Ynew = smacofStepLocal(Y, W, disparities)
  distances = distancesLocal(Y);
  ratio = zeros(size(distances));
  valid = distances > eps & W > 0;
  ratio(valid) = W(valid) .* disparities(valid) ./ distances(valid);
  B = -ratio;
  B(1:size(B, 1) + 1:end) = -sum(B, 2);
  rowWeights = sum(W, 2) + max(W, [], 2);
  Ynew = zeros(size(Y));
  active = rowWeights > 0;
  Ynew(active, :) = (B(active, :) * Y) ./ repmat(rowWeights(active), 1, size(Y, 2));
end
%=============================================================================
function disparities = disparitiesLocal(D, distances, W)
  n = size(D, 1);
  mask = triu(W > 0 & ~isnan(D), 1);
  delta = D(mask);
  dist = distances(mask);
  weights = W(mask);
  [delta, order] = sort(delta);
  dist = dist(order);
  weights = weights(order);
  fitted = isotonicLocal(dist(:), weights(:));
  scale = sqrt(sum(weights .* dist(:) .^ 2) / max(sum(weights .* fitted(:) .^ 2), realmin));
  fitted = fitted * scale;
  disparities = zeros(n);
  linear = find(mask);
  inverseOrder(order) = 1:length(order);
  disparities(linear) = fitted(inverseOrder);
  disparities = disparities + disparities';
end
%=============================================================================
function y = isotonicLocal(x, w)
  values = zeros(0, 1);
  weights = zeros(0, 1);
  counts = zeros(0, 1);
  for i = 1:length(x)
    values(end + 1, 1) = x(i);
    weights(end + 1, 1) = w(i);
    counts(end + 1, 1) = 1;
    while length(values) > 1 && values(end - 1) > values(end)
      totalWeight = weights(end - 1) + weights(end);
      values(end - 1) = (values(end - 1) * weights(end - 1) + values(end) * weights(end)) / totalWeight;
      weights(end - 1) = totalWeight;
      counts(end - 1) = counts(end - 1) + counts(end);
      values(end) = [];
      weights(end) = [];
      counts(end) = [];
    end
  end
  y = zeros(length(x), 1);
  pos = 1;
  for i = 1:length(values)
    y(pos:pos + counts(i) - 1) = values(i);
    pos = pos + counts(i);
  end
end
%=============================================================================
function stress = stressLocal(D, Y, W, disparities, criterion)
  distances = distancesLocal(Y);
  mask = triu(W > 0, 1);
  d = distances(mask);
  delta = D(mask);
  dispv = disparities(mask);
  w = W(mask);
  switch criterion
    case {'stress', 'metricstress'}
      target = metricTargetLocal(criterion, delta, dispv);
      stress = sqrt(sum(w .* (d - target) .^ 2) / max(sum(w .* d .^ 2), realmin));
    case {'sstress', 'metricsstress'}
      target = metricTargetLocal(criterion, delta, dispv);
      stress = sqrt(sum(w .* (d .^ 2 - target .^ 2) .^ 2) / max(sum(w .* d .^ 4), realmin));
    case 'sammon'
      valid = delta > 0;
      stress = sum(w(valid) .* ((d(valid) - delta(valid)) .^ 2) ./ delta(valid)) ...
        / max(sum(w(valid) .* delta(valid)), realmin);
    case 'strain'
      stress = strainStressLocal(D, Y, W);
  end
end
%=============================================================================
function target = metricTargetLocal(criterion, delta, disparities)
  if strcmp(criterion, 'stress') || strcmp(criterion, 'sstress')
    target = disparities;
  else
    target = delta;
  end
end
%=============================================================================
function stress = strainStressLocal(D, Y, W)
  B = -0.5 * centerMatrixLocal(D .^ 2);
  G = Y * Y';
  mask = W > 0;
  stress = sum((B(mask) - G(mask)) .^ 2) / max(sum(B(mask) .^ 2), realmin);
end
%=============================================================================
function B = centerMatrixLocal(A)
  n = size(A, 1);
  rowMean = mean(A, 2);
  colMean = mean(A, 1);
  totalMean = mean(A(:));
  B = A - repmat(rowMean, 1, n) - repmat(colMean, n, 1) + totalMean;
end
%=============================================================================
function tf = shouldStopLocal(Y, Ynew, stress, newStress, options)
  stressStep = abs(stress - newStress) <= options.TolFun * max(1, abs(stress));
  locationStep = max(abs(Ynew(:) - Y(:))) <= options.TolX * max(1, max(abs(Y(:))));
  tf = stressStep || locationStep;
end
%=============================================================================
function Y = centerConfigurationLocal(Y)
  Y = Y - repmat(mean(Y, 1), size(Y, 1), 1);
end
%=============================================================================
function displayProgressLocal(displayValue, stage, iterations, stress)
  if strcmp(displayValue, 'iter') && strcmp(stage, 'iter')
    disp([int2str(iterations), ' iterations, stress criterion = ', num2str(stress)]);
  elseif strcmp(displayValue, 'final') && strcmp(stage, 'final')
    disp([int2str(iterations), ' iterations, Final stress criterion = ', num2str(stress)]);
  end
end
%=============================================================================
function tf = isTextScalarLocal(value)
  tf = ischar(value) || isStringScalar(value);
end
%=============================================================================
function tf = isPositiveIntegerLocal(value)
  tf = isnumeric(value) && isreal(value) && isscalar(value) ...
    && isfinite(value) && floor(value) == value && value > 0;
end
%=============================================================================
function tf = isPositiveScalarLocal(value)
  tf = isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) && value > 0;
end
%=============================================================================
