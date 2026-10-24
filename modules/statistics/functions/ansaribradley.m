%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [h, p, stats] = ansaribradley(x, y, varargin)
  narginchk(2, Inf);
  nargoutchk(0, 3);
  opts = parseOptionsLocal(convertStringToCharArgs(varargin), x);
  [xr, yr, outSize, perm] = reshapeInputsLocal(x, y, opts.Dim);
  cols = size(xr, 2);
  hvals = false(1, cols);
  pvals = NaN(1, cols);
  wvals = NaN(1, cols);
  zvals = NaN(1, cols);
  for k = 1:cols
    [hvals(k), pvals(k), wvals(k), zvals(k)] = ansariVectorLocal(xr(:, k), yr(:, k), opts);
  end
  h = ipermute(reshape(hvals, outSize), perm);
  p = ipermute(reshape(pvals, outSize), perm);
  stats = struct('W', ipermute(reshape(wvals, outSize), perm), ...
    'Wstar', ipermute(reshape(zvals, outSize), perm));
end
%=============================================================================
function opts = parseOptionsLocal(args, x)
  opts = struct('Alpha', 0.05, 'Tail', 'both', 'Method', 'auto', ...
    'Dim', firstNonsingletonLocal(size(x)));
  if rem(length(args), 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  for k = 1:2:length(args)
    if ~isTextScalarLocal(args{k})
      error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Name');
    end
    name = lower(char(args{k}));
    switch name
      case 'alpha'
        opts.Alpha = parseAlphaLocal(args{k + 1});
      case 'tail'
        opts.Tail = parseChoiceLocal(args{k + 1}, {'both', 'right', 'left'}, 'TAIL');
      case 'method'
        opts.Method = parseChoiceLocal(args{k + 1}, {'auto', 'exact', 'approximate'}, 'METHOD');
      case 'dim'
        opts.Dim = parseDimLocal(args{k + 1});
      otherwise
        error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
    end
  end
end
%=============================================================================
function [xr, yr, outSize, perm] = reshapeInputsLocal(x, y, dim)
  validateArrayLocal(x, 'X');
  validateArrayLocal(y, 'Y');
  if isvector(x) && isvector(y)
    xr = double(x(:));
    yr = double(y(:));
    outSize = [1 1];
    perm = [1 2];
    return
  end
  sx = size(x);
  sy = size(y);
  nd = max([length(sx), length(sy), dim]);
  sx(end + 1:nd) = 1;
  sy(end + 1:nd) = 1;
  keep = true(1, nd);
  keep(dim) = false;
  if any(sx(keep) ~= sy(keep))
    error('Nelson:statistics:xYCompatibleSizesOutside', 'X and Y must have %s.', 'compatible sizes outside the test dimension');
  end
  perm = 1:nd;
  perm([1 dim]) = [dim 1];
  xp = permute(double(x), perm);
  yp = permute(double(y), perm);
  psx = size(xp);
  psy = size(yp);
  psx(end + 1:nd) = 1;
  psy(end + 1:nd) = 1;
  xr = reshape(xp, psx(1), prod(psx(2:end)));
  yr = reshape(yp, psy(1), prod(psy(2:end)));
  outSize = psx;
  outSize(1) = 1;
end
%=============================================================================
function [h, p, w, z] = ansariVectorLocal(x, y, opts)
  x = x(~isnan(x));
  y = y(~isnan(y));
  if isempty(x) || isempty(y)
    error('Nelson:statistics:xYContainLeastOne', '%s must contain at least one non-NaN observation.', 'X and Y');
  end
  [w, scores, labels, hasTies] = ansariStatisticLocal(x, y);
  z = zStatisticLocal(w, scores, length(x), length(y));
  method = selectMethodLocal(opts.Method, length(x) + length(y), hasTies);
  if strcmp(method, 'exact')
    p = exactPValueLocal(w, scores, length(x), opts.Tail);
  else
    p = approximatePValueLocal(z, opts.Tail);
  end
  h = p <= opts.Alpha;
  labels;
end
%=============================================================================
function [w, scores, labels, hasTies] = ansariStatisticLocal(x, y)
  n1 = length(x);
  values = [x(:); y(:)];
  labels = [ones(n1, 1); zeros(length(y), 1)];
  [scores, hasTies] = ansariScoresLocal(values);
  w = sum(scores(labels == 1));
end
%=============================================================================
function [scores, hasTies] = ansariScoresLocal(values)
  [sortedValues, order] = sort(values(:));
  n = length(sortedValues);
  base = min((1:n)', (n:-1:1)');
  scores = zeros(n, 1);
  hasTies = false;
  starts = [1; find(diff(sortedValues) ~= 0) + 1];
  stops = [starts(2:end) - 1; n];
  for k = 1:length(starts)
    rows = starts(k):stops(k);
    scores(order(rows)) = mean(base(rows));
    hasTies = hasTies || length(rows) > 1;
  end
end
%=============================================================================
function z = zStatisticLocal(w, scores, n1, n2)
  n = n1 + n2;
  meanScore = mean(scores);
  mu = n1 * meanScore;
  variance = n1 * n2 / (n * (n - 1)) * sum((scores - meanScore) .^ 2);
  if variance <= 0
    z = 0;
  else
    z = (w - mu) / sqrt(variance);
  end
end
%=============================================================================
function method = selectMethodLocal(method, n, hasTies)
  if strcmp(method, 'exact') && hasTies
    error('Nelson:statistics:exactMethodSupportedTiesPresent', 'Exact method is not supported when ties are present.');
  end
  if strcmp(method, 'auto')
    if n <= 25 && ~hasTies
      method = 'exact';
    else
      method = 'approximate';
    end
  end
end
%=============================================================================
function p = approximatePValueLocal(z, tail)
  switch tail
    case 'right'
      p = normcdf(z, 0, 1);
    case 'left'
      p = normcdf(z, 0, 1, 'upper');
    otherwise
      p = 2 * normcdf(-abs(z), 0, 1);
  end
end
%=============================================================================
function p = exactPValueLocal(w, scores, n1, tail)
  scaled = round(scores(:)' * 2);
  target = round(w * 2);
  maxSum = sum(scaled);
  states = zeros(n1 + 1, maxSum + 1);
  states(1, 1) = 1;
  for value = scaled
    for k = n1:-1:1
      states(k + 1, value + 1:end) = states(k + 1, value + 1:end) ...
        + states(k, 1:end - value);
    end
  end
  counts = states(n1 + 1, :);
  idx = target + 1;
  total = sum(counts);
  switch tail
    case 'right'
      p = sum(counts(1:idx)) / total;
    case 'left'
      p = sum(counts(idx:end)) / total;
    otherwise
      lo = sum(counts(1:idx)) / total;
      hi = sum(counts(idx:end)) / total;
      p = min(1, 2 * min(lo, hi));
  end
end
%=============================================================================
function validateArrayLocal(x, name)
  if ~(isnumeric(x) && isreal(x) && ~issparse(x))
    error('Nelson:statistics:mustBeRealNumeric', '%s must be a real numeric array.', name);
  end
end
%=============================================================================
function value = parseAlphaLocal(value)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) ...
      && value > 0 && value < 1)
    error('Nelson:statistics:alphaOutOfRange', '%s must be a scalar in the range (0,1).', 'ALPHA');
  end
  value = double(value);
end
%=============================================================================
function value = parseDimLocal(value)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && value == fix(value) && value >= 1)
    error('Nelson:statistics:dimensionArgumentPositiveIntegerScalar:valueAPositiveIntegerScalar', '%s must be a positive integer scalar.', 'Dimension argument');
  end
  value = double(value);
end
%=============================================================================
function value = parseChoiceLocal(value, choices, name)
  if ~isTextScalarLocal(value)
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', name);
  end
  text = lower(char(value));
  matches = choices(startsWith(choices, text));
  if length(matches) ~= 1
    error('Nelson:statistics:invalidValue:valueHasUnsupportedValue', '%s has an unsupported value.', name);
  end
  value = matches{1};
end
%=============================================================================
function dim = firstNonsingletonLocal(sz)
  dim = 1;
  for k = 1:length(sz)
    if sz(k) ~= 1
      dim = k;
      return
    end
  end
end
%=============================================================================
function tf = isTextScalarLocal(value)
  tf = ischar(value) || (isstring(value) && isscalar(value));
end
%=============================================================================
