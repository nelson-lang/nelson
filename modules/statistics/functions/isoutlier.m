%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [TF, L, U, C] = isoutlier(A, varargin)
  narginchk(1, Inf);
  nargoutchk(0, 4);
  if ~(isnumeric(A) && isreal(A) && ~issparse(A))
    error('Nelson:statistics:aMustBeRealArray', '%s must be a real numeric array.', 'A');
  end
  opts = parseInputsLocal(convertStringToCharArgs(varargin), size(A));
  [TF, L, U, C] = applyAlongDimensionLocal(double(A), opts);
end
%=============================================================================
function opts = parseInputsLocal(args, sz)
  opts = struct('Method', 'median', 'Dim', firstNonsingletonLocal(sz), ...
    'ThresholdFactor', [], 'MaxNumOutliers', [], 'Percentiles', [], ...
    'Window', [], 'Moving', false, 'SamplePoints', []);
  idx = 1;
  if idx <= length(args)
    value = args{idx};
    if isTextScalarLocal(value)
      if ~isLeadingNameValueLocal(value)
        method = parseMethodLocal(value);
        opts.Method = method;
        idx = idx + 1;
        if strcmp(method, 'percentiles')
          if idx > length(args)
            error('Nelson:statistics:percentileThresholdsSpecified', '%s must be specified.', 'Percentile thresholds');
          end
          opts.Percentiles = parsePercentilesLocal(args{idx});
          idx = idx + 1;
        elseif startsWith(method, 'mov')
          if idx > length(args)
            error('Nelson:statistics:windowLengthSpecified', '%s must be specified.', 'Window length');
          end
          opts.Window = parseWindowLocal(args{idx});
          opts.Moving = true;
          idx = idx + 1;
        end
      end
    elseif isDimScalarLocal(value)
      opts.Dim = double(value);
      idx = idx + 1;
    end
  end
  if idx <= length(args) && isDimScalarLocal(args{idx})
    opts.Dim = double(args{idx});
    idx = idx + 1;
  end
  opts = parseNameValuesLocal(opts, args(idx:end));
  opts = setDefaultsLocal(opts);
end
%=============================================================================
function opts = parseNameValuesLocal(opts, args)
  if rem(length(args), 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  names = {'thresholdfactor', 'maxnumoutliers', 'samplepoints', 'datavariables', 'outputformat'};
  for k = 1:2:length(args)
    name = matchedNameLocal(args{k}, names);
    switch name
      case 'thresholdfactor'
        if strcmp(opts.Method, 'percentiles')
          error('Nelson:statistics:thresholdFactorSupportedPercentilesMethod', 'ThresholdFactor is not supported with the percentiles method.');
        end
        opts.ThresholdFactor = parseThresholdFactorLocal(args{k + 1}, opts.Method);
      case 'maxnumoutliers'
        opts.MaxNumOutliers = parsePositiveIntegerLocal(args{k + 1}, 'MaxNumOutliers');
      case 'samplepoints'
        opts.SamplePoints = parseSamplePointsLocal(args{k + 1});
      case {'datavariables', 'outputformat'}
        error('Nelson:statistics:invalidUsage', '%s is not supported for numeric array input.', canonicalNameLocal(name));
    end
  end
end
%=============================================================================
function opts = setDefaultsLocal(opts)
  if isempty(opts.ThresholdFactor)
    switch opts.Method
      case {'median', 'mean', 'movmedian', 'movmean'}
        opts.ThresholdFactor = 3;
      case 'quartiles'
        opts.ThresholdFactor = 1.5;
      case {'grubbs', 'gesd'}
        opts.ThresholdFactor = 0.05;
      otherwise
        opts.ThresholdFactor = NaN;
    end
  end
end
%=============================================================================
function [TF, L, U, C] = applyAlongDimensionLocal(A, opts)
  sz = size(A);
  originalRank = max(2, length(sz));
  nd = max(originalRank, opts.Dim);
  sz(end + 1:nd) = 1;
  opts = validateSamplePointsLocal(opts, sz(opts.Dim));
  perm = [opts.Dim, setdiff(1:nd, opts.Dim, 'stable')];
  Ap = permute(reshape(A, sz), perm);
  psize = size(Ap);
  psize(end + 1:nd) = 1;
  n = psize(1);
  cols = prod(psize(2:end));
  [TFr, lr, ur, cr] = processColumnsLocal(reshape(Ap, n, cols), opts);
  TF = reshape(ipermute(reshape(TFr, psize), perm), sz(1:originalRank));
  if opts.Moving
    L = reshape(ipermute(reshape(lr, psize), perm), sz(1:originalRank));
    U = reshape(ipermute(reshape(ur, psize), perm), sz(1:originalRank));
    C = reshape(ipermute(reshape(cr, psize), perm), sz(1:originalRank));
  else
    outSize = sz;
    outSize(opts.Dim) = 1;
    outSize = outSize(1:originalRank);
    tailSize = psize(2:end);
    L = reshape(ipermute(reshape(lr, [1, tailSize]), perm), outSize);
    U = reshape(ipermute(reshape(ur, [1, tailSize]), perm), outSize);
    C = reshape(ipermute(reshape(cr, [1, tailSize]), perm), outSize);
  end
end
%=============================================================================
function [TF, L, U, C] = processColumnsLocal(X, opts)
  [n, cols] = size(X);
  TF = false(n, cols);
  if opts.Moving
    L = NaN(n, cols);
    U = NaN(n, cols);
    C = NaN(n, cols);
  else
    L = NaN(1, cols);
    U = NaN(1, cols);
    C = NaN(1, cols);
  end
  if n == 0
    return
  end
  for col = 1:cols
    [tfCol, lCol, uCol, cCol] = processVectorLocal(X(:, col), opts);
    TF(:, col) = tfCol;
    L(:, min(col, size(L, 2))) = lCol;
    U(:, min(col, size(U, 2))) = uCol;
    C(:, min(col, size(C, 2))) = cCol;
  end
end
%=============================================================================
function [tf, L, U, C] = processVectorLocal(x, opts)
  if opts.Moving
    [tf, L, U, C] = movingVectorLocal(x, opts);
    return
  end
  switch opts.Method
    case 'median'
      [tf, L, U, C] = medianVectorLocal(x, opts.ThresholdFactor);
    case 'mean'
      [tf, L, U, C] = meanVectorLocal(x, opts.ThresholdFactor);
    case 'quartiles'
      [tf, L, U, C] = quartilesVectorLocal(x, opts.ThresholdFactor);
    case 'percentiles'
      [tf, L, U, C] = percentilesVectorLocal(x, opts.Percentiles);
    case 'grubbs'
      [tf, L, U, C] = grubbsVectorLocal(x, opts.ThresholdFactor);
    otherwise
      [tf, L, U, C] = gesdVectorLocal(x, opts.ThresholdFactor, opts.MaxNumOutliers);
  end
end
%=============================================================================
function [tf, L, U, C] = medianVectorLocal(x, factor)
  good = ~isnan(x);
  values = x(good);
  if isempty(values)
    [tf, L, U, C] = emptyThresholdLocal(x);
    return
  end
  C = median(values);
  scale = scaledMadLocal(values, C);
  L = C - factor * scale;
  U = C + factor * scale;
  tf = false(size(x));
  tf(good) = values < L | values > U;
end
%=============================================================================
function [tf, L, U, C] = meanVectorLocal(x, factor)
  good = ~isnan(x);
  values = x(good);
  if isempty(values)
    [tf, L, U, C] = emptyThresholdLocal(x);
    return
  end
  C = mean(values);
  scale = std(values, 0);
  L = C - factor * scale;
  U = C + factor * scale;
  tf = false(size(x));
  tf(good) = values < L | values > U;
end
%=============================================================================
function [tf, L, U, C] = quartilesVectorLocal(x, factor)
  good = ~isnan(x);
  values = x(good);
  if isempty(values)
    [tf, L, U, C] = emptyThresholdLocal(x);
    return
  end
  q = quantile(values, [0.25 0.75]);
  C = median(values);
  spread = q(2) - q(1);
  L = q(1) - factor * spread;
  U = q(2) + factor * spread;
  tf = false(size(x));
  tf(good) = values < L | values > U;
end
%=============================================================================
function [tf, L, U, C] = percentilesVectorLocal(x, percentiles)
  good = ~isnan(x);
  values = x(good);
  if isempty(values)
    [tf, L, U, C] = emptyThresholdLocal(x);
    return
  end
  bounds = quantile(values, percentiles / 100);
  L = bounds(1);
  U = bounds(2);
  C = median(values);
  tf = false(size(x));
  tf(good) = values < L | values > U;
end
%=============================================================================
function [tf, L, U, C] = grubbsVectorLocal(x, alpha)
  tf = false(size(x));
  [values, positions] = nonNanValuesLocal(x);
  if isempty(values)
    L = NaN;
    U = NaN;
    C = NaN;
    return
  end
  [L, U, C] = meanThresholdLocal(values, grubbsCriticalLocal(length(values), alpha));
  while length(values) >= 3
    [R, pos] = studentizedExtremeLocal(values);
    if R <= grubbsCriticalLocal(length(values), alpha)
      break
    end
    tf(positions(pos)) = true;
    values(pos) = [];
    positions(pos) = [];
  end
end
%=============================================================================
function [tf, L, U, C] = gesdVectorLocal(x, alpha, maxOutliers)
  tf = false(size(x));
  [values, positions] = nonNanValuesLocal(x);
  n = length(values);
  if n == 0
    L = NaN;
    U = NaN;
    C = NaN;
    return
  end
  if isempty(maxOutliers)
    maxOutliers = max(1, round(0.1 * n));
  end
  maxOutliers = min([maxOutliers, max(n - 2, 0)]);
  if maxOutliers == 0
    [L, U, C] = meanThresholdLocal(values, Inf);
    return
  end
  removed = zeros(1, maxOutliers);
  ratios = zeros(1, maxOutliers);
  lambdas = zeros(1, maxOutliers);
  work = values;
  workPositions = positions;
  for k = 1:maxOutliers
    [ratios(k), pos] = studentizedExtremeLocal(work);
    removed(k) = workPositions(pos);
    lambdas(k) = grubbsCriticalLocal(length(work), alpha);
    work(pos) = [];
    workPositions(pos) = [];
  end
  count = find(ratios > lambdas, 1, 'last');
  if ~isempty(count)
    tf(removed(1:count)) = true;
  end
  [L, U, C] = meanThresholdLocal(values, lambdas(1));
end
%=============================================================================
function [tf, L, U, C] = emptyThresholdLocal(x)
  tf = false(size(x));
  L = NaN;
  U = NaN;
  C = NaN;
end
%=============================================================================
function [tf, L, U, C] = movingVectorLocal(x, opts)
  n = length(x);
  tf = false(size(x));
  L = NaN(size(x));
  U = NaN(size(x));
  C = NaN(size(x));
  for k = 1:n
    rows = movingRowsLocal(k, n, opts);
    values = x(rows);
    switch opts.Method
      case 'movmedian'
        [~, lk, uk, ck] = medianVectorLocal(values, opts.ThresholdFactor);
      otherwise
        [~, lk, uk, ck] = meanVectorLocal(values, opts.ThresholdFactor);
    end
    L(k) = lk;
    U(k) = uk;
    C(k) = ck;
    tf(k) = ~isnan(x(k)) && (x(k) < lk || x(k) > uk);
  end
end
%=============================================================================
function scale = scaledMadLocal(values, center)
  if isempty(values)
    scale = NaN;
  else
    scale = -median(abs(values - center)) / (sqrt(2) * erfcinv(1.5));
  end
end
%=============================================================================
function [values, positions] = nonNanValuesLocal(x)
  positions = find(~isnan(x));
  values = x(positions);
end
%=============================================================================
function [R, pos] = studentizedExtremeLocal(values)
  center = mean(values);
  scale = std(values, 0);
  if scale == 0 || isnan(scale)
    R = 0;
    pos = 1;
  else
    [R, pos] = max(abs(values - center) / scale);
  end
end
%=============================================================================
function crit = grubbsCriticalLocal(n, alpha)
  if n < 3
    crit = Inf;
    return
  end
  t = tinv(1 - alpha / (2 * n), n - 2);
  crit = ((n - 1) / sqrt(n)) * sqrt((t ^ 2) / (n - 2 + t ^ 2));
end
%=============================================================================
function [L, U, C] = meanThresholdLocal(values, factor)
  C = mean(values);
  scale = std(values, 0);
  L = C - factor * scale;
  U = C + factor * scale;
end
%=============================================================================
function method = parseMethodLocal(value)
  choices = {'median', 'mean', 'quartiles', 'grubbs', 'gesd', ...
    'percentiles', 'movmedian', 'movmean'};
  method = parseChoiceLocal(value, choices, 'METHOD');
end
%=============================================================================
function threshold = parseThresholdFactorLocal(value, method)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value))
    error('Nelson:statistics:thresholdFactorFiniteScalar', 'ThresholdFactor must be a finite scalar.');
  end
  threshold = double(value);
  if any(strcmp(method, {'grubbs', 'gesd'}))
    if threshold <= 0 || threshold >= 1
      error('Nelson:statistics:thresholdFactorScalarRange01', 'ThresholdFactor must be a scalar in the range (0,1) for this method.');
    end
  elseif threshold < 0
    error('Nelson:statistics:thresholdFactorNonnegativeScalar', '%s must be a nonnegative scalar.', 'ThresholdFactor');
  end
end
%=============================================================================
function percentiles = parsePercentilesLocal(value)
  if ~(isnumeric(value) && isreal(value) && isequal(size(value), [1 2]) ...
      && all(isfinite(value)) && value(1) >= 0 && value(2) <= 100 && value(1) < value(2))
    error('Nelson:statistics:percentileThresholdsTwoElementRow', 'Percentile thresholds must be a two-element row vector in increasing order.');
  end
  percentiles = double(value);
end
%=============================================================================
function window = parseWindowLocal(value)
  if ~(isnumeric(value) && isreal(value) && isvector(value) && (length(value) == 1 || length(value) == 2) ...
      && all(isfinite(value)) && all(value > 0))
    error('Nelson:statistics:windowLengthPositiveNumericScalar', 'Window length must be a positive numeric scalar or two-element vector.');
  end
  window = double(value(:).');
end
%=============================================================================
function samplePoints = parseSamplePointsLocal(value)
  if ~(isnumeric(value) && isreal(value) && isvector(value) && all(isfinite(value)))
    error('Nelson:statistics:samplePointsFiniteRealNumericVector', 'SamplePoints must be a finite real numeric vector.');
  end
  samplePoints = double(value(:));
end
%=============================================================================
function value = parsePositiveIntegerLocal(value, name)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) ...
      && value == fix(value) && value > 0)
    error('Nelson:statistics:mustBePositiveInteger', '%s must be a positive integer scalar.', name);
  end
  value = double(value);
end
%=============================================================================
function opts = validateSamplePointsLocal(opts, n)
  if isempty(opts.SamplePoints)
    if opts.Moving && any(opts.Window ~= fix(opts.Window))
      error('Nelson:statistics:windowLengthPositiveIntegerScalar', 'Window length must be a positive integer scalar or two-element vector.');
    end
    return
  end
  if length(opts.SamplePoints) ~= n
    error('Nelson:statistics:samplePointsOneElementInputElement', 'SamplePoints must have one element for each input element along the operating dimension.');
  end
  if any(diff(opts.SamplePoints) <= 0)
    error('Nelson:statistics:samplePointsSortedContainUniqueValues', 'SamplePoints must be sorted and contain unique values.');
  end
end
%=============================================================================
function rows = movingRowsLocal(k, n, opts)
  if isempty(opts.SamplePoints)
    if length(opts.Window) == 1
      back = floor(opts.Window / 2);
      forward = opts.Window - back - 1;
    else
      back = opts.Window(1);
      forward = opts.Window(2);
    end
    rows = max(1, k - back):min(n, k + forward);
  else
    if length(opts.Window) == 1
      lower = opts.SamplePoints(k) - opts.Window / 2;
      upper = opts.SamplePoints(k) + opts.Window / 2;
    else
      lower = opts.SamplePoints(k) - opts.Window(1);
      upper = opts.SamplePoints(k) + opts.Window(2);
    end
    rows = find(opts.SamplePoints >= lower & opts.SamplePoints <= upper);
  end
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
function tf = isDimScalarLocal(value)
  tf = isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) ...
    && value == fix(value) && value >= 1;
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
function name = matchedNameLocal(value, names)
  if ~isTextScalarLocal(value)
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Name');
  end
  text = lower(char(value));
  matches = names(startsWith(names, text));
  if length(matches) ~= 1
    error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
  end
  name = matches{1};
end
%=============================================================================
function name = canonicalNameLocal(name)
  switch name
    case 'samplepoints'
      name = 'SamplePoints';
    case 'datavariables'
      name = 'DataVariables';
    otherwise
      name = 'OutputFormat';
  end
end
%=============================================================================
function tf = isLeadingNameValueLocal(value)
  names = {'thresholdfactor', 'maxnumoutliers', 'samplepoints', 'datavariables', 'outputformat'};
  text = lower(char(value));
  matches = names(startsWith(names, text));
  tf = length(matches) == 1;
end
%=============================================================================
function tf = isTextScalarLocal(value)
  tf = ischar(value) || (isstring(value) && isscalar(value));
end
%=============================================================================
