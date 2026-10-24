%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [h, p, stats] = chi2gof(x, varargin)
  narginchk(1, Inf);
  nargoutchk(0, 3);
  opts = parseOptionsLocal(convertStringToCharArgs(varargin));
  [values, frequency] = validateSampleLocal(x, opts.Frequency);
  edges = selectEdgesLocal(values, opts);
  observed = observedCountsLocal(values, frequency, edges);
  expected = expectedCountsLocal(edges, values, frequency, opts);
  [observed, expected] = poolBinsLocal(observed, expected, opts.EMin);
  [p, chi2stat, df] = pValueLocal(observed, expected, opts.NParams);
  h = p <= opts.Alpha;
  if isnan(p)
    h = false;
  end
  stats = struct('chi2stat', chi2stat, 'df', df, 'edges', edges, ...
    'O', observed, 'E', expected);
end
%=============================================================================
function opts = parseOptionsLocal(args)
  opts = struct('Alpha', 0.05, 'NBins', [], 'Ctrs', [], 'Edges', [], ...
    'CDF', [], 'Expected', [], 'Frequency', [], 'NParams', 0, 'EMin', 5);
  if rem(length(args), 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  k = 1;
  while k <= length(args)
    if ~(ischar(args{k}) || (isstring(args{k}) && isscalar(args{k})))
      error('Nelson:statistics:nameValueArgumentNameText', 'Name-value argument name must be text.');
    end
    name = lower(char(args{k}));
    value = args{k + 1};
    switch name
      case 'alpha'
        opts.Alpha = scalarInRangeLocal(value, 'ALPHA', 0, 1);
      case 'nbins'
        opts.NBins = positiveIntegerLocal(value, 'NBINS');
      case {'ctrs', 'centers'}
        opts.Ctrs = numericVectorLocal(value, 'CTRS');
      case 'edges'
        opts.Edges = numericVectorLocal(value, 'EDGES');
      case 'cdf'
        opts.CDF = value;
      case 'expected'
        opts.Expected = numericVectorLocal(value, 'EXPECTED');
      case 'frequency'
        opts.Frequency = value;
      case 'nparams'
        opts.NParams = nonnegativeIntegerLocal(value, 'NPARAMS');
      case 'emin'
        opts.EMin = positiveScalarLocal(value, 'EMIN');
      otherwise
        error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
    end
    k = k + 2;
  end
  validateOptionCombinationLocal(opts);
end
%=============================================================================
function validateOptionCombinationLocal(opts)
  nSpecified = ~isempty(opts.Ctrs) + ~isempty(opts.Edges);
  if nSpecified > 1
    error('Nelson:statistics:CTRSEDGESBothSpecified', 'CTRS and EDGES cannot both be specified.');
  end
  if ~isempty(opts.NBins) && (~isempty(opts.Ctrs) || ~isempty(opts.Edges))
    error('Nelson:statistics:NBINSCombinedCTRSEDGES', 'NBINS cannot be combined with CTRS or EDGES.');
  end
end
%=============================================================================
function [values, frequency] = validateSampleLocal(x, frequency)
  if ~(isnumeric(x) && isreal(x) && ~issparse(x) && isvector(x))
    error('Nelson:statistics:xRealNumericVector', '%s must be a real numeric vector.', 'X');
  end
  values = double(x(:));
  if isempty(frequency)
    frequency = ones(size(values));
  elseif ~(isnumeric(frequency) && isreal(frequency) && isvector(frequency))
    error('Nelson:statistics:FREQUENCYRealNumericVector', '%s must be a real numeric vector.', 'FREQUENCY');
  else
    frequency = double(frequency(:));
  end
  if length(frequency) ~= length(values)
    error('Nelson:statistics:FREQUENCYOneValueObservation', '%s must have one value per observation.', 'FREQUENCY');
  end
  keep = isfinite(values) & isfinite(frequency) & frequency > 0;
  values = values(keep);
  frequency = frequency(keep);
  if isempty(values)
    error('Nelson:statistics:xContainLeastOneFinite', 'X must contain at least one finite observation.');
  end
end
%=============================================================================
function edges = selectEdgesLocal(values, opts)
  if ~isempty(opts.Edges)
    edges = opts.Edges(:).';
    if length(edges) < 2 || any(diff(edges) <= 0)
      error('Nelson:statistics:EDGESStrictlyIncreasingVector', '%s must be a strictly increasing vector.', 'EDGES');
    end
  elseif ~isempty(opts.Ctrs)
    edges = centersToEdgesLocal(opts.Ctrs);
  elseif ~isempty(opts.Expected)
    edges = finiteRangeEdgesLocal(values, length(opts.Expected));
  elseif ~isempty(opts.CDF)
    edges = cdfDefaultEdgesLocal(values, opts.CDF, defaultBinCountLocal(opts));
  else
    edges = norminv((0:defaultBinCountLocal(opts)) / defaultBinCountLocal(opts));
  end
end
%=============================================================================
function n = defaultBinCountLocal(opts)
  if isempty(opts.NBins)
    n = 10;
  else
    n = opts.NBins;
  end
end
%=============================================================================
function edges = centersToEdgesLocal(centers)
  centers = centers(:).';
  if isempty(centers) || any(diff(centers) <= 0)
    error('Nelson:statistics:CTRSStrictlyIncreasingVector', '%s must be a strictly increasing vector.', 'CTRS');
  end
  if isscalar(centers)
    edges = [-Inf Inf];
  else
    midpoints = (centers(1:end - 1) + centers(2:end)) / 2;
    edges = [-Inf midpoints Inf];
  end
end
%=============================================================================
function edges = finiteRangeEdgesLocal(values, nBins)
  lo = min(values);
  hi = max(values);
  if lo == hi
    lo = lo - 0.5;
    hi = hi + 0.5;
  end
  edges = linspace(lo, hi, nBins + 1);
  edges(1) = -Inf;
  edges(end) = Inf;
end
%=============================================================================
function edges = cdfDefaultEdgesLocal(values, cdfSpec, nBins)
  probabilities = (0:nBins) / nBins;
  if isnumeric(cdfSpec)
    [support, cdfValues] = validateCdfMatrixLocal(cdfSpec);
    inner = interp1(cdfValues, support, probabilities(2:end - 1), 'linear');
    inner = inner(:).';
    edges = [-Inf inner Inf];
  else
    edges = finiteRangeEdgesLocal(values, nBins);
  end
end
%=============================================================================
function observed = observedCountsLocal(values, frequency, edges)
  observed = zeros(1, length(edges) - 1);
  [values, order] = sort(values);
  frequency = frequency(order);
  bin = 1;
  for k = 1:length(values)
    while bin < length(edges) - 1 && values(k) > edges(bin + 1)
      bin = bin + 1;
    end
    if values(k) >= edges(bin) && values(k) <= edges(bin + 1)
      observed(bin) = observed(bin) + frequency(k);
    end
  end
end
%=============================================================================
function expected = expectedCountsLocal(edges, values, frequency, opts)
  total = sum(frequency);
  if ~isempty(opts.Expected)
    expected = opts.Expected(:).';
    if length(expected) ~= length(edges) - 1
      error('Nelson:statistics:EXPECTEDMatchNumberBins', 'EXPECTED must match the number of bins.');
    end
    if any(expected < 0) || sum(expected) <= 0
      error('Nelson:statistics:EXPECTEDContainNonnegativeCountsPositive', 'EXPECTED must contain nonnegative counts with positive sum.');
    end
    expected = expected * total / sum(expected);
    return
  end
  cdfValues = evaluateCdfLocal(edges, opts.CDF);
  expected = total * diff(cdfValues(:).');
  expected(expected < 0 & expected > -sqrt(eps)) = 0;
  if any(expected < 0)
    error('Nelson:statistics:CDFValuesNondecreasing', 'CDF values must be nondecreasing.');
  end
end
%=============================================================================
function cdfValues = evaluateCdfLocal(edges, cdfSpec)
  if isempty(cdfSpec)
    cdfValues = normcdf(edges);
  elseif isnumeric(cdfSpec)
    [support, probabilities] = validateCdfMatrixLocal(cdfSpec);
    cdfValues = interp1(support, probabilities, edges, 'linear', NaN);
    cdfValues(isinf(edges) & edges < 0) = 0;
    cdfValues(isinf(edges) & edges > 0) = 1;
  elseif isa(cdfSpec, 'function_handle')
    cdfValues = feval(cdfSpec, edges);
  else
    error('Nelson:statistics:CDFTwoColumnMatrixFunction', 'CDF must be a two-column matrix or function handle.');
  end
  cdfValues = double(cdfValues(:).');
  if length(cdfValues) ~= length(edges)
    error('Nelson:statistics:invalidCdfOutput', 'CDF function must return %s.', 'one value per edge');
  end
  if any(isnan(cdfValues)) || any(cdfValues < 0 | cdfValues > 1)
    error('Nelson:statistics:invalidCdf', '%s must be in the range [0,1].', 'CDF values');
  end
end
%=============================================================================
function [support, probabilities] = validateCdfMatrixLocal(cdfSpec)
  if ~(isnumeric(cdfSpec) && isreal(cdfSpec) && size(cdfSpec, 2) == 2)
    error('Nelson:statistics:CDFTwoColumnMatrixFunction', 'CDF must be a two-column matrix or function handle.');
  end
  support = double(cdfSpec(:, 1));
  probabilities = double(cdfSpec(:, 2));
  if any(diff(support) <= 0) || any(diff(probabilities) < 0)
    error('Nelson:statistics:CDFMatrixValuesSortedNondecreasing', 'CDF matrix values must be sorted and nondecreasing.');
  end
  if any(probabilities < 0 | probabilities > 1)
    error('Nelson:statistics:invalidCdf', '%s must be in the range [0,1].', 'CDF values');
  end
end
%=============================================================================
function [observed, expected] = poolBinsLocal(observed, expected, eMin)
  while length(expected) > 1 && any(expected < eMin)
    [~, idx] = min(expected);
    if idx == 1
      target = 2;
    elseif idx == length(expected)
      target = idx - 1;
    elseif expected(idx - 1) <= expected(idx + 1)
      target = idx - 1;
    else
      target = idx + 1;
    end
    observed(target) = observed(target) + observed(idx);
    expected(target) = expected(target) + expected(idx);
    observed(idx) = [];
    expected(idx) = [];
  end
end
%=============================================================================
function [p, chi2stat, df] = pValueLocal(observed, expected, nParams)
  valid = expected > 0;
  chi2stat = sum(((observed(valid) - expected(valid)) .^ 2) ./ expected(valid));
  df = sum(valid) - 1 - nParams;
  if df <= 0
    p = NaN;
  else
    p = chi2cdf(chi2stat, df, 'upper');
  end
end
%=============================================================================
function value = scalarInRangeLocal(value, name, lo, hi)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && value > lo && value < hi)
    error('Nelson:statistics:mustBeInRange', '%s must be a scalar in the range (0,1).', name);
  end
  value = double(value);
end
%=============================================================================
function value = positiveScalarLocal(value, name)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && value > 0)
    error('Nelson:statistics:mustBePositive', '%s must be a positive scalar.', name);
  end
  value = double(value);
end
%=============================================================================
function value = positiveIntegerLocal(value, name)
  value = positiveScalarLocal(value, name);
  if value ~= fix(value)
    error('Nelson:statistics:mustBePositiveInteger', '%s must be a positive integer scalar.', name);
  end
end
%=============================================================================
function value = nonnegativeIntegerLocal(value, name)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && value >= 0 && value == fix(value))
    error('Nelson:statistics:mustBeNonnegativeInteger', '%s must be a nonnegative integer scalar.', name);
  end
  value = double(value);
end
%=============================================================================
function value = numericVectorLocal(value, name)
  if ~(isnumeric(value) && isreal(value) && isvector(value))
    error('Nelson:statistics:mustBeRealVector', '%s must be a real numeric vector.', name);
  end
  value = double(value(:).');
end
%=============================================================================
