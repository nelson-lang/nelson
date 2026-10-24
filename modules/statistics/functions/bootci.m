%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [ci, bootstat] = bootci(varargin)
  narginchk(3, Inf);
  nargoutchk(0, 2);

  nboot = positiveInteger(varargin{1}, sprintf(_('%s must be a positive integer scalar.'), 'NBOOT'));
  [bootfun, dataArgs, nv] = parseInputs(varargin(2:end));
  opts = parseOptions(nv);
  [n, sampleMask, dataArgs] = validateData(dataArgs);
  original = statisticRow(bootfun, dataArgs);
  bootstat = zeros(nboot, length(original));

  for b = 1:nboot
    indices = sampleIndices(n, opts.Weights);
    bootArgs = makeBootstrapArgs(dataArgs, sampleMask, indices);
    bootstat(b, :) = statisticRow(bootfun, bootArgs);
  end

  alpha = opts.Alpha;
  switch opts.Type
    case 'norm'
      ci = normalInterval(bootstat, original, alpha);
    case 'per'
      ci = percentileInterval(bootstat, alpha / 2, 1 - alpha / 2);
    case 'cper'
      ci = correctedPercentileInterval(bootstat, original, alpha);
    case 'bca'
      ci = bcaInterval(bootfun, dataArgs, sampleMask, bootstat, original, alpha);
    otherwise
      error('Nelson:statistics:studentizedBootstrapConfidenceIntervalsYet', 'Studentized bootstrap confidence intervals are not yet supported.');
  end
end
%=============================================================================
function [bootfun, dataArgs, nv] = parseInputs(args)
  if iscell(args{1})
    bootSpec = args{1};
    if isempty(bootSpec) || ~isa(bootSpec{1}, 'function_handle')
      error('Nelson:statistics:BOOTFUNFunctionHandle', '%s must be a function handle.', 'BOOTFUN');
    end
    bootfun = bootSpec{1};
    dataArgs = bootSpec(2:end);
    nv = args(2:end);
  else
    if ~isa(args{1}, 'function_handle')
      error('Nelson:statistics:BOOTFUNFunctionHandle', '%s must be a function handle.', 'BOOTFUN');
    end
    bootfun = args{1};
    firstNameValue = length(args) + 1;
    for k = 2:length(args)
      if isTextScalar(args{k})
        firstNameValue = k;
        break
      end
    end
    dataArgs = args(2:firstNameValue - 1);
    nv = args(firstNameValue:end);
  end
  if isempty(dataArgs)
    error('Nelson:statistics:leastOneDataArgumentRequired', 'At least one %s is required.', 'data argument');
  end
  if rem(length(nv), 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
end
%=============================================================================
function opts = parseOptions(nv)
  opts = struct('Alpha', 0.05, 'Type', 'bca', 'Weights', [], 'Options', []);
  for k = 1:2:length(nv)
    name = optionName(nv{k});
    value = nv{k + 1};
    switch name
      case 'alpha'
        opts.Alpha = positiveScalarInUnit(value, _('Alpha must be a scalar between 0 and 1.'));
      case 'type'
        opts.Type = intervalType(value);
      case 'weights'
        opts.Weights = value;
      case 'options'
        opts.Options = value;
      case {'nbootstd', 'stderr'}
        opts.Type = 'stud';
      otherwise
        error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
    end
  end
end
%=============================================================================
function name = optionName(value)
  if ~isTextScalar(value)
    error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
  end
  text = lower(char(value));
  names = {'alpha', 'type', 'nbootstd', 'stderr', 'weights', 'options'};
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
function type = intervalType(value)
  text = lower(char(value));
  if strncmp('normal', text, length(text)) || strcmp(text, 'norm')
    type = 'norm';
  elseif strncmp('percentile', text, length(text)) || strcmp(text, 'per')
    type = 'per';
  elseif strcmp(text, 'cper') || strncmp('corrected percentile', text, length(text))
    type = 'cper';
  elseif strcmp(text, 'bca')
    type = 'bca';
  elseif strcmp(text, 'stud') || strncmp('student', text, length(text))
    type = 'stud';
  else
    error('Nelson:statistics:invalidTypeValue', 'Invalid %s value.', 'Type');
  end
end
%=============================================================================
function [n, sampleMask, dataArgs] = validateData(dataArgs)
  n = [];
  sampleMask = false(1, length(dataArgs));
  for k = 1:length(dataArgs)
    value = dataArgs{k};
    if isnumeric(value) || islogical(value)
      if length(dataArgs) == 1 && isvector(value)
        dataArgs{k} = value(:);
      end
      if ~isscalar(dataArgs{k})
        rows = size(dataArgs{k}, 1);
        if isempty(n)
          n = rows;
        elseif rows ~= n
          error('Nelson:statistics:nonscalarDataArgumentsSameNumber', '%s must have the same number of rows.', 'Nonscalar data arguments');
        end
        sampleMask(k) = true;
      end
    end
  end
  if isempty(n)
    error('Nelson:statistics:leastOneNonscalarDataArgument', 'At least one %s is required.', 'nonscalar data argument');
  end
end
%=============================================================================
function args = makeBootstrapArgs(dataArgs, sampleMask, indices)
  args = dataArgs;
  for k = 1:length(args)
    if sampleMask(k)
      args{k} = args{k}(indices, :);
    end
  end
end
%=============================================================================
function indices = sampleIndices(n, weights)
  if isempty(weights)
    indices = randi(n, n, 1);
    return
  end
  w = weights(:);
  if length(w) ~= n || any(w < 0) || sum(w) <= 0
    error('Nelson:statistics:weightsNonnegativeVectorOneValue', 'Weights must be a nonnegative vector with one value per observation.');
  end
  cdf = cumsum(w ./ sum(w));
  u = rand(n, 1);
  indices = ones(n, 1);
  for i = 1:n
    indices(i) = find(u(i) <= cdf, 1);
  end
end
%=============================================================================
function row = statisticRow(bootfun, args)
  value = feval(bootfun, args{:});
  row = value(:)';
end
%=============================================================================
function ci = normalInterval(bootstat, original, alpha)
  z = norminvLocal(1 - alpha / 2);
  bias = mean(bootstat, 1) - original;
  sigma = std(bootstat, 0, 1);
  ci = [original - bias - z .* sigma; original - bias + z .* sigma];
end
%=============================================================================
function ci = percentileInterval(bootstat, lo, hi)
  ci = [percentileRows(bootstat, lo); percentileRows(bootstat, hi)];
end
%=============================================================================
function ci = correctedPercentileInterval(bootstat, original, alpha)
  m = size(bootstat, 2);
  ci = zeros(2, m);
  for j = 1:m
    z0 = biasCorrection(bootstat(:, j), original(j));
    lo = normcdfLocal(2 * z0 + norminvLocal(alpha / 2));
    hi = normcdfLocal(2 * z0 + norminvLocal(1 - alpha / 2));
    ci(:, j) = percentileInterval(bootstat(:, j), lo, hi);
  end
end
%=============================================================================
function ci = bcaInterval(bootfun, dataArgs, sampleMask, bootstat, original, alpha)
  jack = jackknifeStats(bootfun, dataArgs, sampleMask);
  m = size(bootstat, 2);
  ci = zeros(2, m);
  for j = 1:m
    z0 = biasCorrection(bootstat(:, j), original(j));
    acc = acceleration(jack(:, j));
    zlo = norminvLocal(alpha / 2);
    zhi = norminvLocal(1 - alpha / 2);
    lo = normcdfLocal(z0 + (z0 + zlo) ./ (1 - acc .* (z0 + zlo)));
    hi = normcdfLocal(z0 + (z0 + zhi) ./ (1 - acc .* (z0 + zhi)));
    ci(:, j) = percentileInterval(bootstat(:, j), lo, hi);
  end
end
%=============================================================================
function jack = jackknifeStats(bootfun, dataArgs, sampleMask)
  n = size(dataArgs{find(sampleMask, 1)}, 1);
  firstArgs = deleteRows(dataArgs, sampleMask, 1);
  first = statisticRow(bootfun, firstArgs);
  jack = zeros(n, length(first));
  jack(1, :) = first;
  for i = 2:n
    jack(i, :) = statisticRow(bootfun, deleteRows(dataArgs, sampleMask, i));
  end
end
%=============================================================================
function args = deleteRows(dataArgs, sampleMask, row)
  args = dataArgs;
  for k = 1:length(args)
    if sampleMask(k)
      mask = true(size(args{k}, 1), 1);
      mask(row) = false;
      args{k} = args{k}(mask, :);
    end
  end
end
%=============================================================================
function acc = acceleration(jack)
  jm = mean(jack);
  diffs = jm - jack;
  denom = 6 * (sum(diffs .^ 2) .^ 1.5);
  if denom == 0
    acc = 0;
  else
    acc = sum(diffs .^ 3) ./ denom;
  end
end
%=============================================================================
function z0 = biasCorrection(samples, original)
  prop = (sum(samples < original) + 0.5 * sum(samples == original)) / length(samples);
  prop = min(max(prop, eps), 1 - eps);
  z0 = norminvLocal(prop);
end
%=============================================================================
function row = percentileRows(values, prob)
  values = sort(values, 1);
  n = size(values, 1);
  pos = 1 + (n - 1) * prob;
  lo = floor(pos);
  hi = ceil(pos);
  frac = pos - lo;
  row = values(lo, :) .* (1 - frac) + values(hi, :) .* frac;
end
%=============================================================================
function z = norminvLocal(p)
  z = sqrt(2) .* erfinv(2 .* p - 1);
end
%=============================================================================
function p = normcdfLocal(z)
  p = 0.5 .* (1 + erf(z ./ sqrt(2)));
end
%=============================================================================
function value = positiveInteger(value, message)
  if ~isscalar(value) || value <= 0 || floor(value) ~= value
    error('Nelson:statistics:mustBePositiveInteger', message);
  end
end
%=============================================================================
function value = positiveScalarInUnit(value, message)
  if ~isscalar(value) || value <= 0 || value >= 1
    error('Nelson:statistics:mustBeInRange', message);
  end
end
%=============================================================================
function tf = isTextScalar(value)
  tf = ischar(value) || isStringScalar(value);
end
%=============================================================================
