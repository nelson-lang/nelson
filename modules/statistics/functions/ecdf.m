%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [f, x, flo, fup] = ecdf(y, varargin)
  narginchk(1, Inf);
  nargoutchk(0, 4);
  parent = [];
  if isAxesHandleLocal(y)
    if isempty(varargin)
      error('Nelson:statistics:yRealNumericVectorTwo', 'Y must be a real numeric vector or two-column matrix.');
    end
    parent = y;
    y = varargin{1};
    varargin = varargin(2:end);
  end
  opts = parseOptionsLocal(varargin);
  [values, censoring, frequency] = prepareInputLocal(y, opts);
  [f, x, variance] = computeEcdfLocal(values, censoring, frequency, opts.Function);
  [flo, fup] = boundsLocal(f, variance, opts.Alpha, opts.Function);
  if nargout == 0
    plotEcdfLocal(parent, x, f, flo, fup, opts);
  end
end
%=============================================================================
function tf = isAxesHandleLocal(value)
  tf = false;
  if ~(isscalar(value) && exist('isgraphics') ~= 0)
    return
  end
  tf = isgraphics(value, 'axes') || isgraphics(value, 'hggroup');
end
%=============================================================================
function opts = parseOptionsLocal(args)
  opts = struct('Function', 'cdf', 'Censoring', [], 'Frequency', [], ...
    'Alpha', 0.05, 'Bounds', 'off');
  k = 1;
  while k <= length(args)
    if k == length(args)
      error('Nelson:statistics:nameValueArgumentFollowedValue', 'Name-value argument must be followed by a value.');
    end
    name = lower(char(args{k}));
    switch name
      case 'function'
        opts.Function = normalizeFunctionLocal(args{k + 1});
      case 'censoring'
        opts.Censoring = args{k + 1};
      case 'frequency'
        opts.Frequency = args{k + 1};
      case 'alpha'
        opts.Alpha = args{k + 1};
      case 'bounds'
        opts.Bounds = normalizeBoundsLocal(args{k + 1});
      case {'iterationlimit', 'tolerance', 'icmfrequency'}
        args{k + 1};
      otherwise
        error('Nelson:statistics:unknownOption', 'Unknown option.');
    end
    k = k + 2;
  end
  if ~(isnumeric(opts.Alpha) && isscalar(opts.Alpha) && opts.Alpha > 0 && opts.Alpha < 1)
    error('Nelson:statistics:alphaScalarBetween01', 'Alpha must be a scalar between 0 and 1.');
  end
end
%=============================================================================
function value = normalizeBoundsLocal(value)
  value = lower(char(value));
  switch value
    case {'on', 'off'}
    otherwise
      error('Nelson:statistics:boundsOff', 'Bounds must be ''on'' or ''off''.');
  end
end
%=============================================================================
function name = normalizeFunctionLocal(value)
  name = lower(char(value));
  switch name
    case {'cdf', 'cumulative distribution function'}
      name = 'cdf';
    case {'survivor', 'survival', 'survivor function'}
      name = 'survivor';
    case {'cumhazard', 'cumulative hazard', 'cumulative hazard function', 'hazard'}
      name = 'cumhazard';
    otherwise
      error('Nelson:statistics:unsupportedFunctionType', 'Unsupported %s.', 'function type');
  end
end
%=============================================================================
function [values, censoring, frequency] = prepareInputLocal(y, opts)
  if ~(isnumeric(y) && isreal(y) && ~issparse(y))
    error('Nelson:statistics:yRealNumericVectorTwo', 'Y must be a real numeric vector or two-column matrix.');
  end
  if ismatrix(y) && size(y, 2) == 2 && ~isvector(y)
    values = y(:, 1);
    censoring = zeros(size(values));
    exact = y(:, 1) == y(:, 2);
    right = isfinite(y(:, 1)) & isinf(y(:, 2));
    left = isinf(y(:, 1)) & isfinite(y(:, 2));
    values(right) = y(right, 1);
    values(left) = y(left, 2);
    censoring(right) = 1;
    censoring(left) = -1;
    censoring(exact) = 0;
  elseif isvector(y)
    values = y(:);
    censoring = zeros(size(values));
    if ~isempty(opts.Censoring)
      censoring = opts.Censoring(:);
    end
  else
    error('Nelson:statistics:yRealNumericVectorTwo', 'Y must be a real numeric vector or two-column matrix.');
  end
  frequency = ones(size(values));
  if ~isempty(opts.Frequency)
    frequency = opts.Frequency(:);
  end
  if length(censoring) ~= length(values) || length(frequency) ~= length(values)
    error('Nelson:statistics:mustMatchY', '%s must match Y.', 'Censoring and frequency');
  end
  keep = ~(isnan(values) | isnan(censoring) | isnan(frequency)) & frequency > 0;
  values = values(keep);
  censoring = censoring(keep);
  frequency = frequency(keep);
  if any(censoring ~= -1 & censoring ~= 0 & censoring ~= 1)
    error('Nelson:statistics:censoringValues101', 'Censoring values must be -1, 0, or 1.');
  end
end
%=============================================================================
function [f, x, variance] = computeEcdfLocal(values, censoring, frequency, functionName)
  if isempty(values)
    f = NaN;
    x = NaN;
    variance = NaN;
    return
  end
  left = censoring == -1;
  censoring(left) = 0;
  if all(censoring == 0)
    [f, x, variance] = computeUncensoredLocal(values, frequency, functionName);
    return
  end
  [values, order] = sort(values);
  censoring = censoring(order);
  frequency = frequency(order);
  eventTimes = unique(values(censoring == 0));
  if isempty(eventTimes)
    x = min(values);
    base = strcmp(functionName, 'survivor');
    f = double(base);
    variance = 0;
    return
  end
  x = [min(values); eventTimes(:)];
  survivor = ones(length(eventTimes) + 1, 1);
  greenwood = zeros(length(eventTimes) + 1, 1);
  gsum = 0;
  for k = 1:length(eventTimes)
    t = eventTimes(k);
    risk = sum(frequency(values >= t));
    events = sum(frequency(values == t & censoring == 0));
    if risk > 0
      survivor(k + 1) = survivor(k) * max(0, 1 - events / risk);
      if risk > events && events > 0
        gsum = gsum + events / (risk * (risk - events));
      end
    else
      survivor(k + 1) = survivor(k);
    end
    greenwood(k + 1) = survivor(k + 1) ^ 2 * gsum;
  end
  switch functionName
    case 'survivor'
      f = survivor;
      variance = greenwood;
    case 'cumhazard'
      f = -log(survivor);
      variance = greenwood ./ max(survivor .^ 2, realmin);
    otherwise
      f = 1 - survivor;
      variance = greenwood;
  end
end
%=============================================================================
function [f, x, variance] = computeUncensoredLocal(values, frequency, functionName)
  [values, order] = sort(values);
  frequency = frequency(order);
  if length(values) == 1
    starts = 1;
  else
    starts = [1; find(diff(values) ~= 0) + 1];
  end
  stops = [starts(2:end) - 1; length(values)];
  eventTimes = values(starts);
  counts = zeros(length(eventTimes), 1);
  for k = 1:length(starts)
    counts(k) = sum(frequency(starts(k):stops(k)));
  end
  total = sum(counts);
  cdfValues = [0; cumsum(counts) / total];
  survivor = 1 - cdfValues;
  x = [min(values); eventTimes(:)];
  switch functionName
    case 'survivor'
      f = survivor;
      variance = cdfValues .* (1 - cdfValues) / total;
    case 'cumhazard'
      f = -log(survivor);
      variance = cdfValues ./ max(total * survivor .^ 2, realmin);
    otherwise
      f = cdfValues;
      variance = cdfValues .* (1 - cdfValues) / total;
  end
end
%=============================================================================
function plotEcdfLocal(parent, x, f, flo, fup, opts)
  if isempty(parent)
    if strcmp(opts.Bounds, 'on')
      stairs(x, [flo f fup]);
    else
      stairs(x, f);
    end
  else
    if strcmp(opts.Bounds, 'on')
      stairs(parent, x, [flo f fup]);
    else
      stairs(parent, x, f);
    end
  end
end
%=============================================================================
function [flo, fup] = boundsLocal(f, variance, alpha, functionName)
  z = norminv(1 - alpha / 2, 0, 1);
  delta = z * sqrt(max(0, variance));
  flo = f - delta;
  fup = f + delta;
  switch functionName
    case {'cdf', 'survivor'}
      flo = min(max(flo, 0), 1);
      fup = min(max(fup, 0), 1);
    otherwise
      flo = max(flo, 0);
  end
end
%=============================================================================
