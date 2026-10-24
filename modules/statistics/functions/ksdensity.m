%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [f, xi, bw] = ksdensity(x, pts, varargin)
  narginchk(1, Inf);
  nargoutchk(0, 3);
  if nargin >= 2 && isOptionNameLocal(pts)
    varargin = [{pts}, varargin];
    pts = [];
  elseif nargin < 2
    pts = [];
  end
  opts = parseOptionsLocal(varargin);
  [x, weights] = prepareDataLocal(x, opts);
  bw = bandwidthLocal(x, weights, opts.Bandwidth);
  xi = pointsLocal(x, pts, bw, opts);
  f = estimateLocal(x, weights, xi, bw, opts);
  if nargout == 0
    plot(xi, f);
  end
end
%=============================================================================
function tf = isOptionNameLocal(value)
  tf = (ischar(value) || (isstring(value) && isscalar(value))) && any(strcmpi(char(value), ...
    {'Bandwidth', 'Width', 'Function', 'NumPoints', 'Support', 'Weights', 'Frequency', ...
    'Censoring', 'Kernel', 'BoundaryCorrection'}));
end
%=============================================================================
function opts = parseOptionsLocal(args)
  opts = struct('Bandwidth', [], 'Function', 'pdf', 'NumPoints', 100, ...
    'Support', 'unbounded', 'Weights', [], 'Censoring', [], 'Kernel', 'normal');
  k = 1;
  while k <= length(args)
    if k == length(args)
      error('Nelson:statistics:nameValueArgumentFollowedValue', 'Name-value argument must be followed by a value.');
    end
    name = lower(char(args{k}));
    switch name
      case {'bandwidth', 'width'}
        opts.Bandwidth = args{k + 1};
      case 'function'
        opts.Function = normalizeFunctionLocal(args{k + 1});
      case 'numpoints'
        opts.NumPoints = args{k + 1};
      case 'support'
        opts.Support = args{k + 1};
      case {'weights', 'frequency'}
        opts.Weights = args{k + 1};
      case 'censoring'
        opts.Censoring = args{k + 1};
      case 'kernel'
        opts.Kernel = lower(char(args{k + 1}));
      case 'boundarycorrection'
        args{k + 1};
      otherwise
        error('Nelson:statistics:unknownOption', 'Unknown option.');
    end
    k = k + 2;
  end
  if ~(isnumeric(opts.NumPoints) && isscalar(opts.NumPoints) && opts.NumPoints >= 1)
    error('Nelson:statistics:numPointsPositiveScalar', '%s must be a positive scalar.', 'NumPoints');
  end
  if ~strcmp(opts.Kernel, 'normal')
    error('Nelson:statistics:normalKernelSupported', 'Only normal kernel is supported.');
  end
end
%=============================================================================
function name = normalizeFunctionLocal(value)
  name = strtrim(lower(char(value)));
  if any(strcmp(name, {'pdf', 'density'}))
    name = 'pdf';
  elseif strcmp(name, 'cdf')
    name = 'cdf';
  elseif any(strcmp(name, {'survivor', 'survival'}))
    name = 'survivor';
  elseif any(strcmp(name, {'cumhazard', 'cumulative hazard', 'hazard'}))
    name = 'cumhazard';
  elseif any(strcmp(name, {'icdf', 'inverse cdf'}))
    name = 'icdf';
  else
    error('Nelson:statistics:unsupportedFunctionType', 'Unsupported %s.', 'function type');
  end
end
%=============================================================================
function [x, weights] = prepareDataLocal(x, opts)
  if ~(isnumeric(x) && isreal(x) && ~issparse(x) && isvector(x))
    error('Nelson:statistics:xRealNumericVector', '%s must be a real numeric vector.', 'X');
  end
  x = double(x(:));
  weights = ones(size(x));
  if ~isempty(opts.Weights)
    weights = double(opts.Weights(:));
  end
  if length(weights) ~= length(x)
    error('Nelson:statistics:weightsMatchX', '%s must match X.', 'Weights');
  end
  keep = ~isnan(x) & ~isnan(weights) & weights > 0;
  if ~isempty(opts.Censoring)
    censoring = opts.Censoring(:);
    if length(censoring) ~= length(x)
      error('Nelson:statistics:censoringMatchX', '%s must match X.', 'Censoring');
    end
    keep = keep & ~logical(censoring);
  end
  x = x(keep);
  weights = weights(keep);
  if isempty(x)
    error('Nelson:statistics:xContainLeastOneValid', 'X must contain at least one valid observation.');
  end
end
%=============================================================================
function bw = bandwidthLocal(x, weights, value)
  if isempty(value) || (ischar(value) && strcmpi(value, 'normal-approx')) ...
      || (isstring(value) && strcmpi(char(value), 'normal-approx'))
    n = sum(weights);
    sx = std(x, 0, 1);
    iq = iqr(x);
    scale = min(sx, iq / 1.349);
    if scale <= 0 || isnan(scale)
      scale = sx;
    end
    if scale <= 0 || isnan(scale)
      scale = max(x) - min(x);
    end
    if scale <= 0 || isnan(scale)
      scale = 1;
    end
    bw = 1.06 * scale * n ^ (-1 / 5);
  elseif ischar(value) || isstring(value)
    error('Nelson:statistics:unsupportedBandwidthMethod', 'Unsupported %s.', 'bandwidth method');
  else
    bw = double(value);
  end
  if ~(isscalar(bw) && isfinite(bw) && bw > 0)
    error('Nelson:statistics:bandwidthPositiveScalar', '%s must be a positive scalar.', 'Bandwidth');
  end
end
%=============================================================================
function xi = pointsLocal(x, pts, bw, opts)
  if isempty(pts)
    support = supportBoundsLocal(opts.Support);
    lo = max(min(x) - 3 * bw, support(1));
    hi = min(max(x) + 3 * bw, support(2));
    if ~isfinite(lo)
      lo = min(x) - 3 * bw;
    end
    if ~isfinite(hi)
      hi = max(x) + 3 * bw;
    end
    if lo == hi
      lo = lo - bw;
      hi = hi + bw;
    end
    xi = linspace(lo, hi, opts.NumPoints)';
  else
    if ~(isnumeric(pts) && isreal(pts) && isvector(pts))
      error('Nelson:statistics:pointsRealNumericVector', '%s must be a real numeric vector.', 'Points');
    end
    xi = double(pts(:));
  end
end
%=============================================================================
function bounds = supportBoundsLocal(support)
  if ischar(support) || isstring(support)
    switch lower(char(support))
      case 'unbounded'
        bounds = [-Inf, Inf];
      case {'positive', 'nonnegative'}
        bounds = [0, Inf];
      case 'negative'
        bounds = [-Inf, 0];
      otherwise
        error('Nelson:statistics:unsupportedSupport', 'Unsupported %s.', 'support');
    end
  else
    bounds = double(support(:)');
    if length(bounds) ~= 2 || bounds(1) >= bounds(2)
      error('Nelson:statistics:supportTwoElementIncreasingVector', 'Support must be a two-element increasing vector.');
    end
  end
end
%=============================================================================
function f = estimateLocal(x, weights, xi, bw, opts)
  totalWeight = sum(weights);
  f = zeros(size(xi));
  switch opts.Function
    case 'icdf'
      f = inverseCdfLocal(x, weights, xi, bw, opts);
      return
  end
  support = supportBoundsLocal(opts.Support);
  for k = 1:length(xi)
    z = (xi(k) - x) / bw;
    switch opts.Function
      case 'pdf'
        value = sum(weights .* normalPdfLocal(z)) / (totalWeight * bw);
      otherwise
        value = sum(weights .* normcdf(z, 0, 1)) / totalWeight;
    end
    f(k) = value;
  end
  f = applyFunctionAndSupportLocal(f, xi, support, opts.Function);
end
%=============================================================================
function f = applyFunctionAndSupportLocal(f, xi, support, functionName)
  below = xi < support(1);
  above = xi > support(2);
  switch functionName
    case 'pdf'
      f(below | above) = 0;
    case 'cdf'
      f(below) = 0;
      f(above) = 1;
    case 'survivor'
      f = 1 - f;
      f(below) = 1;
      f(above) = 0;
    case 'cumhazard'
      f = -log(max(1 - f, realmin));
      f(below) = 0;
      f(above) = Inf;
  end
end
%=============================================================================
function q = inverseCdfLocal(x, weights, p, bw, opts)
  if any(p < 0 | p > 1 | isnan(p))
    error('Nelson:statistics:probabilitiesBetween01', 'Probabilities must be between 0 and 1.');
  end
  oldFunction = opts.Function;
  opts.Function = 'cdf';
  grid = pointsLocal(x, [], bw, opts);
  cdfValues = estimateLocal(x, weights, grid, bw, opts);
  opts.Function = oldFunction;
  q = interp1(cdfValues, grid, p, 'linear');
end
%=============================================================================
function y = normalPdfLocal(x)
  y = exp(-0.5 * x .^ 2) / sqrt(2 * pi);
end
%=============================================================================
