%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = probplot(varargin)
  narginchk(1, 5);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [parent, args] = parseParentLocal(args);
  [dist, y, cens, freq, showRef] = parseInputsLocal(args);
  data = prepareDataLocal(y, cens, freq, dist);
  curves = computeCurvesLocal(data, dist, showRef);
  h = drawCurvesLocal(parent, curves, dist, showRef);
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function [parent, args] = parseParentLocal(args)
  parent = [];
  if ~isempty(args) && isscalar(args{1}) && (isgraphics(args{1}, 'axes') || isgraphics(args{1}, 'hggroup'))
    parent = args{1};
    args = args(2:end);
  end
end
%=============================================================================
function [dist, y, cens, freq, showRef] = parseInputsLocal(args)
  if isempty(args)
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end
  showRef = true;
  if ischar(args{end}) || isStringScalarLocal(args{end})
    text = lower(char(args{end}));
    if strcmp(text, 'noref')
      showRef = false;
      args(end) = [];
    end
  end
  dist = 'normal';
  if ~isempty(args) && (ischar(args{1}) || isStringScalarLocal(args{1}))
    dist = normalizeDistributionLocal(args{1});
    args = args(2:end);
  end
  if isempty(args)
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end
  if length(args) > 3
    error('Nelson:tooManyInputs', 'Too many input arguments.');
  end
  y = validateDataLocal(args{1});
  cens = [];
  freq = [];
  if length(args) >= 2
    cens = validateSideVectorLocal(args{2}, y, 'Censoring');
  end
  if length(args) >= 3
    freq = validateSideVectorLocal(args{3}, y, 'Frequency');
  end
end
%=============================================================================
function tf = isStringScalarLocal(value)
  tf = isstring(value) && isscalar(value);
end
%=============================================================================
function dist = normalizeDistributionLocal(value)
  text = strtrim(lower(char(value)));
  text = strrep(text, '-', ' ');
  while contains(text, '  ')
    text = strrep(text, '  ', ' ');
  end
  if any(strcmp(text, {'normal', 'norm'}))
    dist = 'normal';
  elseif any(strcmp(text, {'exponential', 'exp'}))
    dist = 'exponential';
  elseif any(strcmp(text, {'extreme value', 'ev'}))
    dist = 'extreme value';
  elseif any(strcmp(text, {'half normal', 'halfnormal'}))
    dist = 'half normal';
  elseif any(strcmp(text, {'lognormal', 'log normal'}))
    dist = 'lognormal';
  elseif strcmp(text, 'logistic')
    dist = 'logistic';
  elseif any(strcmp(text, {'loglogistic', 'log logistic'}))
    dist = 'loglogistic';
  elseif strcmp(text, 'rayleigh')
    dist = 'rayleigh';
  elseif strcmp(text, 'weibull')
    dist = 'weibull';
  else
    error('Nelson:statistics:unsupportedDistribution', 'Unsupported %s.', 'distribution');
  end
end
%=============================================================================
function y = validateDataLocal(y)
  if ~(isnumeric(y) && isreal(y) && ~issparse(y) && ~isempty(y))
    error('Nelson:statistics:yRealNumericArray', '%s must be a real numeric array.', 'Y');
  end
  if ndims(y) > 2
    error('Nelson:statistics:yVectorMatrix', '%s must be a vector or matrix.', 'Y');
  end
  y = double(y);
  if isvector(y)
    y = y(:);
  end
end
%=============================================================================
function value = validateSideVectorLocal(value, y, name)
  if isempty(value)
    return
  end
  if ~(isnumeric(value) || islogical(value)) || ~isreal(value)
    error('Nelson:statistics:mustBeRealNumeric', '%s must be a real numeric array.', name);
  end
  value = double(value);
  if ~isequal(size(value), size(y))
    error('Nelson:statistics:mustMatchY', '%s must match Y.', name);
  end
end
%=============================================================================
function data = prepareDataLocal(y, cens, freq, dist)
  data = cell(size(y, 2), 1);
  for k = 1:size(y, 2)
    column = y(:, k);
    keep = isfinite(column);
    if ~isempty(cens)
      ck = cens(:, k);
      keep = keep & isfinite(ck) & (ck == 0);
    end
    column = column(keep);
    if ~isempty(freq)
      fk = freq(:, k);
      fk = fk(keep);
      column = expandByFrequencyLocal(column, fk);
    end
    column = validateRangeLocal(column, dist);
    if isempty(column)
      error('Nelson:statistics:yContainLeastOneValid', 'Y must contain at least one valid observation per column.');
    end
    data{k} = sort(column(:));
  end
end
%=============================================================================
function column = expandByFrequencyLocal(column, freq)
  if any(~isfinite(freq)) || any(freq < 0) || any(floor(freq) ~= freq)
    error('Nelson:statistics:frequencyContainNonnegativeIntegerValues', '%s must contain nonnegative integer values.', 'Frequency');
  end
  expanded = zeros(sum(freq), 1);
  at = 1;
  for k = 1:length(column)
    count = freq(k);
    if count > 0
      expanded(at:(at + count - 1)) = column(k);
      at = at + count;
    end
  end
  column = expanded;
end
%=============================================================================
function column = validateRangeLocal(column, dist)
  if any(strcmp(dist, {'exponential', 'lognormal', 'loglogistic', 'rayleigh', 'weibull'}))
    column = column(column > 0);
  elseif strcmp(dist, 'half normal')
    column = abs(column);
  end
end
%=============================================================================
function curves = computeCurvesLocal(data, dist, showRef)
  curves = cell(numel(data), 1);
  for k = 1:numel(data)
    values = data{k};
    n = length(values);
    p = ((1:n)' - 0.5) / n;
    refX = [];
    refY = [];
    if showRef
      [refX, refY] = referenceLineLocal(values, dist);
    end
    curves{k} = struct('XData', values(:), 'YData', p(:), ...
      'ReferenceX', refX(:), 'ReferenceY', refY(:), 'Distribution', dist);
  end
end
%=============================================================================
function [refX, refY] = referenceLineLocal(values, dist)
  p = [0.25; 0.75];
  if strcmp(dist, 'half normal')
    p = [0; 0.5];
  end
  xQuartiles = quantile(values, p);
  zQuartiles = distributionInvLocal(p, dist);
  dz = zQuartiles(2) - zQuartiles(1);
  if dz == 0 || isnan(dz)
    scale = 1;
  else
    scale = (xQuartiles(2) - xQuartiles(1)) / dz;
  end
  location = xQuartiles(1) - scale * zQuartiles(1);
  refX = linspace(min(values), max(values), 100)';
  refY = distributionCdfLocal((refX - location) / scale, dist);
  refY = min(max(refY, 0), 1);
end
%=============================================================================
function p = distributionCdfLocal(x, dist)
  switch dist
    case 'normal'
      p = normcdf(x, 0, 1);
    case 'exponential'
      p = 1 - exp(-max(x, 0));
    case 'extreme value'
      p = 1 - exp(-exp(x));
    case 'half normal'
      p = 2 * normcdf(max(x, 0), 0, 1) - 1;
    case 'lognormal'
      p = zeros(size(x));
      positive = x > 0;
      p(positive) = normcdf(log(x(positive)), 0, 1);
    case 'logistic'
      p = 1 ./ (1 + exp(-x));
    case 'loglogistic'
      p = zeros(size(x));
      positive = x > 0;
      p(positive) = x(positive) ./ (1 + x(positive));
    case 'rayleigh'
      p = 1 - exp(-0.5 * max(x, 0) .^ 2);
    case 'weibull'
      p = 1 - exp(-max(x, 0));
  end
end
%=============================================================================
function x = distributionInvLocal(p, dist)
  p = min(max(p, realmin), 1 - eps);
  switch dist
    case 'normal'
      x = norminv(p, 0, 1);
    case 'exponential'
      x = -log(1 - p);
    case 'extreme value'
      x = log(-log(1 - p));
    case 'half normal'
      x = norminv((p + 1) / 2, 0, 1);
    case 'lognormal'
      x = exp(norminv(p, 0, 1));
    case 'logistic'
      x = log(p ./ (1 - p));
    case 'loglogistic'
      x = p ./ (1 - p);
    case 'rayleigh'
      x = sqrt(-2 * log(1 - p));
    case 'weibull'
      x = -log(1 - p);
  end
end
%=============================================================================
function h = drawCurvesLocal(parent, curves, dist, showRef)
  if isempty(parent)
    parent = newplot();
  elseif isgraphics(parent, 'axes')
    parent = newplot(parent);
  end
  h = [];
  for k = 1:numel(curves)
    c = curves{k};
    h = [h; line(parent, c.XData, c.YData, 'Marker', '+', 'LineStyle', 'none')];
    if showRef
      h = [h; line(parent, c.ReferenceX, c.ReferenceY, 'LineStyle', '--', 'LineWidth', 1)];
    end
  end
  for k = 1:length(h)
    h(k).UserData = curves{ceil(k / (1 + double(showRef)))};
  end
  labelAxesLocal(parent, dist);
end
%=============================================================================
function labelAxesLocal(parent, dist)
  ax = parent;
  if isgraphics(parent, 'hggroup')
    ax = ancestor(parent, 'axes');
  end
  xlabel(ax, 'Data');
  ylabel(ax, 'Probability');
  title(ax, ['Probability Plot for ', displayNameLocal(dist), ' Distribution']);
  if any(strcmp(dist, {'lognormal', 'loglogistic', 'weibull'}))
    ax.XScale = 'log';
  end
  ax.YLim = [0 1];
end
%=============================================================================
function name = displayNameLocal(dist)
  switch dist
    case 'normal'
      name = 'Normal';
    case 'exponential'
      name = 'Exponential';
    case 'extreme value'
      name = 'Extreme Value';
    case 'half normal'
      name = 'Half Normal';
    case 'lognormal'
      name = 'Lognormal';
    case 'logistic'
      name = 'Logistic';
    case 'loglogistic'
      name = 'Loglogistic';
    case 'rayleigh'
      name = 'Rayleigh';
    case 'weibull'
      name = 'Weibull';
  end
end
%=============================================================================
