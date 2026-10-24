%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
%#ok<NLS0004,NLS0008,NLS0014,NLS0015,NLS0024,NLS0026,NLS0036>
function varargout = histfit(varargin)
  narginchk(1, 4);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [parent, args] = parseParentLocal(args);
  [data, nbins, dist] = parseInputsLocal(args);
  [hHist, ax] = drawHistogramLocal(parent, data, nbins);
  [xFit, yFit, fitInfo] = fittedCurveLocal(data, hHist, dist);
  hLine = line(ax, xFit, yFit, 'Color', [1 0 0], 'LineWidth', 1.5);
  hLine.UserData = fitInfo;
  h = [hHist; hLine];
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
function [data, nbins, dist] = parseInputsLocal(args)
  if isempty(args)
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end
  if length(args) > 3
    error('Nelson:tooManyInputs', 'Too many input arguments.');
  end
  data = validateDataLocal(args{1});
  nbins = ceil(sqrt(length(data)));
  dist = 'normal';
  if length(args) >= 2 && ~isempty(args{2})
    nbins = validateBinsLocal(args{2});
  end
  if length(args) >= 3
    dist = normalizeDistributionLocal(args{3});
  end
end
%=============================================================================
function data = validateDataLocal(data)
  if ~(isnumeric(data) && isreal(data) && ~issparse(data) && isvector(data))
    error('Nelson:statistics:dataRealNumericVector', '%s must be a real numeric vector.', 'Data');
  end
  data = double(data(:));
  data = data(isfinite(data));
  if isempty(data)
    error('Nelson:statistics:dataContainLeastOneFinite', '%s must contain at least one finite value.', 'Data');
  end
end
%=============================================================================
function nbins = validateBinsLocal(nbins)
  if ~(isnumeric(nbins) && isreal(nbins) && isscalar(nbins) && isfinite(nbins) ...
    && nbins > 0 && floor(nbins) == nbins)
    error('Nelson:statistics:numberBinsPositiveIntegerScalar', '%s must be a positive integer scalar.', 'Number of bins');
  end
  nbins = double(nbins);
end
%=============================================================================
function dist = normalizeDistributionLocal(value)
  if ~(ischar(value) || (isstring(value) && isscalar(value)))
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Distribution');
  end
  text = lower(strtrim(char(value)));
  text = strrep(text, '-', ' ');
  text = strrep(text, '_', ' ');
  while contains(text, '  ')
    text = strrep(text, '  ', ' ');
  end
  if any(strcmp(text, {'normal', 'norm'}))
    dist = 'normal';
  elseif any(strcmp(text, {'kernel', 'ks'}))
    dist = 'kernel';
  elseif any(strcmp(text, {'exponential', 'exp'}))
    dist = 'exponential';
  elseif strcmp(text, 'gamma')
    dist = 'gamma';
  elseif strcmp(text, 'beta')
    dist = 'beta';
  elseif any(strcmp(text, {'extreme value', 'ev'}))
    dist = 'extreme value';
  elseif any(strcmp(text, {'half normal', 'halfnormal', 'hn'}))
    dist = 'half normal';
  elseif any(strcmp(text, {'lognormal', 'log normal'}))
    dist = 'lognormal';
  elseif strcmp(text, 'logistic')
    dist = 'logistic';
  elseif any(strcmp(text, {'loglogistic', 'log logistic'}))
    dist = 'loglogistic';
  elseif strcmp(text, 'rayleigh')
    dist = 'rayleigh';
  elseif any(strcmp(text, {'weibull', 'wbl'}))
    dist = 'weibull';
  else
    error('Nelson:statistics:unsupportedDistribution', 'Unsupported %s.', 'distribution');
  end
end
%=============================================================================
function [hHist, ax] = drawHistogramLocal(parent, data, nbins)
  if isempty(parent)
    hHist = histogram(data, nbins);
  else
    hHist = histogram(parent, data, nbins);
  end
  ax = ancestor(hHist, 'axes');
end
%=============================================================================
function [xFit, yFit, fitInfo] = fittedCurveLocal(data, hHist, dist)
  edges = hHist.BinEdges;
  lo = min(data);
  hi = max(data);
  if lo == hi
    lo = lo - 0.5;
    hi = hi + 0.5;
  end
  xFit = linspace(lo, hi, 100)';
  [pdfValues, fitInfo] = densityLocal(data, xFit, dist);
  binWidth = (hi - lo) / hHist.NumBins;
  if binWidth == 0
    binWidth = mean(diff(edges));
  end
  scale = length(data) * binWidth;
  yFit = pdfValues(:) * scale;
end
%=============================================================================
function [pdfValues, fitInfo] = densityLocal(data, x, dist)
  switch dist
    case 'normal'
      [pdfValues, params] = normalDensityLocal(data, x);
    case 'kernel'
      [pdfValues, params] = kernelDensityLocal(data, x);
    case 'exponential'
      [pdfValues, params] = exponentialDensityLocal(data, x);
    case 'gamma'
      [pdfValues, params] = gammaDensityLocal(data, x);
    case 'beta'
      [pdfValues, params] = betaDensityLocal(data, x);
    case 'extreme value'
      [pdfValues, params] = extremeValueDensityLocal(data, x);
    case 'half normal'
      [pdfValues, params] = halfNormalDensityLocal(data, x);
    case 'lognormal'
      [pdfValues, params] = lognormalDensityLocal(data, x);
    case 'logistic'
      [pdfValues, params] = logisticDensityLocal(data, x);
    case 'loglogistic'
      [pdfValues, params] = loglogisticDensityLocal(data, x);
    case 'rayleigh'
      [pdfValues, params] = rayleighDensityLocal(data, x);
    case 'weibull'
      [pdfValues, params] = weibullDensityLocal(data, x);
    otherwise
      error('Nelson:statistics:unsupportedDistribution', 'Unsupported %s.', 'distribution');
  end
  fitInfo = struct('Distribution', dist, 'Parameters', params);
end
%=============================================================================
function [pdfValues, params] = normalDensityLocal(data, x)
  mu = mean(data);
  sigma = sqrt(mean((data - mu) .^ 2));
  sigma = positiveScaleLocal(sigma);
  pdfValues = normalPdfLocal(x, mu, sigma);
  params = [mu sigma];
end
%=============================================================================
function [pdfValues, params] = kernelDensityLocal(data, x)
  [pdfValues, ~, bw] = ksdensity(data, x, 'Function', 'pdf');
  params = bw;
end
%=============================================================================
function [pdfValues, params] = exponentialDensityLocal(data, x)
  data = requirePositiveLocal(data);
  mu = positiveScaleLocal(mean(data));
  pdfValues = zeros(size(x));
  positive = x >= 0;
  pdfValues(positive) = exp(-x(positive) / mu) / mu;
  params = mu;
end
%=============================================================================
function [pdfValues, params] = gammaDensityLocal(data, x)
  data = requirePositiveLocal(data);
  m = mean(data);
  v = mean((data - m) .^ 2);
  shape = positiveScaleLocal(m ^ 2 / v);
  scale = positiveScaleLocal(v / m);
  pdfValues = zeros(size(x));
  positive = x > 0;
  pdfValues(positive) = exp((shape - 1) * log(x(positive)) - x(positive) / scale ...
    - gammaln(shape) - shape * log(scale));
  params = [shape scale];
end
%=============================================================================
function [pdfValues, params] = betaDensityLocal(data, x)
  if any(data <= 0 | data >= 1)
    error('Nelson:statistics:dataInterval01Distribution', 'Data must be in the interval (0,1) for this distribution.');
  end
  m = mean(data);
  v = mean((data - m) .^ 2);
  common = max(m * (1 - m) / v - 1, eps);
  a = positiveScaleLocal(m * common);
  b = positiveScaleLocal((1 - m) * common);
  pdfValues = zeros(size(x));
  inside = x > 0 & x < 1;
  pdfValues(inside) = exp((a - 1) * log(x(inside)) + (b - 1) * log(1 - x(inside)) ...
    + gammaln(a + b) - gammaln(a) - gammaln(b));
  params = [a b];
end
%=============================================================================
function [pdfValues, params] = extremeValueDensityLocal(data, x)
  eulerGamma = 0.577215664901533;
  sigma = positiveScaleLocal(sqrt(6 * mean((data - mean(data)) .^ 2)) / pi);
  mu = mean(data) + eulerGamma * sigma;
  z = (x - mu) / sigma;
  pdfValues = exp(z - exp(z)) / sigma;
  params = [mu sigma];
end
%=============================================================================
function [pdfValues, params] = halfNormalDensityLocal(data, x)
  data = abs(data);
  sigma = positiveScaleLocal(sqrt(mean(data .^ 2)));
  pdfValues = zeros(size(x));
  positive = x >= 0;
  pdfValues(positive) = sqrt(2) / (sigma * sqrt(pi)) * exp(-0.5 * (x(positive) / sigma) .^ 2);
  params = sigma;
end
%=============================================================================
function [pdfValues, params] = lognormalDensityLocal(data, x)
  data = requirePositiveLocal(data);
  logData = log(data);
  [basePdf, params] = normalDensityLocal(logData, log(max(x, realmin)));
  pdfValues = zeros(size(x));
  positive = x > 0;
  pdfValues(positive) = basePdf(positive) ./ x(positive);
end
%=============================================================================
function [pdfValues, params] = logisticDensityLocal(data, x)
  mu = mean(data);
  scale = positiveScaleLocal(sqrt(mean((data - mu) .^ 2)) * sqrt(3) / pi);
  z = exp(-(x - mu) / scale);
  pdfValues = z ./ (scale * (1 + z) .^ 2);
  params = [mu scale];
end
%=============================================================================
function [pdfValues, params] = loglogisticDensityLocal(data, x)
  data = requirePositiveLocal(data);
  [~, params] = logisticDensityLocal(log(data), log(max(x, realmin)));
  mu = params(1);
  scale = params(2);
  pdfValues = zeros(size(x));
  positive = x > 0;
  z = exp(-(log(x(positive)) - mu) / scale);
  pdfValues(positive) = z ./ (scale * x(positive) .* (1 + z) .^ 2);
end
%=============================================================================
function [pdfValues, params] = rayleighDensityLocal(data, x)
  data = requirePositiveLocal(data);
  sigma = positiveScaleLocal(sqrt(mean(data .^ 2) / 2));
  pdfValues = zeros(size(x));
  positive = x >= 0;
  pdfValues(positive) = x(positive) / sigma ^ 2 .* exp(-0.5 * (x(positive) / sigma) .^ 2);
  params = sigma;
end
%=============================================================================
function [pdfValues, params] = weibullDensityLocal(data, x)
  data = requirePositiveLocal(data);
  p = ((1:length(data))' - 0.5) / length(data);
  sx = sort(data);
  coeff = [ones(length(sx), 1), log(-log(1 - p))] \ log(sx);
  shape = positiveScaleLocal(1 / coeff(2));
  scale = positiveScaleLocal(exp(coeff(1)));
  pdfValues = zeros(size(x));
  positive = x > 0;
  z = x(positive) / scale;
  pdfValues(positive) = (shape / scale) * z .^ (shape - 1) .* exp(-z .^ shape);
  params = [scale shape];
end
%=============================================================================
function data = requirePositiveLocal(data)
  if any(data <= 0)
    error('Nelson:statistics:dataPositiveDistribution', 'Data must be positive for this distribution.');
  end
end
%=============================================================================
function value = positiveScaleLocal(value)
  if ~(isfinite(value) && value > 0)
    value = eps;
  end
end
%=============================================================================
function y = normalPdfLocal(x, mu, sigma)
  y = exp(-0.5 * ((x - mu) / sigma) .^ 2) / (sigma * sqrt(2 * pi));
end
%=============================================================================
