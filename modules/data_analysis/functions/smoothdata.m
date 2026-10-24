%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [B, winsz] = smoothdata(A, varargin)
  narginchk(1, Inf);
  if ~(isnumeric(A) || islogical(A))
    error('Nelson:validators:mustBeNumericOrLogical', '%s must be numeric or logical.', 'First input');
  end
  [method, dim, winsz, nanflag, samplePoints, smoothingFactor, degree] = ...
    parseSmoothdataInputs(A, varargin{:});

  % Sample points: only uniformly spaced points are supported. The window is
  % expressed in sample-point units and rescaled to a number of samples.
  dt = 1;
  hasSamplePoints = ~isempty(samplePoints);
  if hasSamplePoints
    n = size(A, dim);
    if numel(samplePoints) ~= n
      error('Nelson:data_analysis:samplePointsOneElementDataPoint', '''SamplePoints'' must have one element per data point along the operating dimension.');
    end
    dtAll = diff(double(samplePoints(:)));
    if isempty(dtAll)
      dt = 1;
    elseif (max(dtAll) - min(dtAll)) > (eps * max(1, max(abs(double(samplePoints(:))))))
      error('Nelson:data_analysis:uniformlySpacedSamplePointsSupported', 'Only uniformly spaced ''SamplePoints'' are supported.');
    else
      dt = dtAll(1);
    end
    if dt <= 0
      error('Nelson:data_analysis:samplePointsSortedAscendingOrder', '''SamplePoints'' must be sorted in ascending order.');
    end
  end

  autoWindow = isempty(winsz);
  if autoWindow
    if isempty(A) || (size(A, dim) < 2)
      sampleWindow = 1;
    else
      sampleWindow = chooseWindowSize(A, dim, 1 - smoothingFactor);
    end
    winsz = sampleWindow * dt;
  else
    if hasSamplePoints
      sampleWindow = winsz / dt;
    else
      sampleWindow = winsz;
    end
  end

  B = smoothCore(A, method, dim, sampleWindow, nanflag, degree);
end
%=============================================================================
function y = smoothCore(A, method, dim, sampleWindow, nanflag, degree)
  omitnan = strcmp(nanflag, 'omitnan');
  switch method
    case 'movmean'
      y = movmean(A, sampleWindow, dim, nanflag);
    case 'movmedian'
      y = movmedian(A, sampleWindow, dim, nanflag);
    case 'gaussian'
      if isscalar(sampleWindow)
        y = uniformGaussianSmooth(A, sampleWindow, dim, omitnan);
      else
        y = samplePointGaussianSmooth(A, sampleWindow, dim, omitnan);
      end
    case {'lowess', 'loess', 'sgolay'}
      if ~isscalar(sampleWindow)
        error('Nelson:data_analysis:twoElementWindowSupportedMethod', 'A two-element window is not supported for this method.');
      end
      y = uniformLocalRegression(A, sampleWindow, dim, degree, method);
    case {'rlowess', 'rloess'}
      error('Nelson:data_analysis:robustLocalRegressionRlowessRloess', 'Robust local regression (''rlowess''/''rloess'') is not supported yet.');
    otherwise
      error('Nelson:data_analysis:unsupportedSmoothingMethod', 'Unsupported "%s" option.', method);
  end
end
%=============================================================================
function winsz = chooseWindowSize(A, dim, tau)
  % Heuristic window based on the average cutoff frequency below which most
  % of the input energy is contained.
  Ac = A;
  if dim ~= 1
    Ac = permute(A, [dim, 1:(dim - 1), (dim + 1):ndims(A)]);
  end
  if ~isfloat(Ac)
    Ac = double(Ac);
  end
  if any(isnan(Ac(:)))
    Ac = fillmissing(Ac, 'pchip', 1);
  end
  Ac = Ac - mean(Ac);
  N = size(Ac, 1);
  Ac = abs(fft(Ac, 2 * N)) .^ 2 / (2 * N);
  Ac = Ac(1:N, :);
  Ac = Ac ./ sum(Ac);
  Ac = cumsum(Ac, 'omitnan');
  Ac = mean(Ac(:, :), 2);
  rho = find(Ac > tau, 1, 'first');
  if isempty(rho)
    winsz = 1;
  else
    winsz = ceil(sqrt((0.44294 * 2 * N / max(rho - 1, 1)) ^ 2 + 1));
  end
end
%=============================================================================
function y = uniformGaussianSmooth(x, winsz, dim, omitnan)
  n = size(x, dim);
  if winsz < 2
    y = doubleFloat(x);
    return
  end
  sig = winsz / 5;
  winsz = min(fix(winsz), 2 * n);
  h = exp(-((1:winsz) - ceil(winsz / 2)) .^ 2 / (2 * sig ^ 2));
  h = h(:) / sum(h);
  y = doubleFloat(x);
  if dim ~= 1
    pind = [dim, 1:(dim - 1), (dim + 1):ndims(y)];
    y = permute(y, pind);
  end
  evenWindow = rem(length(h), 2) == 0;
  if omitnan && any(isnan(y(:)))
    nanInd = isnan(y);
    y(nanInd) = 0;
  else
    nanInd = false(size(y));
  end
  if evenWindow
    y = flip(y, 1);
    h = flip(h, 1);
    if omitnan
      nanInd = flip(nanInd, 1);
    end
  end
  y = conv2(y, h, 'same');
  halfwinsz = ceil(winsz / 2);
  sz = size(nanInd);
  sz(1) = halfwinsz;
  nanInd = cat(1, true(sz), nanInd, true(sz));
  ync = sum(h) - conv2(double(nanInd), h, 'same');
  ync = ync((halfwinsz + 1):(halfwinsz + size(y, 1)), :);
  ync = reshape(ync, size(y));
  y = y ./ ync;
  if evenWindow
    y = flip(y, 1);
  end
  if dim ~= 1
    y = ipermute(y, pind);
  end
end
%=============================================================================
function y = samplePointGaussianSmooth(x, window, dim, omitnan)
  % Gaussian smoothing for a two-element [back forward] window using the
  % sample points 1:n along the operating dimension.
  y = doubleFloat(x);
  if dim ~= 1
    pind = [dim, 1:(dim - 1), (dim + 1):ndims(y)];
    y = permute(y, pind);
  end
  before = window(1);
  after = window(2);
  windowWidth = before + after;
  if windowWidth <= 0
    if dim ~= 1
      y = ipermute(y, pind);
    end
    return
  end
  sig = windowWidth / 5;
  sz = size(y);
  n = sz(1);
  cols = prod(sz(2:end));
  yr = reshape(y, n, cols);
  out = yr;
  t = (1:n)';
  for i = 1:n
    d = t - t(i);
    inWin = (d >= -before) & (d <= after);
    w = exp(-(d .^ 2) / (2 * sig ^ 2));
    w(~inWin) = 0;
    for c = 1:cols
      wc = w;
      col = yr(:, c);
      if omitnan
        nanMask = isnan(col);
        wc(nanMask) = 0;
        col(nanMask) = 0;
      end
      sw = sum(wc);
      if sw == 0
        out(i, c) = NaN;
      else
        out(i, c) = (wc' * col) / sw;
      end
    end
  end
  y = reshape(out, sz);
  if dim ~= 1
    y = ipermute(y, pind);
  end
end
%=============================================================================
function y = uniformLocalRegression(x, winsz, dim, degree, methodName)
  isWeighted = ~isempty(strfind(methodName, 'ess')); %#ok<STREMP>
  if ~isWeighted
    % sgolay: default degree already set to 2 by the parser.
  elseif strcmp(methodName, 'lowess')
    degree = 1;
  else % loess
    degree = 2;
  end
  n = size(x, dim);
  winsz = min(fix(winsz), n);
  if winsz < 2 || n < 2
    y = doubleFloat(x);
    return
  end
  if dim ~= 1
    y = permute(x, [dim, 1:(dim - 1), (dim + 1):ndims(x)]);
  else
    y = x;
  end
  if ~isfloat(y)
    y = double(y);
  end
  yfront = y(1:winsz, :);
  yback = y(end:-1:(end - winsz + 1), :);
  dmax = floor(winsz / 2);
  if isWeighted
    xlim = dmax - 1;
    xd = (-xlim:xlim)';
    temp = abs(xd) / dmax;
    weight = sqrt(1 - (temp .* temp .* temp));
    weight = weight .* weight .* weight;
    V = weight .* power(xd, 0:degree);
  else
    xlim = floor(winsz / 2);
    xd = (0:(winsz - 1))' - xlim;
    V = power(xd / dmax, 0:degree);
  end
  [Q, ~] = qr(V, 0);
  h = Q * Q(xlim + 1, :)';
  if ~allfinite(y)
    h(h == 0) = eps(0);
  end
  evenWindowLength = rem(numel(h), 2) == 0;
  if isWeighted
    h = h .* weight;
  elseif evenWindowLength
    y = flip(y, 1);
  end
  sz = size(y);
  if ~isWeighted || (winsz ~= size(x, dim))
    if isvector(y)
      y = conv(y, h, 'same');
      y = y(:);
    else
      y = conv2(y, h, 'same');
    end
  end
  if evenWindowLength
    y = flip(y, 1);
  end
  fw = floor(winsz / 2);
  if isWeighted
    xd = (-fw:(winsz - 1))';
    lb = fw + 1;
    ub = length(xd);
    Vbase = power(xd, 0:degree);
    for j = 1:ceil(winsz / 2)
      dm = max(xd(lb), xd(ub));
      temp = abs(xd(lb:ub)) / dm;
      weight = sqrt((1 - (temp .* temp .* temp)));
      weight = weight .* weight .* weight;
      V = weight .* Vbase(lb:ub, :);
      weightMask = weight ~= 0;
      [Q, ~] = qr(V, 0);
      alpha = (Q(j, :) * Q') .* weight';
      y(j, :) = alpha(weightMask') * yfront(weightMask, :);
      y(end - j + 1, :) = alpha(weightMask') * yback(weightMask, :);
      lb = lb - 1;
      ub = ub - 1;
    end
  else
    y(1:fw, :) = Q(1:fw, :) * (Q' * yfront);
    y(end:-1:(end - fw + 1), :) = Q(1:fw, :) * (Q' * yback);
  end
  y = reshape(y, sz);
  if dim ~= 1
    y = ipermute(y, [dim, 1:(dim - 1), (dim + 1):ndims(y)]);
  end
end
%=============================================================================
function y = doubleFloat(x)
  if isfloat(x)
    y = x;
  else
    y = double(x);
  end
end
%=============================================================================
function [method, dim, winsz, nanflag, samplePoints, smoothingFactor, degree] = parseSmoothdataInputs(A, varargin)
  method = 'movmean';
  dim = firstNonSingletonDim(A);
  winsz = [];
  nanflag = 'omitnan';
  samplePoints = [];
  smoothingFactor = 0.25;
  degree = 2;
  userDegree = false;

  methodNames = {'movmean', 'movmedian', 'gaussian', 'lowess', 'loess', ...
    'rlowess', 'rloess', 'sgolay'};

  args = varargin;
  k = 1;
  nargs = numel(args);

  % Optional dimension argument.
  if k <= nargs && ~isCharLike(args{k})
    dim = args{k};
    if ~(isnumeric(dim) || islogical(dim)) || ~isscalar(dim) || ~isreal(dim) || ...
        (dim < 1) || (dim ~= round(dim))
      error('Nelson:data_analysis:dimensionArgumentPositiveIntegerScalar', '%s must be a positive integer scalar.', 'Dimension argument');
    end
    dim = double(dim);
    k = k + 1;
  end

  % Optional method argument.
  if k <= nargs && isCharLike(args{k})
    m = partialMatchOne(methodNames, args{k});
    if m < 0
      error('Nelson:data_analysis:ambiguousSmoothingMethod', 'Ambiguous smoothing method.');
    elseif m > 0
      method = methodNames{m};
      k = k + 1;
      % Optional window argument.
      if k <= nargs && (isnumeric(args{k}) || islogical(args{k}))
        winsz = checkWindowSize(args{k});
        k = k + 1;
      end
    end
  end

  % Optional nan flag.
  if k <= nargs && isCharLike(args{k})
    nf = partialMatchOne({'omitnan', 'includenan', 'omitmissing', 'includemissing'}, args{k});
    if nf > 0
      if nf == 2 || nf == 4
        nanflag = 'includenan';
      else
        nanflag = 'omitnan';
      end
      k = k + 1;
    end
  end

  % Name-value pairs.
  paramNames = {'SamplePoints', 'SmoothingFactor', 'Degree'};
  while k <= nargs
    name = args{k};
    if ~isCharLike(name)
      error('Nelson:data_analysis:expectedParameterName', 'Expected a parameter name.');
    end
    p = partialMatchOne(paramNames, name);
    if p <= 0
      error('Nelson:data_analysis:unknownParameterNameOption', 'Unknown parameter name or option.');
    end
    if k + 1 > nargs
      error('Nelson:data_analysis:valueExpectedAfterParameterName', 'Value expected after parameter name.');
    end
    value = args{k + 1};
    switch p
      case 1
        if ~isnumeric(value) || ~isvector(value)
          error('Nelson:data_analysis:samplePointsNumericVector', '''SamplePoints'' must be a numeric vector.');
        end
        samplePoints = value;
      case 2
        if ~isnumeric(value) || ~isscalar(value) || ~isreal(value) || value < 0 || value > 1
          error('Nelson:data_analysis:smoothingFactorScalarBetween01', '''SmoothingFactor'' must be a scalar between 0 and 1.');
        end
        smoothingFactor = double(value);
      case 3
        if ~isnumeric(value) || ~isscalar(value) || ~isreal(value) || value < 0 || value ~= round(value)
          error('Nelson:data_analysis:degreeNonnegativeIntegerScalar', '''Degree'' must be a nonnegative integer scalar.');
        end
        degree = double(value);
        userDegree = true;
    end
    k = k + 2;
  end

  if userDegree && ~strcmp(method, 'sgolay')
    error('Nelson:data_analysis:degreeSupportedSgolayMethod', '''Degree'' is only supported for the ''sgolay'' method.');
  end
end
%=============================================================================
function winsz = checkWindowSize(winsz)
  if islogical(winsz)
    winsz = double(winsz);
  end
  if ~isnumeric(winsz) || ~isreal(winsz) || isempty(winsz) || numel(winsz) > 2
    error('Nelson:data_analysis:windowPositiveScalarTwoElement', 'Window must be a positive scalar or a two-element vector.');
  end
  winsz = double(winsz(:))';
  if any(winsz < 0)
    error('Nelson:validators:mustBeNonnegative', '%s must be nonnegative.', 'Window values');
  end
  if isscalar(winsz) && winsz == 0
    error('Nelson:data_analysis:windowLengthPositiveScalar', '%s must be a positive scalar.', 'Window length');
  end
end
%=============================================================================
function idx = partialMatchOne(names, value)
  % Returns the index of the unique (case-insensitive) prefix match,
  % 0 when there is no match, -1 when the match is ambiguous.
  if isstring(value) && isscalar(value)
    value = char(value);
  end
  if ~ischar(value) || isempty(value)
    idx = 0;
    return
  end
  value = lower(value);
  hits = [];
  for i = 1:numel(names)
    nm = lower(names{i});
    if strcmp(nm, value)
      idx = i;
      return
    end
    if length(value) <= length(nm) && strcmp(nm(1:length(value)), value)
      hits(end + 1) = i; %#ok<AGROW>
    end
  end
  if isempty(hits)
    idx = 0;
  elseif isscalar(hits)
    idx = hits;
  else
    idx = -1;
  end
end
%=============================================================================
function tf = isCharLike(x)
  tf = (ischar(x) && (isrow(x) || isempty(x))) || (isstring(x) && isscalar(x));
end
%=============================================================================
function dim = firstNonSingletonDim(A)
  sz = size(A);
  dim = 1;
  for k = 1:length(sz)
    if sz(k) ~= 1
      dim = k;
      return
    end
  end
end
%=============================================================================
