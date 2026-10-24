%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = xcorr(varargin)
  narginchk(1, 4);
  nargoutchk(0, 2);

  xInput = varargin{1};
  yInput = [];
  maxlag = [];
  scaleOption = 'none';
  argumentIndex = 2;

  if argumentIndex <= nargin
    value = varargin{argumentIndex};
    if ischar(value) || isstring(value)
      scaleOption = char(value);
      argumentIndex = argumentIndex + 1;
    elseif isscalar(value)
      maxlag = value;
      argumentIndex = argumentIndex + 1;
    else
      yInput = value;
      argumentIndex = argumentIndex + 1;
    end
  end

  if argumentIndex <= nargin
    value = varargin{argumentIndex};
    if ischar(value) || isstring(value)
      scaleOption = char(value);
    else
      maxlag = value;
    end
    argumentIndex = argumentIndex + 1;
  end

  if argumentIndex <= nargin
    value = varargin{argumentIndex};
    if ischar(value) || isstring(value)
      scaleOption = char(value);
    else
      maxlag = value;
    end
  end

  if isempty(yInput) && ~isvector(xInput)
    [c, lags] = matrixAutocorrelation(xInput, maxlag, scaleOption);
    varargout{1} = c;
    if nargout > 1
      varargout{2} = lags;
    end
    return;
  end

  x = xInput(:);
  if isempty(yInput)
    y = x;
  else
    y = yInput(:);
  end
  if isempty(maxlag)
    maxlag = length(x) - 1;
    if length(y) > length(x)
      maxlag = length(y) - 1;
    end
  else
    maxlag = validateMaxlag(maxlag);
  end

  fullCorrelation = conv(x, conj(y(end:-1:1)));
  fullLags = (-(length(y) - 1):(length(x) - 1)).';
  lags = (-maxlag:maxlag).';
  c = zeros(length(lags), 1);
  for idx = 1:length(lags)
    loc = find(fullLags == lags(idx), 1, 'first');
    if ~isempty(loc)
      c(idx) = fullCorrelation(loc);
    end
  end
  c = applyScale(c, lags, length(x), x, y, scaleOption);

  if size(xInput, 1) == 1
    c = c.';
    lags = lags.';
  end

  varargout{1} = c;
  if nargout > 1
    varargout{2} = lags;
  end
end
%=============================================================================
function maxlag = validateMaxlag(maxlag)
  mustBeInteger(maxlag);
  maxlag = abs(double(maxlag));
end
%=============================================================================
function [c, lags] = matrixAutocorrelation(x, maxlag, scaleOption)
  if isempty(maxlag)
    maxlag = size(x, 1) - 1;
  else
    maxlag = validateMaxlag(maxlag);
  end
  columns = size(x, 2);
  c = zeros(2 * maxlag + 1, columns * columns);
  lags = (-maxlag:maxlag).';
  outputColumn = 1;
  for firstColumn = 1:columns
    for secondColumn = 1:columns
      c(:, outputColumn) = xcorr(x(:, firstColumn), x(:, secondColumn), maxlag, scaleOption);
      outputColumn = outputColumn + 1;
    end
  end
end
%=============================================================================
function c = applyScale(c, lags, n, x, y, scaleOption)
  if strcmp(scaleOption, 'none')
    return;
  elseif strcmp(scaleOption, 'biased')
    c = c ./ n;
  elseif strcmp(scaleOption, 'unbiased')
    c = c ./ (n - abs(lags));
  elseif strcmp(scaleOption, 'coeff') || strcmp(scaleOption, 'normalized')
    if length(x) ~= length(y)
      error('Nelson:signal:xcorr:invalidScaleOption', 'Scale option must be ''none'' for input vectors of different lengths.');
    end
    scale = sqrt(sum(abs(x) .^ 2) * sum(abs(y) .^ 2));
    if scale == 0
      c(:) = NaN;
    else
      c = c ./ scale;
    end
  else
    error('Nelson:signal:xcorr:unknownScaleOption', 'Unknown scale option.');
  end
end
%=============================================================================
