%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = rms(x, varargin)
  narginchk(1, 4);
  nargoutchk(0, 1);
  inputClass = class(x);
  [mode, dims, omitNan, outputType] = parseRmsOptions(x, varargin{:});
  inputIsInteger = isinteger(x);
  if inputIsInteger
    % integer arithmetic saturates: abs and the square must run in double
    x = double(x);
  end
  values = abs(x) .^ 2;
  if omitNan
    valid = ~isnan(values);
    values(~valid) = 0;
    sums = reduceSum(values, mode, dims);
    counts = reduceSum(double(valid), mode, dims);
    y = sqrt(sums ./ counts);
    y(counts == 0) = NaN;
  else
    y = sqrt(reduceMean(values, mode, dims));
  end
  if strcmp(outputType, 'double')
    y = double(y);
  elseif inputIsInteger && strcmp(outputType, 'native')
    y = cast(y, inputClass);
  elseif strcmp(inputClass, 'single')
    y = single(y);
  end
end
%=============================================================================
function [mode, dims, omitNan, outputType] = parseRmsOptions(x, varargin)
  mode = 'dim';
  dims = [];
  omitNan = false;
  outputType = 'default';
  hasDimension = false;
  for k = 1:length(varargin)
    value = varargin{k};
    if ischar(value) || isstring(value)
      option = lower(char(value));
      if strcmp(option, 'all')
        if hasDimension
          error('Nelson:signal:dimensionAlreadySpecified', 'Dimension already specified.');
        end
        mode = 'all';
        hasDimension = true;
      elseif strcmp(option, 'omitnan') || strcmp(option, 'omitmissing')
        omitNan = true;
      elseif strcmp(option, 'includenan') || strcmp(option, 'includemissing')
        omitNan = false;
      elseif strcmp(option, 'default') || strcmp(option, 'double') || strcmp(option, 'native')
        outputType = option;
      else
        error('Nelson:signal:unsupportedOption', 'Unsupported option.');
      end
    else
      if hasDimension
        error('Nelson:signal:dimensionAlreadySpecified', 'Dimension already specified.');
      end
      dims = value;
      hasDimension = true;
    end
  end
  if ~hasDimension
    dims = firstNonSingletonDimension(x);
  elseif strcmp(mode, 'dim')
    dims = validateDimensions(dims);
  end
end
%=============================================================================
function dims = validateDimensions(dims)
  if isempty(dims) || ~isnumeric(dims) || ~isvector(dims)
    error('Nelson:signal:dimensionScalarOrVector', 'Dimension must be a positive integer scalar or vector.');
  end
  if any(~isfinite(dims(:))) || any(dims(:) <= 0) || any(dims(:) ~= fix(dims(:)))
    error('Nelson:signal:dimensionScalarOrVector', 'Dimension must be a positive integer scalar or vector.');
  end
  dims = uniqueSorted(dims);
end
%=============================================================================
function dims = uniqueSorted(dims)
  sorted = sort(dims(:).');
  dims = sorted(1);
  for k = 2:length(sorted)
    if sorted(k) ~= dims(end)
      dims = [dims, sorted(k)];
    end
  end
end
%=============================================================================
function dim = firstNonSingletonDimension(x)
  shape = size(x);
  dim = find(shape ~= 1, 1, 'first');
  if isempty(dim)
    dim = 1;
  end
end
%=============================================================================
function y = reduceMean(values, mode, dims)
  if strcmp(mode, 'all')
    y = mean(values(:));
    return;
  end
  y = values;
  for dim = dims
    y = mean(y, dim);
  end
end
%=============================================================================
function y = reduceSum(values, mode, dims)
  if strcmp(mode, 'all')
    y = sum(values(:));
    return;
  end
  y = values;
  for dim = dims
    y = sum(y, dim);
  end
end
%=============================================================================
