%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function m = errorMetric(F, A, relative, varargin)
  % weighted mean of |F - A|.^2 (or |(F - A) ./ A| when relative) along a dimension
  if ~(isfloat(F) && isfloat(A))
    error('Nelson:errorMetric:invalidType', 'Inputs must be single or double arrays.');
  end
  dim = [];
  nanflag = 'includenan';
  weights = [];
  k = 1;
  while k <= numel(varargin)
    arg = varargin{k};
    if ischar(arg) || isstring(arg)
      word = lower(char(arg));
      if strcmp(word, 'all')
        dim = 'all';
      elseif any(strcmp(word, {'omitnan', 'includenan'}))
        nanflag = word;
      elseif strcmp(word, 'weights') && k < numel(varargin)
        k = k + 1;
        weights = varargin{k};
      else
        error('Nelson:errorMetric:invalidOption', 'Unknown option.');
      end
    elseif isnumeric(arg) && isscalar(arg) && isempty(dim)
      dim = arg;
    else
      error('Nelson:errorMetric:invalidOption', 'Unknown option.');
    end
    k = k + 1;
  end
  defaultDim = false;
  E = F - A;
  if relative
    T = abs(E ./ A);
  else
    T = abs(E) .^ 2;
  end
  if ischar(dim)
    T = T(:);
    if ~isempty(weights)
      weights = weights(:);
    end
    dim = 1;
  elseif isempty(dim)
    defaultDim = true;
    dim = find(size(T) ~= 1, 1);
    if isempty(dim)
      dim = 1;
    end
  end
  if isempty(weights)
    if defaultDim
      % the default dimension of mean (an empty 0x0 input gives NaN)
      m = mean(T, nanflag);
    else
      m = mean(T, dim, nanflag);
    end
    return
  end
  if isvector(weights) && numel(weights) == size(T, dim) && ~isequal(size(weights), size(T))
    shape = ones(1, max(ndims(T), dim));
    shape(dim) = numel(weights);
    weights = reshape(weights, shape);
  elseif ~isequal(size(weights), size(T))
    error('Nelson:errorMetric:weightsSize', 'Weights must match the data along the operating dimension.');
  end
  W = weights .* ones(size(T), 'like', T);
  if strcmp(nanflag, 'omitnan')
    missing = isnan(T);
    W(missing) = 0;
    T(missing) = 0;
  end
  m = sum(W .* T, dim) ./ sum(W, dim);
end
%=============================================================================
