%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [B, C, S] = normalize(A, varargin)
  narginchk(1, 5);
  nargoutchk(0, 3);
  if ~isnumeric(A) && ~islogical(A)
    error('Nelson:validators:mustBeNumericOrLogical', '%s must be numeric or logical.', 'Input');
  end
  A = double(A);

  args = varargin;
  % optional leading dimension
  if ~isempty(args) && isnumeric(args{1})
    dim = args{1};
    args(1) = [];
  else
    dim = find(size(A) ~= 1, 1, 'first');
    if isempty(dim)
      dim = 1;
    end
  end

  method = 'zscore';
  if ~isempty(args)
    method = lower(char(args{1}));
    args(1) = [];
  end
  param = [];
  if ~isempty(args)
    param = args{1};
  end

  C = 0;
  S = 1;
  switch method
    case 'zscore'
      kind = 'std';
      if ~isempty(param)
        kind = lower(char(param));
      end
      if strcmp(kind, 'robust')
        C = median(A, dim);
        S = local_mad(A, dim);
      else
        C = mean(A, dim);
        S = std(A, 0, dim);
      end
    case 'center'
      C = local_center(A, dim, param);
      S = 1;
    case 'scale'
      C = 0;
      S = local_scale(A, dim, param);
    case 'norm'
      p = 2;
      if ~isempty(param)
        p = param;
      end
      C = 0;
      S = vecnorm(A, p, dim);
    case 'range'
      lohi = [0 1];
      if ~isempty(param)
        lohi = param;
      end
      mn = min(A, [], dim);
      mx = max(A, [], dim);
      S = mx - mn;
      S(S == 0) = 1;
      B = lohi(1) + (A - mn) ./ S .* (lohi(2) - lohi(1));
      C = mn;
      return;
    case 'medianiqr'
      C = median(A, dim);
      S = local_iqr(A, dim);
    otherwise
      error('Nelson:normalize:invalidMethod', 'Unsupported "%s" option.', method);
  end
  S(S == 0) = 1;
  B = (A - C) ./ S;
end
%=============================================================================
function C = local_center(A, dim, param)
  if isempty(param)
    C = mean(A, dim);
  elseif isnumeric(param)
    C = param;
  else
    switch lower(char(param))
      case 'mean'
        C = mean(A, dim);
      case 'median'
        C = median(A, dim);
      otherwise
        error('Nelson:normalize:invalidCenter', 'Unsupported "%s" option.', char(param));
    end
  end
end
%=============================================================================
function S = local_scale(A, dim, param)
  if isempty(param)
    S = std(A, 0, dim);
  elseif isnumeric(param)
    S = param;
  else
    switch lower(char(param))
      case 'std'
        S = std(A, 0, dim);
      case 'mad'
        S = local_mad(A, dim);
      case 'iqr'
        S = local_iqr(A, dim);
      case 'first'
        idx = repmat({':'}, 1, ndims(A));
        idx{dim} = 1;
        S = A(idx{:});
      otherwise
        error('Nelson:normalize:invalidScale', 'Unsupported "%s" option.', char(param));
    end
  end
  S(S == 0) = 1;
end
%=============================================================================
function m = local_mad(A, dim)
  m = median(abs(A - median(A, dim)), dim);
end
%=============================================================================
function q = local_iqr(A, dim)
  q = prctile(A, 75, dim) - prctile(A, 25, dim);
end
%=============================================================================
