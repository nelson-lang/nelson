%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = corrcoef(varargin)
  % correlation coefficients from the device covariance; options, p-values and NaN data run on
  % the host
  done = false;
  if nargout <= 1 && (nargin == 1 || nargin == 2) && all(cellfun(@(v) isfloat(v) && isreal(v) && ismatrix(v) && ~isempty(v), varargin))
    if nargin == 2
      if numel(varargin{1}) == numel(varargin{2})
        X = [reshape(gpuArray(varargin{1}), [], 1), reshape(gpuArray(varargin{2}), [], 1)];
      else
        X = [];
      end
    else
      X = varargin{1};
      if isrow(X)
        X = X(:);
      end
    end
    if ~isempty(X) && size(X, 1) > 1 && ~gather(any(isnan(X(:))))
      C = cov(X);
      d = sqrt(diag(C));
      R = C ./ (d * d.');
      R = (R + R.') / 2;
      R(R > 1) = 1;
      R(R < -1) = -1;
      n = size(R, 1);
      R(1:n + 1:end) = sign(diag(R));
      varargout{1} = R;
      done = true;
    end
  end
  if ~done
    [varargout{1:max(1, nargout)}] = __gpuArray_host__('corrcoef', varargin{:});
  end
end
%=============================================================================
