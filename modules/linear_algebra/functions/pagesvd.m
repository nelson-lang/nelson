%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = pagesvd(X, varargin)
  % singular value decomposition of every page of X
  narginchk(1, 3);
  nargoutchk(0, 3);
  if ~isfloat(X)
    error('Nelson:linalg:pageFloatInput', 'Inputs must be single or double arrays.');
  end
  econ = false;
  if nargout > 1
    vectorForm = false;
  else
    vectorForm = true;
  end
  for k = 1:numel(varargin)
    option = varargin{k};
    if isnumeric(option) && isscalar(option) && option == 0
      econ = true;
      continue
    end
    if ~(ischar(option) || isstring(option))
      error('Nelson:pagesvd:invalidOption', 'Options must be ''econ'', ''vector'' or ''matrix''.');
    end
    switch lower(char(option))
      case 'econ'
        econ = true;
      case 'vector'
        vectorForm = true;
      case 'matrix'
        vectorForm = false;
      otherwise
        error('Nelson:pagesvd:invalidOption', 'Options must be ''econ'', ''vector'' or ''matrix''.');
    end
  end
  sz = size(X);
  m = sz(1);
  n = sz(2);
  pageSize = sz(3:end);
  count = prod(pageSize);
  Xr = reshape(X, m, n, count);
  r = min(m, n);
  if nargout <= 1
    if vectorForm
      S = zeros([r, 1, count], 'like', real(Xr));
    elseif econ
      S = zeros([r, r, count], 'like', real(Xr));
    else
      S = zeros([m, n, count], 'like', real(Xr));
    end
    for k = 1:count
      if econ
        [~, Sk] = svd(Xr(:, :, k), 'econ');
      else
        [~, Sk] = svd(Xr(:, :, k));
      end
      if vectorForm
        S(:, 1, k) = diag(Sk);
      else
        S(:, :, k) = Sk;
      end
    end
    varargout{1} = reshape(S, [size(S, 1), size(S, 2), pageSize]);
    return
  end
  if econ
    uCols = r;
    sShape = [r, r];
    vCols = r;
  else
    uCols = m;
    sShape = [m, n];
    vCols = n;
  end
  if vectorForm
    sShape = [r, 1];
  end
  U = zeros([m, uCols, count], 'like', Xr);
  S = zeros([sShape, count], 'like', real(Xr));
  V = zeros([n, vCols, count], 'like', Xr);
  for k = 1:count
    if econ
      [Uk, Sk, Vk] = svd(Xr(:, :, k), 'econ');
    else
      [Uk, Sk, Vk] = svd(Xr(:, :, k));
    end
    U(:, :, k) = Uk;
    V(:, :, k) = Vk;
    if vectorForm
      S(:, 1, k) = diag(Sk);
    else
      S(:, :, k) = Sk;
    end
  end
  varargout{1} = reshape(U, [m, uCols, pageSize]);
  varargout{2} = reshape(S, [sShape, pageSize]);
  if nargout > 2
    varargout{3} = reshape(V, [n, vCols, pageSize]);
  end
end
%=============================================================================
