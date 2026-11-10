%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = svd(A, varargin)
  % singular values and vectors by one-sided Jacobi on the device; other forms run on the host
  economy = nargin == 2 && ((isnumeric(varargin{1}) && isscalar(varargin{1}) && varargin{1} == 0) ...
    || ((ischar(varargin{1}) || isstring(varargin{1})) && strcmpi(varargin{1}, 'econ')));
  original = A;
  [m, n] = size(A);
  device = isfloat(A) && isreal(A) && ismatrix(A) && ~isempty(A) && (nargin == 1 || economy) ...
    && (nargout <= 1 || economy || m == n);
  if device
    transposed = m < n;
    if transposed
      A = A.';
    end
    [W, V, ok] = __gpuArray_jacobi_svd__(A);
    device = ok;
  end
  if device
    s = vecnorm(W, 2, 1);
    [hostValues, order] = sort(gather(s), 'descend');
    s = s(order);
    if nargout <= 1
      varargout{1} = s(:);
      return
    end
    % columns of a rank deficient matrix have no direction: the host completes them
    device = hostValues(end) > hostValues(1) * numel(hostValues) * eps('single');
  end
  if device
    U = W(:, order) ./ s;
    V = V(:, order);
    S = diag(s);
    if transposed
      [U, V] = deal(V, U);
    end
    varargout = {U, S, V};
    if nargout <= 2
      varargout = varargout(1:max(nargout, 1));
    end
    return
  end
  [varargout{1:max(1, nargout)}] = __gpuArray_host__('svd', original, varargin{:});
end
%=============================================================================
