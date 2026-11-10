%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = eig(A, varargin)
  % eigenvalues of a real symmetric matrix from the Jacobi sweeps on the device; other cases
  % (not symmetric, options, generalized problems, mixed signs of equal size) run on the host
  done = false;
  if nargin == 1 && nargout <= 2 && isfloat(A) && isreal(A) && ismatrix(A) && ~isempty(A) ...
      && size(A, 1) == size(A, 2) && issymmetric(A)
    [W, V, ok] = __gpuArray_jacobi_svd__(A);
    if ok
      % signed eigenvalues: Rayleigh quotient of each right vector
      lambda = sum(V .* W, 1);
      [~, order] = sort(gather(lambda), 'ascend');
      lambda = lambda(order);
      V = V(:, order);
      scale = max(1, double(gather(max(abs(lambda)))));
      residual = double(gather(max(max(abs(A * V - V .* lambda)))));
      done = residual <= 1e-4 * scale;
    end
  end
  if done
    if nargout <= 1
      varargout{1} = lambda(:);
    else
      varargout{1} = V;
      varargout{2} = diag(lambda);
    end
    return
  end
  [varargout{1:max(1, nargout)}] = __gpuArray_host__('eig', A, varargin{:});
end
%=============================================================================
