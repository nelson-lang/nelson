%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function L = logm(A)
  % principal logarithm on the device: square roots until close to the identity, then a
  % Gauss-Legendre Pade form of log(I + Y); other cases keep the host answer
  L = [];
  if isfloat(A) && isreal(A) && ismatrix(A) && size(A, 1) == size(A, 2) && ~isempty(A)
    n = size(A, 1);
    I = eye(n, 'like', A);
    X = A;
    k = 0;
    ok = true;
    while ok && double(gather(max(sum(abs(X - I), 1)))) > 0.25 && k < 40
      [X, ok] = __gpuArray_sqrtm__(X);
      k = k + 1;
    end
    if ok && k < 40
      Y = X - I;
      nodes = [0.0254460438286207, 0.1292344072003028, 0.2970774243113014, 0.5, ...
               0.7029225756886986, 0.8707655927996972, 0.9745539561713793];
      weights = [0.0647424830844349, 0.1398526957446383, 0.1909150252525595, 0.2089795918367347, ...
                 0.1909150252525595, 0.1398526957446383, 0.0647424830844349];
      S = zeros(n, 'like', A);
      for j = 1:numel(nodes)
        S = S + weights(j) * ((I + nodes(j) * Y) \ Y);
      end
      L = 2 ^ k * S;
      % the result must give A back
      residual = double(gather(max(sum(abs(expm(L) - A), 1)))) / max(double(gather(max(sum(abs(A), 1)))), realmin('single'));
      if ~(residual < 1e-3)
        L = [];
      end
    end
  end
  if isempty(L)
    L = __gpuArray_host__('logm', A);
  end
end
%=============================================================================
