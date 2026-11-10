%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [X, ok] = __gpuArray_sqrtm__(A)
  % Denman-Beavers iteration for a real square device matrix; ok is false when it does not
  % converge to a root (negative eigenvalues, singular or not square)
  X = A;
  ok = false;
  if ~(isfloat(A) && isreal(A) && ismatrix(A) && size(A, 1) == size(A, 2) && ~isempty(A))
    return
  end
  if ~isgpuarray(A)
    A = gpuArray(A);
  end
  n = size(A, 1);
  normA = double(gather(max(sum(abs(A), 1))));
  if normA == 0
    return
  end
  % a singular step only means no real root here: the host then answers with its warning
  previous = warning('off', 'Nelson:singularMatrix');
  restore = onCleanup(@() warning(previous));
  Y = A;
  Z = eye(n, 'like', A);
  for k = 1:60
    Yinv = inv(Y);
    Zinv = inv(Z);
    Ynext = (Y + Zinv) / 2;
    Z = (Z + Yinv) / 2;
    change = double(gather(max(sum(abs(Ynext - Y), 1))));
    Y = Ynext;
    if ~isfinite(change)
      return
    end
    if change <= 1e-6 * double(gather(max(sum(abs(Y), 1))))
      break
    end
  end
  residual = double(gather(max(sum(abs(Y * Y - A), 1)))) / normA;
  X = Y;
  ok = residual < 1e-4;
end
%=============================================================================
