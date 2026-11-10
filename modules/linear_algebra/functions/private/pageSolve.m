%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [Z, rc] = pageSolve(X, Y, leftDivide, wantRcond)
  % page-wise X\Y (or X/Y) with page expansion and warnings turned off
  [Xr, Yr, xMap, yMap, pageSize] = pageBroadcast(X, Y);
  if leftDivide
    if size(Xr, 1) ~= size(Yr, 1)
      error('Nelson:linalg:pageRowsMismatch', 'The two inputs must have the same number of rows.');
    end
    outSize = [size(Xr, 2), size(Yr, 2)];
  else
    if size(Xr, 2) ~= size(Yr, 2)
      error('Nelson:linalg:pageColumnsMismatch', 'The two inputs must have the same number of columns.');
    end
    outSize = [size(Xr, 1), size(Yr, 1)];
  end
  count = numel(xMap);
  proto = zeros(1, 'like', X) + zeros(1, 'like', Y);
  Z = zeros([outSize, count], 'like', proto);
  rc = zeros([1, 1, count], 'like', real(proto));
  previous = warning();
  warning('off', 'all');
  try
    for k = 1:count
      P = Xr(:, :, xMap(k));
      Q = Yr(:, :, yMap(k));
      if leftDivide
        Z(:, :, k) = P \ Q;
      else
        Z(:, :, k) = P / Q;
      end
      if wantRcond
        rc(k) = pageRcond(P);
      end
    end
  catch err
    warning(previous);
    rethrow(err);
  end
  warning(previous);
  Z = reshape(Z, [outSize, pageSize]);
  rc = reshape(rc, [1, 1, pageSize]);
end
%=============================================================================
function r = pageRcond(P)
  % rcond of a square page, or of the triangular factor of a rectangular one
  if size(P, 1) == size(P, 2)
    r = rcond(P);
  else
    [~, R] = qr(P, 0);
    k = min(size(R));
    r = rcond(R(1:k, 1:k));
  end
end
%=============================================================================
