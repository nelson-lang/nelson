%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Adaptive Gauss-Kronrod (G7-K15) quadrature with support for infinite
% limits and array-valued integrands.
% The optional mesh output describes the final subdivision of the
% normalized variable: map (kind, a, b), sorted intervals [L, R] and their
% integrals Q (row vector, or cell array for array-valued integrands).
%=============================================================================
function [q, errbnd, mesh] = integral_gk(fun, a, b, reltol, abstol, arrayValued, maxIntervals)
  if nargin < 7 || isempty(maxIntervals)
    maxIntervals = 100000;
  end
  a = double(a);
  b = double(b);
  if isnan(a) || isnan(b)
    q = NaN;
    errbnd = NaN;
    mesh = [];
    return
  end
  if a == b
    q = integral_zero_result(fun, a, arrayValued);
    errbnd = 0;
    mesh = [];
    return
  end
  sgn = 1;
  if a > b
    tmp = a;
    a = b;
    b = tmp;
    sgn = -1;
  end
  kind = integral_transform_kind(a, b);

  [nodes, wk, wgw] = integral_gk_rule();

  % initial subdivision of the normalized interval [-1, 1]
  edges = linspace(-1, 1, 11);
  L = edges(1:end - 1);
  R = edges(2:end);

  if arrayValued
    [q, errbnd, L, R, Q] = integral_gk_arrayvalued(fun, kind, a, b, nodes, wk, wgw, ...
      L, R, reltol, abstol, maxIntervals);
  else
    [q, errbnd, L, R, Q] = integral_gk_vectorized(fun, kind, a, b, nodes, wk, wgw, ...
      L, R, reltol, abstol, maxIntervals);
  end
  q = sgn * q;
  if nargout > 2
    [L, order] = sort(L);
    mesh = struct('kind', kind, 'a', a, 'b', b, 'L', L, 'R', R(order));
    mesh.Q = Q(order);
  end
end
%=============================================================================
function [q, errbnd, L, R, Qk] = integral_gk_vectorized(fun, kind, a, b, nodes, wk, wgw, ...
  L, R, reltol, abstol, maxIntervals)
  [Qk, err] = integral_gk_eval_vec(fun, kind, a, b, nodes, wk, wgw, L, R);
  while true
    Q = sum(Qk);
    E = sum(err);
    if ~isfinite(Q)
      break
    end
    tol = max(abstol, reltol * abs(Q));
    if E <= tol
      break
    end
    thresh = tol / numel(L);
    idx = find(err > thresh);
    if isempty(idx)
      break
    end
    Ls = L(idx);
    Rs = R(idx);
    Ms = 0.5 * (Ls + Rs);
    nL = [Ls, Ms];
    nR = [Ms, Rs];
    [nQk, nErr] = integral_gk_eval_vec(fun, kind, a, b, nodes, wk, wgw, nL, nR);
    keep = true(1, numel(L));
    keep(idx) = false;
    L = [L(keep), nL];
    R = [R(keep), nR];
    Qk = [Qk(keep), nQk];
    err = [err(keep), nErr];
    if numel(L) > maxIntervals
      warning('Nelson:integral:maxIntervalCount', 'Reached the limit on the maximum number of intervals in use. Answer may not be accurate.');
      break
    end
  end
  q = sum(Qk);
  errbnd = sum(err);
end
%=============================================================================
function [q, errbnd, L, R, Qcell] = integral_gk_arrayvalued(fun, kind, a, b, nodes, wk, wgw, ...
  L, R, reltol, abstol, maxIntervals)
  [Qcell, err] = integral_gk_eval_av(fun, kind, a, b, nodes, wk, wgw, L, R);
  while true
    Q = integral_sum_cells(Qcell);
    E = sum(err);
    Qnorm = max(abs(Q(:)));
    if ~all(isfinite(Q(:)))
      break
    end
    tol = max(abstol, reltol * Qnorm);
    if E <= tol
      break
    end
    thresh = tol / numel(L);
    idx = find(err > thresh);
    if isempty(idx)
      break
    end
    Ls = L(idx);
    Rs = R(idx);
    Ms = 0.5 * (Ls + Rs);
    nL = [Ls, Ms];
    nR = [Ms, Rs];
    [nQcell, nErr] = integral_gk_eval_av(fun, kind, a, b, nodes, wk, wgw, nL, nR);
    keep = true(1, numel(L));
    keep(idx) = false;
    L = [L(keep), nL];
    R = [R(keep), nR];
    Qcell = [Qcell(keep), nQcell];
    err = [err(keep), nErr];
    if numel(L) > maxIntervals
      warning('Nelson:integral:maxIntervalCount', 'Reached the limit on the maximum number of intervals in use. Answer may not be accurate.');
      break
    end
  end
  q = integral_sum_cells(Qcell);
  errbnd = sum(err);
end
%=============================================================================
function s = integral_sum_cells(Qcell)
  s = Qcell{1};
  for m = 2:numel(Qcell)
    s = s + Qcell{m};
  end
end
%=============================================================================
function kind = integral_transform_kind(a, b)
  fa = isfinite(a);
  fb = isfinite(b);
  if fa && fb
    kind = 'finite';
  elseif fa && ~fb
    kind = 'ainf';
  elseif ~fa && fb
    kind = 'binf';
  else
    kind = 'both';
  end
end
%=============================================================================
