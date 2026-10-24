%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Gauss-Kronrod rule on the intervals [L(k), R(k)] of the normalized
% variable (array-valued integrand evaluated at scalar abscissae).
%=============================================================================
function [Qcell, err] = integral_gk_eval_av(fun, kind, a, b, nodes, wk, wgw, L, R)
  M = numel(L);
  Qcell = cell(1, M);
  err = zeros(1, M);
  ng = numel(nodes);
  for m = 1:M
    half = 0.5 * (R(m) - L(m));
    mid = 0.5 * (R(m) + L(m));
    tn = mid + half * nodes;
    [xn, wn] = integral_map(kind, a, b, tn);
    Sk = [];
    Sg = [];
    for k = 1:ng
      gv = fun(xn(k)) .* wn(k);
      if k == 1
        Sk = gv .* wk(k);
        Sg = gv .* wgw(k);
      else
        Sk = Sk + gv .* wk(k);
        Sg = Sg + gv .* wgw(k);
      end
    end
    Qk = half .* Sk;
    Qg = half .* Sg;
    Qcell{m} = Qk;
    d = abs(Qg(:) - Qk(:));
    err(m) = max(d);
  end
end
%=============================================================================
