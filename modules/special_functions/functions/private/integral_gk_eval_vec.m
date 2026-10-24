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
% variable (vectorized integrand): integrals and error estimates.
%=============================================================================
function [Qk, err] = integral_gk_eval_vec(fun, kind, a, b, nodes, wk, wgw, L, R)
  half = 0.5 * (R - L);
  mid = 0.5 * (R + L);
  T = mid.' + half.' .* nodes;
  [X, W] = integral_map(kind, a, b, T);
  G = fun(X) .* W;
  Qk = half .* (G * wk.').';
  Qg = half .* (G * wgw.').';
  err = abs(Qk - Qg);
end
%=============================================================================
