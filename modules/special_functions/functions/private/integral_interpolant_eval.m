%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% F(x) = integral of fun from lower to x, for the data computed by
% integral_interpolant_build: partial sum of the mesh intervals before x
% plus the Gauss-Kronrod rule on the part of the interval containing x.
% NaN outside the integration interval.
%=============================================================================
function Fq = integral_interpolant_eval(data, xq)
  x = double(xq(:).');
  nq = numel(x);
  nv = prod(data.valueSize);
  G = NaN(nq, nv);
  done = false(1, nq);
  points = data.points;
  for k = 1:numel(data.segments)
    in = find(~done & x >= points(k) & x <= points(k + 1));
    if ~isempty(in)
      G(in, :) = integralSegmentEval(data, data.segments{k}, x(in));
      done(in) = true;
    end
  end
  if data.backward
    G = G - data.total(:).';
  end
  if ~data.arrayValued
    Fq = reshape(G, size(xq));
  elseif numel(data.valueSize) == 2 && min(data.valueSize) == 1
    Fq = G;
  else
    Fq = reshape(G, [nq, data.valueSize]);
  end
end
%=============================================================================
function G = integralSegmentEval(data, mesh, x)
  t = integralInverseMap(mesh.kind, mesh.a, mesh.b, x);
  nint = numel(mesh.L);
  [~, j] = histc(t, mesh.edges);
  j = min(max(j, 1), nint);
  G = mesh.Qcum(j, :);
  % partial interval [L(j), t]: skipped when empty (x on a mesh point)
  part = find(t > mesh.L(j));
  if isempty(part)
    return
  end
  [nodes, wk, wgw] = integral_gk_rule();
  L = mesh.L(j(part));
  R = t(part);
  if data.arrayValued
    Qcell = integral_gk_eval_av(data.fun, mesh.kind, mesh.a, mesh.b, nodes, wk, wgw, L, R);
    for m = 1:numel(part)
      v = Qcell{m};
      G(part(m), :) = G(part(m), :) + v(:).';
    end
  else
    Qk = integral_gk_eval_vec(data.fun, mesh.kind, mesh.a, mesh.b, nodes, wk, wgw, L, R);
    G(part) = G(part) + Qk(:);
  end
end
%=============================================================================
function t = integralInverseMap(kind, a, b, x)
  % inverse of integral_map
  switch kind
    case 'finite'
      u = min(max((x - 0.5 * (a + b)) / (0.5 * (b - a)), -1), 1);
      t = 2 * sin(asin(u) / 3);
    case 'ainf'
      s = x - a;
      t = (s - 1) ./ (s + 1);
    case 'binf'
      s = b - x;
      t = (1 - s) ./ (1 + s);
    otherwise
      t = 2 * x ./ (1 + sqrt(1 + 4 * x .^ 2));
  end
  t(x == a) = -1;
  t(x == b) = 1;
end
%=============================================================================
