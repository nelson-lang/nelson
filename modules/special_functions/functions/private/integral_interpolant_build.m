%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Adaptive integration of fun from lower to upper, keeping the final mesh
% of every segment (split at the waypoints) with the partial sums before
% each of its intervals, so that integral_interpolant_eval can evaluate
% the integral from lower to any x.
%=============================================================================
function data = integral_interpolant_build(fun, lower, upper, opts)
  if ~opts.Vectorized && ~opts.ArrayValued
    fun = @(x) arrayfun(fun, x);
  end
  lo = min(lower, upper);
  hi = max(lower, upper);
  points = integral_path(lo, hi, opts.Waypoints);
  nseg = numel(points) - 1;
  segments = cell(1, nseg);
  subintervals = cell(1, nseg);
  total = 0;
  errbnd = 0;
  for k = 1:nseg
    [q, err, mesh] = integral_gk(fun, points(k), points(k + 1), opts.RelTol, opts.AbsTol, opts.ArrayValued);
    Q = mesh.Q;
    if opts.ArrayValued
      Q = cell2mat(cellfun(@(c) c(:).', Q(:), 'UniformOutput', false));
      mesh.Qcum = [zeros(1, size(Q, 2)); cumsum(Q(1:end - 1, :), 1)] + total(:).';
    else
      mesh.Qcum = [0; cumsum(Q(1:end - 1)).'] + total;
    end
    mesh.edges = [mesh.L, mesh.R(end)];
    x = integral_map(mesh.kind, mesh.a, mesh.b, mesh.edges);
    x([1, end]) = [mesh.a, mesh.b];
    if k > 1
      x = x(2:end);
    end
    subintervals{k} = x;
    segments{k} = mesh;
    total = total + q;
    errbnd = errbnd + err;
  end
  subintervals = [subintervals{:}];
  data = struct();
  data.fun = fun;
  data.arrayValued = opts.ArrayValued;
  data.points = points;
  data.segments = segments;
  data.backward = lower > upper;
  data.total = total;
  data.valueSize = size(total);
  if data.backward
    data.integral = -total;
    data.subintervals = fliplr(subintervals);
  else
    data.integral = total;
    data.subintervals = subintervals;
  end
  data.errbnd = errbnd;
end
%=============================================================================
