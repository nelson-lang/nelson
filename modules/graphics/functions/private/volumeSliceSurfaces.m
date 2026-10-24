%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function handles = volumeSliceSurfaces(parent, x, y, z, V, xs, ys, zs, method, surfaceArgs)
  if ~areAxisSlices(xs, ys, zs)
    C = interp3(x, y, z, V, xs, ys, zs, method, NaN);
    handles = surface(parent, xs, ys, zs, C, surfaceArgs{:});
    return
  end
  handles = gobjects(0, 1);
  for k = 1:numel(xs)
    [Xq, Yq, Zq] = xSliceGrid(x, y, z, xs(k));
    C = interp3(x, y, z, V, Xq, Yq, Zq, method, NaN);
    handles(end + 1, 1) = surface(parent, Xq, Yq, Zq, C, surfaceArgs{:});
  end
  for k = 1:numel(ys)
    [Xq, Yq, Zq] = ySliceGrid(x, y, z, ys(k));
    C = interp3(x, y, z, V, Xq, Yq, Zq, method, NaN);
    handles(end + 1, 1) = surface(parent, Xq, Yq, Zq, C, surfaceArgs{:});
  end
  for k = 1:numel(zs)
    [Xq, Yq, Zq] = zSliceGrid(x, y, z, zs(k));
    C = interp3(x, y, z, V, Xq, Yq, Zq, method, NaN);
    handles(end + 1, 1) = surface(parent, Xq, Yq, Zq, C, surfaceArgs{:});
  end
end
%=============================================================================
function [Xq, Yq, Zq] = xSliceGrid(x, y, z, value)
  [Yq, Zq] = ndgrid(y, z);
  Xq = value * ones(size(Yq));
end
%=============================================================================
function [Xq, Yq, Zq] = ySliceGrid(x, y, z, value)
  [Xq, Zq] = ndgrid(x, z);
  Yq = value * ones(size(Xq));
end
%=============================================================================
function [Xq, Yq, Zq] = zSliceGrid(x, y, z, value)
  [Xq, Yq] = meshgrid(x, y);
  Zq = value * ones(size(Xq));
end
%=============================================================================
function tf = areAxisSlices(xs, ys, zs)
  tf = min(size(xs)) <= 1 && min(size(ys)) <= 1 && min(size(zs)) <= 1;
end
%=============================================================================
