%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = slice(varargin)
  narginchk(4, 9);
  nargoutchk(0, 1);
  [parent, x, y, z, V, xs, ys, zs, method] = parseVolumeSliceInputs(varargin);
  ax = graphicsTargetAxes(parent);
  if isempty(parent)
    parent = ax;
  end
  h = volumeSliceSurfaces(parent, x, y, z, V, xs, ys, zs, method, ...
    {'FaceColor', 'flat', 'EdgeColor', graphicsDefaultEdgeColor()});
  applySliceAxes(ax, x, y, z, V);
  if ~isempty(h)
    h(1).UserData = struct('ChartType', 'slice', 'XSlice', xs, 'YSlice', ys, 'ZSlice', zs);
  end
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function applySliceAxes(ax, x, y, z, V)
  ax.XLim = [min(x) max(x)];
  ax.YLim = [min(y) max(y)];
  ax.ZLim = [min(z) max(z)];
  finiteValues = V(isfinite(V));
  if ~isempty(finiteValues)
    limits = [min(finiteValues(:)) max(finiteValues(:))];
    if limits(1) ~= limits(2)
      ax.CLim = limits;
    end
  end
  view(ax, 3);
  grid(ax, 'on');
end
%=============================================================================
