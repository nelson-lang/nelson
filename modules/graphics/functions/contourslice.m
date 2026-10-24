%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = contourslice(varargin)
  narginchk(4, 1000);
  nargoutchk(0, 1);
  [parent, x, y, z, V, xs, ys, zs, levels, method] = parseContourSliceInputs(varargin);
  ax = graphicsAddTargetAxes(parent);
  if isempty(parent)
    parent = ax;
  end
  [xs, ys, zs] = expandAllSlices(x, y, z, xs, ys, zs);
  h = drawContourSlices(parent, x, y, z, V, xs, ys, zs, levels, method);
  applyContourColors(ax, h);
  applyContourAxesLimits(ax, h);
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function [parent, x, y, z, V, xs, ys, zs, levels, method] = parseContourSliceInputs(args)
  args = convertStringToCharArgs(args);
  [parent, args] = graphicsParseParent(args);
  method = [];
  if ~isempty(args) && ischar(args{end})
    method = args{end};
    args = args(1:end - 1);
    validateInterpolationMethod(method);
  end
  dataArgs = args;
  levels = [];
  if numel(dataArgs) == 5 || numel(dataArgs) == 8
    levels = dataArgs{end};
    if ~isnumeric(levels)
      error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Contour levels');
    end
    dataArgs = dataArgs(1:end - 1);
  end
  [x, y, z, V, xs, ys, zs] = parseContourVolumeInputs(dataArgs);
  if isempty(method)
    if isAxisAlignedSlices(xs, ys, zs)
      method = 'nearest';
    else
      method = 'linear';
    end
  end
end
%=============================================================================
function [x, y, z, V, xs, ys, zs] = parseContourVolumeInputs(args)
  switch numel(args)
    case 4
      V = validateContourVolume(args{1});
      [x, y, z] = volumeGridVectors(V);
      xs = args{2};
      ys = args{3};
      zs = args{4};
    case 7
      V = validateContourVolume(args{4});
      [x, y, z] = volumeGridVectors(args{1}, args{2}, args{3}, V);
      xs = args{5};
      ys = args{6};
      zs = args{7};
    otherwise
      if numel(args) < 4
        error('Nelson:notEnoughInputs', 'Not enough input arguments.');
      end
      if numel(args) > 7
        error('Nelson:tooManyInputs', 'Too many input arguments.');
      end
      error('Nelson:wrongNumberOfInputs', 'Wrong number of input arguments.');
  end
  xs = numericSliceInput(xs);
  ys = numericSliceInput(ys);
  zs = numericSliceInput(zs);
end
%=============================================================================
function V = validateContourVolume(V)
  if ~isnumeric(V) || ndims(V) ~= 3
    error('Nelson:graphics:invalidVolumeData', 'Volume data must be a numeric 3-D array.');
  end
  V = double(V);
end
%=============================================================================
function values = numericSliceInput(values)
  if isempty(values)
    values = [];
    return
  end
  if ~isnumeric(values)
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Slice locations');
  end
  values = double(values);
  if isvector(values)
    values = values(:).';
  end
end
%=============================================================================
function validateInterpolationMethod(method)
  if ~any(strcmp(method, {'linear', 'nearest', 'cubic'}))
    error('Nelson:contourslice:invalidInterpolationMethod', 'Interpolation method must be ''linear'', ''nearest'', or ''cubic''.');
  end
end
%=============================================================================
function [xs, ys, zs] = expandAllSlices(x, y, z, xs, ys, zs)
  if isscalar(xs) && isnan(xs)
    xs = x;
  end
  if isscalar(ys) && isnan(ys)
    ys = y;
  end
  if isscalar(zs) && isnan(zs)
    zs = z;
  end
end
%=============================================================================
function h = drawContourSlices(parent, x, y, z, V, xs, ys, zs, levels, method)
  if ~isAxisAlignedSlices(xs, ys, zs)
    h = drawContourSurface(parent, x, y, z, V, xs, ys, zs, levels);
    return
  end
  h = gobjects(0, 1);
  for k = 1:numel(xs)
    [Xq, Yq, Zq] = xSliceGridLocal(x, y, z, xs(k));
    C = interp3(x, y, z, V, Xq, Yq, Zq, method, NaN);
    h = [h; contourPatches(parent, y, z, C, levels, @(a, b) deal(xs(k) + 0 * a, a, b))];
  end
  for k = 1:numel(ys)
    [Xq, Yq, Zq] = ySliceGridLocal(x, y, z, ys(k));
    C = interp3(x, y, z, V, Xq, Yq, Zq, method, NaN);
    h = [h; contourPatches(parent, x, z, C, levels, @(a, b) deal(a, ys(k) + 0 * a, b))];
  end
  for k = 1:numel(zs)
    [Xq, Yq, Zq] = zSliceGridLocal(x, y, z, zs(k));
    C = interp3(x, y, z, V, Xq, Yq, Zq, method, NaN);
    h = [h; contourPatches(parent, x, y, C, levels, @(a, b) deal(b, a, zs(k) + 0 * a))];
  end
end
%=============================================================================
function tf = isAxisAlignedSlices(xs, ys, zs)
  tf = min(size(xs)) <= 1 && min(size(ys)) <= 1 && min(size(zs)) <= 1;
end
%=============================================================================
function h = drawContourSurface(parent, x, y, z, V, xs, ys, zs, levels)
  C = interp3(x, y, z, V, xs, ys, zs, 'linear', NaN);
  if all(isnan(C(:)))
    h = gobjects(0, 1);
    return
  end
  if isempty(levels)
    M = contourc(C);
  else
    M = contourc(C, levels);
  end
  h = gobjects(0, 1);
  idx = 1;
  while idx < size(M, 2)
    level = M(1, idx);
    n = M(2, idx);
    pts = normalizeContourPoints(M(:, idx + 1:idx + n));
    if size(pts, 2) < 2
      idx = idx + n + 1;
      continue
    end
    xx = interp2(xs, pts(1, :), pts(2, :));
    yy = interp2(ys, pts(1, :), pts(2, :));
    zz = interp2(zs, pts(1, :), pts(2, :));
    cdata = level + zeros(size(xx));
    h(end + 1, 1) = patch(parent, 'XData', [xx(:); NaN], 'YData', [yy(:); NaN], ...
      'ZData', [zz(:); NaN], 'CData', [cdata(:); NaN], ...
      'FaceVertexCData', zeros(numel(xx) + 1, 3), 'FaceColor', 'none', ...
      'EdgeColor', 'flat', 'UserData', level, 'Tag', 'contourslice');
    idx = idx + n + 1;
  end
end
%=============================================================================
function h = contourPatches(parent, x, y, C, levels, mapper)
  if all(isnan(C(:)))
    h = gobjects(0, 1);
    return
  end
  if isempty(levels)
    M = contourc(x, y, C);
  else
    M = contourc(x, y, C, levels);
  end
  h = gobjects(0, 1);
  idx = 1;
  while idx < size(M, 2)
    level = M(1, idx);
    n = M(2, idx);
    pts = normalizeContourPoints(M(:, idx + 1:idx + n));
    if size(pts, 2) < 2
      idx = idx + n + 1;
      continue
    end
    [xx, yy, zz] = mapper(pts(1, :), pts(2, :));
    cdata = level + zeros(size(xx));
    xdata = [xx(:); NaN];
    ydata = [yy(:); NaN];
    zdata = [zz(:); NaN];
    cdata = [cdata(:); NaN];
    faceVertexCData = zeros(numel(xdata), 3);
    h(end + 1, 1) = patch(parent, 'XData', xdata, 'YData', ydata, 'ZData', zdata, ...
      'CData', cdata, 'FaceVertexCData', faceVertexCData, 'FaceColor', 'none', ...
      'EdgeColor', 'flat', 'UserData', level, 'Tag', 'contourslice');
    idx = idx + n + 1;
  end
end
%=============================================================================
function pts = normalizeContourPoints(pts)
  if isempty(pts)
    return
  end
  keep = [true, any(abs(diff(pts, 1, 2)) > 0, 1)];
  pts = pts(:, keep);
  if size(pts, 2) > 1
    pts = pts(:, end:-1:1);
  end
end
%=============================================================================
function applyContourColors(ax, h)
  if isempty(h)
    return
  end
  levels = zeros(numel(h), 1);
  for k = 1:numel(h)
    levels(k) = h(k).UserData;
  end
  finiteLevels = levels(isfinite(levels));
  if isempty(finiteLevels)
    return
  end
  limits = [min(finiteLevels), max(finiteLevels)];
  if limits(1) == limits(2)
    limits = limits + [-0.5, 0.5];
  end
  ax.CLim = limits;
  colors = colormap(ax);
  if isempty(colors)
    colors = jet(64);
  end
  for k = 1:numel(h)
    color = contourLevelColor(h(k).UserData, limits, colors);
    h(k).FaceVertexCData = repmat(color, length(h(k).XData), 1);
  end
end
%=============================================================================
function color = contourLevelColor(level, limits, colors)
  if ~isfinite(level)
    color = colors(1, :);
    return
  end
  nbColors = size(colors, 1);
  idx = 1 + round((level - limits(1)) / (limits(2) - limits(1)) * (nbColors - 1));
  idx = max(1, min(nbColors, idx));
  color = colors(idx, :);
end
%=============================================================================
function applyContourAxesLimits(ax, h)
  if isempty(h)
    return
  end
  [xb, yb, zb] = contourSliceDrawnExtent(h);
  step = contourSliceCommonStep(xb, yb, zb);
  xlim = snapLimitsToStep(xb, step);
  ylim = snapLimitsToStep(yb, step);
  zlim = snapLimitsToStep(zb, step);
  if ~isempty(xlim)
    ax.XLim = xlim;
  end
  if ~isempty(ylim)
    ax.YLim = ylim;
  end
  if ~isempty(zlim)
    ax.ZLim = zlim;
  end
  applyContourSliceTicks(ax, xlim, ylim, zlim, step);
end
%=============================================================================
function [xb, yb, zb] = contourSliceDrawnExtent(h)
  allx = [];
  ally = [];
  allz = [];
  for k = 1:numel(h)
    allx = [allx; h(k).XData(:)];
    ally = [ally; h(k).YData(:)];
    allz = [allz; h(k).ZData(:)];
  end
  xb = finiteBounds(allx);
  yb = finiteBounds(ally);
  zb = finiteBounds(allz);
end
%=============================================================================
function bounds = finiteBounds(values)
  values = values(isfinite(values));
  if isempty(values)
    bounds = [];
    return
  end
  bounds = [min(values), max(values)];
end
%=============================================================================
function step = contourSliceCommonStep(xb, yb, zb)
  spans = [];
  if ~isempty(xb)
    spans(end + 1) = xb(2) - xb(1);
  end
  if ~isempty(yb)
    spans(end + 1) = yb(2) - yb(1);
  end
  if ~isempty(zb)
    spans(end + 1) = zb(2) - zb(1);
  end
  maxSpan = max(spans);
  if isempty(maxSpan) || ~isfinite(maxSpan) || maxSpan <= 0
    step = contourSliceNiceStep(1);
    return
  end
  step = contourSliceNiceStep(maxSpan / 6);
end
%=============================================================================
function limits = snapLimitsToStep(bounds, step)
  if isempty(bounds)
    limits = [];
    return
  end
  limits = [floor(bounds(1) / step) * step, ceil(bounds(2) / step) * step];
  if limits(1) == limits(2)
    limits = limits + [-step, step];
  end
end
%=============================================================================
function applyContourSliceTicks(ax, xlim, ylim, zlim, step)
  if strcmp(ax.XTickMode, 'auto')
    ticks = contourSliceStepTicks(xlim, step);
    if ~isempty(ticks)
      ax.XTick = ticks;
    end
  end
  if strcmp(ax.YTickMode, 'auto')
    ticks = contourSliceStepTicks(ylim, step);
    if ~isempty(ticks)
      ax.YTick = ticks;
    end
  end
  if strcmp(ax.ZTickMode, 'auto')
    ticks = contourSliceStepTicks(zlim, step);
    if ~isempty(ticks)
      ax.ZTick = ticks;
    end
  end
end
%=============================================================================
function ticks = contourSliceStepTicks(limits, step)
  ticks = [];
  if numel(limits) < 2 || ~isfinite(step) || step <= 0
    return
  end
  first = ceil(limits(1) / step) * step;
  last = floor(limits(2) / step) * step;
  ticks = first:step:last;
  ticks(abs(ticks) < step * 1e-10) = 0;
end
%=============================================================================
function step = contourSliceNiceStep(value)
  if ~isfinite(value) || value <= 0
    step = 1;
    return
  end
  scale = 10 ^ floor(log10(value));
  normalized = value / scale;
  if normalized < sqrt(2)
    nice = 1;
  elseif normalized < sqrt(10)
    nice = 2;
  elseif normalized <= 5.8
    nice = 5;
  else
    nice = 10;
  end
  step = nice * scale;
end
%=============================================================================
function [Xq, Yq, Zq] = xSliceGridLocal(x, y, z, value)
  [Yq, Zq] = meshgrid(y, z);
  Xq = value * ones(size(Yq));
end
%=============================================================================
function [Xq, Yq, Zq] = ySliceGridLocal(x, y, z, value)
  [Xq, Zq] = meshgrid(x, z);
  Yq = value * ones(size(Xq));
end
%=============================================================================
function [Xq, Yq, Zq] = zSliceGridLocal(x, y, z, value)
  [Xq, Yq] = meshgrid(x, y);
  Zq = value * ones(size(Xq));
end
%=============================================================================
