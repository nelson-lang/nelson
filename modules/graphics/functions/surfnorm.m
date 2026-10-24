%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = surfnorm(varargin)
  narginchk(1, 3);
  nargoutchk(0, 3);

  [x, y, z] = parseSurfnormInputs(varargin);
  [nx, ny, nz] = computeSurfaceNormals(x, y, z);

  if nargout == 0
    plotSurfaceNormals(x, y, z, nx, ny, nz);
  else
    varargout{1} = nx;
    if nargout > 1
      varargout{2} = ny;
    end
    if nargout > 2
      varargout{3} = nz;
    end
  end
end
%=============================================================================
function [x, y, z] = parseSurfnormInputs(args)
  if numel(args) == 1
    z = args{1};
    validateZ(z);
    [x, y] = meshgrid(1:size(z, 2), 1:size(z, 1));
  elseif numel(args) == 3
    x = args{1};
    y = args{2};
    z = args{3};
    validateXYZ(x, y, z);
  else
    error('Nelson:wrongNumberOfInputs:wrongNumberMatrixInputArguments', 'Wrong number of matrix input arguments.');
  end
end
%=============================================================================
function validateZ(z)
  if ~isRealNumericMatrix(z)
    error('Nelson:graphics:mustBeRealMatrix', '%s must be a real numeric matrix.', 'Z');
  end
  if size(z, 1) < 3 || size(z, 2) < 3
    error('Nelson:surfnorm:zTooSmall', 'Z must be at least 3-by-3.');
  end
end
%=============================================================================
function validateXYZ(x, y, z)
  if ~isRealNumericMatrix(x) || ~isRealNumericMatrix(y) || ~isRealNumericMatrix(z)
    error('Nelson:surfnorm:invalidXYAnd', 'X, Y, and Z must be real numeric matrices.');
  end
  if ~isequal(size(x), size(y)) || ~isequal(size(x), size(z))
    error('Nelson:dimagree:yZSizeX', 'Y and Z must be the same size as X.');
  end
  validateZ(z);
end
%=============================================================================
function tf = isRealNumericMatrix(value)
  tf = (isnumeric(value) || islogical(value)) && isreal(value) && ismatrix(value);
end
%=============================================================================
function [nx, ny, nz] = computeSurfaceNormals(x, y, z)
  x = expandForSurfaceNormals(double(x));
  y = expandForSurfaceNormals(double(y));
  z = expandForSurfaceNormals(double(z));

  rows = 2:size(x, 1) - 1;
  cols = 2:size(x, 2) - 1;
  ax = (x(rows, cols - 1) - x(rows, cols + 1)) / 2;
  ay = (y(rows, cols - 1) - y(rows, cols + 1)) / 2;
  az = (z(rows, cols - 1) - z(rows, cols + 1)) / 2;
  bx = (x(rows + 1, cols) - x(rows - 1, cols)) / 2;
  by = (y(rows + 1, cols) - y(rows - 1, cols)) / 2;
  bz = (z(rows + 1, cols) - z(rows - 1, cols)) / 2;

  nx = -(ay .* bz - az .* by);
  ny = -(az .* bx - ax .* bz);
  nz = -(ax .* by - ay .* bx);

  magnitude = sqrt(nx .* nx + ny .* ny + nz .* nz);
  valid = magnitude ~= 0;
  nx(valid) = nx(valid) ./ magnitude(valid);
  ny(valid) = ny(valid) ./ magnitude(valid);
  nz(valid) = nz(valid) ./ magnitude(valid);
end
%=============================================================================
function data = expandForSurfaceNormals(data)
  data = [3 * data(1, :) - 3 * data(2, :) + data(3, :); data; ...
    3 * data(end, :) - 3 * data(end - 1, :) + data(end - 2, :)];
  data = [3 * data(:, 1) - 3 * data(:, 2) + data(:, 3), data, ...
    3 * data(:, end) - 3 * data(:, end - 1) + data(:, end - 2)];
end
%=============================================================================
function plotSurfaceNormals(x, y, z, nx, ny, nz)
  s = surf(x, y, z);
  ax = ancestor(s, 'axes');
  fig = ancestor(ax, 'figure');
  axesNextPlot = ax.NextPlot;
  figureNextPlot = fig.NextPlot;
  hold(ax, 'on');
  plotNormalSegments(ax, x, y, z, nx, ny, nz);
  ax.NextPlot = axesNextPlot;
  fig.NextPlot = figureNextPlot;
end
%=============================================================================
function plotNormalSegments(ax, x, y, z, nx, ny, nz)
  [sx, sy, sz] = scaleNormalsForDisplay(ax, nx, ny, nz);
  xdata = normalSegmentData(x, sx);
  ydata = normalSegmentData(y, sy);
  zdata = normalSegmentData(z, sz);
  line(ax, xdata, ydata, zdata, 'Color', [1 0 0]);
  applySurfnormXLimits(ax, xdata);
end
%=============================================================================
function [sx, sy, sz] = scaleNormalsForDisplay(ax, nx, ny, nz)
  dar = ax.DataAspectRatio;
  scale = 0.6;
  denominator = sqrt((nx * dar(1)) .^ 2 + (ny * dar(2)) .^ 2 + (nz * dar(3)) .^ 2);
  valid = denominator ~= 0;
  sx = zeros(size(nx));
  sy = zeros(size(ny));
  sz = zeros(size(nz));
  sx(valid) = scale * nx(valid) * dar(1) ^ 2 ./ denominator(valid);
  sy(valid) = scale * ny(valid) * dar(2) ^ 2 ./ denominator(valid);
  sz(valid) = scale * nz(valid) * dar(3) ^ 2 ./ denominator(valid);
end
%=============================================================================
function applySurfnormXLimits(ax, x)
  if ~strcmp(ax.XLimMode, 'auto') || ~strcmp(ax.XTickMode, 'auto')
    return
  end
  values = x(isfinite(x));
  if isempty(values)
    return
  end
  limits = [min(values(:)) max(values(:))];
  span = limits(2) - limits(1);
  if ~isfinite(span) || span <= 0
    return
  end
  step = surfnormNiceStep(span / 7);
  startValue = ceil(limits(1) / step) * step;
  stopValue = ceil(limits(2) / step) * step;
  ax.XLim = [startValue stopValue];
  ax.XTick = startValue:step:stopValue;
end
%=============================================================================
function step = surfnormNiceStep(rawStep)
  magnitude = 10 ^ floor(log10(rawStep));
  scaled = rawStep / magnitude;
  if scaled <= 1.2
    multiplier = 1;
  elseif scaled <= 2.5
    multiplier = 2;
  elseif scaled <= 5
    multiplier = 5;
  else
    multiplier = 10;
  end
  step = multiplier * magnitude;
end
%=============================================================================
function data = normalSegmentData(origin, component)
  data = [origin(:).'; origin(:).' + component(:).'; NaN(1, numel(origin))];
  data = data(:).';
end
%=============================================================================
