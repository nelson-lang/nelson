%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = qqplot(varargin)
  narginchk(1, 4);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [parent, args] = parseParentLocal(args);
  [x, y, pvec] = parseInputsLocal(args);
  curves = computeCurvesLocal(x, y, pvec);
  h = drawCurvesLocal(parent, curves, isempty(y));
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function [parent, args] = parseParentLocal(args)
  parent = [];
  if ~isempty(args) && isscalar(args{1}) && (isgraphics(args{1}, 'axes') || isgraphics(args{1}, 'hggroup'))
    parent = args{1};
    args = args(2:end);
  end
end
%=============================================================================
function [x, y, pvec] = parseInputsLocal(args)
  if isempty(args)
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end
  if length(args) > 3
    error('Nelson:tooManyInputs', 'Too many input arguments.');
  end
  x = validateDataLocal(args{1}, 'X');
  y = [];
  pvec = [];
  if length(args) >= 2
    second = args{2};
    if isnumeric(second) && isreal(second)
      if length(args) == 2 && isProbabilityVectorLocal(second)
        pvec = validatePvecLocal(second);
      else
        y = validateDataLocal(second, 'Y');
      end
    else
      error('Nelson:statistics:yRealNumericArray', '%s must be a real numeric array.', 'Y');
    end
  end
  if length(args) == 3
    pvec = validatePvecLocal(args{3});
  end
  [x, y] = alignColumnsLocal(x, y);
end
%=============================================================================
function tf = isProbabilityVectorLocal(value)
  tf = isvector(value) && ~isempty(value) && all(isfinite(double(value(:)))) ...
    && all(double(value(:)) >= 0) && all(double(value(:)) <= 100);
end
%=============================================================================
function data = validateDataLocal(data, name)
  if ~(isnumeric(data) && isreal(data) && ~issparse(data) && ~isempty(data))
    error('Nelson:statistics:mustBeRealNumeric', '%s must be a real numeric array.', name);
  end
  if ndims(data) > 2
    error('Nelson:statistics:mustBeVectorOrMatrix', '%s must be a vector or matrix.', name);
  end
  data = double(data);
  if isvector(data)
    data = data(:);
  end
end
%=============================================================================
function pvec = validatePvecLocal(pvec)
  if ~(isnumeric(pvec) && isreal(pvec) && isvector(pvec)) || isempty(pvec) ...
      || any(~isfinite(double(pvec(:)))) || any(double(pvec(:)) < 0) ...
      || any(double(pvec(:)) > 100)
    error('Nelson:statistics:pContainValuesBetween0', 'P must contain values between 0 and 100.');
  end
  pvec = double(pvec(:)) / 100;
end
%=============================================================================
function [x, y] = alignColumnsLocal(x, y)
  if isempty(y)
    return
  end
  nx = size(x, 2);
  ny = size(y, 2);
  if nx == ny
    return
  end
  if nx == 1
    x = repmat(x, 1, ny);
  elseif ny == 1
    y = repmat(y, 1, nx);
  else
    error('Nelson:statistics:xYCompatibleNumbersColumns', 'X and Y must have %s.', 'compatible numbers of columns');
  end
end
%=============================================================================
function curves = computeCurvesLocal(x, y, pvec)
  columns = size(x, 2);
  curves = cell(columns, 1);
  for k = 1:columns
    xk = cleanColumnLocal(x(:, k));
    if isempty(y)
      [xq, yq, p] = normalQuantilesLocal(xk, pvec);
      lineStats = quartileLineLocal(norminv([0.25 0.75], 0, 1), quantile(xk, [0.25 0.75]));
    else
      yk = cleanColumnLocal(y(:, k));
      [xq, yq, p] = sampleQuantilesLocal(xk, yk, pvec);
      lineStats = quartileLineLocal(quantile(xk, [0.25 0.75]), quantile(yk, [0.25 0.75]));
    end
    curves{k} = curveStructLocal(xq, yq, p, lineStats);
  end
end
%=============================================================================
function x = cleanColumnLocal(x)
  x = x(isfinite(x));
  if isempty(x)
    error('Nelson:statistics:dataFinitePerColumn', 'Data must contain at least one finite value per column.');
  end
end
%=============================================================================
function [xq, yq, p] = normalQuantilesLocal(x, pvec)
  if isempty(pvec)
    n = length(x);
    p = ((1:n)' - 0.5) / n;
    yq = sort(x);
  else
    p = pvec;
    yq = quantile(x, p);
    yq = yq(:);
  end
  xq = norminv(p, 0, 1);
end
%=============================================================================
function [xq, yq, p] = sampleQuantilesLocal(x, y, pvec)
  if isempty(pvec)
    n = min(length(x), length(y));
    p = ((1:n)' - 0.5) / n;
  else
    p = pvec;
  end
  xq = quantile(x, p);
  yq = quantile(y, p);
  xq = xq(:);
  yq = yq(:);
end
%=============================================================================
function lineStats = quartileLineLocal(xQuartiles, yQuartiles)
  xQuartiles = xQuartiles(:);
  yQuartiles = yQuartiles(:);
  dx = xQuartiles(2) - xQuartiles(1);
  if dx == 0 || isnan(dx)
    slope = 0;
  else
    slope = (yQuartiles(2) - yQuartiles(1)) / dx;
  end
  intercept = yQuartiles(1) - slope * xQuartiles(1);
  lineStats = struct('Slope', slope, 'Intercept', intercept, ...
    'XQuartiles', xQuartiles, 'YQuartiles', yQuartiles);
end
%=============================================================================
function curve = curveStructLocal(xq, yq, p, lineStats)
  finiteX = xq(isfinite(xq));
  if isempty(finiteX)
    referenceX = lineStats.XQuartiles(:).';
  else
    referenceX = [min(finiteX), max(finiteX)];
  end
  referenceY = lineStats.Intercept + lineStats.Slope * referenceX;
  curve = struct('XData', xq(:), 'YData', yq(:), 'P', p(:), ...
    'QuartileX', lineStats.XQuartiles(:), 'QuartileY', lineStats.YQuartiles(:), ...
    'ReferenceX', referenceX(:), 'ReferenceY', referenceY(:), ...
    'Slope', lineStats.Slope, 'Intercept', lineStats.Intercept);
end
%=============================================================================
function h = drawCurvesLocal(parent, curves, useNormalLabels)
  if isempty(parent)
    parent = newplot();
  elseif isgraphics(parent, 'axes')
    parent = newplot(parent);
  end
  h = [];
  for k = 1:numel(curves)
    c = curves{k};
    h = [h; line(parent, c.XData, c.YData, 'Marker', '+', 'LineStyle', 'none')];
    h = [h; line(parent, c.QuartileX, c.QuartileY, 'LineStyle', '-', 'LineWidth', 1)];
    h = [h; line(parent, c.ReferenceX, c.ReferenceY, 'LineStyle', '--', 'LineWidth', 1)];
  end
  for k = 1:length(h)
    h(k).UserData = curves{ceil(k / 3)};
  end
  labelAxesLocal(parent, useNormalLabels);
end
%=============================================================================
function labelAxesLocal(parent, useNormalLabels)
  ax = parent;
  if isgraphics(parent, 'hggroup')
    ax = ancestor(parent, 'axes');
  end
  if useNormalLabels
    xlabel(ax, 'Standard Normal Quantiles');
    ylabel(ax, 'Quantiles of Input Sample');
    title(ax, 'QQ Plot of Sample Data versus Standard Normal');
  else
    xlabel(ax, 'X Quantiles');
    ylabel(ax, 'Y Quantiles');
  end
end
%=============================================================================
