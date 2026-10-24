%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = compass(varargin)
  narginchk(1, 1000);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [parent, args] = graphicsParseParent(args);
  [u, v, lineSpec, colorSpec, markerSpec, propertiesList] = parseCompassArgs(args);
  % Each vector is drawn as a single arrow (shaft plus a two-segment head)
  % from the origin, scaled by the complex value it represents.
  arrow = [0; 1; 0.8 + 0.08i; 1; 0.8 - 0.08i];
  z = u + v .* 1i;
  a = arrow * z;
  if isempty(parent)
    ax = newplot();
  else
    ax = newplot(parent);
  end
  holdState = ishold(ax);
  maxr = max(abs(z));
  if isempty(maxr) || ~isfinite(maxr) || maxr <= 0
    maxr = 1;
  end
  if ~holdState
    hold(ax, 'on');
    drawCompassGrid(ax, maxr);
  end
  h = plot(ax, real(a), imag(a), lineSpec, propertiesList{:});
  if ~holdState
    set(ax, 'DataAspectRatio', [1 1 1]);
    axis(ax, 'off');
    hold(ax, 'off');
  end
  if isempty(colorSpec) && ~hasPropertyName(propertiesList, 'Color')
    colorOrder = get(ax, 'ColorOrder');
    if ~isempty(colorOrder)
      for k = 1:numel(h)
        h(k).Color = colorOrder(1, :);
      end
    end
  end
  if isempty(markerSpec) && ~hasPropertyName(propertiesList, 'Marker')
    for k = 1:numel(h)
      h(k).Marker = 'none';
    end
  end
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function [u, v, lineSpec, colorSpec, markerSpec, propertiesList] = parseCompassArgs(args)
  if isempty(args)
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Value');
  end
  if numel(args) >= 2 && isnumeric(args{2})
    u = args{1};
    v = args{2};
    args = args(3:end);
  else
    if ~isnumeric(args{1})
      error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Value');
    end
    z = args{1};
    u = real(z);
    v = imag(z);
    args = args(2:end);
  end
  if numel(u) ~= numel(v)
    error('Nelson:dimagree:valueValueNumberElements', '%s and %s must have the same number of elements.', 'U', 'V');
  end
  if ~(isnumeric(u) && isnumeric(v))
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Value');
  end
  u = double(u(:).');
  v = double(v(:).');
  [lineSpec, colorSpec, markerSpec, propertiesList] = parseCompassLineSpec(args);
end
%=============================================================================
function [lineSpec, colorSpec, markerSpec, propertiesList] = parseCompassLineSpec(args)
  lineSpec = '-';
  colorSpec = '';
  markerSpec = '';
  if ~isempty(args) && (ischar(args{1}) || isStringScalar(args{1}))
    [~, colorSpec, markerSpec, msg] = colstyle(args{1}, 'plot', false);
    if isempty(msg)
      lineSpec = args{1};
      args = args(2:end);
    else
      colorSpec = '';
      markerSpec = '';
    end
  end
  if mod(numel(args), 2) ~= 0
    error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  propertiesList = args;
end
%=============================================================================
function drawCompassGrid(ax, dataMaxRadius)
  gridColor = [0.85 0.85 0.85];
  % Let the axes auto-scale a spanning segment to pick the radial tick step,
  % then reproduce the reference's radial grid layout from it.
  probe = line(ax, 'XData', [-dataMaxRadius dataMaxRadius NaN 0 0], ...
    'YData', [0 0 NaN -dataMaxRadius dataMaxRadius], ...
    'Color', gridColor, 'HandleVisibility', 'off');
  yLimits = get(ax, 'YLim');
  yTicks = get(ax, 'YTick');
  delete(probe);
  radiusMin = 0;
  radiusMax = yLimits(2);
  radiusTicks = max(yTicks);
  if radiusTicks > 5
    if rem(radiusTicks, 2) == 0
      radiusTicks = radiusTicks / 2;
    elseif rem(radiusTicks, 3) == 0
      radiusTicks = radiusTicks / 3;
    end
  end
  if ~isfinite(radiusTicks) || radiusTicks <= 0
    radiusTicks = 1;
  end
  if ~isfinite(radiusMax) || radiusMax <= 0
    radiusMax = dataMaxRadius;
  end
  % Unit circle, with the four axis-crossing points forced exactly onto the axes.
  th = 0:pi / 50:2 * pi;
  xUnit = cos(th);
  yUnit = sin(th);
  crossings = 1:(numel(th) - 1) / 4:numel(th);
  xUnit(crossings(2:2:4)) = 0;
  yUnit(crossings(1:2:5)) = 0;
  radiusStep = (radiusMax - radiusMin) / radiusTicks;
  for radius = (radiusMin + radiusStep):radiusStep:radiusMax
    line(ax, 'XData', xUnit * radius, 'YData', yUnit * radius, ...
      'LineStyle', '-', 'Color', gridColor, 'LineWidth', 1, 'HandleVisibility', 'off');
    text(ax, 0, radius + radiusStep / 20, ['  ' num2str(radius)], ...
      'VerticalAlignment', 'bottom', 'HandleVisibility', 'off');
  end
  % Radial spokes every 30 degrees, drawn as full diameters.
  spokeAngles = (1:6) * 2 * pi / 12;
  cosAngles = cos(spokeAngles);
  sinAngles = sin(spokeAngles);
  for k = 1:numel(spokeAngles)
    line(ax, 'XData', [-cosAngles(k) cosAngles(k)] * radiusMax, ...
      'YData', [-sinAngles(k) sinAngles(k)] * radiusMax, ...
      'LineStyle', '-', 'Color', gridColor, 'LineWidth', 1, 'HandleVisibility', 'off');
  end
  % Degree annotations around the outer rim.
  labelRadius = 1.1 * radiusMax;
  for k = 1:numel(spokeAngles)
    text(ax, labelRadius * cosAngles(k), labelRadius * sinAngles(k), int2str(k * 30), ...
      'HorizontalAlignment', 'center', 'HandleVisibility', 'off');
    if k == numel(spokeAngles)
      oppositeLabel = int2str(0);
    else
      oppositeLabel = int2str(180 + k * 30);
    end
    text(ax, -labelRadius * cosAngles(k), -labelRadius * sinAngles(k), oppositeLabel, ...
      'HorizontalAlignment', 'center', 'HandleVisibility', 'off');
  end
  view(ax, 2);
  axis(ax, radiusMax * [-1 1 -1.15 1.15]);
  xTicks = ceil(-radiusMax):floor(radiusMax);
  if ~isempty(xTicks)
    ax.XTick = xTicks;
  end
end
%=============================================================================
function tf = hasPropertyName(propertiesList, name)
  tf = false;
  for k = 1:2:numel(propertiesList)
    propertyName = propertiesList{k};
    if (ischar(propertyName) || isStringScalar(propertyName)) && strcmpi(propertyName, name)
      tf = true;
      return
    end
  end
end
%=============================================================================
