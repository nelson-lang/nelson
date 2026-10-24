%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = surfl(varargin)
  narginchk(1, 1000);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [ax, args] = parseSurfaceParent(args);
  [args, useLight] = parseSurflMode(args);
  [surfaceArgs, lightSpec, coefficients] = parseSurflData(args);
  % The light and reflectance geometry depend on the view direction, so the
  % 3-D view must be established before it is computed.
  if ~strcmp(ax.NextPlot, 'add')
    view(ax, 3);
  end
  lightVector = resolveLightVector(lightSpec, ax);
  if useLight
    s = surf(ax, surfaceArgs{:}, 'AmbientStrength', coefficients(1), ...
      'DiffuseStrength', coefficients(2), 'SpecularStrength', coefficients(3), ...
      'SpecularExponent', coefficients(4));
    s.FaceLighting = 'flat';
    l = light(ax, 'Position', lightVector, 'Color', [1 1 1], 'Style', 'infinite');
    h = [s; l];
  else
    s = surf(ax, surfaceArgs{:});
    if strcmp(s.CDataMode, 'auto')
      s.CData = surfaceReflectance(ax, s, lightVector, coefficients);
    end
    clim(ax, [0 1]);
    h = s;
  end
  view(ax, 3);
  grid(ax, 'on');
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function [args, useLight] = parseSurflMode(args)
  useLight = false;
  if ~isempty(args) && ischar(args{end}) && strcmpi(args{end}, 'light')
    useLight = true;
    args = args(1:end - 1);
  end
end
%=============================================================================
function [ax, args] = parseSurfaceParent(args)
  if ~isempty(args) && isscalar(args{1}) && isgraphics(args{1}, 'axes')
    ax = newplot(args{1});
    args = args(2:end);
  else
    [parent, args] = graphicsExtractParentProperty(args);
    ax = newplotTarget(parent);
  end
end
%=============================================================================
function [surfaceArgs, lightSpec, coefficients] = parseSurflData(args)
  coefficients = [0.55 0.6 0.4 10];
  lightSpec = [];
  surfaceArgs = args;
  if numel(args) == 2 && isnumeric(args{2})
    lightSpec = args{2};
    surfaceArgs = args(1);
  elseif numel(args) == 4 && isnumeric(args{4})
    lightSpec = args{4};
    surfaceArgs = args(1:3);
  elseif numel(args) == 5 && isnumeric(args{5})
    lightSpec = args{4};
    coefficients = validateLightingCoefficients(args{5});
    surfaceArgs = args(1:3);
  end
end
%=============================================================================
function vector = resolveLightVector(lightSpec, ax)
  if isempty(lightSpec)
    vector = defaultLightVector(ax);
  else
    vector = lightVectorFromInput(lightSpec);
  end
end
%=============================================================================
function coefficients = validateLightingCoefficients(coefficients)
  if ~isnumeric(coefficients) || numel(coefficients) ~= 4
    error('Nelson:surfl:invalidLightingCoefficients', 'Lighting coefficients must have four elements.');
  end
  coefficients = double(coefficients(:).');
end
%=============================================================================
function vector = lightVectorFromInput(value)
  if ~isnumeric(value) || ~(numel(value) == 2 || numel(value) == 3)
    error('Nelson:surfl:invalidLightDirection', 'Light direction must have two or three elements.');
  end
  value = double(value(:).');
  if numel(value) == 2
    az = value(1) * pi / 180;
    el = value(2) * pi / 180;
    vector = [sin(az) * cos(el), -cos(az) * cos(el), sin(el)];
  else
    vector = value;
  end
end
%=============================================================================
function vector = defaultLightVector(ax)
  viewAngles = ax.View * pi / 180;
  az = viewAngles(1);
  el = viewAngles(2);
  phi = 45 * pi / 180;
  vector = [cos(az) * sin(phi) + sin(az) * cos(el) * cos(phi), ...
    sin(phi) * sin(az) - cos(az) * cos(el) * cos(phi), ...
    sin(phi) * sin(el)];
end
%=============================================================================
function cdata = surfaceReflectance(ax, h, lightVector, coefficients)
  % Reflectance-based shading: combine ambient, Lambertian (diffuse) and
  % specular terms evaluated from the surface normals, the light direction
  % and the viewing direction. This shades the surface as if lit, which is
  % the defining behaviour of surfl.
  [x, y, z] = surfaceGrid(h);
  % Scale the coordinates to a cube-like domain so the normals are not
  % dominated by the largest axis span before they are computed.
  limits = [ax.XLim, ax.YLim, ax.ZLim];
  sx = limits(2) - limits(1);
  sy = limits(4) - limits(3);
  sz = limits(6) - limits(5);
  scale = max([sx, sy, sz]);
  if scale > 0
    sx = nonZeroScale(sx / scale);
    sy = nonZeroScale(sy / scale);
    sz = nonZeroScale(sz / scale);
  else
    sx = 1; sy = 1; sz = 1;
  end
  [nx, ny, nz] = surfnorm(x ./ sx, y ./ sy, z ./ sz);
  s = lightVector(:).' / norm(lightVector);
  viewAngles = ax.View;
  ambient = coefficients(1);
  diffuseWeight = coefficients(2);
  specularWeight = coefficients(3);
  spread = coefficients(4);
  reflectance = ambient ...
    + diffuseWeight .* diffuseReflectance(nx, ny, nz, s) ...
    + specularWeight .* specularReflectance(nx, ny, nz, s, viewAngles, spread);
  cdata = reflectance ./ sum(coefficients(1:3));
end
%=============================================================================
function value = nonZeroScale(value)
  if value == 0
    value = 1;
  end
end
%=============================================================================
function [x, y, z] = surfaceGrid(h)
  z = double(h.ZData);
  x = double(h.XData);
  y = double(h.YData);
  if isvector(x) && isvector(y) && ~isvector(z)
    [x, y] = meshgrid(x, y);
  end
end
%=============================================================================
function r = diffuseReflectance(nx, ny, nz, s)
  s = s(:).' / norm(s);
  magnitude = sqrt(nx .* nx + ny .* ny + nz .* nz);
  magnitude(magnitude == 0) = eps;
  r = max(0, (s(1) .* nx + s(2) .* ny + s(3) .* nz) ./ magnitude);
end
%=============================================================================
function r = specularReflectance(nx, ny, nz, s, v, spread)
  s = s(:) / norm(s);
  if numel(v) == 2
    az = v(1) * pi / 180;
    el = v(2) * pi / 180;
    v = [sin(az) * cos(el); -cos(az) * cos(el); sin(el)];
  else
    v = v(:) / norm(v);
  end
  magnitude = nx .* nx + ny .* ny + nz .* nz;
  magnitude(magnitude == 0) = eps;
  sDotN = s(1) .* nx + s(2) .* ny + s(3) .* nz;
  vDotN = v(1) .* nx + v(2) .* ny + v(3) .* nz;
  r = max(0, 2 .* (sDotN .* vDotN) ./ magnitude - (v.' * s));
  r = r .^ spread;
end
%=============================================================================
