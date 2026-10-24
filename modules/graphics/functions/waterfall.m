%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = waterfall(varargin)
  % waterfall(X, Y, Z)
  % waterfall(X, Y, Z, C)
  % waterfall(Z)
  % waterfall(Z, C)
  % waterfall(ax, ...)
  % p = waterfall(...)
  narginchk(1, 10000);
  nargoutchk(0, 1);
  inputArguments = convertStringToCharArgs(varargin);
  nbInputArguments = length(inputArguments);
  if (nbInputArguments >= 2) && isscalar(inputArguments{1}) ...
    && (isgraphics(inputArguments{1}, 'axes') || isgraphics(inputArguments{1}, 'hggroup'))
    designatedParent = inputArguments{1}(1);
    inputArguments = inputArguments(2:end);
  else
    % The pair is pulled out because the patch is given a parent of its own
    % below: left in place it would reach patch a second time.
    [designatedParent, inputArguments] = graphicsExtractParentProperty(inputArguments);
  end
  nbInputArguments = length(inputArguments);
  % The curtains are drawn in an axes, so a group designated as target is
  % resolved to the axes holding it, and that axes is the one to start anew.
  if isgraphics(designatedParent, 'hggroup')
    designatedParent = ancestor(designatedParent, 'axes');
  end
  ax = newplotTarget(designatedParent);

  firstString = find(cellfun('isclass', inputArguments, 'char'), 1);
  if (isempty(firstString))
    firstString = nbInputArguments + 1;
  end
  propertiesList = inputArguments(firstString:end);
  inputArguments = inputArguments(1:firstString - 1);

  [X, Y, Z, C] = parseWaterfallData(inputArguments);

  h = buildWaterfallPatch(ax, X, Y, Z, C, propertiesList);
  applyWaterfallAxesState(ax, Z);

  if (~ishold(ax))
    view(ax, 3);
    grid(ax, 'on');
  end

  varargout = {};
  if (nargout > 0)
    varargout{1} = h;
  end
end
%=============================================================================
function applyWaterfallAxesState(ax, Z)
  finiteZ = Z(isfinite(Z));
  if (isempty(finiteZ))
    return
  end
  zMin = min(finiteZ);
  zMax = max(finiteZ);
  if (zMin == zMax)
    zMin = zMin - 1;
    zMax = zMax + 1;
  end
  % The curtain Z axis takes the shared nice-number auto limits so its ticks
  % match what an automatic axis settles on. A tick budget of 6 intervals keeps
  % the curtain depth readable (a handful of coarse z levels) rather than the
  % dense ladder a 10-interval budget would place.
  [zLimits, zTicks] = graphicsNiceAxisLimits(zMin, zMax, 6);
  ax.ZLim = zLimits;
  ax.ZTick = zTicks;
end
%=============================================================================
function [X, Y, Z, C] = parseWaterfallData(inputArguments)
  switch (length(inputArguments))
    case 1
      Z = inputArguments{1};
      [m, n] = size(Z);
      [X, Y] = meshgrid(0:n - 1, 0:m - 1);
      C = Z;
    case 2
      Z = inputArguments{1};
      C = inputArguments{2};
      [m, n] = size(Z);
      [X, Y] = meshgrid(0:n - 1, 0:m - 1);
    case 3
      X = inputArguments{1};
      Y = inputArguments{2};
      Z = inputArguments{3};
      C = Z;
    case 4
      X = inputArguments{1};
      Y = inputArguments{2};
      Z = inputArguments{3};
      C = inputArguments{4};
    otherwise
      error('Nelson:graphics:invalidNameValueArguments', 'Invalid parameter/value pair arguments.');
  end
  % Promote vector coordinates to a full grid matching Z.
  if (isvector(X) && isvector(Y) && ~isvector(Z))
    [X, Y] = meshgrid(X, Y);
  end
end
%=============================================================================
function h = buildWaterfallPatch(ax, X, Y, Z, C, propertiesList)
  X = double(X);
  Y = double(Y);
  Z = double(Z);
  [m, n] = size(Z);
  finiteZ = Z(isfinite(Z));
  if (isempty(finiteZ))
    zBase = 0;
  else
    zBase = min(finiteZ);
  end
  % Each matrix row becomes one curtain polygon (a patch face). The row is
  % drawn left to right along the top, dropped to the baseline at the right
  % edge, returned along the baseline and closed at the left edge. Emitting
  % the fill and its outline as a single polygon per row keeps the row line
  % continuous (a row-only surface mesh fragments because its face and edge
  % quads are depth-sorted independently).
  nv = n + 2;
  patchX = zeros(nv, m);
  patchY = zeros(nv, m);
  patchZ = zeros(nv, m);
  for i = 1:m
    xr = X(i, :);
    yr = Y(i, :);
    zr = Z(i, :);
    patchX(:, i) = [xr, xr(n), xr(1)]';
    patchY(:, i) = [yr, yr(n), yr(1)]';
    patchZ(:, i) = [zr, zBase, zBase]';
  end

  faceVertexCData = waterfallVertexColors(ax, X, Y, Z, C, m, n, nv);

  % Defaults first, caller properties next: a repeated property keeps its last
  % value, so a property given to waterfall wins over the default. FaceColor
  % comes last because the curtain fill stays white whatever the caller asks.
  % EdgeColor is 'interp' so the colour runs along the top row line while the
  % vertical drops and the baseline fade to the white baseline vertices: the
  % plot reads as coloured ribbon lines above light curtains, not solid boxes
  % outlined in a single bright colour.
  args = {'Parent', ax, ...
    'XData', patchX, ...
    'YData', patchY, ...
    'ZData', patchZ, ...
    'FaceVertexCData', faceVertexCData, ...
    'EdgeColor', 'interp'};
  args = [args, propertiesList, {'FaceColor', [1 1 1]}];
  h = patch(args{:});
end
%=============================================================================
function rgb = waterfallVertexColors(ax, X, Y, Z, C, m, n, nv)
  cmap = colormap(ax);
  if (isempty(cmap))
    cmap = parula(256);
  end
  nc = size(cmap, 1);
  if (ndims(C) == 3 && size(C, 3) == 3)
    % True-color: C is m-by-n-by-3 RGB.
    cr = C(:, :, 1);
    cg = C(:, :, 2);
    cb = C(:, :, 3);
    rowRGB = @(i)[cr(i, :)', cg(i, :)', cb(i, :)'];
  else
    C = double(C);
    finiteC = C(isfinite(C));
    if (isempty(finiteC))
      cMin = 0;
      cMax = 1;
    else
      cMin = min(finiteC);
      cMax = max(finiteC);
    end
    if (cMin == cMax)
      cMax = cMin + 1;
    end
    ax.CLim = [cMin, cMax];
    idx = round((C - cMin) / (cMax - cMin) * (nc - 1)) + 1;
    idx(idx < 1) = 1;
    idx(idx > nc) = nc;
    idx(~isfinite(idx)) = 1;
    rowRGB = @(i) cmap(idx(i, :), :);
  end
  rgb = zeros(nv * m, 3);
  baseColor = [1 1 1];
  for i = 1:m
    rc = rowRGB(i);
    % Vertices: n data points carry the row colours, then the two baseline
    % points (right, left) are white. With EdgeColor 'interp' this fades the
    % vertical drops from the row colour down to a white baseline, so the
    % curtains stay light instead of being boxed in a bright edge colour.
    block = [rc; baseColor; baseColor];
    rgb((i - 1) * nv + (1:nv), :) = block;
  end
end
%=============================================================================
