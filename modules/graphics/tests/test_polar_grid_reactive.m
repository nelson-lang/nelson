%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% Grid properties of a polaraxes must be reactive: a dot-set rebuilds the grid
% and the plotted data stays aligned with it (same angle convention in C++ and
% in the .m grid renderer).
%=============================================================================
f = figure('Visible', 'off');
ax = polaraxes('Parent', f);
% A green line from the center out along theta = 0, so it is easy to find in
% the rendered display list.
h = polarplot(ax, [0 0], [0 1]);
set(h, 'Color', [0 1 0]);
drawnow();
%=============================================================================
% Default orientation: theta = 0 points to the right and lies on a grid spoke.
tip = polarDataTipAngle(f);
assert_isapprox(tip, 0, 1e-3);
assert_istrue(polarHasSpokeAt(ax, tip));
%=============================================================================
% ThetaZeroLocation = 'top': theta = 0 now points up, data and grid together.
ax.ThetaZeroLocation = 'top';
drawnow();
tip = polarDataTipAngle(f);
assert_isapprox(tip, pi / 2, 1e-3);
assert_istrue(polarHasSpokeAt(ax, tip));
%=============================================================================
% ThetaZeroLocation = 'left': theta = 0 points left.
ax.ThetaZeroLocation = 'left';
drawnow();
tip = polarDataTipAngle(f);
assert_istrue(polarAngleClose(tip, pi, 1e-3));
assert_istrue(polarHasSpokeAt(ax, tip));
%=============================================================================
% ThetaDir = 'clockwise': a theta = pi/2 point flips from up to down.
ax.ThetaZeroLocation = 'right';
h2 = polarplot(ax, [0 pi / 2], [0 1]);
set(h, 'Color', [1 1 1]);   % hide the first line so only the second is green
set(h2, 'Color', [0 1 0]);
drawnow();
assert_isapprox(polarDataTipAngle(f), pi / 2, 1e-3);
ax.ThetaDir = 'clockwise';
drawnow();
assert_isapprox(polarDataTipAngle(f), -pi / 2, 1e-3);
%=============================================================================
% RColor drives the radial ring color (axis color blended toward white).
ax.ThetaDir = 'counterclockwise';
ax.RColor = [1 0 0];
drawnow();
assert_istrue(polarHasLineColor(ax, [1 0.7128 0.7128], 1e-2));
%=============================================================================
% RGrid = 'off' hides the radial rings but keeps the tick labels.
ax.RGrid = 'off';
drawnow();
[nOff, nLabels] = polarRingVisibility(ax);
assert_istrue(nOff >= 1);
assert_istrue(nLabels >= 1);
close(f);
%=============================================================================
% RDir = 'reverse' mirrors the radial axis: the data endpoints swap and the
% radial tick labels move the same way, so data and grid stay aligned.
f = figure('Visible', 'off');
ax = polaraxes('Parent', f);
h = polarplot(ax, [0 0], [0 1]);
set(h, 'Color', [0 1 0]);
drawnow();
ptsNormal = polarGreenEndpoints(f);
assert_isapprox(polarLabelRadius(ax, '0.6'), 0.6, 1e-6);
ax.RDir = 'reverse';
drawnow();
ptsReverse = polarGreenEndpoints(f);
% The r=0 end (first point) and r=1 end (second point) exchange positions.
assert_istrue(norm(ptsReverse(1:2) - ptsNormal(3:4)) < 2);
assert_istrue(norm(ptsReverse(3:4) - ptsNormal(1:2)) < 2);
assert_isapprox(polarLabelRadius(ax, '0.6'), 0.4, 1e-6);
close(f);
%=============================================================================
% RAxisLocation places the radial tick labels at the given theta angle.
f = figure('Visible', 'off');
ax = polaraxes('Parent', f);
polarplot(ax, [0 pi / 2], [1 1]);
drawnow();
assert_isapprox(polarLabelAngleDeg(ax, '0.6'), 80, 1e-3);
ax.RAxisLocation = 0;
drawnow();
assert_isapprox(polarLabelAngleDeg(ax, '0.6'), 0, 1e-3);
ax.RAxisLocation = 45;
drawnow();
assert_isapprox(polarLabelAngleDeg(ax, '0.6'), 45, 1e-3);
close(f);
%=============================================================================
% RMinorGrid / ThetaMinorGrid add lighter grid lines on top of the major ones.
f = figure('Visible', 'off');
ax = polaraxes('Parent', f);
polarplot(ax, [0 pi / 2], [1 1]);
drawnow();
nBase = polarVisibleLineCount(ax);
ax.RMinorGrid = 'on';
drawnow();
assert_istrue(polarVisibleLineCount(ax) > nBase);
assert_istrue(polarHasLighterLine(ax));
nAfterR = polarVisibleLineCount(ax);
ax.ThetaMinorGrid = 'on';
drawnow();
assert_istrue(polarVisibleLineCount(ax) > nAfterR);
close(f);
%=============================================================================
% RMinorTick / ThetaMinorTick add short tick strokes in the axis color and are
% reactive: turning them off removes the strokes again.
f = figure('Visible', 'off');
ax = polaraxes('Parent', f);
polarplot(ax, [0 pi / 2], [1 1]);
drawnow();
nBase = polarVisibleLineCount(ax);
ax.RMinorTick = 'on';
drawnow();
assert_istrue(polarVisibleLineCount(ax) > nBase);
assert_istrue(polarHasDarkTickLine(ax));
nAfterR = polarVisibleLineCount(ax);
ax.ThetaMinorTick = 'on';
drawnow();
assert_istrue(polarVisibleLineCount(ax) > nAfterR);
ax.RMinorTick = 'off';
ax.ThetaMinorTick = 'off';
drawnow();
assert_isequal(polarVisibleLineCount(ax), nBase);
close(f);
%=============================================================================
% A partial theta span draws a wedge; its bounding box follows the same
% orientation convention as the grid, so rotating it moves into the matching
% quadrant instead of staying on the raw angle.
f = figure('Visible', 'off');
ax = polaraxes('Parent', f);
polarplot(ax, [0 pi / 4], [1 1]);
drawnow();
ax.ThetaLim = [0 90];
% Each ThetaZeroLocation / ThetaDir change is a real transition so the grid
% rebuild fires (setting a property to its current value is a no-op).
ax.ThetaZeroLocation = 'top';
drawnow();
% Top: the 0..90 wedge is rotated into the second quadrant (x <= 0, y >= 0).
assert_istrue(ax.XLim(2) <= 1e-6 && ax.YLim(1) >= -1e-6);
ax.ThetaZeroLocation = 'right';
drawnow();
% Right: back into the first quadrant (x >= 0, y >= 0).
assert_istrue(ax.XLim(1) >= -1e-6 && ax.YLim(1) >= -1e-6);
assert_istrue(ax.XLim(2) > 0.5 && ax.YLim(2) > 0.5);
ax.ThetaDir = 'clockwise';
drawnow();
% Clockwise: reflected into the fourth quadrant (x >= 0, y <= 0).
assert_istrue(ax.YLim(2) <= 1e-6 && ax.XLim(1) >= -1e-6);
close(f);
%=============================================================================
% Major tick strokes are drawn by default in the axis color: theta ticks reach
% the rim, radial ticks sit on the radial axis. ThetaGrid 'off' hides its ticks.
f = figure('Visible', 'off');
ax = polaraxes('Parent', f);
polarplot(ax, [0 pi / 2], [1 1]);
drawnow();
assert_istrue(polarHasDarkTickLine(ax));
assert_istrue(polarHasRimTick(ax));
nAll = polarVisibleLineCount(ax);
ax.ThetaGrid = 'off';
drawnow();
assert_istrue(polarVisibleLineCount(ax) < nAll);
close(f);
%=============================================================================
function angle = polarDataTipAngle(f)
  % Direction (math convention) of the green data line, read from the render.
  angle = NaN;
  dl = jsondecode(__web_display_list__(f));
  prims = dl.primitives;
  for k = 1:numel(prims)
    if iscell(prims), p = prims{k}; else, p = prims(k); end
    if ~isfield(p, 'kind') || ~strcmp(p.kind, 'polyline'), continue; end
    if ~isfield(p, 'stroke') || isempty(strfind(p.stroke, 'rgba(0,255,0')), continue; end
    xy = typecast(polarBase64Bytes(p.pointsB64), 'single');
    if numel(xy) >= 4
      % Pixel y grows downward; flip it for the math-convention angle.
      angle = atan2(-(double(xy(4)) - double(xy(2))), double(xy(3)) - double(xy(1)));
      return;
    end
  end
end
%=============================================================================
function tf = polarHasSpokeAt(ax, angle)
  tf = false;
  gh = findall(ax, 'Tag', '__nelson_polar_grid__');
  for k = 1:numel(gh)
    if ~isgraphics(gh(k), 'line'), continue; end
    gx = gh(k).XData; gy = gh(k).YData;
    for j = 1:numel(gx) - 1
      if gx(j) == 0 && gy(j) == 0 && isfinite(gx(j + 1)) && isfinite(gy(j + 1)) ...
          && hypot(gx(j + 1), gy(j + 1)) > 1e-6
        if polarAngleClose(atan2(gy(j + 1), gx(j + 1)), angle, 1e-3)
          tf = true;
          return;
        end
      end
    end
  end
end
%=============================================================================
function tf = polarHasLineColor(ax, color, tol)
  tf = false;
  gh = findall(ax, 'Tag', '__nelson_polar_grid__');
  for k = 1:numel(gh)
    if isgraphics(gh(k), 'line') && max(abs(gh(k).Color - color)) < tol
      tf = true;
      return;
    end
  end
end
%=============================================================================
function [nOff, nLabels] = polarRingVisibility(ax)
  nOff = 0;
  nLabels = 0;
  gh = findall(ax, 'Tag', '__nelson_polar_grid__');
  for k = 1:numel(gh)
    if isgraphics(gh(k), 'line')
      if strcmp(char(gh(k).Visible), 'off')
        nOff = nOff + 1;
      end
    elseif isgraphics(gh(k), 'text')
      nLabels = nLabels + 1;
    end
  end
end
%=============================================================================
function pts = polarGreenEndpoints(f)
  pts = [NaN NaN NaN NaN];
  dl = jsondecode(__web_display_list__(f));
  prims = dl.primitives;
  for k = 1:numel(prims)
    if iscell(prims), p = prims{k}; else, p = prims(k); end
    if ~isfield(p, 'kind') || ~strcmp(p.kind, 'polyline'), continue; end
    if ~isfield(p, 'stroke') || isempty(strfind(p.stroke, 'rgba(0,255,0')), continue; end
    xy = typecast(polarBase64Bytes(p.pointsB64), 'single');
    if numel(xy) >= 4
      pts = double(xy(1:4));
      return;
    end
  end
end
%=============================================================================
function r = polarLabelRadius(ax, str)
  r = NaN;
  t = polarFindLabel(ax, str);
  if ~isempty(t)
    pos = t.Position;
    r = hypot(pos(1), pos(2));
  end
end
%=============================================================================
function a = polarLabelAngleDeg(ax, str)
  a = NaN;
  t = polarFindLabel(ax, str);
  if ~isempty(t)
    pos = t.Position;
    a = atan2(pos(2), pos(1)) * 180 / pi;
  end
end
%=============================================================================
function t = polarFindLabel(ax, str)
  t = [];
  gh = findall(ax, 'Tag', '__nelson_polar_grid__');
  for k = 1:numel(gh)
    if isgraphics(gh(k), 'text') && strcmp(strrep(gh(k).String, char(176), ''), str)
      t = gh(k);
      return;
    end
  end
end
%=============================================================================
function n = polarVisibleLineCount(ax)
  n = 0;
  gh = findall(ax, 'Tag', '__nelson_polar_grid__');
  for k = 1:numel(gh)
    if isgraphics(gh(k), 'line') && strcmp(char(gh(k).Visible), 'on')
      n = n + 1;
    end
  end
end
%=============================================================================
function tf = polarHasLighterLine(ax)
  tf = false;
  gh = findall(ax, 'Tag', '__nelson_polar_grid__');
  for k = 1:numel(gh)
    if isgraphics(gh(k), 'line') && min(gh(k).Color) > 0.8
      tf = true;
      return;
    end
  end
end
%=============================================================================
function tf = polarHasDarkTickLine(ax)
  % Tick marks use the axis color, darker than the blended grid lines.
  tf = false;
  gh = findall(ax, 'Tag', '__nelson_polar_grid__');
  for k = 1:numel(gh)
    if isgraphics(gh(k), 'line') && strcmp(char(gh(k).Visible), 'on') ...
        && max(gh(k).Color) < 0.3
      tf = true;
      return;
    end
  end
end
%=============================================================================
function tf = polarHasRimTick(ax)
  % A dark tick line with a vertex at the outer radius (RLim(2)).
  tf = false;
  gh = findall(ax, 'Tag', '__nelson_polar_grid__');
  for k = 1:numel(gh)
    if isgraphics(gh(k), 'line') && strcmp(char(gh(k).Visible), 'on') ...
        && max(gh(k).Color) < 0.3
      rr = hypot(gh(k).XData, gh(k).YData);
      if any(abs(rr - 1) < 1e-6)
        tf = true;
        return;
      end
    end
  end
end
%=============================================================================
function tf = polarAngleClose(a, b, tol)
  d = mod(a - b + pi, 2 * pi) - pi;
  tf = abs(d) < tol;
end
%=============================================================================
function bytes = polarBase64Bytes(s)
  s = char(s);
  alphabet = ['A':'Z', 'a':'z', '0':'9', '+', '/'];
  map = -ones(1, 256);
  map(double(alphabet) + 1) = 0:63;
  s = s(s ~= '=');
  vals = map(double(s) + 1);
  n = numel(vals);
  nq = floor(n / 4);
  rem = n - nq * 4;
  nbytes = nq * 3 + max(rem - 1, 0);
  bytes = zeros(1, nbytes, 'uint8');
  for i = 1:nq
    v = vals((i - 1) * 4 + (1:4));
    acc = v(1) * 262144 + v(2) * 4096 + v(3) * 64 + v(4);
    bytes((i - 1) * 3 + 1) = uint8(floor(acc / 65536));
    bytes((i - 1) * 3 + 2) = uint8(mod(floor(acc / 256), 256));
    bytes((i - 1) * 3 + 3) = uint8(mod(acc, 256));
  end
  if rem == 2
    v = vals(nq * 4 + (1:2));
    bytes(nq * 3 + 1) = uint8(floor((v(1) * 64 + v(2)) / 16));
  elseif rem == 3
    v = vals(nq * 4 + (1:3));
    acc = v(1) * 4096 + v(2) * 64 + v(3);
    bytes(nq * 3 + 1) = uint8(floor(acc / 1024));
    bytes(nq * 3 + 2) = uint8(mod(floor(acc / 4), 256));
  end
end
%=============================================================================
