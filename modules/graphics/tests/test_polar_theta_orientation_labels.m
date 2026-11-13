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
% Theta labels, spokes and r labels follow ThetaZeroLocation and ThetaDir when
% they are set together with ThetaTick and ThetaTickLabel (24 h clock case).
%=============================================================================
locations = {'right', 'top', 'left', 'bottom'};
zeroAngles = [0, 90, 180, 270];
directions = {'counterclockwise', 'clockwise'};
dirSigns = [1, -1];
% Auto RAxisLocation of the reference for each orientation (labels stay at 80 degrees on screen).
expectedRAxis = [80 280; 350 10; 260 100; 170 190];
hours = {'0h', '3h', '6h', '9h', '12h', '15h', '18h', '21h'};
for i = 1:numel(locations)
  for j = 1:numel(directions)
    f = figure('Visible', 'off');
    pax = polaraxes(f);
    set(pax, 'ThetaZeroLocation', locations{i}, 'ThetaDir', directions{j}, ...
      'ThetaTick', 0:45:315, 'ThetaTickLabel', hours, 'RLim', [0 3]);
    hold(pax, 'on');
    polarplot(pax, linspace(0, pi / 2, 50), 2 * ones(1, 50), 'LineWidth', 8);
    assert_isequal(pax.RAxisLocation, expectedRAxis(i, j));
    assert_isequal(pax.RAxisLocationMode, 'auto');
    for k = 1:numel(hours)
      expected = zeroAngles(i) + dirSigns(j) * 45 * (k - 1);
      assert_istrue(screenAngleClose(labelAngle(pax, hours{k}), expected), ...
        sprintf('%s %s %s', locations{i}, directions{j}, hours{k}));
    end
    % r labels sit at 80 degrees on screen, inside the outer ring.
    for r = {'1', '2', '3'}
      assert_istrue(screenAngleClose(labelAngle(pax, r{1}), 80));
      assert_isapprox(labelRadius(pax, r{1}), str2double(r{1}), 1e-10);
    end
    close(f);
  end
end
%=============================================================================
% A manual RAxisLocation keeps its screen angle when the orientation changes.
f = figure('Visible', 'off');
pax = polaraxes(f);
pax.RAxisLocation = 30;
assert_isequal(pax.RAxisLocationMode, 'manual');
pax.ThetaZeroLocation = 'left';
assert_isapprox(pax.RAxisLocation, 210, 1e-10);
pax.ThetaDir = 'clockwise';
assert_isapprox(pax.RAxisLocation, 150, 1e-10);
pax.ThetaZeroLocation = 'top';
assert_isapprox(pax.RAxisLocation, 60, 1e-10);
assert_istrue(screenAngleClose(labelAngle(pax, '1'), 30));
pax.RAxisLocationMode = 'auto';
assert_isapprox(pax.RAxisLocation, 10, 1e-10);
pax.RAxisLocation = 400;
assert_isapprox(pax.RAxisLocation, 40, 1e-10);
pax.RAxisLocation = -30;
assert_isapprox(pax.RAxisLocation, 330, 1e-10);
close(f);
%=============================================================================
% RAxisLocation follows ThetaAxisUnits.
f = figure('Visible', 'off');
pax = polaraxes(f);
pax.ThetaAxisUnits = 'radians';
assert_isapprox(pax.RAxisLocation, 80 * pi / 180, 1e-12);
assert_istrue(screenAngleClose(labelAngle(pax, '0.2'), 80));
close(f);
%=============================================================================
% A dot-set of ThetaTick rebuilds the labels and the spokes in the orientation.
f = figure('Visible', 'off');
pax = polaraxes(f);
pax.ThetaZeroLocation = 'top';
pax.ThetaDir = 'clockwise';
pax.ThetaTick = 0:90:270;
deg = char(176);
labels = polarThetaLabelStrings(pax);
assert_isequal(sort(labels), sort({['0', deg], ['90', deg], ['180', deg], ['270', deg]}));
assert_istrue(screenAngleClose(labelAngle(pax, ['0', deg]), 90));
assert_istrue(screenAngleClose(labelAngle(pax, ['90', deg]), 0));
pax.ThetaTickLabel = {'N', 'E', 'S', 'W'};
assert_istrue(screenAngleClose(labelAngle(pax, 'N'), 90));
assert_istrue(screenAngleClose(labelAngle(pax, 'E'), 0));
assert_istrue(screenAngleClose(labelAngle(pax, 'S'), 270));
close(f);
%=============================================================================
function angle = labelAngle(pax, str)
  t = findall(pax, 'Type', 'text', 'Tag', '__nelson_polar_grid__', 'String', str);
  assert_isequal(numel(t), 1);
  p = t.Position;
  angle = atan2(p(2), p(1)) * 180 / pi;
end
%=============================================================================
function r = labelRadius(pax, str)
  t = findall(pax, 'Type', 'text', 'Tag', '__nelson_polar_grid__', 'String', str);
  p = t(1).Position;
  r = hypot(p(1), p(2));
end
%=============================================================================
function labels = polarThetaLabelStrings(pax)
  t = findall(pax, 'Type', 'text', 'Tag', '__nelson_polar_grid__');
  labels = {};
  for k = 1:numel(t)
    p = t(k).Position;
    if hypot(p(1), p(2)) > 1.01
      labels{end + 1} = t(k).String;
    end
  end
end
%=============================================================================
function tf = screenAngleClose(a, b)
  d = mod(a - b + 180, 360) - 180;
  tf = abs(d) < 1e-6;
end
%=============================================================================
