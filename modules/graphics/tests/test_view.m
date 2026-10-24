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
close all
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 560 420]);
plot(sin(0:0.1:2 * pi))
ax = gca();
asserts.isequal(ax.View, [0 90]);
asserts.isapprox(ax.CameraTarget, [35 0 0], 1e-4)
asserts.isapprox(ax.CameraPosition, expectedCameraPosition(ax), 1e-4);
asserts.isequal(ax.CameraUpVector, [0 1 0]);
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 560 420]);
surf(peaks);
ax = gca();
asserts.isapprox(ax.View, [-37.5000 30.0000], 1e-4);
v = view();
asserts.isapprox(v, [-37.5000 30.0000], 1e-4);
[az, el] = view();
asserts.isapprox(az, -37.5000, 1e-4);
asserts.isapprox(el, 30.0000, 1e-4);
asserts.isapprox(ax.CameraTarget, expectedCameraTarget(ax), 1e-4)
asserts.isapprox(ax.CameraPosition, expectedCameraPosition(ax), 1e-4);
asserts.isequal(ax.CameraUpVector, [0 0 1]);
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 560 420]);
f.ToolBar = 'none';
[X, Y, Z] = peaks();
surf(X, Y, Z);
ax = gca();
asserts.isapprox(ax.View, [-37.5000 30.0000], 1e-4);
asserts.isapprox(ax.CameraTarget, expectedCameraTarget(ax), 1e-4)
asserts.isapprox(ax.CameraPosition, expectedCameraPosition(ax), 1e-4);
asserts.isequal(ax.CameraUpVector, [0 0 1]);
%=============================================================================
view(90, 0)
asserts.isapprox(ax.View, [90 0], 1e-4);
asserts.isapprox(ax.CameraTarget, expectedCameraTarget(ax), 1e-4)
asserts.isapprox(ax.CameraPosition, expectedCameraPosition(ax), 1e-4);
asserts.isequal(ax.CameraUpVector, [0 0 1]);
%=============================================================================
view(2)
asserts.isapprox(ax.View, [0 90], 1e-4);
asserts.isapprox(ax.CameraTarget, expectedCameraTarget(ax), 1e-4)
asserts.isapprox(ax.CameraPosition, expectedCameraPosition(ax), 1e-4);
asserts.isequal(ax.CameraUpVector, [0 1 0]);
%=============================================================================
view([30, 50, 70])
asserts.isapprox(ax.View, [149.0362 50.2059], 1e-4);
asserts.isapprox(ax.CameraTarget, expectedCameraTarget(ax), 1e-4)
asserts.isapprox(ax.CameraPosition, expectedCameraPosition(ax), 1e-4);
asserts.isequal(ax.CameraUpVector, [0 0 1]);
%=============================================================================
close all
%=============================================================================
function cameraPosition = expectedCameraPosition(ax)
  limits = [ax.XLim, ax.YLim, ax.ZLim];
  cameraCenter = [(limits(1) + limits(2)) / 2, (limits(3) + limits(4)) / 2, ...
    (limits(5) + limits(6)) / 2];
  azimuth = ax.View(1);
  elevation = ax.View(2);
  if (elevation > 180)
    elevation = elevation - 360;
  elseif (elevation < -180)
    elevation = elevation + 360;
  end
  if (azimuth > 180)
    azimuth = azimuth - 360;
  elseif (azimuth < -180)
    azimuth = azimuth + 360;
  end

  dar = ax.DataAspectRatio;
  xmax = max(abs(limits(1) - cameraCenter(1)), abs(limits(2) - cameraCenter(1)));
  ymax = max(abs(limits(3) - cameraCenter(2)), abs(limits(4) - cameraCenter(2)));
  zmax = max(abs(limits(5) - cameraCenter(3)), abs(limits(6) - cameraCenter(3)));
  radius = 10 * sqrt((xmax / dar(1)) ^ 2 + (ymax / dar(2)) ^ 2 + (zmax / dar(3)) ^ 2);
  azimuthRadians = azimuth * pi / 180;
  elevationRadians = elevation * pi / 180;
  cameraPosition = [cos(elevationRadians) * sin(azimuthRadians) * radius * dar(1) + cameraCenter(1), ...
    -cos(elevationRadians) * cos(azimuthRadians) * radius * dar(2) + cameraCenter(2), ...
    sin(elevationRadians) * radius * dar(3) + cameraCenter(3)];

  if (strcmp(ax.XDir, 'reverse'))
    cameraPosition(1) = 2 * cameraCenter(1) - cameraPosition(1);
  end
  if (strcmp(ax.YDir, 'reverse'))
    cameraPosition(2) = 2 * cameraCenter(2) - cameraPosition(2);
  end
  if (strcmp(ax.ZDir, 'reverse'))
    cameraPosition(3) = 2 * cameraCenter(3) - cameraPosition(3);
  end
end
%=============================================================================
function cameraTarget = expectedCameraTarget(ax)
  limits = [ax.XLim, ax.YLim, ax.ZLim];
  cameraTarget = [(limits(1) + limits(2)) / 2, (limits(3) + limits(4)) / 2, ...
    (limits(5) + limits(6)) / 2];
end
%=============================================================================
