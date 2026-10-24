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
% 'axis vis3d' freezes the camera view angle with the aspect ratios, so a later
% view change keeps the scene at the size it had when vis3d was applied.
%=============================================================================
f = figure('Visible', 'off');
f.Position(3:4) = [640 640];
[x, y, z] = sphere(40);
surf(x, y, z);
daspect([1 1 1]);
axis tight
ax = gca();
drawnow();
asserts.isequal(ax.CameraViewAngleMode, 'auto');
va0 = ax.CameraViewAngle;
asserts.isapprox(va0, 10.3396, 1e-3);
%=============================================================================
axis vis3d
asserts.isequal(ax.CameraViewAngleMode, 'manual');
asserts.isequal(ax.DataAspectRatioMode, 'manual');
asserts.isequal(ax.PlotBoxAspectRatioMode, 'manual');
asserts.isapprox(ax.CameraViewAngle, va0, 1e-9);
%=============================================================================
view([90 0]);
drawnow();
asserts.isequal(ax.CameraViewAngleMode, 'manual');
asserts.isapprox(ax.CameraViewAngle, va0, 1e-9);
view([45 30]);
drawnow();
asserts.isapprox(ax.CameraViewAngle, va0, 1e-9);
%=============================================================================
camzoom(1.5);
asserts.isapprox(ax.CameraViewAngle, 2 * atand(tand(va0 / 2) / 1.5), 1e-9);
asserts.isapprox(ax.CameraViewAngle, 6.9035, 1e-3);
%=============================================================================
axis normal
asserts.isequal(ax.CameraViewAngleMode, 'auto');
asserts.isequal(ax.DataAspectRatioMode, 'auto');
asserts.isequal(ax.PlotBoxAspectRatioMode, 'auto');
%=============================================================================
% vis3d applied with a 2-D view then rotated to 3-D keeps the frozen angle.
f2 = figure('Visible', 'off');
plot(1:10);
ax2 = gca();
drawnow();
va2 = ax2.CameraViewAngle;
axis vis3d
view(3);
drawnow();
asserts.isequal(ax2.CameraViewAngleMode, 'manual');
asserts.isapprox(ax2.CameraViewAngle, va2, 1e-9);
%=============================================================================
close(f);
close(f2);
%=============================================================================
