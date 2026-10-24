%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
f = figure('Visible', 'off', 'Position', [100 100 800 600]);
a1 = subplot(2, 2, 1, 'Parent', f);
plot(a1, 1:5);
a2 = subplot(2, 2, 2, 'Parent', f);
plot3(a2, [0 1 2], [0 1 0], [0 1 2]);
a3 = subplot(2, 2, 3, 'Parent', f);
surf(a3, peaks(15));
title(a3, 'A smooth surface');
a4 = subplot(2, 2, 4, 'Parent', f);
plot(a4, 5:-1:1);
camva(a3, 15);
drawnow();
fh = __web_graphics_handle__(f, 'number');
ah = __web_graphics_handle__(a3, 'number');
otherViews = [a1.View; a2.View; a4.View];
otherPositions = [a1.CameraPosition; a2.CameraPosition; a4.CameraPosition];

% Pan only the surface, then rotate it without resetting its translated target.
__web_figure_action__(fh, jsonencode(struct('action', 'pan', 'axesHandle', ah, ...
  'dxFrac', 0.12, 'dyFrac', -0.08)));
asserts.isequal(a3.CameraPositionMode, 'manual');
asserts.isequal(a3.CameraTargetMode, 'manual');
p0 = a3.CameraPosition; target = a3.CameraTarget;
aspect = a3.DataAspectRatio;
radius = norm((p0 - target) ./ aspect);
angle = a3.CameraViewAngle;
oldView = a3.View;
jsondecode(__web_display_list__(f, a3, oldView(1) + 30, oldView(2) + 10, true));
asserts.istrue(norm(a3.CameraPosition - p0) > 1e-6);
asserts.isequal(a3.CameraTarget, target);
asserts.isequal(a3.CameraPositionMode, 'manual');
asserts.isequal(a3.CameraTargetMode, 'manual');
asserts.isapprox(norm((a3.CameraPosition - target) ./ aspect), radius, 1e-8);
asserts.isapprox(a3.CameraViewAngle, angle, 1e-8);
asserts.isequal(a3.View, oldView + [30 10]);
az = (oldView(1) + 30) * pi / 180;
el = (oldView(2) + 10) * pi / 180;
expected = target + radius * [cos(el)*sin(az), -cos(el)*cos(az), sin(el)] .* aspect;
asserts.isapprox(a3.CameraPosition, expected, 1e-8);
asserts.isequal([a1.View; a2.View; a4.View], otherViews);
asserts.isequal([a1.CameraPosition; a2.CameraPosition; a4.CameraPosition], otherPositions);

% Repeating the final pointer frame must not rotate twice.
p1 = a3.CameraPosition;
jsondecode(__web_display_list__(f, a3, oldView(1) + 30, oldView(2) + 10, false));
asserts.isapprox(a3.CameraPosition, p1, 1e-8);

% Inspector view edits use the same orbit behavior for a manual camera.
__web_figure_action__(fh, jsonencode(struct('action', 'view', 'axesHandle', ah, ...
  'azimuth', oldView(1) - 15, 'elevation', oldView(2) - 5)));
asserts.istrue(norm(a3.CameraPosition - p1) > 1e-6);
asserts.isequal(a3.CameraTarget, target);
asserts.isapprox(norm((a3.CameraPosition - target) ./ aspect), radius, 1e-8);

% A manual up vector and unequal data units stay consistent through an orbit.
set(a3, 'DataAspectRatio', [1 2 3], 'XDir', 'reverse', ...
  'CameraPosition', target + [10 -20 30], 'CameraUpVector', [0 0 1]);
drawnow();
up0 = a3.CameraUpVector;
radius = norm((a3.CameraPosition - target) ./ a3.DataAspectRatio);
jsondecode(__web_display_list__(f, a3, oldView(1) + 45, oldView(2) + 20, false));
asserts.isequal(a3.CameraTarget, target);
asserts.isequal(a3.CameraUpVectorMode, 'manual');
asserts.istrue(norm(a3.CameraUpVector - up0) > 1e-6);
asserts.isapprox(norm((a3.CameraPosition - target) ./ a3.DataAspectRatio), radius, 1e-8);
asserts.istrue(contains(__web_display_list__(f), 'A smooth surface'));
close(f);
