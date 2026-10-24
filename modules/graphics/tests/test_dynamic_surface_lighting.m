%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
% A lit surface must keep its lighting after its geometry changes. This covers
% the OpenGL surface path used by real-time animations: normals, vertex colors,
% and the new geometry must all reach the next rendered frame.
%=============================================================================
close all;
[x, y, z] = sphere(24);
f = figure('Visible', 'on', 'Position', [100 100 620 460], 'Color', [1 1 1]);
ax = axes('Parent', f);
s = surf(ax, x, y, z, ...
  'FaceColor', [0.82 0.18 0.12], 'EdgeColor', 'none', ...
  'FaceLighting', 'gouraud');
axis(ax, 'equal');
axis(ax, [-2 2 -2 2 -2 2]);
axis(ax, 'off');
view(ax, 35, 24);
light(ax, 'Position', [-3 -4 6], 'Style', 'local');
drawnow();
before = getframe(f).cdata;

angle = pi / 3;
set(s, 'XData', cos(angle) * x + sin(angle) * z + 0.55, ...
  'YData', y, 'ZData', -sin(angle) * x + cos(angle) * z + 0.35);
drawnow();
after = getframe(f).cdata;
close(f);

difference = mean(abs(double(after(:)) - double(before(:))));
asserts.istrue(difference > 3);
%=============================================================================
