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
%=============================================================================
% Perspective camera flyover over a lit heightfield (Moonrun-style): measures
% the per-frame cost of the true perspective projection plus the cam* orbit.
tic()
f = figure('Visible', 'off', 'Position', [100 100 640 480], 'Color', [0 0 0]);
[x, y, z] = peaks(80);
ax = axes('Parent', f);
surf(ax, x, y, z, 'EdgeColor', 'none', 'FaceLighting', 'gouraud');
colormap(ax, copper(128));
light(ax, 'Position', [1 -1 1], 'Style', 'infinite');
material(ax, 'dull');
axis(ax, 'off');
axis(ax, 'vis3d');
camproj(ax, 'perspective');
camtarget(ax, [0 0 0]);
campos(ax, [0 -40 25]);
camva(ax, 35);
for k = 1:48
  camorbit(ax, 360 / 48, 0);
  drawnow();
end
close(f);
toc()
%=============================================================================
