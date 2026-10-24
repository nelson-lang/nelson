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
f = figure('WindowStyle', 'normal', 'Position', [100 100 400 300]);
ax = axes('Parent', f, 'Position', [0.1 0.1 0.8 0.8]);
h = line('Parent', ax, 'XData', [0 5 10], 'YData', [0 5 10]);
set(ax, 'XLim', [0 10], 'YLim', [0 10]);
for index = 1:15
  drawnow();
  pause(0.01);
end
geometry = __figure_native_geometry__(f);
point = jsondecode(__web_figure_pick__(f, geometry(3) / 2, geometry(4) / 2));
asserts.istrue(point.found);
asserts.isequal(point.objectHandle, __web_graphics_handle__(h, 'number'));
asserts.isequal(point.index, 2);
asserts.isequal(point.position(:)', [5 5 0]);
expectedPixel = geometry(3:4) * f.DevicePixelRatio / 2;
pixelTolerance = 1 + abs(geometry(3:4) - [400 300]);
asserts.istrue(all(abs(point.pixel(:)' - expectedPixel) <= pixelTolerance));
close(f);
