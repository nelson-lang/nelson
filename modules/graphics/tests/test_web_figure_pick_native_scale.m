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
skip_if_display_cannot_place_figures('web figure pick native scale');
f = figure('WindowStyle', 'normal', 'Position', [100 100 400 300]);
ax = axes('Parent', f, 'Position', [0.1 0.1 0.8 0.8]);
h = line('Parent', ax, 'XData', [0 5 10], 'YData', [0 5 10]);
set(ax, 'XLim', [0 10], 'YLim', [0 10]);
% The native figure window realizes asynchronously on the desktop; wait for
% its on-screen geometry to be reported before picking (a fixed delay races the
% window and reads an empty geometry).
geometry = [];
for attempt = 1:80
  drawnow();
  geometry = __figure_native_geometry__(f);
  if ~isempty(geometry)
    break
  end
  pause(0.05);
end
point = jsondecode(__web_figure_pick__(f, geometry(3) / 2, geometry(4) / 2));
asserts.istrue(point.found);
asserts.isequal(point.objectHandle, __web_graphics_handle__(h, 'number'));
asserts.isequal(point.index, 2);
asserts.isequal(point.position(:)', [5 5 0]);
expectedPixel = geometry(3:4) * f.DevicePixelRatio / 2;
pixelTolerance = 1 + abs(geometry(3:4) - [400 300]);
asserts.istrue(all(abs(point.pixel(:)' - expectedPixel) <= pixelTolerance));
close(f);
