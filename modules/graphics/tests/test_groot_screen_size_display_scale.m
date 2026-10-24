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
% <--WINDOWS ONLY-->
%=============================================================================
% Graphics measure in logical pixels (1 pixel = 1/96 inch). ScreenSize is the
% primary display's physical size divided by its scale, figures report that
% same scale as DevicePixelRatio, and ScreenPixelsPerInch stays 96 at any
% scale. This must hold whatever the display state was when the process
% started, including the placeholder display a remote session exposes while
% its client is minimized or disconnected.
%=============================================================================
close all
info = __native_screen_info__();
asserts.isfalse(isempty(info));
scale = info.Dpi / 96;
expectedSize = [1 1 (info.Position(3:4) / scale)];
%=============================================================================
screenSize = get(0, 'ScreenSize');
% a fractional logical size is rounded by the windowing layer
asserts.istrue(all(abs(screenSize - expectedSize) <= 1));
asserts.isequal(get(0, 'MonitorPositions'), screenSize);
asserts.isequal(get(0, 'ScreenPixelsPerInch'), 96);
%=============================================================================
previousUnits = get(0, 'Units');
set(0, 'Units', 'inches');
screenInches = get(0, 'ScreenSize');
set(0, 'Units', previousUnits);
asserts.istrue(abs(screenInches(3) - screenSize(3) / 96) < 1e-9);
asserts.istrue(abs(screenInches(4) - screenSize(4) / 96) < 1e-9);
%=============================================================================
f = figure('Visible', 'off');
asserts.istrue(abs(get(f, 'DevicePixelRatio') - scale) < 1e-6);
close(f);
%=============================================================================
requested = [120 110 400 300];
f = figure('Position', requested);
drawnow();
asserts.istrue(abs(get(f, 'DevicePixelRatio') - scale) < 1e-6);
% a scaled display rounds logical pixels through device pixels
asserts.istrue(all(abs(get(f, 'Position') - requested) <= 1));
close(f);
%=============================================================================
