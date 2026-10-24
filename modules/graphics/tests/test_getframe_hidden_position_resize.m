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
f = figure('Visible', 'off');
x = linspace(0, 10)';
line(x, sin(x), 'Color', [0 1 0]);
line(x, cos(x), 'Color', [1 0 0]);
set(f, 'Position', [100 100 800 600]);
drawnow();

frame = getframe(f);
ratio = f.DevicePixelRatio;
asserts.isequal(size(frame.cdata), [round(600 * ratio), round(800 * ratio), 3]);
close(f);
%=============================================================================
