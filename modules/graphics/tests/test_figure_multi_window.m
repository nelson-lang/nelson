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
f = figure('Visible', 'off');
f2 = figure('Visible', 'off');
themeClassName = 'nelson.graphics.theme.GraphicsTheme';
asserts.isequal(class(f.Theme), themeClassName);
asserts.isequal(f.Theme.Name, 'Light Theme');
asserts.isequal(f.Theme.BaseColorStyle, 'light');
asserts.istrue(isequalto(f.Theme, f2.Theme));
asserts.isequal(f.ThemeMode, 'auto');
global themeCallbackCount;
themeCallbackCount = 0;
f.ThemeChangedFcn = 'global themeCallbackCount; themeCallbackCount = themeCallbackCount + 1;';
f.Theme = 'dark';
asserts.isequal(f.Theme.Name, 'Dark Theme');
asserts.isequal(f.Theme.BaseColorStyle, 'dark');
asserts.isfalse(isequalto(f.Theme, f2.Theme));
asserts.isequal(f.ThemeMode, 'manual');
asserts.isapprox(f.Color, [33 33 33] / 255, 1e-12);
asserts.isequal(themeCallbackCount, 1);
ax = axes('Parent', f);
asserts.isapprox(ax.Color, [18 18 18] / 255, 1e-12);
asserts.isapprox(ax.XColor, [217 217 217] / 255, 1e-12);
asserts.isapprox(ax.YColor, [217 217 217] / 255, 1e-12);
asserts.isequal(ax.GridColor, [1 1 1]);
ax.YColor = [0 0 1];
f.Theme = 'light';
asserts.isequal(f.ThemeMode, 'manual');
asserts.isequal(f.Color, [0.94 0.94 0.94]);
asserts.isequal(ax.Color, [1 1 1]);
asserts.isapprox(ax.XColor, [33 33 33] / 255, 1e-12);
asserts.isequal(ax.YColor, [0 0 1]);
asserts.isequal(themeCallbackCount, 2);
f.ThemeMode = 'auto';
asserts.isequal(f.ThemeMode, 'auto');
asserts.isequal(f.Theme.Name, 'Light Theme');
asserts.isequal(f.Theme.BaseColorStyle, 'light');
asserts.isequal(themeCallbackCount, 2);
asserts.checkerror('f.Theme = ''blue''', graphicsPropertyErrorMessage('Theme', 'Nelson:figure:invalidTheme'));
clear global themeCallbackCount;
close(f2);
close(f);
%=============================================================================
%=============================================================================
function position = expected_default_figure_position()
  position = groot().DefaultFigurePosition;
end
%=============================================================================
function frameSize = expected_figure_frame_size(f)
  position = f.Position;
  % A hidden empty figure keeps logical pixels; visible figures and figures
  % containing axes are captured at device resolution.
  if strcmp(f.Visible, 'off') && isempty(findobj(f, 'Type', 'axes'))
    frameSize = [round(position(4)), round(position(3)), 3];
  else
    ratio = f.DevicePixelRatio;
    frameSize = [round(position(4) * ratio), round(position(3) * ratio), 3];
  end
end
%=============================================================================
function visibleSize = expected_visible_figure_size(position, f)
  visibleSize = position(3:4) / f.DevicePixelRatio;
end
%=============================================================================
