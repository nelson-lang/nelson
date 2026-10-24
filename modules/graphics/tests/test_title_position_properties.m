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
f = figure();
ax = gca();
ax.FontSize = 13;
t = title('Line Plot of Sine and Cosine Between -2\pi and 2\pi');
asserts.isequal(t.Units, 'data');
asserts.isapprox(t.Position, expectedTitlePosition(ax, false), 1e-12);
asserts.isequal(t.HorizontalAlignment, 'center');
asserts.isequal(t.VerticalAlignment, 'bottom');
asserts.isapprox(t.FontSize, 14.3, 1e-12);
asserts.isequal(t.FontWeight, 'bold');
asserts.isequal(char(t.Clipping), 'off');
asserts.isequal(char(t.HandleVisibility), 'off');
asserts.isequal(char(t.AffectAutoLimits), 'off');
%=============================================================================
t = title('Sized title', 'FontSize', 12);
asserts.isequal(t.FontSize, 12);
ax.TitleHorizontalAlignment = 'left';
asserts.isapprox(t.Position, expectedTitlePosition(ax, false), 1e-12);
asserts.isequal(t.HorizontalAlignment, 'left');
ax.TitleHorizontalAlignment = 'center';
asserts.isapprox(t.Position, expectedTitlePosition(ax, false), 1e-12);
asserts.isequal(t.HorizontalAlignment, 'center');
%=============================================================================
f = figure();
x = linspace(-1, 1);
y = sin(2 * pi * x);
plot(x, y);
t = title('Explicit title font', 'FontSize', 14);
drawnow();
asserts.isequal(t.FontSize, 14);
asserts.isapprox(t.Position, [1.2769148900471095e-06 1.01328273244782 0], 1e-12);
close(f);
close all;
%=============================================================================
function pos = expectedTitlePosition(ax, hasSubtitle)
  yLimits = ax.YLim;
  x = expectedTitleXPosition(ax);
  yRange = yLimits(2) - yLimits(1);
  if yRange <= 0
    yRange = 1;
  end
  if hasSubtitle && ~isempty(ax.Children)
    yOffset = 0.050572520297295;
  elseif hasSubtitle
    if isEmptyTitleString(ax)
      yOffset = 0.06191588933594;
    else
      yOffset = 0.06370192460525;
    end
  else
    yOffset = 0.0125;
  end
  z = expectedTextZ(ax);
  pos = [x, yLimits(2) + yRange * yOffset, z];
end
%=============================================================================
function x = expectedTitleXPosition(ax)
  xLimits = ax.XLim;
  if strcmp(ax.TitleHorizontalAlignment, 'left')
    x = xLimits(1);
  elseif strcmp(ax.TitleHorizontalAlignment, 'right')
    x = xLimits(2);
  else
    x = (xLimits(1) + xLimits(2)) * 0.5;
    position = ax.Position;
    if ~isempty(ax.Children) && numel(position) >= 4 && abs(position(1) - 0.13) < 1e-12 ...
        && abs(position(2) - 0.11) < 1e-12 && abs(position(3) - 0.775) < 1e-12 ...
        && position(4) <= 0.815 + 1e-12 && position(4) >= 0.75
      x = x + (xLimits(2) - xLimits(1)) * 6.38457445e-7;
    end
  end
end
%=============================================================================
function z = expectedTextZ(ax)
  if isempty(ax.Children)
    zLimits = ax.ZLim;
    z = (zLimits(1) + zLimits(2)) * 0.5;
  else
    z = 0;
  end
end
%=============================================================================
function tf = isEmptyTitleString(ax)
  titleString = ax.Title.String;
  tf = length(titleString) == 0 || strcmp(titleString, '');
end
%=============================================================================
