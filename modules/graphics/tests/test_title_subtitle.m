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
% Pin the calibration geometry for 573x460 on a 1024x768 CI runner.
set(groot, 'DefaultFigurePosition', [423 180 1074 646]);
%=============================================================================
f = figure('Visible', 'off');
plot([0 1]);
[t, s] = title('Straight Line', 'Slope = 1, y-Intercept = 0', 'Color', 'blue');
asserts.isequal(t.String, 'Straight Line');
asserts.isequal(s.String, 'Slope = 1, y-Intercept = 0');
asserts.isequal(t.Color, [0 0 1]);
asserts.isequal(s.Color, [0 0 1]);
asserts.isequal(t.Units, 'data');
asserts.isequal(s.Units, 'data');
asserts.isapprox(t.Position, expectedTitlePosition(gca(), true), 1e-12);
asserts.isapprox(s.Position, expectedSubtitlePosition(gca()), 1e-12);
asserts.isequal(t.FontSize, s.FontSize);
asserts.isequal(t.FontWeight, 'bold');
asserts.isequal(s.FontWeight, 'normal');
%=============================================================================
f = figure('Visible', 'off');
plot([0 1]);
ax = gca();
[t, s] = title('Straight Line', 'Slope = 1');
drawnow();
heightBefore = ax.Position(4);
% Default title+subtitle: axes height matches expected behavior (0.815).
asserts.isapprox(heightBefore, 0.815, 5e-6);
t.FontSize = 16;
drawnow();
% A 16pt title with a subtitle still fits inside the default top margin, so the
% axes height stays 0.815 (matches expected behavior, which keeps 0.815 until the
% title font genuinely overflows the top margin -- shrinking starts at 17pt).
asserts.isapprox(ax.Position(4), heightBefore, 5e-6);
asserts.isapprox(ax.Position(4), 0.815, 5e-6);
asserts.isapprox(t.Position, [1.5000006384574451 1.054093568490805 0], 1e-12);
asserts.isapprox(s.Position, [1.5000006384574451 1.0053606237816766 0], 1e-12);
%=============================================================================
title('Only title');
ax = gca();
asserts.isequal(ax.Subtitle.String, '');
close all;
%=============================================================================
function pos = expectedSubtitlePosition(ax)
  yLimits = ax.YLim;
  x = expectedTitleXPosition(ax);
  yRange = yLimits(2) - yLimits(1);
  if yRange <= 0
    yRange = 1;
  end
  if ~isempty(ax.Children)
    if isEmptyTitleString(ax)
      yOffset = 0.005218216318785;
    else
      yOffset = 0.005248091603053;
    end
  elseif isEmptyTitleString(ax)
    yOffset = 0.00642523364486;
  else
    yOffset = 0.00661057692308;
  end
  z = expectedTextZ(ax);
  pos = [x, yLimits(2) + yRange * yOffset, z];
end
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
