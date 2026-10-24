%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
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
f = figure('Visible', 'off');
plot([0 2], [1 5]);
s = subtitle('Slope = 2');
ax = gca();
asserts.isequal(s.Type, 'text');
asserts.isequal(ax.Subtitle, s);
asserts.isequal(fieldnames(s), referenceTextProperties());
asserts.isequal(s.String, 'Slope = 2');
asserts.isequal(s.Units, 'data');
asserts.isapprox(s.Position, expectedSubtitlePosition(ax), 1e-12);
asserts.isequal(s.HorizontalAlignment, 'center');
asserts.isequal(s.VerticalAlignment, 'bottom');
asserts.isequal(s.FontWeight, 'normal');
asserts.isequal(s.FontName, 'Helvetica');
asserts.isequal(char(s.Clipping), 'off');
asserts.isequal(char(s.HandleVisibility), 'off');
asserts.isequal(char(s.AffectAutoLimits), 'off');
asserts.isequal(s.Color, [0.129411764705882 0.129411764705882 0.129411764705882]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
plot([0 2], [1 5]);
t = title('Straight Line');
s = subtitle('Slope = 2, y-Intercept = 1');
ax = gca();
drawnow();
% the title layout depends on the platform font metrics
asserts.isapprox(ax.Position, [0.13 0.11 0.775 0.815], 2.5e-6);
asserts.isequal(t.FontSize, 11);
asserts.isequal(s.FontSize, 11);
asserts.isapprox(t.Position, [1.00000127691489 5.20229008118918 0], 1e-12);
asserts.isapprox(s.Position, [1.00000127691489 5.02099236641221 0], 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = gca();
ax.FontSize = 13;
s = subtitle('Scaled subtitle');
asserts.isapprox(s.FontSize, 14.3, 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = gca();
ax.Visible = 'off';
s = subtitle('Hidden subtitle', 'Color', 'm');
asserts.isequal(char(s.Visible), 'off');
asserts.isequal(s.Color, [1 0 1]);
ax.Visible = 'on';
asserts.isequal(char(s.Visible), 'off');
close(f);
%=============================================================================
f = figure('Visible', 'off');
plot([0 1]);
t = title('Straight Line');
s = subtitle('Slope = 1, y-Intercept = 0', 'Color', 'blue');
asserts.isequal(t.String, 'Straight Line');
asserts.isequal(s.String, 'Slope = 1, y-Intercept = 0');
asserts.isequal(s.Color, [0 0 1]);
asserts.isequal(t.Units, 'data');
asserts.isapprox(t.Position, expectedTitlePosition(gca(), true), 1e-12);
asserts.isapprox(s.Position, expectedSubtitlePosition(gca()), 1e-12);
asserts.isequal(t.FontWeight, 'bold');
asserts.isequal(s.FontWeight, 'normal');
asserts.isequal(char(t.AffectAutoLimits), 'off');
asserts.isequal(char(s.AffectAutoLimits), 'off');
close(f);
%=============================================================================
f = figure('Visible', 'off');
plot([0 1]);
t = title('Aligned title');
s = subtitle('Aligned subtitle');
ax = gca();
ax.TitleHorizontalAlignment = 'left';
asserts.isapprox(t.Position, expectedTitlePosition(ax, true), 1e-12);
asserts.isapprox(s.Position, expectedSubtitlePosition(ax), 1e-12);
asserts.isequal(t.HorizontalAlignment, 'left');
asserts.isequal(s.HorizontalAlignment, 'left');
ax.TitleHorizontalAlignment = 'right';
asserts.isapprox(t.Position, expectedTitlePosition(ax, true), 1e-12);
asserts.isapprox(s.Position, expectedSubtitlePosition(ax), 1e-12);
asserts.isequal(t.HorizontalAlignment, 'right');
asserts.isequal(s.HorizontalAlignment, 'right');
ax.TitleHorizontalAlignment = 'center';
asserts.isapprox(t.Position, expectedTitlePosition(ax, true), 1e-12);
asserts.isapprox(s.Position, expectedSubtitlePosition(ax), 1e-12);
asserts.isequal(t.HorizontalAlignment, 'center');
asserts.isequal(s.HorizontalAlignment, 'center');
close(f);
%=============================================================================
f = figure('Visible', 'off');
s = subtitle({'Slope = 2', 'y-Intercept = 1'});
asserts.isequal(s.String, {'Slope = 2'; 'y-Intercept = 1'});
s = subtitle(["Top line" "Bottom line"], "Interpreter", "none");
asserts.isequal(s.String, {'Top line'; 'Bottom line'});
asserts.isequal(s.Interpreter, 'none');
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
s = subtitle(ax, 'Target axes', 'FontSize', 12, 'FontWeight', 'bold');
asserts.isequal(ax.Subtitle, s);
asserts.isequal(s.String, 'Target axes');
asserts.isequal(s.FontSize, 12);
asserts.isequal(s.FontWeight, 'bold');
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
s = subtitle(ax, 'Manual subtitle position', 'Units', 'normalized', 'Position', [0.25 0.9 0]);
asserts.isequal(s.Units, 'normalized');
asserts.isequal(s.Position, [0.25 0.9 0]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
t = tiledlayout(f, 1, 2);
s = subtitle(t, 'Shared subtitle', 'Color', 'red');
asserts.istrue(isgraphics(s));
asserts.isfalse(isprop(s, 'Type'));
asserts.isfalse(isprop(s, 'Position'));
asserts.istrue(isprop(s, 'FontSizeMode'));
asserts.isequal(fieldnames(s), referenceLayoutTextProperties());
asserts.isequal(t.Subtitle, s);
asserts.isequal(s.String, 'Shared subtitle');
asserts.isequal(s.Color, [1 0 0]);
asserts.isequal(s.HorizontalAlignment, 'center');
asserts.isequal(s.VerticalAlignment, 'bottom');
asserts.isequal(s.FontSize, 12);
asserts.isequal(s.FontSizeMode, 'auto');
s.FontSizeMode = 'manual';
asserts.isequal(s.FontSizeMode, 'manual');
asserts.isequal(s.FontWeight, 'normal');
close(f);
%=============================================================================
f = figure('Visible', 'off', 'InnerPosition', [100 100 560 420]);
t = tiledlayout(f, 2, 2);
ax1 = nexttile(t);
plot(1:5, 1:5);
ax2 = nexttile(t);
plot(1:5, 5:-1:1);
subtitle(t, 'Layout subtitle');
drawnow();
layoutTolerance = 1e-12;
asserts.isapprox(ax1.Position, [0.13 0.578571428571429 0.3175 0.328571428571429], layoutTolerance);
asserts.isapprox(ax1.OuterPosition, [0.13 0.508571428571429 0.3875 0.398571428571429], layoutTolerance);
title(t, 'Layout title');
drawnow();
asserts.isapprox(ax1.Position, [0.13 0.549603174603175 0.3175 0.299603174603175], layoutTolerance);
asserts.isapprox(ax1.OuterPosition, [0.13 0.479603174603175 0.3875 0.369603174603175], layoutTolerance);
asserts.isapprox(ax2.Position, [0.5875 0.549603174603175 0.3175 0.299603174603175], layoutTolerance);
asserts.isapprox(ax2.OuterPosition, [0.5175 0.479603174603175 0.3875 0.369603174603175], layoutTolerance);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax1 = subplot(1, 2, 1);
ax2 = subplot(1, 2, 2);
s = subtitle([ax1 ax2], 'Array subtitle');
asserts.isequal(size(s), [1 2]);
asserts.isequal(s(1), ax1.Subtitle);
asserts.isequal(s(2), ax2.Subtitle);
asserts.isequal(s(1).String, 'Array subtitle');
asserts.isequal(s(2).String, 'Array subtitle');
close(f);
%=============================================================================
f = figure('Visible', 'off');
s1 = subtitle('First subtitle');
s1.Color = [1 0 0];
s2 = subtitle('Second subtitle');
ax = gca();
asserts.istrue(isgraphics(s1));
asserts.isequal(s1, s2);
asserts.isequal(s2.Type, 'text');
asserts.isequal(ax.Subtitle, s2);
asserts.isequal(s2.String, 'Second subtitle');
asserts.isequal(s2.Color, [1 0 0]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
asserts.checkerror('subtitle(gcf(), ''bad'')', getString(message('Nelson:subtitle:invalidInputArgument')));
asserts.checkerror('subtitle(axes())', getString(message('Nelson:subtitle:textExpected')));
asserts.checkerror('subtitle(42)', getString(message('Nelson:subtitle:textExpected')));
close(f);
%=============================================================================
function pos = expectedSubtitlePosition(ax)
  xLimits = ax.XLim;
  yLimits = ax.YLim;
  if strcmp(ax.TitleHorizontalAlignment, 'left')
    x = xLimits(1);
  elseif strcmp(ax.TitleHorizontalAlignment, 'right')
    x = xLimits(2);
  else
    x = expectedTitleXPosition(ax);
  end
  yRange = yLimits(2) - yLimits(1);
  if yRange <= 0
    yRange = 1;
  end
  if ~isempty(ax.Children) && isEmptyTitleString(ax)
    yOffset = 0.005218216318785;
  elseif ~isempty(ax.Children)
    yOffset = 0.005248091603053;
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
  xLimits = ax.XLim;
  yLimits = ax.YLim;
  if strcmp(ax.TitleHorizontalAlignment, 'left')
    x = xLimits(1);
  elseif strcmp(ax.TitleHorizontalAlignment, 'right')
    x = xLimits(2);
  else
    x = expectedTitleXPosition(ax);
  end
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
function z = expectedTextZ(ax)
  if isempty(ax.Children)
    zLimits = ax.ZLim;
    z = (zLimits(1) + zLimits(2)) * 0.5;
  else
    z = 0;
  end
end
%=============================================================================
function x = expectedTitleXPosition(ax)
  xLimits = ax.XLim;
  x = (xLimits(1) + xLimits(2)) * 0.5;
  if ~isempty(ax.Children)
    x = x + (xLimits(2) - xLimits(1)) * 6.38457445e-7;
  end
end
%=============================================================================
function tf = isEmptyTitleString(ax)
  titleString = ax.Title.String;
  tf = length(titleString) == 0 || strcmp(titleString, '');
end
%=============================================================================
function names = referenceTextProperties()
  names = {'Color'; 'ColorMode'; 'Position'; 'String'; 'Interpreter'; 'Extent'; 'Rotation';
    'FontName'; 'FontUnits'; 'FontSize'; 'FontAngle'; 'FontWeight'; 'HorizontalAlignment';
    'VerticalAlignment'; 'EdgeColor'; 'LineStyle'; 'LineWidth'; 'BackgroundColor'; 'Margin';
    'Editing'; 'Clipping'; 'FontSmoothing'; 'Interactions'; 'Units'; 'Children'; 'Parent';
    'Visible'; 'HandleVisibility'; 'ButtonDownFcn'; 'ContextMenu'; 'BusyAction'; 'BeingDeleted';
    'Interruptible'; 'CreateFcn'; 'DeleteFcn'; 'Type'; 'Tag'; 'UserData'; 'Selected';
    'SelectionHighlight'; 'HitTest'; 'PickableParts'; 'AffectAutoLimits'; 'SeriesIndex'};
end
%=============================================================================
function names = referenceLayoutTextProperties()
  names = {'String'; 'FontName'; 'FontSize'; 'FontAngle'; 'FontWeight'; 'Rotation';
    'HorizontalAlignment'; 'VerticalAlignment'; 'Interpreter'; 'Color'; 'Children'; 'Parent';
    'Visible'; 'HandleVisibility'};
end
%=============================================================================
