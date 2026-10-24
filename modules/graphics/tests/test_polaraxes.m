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
% A polaraxes is a dedicated PolarAxes graphics object exposing the polar
% property API. Defaults are verified against the reference.
%=============================================================================
referenceColor = 33 / 255;
%=============================================================================
f = figure('Visible', 'off');
ax = polaraxes(f);
asserts.isequal(ax.Type, 'polaraxes');
asserts.isequal(class(ax), 'nelson.graphics.axis.PolarAxes');
asserts.istrue(isgraphics(ax, 'polaraxes'));
asserts.isfalse(isgraphics(ax, 'axes'));
% Directions and axis locations.
asserts.isequal(ax.ThetaDir, 'counterclockwise');
asserts.isequal(ax.ThetaZeroLocation, 'right');
asserts.isequal(ax.RDir, 'normal');
asserts.isequal(ax.ThetaAxisUnits, 'degrees');
asserts.isequal(ax.RAxisLocation, 80);
asserts.isequal(ax.RAxisLocationMode, 'auto');
asserts.isequal(ax.RTickLabelRotation, 0);
% Colors.
asserts.isapprox(ax.RColor, [referenceColor referenceColor referenceColor], 1e-6);
asserts.isapprox(ax.ThetaColor, [referenceColor referenceColor referenceColor], 1e-6);
asserts.isapprox(ax.GridColor, [referenceColor referenceColor referenceColor], 1e-6);
asserts.isapprox(ax.MinorGridColor, [referenceColor referenceColor referenceColor], 1e-6);
asserts.isequal(ax.RColorMode, 'auto');
asserts.isequal(ax.ThetaColorMode, 'auto');
% Grid and lines.
asserts.isequal(ax.GridAlpha, 0.15);
asserts.isequal(ax.MinorGridAlpha, 0.15);
asserts.isequal(ax.GridLineStyle, '-');
asserts.isequal(ax.MinorGridLineStyle, ':');
asserts.isequal(ax.LineWidth, 0.5);
asserts.isequal(ax.Layer, 'bottom');
asserts.isequal(ax.TickDir, 'in');
asserts.isequal(char(ax.ThetaGrid), 'on');
asserts.isequal(char(ax.RGrid), 'on');
asserts.isequal(char(ax.ThetaMinorGrid), 'off');
asserts.isequal(char(ax.RMinorGrid), 'off');
asserts.isequal(char(ax.ThetaMinorTick), 'off');
asserts.isequal(char(ax.RMinorTick), 'off');
asserts.isequal(ax.TitleHorizontalAlignment, 'center');
% Limits and ticks.
asserts.isequal(ax.RLim, [0 1]);
asserts.isequal(ax.ThetaLim, [0 360]);
asserts.isapprox(ax.RTick, [0 0.2 0.4 0.6 0.8 1], 1e-12);
asserts.isequal(ax.ThetaTick, 0:30:360);
thetaLabels = ax.ThetaTickLabel;
asserts.isequal(thetaLabels{1}, ['0', char(176)]);
asserts.isequal(thetaLabels{end}, ['330', char(176)]);
% Cartesian-only properties are hidden from a polaraxes.
visibleProperties = properties(ax);
newPolarProperties = {'ThetaDir', 'ThetaZeroLocation', 'RDir', 'RColor', 'ThetaColor', ...
  'RAxisLocation', 'RTickLabelRotation', 'ThetaGrid', 'RGrid', 'MinorGridColor'};
asserts.istrue(all(ismember(newPolarProperties, visibleProperties)));
asserts.isfalse(any(ismember({'XLim', 'YLim', 'XColor', 'XDir'}, visibleProperties)));
close(f);
%=============================================================================
% Set / get round trips on the polar property API.
f = figure('Visible', 'off');
ax = polaraxes(f);
ax.ThetaDir = 'clockwise';
asserts.isequal(ax.ThetaDir, 'clockwise');
ax.ThetaZeroLocation = 'top';
asserts.isequal(ax.ThetaZeroLocation, 'top');
ax.RDir = 'reverse';
asserts.isequal(ax.RDir, 'reverse');
ax.RColor = [1 0 0];
asserts.isequal(ax.RColor, [1 0 0]);
ax.ThetaColor = [0 0 1];
asserts.isequal(ax.ThetaColor, [0 0 1]);
ax.RAxisLocation = 45;
asserts.isequal(ax.RAxisLocation, 45);
ax.MinorGridAlpha = 0.3;
asserts.isequal(ax.MinorGridAlpha, 0.3);
ax.ThetaGrid = 'off';
asserts.isequal(char(ax.ThetaGrid), 'off');
ax.RMinorTick = 'on';
asserts.isequal(char(ax.RMinorTick), 'on');
% Name/value at construction.
axnv = polaraxes('Parent', f, 'ThetaDir', 'clockwise', 'ThetaZeroLocation', 'top');
asserts.isequal(axnv.ThetaDir, 'clockwise');
asserts.isequal(axnv.ThetaZeroLocation, 'top');
close(f);
%=============================================================================
% Invalid enumeration values are rejected.
f = figure('Visible', 'off');
ax = polaraxes(f);
rejected = false;
try
  ax.ThetaDir = 'sideways';
catch
  rejected = true;
end
asserts.istrue(rejected);
rejected = false;
try
  ax.ThetaZeroLocation = 'northwest';
catch
  rejected = true;
end
asserts.istrue(rejected);
close(f);
%=============================================================================
% A cartesian axes does not expose the polar property API.
f = figure('Visible', 'off');
ax = axes('Parent', f);
asserts.isfalse(isprop(ax, 'ThetaDir'));
asserts.isfalse(isprop(ax, 'RColor'));
asserts.isfalse(isprop(ax, 'ThetaZeroLocation'));
close(f);
%=============================================================================
