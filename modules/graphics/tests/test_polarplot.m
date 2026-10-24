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
f = figure('Visible', 'off', 'InnerPosition', [100 100 560 420], 'Color', [1 1 1]);
theta = linspace(0, 2 * pi, 5);
rho = [1 -2 3 -4 5];
p = polarplot(theta, rho, 'r-o', 'LineWidth', 2);
ax = gca();
state = ax.UserData;
asserts.istrue(isstruct(state));
asserts.istrue(isfield(state, 'PolarAxes'));
asserts.istrue(state.PolarAxes);
asserts.isequal(ax.Type, 'polaraxes');
asserts.isequal(class(ax), 'nelson.graphics.axis.PolarAxes');
asserts.istrue(isobject(ax));
asserts.istrue(isa(ax, 'nelson.graphics.axis.PolarAxes'));
asserts.istrue(isa(ax, 'handle'));
asserts.istrue(isgraphics(ax, 'polaraxes'));
asserts.isfalse(isgraphics(ax, 'axes'));
asserts.istrue(any(findobj(f, 'Type', 'polaraxes') == ax));
asserts.istrue(isempty(findobj(f, 'Type', 'axes')));
asserts.isequal(ax.Color, [1 1 1]);
asserts.isequal(p.Type, 'line');
asserts.isequal(p.ThetaData(:), theta(:));
asserts.isequal(p.RData(:), rho(:));
asserts.istrue(isempty(p.XData));
asserts.istrue(isempty(p.YData));
asserts.isequal(p.LineStyle, '-');
asserts.isequal(p.Marker, 'o');
asserts.isequal(p.Color, [1 0 0]);
asserts.isequal(rlim(), [0 5]);
asserts.isequal(thetalim(), [0 360]);
asserts.isequal(thetaticks(), 0:30:360);
thetaLabels = thetaticklabels();
asserts.isequal(thetaLabels{1}, ['0', char(176)]);
asserts.isequal(thetaLabels{end}, ['330', char(176)]);
asserts.isfalse(any(strcmp(thetaLabels, ['360', char(176)])));
frame = getframe(f);
asserts.isequal(size(frame.cdata), expectedFrameSize(f));
close(f);
%=============================================================================
% an abbreviated line property name is accepted
g = figure('Visible', 'off');
theta = linspace(0, 2 * pi, 50);
p = polarplot(theta, sin(theta), 'LineW', 2);
asserts.isapprox(p.LineWidth, 2, 1e-12);
% a LineSpec that also abbreviates a property stays positional: 'm' is magenta,
% not the Marker property, and the trailing LineWidth still applies
p2 = polarplot(theta, sin(theta), 'm', 'LineWidth', 3);
asserts.isequal(p2.Color, [1 0 1]);
asserts.isapprox(p2.LineWidth, 3, 1e-12);
close(g);
%=============================================================================
function frameSize = expectedFrameSize(f)
  pos = f.Position;
  % getframe captures a figure that contains axes at device resolution.
  ratio = f.DevicePixelRatio;
  frameSize = [round(pos(4) * ratio), round(pos(3) * ratio), 3];
end
%=============================================================================
