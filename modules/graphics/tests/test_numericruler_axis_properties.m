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
% A NumericRuler (Axes.XAxis / YAxis / ZAxis) stands in as axisData on the
% figure export path, which reads a wide surface of axes properties on it. In
% addition to the font, tick direction and tick length, the ruler exposes the
% shared axes state FontUnits, LineWidth, TickDir, GridColor, GridAlpha, Box and
% Units, all mirroring the parent axes.
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
%=============================================================================
% Each ruler exposes the additional mirrored axes properties.
mirrored = {'FontUnits', 'LineWidth', 'TickDir', 'GridColor', 'GridAlpha', 'Box', 'Units'};
for name = {'XAxis', 'YAxis', 'ZAxis'}
  r = get(ax, name{1});
  for p = mirrored
    asserts.istrue(isprop(r, p{1}));
  end
end
%=============================================================================
% The ruler values match the parent axes defaults.
r = get(ax, 'XAxis');
asserts.isequal(get(r, 'FontUnits'), get(ax, 'FontUnits'));
asserts.isequal(get(r, 'LineWidth'), get(ax, 'LineWidth'));
asserts.isequal(get(r, 'TickDir'), get(ax, 'TickDir'));
asserts.isequal(get(r, 'GridColor'), get(ax, 'GridColor'));
asserts.isequal(get(r, 'GridAlpha'), get(ax, 'GridAlpha'));
asserts.isequal(get(r, 'Box'), get(ax, 'Box'));
asserts.isequal(get(r, 'Units'), get(ax, 'Units'));
%=============================================================================
% Editing the axes is reflected on every ruler after a refresh.
set(ax, 'LineWidth', 2.5);
set(ax, 'TickDir', 'out');
set(ax, 'GridAlpha', 0.5);
set(ax, 'Box', 'on');
for name = {'XAxis', 'YAxis', 'ZAxis'}
  r = get(ax, name{1});
  asserts.isequal(get(r, 'LineWidth'), 2.5);
  asserts.isequal(get(r, 'TickDir'), 'out');
  asserts.isequal(get(r, 'GridAlpha'), 0.5);
  asserts.isequal(char(get(r, 'Box')), 'on');
end
%=============================================================================
% The yyaxis path exposes both sides, each carrying the mirrored surface.
close(f);
f = figure('Visible', 'off');
yyaxis('left');
plot(1:10);
yyaxis('right');
plot((1:10) .^ 2);
r = get(gca(), 'YAxis');
for k = 1:numel(r)
  for p = mirrored
    asserts.istrue(isprop(r(k), p{1}));
  end
end
%=============================================================================
close(f);
%=============================================================================
