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
% A NumericRuler (Axes.XAxis / YAxis / ZAxis) mirrors the font of the parent
% axes: FontSize, FontName, FontWeight and FontAngle are exposed and track the
% axes. TickDirection likewise mirrors the axes TickDir, and TickLength mirrors
% the axes TickLength. Behaviour is verified against the reference.
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
%=============================================================================
% Each ruler exposes the font properties and TickDirection.
for name = {'XAxis', 'YAxis', 'ZAxis'}
  r = get(ax, name{1});
  asserts.istrue(isprop(r, 'FontSize'));
  asserts.istrue(isprop(r, 'FontName'));
  asserts.istrue(isprop(r, 'FontWeight'));
  asserts.istrue(isprop(r, 'FontAngle'));
  asserts.istrue(isprop(r, 'TickDirection'));
  asserts.istrue(isprop(r, 'TickLength'));
end
%=============================================================================
% The ruler font matches the parent axes font.
r = get(ax, 'XAxis');
asserts.isequal(get(r, 'FontSize'), get(ax, 'FontSize'));
asserts.isequal(get(r, 'FontName'), get(ax, 'FontName'));
asserts.isequal(get(r, 'FontWeight'), get(ax, 'FontWeight'));
asserts.isequal(get(r, 'FontAngle'), get(ax, 'FontAngle'));
%=============================================================================
% Editing the axes font is reflected on every ruler after a refresh.
set(ax, 'FontSize', 14);
set(ax, 'FontName', 'Courier New');
set(ax, 'FontWeight', 'bold');
set(ax, 'FontAngle', 'italic');
for name = {'XAxis', 'YAxis', 'ZAxis'}
  r = get(ax, name{1});
  asserts.isequal(get(r, 'FontSize'), 14);
  asserts.isequal(get(r, 'FontName'), 'Courier New');
  asserts.isequal(get(r, 'FontWeight'), 'bold');
  asserts.isequal(get(r, 'FontAngle'), 'italic');
end
%=============================================================================
% TickDirection mirrors the axes TickDir.
set(ax, 'TickDir', 'out');
r = get(ax, 'XAxis');
asserts.isequal(get(r, 'TickDirection'), 'out');
%=============================================================================
% Every ruler exposes TickLength and matches the parent axes TickLength.
for name = {'XAxis', 'YAxis', 'ZAxis'}
  r = get(ax, name{1});
  asserts.isequal(get(r, 'TickLength'), get(ax, 'TickLength'));
end
%=============================================================================
close(f);
%=============================================================================
