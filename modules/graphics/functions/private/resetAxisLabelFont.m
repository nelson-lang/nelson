%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function resetAxisLabelFont(ax, h)
  % xlabel/ylabel give the label back the axes font, like the reference.
  if ~isgraphics(ax, 'axes')
    return
  end
  set(h, 'FontUnits', ax.FontUnits, ...
    'FontName', ax.FontName, ...
    'FontWeight', ax.FontWeight, ...
    'FontAngle', ax.FontAngle, ...
    'FontSize', ax.FontSize * ax.LabelFontSizeMultiplier);
  % Labels are in points; changing the units converts the size.
  set(h, 'FontUnits', 'points');
  % A separate set, so the modes are not turned to manual by the values above.
  set(h, 'FontSizeMode', 'auto', 'FontWeightMode', 'auto', ...
    'FontAngleMode', 'auto', 'FontNameMode', 'auto', 'FontUnitsMode', 'auto');
end
%=============================================================================
