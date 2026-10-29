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
f = figure('Visible', 'off', 'DrawLater', 'on', 'Color', [0 0 0], ...
  'Position', [100 100 240 180]);
plot([0 1], [0 1], 'Color', [1 1 1], 'LineWidth', 4);
f.DrawLater = 'off';
f.Visible = 'on';
asserts.isequal(char(f.DrawLater), 'off');
asserts.isequal(char(f.Visible), 'on');
frame = getframe(f);
asserts.isequal(size(frame.cdata), expected_figure_frame_size(f));
asserts.istrue(any(frame.cdata(:) > 32));
close(f);
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
