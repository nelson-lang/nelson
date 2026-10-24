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
referencePosition = [100 100 560 420];
f = figure('Position', referencePosition);
drawnow();
asserts.istrue(all(f.Position(3:4) > 0));
asserts.istrue(all(f.InnerPosition(3:4) > 0));
frame = getframe(f);
% getframe() may capture at device resolution or at logical resolution depending
% on the display's DPI scaling, so accept either. The two coincide at 100%
% scaling and differ by the device pixel ratio on scaled displays.
frameSize = size(frame.cdata);
deviceSize = expected_figure_frame_size(f);
logicalSize = [round(f.Position(4)), round(f.Position(3)), 3];
asserts.istrue(isequal(frameSize, deviceSize) || isequal(frameSize, logicalSize));
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
