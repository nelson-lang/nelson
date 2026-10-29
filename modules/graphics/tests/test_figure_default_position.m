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
close all
defaultPosition = expected_default_figure_position();
f = figure('Visible', 'off');
asserts.isequal(f.Position, defaultPosition);
asserts.isequal(f.InnerPosition, defaultPosition);
frame = getframe(f);
asserts.isequal(size(frame.cdata), expected_figure_frame_size(f));
close(f);
%=============================================================================
f = figure();
drawnow();
% a shown figure keeps its logical size (device pixels round-trip through the
% device pixel ratio on a scaled display)
asserts.istrue(all(abs(f.Position(3:4) - defaultPosition(3:4)) <= 1));
asserts.istrue(all(abs(f.InnerPosition(3:4) - defaultPosition(3:4)) <= 1));
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
