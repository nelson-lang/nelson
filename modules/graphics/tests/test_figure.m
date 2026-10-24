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
for i = 1:10
  figure(i);
end
%=============================================================================
current = gcf();
asserts.isequal(current.Number, 10)
%=============================================================================
f1 = figure(1);
current = gcf();
asserts.isequal(f1, current);
asserts.isequal(current.Number, 1)
%=============================================================================
f11 = figure();
current = gcf();
asserts.isequal(f11, current);
asserts.isequal(current.Number, 11)
%=============================================================================
f33 = figure(33);
current = gcf();
asserts.isequal(f33, current);
asserts.isequal(current.Number, 33)
%=============================================================================
f12 = figure();
current = gcf();
asserts.isequal(f12, current);
asserts.isequal(current.Number, 12)
%=============================================================================
f1 = figure(33);
asserts.isequal(f1, gcf());
f35 = figure(35);
asserts.isequal(f35, gcf());
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
