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
% A figure with axes is captured at its device pixel ratio. A hidden figure
% holding UI components goes through the software path, which must scale the
% components with the rest of the frame, whatever their Units: the capture
% matches the one of the same figure shown on screen.
%=============================================================================
close all
%=============================================================================
cases = {{'uicontrol', 'pixels', [50 50 200 100]}, ...
  {'uicontrol', 'normalized', [0.125 1 / 6 0.5 1 / 3]}, ...
  {'uicontrol', 'points', [37.5 37.5 150 75]}, ...
  {'uipanel', 'pixels', [50 50 200 100]}, ...
  {'uipanel', 'normalized', [0.125 1 / 6 0.5 1 / 3]}};
for k = 1:numel(cases)
  shown = component_box(cases{k}, 'on');
  hidden = component_box(cases{k}, 'off');
  assert_isequal(shown.frame, [round(shown.pos(4) * shown.dpr), round(shown.pos(3) * shown.dpr), 3]);
  assert_isequal(hidden.frame, [round(hidden.pos(4) * hidden.dpr), round(hidden.pos(3) * hidden.dpr), 3]);
  assert_istrue(all(abs(hidden.box - shown.box) <= 4));
end
%=============================================================================
function result = component_box(spec, visible)
  f = figure('Visible', visible, 'Position', [100 100 400 300], 'Color', [0 0 0]);
  axes('Parent', f, 'Position', [0.6 0.6 0.3 0.3], 'Color', [0 0 1], ...
    'XColor', 'none', 'YColor', 'none');
  if strcmp(spec{1}, 'uicontrol')
    uicontrol('Parent', f, 'Style', 'pushbutton', 'Units', spec{2}, ...
      'Position', spec{3}, 'BackgroundColor', [1 0 0], 'String', '');
  else
    uipanel('Parent', f, 'Units', spec{2}, 'Position', spec{3}, ...
      'BackgroundColor', [1 0 0]);
  end
  drawnow();
  drawnow();
  frame = getframe(f);
  pos = f.Position;
  dpr = f.DevicePixelRatio;
  close(f);
  image = frame.cdata;
  [rows, cols] = find((image(:, :, 1) > 200) & (image(:, :, 2) < 80) & (image(:, :, 3) < 80));
  assert_istrue(~isempty(rows));
  result.frame = size(image);
  result.pos = pos;
  result.dpr = dpr;
  result.box = [min(cols), size(image, 1) - max(rows) + 1, max(cols) - min(cols) + 1, max(rows) - min(rows) + 1];
end
%=============================================================================
