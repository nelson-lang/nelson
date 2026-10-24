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
% A UI child with pixel Units is anchored to the bottom-left corner of its
% figure: when the figure grows, its Position is unchanged and the widget
% stays near the bottom (AutoResizeChildren 'off' only means no scaling).
%=============================================================================
close all
%=============================================================================
cases = {'uicontrol', 'uibutton', 'uipanel'};
for k = 1:numel(cases)
  f = figure('Position', [100 100 500 300], 'Color', [0 0 0]);
  switch cases{k}
    case 'uicontrol'
      c = uicontrol(f, 'Style', 'pushbutton', 'String', '', ...
        'Position', [12 8 86 28], 'BackgroundColor', [1 0 0]);
    case 'uibutton'
      c = uibutton(f, 'Text', '', 'Position', [12 8 86 28], ...
        'BackgroundColor', [1 0 0]);
    case 'uipanel'
      c = uipanel(f, 'Units', 'pixels', 'Position', [12 8 86 28], ...
        'BackgroundColor', [1 0 0], 'BorderType', 'none');
  end
  drawnow();
  set(f, 'Position', [100 100 800 600]);
  drawnow();
  pause(0.3);
  drawnow();
  assert_isequal(c.Position, [12 8 86 28]);
  frame = getframe(f);
  img = double(frame.cdata);
  red = img(:, :, 1) > 200 & img(:, :, 2) < 80 & img(:, :, 3) < 80;
  rows = find(any(red, 2));
  assert_isfalse(isempty(rows));
  h = size(img, 1);
  % bottom edge 8 px above the frame bottom, top edge 36 px above it
  % (scaled by the capture device pixel ratio)
  scale = h / 600;
  assert_istrue(abs((h - rows(end)) - 8 * scale) <= 4 * scale);
  assert_istrue(abs((h - rows(1)) - 36 * scale) <= 4 * scale);
  close(f);
end
%=============================================================================
