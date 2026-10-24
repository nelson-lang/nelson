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
for repetition = 1:6
  fig = uifigure('Visible', 'off', 'Position', [80 60 630 420]);
  fig.AutoResizeChildren = 'off';
  root = uipanel(fig, 'BorderType', 'none', 'AutoResizeChildren', 'off', ...
    'Position', [0 0 1050 700]);
  tabs = uitabgroup(root, 'Visible', 'off', 'Position', [630 82 370 526]);
  controls = [];
  for tabIndex = 1:37
    tab = uitab(tabs, 'Title', num2str(tabIndex));
    for controlIndex = 1:23
      row = floor((controlIndex - 1) / 4);
      column = mod(controlIndex - 1, 4);
      control = uilabel(tab, 'Text', num2str(controlIndex), ...
        'Position', [12 + 82 * column, 16 + 30 * row, 74, 22]);
      controls = [controls control];
    end
  end
  drawnow();

  fig.Position = [80 60 840 560];
  root.Position = [0 0 840 560];
  for controlIndex = 1:numel(controls)
    position = controls(controlIndex).Position;
    controls(controlIndex).Position = [position(1) + 2, position(2) + 2, ...
      position(3) + 4, position(4) + 1];
    controls(controlIndex).FontSize = 8;
  end
  drawnow();
  close(fig);
  drawnow();
  asserts.istrue(isempty(groot().Children));
  asserts.istrue(isempty(timerfindall()));
end
%=============================================================================
