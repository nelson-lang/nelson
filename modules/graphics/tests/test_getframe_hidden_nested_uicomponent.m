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
% getframe of a hidden figure holding a UI component goes through the software
% path, which never touches the unrealized widget. A component held by a
% uipanel counts too, and must still be captured there. (A UI component cannot
% be re-parented under an axes or an hggroup: see test_ui_parent_setter.)
%=============================================================================
close all
%=============================================================================
builders = {
  @(h) uicontrol('Parent', h, 'Style', 'pushbutton', 'Units', 'pixels', ...
    'Position', [50 50 200 100], 'BackgroundColor', [1 0 0], 'String', ''), ...
  @(h) uipanel('Parent', h, 'Units', 'pixels', 'Position', [50 50 200 100], ...
    'BackgroundColor', [1 0 0]), ...
  @(h) uibutton(h, 'Position', [50 50 200 100], 'BackgroundColor', [1 0 0], 'Text', '')};
for k = 1:numel(builders)
  for inPanel = [false true]
    f = figure('Visible', 'off', 'Position', [10 10 400 300], 'Color', [0 0 0]);
    holder = f;
    if inPanel
      holder = uipanel('Parent', f, 'Units', 'pixels', 'Position', [0 0 400 300], ...
        'BackgroundColor', [0 0 0]);
    end
    builders{k}(holder);
    frame = getframe(f);
    close(f);
    image = frame.cdata;
    [rows, cols] = find((image(:, :, 1) > 200) & (image(:, :, 2) < 80) & (image(:, :, 3) < 80));
    assert_istrue(~isempty(rows));
    % the 200 x 100 component is there, at its size in pixels
    assert_istrue(abs((max(cols) - min(cols) + 1) - 200) <= 3);
    assert_istrue(abs((max(rows) - min(rows) + 1) - 100) <= 3);
  end
end
%=============================================================================
