%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% SelectionHighlight 'off' on an axes or on its figure hides the hover frame
% of the web front end: the display list flags that axes with highlight=false.
% Axes left 'on' carry no flag.
f = figure('Visible', 'off');
assert_isequal(char(f.SelectionHighlight), 'on');
left = subplot(1, 2, 1); plot(1:3);
right = subplot(1, 2, 2); plot(3:-1:1);
drawnow();
frame = jsondecode(__web_display_list__(f));
assert_isequal(numel(frame.rotatableAxes), 2);
assert_isfalse(isfield(axesEntry(frame, left), 'highlight'));
assert_isfalse(isfield(axesEntry(frame, right), 'highlight'));

left.SelectionHighlight = 'off';
drawnow();
frame = jsondecode(__web_display_list__(f));
assert_isfalse(axesEntry(frame, left).highlight);
assert_isfalse(isfield(axesEntry(frame, right), 'highlight'));

f.SelectionHighlight = 'off';
drawnow();
frame = jsondecode(__web_display_list__(f));
assert_isfalse(axesEntry(frame, left).highlight);
assert_isfalse(axesEntry(frame, right).highlight);

f.SelectionHighlight = 'on';
left.SelectionHighlight = 'on';
drawnow();
frame = jsondecode(__web_display_list__(f));
assert_isfalse(isfield(axesEntry(frame, left), 'highlight'));
assert_isfalse(isfield(axesEntry(frame, right), 'highlight'));
close(f);
%=============================================================================
% The root default applies to new axes, as in MATLAB.
set(0, 'DefaultAxesSelectionHighlight', 'off');
f = figure('Visible', 'off');
ax = axes('Parent', f);
assert_isequal(char(ax.SelectionHighlight), 'off');
set(0, 'DefaultAxesSelectionHighlight', 'on');
close(f);
%=============================================================================
function entry = axesEntry(frame, ax)
  boxes = frame.rotatableAxes;
  for index = 1:numel(boxes)
    if iscell(boxes)
      entry = boxes{index};
    else
      entry = boxes(index);
    end
    if entry.handle == __web_graphics_handle__(ax, 'number')
      return;
    end
  end
  error('No display list entry for the axes.');
end
%=============================================================================
