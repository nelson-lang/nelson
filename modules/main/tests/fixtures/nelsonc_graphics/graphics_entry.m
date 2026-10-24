%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function graphics_entry(mode)
  if nargin() == 0
    mode = 'batch';
  end
  f = figure('Visible', 'off', 'Position', [100 100 640 420], ...
    'Color', [1 1 1], 'Name', 'nelsonc graphics validation', 'NumberTitle', 'off');
  ax = axes('Parent', f, 'Position', [0.12 0.22 0.82 0.7]);
  x = linspace(0, 2 * pi, 101);
  line('Parent', ax, 'XData', x, 'YData', sin(x), 'Color', [0 0.4 0.7], 'LineWidth', 3);
  set(ax, 'XLim', [0 2 * pi], 'YLim', [-1.2 1.2], 'XGrid', 'on', 'YGrid', 'on');
  uicontrol(f, 'Style', 'pushbutton', 'String', 'Close validation', ...
    'Position', [225 20 190 35], 'Callback', {@close_button, f});
  drawnow();
  frame = getframe(f);
  if ~isequal(size(frame.cdata), [420 640 3]) || numel(unique(frame.cdata)) < 8
    error('Invalid packaged graphics frame.');
  end
  imwrite(frame.cdata, fullfile(pwd(), 'packaged-graphics.png'));
  fprintf('GUI_RENDER_OK\n');
  if strcmp(mode, 'batch')
    close(f);
  elseif strcmp(mode, 'offscreen')
    fprintf('GUI_OFFSCREEN_RETURN\n');
  else
    set(f, 'Visible', 'on', 'HandleVisibility', 'off');
    if strcmp(mode, 'manual')
      delay = 120;
    else
      delay = 0.25;
    end
    clock = timer('StartDelay', delay, 'TimerFcn', {@finish_first, f, mode});
    set(f, 'UserData', clock);
    start(clock);
    fprintf('GUI_ENTRY_RETURN\n');
  end
end

function close_button(source, event, f)
  fprintf('GUI_BUTTON_CALLBACK\n');
  delete(f);
end

function finish_first(source, event, f, mode)
  if strcmp(mode, 'hide')
    set(f, 'Visible', 'off');
    fprintf('GUI_HIDDEN_OK\n');
    return;
  end
  delete(f);
  if strcmp(mode, 'manual')
    return;
  end
  next = figure('Name', 'nelsonc second window', 'NumberTitle', 'off', ...
    'Position', [130 130 300 200], 'HandleVisibility', 'off');
  clock = timer('StartDelay', 0.25, 'TimerFcn', {@finish_last, next});
  set(next, 'UserData', clock);
  start(clock);
  fprintf('GUI_NEXT_WINDOW\n');
end

function finish_last(source, event, f)
  delete(f);
  fprintf('GUI_LIFETIME_OK\n');
end
