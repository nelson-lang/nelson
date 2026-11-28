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
% A figure is captured at the display resolution when it shows axes and no UI component,
% at its logical pixel size otherwise. The figure is shown or hidden.
%=============================================================================
figureSize = [560 420];
% kind of the extra child, whether the figure has an axes, whether the frame is scaled
cases = {'none', true, true; ...
  'uicontrol', true, false; ...
  'hidden uicontrol', true, true; ...
  'uipanel', true, false; ...
  'hidden uipanel', true, true; ...
  'uibuttongroup', true, false; ...
  'uitabgroup', true, false; ...
  'uitable', true, false; ...
  'uimenu', true, true; ...
  'annotation', true, true; ...
  'legend', true, true; ...
  'colorbar', true, true; ...
  'tiledlayout', false, true; ...
  'none', false, false; ...
  'uicontrol', false, false; ...
  'uipanel', false, false};
visible = {'on', 'off'};
for v = 1:2
  for k = 1:size(cases, 1)
    f = figure('Visible', visible{v}, 'Position', [100 100 figureSize]);
    if cases{k, 2}
      ax = axes('Parent', f);
      plot(ax, 1:3);
    end
    switch cases{k, 1}
      case 'uicontrol'
        uicontrol('Parent', f, 'Style', 'pushbutton', 'String', 'ok', 'Position', [10 10 60 20]);
      case 'hidden uicontrol'
        uicontrol('Parent', f, 'Style', 'pushbutton', 'String', 'ok', 'Position', [10 10 60 20], ...
          'Visible', 'off');
      case 'uipanel'
        uipanel('Parent', f, 'Position', [0.6 0.6 0.3 0.3]);
      case 'hidden uipanel'
        uipanel('Parent', f, 'Position', [0.6 0.6 0.3 0.3], 'Visible', 'off');
      case 'uibuttongroup'
        uibuttongroup('Parent', f, 'Position', [0.6 0.6 0.3 0.3]);
      case 'uitabgroup'
        group = uitabgroup('Parent', f, 'Position', [0.6 0.6 0.3 0.3]);
        uitab(group, 'Title', 'A');
      case 'uitable'
        uitable('Parent', f, 'Data', magic(3), 'Position', [300 250 200 100]);
      case 'uimenu'
        uimenu(f, 'Text', 'Menu');
      case 'annotation'
        annotation(f, 'textbox', [0.6 0.6 0.2 0.1], 'String', 'note');
      case 'legend'
        legend(ax, 'a');
      case 'colorbar'
        colorbar(ax);
      case 'tiledlayout'
        t = tiledlayout(f, 2, 2);
        plot(nexttile(t), 1:3);
    end
    drawnow();
    frame = getframe(f);
    scale = 1;
    if cases{k, 3}
      scale = f.DevicePixelRatio;
    end
    close(f);
    assert_isequal([size(frame.cdata, 2), size(frame.cdata, 1)], round(figureSize * scale));
  end
end
%=============================================================================
