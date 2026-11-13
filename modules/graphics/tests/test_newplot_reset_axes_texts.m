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
% NextPlot 'replace', newplot and 'cla reset' give new default title and labels.
% 'hold on', cla and NextPlot 'replacechildren' keep them.
function test_newplot_reset_axes_texts()
  names = {'Title', 'Subtitle', 'XLabel', 'YLabel'};
  resetActions = {@(ax) plot(ax, 1:3), @(ax) newplot(ax), @(ax) cla(ax, 'reset')};
  for k = 1:numel(resetActions)
    [f, ax, handles] = customizedAxes();
    resetActions{k}(ax);
    for n = 1:numel(names)
      h = ax.(names{n});
      assert_isequal(h.String, '');
      assert_isequal(h.FontSize, 11);
      assert_isequal(h.FontSizeMode, 'auto');
      assert_isequal(h.FontWeightMode, 'auto');
      assert_isequal(h.FontName, 'Helvetica');
      assert_isequal(h.FontAngle, 'normal');
      assert_isapprox(h.Color, [33 33 33] / 255, 1e-12);
      assert_isequal(h.ColorMode, 'auto');
      assert_isequal(h.PositionMode, 'auto');
      assert_isequal(char(h.Visible), 'on');
      assert_isequal(h.Interpreter, 'tex');
      assert_isequal(h.Rotation, 90 * strcmp(names{n}, 'YLabel'));
      assert_isequal(h.Units, 'data');
      assert_isequal(h.Tag, '');
      assert_isequal(h.UserData, []);
    end
    assert_isequal(ax.Title.FontWeight, 'bold');
    assert_isequal(ax.Subtitle.FontWeight, 'normal');
    assert_isequal(ax.XLabel.FontWeight, 'normal');
    assert_isequal(ax.ZLabel.String, '');
    assert_isequal(ax.ZLabel.FontSize, 10);
    close(f);
  end
  %=============================================================================
  keepActions = {@(ax) holdAndPlot(ax), @(ax) cla(ax), @(ax) replaceChildrenAndPlot(ax)};
  for k = 1:numel(keepActions)
    [f, ax, handles] = customizedAxes();
    keepActions{k}(ax);
    for n = 1:numel(names)
      h = ax.(names{n});
      assert_isequal(h, handles{n});
      assert_isequal(h.FontSize, 30);
      assert_isequal(h.FontSizeMode, 'manual');
      assert_isequal(h.FontWeight, 'normal');
      assert_isequal(h.Color, [1 0 0]);
      assert_isequal(char(h.Visible), 'off');
      assert_isequal(h.Tag, 'tg');
    end
    assert_isequal(ax.Title.String, 't');
    assert_isequal(ax.XLabel.String, 'x');
    close(f);
  end
  %=============================================================================
  % The new texts follow the axes fonts again.
  [f, ax] = customizedAxes();
  plot(ax, 1:3);
  ax.FontSize = 20;
  assert_isequal(ax.Title.FontSize, 22);
  assert_isequal(ax.XLabel.FontSize, 22);
  close(f);
  %=============================================================================
  % Repro of a replace plot.
  f = figure('Visible', 'off');
  ax = axes(f);
  title(ax, 't');
  xlabel(ax, 'x');
  ax.Title.FontWeight = 'normal';
  ax.Title.FontSize = 30;
  ax.XLabel.FontSize = 25;
  plot(ax, 1:3);
  R = {ax.Title.FontWeight, ax.Title.FontSize, ax.XLabel.FontSize, ax.Title.String};
  assert_isequal(R, {'bold', 11, 11, ''});
  close(f);
end
%=============================================================================
function [f, ax, handles] = customizedAxes()
  f = figure('Visible', 'off');
  ax = axes(f);
  title(ax, 't');
  subtitle(ax, 's');
  xlabel(ax, 'x');
  ylabel(ax, 'y');
  zlabel(ax, 'z');
  names = {'Title', 'Subtitle', 'XLabel', 'YLabel', 'ZLabel'};
  handles = cell(1, numel(names));
  for n = 1:numel(names)
    h = ax.(names{n});
    handles{n} = h;
    set(h, 'FontWeight', 'normal', 'FontSize', 30, 'Color', [1 0 0], ...
      'FontName', 'Courier', 'FontAngle', 'italic', 'Position', [0.5 0.5 0], ...
      'Visible', 'off', 'Interpreter', 'none', 'Rotation', 10, ...
      'UserData', 5, 'Tag', 'tg', 'Units', 'normalized');
  end
end
%=============================================================================
function holdAndPlot(ax)
  hold(ax, 'on');
  plot(ax, 1:3);
end
%=============================================================================
function replaceChildrenAndPlot(ax)
  ax.NextPlot = 'replacechildren';
  plot(ax, 1:3);
end
%=============================================================================
