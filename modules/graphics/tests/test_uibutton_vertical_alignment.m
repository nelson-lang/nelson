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
% VerticalAlignment of uibutton, state button and uitogglebutton:
% property round-trip, then the rendered label position.
%=============================================================================
function test_uibutton_vertical_alignment()
  checkRoundTrip();
  checkRendered(@(fig) fig, @(parent, args) uibutton(parent, args{:}));
  checkRendered(@(fig) fig, @(parent, args) uibutton(parent, 'state', args{:}));
  % one button group covering the whole figure holds the toggle buttons
  checkRendered(@(fig) uibuttongroup(fig, 'Position', [0 0 420 260]), ...
    @(parent, args) uitogglebutton(parent, args{:}));
  checkHorizontalKeptWithVertical();
end
%=============================================================================
function checkRoundTrip()
  fig = uifigure('Visible', 'off');
  buttons = {uibutton(fig), uibutton(fig, 'state'), uitogglebutton(uibuttongroup(fig))};
  for k = 1:numel(buttons)
    b = buttons{k};
    assert_isequal(b.VerticalAlignment, 'center');
    b.VerticalAlignment = 'top';
    assert_isequal(b.VerticalAlignment, 'top');
    b.VerticalAlignment = 'bottom';
    assert_isequal(b.VerticalAlignment, 'bottom');
    didError = false;
    try
      b.VerticalAlignment = 'left';
    catch
      didError = true;
    end
    assert_istrue(didError);
    assert_isequal(b.VerticalAlignment, 'bottom');
  end
  b = uibutton(fig, 'VerticalAlignment', 'top', 'HorizontalAlignment', 'right', 'IconAlignment', 'top');
  assert_isequal(b.VerticalAlignment, 'top');
  assert_isequal(b.HorizontalAlignment, 'right');
  assert_isequal(b.IconAlignment, 'top');
  delete(fig);
end
%=============================================================================
function checkRendered(makeParent, makeButton)
  % tall buttons: the label must sit in the matching third of the button
  fig = uifigure('Position', [100 100 420 260]);
  parent = makeParent(fig);
  args = {'FontSize', 16, 'FontWeight', 'bold', 'FontColor', [0 0 0], 'BackgroundColor', [1 1 1]};
  makeButton(parent, [{'Text', {'top'; 'bottom'}, 'VerticalAlignment', 'bottom', 'Position', [250 20 60 180]}, args]);
  makeButton(parent, [{'Text', 'X', 'VerticalAlignment', 'top', 'Position', [330 20 60 180]}, args]);
  makeButton(parent, [{'Text', 'X', 'VerticalAlignment', 'center', 'Position', [170 20 60 180]}, args]);
  drawnow();
  F = getframe(fig);
  rows = labelRows(F.cdata, fig, [250 20 60 180]);
  assert_istrue(mean(rows) > 2 / 3);
  rows = labelRows(F.cdata, fig, [330 20 60 180]);
  assert_istrue(mean(rows) < 1 / 3);
  rows = labelRows(F.cdata, fig, [170 20 60 180]);
  assert_istrue(abs(mean(rows) - 0.5) < 0.1);
  delete(fig);
end
%=============================================================================
function checkHorizontalKeptWithVertical()
  fig = uifigure('Position', [100 100 420 260]);
  args = {'FontSize', 16, 'FontWeight', 'bold', 'FontColor', [0 0 0], 'BackgroundColor', [1 1 1], 'Text', 'X'};
  uibutton(fig, args{:}, 'VerticalAlignment', 'bottom', 'HorizontalAlignment', 'right', 'Position', [20 20 150 180]);
  uibutton(fig, args{:}, 'VerticalAlignment', 'top', 'HorizontalAlignment', 'left', 'Position', [200 20 150 180]);
  drawnow();
  F = getframe(fig);
  [rows, cols] = labelRows(F.cdata, fig, [20 20 150 180]);
  assert_istrue(mean(rows) > 2 / 3);
  assert_istrue(mean(cols) > 2 / 3);
  [rows, cols] = labelRows(F.cdata, fig, [200 20 150 180]);
  assert_istrue(mean(rows) < 1 / 3);
  assert_istrue(mean(cols) < 1 / 3);
  delete(fig);
end
%=============================================================================
function [rows, cols] = labelRows(cdata, fig, position)
  % dark label pixels inside the button, as fractions of its height (0 = top)
  % and width (0 = left)
  figPosition = fig.Position;
  sy = size(cdata, 1) / figPosition(4);
  sx = size(cdata, 2) / figPosition(3);
  top = round((figPosition(4) - position(2) - position(4)) * sy) + 1;
  left = round(position(1) * sx) + 1;
  h = round(position(4) * sy);
  w = round(position(3) * sx);
  inner = 3;
  % clamp the region to the captured frame: display scaling or window-manager
  % clamping can shrink the frame below the computed button rectangle.
  r0 = max(1, top + inner);
  r1 = min(size(cdata, 1), top + h - 1 - inner);
  c0 = max(1, left + inner);
  c1 = min(size(cdata, 2), left + w - 1 - inner);
  region = double(cdata(r0:r1, c0:c1, :));
  dark = all(region < 128, 3);
  [r, c] = find(dark);
  assert_istrue(numel(r) > 5);
  rows = (r + inner) / h;
  cols = (c + inner) / w;
end
%=============================================================================
