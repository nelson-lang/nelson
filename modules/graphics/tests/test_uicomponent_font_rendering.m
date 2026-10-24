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
% FontSize and FontWeight of UI components must reach the rendered text:
% a 28 px bold label is taller and inkier than the default one.
% getframe of a uifigure without axes returns its Position size, with the
% figure Color behind the components, whether the figure is shown (native
% capture) or hidden (software capture).
%=============================================================================
function test_uicomponent_font_rendering()
  for visible = {'on', 'off'}
    checkFont(@(p, t, a) uibutton(p, 'Text', t, a{:}), visible{1});
    checkFont(@(p, t, a) uibutton(p, 'state', 'Text', t, a{:}), visible{1});
    checkFont(@(p, t, a) uitogglebutton(whiteGroup(p), 'Text', t, a{:}), visible{1});
    checkFont(@(p, t, a) uiradiobutton(whiteGroup(p), 'Text', t, a{:}), visible{1});
    checkFont(@(p, t, a) uilabel(p, 'Text', t, a{:}), visible{1});
    checkFont(@(p, t, a) uicheckbox(p, 'Text', t, a{:}), visible{1});
    checkFont(@(p, t, a) uieditfield(p, 'Value', t, a{:}), visible{1});
    checkFont(@(p, t, a) uidropdown(p, 'Items', {t}, a{:}), visible{1});
    checkFont(@(p, t, a) uitextarea(p, 'Value', t, a{:}), visible{1});
    checkFont(@(p, t, a) uilistbox(p, 'Items', {t}, a{:}), visible{1});
  end
  checkFontChangedLater();
end
%=============================================================================
function g = whiteGroup(parent)
  g = uibuttongroup(parent, 'Position', [0 0 420 260], 'BackgroundColor', [1 1 1], 'BorderType', 'none');
end
%=============================================================================
function checkFont(maker, visible)
  [bigHeight, bigInk] = measure(maker, {'FontSize', 28, 'FontWeight', 'bold'}, visible);
  [defaultHeight, defaultInk] = measure(maker, {}, visible);
  % "Hg" spans about 11 px at the 12 px default and 26 px at 28 px bold
  assert_istrue(defaultHeight >= 8 && defaultHeight <= 14);
  assert_istrue(bigHeight >= 22 && bigHeight <= 30);
  assert_istrue(bigHeight > 1.8 * defaultHeight);
  assert_istrue(bigInk > 4 * defaultInk);
end
%=============================================================================
function checkFontChangedLater()
  % a font set after creation must be rendered too
  maker = @(p, t, a) uibutton(p, 'Text', t, a{:});
  [defaultHeight, defaultInk] = measure(maker, {}, 'on');
  blank = darkOf(makeFigure(maker, '', {}, 'on'));
  fig = makeFigure(maker, 'Hg', {}, 'on');
  drawnow();
  b = fig.Children(1);
  b.FontSize = 28;
  b.FontWeight = 'bold';
  [height, ink] = textInk(darkOf(fig), blank);
  assert_istrue(height > 1.8 * defaultHeight);
  assert_istrue(ink > 4 * defaultInk);
end
%=============================================================================
function [height, ink] = measure(maker, args, visible)
  % text pixels only: the same component without text is the baseline, so a
  % fixed-size indicator (radio button, check box) does not count as text
  blank = darkOf(makeFigure(maker, '', args, visible));
  [height, ink] = textInk(darkOf(makeFigure(maker, 'Hg', args, visible)), blank);
end
%=============================================================================
function fig = makeFigure(maker, text, args, visible)
  fig = uifigure('Position', [100 100 420 260], 'Color', [1 1 1], 'Visible', visible);
  maker(fig, text, [{'Position', [20 20 380 200], 'FontColor', [0 0 0]}, args]);
end
%=============================================================================
function dark = darkOf(fig)
  % dark pixels of the whole figure (borders and chrome are lighter)
  drawnow();
  F = getframe(fig);
  delete(fig);
  % tolerate display scaling / clamping: the capture need not be exactly
  % 420x260, only a real rendered window (text height and ink are compared as
  % fractions below, so the absolute size does not matter).
  assert_isequal(size(F.cdata, 3), 3);
  assert_istrue(size(F.cdata, 1) >= 150 && size(F.cdata, 2) >= 250);
  % the figure Color shows around the component, never a blank black surface
  assert_isequal(squeeze(F.cdata(5, 5, :))', uint8([255 255 255]));
  dark = all(double(F.cdata) < 100, 3);
end
%=============================================================================
function [height, ink] = textInk(dark, blank)
  text = dark & ~blank;
  rows = find(any(text, 2));
  if isempty(rows)
    height = 0;
  else
    height = rows(end) - rows(1) + 1;
  end
  ink = nnz(text);
end
%=============================================================================
