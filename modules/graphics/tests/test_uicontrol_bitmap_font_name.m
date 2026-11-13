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
% Bitmap-only font names (default 'MS Sans Serif', 'System', 'Courier', ...)
% keep their FontName value but are drawn with a scalable family.
%=============================================================================
close all
%=============================================================================
f = figure('Visible', 'off');
u = uicontrol(f, 'Style', 'text');
if ispc()
  assert_isequal(u.FontName, 'MS Sans Serif');
else
  assert_isequal(u.FontName, 'Helvetica');
end
u.FontName = 'System';
assert_isequal(u.FontName, 'System');
close(f);
%=============================================================================
pairs = {{'MS Sans Serif', 'Helvetica'}, {'System', 'Helvetica'}, ...
  {'Small Fonts', 'Helvetica'}, {'Courier', 'Courier New'}, ...
  {'Fixedsys', 'Courier New'}, {'Terminal', 'Courier New'}, ...
  {'MS Serif', 'Times New Roman'}};
for visible = {'on', 'off'}
  for k = 1:numel(pairs)
    bitmapFrame = textFrame(pairs{k}{1}, visible{1});
    scalableFrame = textFrame(pairs{k}{2}, visible{1});
    assert_isequal(bitmapFrame, scalableFrame);
  end
end
%=============================================================================
function cdata = textFrame(fontName, visible)
  f = figure('Visible', visible, 'Position', [100 100 600 200], 'Color', [1 1 1]);
  uicontrol(f, 'Style', 'text', 'String', '9.2  32.2  Hg', 'FontSize', 26, ...
    'FontWeight', 'bold', 'FontName', fontName, 'BackgroundColor', [1 1 1], ...
    'Units', 'normalized', 'Position', [0.05 0.3 0.9 0.4]);
  drawnow();
  frame = getframe(f);
  cdata = frame.cdata;
  close(f);
end
%=============================================================================
