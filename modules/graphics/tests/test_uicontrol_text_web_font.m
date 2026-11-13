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
% A text uicontrol is centred by default and keeps its String spaces.
f = figure('Visible', 'off', 'Position', [100 100 600 300]);
u = uicontrol(f, 'Style', 'text', 'String', '9.2  32.2  ÉÀé', 'FontSize', 26, ...
  'FontWeight', 'bold', 'Units', 'normalized', 'Position', [0.05 0.5 0.9 0.3]);
assert_isequal(u.HorizontalAlignment, 'center');
drawnow();
c = find_control(jsondecode(__web_display_list__(f)), '9.2  32.2  ÉÀé');
assert_isequal(c.hAlign, 'center');
assert_istrue(c.bold);
close(f);
%=============================================================================
% FontName reaches the web figure as a CSS family list with a generic fallback.
f = figure('Visible', 'off');
u = uicontrol(f, 'Style', 'text', 'String', 'mono', 'FontName', 'Courier New');
drawnow();
c = find_control(jsondecode(__web_display_list__(f)), 'mono');
assert_isequal(c.fontFamily, '"Courier New", monospace');
u.FontName = 'Times New Roman';
drawnow();
c = find_control(jsondecode(__web_display_list__(f)), 'mono');
assert_isequal(c.fontFamily, '"Times New Roman", serif');
% bitmap-only names are sent as the scalable family the native window draws
u.FontName = 'Courier';
drawnow();
c = find_control(jsondecode(__web_display_list__(f)), 'mono');
fixed = get(groot, 'FixedWidthFontName');
assert_isequal(c.fontFamily, ['"', fixed, '", monospace']);
u.FontName = 'MS Sans Serif';
drawnow();
c = find_control(jsondecode(__web_display_list__(f)), 'mono');
assert_istrue(endsWith(c.fontFamily, '", sans-serif'));
assert_isfalse(contains(c.fontFamily, 'MS Sans Serif'));
close(f);
%=============================================================================
function c = find_control(dl, text)
  c = [];
  controls = dl.controls;
  for k = 1:numel(controls)
    if iscell(controls)
      item = controls{k};
    else
      item = controls(k);
    end
    if strcmp(item.text, text)
      c = item;
      return
    end
  end
  error(['Control not found in web display list: ', text]);
end
%=============================================================================
