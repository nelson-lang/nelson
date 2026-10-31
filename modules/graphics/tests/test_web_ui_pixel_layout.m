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
f = uifigure('Visible', 'off', 'Position', [120 80 1000 680]);
field = uieditfield(f, 'numeric', 'Position', [145 555 125 24]);
for size = {[1478 691], [800 600]}
  dimensions = size{1};
  assert_istrue(__web_figure_size__(f, dimensions(1), dimensions(2)));
  frame = jsondecode(__web_display_list__(f));
  assert_isequal([frame.width frame.height], dimensions);
  assert_isequal([frame.controlLayoutWidth frame.controlLayoutHeight], [1000 680]);
  controls = frame.controls;
  if iscell(controls)
    item = controls{1};
  else
    item = controls(1);
  end
  assert_isequal(item.units, 'pixels');
  assert_isequal([item.x item.y item.w item.h], [144 102 125 24]);
  assert_isequal(field.Position, [145 555 125 24]);
end
close(f);
%=============================================================================
