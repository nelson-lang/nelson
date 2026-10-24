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
f = figure('Visible', 'off', 'Position', [20 30 320 240]);
plot3([0 1 2], [0 1 0], [0 1 2]);
drawnow();
fh = __web_graphics_handle__(f, 'number');
original = f.Position;
frame = jsondecode(__web_display_list__(f));
for format = {'png', 'svg', 'pdf'}
  result = jsondecode(__web_figure_action__(fh, ...
    jsonencode(struct('action', 'export', 'format', format{1}, 'scale', 2))));
  asserts.istrue(numel(result.bytes) > 20);
  if strcmp(format{1}, 'png')
    asserts.isequal(double(result.bytes(1:8))(:)', [137 80 78 71 13 10 26 10]);
    w = double(result.bytes(17:20))(:)' * [16777216; 65536; 256; 1];
    h = double(result.bytes(21:24))(:)' * [16777216; 65536; 256; 1];
    asserts.isequal([w h], [frame.width frame.height] * 2);
  elseif strcmp(format{1}, 'pdf')
    asserts.isequal(char(result.bytes(1:4))(:)', '%PDF');
  else
    asserts.istrue(contains(char(result.bytes(:)'), '<svg'));
  end
  asserts.isequal(f.Position, original);
  restored = jsondecode(__web_display_list__(f));
  asserts.isequal([restored.width restored.height], [frame.width frame.height]);
end
close(f);
