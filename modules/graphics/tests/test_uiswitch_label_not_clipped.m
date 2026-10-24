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
% <--GUI MODE-->
%=============================================================================
skip_if_portable_graphics_backend('switch label getframe differs in portable rendering.');
% Non-regression: a horizontal (slider) uiswitch laid its two state labels in
% fixed 24 px boxes, so longer labels were clipped ("Hidden" -> "den",
% "Visible" -> "Visib"). The label areas are now sized to the text. getframe on
% a visible figure composites the real widget, so the clipped characters show
% as missing dark text. The right-hand label sits in the right third of the
% widget; clipping drops the dark text there to almost nothing.
%=============================================================================
f = uifigure('Position', [200 200 260 90], 'Color', [1 1 1]);
uiswitch(f, 'Items', {'Hidden', 'Visible'}, 'Value', 'Visible', ...
  'Position', [100 35 46 22]);
drawnow();
img = getframe(f).cdata;
close(f);
darkText = all(img < 90, 3);
% The full right label must be painted: its tail falls in the right third,
% which the clipping bug left empty. Measured: 32 with the full label, 1 when
% clipped.
width = size(img, 2);
rightZone = sum(sum(darkText(:, round(width * 0.67):end)));
assert_istrue(rightZone > 15);
% Both labels together carry far more ink than the two clipped stubs. Measured:
% 215 with the full labels, 137 when clipped.
assert_istrue(sum(darkText(:)) > 180);
%=============================================================================
