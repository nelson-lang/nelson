%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
reset(groot());
f = figure('Visible', 'off', 'Position', [100 100 120 120]);
axes('Position', [0 0 1 1]);
h = imshow(logical([0 1; 1 0]));
asserts.istrue(isgraphics(h, 'image'));
asserts.isequal(h.CData, logical([0 1; 1 0]));
drawnow();
frame = getframe(gca()).cdata;
topLeft = meanQuadrant(frame, [0.15 0.35], [0.15 0.35]);
topRight = meanQuadrant(frame, [0.15 0.35], [0.65 0.85]);
bottomLeft = meanQuadrant(frame, [0.65 0.85], [0.15 0.35]);
bottomRight = meanQuadrant(frame, [0.65 0.85], [0.65 0.85]);
asserts.istrue(all(topLeft < 32));
asserts.istrue(all(topRight > 220));
asserts.istrue(all(bottomLeft > 220));
asserts.istrue(all(bottomRight < 32));
close(f);
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 120 120]);
axes('Position', [0 0 1 1]);
h = imshow(true(20, 20));
asserts.istrue(isgraphics(h, 'image'));
ax = h.Parent;
asserts.isequal(char(ax.Visible), 'off');
drawnow();
frame = getframe(ax).cdata;
asserts.istrue(all(frame(:) > 220));
close(f);
close all;
%=============================================================================
function rgb = meanQuadrant(frame, rowBounds, colBounds)
  rows = size(frame, 1);
  cols = size(frame, 2);
  r1 = max(1, round(rows * rowBounds(1)));
  r2 = min(rows, round(rows * rowBounds(2)));
  c1 = max(1, round(cols * colBounds(1)));
  c2 = min(cols, round(cols * colBounds(2)));
  patch = double(frame(r1:r2, c1:c2, :));
  rgb = squeeze(mean(mean(patch, 1), 2))';
end
%=============================================================================
