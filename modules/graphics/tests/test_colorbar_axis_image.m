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
% A docked colorbar hugs the drawn data box. When the peer axes locks its
% aspect ratio (axis image / axis equal) the data is drawn inside a smaller box
% centered in the axes Position rectangle, and the colorbar must follow that box
% instead of the full Position rectangle.
%=============================================================================
close all
%=============================================================================
% Wide figure: axis image draws a full-height square narrower than the axes box.
% The eastoutside colorbar must sit just right of the square, well inside the
% axes right edge.
f = figure('Visible', 'off', 'Position', [100 100 640 400]);
imagesc(magic(10));
axis image
cb = colorbar();
ax = gca();
axRight = ax.Position(1) + ax.Position(3);
squareRight = drawn_square_right(f, ax);
gap = cb.Position(1) - squareRight;
% Colorbar hugs the square (not the axes box): its left edge is inside the axes
% right edge and just past the square's right edge.
asserts.istrue(cb.Position(1) < axRight);
asserts.istrue(gap > 0);
asserts.istrue(gap < 0.05);
% Square is full-height here, so the colorbar keeps the axes height.
asserts.isapprox(cb.Position(4), ax.Position(4), 1e-6);
close(f);
%=============================================================================
% Tall figure: axis image draws a width-limited square shorter than the axes
% box. The colorbar cross-extent (height) shrinks to the square and is centered.
f = figure('Visible', 'off', 'Position', [100 100 420 760]);
imagesc(magic(10));
axis image
cb = colorbar();
ax = gca();
[squareBottom, squareTop] = drawn_square_vertical(f, ax);
asserts.istrue(cb.Position(4) < ax.Position(4) - 1e-3);
asserts.isapprox(cb.Position(2), squareBottom, 1e-3);
asserts.isapprox(cb.Position(2) + cb.Position(4), squareTop, 1e-3);
close(f);
%=============================================================================
% Control: without a locked aspect ratio the colorbar hugs the axes box, so its
% left edge is at (or past) the axes right edge.
f = figure('Visible', 'off', 'Position', [100 100 640 400]);
imagesc(magic(10));
cb = colorbar();
ax = gca();
asserts.istrue(cb.Position(1) >= ax.Position(1) + ax.Position(3) - 1e-6);
close(f);
%=============================================================================
function r = drawn_square_right(f, ax)
  figpx = f.Position(3:4);
  axpx = ax.Position .* [figpx(1), figpx(2), figpx(1), figpx(2)];
  if axpx(3) >= axpx(4)
    side = axpx(4);
    left = axpx(1) + (axpx(3) - side) / 2;
  else
    side = axpx(3);
    left = axpx(1);
  end
  r = (left + side) / figpx(1);
end
%=============================================================================
function [b, t] = drawn_square_vertical(f, ax)
  figpx = f.Position(3:4);
  axpx = ax.Position .* [figpx(1), figpx(2), figpx(1), figpx(2)];
  if axpx(3) >= axpx(4)
    side = axpx(4);
    bottom = axpx(2);
  else
    side = axpx(3);
    bottom = axpx(2) + (axpx(4) - side) / 2;
  end
  b = bottom / figpx(2);
  t = (bottom + side) / figpx(2);
end
%=============================================================================
