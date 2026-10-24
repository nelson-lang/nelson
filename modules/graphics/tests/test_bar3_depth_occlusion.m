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
% bar3 draws each column as its own surface. In the software (print/saveas)
% capture every opaque surface bakes to a full-frame image, and those images
% composite in call order. Without a per-pixel depth test between them, a column
% drawn later would paint over a nearer column drawn earlier -- inter-column
% occlusion following draw order instead of depth. These checks pin the depth
% behaviour: the FRONT column must win the overlap no matter which column is
% drawn first, and a translucent bar must still blend (not wash out).
%=============================================================================
close all;
red = [0.85 0.1 0.1];
blue = [0.1 0.1 0.85];
%=============================================================================
% Front column red, back column blue: the front (red, drawn first) must occlude
% the back (blue, drawn second), so red covers more pixels than blue.
imgA = capture_bar3_overlap(red, blue);
asserts.istrue(count_red(imgA) > 0);
asserts.istrue(count_blue(imgA) > 0);
asserts.istrue(count_red(imgA) > 1.5 * count_blue(imgA));
%=============================================================================
% Swap the colours. The FRONT column (now blue) must still win: the dominant
% colour follows depth, not draw order. A draw-order regression would keep the
% later-drawn column (col2) on top in both captures instead.
imgB = capture_bar3_overlap(blue, red);
asserts.istrue(count_blue(imgB) > 1.5 * count_red(imgB));
%=============================================================================
% The front column covers the same screen area whichever colour it carries.
asserts.istrue(abs(count_red(imgA) - count_blue(imgB)) < 0.1 * count_red(imgA));
%=============================================================================
% A translucent bar3 must still render saturated, blended faces through the
% painter (back-to-front) path -- not collapse to white or vanish.
f = figure('Color', 'w', 'Visible', 'off', 'Position', [100 100 420 320]);
h = bar3([3 2; 1 4]);
set(h, 'FaceColor', [0.2 0.4 0.9], 'FaceAlpha', 0.5);
drawnow();
png = [tempdir(), 'test_bar3_depth_occlusion_trans.png'];
print(f, png, '-dpng');
timg = double(imread(png));
close(f);
bluish = timg(:, :, 3) > 120 & timg(:, :, 3) > timg(:, :, 1) + 20;
asserts.istrue(sum(bluish(:)) > 200);
%=============================================================================
function img = capture_bar3_overlap(c1, c2)
  % Two overlapping columns of equal height. col1 (x = 1) is drawn first and is
  % the nearer column at this view; col2 (x = 2) is drawn second and sits behind
  % it. A depth-correct capture keeps col1 (c1) in front of col2 (c2).
  f = figure('Color', 'w', 'Visible', 'off', 'Position', [100 100 420 320]);
  h = bar3([5 5; 5 5]);
  set(h(1), 'FaceColor', c1);
  set(h(2), 'FaceColor', c2);
  view(-130, 12);
  drawnow();
  png = [tempdir(), 'test_bar3_depth_occlusion_tmp.png'];
  print(f, png, '-dpng');
  img = double(imread(png));
  close(f);
end
%=============================================================================
function n = count_red(img)
  n = sum(sum(img(:, :, 1) > 120 & img(:, :, 2) < 90 & img(:, :, 3) < 90));
end
%=============================================================================
function n = count_blue(img)
  n = sum(sum(img(:, :, 3) > 120 & img(:, :, 1) < 90 & img(:, :, 2) < 90));
end
%=============================================================================
