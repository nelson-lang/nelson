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
addpath([modulepath('graphics_io'), '/tests/helpers']);
%=============================================================================
out = [tempdir(), 'saveas_figure_color'];
if ~isfolder(out)
  mkdir(out);
end
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 320 240]);
plot(1:3, 'b', 'LineWidth', 2);
axis off;
drawnow();
%=============================================================================
pngFile = [out, '/default_figure.png'];
svgFile = [out, '/default_figure.svg'];
saveas(f, pngFile);
saveas(f, svgFile);
%=============================================================================
pngImage = imread(pngFile);
assert_white_corner(pngImage);
svgInfo = assert_valid_svg(svgFile, [320 240], 'line|polyline|path');
% InvertHardcopy defaults to 'on': the default light gray figure background is
% exported as white.
asserts.istrue(contains(svgInfo.text, 'fill="rgba(255,255,255,1.000000)"'));
close(f);
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 320 240], 'Color', [0 0 0]);
plot(1:3, 'y', 'LineWidth', 2);
axis off;
drawnow();
%=============================================================================
% InvertHardcopy 'on' whitens only the default background: an explicitly set
% figure Color is preserved in the export (the reference keeps black too).
pngFile = [out, '/black_figure_inverted.png'];
svgFile = [out, '/black_figure_inverted.svg'];
saveas(f, pngFile);
saveas(f, svgFile);
%=============================================================================
pngImage = imread(pngFile);
assert_dark_corner(pngImage);
svgInfo = assert_valid_svg(svgFile, [320 240], 'line|polyline|path');
asserts.istrue(contains(svgInfo.text, 'fill="rgba(0,0,0,1.000000)"'));
asserts.isequal(get(f, 'Color'), [0 0 0]);
%=============================================================================
% InvertHardcopy 'off' keeps the on-screen figure Color in the export.
set(f, 'InvertHardcopy', 'off');
pngFile = [out, '/black_figure.png'];
svgFile = [out, '/black_figure.svg'];
saveas(f, pngFile);
saveas(f, svgFile);
%=============================================================================
pngImage = imread(pngFile);
assert_dark_corner(pngImage);
svgInfo = assert_valid_svg(svgFile, [320 240], 'line|polyline|path');
asserts.istrue(contains(svgInfo.text, 'fill="rgba(0,0,0,1.000000)"'));
close(f);
%=============================================================================
function assert_dark_corner(img)
  corner = double(reshape(img(5, 5, 1:3), 1, 3));
  asserts.istrue(max(corner) < 8);
end
%=============================================================================
function assert_white_corner(img)
  corner = double(reshape(img(5, 5, 1:3), 1, 3));
  asserts.istrue(min(corner) > 240);
end
%=============================================================================
