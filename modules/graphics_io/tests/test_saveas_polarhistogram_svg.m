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
addpath([modulepath('graphics_io'), '/tests/helpers']);
%=============================================================================
addpath([modulepath('graphics', 'tests')]);
svgFile = [tempname(), '.svg'];
pngFile = [tempname(), '.png'];
%=============================================================================
f = figure('Visible', 'off');
theta = linspace(0, 2 * pi, 48);
polarhistogram(theta, 8, 'FaceAlpha', 0.6);
saveas(f, svgFile);
saveas(f, pngFile);
close(f);
%=============================================================================
svgInfo = assert_valid_svg(svgFile, [], 'polygon');
svg = svgInfo.text;
asserts.isequal(contains(svg, '<image '), false);
asserts.isequal(contains(svg, 'fill="rgba(16,112,189,0.600000)"'), true);
%=============================================================================
img = imread(pngFile);
render_assert_not_blank(img);
rmfile(svgFile);
rmfile(pngFile);
%=============================================================================
