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
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
out = [tempdir(), 'saveas_svg_uifigure_uiaxes_capture'];
if ~isfolder(out)
  mkdir(out);
end
%=============================================================================
f = uifigure('Visible', 'off', 'Position', [100 100 420 260]);
ax = uiaxes(f, 'Position', [45 45 330 175]);
x = 0:0.1:2 * pi;
line('Parent', ax, 'XData', x, 'YData', sin(x), 'LineWidth', 1.5);
drawnow();
svgFile = [out, '/uiaxes_capture.svg'];
saveas(f, svgFile);
svgInfo = assert_valid_svg(svgFile, [420 260], 'image');
svgText = svgInfo.text;
asserts.istrue(contains(svgText, '<image '));
asserts.istrue(contains(svgText, 'data:image/png;base64,'));
close(f);
%=============================================================================
