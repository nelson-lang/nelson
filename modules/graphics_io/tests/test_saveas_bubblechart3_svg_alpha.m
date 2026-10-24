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
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
out = [tempdir(), 'saveas_bubblechart3_svg_alpha'];
if ~isfolder(out)
  mkdir(out);
end
svgFile = [out, '/bubblechart3_alpha.svg'];
if isfile(svgFile)
  rmfile(svgFile);
end
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 620 460], 'Color', [1 1 1]);
t = 0:0.4:2 * pi;
bubblechart3(cos(t), sin(t), t, 30 + 20 * t, 'b');
drawnow();
saveas(f, svgFile);
close(f);
%=============================================================================
svgInfo = assert_valid_svg(svgFile, [620 460], 'polygon');
svgText = svgInfo.text;
asserts.istrue(contains(svgText, 'fill="rgba(0,0,255,0.600000)"'));
asserts.istrue(contains(svgText, 'stroke="rgba(0,0,255,1.000000)"'));
rmfile(svgFile);
%=============================================================================
