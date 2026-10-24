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
out = [tempdir(), 'saveas_violinplot_svg'];
if ~isfolder(out)
  mkdir(out);
end
%=============================================================================
f = figure('Visible', 'off');
violinplot([1 1 1 2 2 2], [1 2 2 3 4 5]);
drawnow();
svgFile = [out, '/violinplot.svg'];
saveas(f, svgFile);
close(f);
svgInfo = assert_valid_svg(svgFile, [], 'polygon');
svgText = svgInfo.text;
polygons = regexp(svgText, '<polygon[^>]*points="([^"]+)"', 'tokens');
asserts.istrue(numel(polygons) >= 3);
asserts.istrue(contains(svgText, 'fill="rgba(16,112,189,0.200000)"'));
%=============================================================================
