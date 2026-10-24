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
out = [tempdir(), 'saveas_lit_surface_svg'];
if ~isdir(out)
  mkdir(out);
end
svgFile = [out, '/lit_surface.svg'];
f = figure('Visible', 'off');
surf(peaks(30), 'EdgeColor', 'none', 'FaceLighting', 'gouraud');
light('Position', [1 -1 1]);
material('shiny');
view(35, 28);
drawnow();
saveas(f, svgFile);
svgInfo = assert_valid_svg(svgFile, [], 'image');
svgText = svgInfo.text;
asserts.istrue(contains(svgText, '<image '));
asserts.istrue(contains(svgText, 'data:image/png;base64'));
close(f);
%=============================================================================
