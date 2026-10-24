%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ADV-CLI MODE-->
addpath([modulepath('graphics_io'), '/tests/helpers']);
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
addpath([modulepath('graphics', 'tests')]);
out = [tempdir(), 'bar3_svg_export_matches_png'];
if ~isfolder(out)
  mkdir(out);
end
%=============================================================================
bar3SvgCompare(out, 'bar3_user_case', 'bar3([1 4 7; 2 5 8; 3 6 9; 4 7 10]);');
bar3SvgCompare(out, 'bar3_dense', 'bar3(magic(6));');
bar3SvgCompare(out, 'bar3_grouped', 'bar3([1 2 3; 4 5 6; 7 8 9], ''grouped'');');
bar3SvgCompare(out, 'bar3_stacked', 'bar3([1 -2 3; -4 5 -6; 7 -8 9], ''stacked'');');
bar3SvgCompare(out, 'bar3h_user_case', 'bar3h([1 4 7; 2 5 8; 3 6 9; 4 7 10]);');
bar3SvgCompare(out, 'bar3h_stacked', 'bar3h([1 -2 3; -4 5 -6; 7 -8 9], ''stacked'');');
%=============================================================================
function bar3SvgCompare(out, name, scene)
  f = figure('Visible', 'off', 'Color', [1 1 1]);
  eval(scene);
  drawnow();
  svgFile = [out, '/', name, '.svg'];
  saveas(f, svgFile);
  assert_valid_svg(svgFile, [], 'polygon|image');
  close(f);
end
%=============================================================================
