%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
addpath([modulepath('graphics_io'), '/tests/helpers']);
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
addpath([modulepath('graphics', 'tests')]);
out = [tempdir(), 'saveas_parallelplot_svg'];
if ~isfolder(out)
  mkdir(out);
end
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 700 520], 'Color', [1 1 1]);
X = [1 10 100; 2 20 50; 3 30 0; 4 15 70];
parallelplot(X, 'CoordinateTickLabels', {'A', 'B', 'C'});
drawnow();
assertSavedFigure(f, out, 'parallelplot_1');
close(f);
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 700 520], 'Color', [1 1 1]);
T = table([1; 2; 3], [4; 5; 6], {'a'; 'a'; 'b'}, 'VariableNames', {'A', 'B', 'G'});
parallelplot(T, 'CoordinateVariables', {'A', 'B'}, 'GroupVariable', 'G');
drawnow();
assertSavedFigure(f, out, 'parallelplot_2');
close(f);
%=============================================================================
function assertSavedFigure(f, out, name)
  svgFile = [out, '/', name, '.svg'];
  saveas(f, svgFile);
  svgInfo = assert_valid_svg(svgFile, [700 520], {'polyline', 'text'});
  svgText = svgInfo.text;
  asserts.isequal(contains(svgText, '<image '), false);
end
%=============================================================================
