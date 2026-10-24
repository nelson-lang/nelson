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
addpath([modulepath('graphics', 'tests')]);
out = [tempdir(), 'saveas_wordcloud_svg_matches_png'];
if ~isfolder(out)
  mkdir(out);
end
%=============================================================================
terms = {'solver'; 'matrix'; 'plot'; 'graphics'; 'signal'; 'model'; 'tests'; 'data'; 'script'; 'analysis'};
counts = [42; 37; 31; 29; 22; 18; 16; 13; 11; 8];
T = table(terms, counts, 'VariableNames', {'Term', 'Count'});
f = figure('Visible', 'off', 'Position', [100 100 1074 647], 'Color', [1 1 1]);
wordcloud(f, T, 'Term', 'Count', 'MaxDisplayWords', 8, 'Shape', 'rectangle', ...
  'Color', [0.066 0.443 0.745]);
drawnow();
svgFile = [out, '/wordcloud_1.svg'];
saveas(f, svgFile);
svgInfo = assert_valid_svg(svgFile, [1074 647], {'rect', 'text'});
asserts.istrue(contains(svgInfo.text, 'solver'));
close(f);
%=============================================================================
