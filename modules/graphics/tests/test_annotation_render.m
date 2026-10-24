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
fRender = figure('Visible', 'off', 'Position', [100 100 260 220], 'Color', [1 1 1]);
annotation('line', [0.05 0.35], [0.9 0.9], 'Color', 'red', 'LineWidth', 2);
annotation('arrow', [0.05 0.35], [0.75 0.75], 'Color', 'green', 'HeadStyle', 'vback2');
annotation('doublearrow', [0.05 0.35], [0.6 0.6], 'Color', 'blue', 'Head2Style', 'ellipse');
annotation('textarrow', [0.05 0.35], [0.45 0.45], 'String', 'txt', 'TextEdgeColor', 'black');
annotation('rectangle', [0.48 0.72 0.18 0.12], 'FaceColor', 'yellow', 'Color', 'red');
annotation('ellipse', [0.48 0.50 0.18 0.12], 'FaceColor', 'cyan', 'Color', 'blue');
annotation('textbox', [0.48 0.25 0.18 0.12], 'String', 'box', 'BackgroundColor', 'white');
drawnow();
renderFrame = getframe(fRender);
nonWhiteMask = renderFrame.cdata(:, :, 1) < 245 | renderFrame.cdata(:, :, 2) < 245 ...
  | renderFrame.cdata(:, :, 3) < 245;
nonWhite = any(nonWhiteMask(:));
asserts.istrue(nonWhite);
close(fRender);
%=============================================================================
