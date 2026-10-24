%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Export a generated plot to a temporary PNG image.
t = linspace(0, 2 * pi, 200);
figureHandle = figure('Visible', 'off');
plot(t, sin(t), 'LineWidth', 1.5);
title('Sine wave');
filename = [tempdir(), 'nelson-sine.png'];
saveas(figureHandle, filename);
close(figureHandle);
disp(['Image written to: ', filename]);
