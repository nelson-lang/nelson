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
fAlpha = figure('Visible', 'off', 'Position', [100 100 560 420], 'Color', 'white');
data = [2 4 6 7 8 7 5 2];
stem(data);
annotation('rectangle', [.74 .56 .1 .1], 'FaceColor', 'blue', 'FaceAlpha', .2);
drawnow();
alphaFrame = getframe(fAlpha);
% getframe returns the figure at its logical pixel size
regionAt = @(rows, cols) double(alphaFrame.cdata(rows(1):rows(2), cols(1):cols(2), :));
blueRegion = regionAt([145 185], [415 470]);
meanBlue = squeeze(mean(mean(blueRegion, 1), 2));
asserts.istrue(meanBlue(3) > meanBlue(1));
asserts.istrue(meanBlue(3) > meanBlue(2));
solidBlueRegion = regionAt([152 172], [440 460]);
solidBlueMean = squeeze(mean(mean(solidBlueRegion, 1), 2));
solidBlueStd = squeeze(std(reshape(solidBlueRegion, [], 3), 0, 1));
asserts.istrue(solidBlueMean(1) >= 150);
asserts.istrue(solidBlueMean(2) >= 150);
asserts.istrue(solidBlueMean(3) >= 240);
asserts.istrue(max(solidBlueStd) <= 60);
close(fAlpha);
%=============================================================================
