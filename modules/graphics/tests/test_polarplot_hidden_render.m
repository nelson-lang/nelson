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
f = figure('Visible', 'off');
theta = 0:0.01:2 * pi;
rho = sin(2 * theta) .* cos(2 * theta);
polarplot(theta, rho);
ax = gca();
asserts.isequal(rlim(), [0 0.5]);
asserts.isequal(thetaticks(), 0:30:360);
thetaLabels = thetaticklabels();
asserts.isequal(thetaLabels{end}, ['330', char(176)]);
asserts.isfalse(any(strcmp(thetaLabels, ['360', char(176)])));
asserts.isequal(char(ax.Visible), 'off');
asserts.isequal(char(ax.Box), 'off');
asserts.isequal(ax.Color, [1 1 1]);
asserts.istrue(isempty(ax.XTick));
asserts.istrue(isempty(ax.YTick));
frame = getframe(f);
img = double(frame.cdata);
bluePixels = img(:, :, 3) > img(:, :, 1) + 30 & img(:, :, 2) > img(:, :, 1) + 15;
asserts.istrue(nnz(bluePixels) > 100);
close(f);
%=============================================================================
