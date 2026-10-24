%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
% A small image keeps its natural size (InitialMagnification default 100%):
% imshow sizes the axes to the image and resizes the figure around it instead
% of stretching a tiny image to fill the whole figure.
f = figure('Visible', 'off', 'Position', [100 100 500 400]);
h = imshow(reshape(uint8(mod(0:191, 256)), [8 8 3]));
ax = h.Parent;
drawnow();
ax.Units = 'pixels';
axpix = ax.Position;
asserts.isequal(axpix(3:4), [8 8]);
asserts.isequal(f.Position(3:4), [176 128]);
close(f);
%=============================================================================
% A pre-positioned full-figure axes is honoured (no resize): the image fills it.
f = figure('Visible', 'off', 'Position', [100 100 300 300]);
axes('Position', [0 0 1 1]);
h = imshow(uint8(zeros(8, 8, 3)));
drawnow();
asserts.isequal(f.Position(3:4), [300 300]);
asserts.isequal(h.Parent.Position, [0 0 1 1]);
close(f);
%=============================================================================
% 'InitialMagnification','fit' keeps the current figure and fills the axes.
f = figure('Visible', 'off', 'Position', [100 100 500 400]);
imshow(uint8(zeros(8, 8, 3)), 'InitialMagnification', 'fit');
drawnow();
asserts.isequal(f.Position(3:4), [500 400]);
close(f);
close all;
%=============================================================================
