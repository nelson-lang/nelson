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
fig = figure('Position', [500, 500, 400, 300]);
figPosition = get(fig, 'Position');
figWidth = figPosition(3);
figHeight = figPosition(4);
buttonWidth = 100;
buttonHeight = 50;
buttonX = (figWidth - buttonWidth) / 2;
buttonY = (figHeight - buttonHeight) / 2;
fun = @(src, event)disp('Button Clicked!')
button = uicontrol('Style', 'pushbutton', 'String', 'Click Me!', 'Position', [buttonX, buttonY, buttonWidth, buttonHeight], 'Callback', fun);
pause(2);
%=============================================================================
fig = figure('Position', [500, 500, 400, 300]);
h = uicontrol('Style', 'pushbutton', 'Position', [50, 50, 100, 100]);