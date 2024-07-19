%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
images_path = [modulepath('graphics', 'tests'), '/images/'];
[img, map, alpha] = imread([images_path,'winter-fox-64x64-rgb32.jpg']);
%=============================================================================
fig = figure('Position', [500, 500, 400, 300]);
figPosition = get(fig, 'Position');
figWidth = figPosition(3);
figHeight = figPosition(4);
buttonWidth = 100;
buttonHeight = 100;
buttonX = (figWidth - buttonWidth) / 2;
buttonY = (figHeight - buttonHeight) / 2;
button = uicontrol('Style', 'pushbutton', 'Position', [buttonX, buttonY, buttonWidth, buttonHeight]);
button.CData = img;
pause(2);
%=============================================================================
fig = figure('Position', [500, 500, 400, 300]);
figPosition = get(fig, 'Position');
figWidth = figPosition(3);
figHeight = figPosition(4);
buttonWidth = 100;
buttonHeight = 100;
buttonX = (figWidth - buttonWidth) / 2;
buttonY = (figHeight - buttonHeight) / 2;
button = uicontrol('Style', 'pushbutton', 'Position', [buttonX, buttonY, buttonWidth, buttonHeight], 'CData', img);
button.CData = img;
pause(2);
%=============================================================================
fig = figure('Position', [500, 500, 400, 300]);
figPosition = get(fig, 'Position');
figWidth = figPosition(3);
figHeight = figPosition(4);
buttonWidth = 100;
buttonHeight = 100;
buttonX = (figWidth - buttonWidth) / 2;
buttonY = (figHeight - buttonHeight) / 2;
button = uicontrol('Style', 'pushbutton', 'String', 'Click Me!', 'Position', [buttonX, buttonY, buttonWidth, buttonHeight], 'CData', img);
button.CData = img;
pause(2);
%=============================================================================
hFig = figure('Position', [100, 100, 300, 300]);
imgSize = 50;  % Size of the image
[X, Y] = meshgrid(1:imgSize, 1:imgSize);
CData = cat(3, X/imgSize, Y/imgSize, zeros(imgSize));  
CData = im2double(CData);  % Ensure the image is of type double
hButton = uicontrol('Style', 'pushbutton',  'Position', [100, 100, 100, 100], 'CData', CData, 'String', 'Click Me!');
pause(2);
hButton.String = 'Hello World!';
pause(2);
hButton.CData = [];
pause(2);
%=============================================================================
