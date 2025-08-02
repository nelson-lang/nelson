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
% <--INTERACTIVE TEST -->
%=============================================================================
h = figure()
waitfor(h);
% close figure
%=============================================================================
hFig = figure('Position', [300, 300, 300, 150]);
hButton = uicontrol('Style', 'togglebutton', 'String', 'Toggle Me', 'Position', [100, 50, 100, 40], 'Value', 0);
hButton.Callback = @(src, event) set(src, 'Value', 1);
waitfor(hButton, 'Value');
% press toggle button
%=============================================================================
hFig = figure('Position', [300, 300, 300, 150]);
hButton = uicontrol('Style', 'togglebutton', 'String', 'Toggle Me', 'Position', [100, 50, 100, 40], 'Value', 0);
hButton.Callback = @(src, event) set(src, 'Value', 1);
waitfor(hButton, 'Value', 1);
% press toggle button
%=============================================================================
