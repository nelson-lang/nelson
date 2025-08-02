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
% <--INTERACTIVE TEST-->
%=============================================================================
addpath([nelsonroot(),'/modules/graphics/tests/test_callback']);
% Create a figure window
fig = figure('Name', 'Interruptible Example');

% Create a push button with Interruptible set to 'on'
btn1 = uicontrol('Style', 'pushbutton', ...
                 'String', 'Interruptible Button', ...
                 'Position', [50 100 150 70], ...
                 'Callback', @button1Callback, ...
                 'Interruptible', 'on');

% Create a push button with Interruptible set to 'off'
btn2 = uicontrol('Style', 'pushbutton', ...
                 'String', 'Not interruptible Button', ...
                 'Position', [200 100 150 70], ...
                 'Callback', @button2Callback, ...
                 'Interruptible', 'off');

