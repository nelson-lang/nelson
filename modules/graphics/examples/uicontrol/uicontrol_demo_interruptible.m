%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addpath([modulepath('graphics','root'), '/examples/uicontrol'])

% Create a figure window with the title 'Interruptible Example'
fig = figure('Name', 'Interruptible Example');

% Create a push button with the label 'Interruptible Button'
% Set the 'Interruptible' property to 'on' allowing it to be interrupted
btn1 = uicontrol('Style', 'pushbutton', ...
                 'String', 'Interruptible Button', ... % Text displayed on the button
                 'Position', [50 100 150 70], ...     % Position and size of the button [left bottom width height]
                 'Callback', @button1Callback, ...    % Function to call when the button is pressed
                 'Interruptible', 'on');              % Allows this button's callback to be interrupted by other callbacks

% Create a push button with the label 'Not interruptible Button'
% Set the 'Interruptible' property to 'off' making it non-interruptible
btn2 = uicontrol('Style', 'pushbutton', ...
                 'String', 'Not interruptible Button', ... % Text displayed on the button
                 'Position', [200 100 150 70], ...        % Position and size of the button [left bottom width height]
                 'Callback', @button2Callback, ...        % Function to call when the button is pressed
                 'Interruptible', 'off');                % Prevents this button's callback from being interrupted by other callbacks
