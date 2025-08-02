%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================

% Create a new figure window with specific properties
fig = figure('Name', 'UIControl Demo', 'NumberTitle', 'off', 'Position', [300, 300, 600, 400]);

% Create an axes object within the figure for plotting
ax = axes('Parent', fig, 'Position', [0.2, 0.4, 0.7, 0.5]);

% Set the limits for the Y-axis of the plot
ylim(ax, [-10, 10]);

% Label the X-axis
xlabel('X-Axis');

% Label the Y-axis
ylabel('Y-Axis');

% Enable grid lines on the plot
grid on;

% Set the initial value for the slider control
initialValue = 5;

% Create a push button UI control with a callback function to reset the plot
btn = uicontrol('Style', 'pushbutton', 'String', 'Reset Plot',  'Position', [50, 50, 100, 30], 'Callback', @resetPlot);

% Create a slider UI control with a callback function to update the plot
sld = uicontrol('Style', 'slider', 'Min', 1, 'Max', 10, 'Value', initialValue, 'Position', [200, 50, 300, 30], 'Callback', @updatePlot);

% Generate X data for the plot, ranging from 0 to 2*pi with 100 points
x = linspace(0, 2*pi, 100);

% Compute the sine of the X data to generate Y data
y = sin(x);

% Plot the X and Y data on the axes
p = plot(ax, x, y);

% Store the plot object in the slider's UserData for later use in the callback function
sld.UserData = p;

% Store the plot object, slider, and initial value in the button's UserData for use in the reset callback
btn.UserData = {p, sld, initialValue};
