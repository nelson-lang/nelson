%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function resetPlot(h, event)
  % Callback function to reset the plot to its original state

  % Reset the labels of the X and Y axes
  xlabel('X-Axis');
  ylabel('Y-Axis');

  % Retrieve the plot object from the button's UserData
  p = h.UserData{1};

  % Generate the original X data for the plot
  x = linspace(0, 2*pi, 100);

  % Generate the original Y data as a sine wave
  y = sin(x);

  % Update the plot with the original X and Y data
  p.XData = x;
  p.YData = y;

  % Ensure the grid is on after resetting the plot
  grid on;

  % Retrieve the slider control from the button's UserData
  sld = h.UserData{2};

  % Reset the slider's value to the initial value stored in UserData
  sld.Value = h.UserData{3};

  % Update the plot's title to reflect the reset amplitude value
  title(['Sine Wave with Amplitude = ', num2str(sld.Value)]);
end
