%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function updatePlot(h, event)
  % Callback function to update the plot based on the slider value

  % Get the current value of the slider, which determines the amplitude
  scale = h.Value;

  % Retrieve the plot object from the slider's UserData
  p = h.UserData;

  % Access the existing X data of the plot
  x = p.XData;

  % Compute the new Y data by scaling the sine wave based on the slider value
  y = sin(x) * scale;

  % Update the plot with the new X and Y data
  p.XData = x;
  p.YData = y;

  % Update the plot's title to reflect the current amplitude
  title(['Sine Wave with Amplitude = ', num2str(scale)]);

  % Ensure the grid remains on after updating the plot
  grid on;
end
