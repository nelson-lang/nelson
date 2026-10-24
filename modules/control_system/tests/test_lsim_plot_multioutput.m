%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
% <--ADV-CLI MODE-->
%=============================================================================
A = [-1.5 -0.2 1.0;
     -0.2 -1.7 0.6;
      1.0 0.6 -1.4];
B = [1.5 0.6;
     -1.8 1.0;
      0 0];
C = [0 -0.5 -0.1;
      0.35 -0.1 -0.15
      0.65 0 0.6];
D = [0.5 0;
      0.05 0.75
      0 0];
sys = ss(A, B, C, D);
Tf = 10;
Ts = 0.1;
[uSq, t] = gensig("square", 4, Tf, Ts);
uP = gensig("pulse", 3, Tf, Ts);
u = [uSq uP];
f = figure();
lsim(sys, u, t);
axesHandles = findobj(f, 'Type', 'axes');
lineCounts = zeros(length(axesHandles), 1);
for k = 1:length(axesHandles)
  lineCounts(k) = length(findobj(axesHandles(k), 'Type', 'line'));
end
asserts.isequal(sort(lineCounts), [0; 3; 3; 3]);
% The gray input-signal overlays must use the dashed '--' line style.
inputColor = [0.4902 0.4902 0.4902];
lineHandles = findobj(f, 'Type', 'line');
inputLineCount = 0;
for k = 1:length(lineHandles)
  if isequal(lineHandles(k).Color, inputColor)
    inputLineCount = inputLineCount + 1;
    asserts.isequal(lineHandles(k).LineStyle, '--');
  end
end
asserts.isequal(inputLineCount, 6);
% The stacked panels share the time axis: every response subplot keeps the
% tick marks and the same tick positions, but the x number labels appear only
% under the bottom subplot.
responseAxes = [];
for k = 1:length(axesHandles)
  if (length(findobj(axesHandles(k), 'Type', 'line')) == 3)
    responseAxes = [responseAxes; axesHandles(k)];
  end
end
asserts.isequal(numel(responseAxes), 3);
yPos = zeros(numel(responseAxes), 1);
for k = 1:numel(responseAxes)
  yPos(k) = responseAxes(k).Position(2);
end
[~, order] = sort(yPos);
bottomAx = responseAxes(order(1));
asserts.isfalse(isempty(bottomAx.XTickLabel));
asserts.isequal(bottomAx.XTick, [0 2 4 6 8 10]);
asserts.isequal(bottomAx.XLim, [0 10]);
for k = 2:numel(order)
  upperAx = responseAxes(order(k));
  asserts.istrue(isempty(upperAx.XTickLabel));
  asserts.isequal(upperAx.XTick, bottomAx.XTick);
  asserts.isequal(upperAx.XLim, bottomAx.XLim);
end
close(f);
%=============================================================================
