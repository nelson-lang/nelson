%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ADV-CLI MODE-->
H = tf([1 0.1 7.5], [1 0.12 9 0 0]);
bode(H, {1 10}, '-.')
axesHandles = findobj(gcf(), 'Type', 'axes');
phaseAxes = [];
for k = 1:length(axesHandles)
  if isequal(axesHandles(k).YLim, [-180 -45])
    phaseAxes = axesHandles(k);
  end
end
asserts.istrue(isgraphics(phaseAxes, 'axes'));
asserts.isequal(phaseAxes.YTick, -180:45:-45);
asserts.istrue(phaseAxes.Position(2) >= 0.1);
phaseLines = findobj(phaseAxes, 'Type', 'line');
asserts.isequal(length(phaseLines), 1);
asserts.isequal(phaseLines(1).LineStyle, '-.');
close(gcf());
%=============================================================================
