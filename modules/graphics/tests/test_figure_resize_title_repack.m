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
% An auto axes Position is derived from OuterPosition with pixel insets
% (ticks, labels, title). Shrinking the figure after the title was set must
% re-derive it, or the title no longer fits above the axes and is clipped.
%=============================================================================
f = figure();
initialPosition = f.Position;
plot(1:10);
title('Damped oscillation');
ax = gca;
drawnow();
% baseline axes height at the default size; the exact value follows the figure pixel size
defaultHeight = ax.Position(4);
asserts.istrue(defaultHeight > 0.5);
%=============================================================================
f.Position = [initialPosition(1), initialPosition(2), 295, 127];
drawnow();
axesTop = ax.Position(2) + ax.Position(4);
outerTop = ax.OuterPosition(2) + ax.OuterPosition(4);
% The title (TightInset top) fits between the axes top and the outer edge.
asserts.istrue(axesTop + ax.TightInset(4) <= outerTop + 1e-3);
% shrinking re-derives a smaller axes height than the baseline
asserts.istrue(ax.Position(4) < defaultHeight);
%=============================================================================
% Growing the figure back restores the baseline layout.
f.Position = initialPosition;
% the grow-back relayout can settle over a few frames on a loaded runner, so poll for it
for k = 1:40
  drawnow();
  if abs(ax.Position(4) - defaultHeight) <= 1e-3
    break;
  end
  pause(0.05);
end
asserts.isapprox(ax.Position(4), defaultHeight, 1e-3);
%=============================================================================
close(f);
%=============================================================================
