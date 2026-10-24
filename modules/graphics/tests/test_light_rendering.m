%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
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
close all
%=============================================================================
f2 = figure('Visible', 'off');
ax2 = axes('Parent', f2);
surf(ax2, peaks(20), 'FaceLighting', 'gouraud', 'EdgeColor', 'none', 'FaceColor', 'interp');
view(ax2, 3);
h = light(ax2, 'Color', [1 0 0], 'Position', [1 0 1], 'Style', 'infinite');
drawnow();
redFrame = getframe(f2).cdata;
h.Color = [0 0 1];
drawnow();
blueFrame = getframe(f2).cdata;
redBlueDelta = sum(abs(double(redFrame(:)) - double(blueFrame(:))));
h.Visible = 'off';
drawnow();
offFrame = getframe(f2).cdata;
h.Visible = 'on';
h.Style = 'local';
h.Position = [-5 0 5];
drawnow();
localFrame = getframe(f2).cdata;
h.Position = [5 0 5];
drawnow();
movedFrame = getframe(f2).cdata;
if redBlueDelta > 0
  asserts.istrue(sum(abs(double(blueFrame(:)) - double(offFrame(:)))) > 0);
  asserts.istrue(sum(abs(double(localFrame(:)) - double(movedFrame(:)))) > 0);
end
close(f2);
close all
%=============================================================================
