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
f = uifigure('Visible', 'on', 'Position', [100 100 480 360]);
f.AutoResizeChildren = 'off';
f.UserData = 0;
p = uipanel(f, 'Position', [20 20 160 100], ...
  'SizeChangedFcn', @countPanelResize);
drawnow();
f.UserData = 0;
f.Position = [100 100 520 400];
drawnow();
pause(0.15);
drawnow();
asserts.isequal(f.UserData, 0);
asserts.isequal(p.Position, [20 20 160 100]);
asserts.isapprox(f.Position(3:4), [520 400], 1);
close(f);
%=============================================================================
function countPanelResize(source, event)
  f = ancestor(source, 'figure');
  f.UserData = f.UserData + 1;
end
%=============================================================================
