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
% a classic figure resized right after its creation reports before the assignment returns
f = figure('Position', [100 100 480 360], 'UserData', 0, 'SizeChangedFcn', @countResize);
drawnow();
drawnow();
asserts.isequal(f.UserData, 1);
f.Position = [100 100 520 400];
asserts.isequal(f.UserData, 2);
asserts.isequal(f.Position, [100 100 520 400]);
% no late duplicate
drawnow();
pause(0.3);
drawnow();
asserts.isequal(f.UserData, 2);
asserts.isequal(f.Position, [100 100 520 400]);
%=============================================================================
% the same holds once the window is settled
f.Position = [100 100 560 420];
asserts.isequal(f.UserData, 3);
drawnow();
pause(0.3);
asserts.isequal(f.UserData, 3);
asserts.isequal(f.Position, [100 100 560 420]);
close(f);
%=============================================================================
function countResize(src, ~)
  set(src, 'UserData', get(src, 'UserData') + 1);
end
%=============================================================================
