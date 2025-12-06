
%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function demo_onCleanup(test_level)
    disp('Start of demo_onCleanup');
    cleanupObj = onCleanup(@()disp('Cleanup action executed'));
    if test_level == 1
        clear cleanupObj; % Explicitly call cleanup
        disp('Cleanup called explicitly');
    end
    if test_level == 2
       cleanupObj.cancel();
    end
    disp('Doing some work...');
    pause(1); % Simulate some work with a pause
    disp('End of demo_onCleanup');
end
%=============================================================================
