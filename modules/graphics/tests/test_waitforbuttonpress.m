%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--INTERACTIVE TEST-->
%=============================================================================
disp('Press a mouse button on figure to continue...');
w = waitforbuttonpress()
assert_isequal(w, 0);
%=============================================================================
disp('Press a key button on figure to continue...');
w = waitforbuttonpress()
assert_isequal(w, 1);
%=============================================================================
disp('Press X on figure to close figure and continue...');
assert_checkerror('w = waitforbuttonpress()', _('waitforbuttonpress exit because figure has been deleted.'))
%=============================================================================