%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function button1Callback(src, event)
  disp('Starting interrupt operation (Interruptible Button) ...');
  pause(5);
  disp('operation finished (Interruptible Button).');
end
