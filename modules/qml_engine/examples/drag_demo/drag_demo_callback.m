%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function drag_demo_callback(x, y)
  msg = ['x: ', int2str(x), ' ', 'y: ', int2str(y)];
  disp(msg);
end