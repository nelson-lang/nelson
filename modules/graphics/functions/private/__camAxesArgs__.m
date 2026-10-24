%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Resolve an optional leading axes handle for the cam* commands.
% Returns the target axes and the remaining arguments.
function [ax, args] = __camAxesArgs__(inArgs)
  if (~isempty(inArgs) && isscalar(inArgs{1}) && isgraphics(inArgs{1}, 'axes'))
    ax = inArgs{1};
    args = inArgs(2:end);
  else
    ax = gca();
    args = inArgs;
  end
end
%=============================================================================
