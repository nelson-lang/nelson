%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = polarIsAxes(ax)
  tf = false;
  if ~(isgraphics(ax, 'axes') || isgraphics(ax, 'polaraxes'))
    return
  end
  ud = ax.UserData;
  tf = isstruct(ud) && isfield(ud, 'PolarAxes') && ud.PolarAxes;
end
%=============================================================================
