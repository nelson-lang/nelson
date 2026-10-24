%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [xe, ye, ie] = odeAppendEvent(xe, ye, ie, ev)
  if isempty(ev.t)
    return
  end
  xe = [xe, ev.t];
  ye = [ye, ev.y];
  ie = [ie, ev.i];
end
%=============================================================================
