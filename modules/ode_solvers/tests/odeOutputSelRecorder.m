%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function stop = odeOutputSelRecorder(t, y, flag)
  global ODE_OUTPUT_SEL_LOG;
  if isempty(ODE_OUTPUT_SEL_LOG)
    ODE_OUTPUT_SEL_LOG = {};
  end
  entry = struct();
  entry.flag = flag;
  entry.t = t;
  entry.y = y;
  ODE_OUTPUT_SEL_LOG{end + 1} = entry;
  stop = false;
end
%=============================================================================
