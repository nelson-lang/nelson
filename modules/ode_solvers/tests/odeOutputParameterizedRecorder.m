%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function stop = odeOutputParameterizedRecorder(t, y, flag, scale, offset)
  global ODE_OUTPUT_PARAMETER_LOG;
  if isempty(ODE_OUTPUT_PARAMETER_LOG)
    ODE_OUTPUT_PARAMETER_LOG = {};
  end
  entry = struct();
  entry.flag = flag;
  entry.t = t;
  entry.y = y;
  entry.scale = scale;
  entry.offset = offset;
  ODE_OUTPUT_PARAMETER_LOG{end + 1} = entry;
  stop = false;
end
%=============================================================================
