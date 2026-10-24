%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% NFlow.internal.runStopFcn(code, out)
%
% Run a model's StopFcn once a simulation stops: the
% callback text is executed in the base workspace, where the To Workspace
% variables already live, with the SimulationOutput exposed as 'out'. A failing
% callback warns instead of aborting the completed run. Shared by sim() and the
% GUI Run hook.
%=============================================================================
function runStopFcn(code, out)
  narginchk(2, 2);
  if ~(ischar(code) || (isstring(code) && isscalar(code)))
    return
  end
  code = strtrim(char(code));
  if isempty(code)
    return
  end
  assignin('base', 'out', out);
  try
    evalin('base', code);
  catch e
    warning('Nelson:nflow_engine:stopFcn', sprintf('StopFcn failed: %s', e.message));
  end
end
%=============================================================================
