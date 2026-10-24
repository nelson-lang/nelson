%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% fired = NFlow.internal.runStopFcnForRun(documentJson, resultJson)
%
% Fire a model's StopFcn from the GUI Run path. The editor runs the engine in
% C++ (not through sim()), so this bare-identifier helper is called by name
% (feval) from the interpreter thread once the run stops: it decodes the model
% and the engine result, and if the model carries a top-level 'stopFcn' it
% builds the same 'out' struct sim() exposes and runs the callback in the base
% workspace. Returns true when a callback was present and executed.
% See NFlow.internal.buildSimOutput, NFlow.internal.runStopFcn, sim.
%=============================================================================
function fired = runStopFcnForRun(documentJson, resultJson)
  narginchk(2, 2);
  fired = false;
  document = jsondecode(documentJson);
  if ~isfield(document, 'stopFcn')
    return
  end
  stopFcn = document.stopFcn;
  if ~(ischar(stopFcn) || (isstring(stopFcn) && isscalar(stopFcn)))
    return
  end
  if isempty(strtrim(char(stopFcn)))
    return
  end
  result = jsondecode(resultJson);
  out = NFlow.internal.buildSimOutput(result, document);
  NFlow.internal.runStopFcn(char(stopFcn), out);
  fired = true;
end
%=============================================================================
