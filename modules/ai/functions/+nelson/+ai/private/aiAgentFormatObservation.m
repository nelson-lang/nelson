%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Format a tool result as an observation line fed back to the model.
function text = aiAgentFormatObservation(name, result)
  if result.success
    body = result.output;
    if isempty(body) && ~isempty(result.data)
      body = result.data;
    end
  else
    body = ['ERROR: ', result.error];
  end
  text = ['Tool result (', name, '): ', body];
end
%=============================================================================
