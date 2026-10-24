%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function nativeResult = run_processes_progressive_event_to_native(event)
  nativeResult = emptyNativeResults(1);
  fields = fieldnames(nativeResult);
  for k = 1:length(fields)
    name = fields{k};
    if isfield(event, name)
      nativeResult.(name) = event.(name);
    end
  end
  nativeResult.index = progressive_event_index(event);
end
%=============================================================================
