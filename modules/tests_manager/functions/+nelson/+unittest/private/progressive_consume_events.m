%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function [updatedState, done] = progressive_consume_events(...
    currentState, polledEvents, allowFallback)
  updatedState = currentState;
  done = false;
  for k = 1:length(polledEvents)
    event = normalize_progressive_event(polledEvents{k});
    tests_manager_trace('events', ['event=', event.event_type, ...
      ' index=', mat2str(progressive_event_index(event)), ...
      ' reason=', event.reason]);
    if strcmp(event.event_type, 'done')
      done = true;
    else
      updatedState = progressive_record_pid(updatedState, event);
      caseIndex = round(progressive_event_index(event));
      valid = caseIndex >= 1 && caseIndex <= length(updatedState.cases);
      if valid && ~updatedState.completed(caseIndex)
        if strcmp(event.event_type, 'payload')
          updatedState = progressive_apply_payload(updatedState, caseIndex, event);
        elseif strcmp(event.event_type, 'process')
          updatedState = progressive_apply_process(...
            updatedState, caseIndex, event, allowFallback);
        end
      end
    end
  end
end
%=============================================================================
