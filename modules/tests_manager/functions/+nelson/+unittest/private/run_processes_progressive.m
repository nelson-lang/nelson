%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function events = run_processes_progressive(jobs, nbWorkers)
  traceProgressive(['start jobs=', int2str(length(jobs)), ...
    ' workers=', int2str(nbWorkers)]);
  handle = __nelson_unittest_process_supervisor_start__(jobs, nbWorkers);
  traceProgressive(['handle=', int2str(handle)]);
  events = {};
  try
    done = false;
    while ~done
      polled = __nelson_unittest_process_supervisor_poll__(handle, 25);
      for k = 1:length(polled)
        event = normalize_progressive_event(polled{k});
        traceProgressive(['event type=', event.event_type, ...
          ' kind=', event.kind, ...
          ' index=', mat2str(progressive_event_index(event)), ...
          ' job_id=', event.job_id]);
        if strcmp(event.event_type, 'done')
          done = true;
        else
          events{end + 1} = event;
        end
      end
    end
  catch ex
    __nelson_unittest_process_supervisor_stop__(handle);
    rethrow(ex);
  end
  __nelson_unittest_process_supervisor_stop__(handle);
end
%=============================================================================
function traceProgressive(message)
  if strcmp(getenv('NELSON_UNITTEST_TRACE_PROGRESSIVE'), '1')
    fprintf(stderr(), '[tests_manager progressive m] %s%s', message, newline());
  end
end
%=============================================================================
