%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function accumulator = run_test_batch(accumulator, abortedCode, nbWorkers)
  accumulator = configure_test_case_launchers(accumulator);
  [s, m, nativeResults] = run_accumulator(accumulator, nbWorkers); %#ok<NLS0004>
  for i = 1:length(accumulator)
    aborted = is_aborted_test(s(i), m{i}, accumulator(i), abortedCode);
    if ~isempty(nativeResults)
      accumulator(i) = applyNativeProcessResult(accumulator(i), nativeResults(i));
    end
    accumulator(i).msg = test_case_message(accumulator(i), m{i});
    accumulator(i).res_cmd = s(i);
    accumulator(i).exit_code = s(i);
    accumulator(i) = post_run_test_case(accumulator(i), aborted);
  end
end
%=============================================================================
