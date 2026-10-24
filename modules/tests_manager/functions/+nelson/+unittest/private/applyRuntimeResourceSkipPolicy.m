%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_case = applyRuntimeResourceSkipPolicy(test_case)
  if (test_case.options.mpi_mode && ~have_mpi())
    test_case = markSkipped(test_case, 'Skip');
  end
  if (test_case.options.audio_input_required && ~have_audio_input())
    test_case = markSkipped(test_case, 'Skip');
  end
  if (test_case.options.audio_output_required && ~have_audio_output())
    test_case = markSkipped(test_case, 'Skip');
  end
  if (test_case.options.excel_required && ~have_excel())
    test_case = markSkipped(test_case, 'Skip');
  end
end
%=============================================================================
