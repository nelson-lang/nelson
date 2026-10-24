%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_case = applyLanguageEngineSkipPolicy(test_case) %#ok<NLS0026>
  if (ismodule('python_engine') && test_case.options.python_environment_required)
    pe = pyenv();
    if (pe.Version == "")
      test_case = markSkipped(test_case, 'Skip');
    end
  end
  if (ismodule('julia_engine') && test_case.options.julia_environment_required)
    je = jlenv();
    if je.Version == ""
      je = tryFindJuliaEnvironment(je);
    end
    if (je.Version == "")
      test_case = markSkipped(test_case, 'Skip');
    end
  end
end
%=============================================================================
