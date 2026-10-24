%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_case = applyBuildResourceSkipPolicy(test_case)
  if (test_case.options.index_64_bit_required && ~have_64_bit_index_supported())
    test_case = markSkipped(test_case, 'Skip');
  end
  if (test_case.options.c_cpp_compiler_required && ~have_c_cpp_compiler())
    test_case = markSkipped(test_case, 'Skip');
  end
  if (test_case.options.native_architecture_required && ~is_native_platform())
    test_case = markSkipped(test_case, 'Skip');
  end
  if (test_case.options.release_only && ~is_release())
    test_case = markSkipped(test_case, 'Skip');
  end
end
%=============================================================================
