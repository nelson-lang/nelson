%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_case = create_test_case(filename, classname, runtimeOptions, parsedTags)
  if nargin < 4
    test_case = initialize_test_case(filename, classname, runtimeOptions);
  else
    test_case = initialize_test_case(filename, classname, runtimeOptions, parsedTags);
  end
  test_case = applySkipPolicy(test_case);
  if test_case.skip
    return
  end
  if ~isSupportedPlatform(test_case)
    test_case.status = 'Skip';
    test_case.skip = true;
    return
  end
end
%=============================================================================
