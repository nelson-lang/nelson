%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_case_res = applyRunfileResultStatus(test_case_res, s) %#ok<NLS0026>
  test_case_res = applyRunfileOutput(test_case_res, s);
  test_case_res.time = s.timing;
  switch (s.r) %#ok<NLS0008>
    case 0
      test_case_res.skip = false;
      test_case_res.status = 'Pass';
      test_case_res.msg = '';
    case 1
      test_case_res.skip = false;
      test_case_res.status = 'Fail';
      test_case_res.msg = s.msg;
    case 2
      test_case_res.skip = true;
      test_case_res.status = 'Skip';
      if (strcmp(s.msg, _('Testsuite skipped')) == false)
        test_case_res.msg = s.msg;
      else
        test_case_res.msg = '';
      end
    otherwise
      error('Nelson:tests_manager:unhandledCase', 'Case not managed.');
  end
end
