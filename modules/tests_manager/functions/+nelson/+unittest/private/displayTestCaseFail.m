%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function displayTestCaseFail(test_case) %#ok<NLS0026>
  if strcmp(test_case.status, 'Fail') == true
    disp(['    run(''', test_case.filename, ''')']);
    disp(['    ', _('Error:')]);
    if ~isempty(test_case.msg)
      if strcmp(class(test_case.msg), 'cell') == 1
        msg = test_case.msg(:)';
        for k = msg(1:3)
          disp(['      ', k{1}]);
        end
      else
        if strcmp(class(test_case.msg), 'struct') == true
          disp(['      ', test_case.msg.message]);
          displayFilenameAndLine(test_case.msg);
        else
          disp(['      ', test_case.msg]);
        end
      end
    else
      disp(test_case)
    end
    disp(' ');
  end
end
