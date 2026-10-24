%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
%#ok<NLS0002,NLS0026>
function [modules_list, files_to_test, result_outputfile, option, stoponfail] = ...
    parseThreeArguments(param1, param2, param3)
  [modules_list, files_to_test] = parseTarget(param1); %#ok<NLS0004>
  if ~isempty(modules_list) && ~isempty(files_to_test)
    error('Nelson:tests_manager:invalidArgument:nArgumentCellModulesFilenamesExpected', '#%d argument: cell of modules or filenames expected.', 1);
  end
  stoponfail = haveStopOnFailOption(param2);
  if stoponfail
    option = 'all_tests';
  else
    option = getOption(param2);
    if isequal(size(option), [0 0]) && isdouble(option)
      if ischar(param1) && strcmp(param1, 'minimal_tests')
        option = 'unitary_tests';
      else
        option = 'all_tests';
      end
    end
  end
  result_outputfile = parseOutputFile(param3, 'filename expected.');
end
