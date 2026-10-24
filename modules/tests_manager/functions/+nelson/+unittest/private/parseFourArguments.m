%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [modules_list, files_to_test, result_outputfile, option, stoponfail] = ...
    parseFourArguments(param1, param2, param3, param4)
  [modules_list, files_to_test] = parseTarget(param1); %#ok<NLS0004>
  if ~isempty(modules_list) && ~isempty(files_to_test)
    error('Nelson:tests_manager:invalidArgument:nArgumentCellModulesFilenamesExpected', '#%d argument: cell of modules or filenames expected.', 1);
  end
  option = getOption(param2);
  if isempty(option)
    error('Nelson:tests_manager:invalidArgument:nArgumentOptionExpected', '#%d argument: option expected.', 2);
  end
  result_outputfile = parseOutputFile(param3, '#3 argument: filename expected.');
  stoponfail = parseStopOnFail(param4);
end
