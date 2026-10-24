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
    parseTwoArguments(param1, param2)
  result_outputfile = [];
  [modules_list, files_to_test] = parseTarget(param1); %#ok<NLS0004>
  if ~isempty(modules_list) && ~isempty(files_to_test)
    error('Nelson:tests_manager:invalidArgument:nArgumentCellModulesFilenamesExpected', '#%d argument: cell of modules or filenames expected.', 1);
  end
  option = 'all_tests';
  if ischar(param1) && strcmp(param1, 'minimal_tests')
    option = 'unitary_tests';
  end
  stoponfail = haveStopOnFailOption(param2);
  if ~stoponfail
    option = getOption(param2);
    if isempty(option)
      files_from_module = getModuleTestFilesToTest(modules_list, param2);
      if ~isempty(files_from_module)
        files_to_test = files_from_module;
      else
        option = 'all_tests';
      end
      if isempty(files_from_module) && ischar(param2)
        result_outputfile = param2;
      end
    end
  end
end
