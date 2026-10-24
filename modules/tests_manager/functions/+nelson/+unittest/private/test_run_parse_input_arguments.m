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
    test_run_parse_input_arguments(nbArgs, args)
  stoponfail = false;
  modules_list = [];
  files_to_test = [];
  result_outputfile = [];
  option = 'all_tests';
  switch nbArgs %#ok<NLS0008>
    case 0
      modules_list = getAllModulesList();
    case 1
      [modules_list, files_to_test, option, stoponfail] = parseOneArgument(args{1}); %#ok<NLS0004>
    case 2
      [modules_list, files_to_test, result_outputfile, option, stoponfail] = ...
        parseTwoArguments(args{1}, args{2});
    case 3
      [modules_list, files_to_test, result_outputfile, option, stoponfail] = ...
        parseThreeArguments(args{1}, args{2}, args{3});
    case 4
      [modules_list, files_to_test, result_outputfile, option, stoponfail] = ...
        parseFourArguments(args{1}, args{2}, args{3}, args{4});
    otherwise
      error('Nelson:tooManyInputs', 'Too many input arguments.');
  end
end
