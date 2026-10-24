%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function [executables, processArguments, nativeTimeout, nativeIndex, message] = ...
    collectNativeRunInputs(accumulator, message)
  executables = {};
  processArguments = {};
  nativeTimeout = [];
  nativeIndex = [];
  for i = 1:length(accumulator)
    if strcmp(accumulator(i).command, 'echo skip')
      message{i} = '';
    else
      if isempty(accumulator(i).executable)
        error('Nelson:tests_manager:executableExpected', 'Native test process supervisor requires an executable for each test.');
      end
      nativeIndex = [nativeIndex, i];
      executables = [executables, cellstr(accumulator(i).executable)];
      processArguments = [processArguments, cellstr(accumulator(i).process_arguments)];
      nativeTimeout = [nativeTimeout, accumulator(i).launcher_timeout];
    end
  end
end
%=============================================================================
