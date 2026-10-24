%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function [format, outputFile, verbose] = report_parseReportOptions(args) %#ok<NLS0026>
  format = 'console';
  outputFile = '';
  verbose = false;
  k = 1;
  while k <= length(args)
    if k == length(args)
      error('Nelson:tests_manager:optionValueExpected', 'Option value expected.');
    end
    name = args{k};
    value = args{k + 1};
    switch name %#ok<NLS0008>
      case 'Format'
        format = value;
      case 'OutputFile'
        outputFile = value;
      case 'Verbose'
        verbose = logical(value);
      otherwise
        error('Nelson:tests_manager:unsupportedOption', 'Unsupported option.');
    end
    k = k + 2;
  end
end
