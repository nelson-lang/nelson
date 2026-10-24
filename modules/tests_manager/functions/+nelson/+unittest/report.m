%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function status = report(results, varargin) %#ok<NLS0026>
  [format, outputFile, verbose] = report_parseReportOptions(varargin); %#ok<NLS0004>
  switch lower(format) %#ok<NLS0008>
    case 'json'
      status = report_writeJson(results, outputFile);
    case {'junit', 'xml'}
      status = report_writeJUnit(results, outputFile);
    case 'tap'
      status = report_writeTap(results, outputFile);
    case 'html'
      status = report_writeHtml(results, outputFile);
    case 'console'
      report_writeConsole(results, verbose);
      status = true;
    otherwise
      error('Nelson:tests_manager:unsupportedReportFormat', 'Unsupported report format.');
  end
end
