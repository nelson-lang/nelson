%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function status = report_writeTap(results, outputFile) %#ok<NLS0026>
  if isempty(outputFile)
    error('Nelson:tests_manager:outputFilenameExpected', 'Output filename expected.');
  end
  cases = report_resultCases(results);
  lines = {'TAP version 13'; ['1..', int2str(length(cases))]};
  for k = 1:length(cases)
    prefix = 'ok ';
    if strcmp(report_caseOutcome(cases(k)), 'failed') || report_isErrorStatus(cases(k))
      prefix = 'not ok ';
    end
    line = [prefix, int2str(k), ' - ', report_caseName(cases(k))];
    if report_isSkippedStatus(cases(k))
      line = [line, ' # SKIP'];
    end
    lines{end + 1} = line;
    hasFailure = strcmp(report_caseOutcome(cases(k)), 'failed') || report_isErrorStatus(cases(k));
    if hasFailure && ~isempty(report_caseMessage(cases(k)))
      lines = [lines; report_tapDiagnostics(report_caseMessage(cases(k)))];
    end
  end
  filewrite(outputFile, lines);
  status = isfile(outputFile);
end
