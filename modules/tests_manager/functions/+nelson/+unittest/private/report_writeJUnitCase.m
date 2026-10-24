%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function report_writeJUnitCase(fp, testCase)
  fprintf(fp, '%s', ['    <testcase name="', report_xmlEscape(report_caseName(testCase)), ...
    '" time="', mat2str(report_caseDuration(testCase), 2), ...
    '" classname="', report_xmlEscape(report_caseClassname(testCase)), '">', newline()]);
  if strcmp(report_caseOutcome(testCase), 'failed')
    fprintf(fp, '%s', ['      <failure message="test failed"><![CDATA[', ...
      report_cdataText(report_caseMessage(testCase)), ']]></failure>', newline()]);
  elseif report_isErrorStatus(testCase)
    fprintf(fp, '%s', ['      <error message="test error"><![CDATA[', ...
      report_cdataText(report_caseMessage(testCase)), ']]></error>', newline()]);
  elseif report_isSkippedStatus(testCase)
    fprintf(fp, '%s', ['      <skipped/>', newline()]);
  end
  if ~isempty(report_caseStdout(testCase))
    fprintf(fp, '%s', ['      <system-out><![CDATA[', ...
      report_cdataText(report_caseStdout(testCase)), ']]></system-out>', newline()]);
  end
  if ~isempty(report_caseStderr(testCase))
    fprintf(fp, '%s', ['      <system-err><![CDATA[', ...
      report_cdataText(report_caseStderr(testCase)), ']]></system-err>', newline()]);
  end
  fprintf(fp, '%s', ['    </testcase>', newline()]);
end
