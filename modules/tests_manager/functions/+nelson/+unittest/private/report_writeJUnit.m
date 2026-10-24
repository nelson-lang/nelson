%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function status = report_writeJUnit(results, outputFile)
  if isempty(outputFile)
    error('Nelson:tests_manager:outputFilenameExpected', 'Output filename expected.');
  end
  cases = report_resultCases(results);
  fp = fopen(outputFile, 'wt');
  if fp == -1
    status = false;
    return
  end
  fprintf(fp, '%s', ['<?xml version="1.0"?>', newline()]);
  fprintf(fp, '%s', ['<testsuites tests="', int2str(length(cases)), '" failures="', ...
    int2str(report_countOutcome(cases, 'failed')), '" errors="', int2str(report_countErrors(cases)), ...
    '" skipped="', int2str(report_countSkipped(cases)), '">', newline()]);
  fprintf(fp, '%s', ['  <testsuite name="nelson.unittest" tests="', int2str(length(cases)), '">', newline()]);
  for k = 1:length(cases)
    report_writeJUnitCase(fp, cases(k));
  end
  fprintf(fp, '%s', ['  </testsuite>', newline()]);
  fprintf(fp, '%s', ['</testsuites>', newline()]);
  fclose(fp);
  status = isfile(outputFile);
end
