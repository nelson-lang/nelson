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
  % suite time in seconds: JUnit reporters sum it, a missing value shows as NaN
  totalTime = 0;
  for k = 1:length(cases)
    totalTime = totalTime + report_caseDuration(cases(k));
  end
  timeAttribute = sprintf('%.3f', totalTime);
  fprintf(fp, '%s', ['<?xml version="1.0"?>', newline()]);
  fprintf(fp, '%s', ['<testsuites tests="', int2str(length(cases)), '" failures="', ...
    int2str(report_countOutcome(cases, 'failed')), '" errors="', int2str(report_countErrors(cases)), ...
    '" skipped="', int2str(report_countSkipped(cases)), '" time="', timeAttribute, '">', newline()]);
  % one testsuite per module, so a CI report lists only the modules that failed
  moduleNames = {};
  for k = 1:length(cases)
    name = report_caseClassname(cases(k));
    if isempty(name)
      name = 'nelson.unittest';
    end
    if ~any(strcmp(moduleNames, name))
      moduleNames{end + 1} = name;
    end
  end
  for m = 1:length(moduleNames)
    moduleName = moduleNames{m};
    % the cases of this module, in their original order
    moduleCases = struct([]);
    for k = 1:length(cases)
      name = report_caseClassname(cases(k));
      if isempty(name)
        name = 'nelson.unittest';
      end
      if strcmp(name, moduleName)
        moduleCases = [moduleCases; cases(k)];
      end
    end
    moduleTime = 0;
    for k = 1:length(moduleCases)
      moduleTime = moduleTime + report_caseDuration(moduleCases(k));
    end
    fprintf(fp, '%s', ['  <testsuite name="', report_xmlEscape(moduleName), ...
      '" tests="', int2str(length(moduleCases)), ...
      '" failures="', int2str(report_countOutcome(moduleCases, 'failed')), ...
      '" errors="', int2str(report_countErrors(moduleCases)), ...
      '" skipped="', int2str(report_countSkipped(moduleCases)), ...
      '" time="', sprintf('%.3f', moduleTime), '">', newline()]);
    for k = 1:length(moduleCases)
      report_writeJUnitCase(fp, moduleCases(k));
    end
    fprintf(fp, '%s', ['  </testsuite>', newline()]);
  end
  fprintf(fp, '%s', ['</testsuites>', newline()]);
  fclose(fp);
  status = isfile(outputFile);
end
