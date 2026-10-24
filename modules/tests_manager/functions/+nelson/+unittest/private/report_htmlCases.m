%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function cases = report_htmlCases(results)
  cases = report_resultCases(results);
  rawCases = report_htmlRawCases(results);
  count = min(length(cases), length(rawCases));
  for k = 1:count
    cases(k) = report_htmlEnrichCase(cases(k), rawCases(k));
  end
end
%=============================================================================
function testCase = report_htmlEnrichCase(testCase, rawCase)
  testCase = report_htmlEnrichModule(testCase, rawCase);
  testCase = report_htmlEnrichFilename(testCase, rawCase);
  testCase = report_htmlEnrichKind(testCase, rawCase);
end
%=============================================================================
function testCase = report_htmlEnrichModule(testCase, rawCase)
  if ~report_htmlCaseNeedsField(testCase, 'module')
    return
  end
  moduleName = report_htmlCaseModule(rawCase);
  if ~isempty(moduleName)
    testCase.module = moduleName;
  end
end
%=============================================================================
function testCase = report_htmlEnrichFilename(testCase, rawCase)
  if ~report_htmlCaseNeedsField(testCase, 'filename') || ~isfield(rawCase, 'filename')
    return
  end
  testCase.filename = rawCase.filename;
end
%=============================================================================
function testCase = report_htmlEnrichKind(testCase, rawCase)
  if ~report_htmlCaseNeedsField(testCase, 'kind')
    return
  end
  testCase.kind = report_htmlCaseKind(rawCase);
end
%=============================================================================
function need = report_htmlCaseNeedsField(testCase, fieldName)
  need = ~isfield(testCase, fieldName);
  if ~need
    value = testCase.(fieldName);
    need = isempty(value);
  end
end
%=============================================================================
