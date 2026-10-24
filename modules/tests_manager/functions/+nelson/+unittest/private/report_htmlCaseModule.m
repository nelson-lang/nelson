%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function moduleName = report_htmlCaseModule(testCase)
  moduleName = report_htmlModuleFromField(testCase);
  if isempty(moduleName)
    moduleName = report_htmlModuleFromClassname(testCase);
  end
  if isempty(moduleName)
    moduleName = report_htmlModuleFromCaseFilename(testCase);
  end
  if isempty(moduleName)
    moduleName = report_htmlModuleFromDiagnostics(testCase);
  end
end
%=============================================================================
function moduleName = report_htmlModuleFromField(testCase)
  moduleName = '';
  if isfield(testCase, 'module')
    moduleName = testCase.module;
  end
end
%=============================================================================
function moduleName = report_htmlModuleFromClassname(testCase)
  moduleName = '';
  if ~isfield(testCase, 'classname')
    return
  end
  parts = strsplit(testCase.classname, '.');
  if length(parts) >= 2
    moduleName = parts{end};
  end
end
%=============================================================================
function moduleName = report_htmlModuleFromCaseFilename(testCase)
  moduleName = '';
  if isfield(testCase, 'filename')
    moduleName = report_htmlModuleFromFilename(testCase.filename);
  end
end
%=============================================================================
function moduleName = report_htmlModuleFromDiagnostics(testCase)
  moduleName = '';
  if ~isfield(testCase, 'diagnostics')
    return
  end
  diagnostics = testCase.diagnostics;
  if isfield(diagnostics, 'metadata')
    moduleName = report_htmlModuleFromFilename(diagnostics.metadata);
  end
end
%=============================================================================
function moduleName = report_htmlModuleFromFilename(filename)
  moduleName = '';
  normalized = replace(filename, '\', '/');
  marker = '/modules/';
  markerIndex = strfind(normalized, marker);
  if isempty(markerIndex)
    return
  end
  moduleBegin = markerIndex(1) + length(marker);
  remaining = normalized(moduleBegin:end);
  separatorIndex = strfind(remaining, '/');
  if isempty(separatorIndex)
    return
  end
  moduleName = remaining(1:separatorIndex(1) - 1);
end
%=============================================================================
