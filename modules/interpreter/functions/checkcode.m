%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = checkcode(filenameOrNames, varargin)
  includeSuppressed = false;
  fullPath = false;
  includeCyclomatic = false;
  includeModifiedCyclomatic = false;
  configFile = '';
  returnStruct = false;
  returnString = false;

  for k = 1:length(varargin)
    option = normalizeTextOption(varargin{k});
    if strcmp(option, '-id')
      continue
    elseif strcmp(option, '-fullpath')
      fullPath = true;
    elseif strcmp(option, '-notok')
      includeSuppressed = true;
    elseif strcmp(option, '-cyc')
      includeCyclomatic = true;
    elseif strcmp(option, '-modcyc')
      includeModifiedCyclomatic = true;
    elseif strcmp(option, '-struct')
      returnStruct = true;
    elseif strcmp(option, '-string')
      returnString = true;
    elseif length(option) > 8 && strcmp(option(1:8), '-config=')
      configFile = option(9:end);
    else
      error('Nelson:checkcode:unsupportedOption', 'Unsupported checkcode option.');
    end
  end

  diagnostics = __code_analyzer__(filenameOrNames, false, configFile, includeSuppressed, ...
    fullPath, includeCyclomatic, includeModifiedCyclomatic);
  if returnString
    result = diagnosticsToText(diagnostics);
  elseif returnStruct || nargout > 0
    result = diagnosticsToCheckcodeStruct(diagnostics);
  else
    disp(diagnosticsToText(diagnostics));
    result = [];
  end

  if nargout > 0
    varargout{1} = result;
  end
end
%=============================================================================
function option = normalizeTextOption(option)
  if isstring(option) && isscalar(option)
    option = char(option);
  end
  if ~ischar(option)
    error('Nelson:types:optionNotText', 'Option must be a character vector or string scalar.');
  end
end
%=============================================================================
function out = diagnosticsToCheckcodeStruct(diagnostics)
  out = struct('id', {}, 'message', {}, 'fix', {}, 'line', {}, 'column', {});
  for k = 1:numel(diagnostics)
    out(k).id = diagnostics(k).CheckID;
    out(k).message = diagnostics(k).Description;
    out(k).fix = diagnostics(k).Fixability;
    out(k).line = diagnostics(k).LineStart;
    out(k).column = diagnostics(k).ColumnStart;
  end
end
%=============================================================================
function txt = diagnosticsToText(diagnostics)
  lines = strings(numel(diagnostics), 1);
  for k = 1:numel(diagnostics)
    lines(k) = sprintf('%s:%d:%d: %s %s %s', diagnostics(k).FullFilename, ...
      diagnostics(k).LineStart, diagnostics(k).ColumnStart, diagnostics(k).Severity, ...
      diagnostics(k).CheckID, diagnostics(k).Description);
  end
  txt = join(lines, newline());
end
%=============================================================================
