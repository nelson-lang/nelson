%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function export(obj, filename)
  if isstring(filename) && isscalar(filename)
    filename = char(filename);
  end
  if ~ischar(filename)
    error('Nelson:types:filenameNotText', 'Filename must be a character vector or string scalar.');
  end
  [~, ~, ext] = fileparts(filename);
  switch lower(ext)
    case '.json'
      filewrite(filename, jsonprettyprint(jsonencode(obj.RawDiagnostics)));
    case '.csv'
      writetable(obj.Issues, filename);
    otherwise
      filewrite(filename, issuesToText(obj.RawDiagnostics));
  end
end
%=============================================================================
function txt = issuesToText(diagnostics)
  lines = strings(numel(diagnostics), 1);
  for k = 1:numel(diagnostics)
    lines(k) = sprintf('%s:%d:%d: %s %s %s', diagnostics(k).FullFilename, ...
      diagnostics(k).LineStart, diagnostics(k).ColumnStart, diagnostics(k).Severity, ...
      diagnostics(k).CheckID, diagnostics(k).Description);
  end
  txt = join(lines, newline());
end
%=============================================================================
