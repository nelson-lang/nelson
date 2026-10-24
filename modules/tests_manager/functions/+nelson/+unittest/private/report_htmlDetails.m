%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function lines = report_htmlDetails(testCase, index, expanded)
  className = 'details-row hidden';
  if expanded
    className = 'details-row';
  end
  lines = {['<tr class="', className, '" id="details-', int2str(index), '"><td colspan="7">']};
  lines = lines(:);
  lines{end + 1} = '<div class="details-grid">';
  lines = report_htmlAppendPreBlock(lines, 'Message', report_caseMessage(testCase));
  lines = report_htmlAppendPreBlock(lines, 'Stdout', report_caseStdout(testCase));
  lines = report_htmlAppendPreBlock(lines, 'Stderr', report_caseStderr(testCase));
  lines = report_htmlAppendPreBlock(lines, 'Reproduce', report_htmlReproduceCommand(testCase));
  if isfield(testCase, 'diagnostics')
    lines = report_htmlAppendPreBlock(...
      lines, 'Diagnostics', jsonprettyprint(jsonencode(testCase.diagnostics)));
  end
  if isfield(testCase, 'artifacts') && ~isempty(testCase.artifacts)
    lines = report_htmlAppendPreBlock(...
      lines, 'Artifacts', jsonprettyprint(jsonencode(testCase.artifacts)));
  end
  lines{end + 1} = '</div></td></tr>';
  lines = lines(:);
end
%=============================================================================
function command = report_htmlReproduceCommand(testCase)
  command = '';
  filename = report_htmlCaseFilename(testCase);
  if isempty(filename)
    return
  end
  filename = replace(filename, '''', '''''');
  kind = replace(report_htmlCaseKind(testCase), '''', '''''');
  command = ['nelson.unittest.run(''', filename, ''', ''Kind'', ''', kind, ''');'];
end
%=============================================================================
function filename = report_htmlCaseFilename(testCase)
  filename = '';
  if isfield(testCase, 'filename')
    filename = testCase.filename;
  elseif isfield(testCase, 'file')
    filename = testCase.file;
  elseif isfield(testCase, 'diagnostics') && isfield(testCase.diagnostics, 'metadata')
    filename = testCase.diagnostics.metadata;
  end
end
%=============================================================================
function lines = report_htmlAppendPreBlock(lines, title, value)
  block = report_htmlPreBlock(title, value);
  if ~isempty(block)
    lines = [lines(:); block(:)];
  end
end
%=============================================================================
