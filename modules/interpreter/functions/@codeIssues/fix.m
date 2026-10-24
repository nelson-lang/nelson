%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function obj = fix(obj)
  diagnostics = obj.RawDiagnostics;
  if isempty(diagnostics)
    return
  end
  suppressed = reshape([diagnostics.Suppressed], size(diagnostics));
  fixable = false(size(diagnostics));
  if isfield(diagnostics, 'Fixes')
    for k = 1:numel(diagnostics)
      fixable(k) = hasSafeFix(diagnostics(k));
    end
    fixable = fixable & ~suppressed;
  end
  files = unique({diagnostics(fixable).FullFilename});
  for k = 1:length(files)
    sameFile = reshape(strcmp({diagnostics.FullFilename}, files{k}), size(diagnostics));
    fileDiagnostics = diagnostics(fixable & sameFile);
    if isfield(fileDiagnostics, 'Fixes')
      applyDiagnosticFixes(files{k}, fileDiagnostics);
    else
      fixFile(files{k});
    end
  end
  obj = codeIssues(obj.Files, 'CodeAnalyzerConfiguration', obj.CodeAnalyzerConfiguration, ...
    'IncludeSubfolders', obj.IncludeSubfolders);
end
%=============================================================================
function applyDiagnosticFixes(filename, diagnostics)
  lines = fileread(filename, 'cell');
  text = char(join(string(lines), newline()));
  edits = [];
  for k = 1:numel(diagnostics)
    fixes = diagnostics(k).Fixes;
    if isfield(fixes, 'Safe')
      fixes = fixes([fixes.Safe]);
    end
    for j = 1:numel(fixes)
      edits = [edits, fixes(j)];
    end
  end
  if isempty(edits)
    return
  end
  edits = selectNonConflictingTextEdits(edits, lines);
  if isempty(edits)
    return
  end
  order = sortTextEdits(edits);
  for k = 1:numel(order)
    text = applyTextEdit(text, lines, edits(order(k)));
  end
  lines = splitTextLines(text);
  filewrite(filename, [join(string(lines), newline()), newline()]);
end
%=============================================================================
function tf = hasSafeFix(diagnostic)
  tf = false;
  if ~isfield(diagnostic, 'Fixes') || isempty(diagnostic.Fixes)
    return
  end
  fixes = diagnostic.Fixes;
  if isfield(fixes, 'Safe')
    tf = any([fixes.Safe]);
  else
    tf = true;
  end
end
%=============================================================================
function order = sortTextEdits(edits)
  keys = zeros(1, numel(edits));
  for k = 1:numel(edits)
    keys(k) = edits(k).LineStart * 1000000 + edits(k).ColumnStart;
  end
  [~, ascending] = sort(keys);
  order = fliplr(ascending);
end
%=============================================================================
function edits = selectNonConflictingTextEdits(edits, originalLines)
  starts = zeros(1, numel(edits));
  ends = zeros(1, numel(edits));
  lengths = zeros(1, numel(edits));
  valid = true(1, numel(edits));
  for k = 1:numel(edits)
    starts(k) = textOffset(originalLines, edits(k).LineStart, edits(k).ColumnStart);
    ends(k) = textOffset(originalLines, edits(k).LineEnd, edits(k).ColumnEnd);
    if ends(k) < starts(k)
      valid(k) = false;
      lengths(k) = -1;
    else
      lengths(k) = ends(k) - starts(k);
    end
  end
  candidates = find(valid);
  [~, byLength] = sort(-lengths(candidates));
  keep = false(1, numel(edits));
  for k = 1:numel(byLength)
    index = candidates(byLength(k));
    if ~hasOverlappingEdit(index, keep, starts, ends)
      keep(index) = true;
    end
  end
  edits = edits(keep);
end
%=============================================================================
function tf = hasOverlappingEdit(index, keep, starts, ends)
  tf = false;
  kept = find(keep);
  for k = 1:numel(kept)
    other = kept(k);
    if starts(index) < ends(other) && starts(other) < ends(index)
      tf = true;
      return
    end
  end
end
%=============================================================================
function text = applyTextEdit(text, originalLines, edit)
  startOffset = textOffset(originalLines, edit.LineStart, edit.ColumnStart);
  endOffset = textOffset(originalLines, edit.LineEnd, edit.ColumnEnd);
  if endOffset < startOffset
    return
  end
  prefix = '';
  suffix = '';
  if startOffset > 1
    prefix = text(1:startOffset - 1);
  end
  if endOffset <= length(text)
    suffix = text(endOffset:end);
  end
  replacement = edit.ReplacementText;
  text = [prefix, replacement, suffix];
end
%=============================================================================
function offset = textOffset(lines, lineIndex, columnIndex)
  lineIndex = max(1, lineIndex);
  columnIndex = max(1, columnIndex);
  offset = 1;
  beforeLine = min(lineIndex - 1, length(lines));
  for k = 1:beforeLine
    offset = offset + length(lines{k}) + 1;
  end
  if lineIndex <= length(lines)
    offset = offset + min(columnIndex - 1, length(lines{lineIndex}));
  end
end
%=============================================================================
function lines = splitTextLines(text)
  if isempty(text)
    lines = {};
    return
  end
  lines = regexp(text, newline(), 'split');
  if ~isempty(lines) && isempty(lines{end})
    lines(end) = [];
  end
end
%=============================================================================
function fixFile(filename)
  lines = fileread(filename, 'cell');
  for k = 1:length(lines)
    lines{k} = stripRight(lines{k});
  end
  filewrite(filename, [join(string(lines), newline()), newline()]);
end
%=============================================================================
function line = stripRight(line)
  while ~isempty(line) && isspace(line(end))
    line(end) = [];
  end
end
%=============================================================================
