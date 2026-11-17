%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Build a compact "Verified Nelson API" block for the functions named in a
% prompt, so the agent uses REAL signatures instead of inventing them. The source
% is the help index JSON that ships inside the binary (and is regenerated for
% wasm): modules/help_tools/help/nelson_help_<lang>.json, the same index help()
% and the MCP tools use. No source tree, no subprocess: works wherever Nelson runs.
%
% For each named function it injects its syntax, a one-line description, and its
% "see also" neighbours (so the agent can reach a better-fitting function). For a
% name with no exact entry it adds a conservative "did you mean" hint (a unique
% prefix completion, or a single close typo match) so a partial recall still
% resolves. The index holds only plain builtin/macro names, not package-qualified
% ones (nelson.ai.*), so those are not grounded here.
% Returns '' when nothing matches, so the caller can prepend unconditionally.
function text = aiApiGrounding(prompt)
  narginchk(1, 1);
  text = '';
  candidates = extractCandidates(char(prompt));
  if isempty(candidates)
    return
  end
  [index, names, lowerNames] = loadHelpIndex();
  if isempty(index)
    return
  end
  entries = {};
  seen = {};
  misses = {};
  maxEntries = 8;
  for k = 1:numel(candidates)
    candidate = candidates{k};
    if any(strcmp(seen, candidate))
      continue
    end
    position = find(strcmp(lowerNames, candidate), 1);
    if isempty(position)
      misses{end + 1} = candidate; %#ok<AGROW>
      continue
    end
    seen{end + 1} = candidate; %#ok<AGROW>
    entry = formatEntry(index.(names{position}));
    if ~isempty(entry)
      entries{end + 1} = entry; %#ok<AGROW>
    end
    if numel(entries) >= maxEntries
      break
    end
  end
  hints = nearMatchHints(misses, names, lowerNames);
  if isempty(entries) && isempty(hints)
    return
  end
  parts = {};
  if ~isempty(entries)
    header = ['Verified Nelson API (real signatures from this build''s help index; ', ...
      'use these exactly, do not invent other signatures):'];
    parts{end + 1} = [header, char(10), strjoin(entries, char(10))];
  end
  if ~isempty(hints)
    parts{end + 1} = ['Possible Nelson functions (no exact match found; verify with ', ...
      'help/doc before use):', char(10), strjoin(hints, char(10))];
  end
  text = strjoin(parts, [char(10), char(10)]);
end
%=============================================================================
% Lower-cased identifier-like tokens from the prompt, in order, deduplicated.
% Only tokens that could be a Nelson function name (letter start, >= 2 chars).
function candidates = extractCandidates(prompt)
  tokens = regexp(lower(prompt), '[a-z][a-z0-9_]+', 'match');
  candidates = {};
  for k = 1:numel(tokens)
    if ~any(strcmp(candidates, tokens{k}))
      candidates{end + 1} = tokens{k}; %#ok<AGROW>
    end
  end
end
%=============================================================================
% The decoded help index plus the FUNCTION name lists (NFlow block pages, tagged
% with a block: keyword_alias, are excluded so the grounding never matches a block
% name such as "comment" against an ordinary word). Cached across calls, since the
% file is static in a build. index is [] when the file is missing or undecodable.
function [index, names, lowerNames] = loadHelpIndex()
  persistent cachedIndex cachedNames cachedLower loadedFrom
  index = [];
  names = {};
  lowerNames = {};
  jsonPath = helpIndexPath();
  if isempty(jsonPath)
    return
  end
  if ~isempty(cachedIndex) && strcmp(loadedFrom, jsonPath)
    index = cachedIndex;
    names = cachedNames;
    lowerNames = cachedLower;
    return
  end
  try
    index = jsondecode(fileread(jsonPath));
  catch
    index = [];
    return
  end
  if ~isstruct(index)
    index = [];
    return
  end
  allNames = fieldnames(index);
  keep = true(numel(allNames), 1);
  for k = 1:numel(allNames)
    if isBlockPage(index.(allNames{k}))
      keep(k) = false;
    end
  end
  names = allNames(keep);
  lowerNames = lower(names);
  cachedIndex = index;
  cachedNames = names;
  cachedLower = lowerNames;
  loadedFrom = jsonPath;
end
%=============================================================================
% True for an NFlow block help page (its keyword_alias carries "block:<type>").
% Those are documented by the live block catalog on the NFlow surface, not grounded
% as functions here.
function tf = isBlockPage(page)
  tf = false;
  if ~isstruct(page) || ~isfield(page, 'keyword_alias')
    return
  end
  aliases = normalizeToCell(page.keyword_alias);
  for k = 1:numel(aliases)
    if contains(lower(char(aliases{k})), 'block:')
      tf = true;
      return
    end
  end
end
%=============================================================================
% Path to the shipped help index for the current language, falling back to the
% English index, then ''. Uses modulepath so it resolves inside the binary.
function jsonPath = helpIndexPath()
  jsonPath = '';
  try
    base = [modulepath('help_tools'), '/help/nelson_help_'];
  catch
    return
  end
  localized = [base, getlanguage(), '.json'];
  if isfile(localized)
    jsonPath = localized;
    return
  end
  english = [base, 'en_US.json'];
  if isfile(english)
    jsonPath = english;
  end
end
%=============================================================================
% One compact entry: name + short description, a few syntax lines, and the
% "see also" neighbours so the agent can pivot to a better-fitting function.
function entry = formatEntry(page)
  entry = '';
  if ~isstruct(page)
    return
  end
  name = getField(page, 'keyword', '');
  short = getField(page, 'short_description', '');
  head = ['- ', char(name)];
  if ~isempty(short)
    head = [head, ' - ', char(short)];
  end
  lines = {head};
  syntax = normalizeToCell(getField(page, 'syntax', {}));
  for s = 1:min(numel(syntax), 6)
    form = strtrim(char(syntax{s}));
    if ~isempty(form)
      lines{end + 1} = ['    ', form]; %#ok<AGROW>
    end
  end
  seeAlso = normalizeToCell(getField(page, 'see_also', {}));
  if ~isempty(seeAlso)
    related = {};
    for s = 1:min(numel(seeAlso), 8)
      item = strtrim(char(seeAlso{s}));
      if ~isempty(item)
        related{end + 1} = item; %#ok<AGROW>
      end
    end
    if ~isempty(related)
      lines{end + 1} = ['    see also: ', strjoin(related, ', ')]; %#ok<AGROW>
    end
  end
  entry = strjoin(lines, char(10));
end
%=============================================================================
% Conservative "did you mean" lines for names that did not match exactly. Kept
% low-noise on purpose (natural-language words must not suggest a function): a
% unique prefix completion for a longish stem, or a single close typo match for a
% long word. At most a few, so the block stays short.
function hints = nearMatchHints(misses, names, lowerNames)
  hints = {};
  used = {};
  for k = 1:numel(misses)
    if numel(hints) >= 4
      break
    end
    candidate = misses{k};
    suggestion = nearestName(candidate, names, lowerNames);
    if isempty(suggestion) || any(strcmp(used, suggestion))
      continue
    end
    used{end + 1} = suggestion; %#ok<AGROW>
    hints{end + 1} = ['- "', candidate, '" -> ', suggestion]; %#ok<AGROW>
  end
end
%=============================================================================
% A single safe suggestion for a miss, or '' when there is no confident one: a
% unique prefix completion for a longish stem, then a unique close typo match for
% a long word. Both scans are pruned by first characters so they stay cheap.
function suggestion = nearestName(candidate, names, lowerNames)
  suggestion = '';
  if length(candidate) >= 5
    suggestion = uniquePrefixName(candidate, names, lowerNames);
  end
  if isempty(suggestion) && length(candidate) >= 6
    suggestion = uniqueTypoName(candidate, names, lowerNames);
  end
end
%=============================================================================
% The one index name that starts with the candidate, or '' when none or several.
function name = uniquePrefixName(candidate, names, lowerNames)
  name = '';
  hit = find(strncmp(lowerNames, candidate, length(candidate)));
  hit = hit(~strcmp(lowerNames(hit), candidate));
  if numel(hit) == 1
    name = names{hit(1)};
  end
end
%=============================================================================
% The one index name within edit distance 1 of the candidate, or '' when none or
% several. Pruned by length and first two characters so the scan stays cheap.
function name = uniqueTypoName(candidate, names, lowerNames)
  name = '';
  n = length(candidate);
  first2 = candidate(1:2);
  found = '';
  for k = 1:numel(lowerNames)
    other = lowerNames{k};
    if abs(length(other) - n) > 1 || ~strncmp(other, first2, 2)
      continue
    end
    if editDistanceWithin1(candidate, other)
      if ~isempty(found)
        return  % not unique -> no confident suggestion
      end
      found = names{k};
    end
  end
  name = found;
end
%=============================================================================
% True when s and t differ by at most one single-character edit (insert, delete,
% or substitute). Cheap, early-exit; good enough for a typo hint.
function tf = editDistanceWithin1(s, t)
  if length(s) > length(t)
    shorter = t;
    longer = s;
  else
    shorter = s;
    longer = t;
  end
  if length(longer) - length(shorter) > 1
    tf = false;
    return
  end
  if length(shorter) == length(longer)
    tf = sum(shorter ~= longer) <= 1;
    return
  end
  % longer is one character more: accept when it is shorter with one insertion.
  i = 1;
  j = 1;
  skipped = false;
  while i <= length(shorter) && j <= length(longer)
    if shorter(i) == longer(j)
      i = i + 1;
      j = j + 1;
    elseif skipped
      tf = false;
      return
    else
      skipped = true;
      j = j + 1;
    end
  end
  tf = true;
end
%=============================================================================
function value = getField(st, field, default)
  if isfield(st, field)
    value = st.(field);
  else
    value = default;
  end
end
%=============================================================================
% jsondecode gives a char for a single-string array and a cell for several; a
% string array is possible too. Normalize all three to a cell of char rows.
function items = normalizeToCell(value)
  if iscell(value)
    items = value;
  elseif isstring(value)
    items = cellstr(value);
  elseif ischar(value)
    if isempty(value)
      items = {};
    else
      items = {value};
    end
  else
    items = {};
  end
end
%=============================================================================
