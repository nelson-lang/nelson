%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Point the assistant at the shipped runnable example closest to the request, so
% the agent answers with (and can cite) real Nelson idiom instead of inventing it.
% The catalog is the per-module examples/index.json files that ship in the binary
% (bilingual: title/description in en_US and fr_FR), so a French request matches
% French metadata directly. One example at most, only on a clear match, so an
% unrelated demo never derails the answer.
%
% Skipped on the NFlow surface, which has its own example grounding.
function text = aiNelsonDemoContext(prompt, surfaceId)
  narginchk(1, 2);
  if nargin < 2
    surfaceId = '';
  end
  text = '';
  if strcmp(char(surfaceId), 'nflow')
    return
  end
  query = queryTokens(prompt);
  if isempty(query)
    return
  end
  entries = demoIndex();
  if isempty(entries)
    return
  end
  position = bestDemo(query, entries);
  if isempty(position)
    return
  end
  text = formatDemo(entries(position));
end
%=============================================================================
% Meaningful lower-case tokens (>= 4 chars, not a stop word) from the request.
function tokens = queryTokens(prompt)
  raw = regexp(nelson.ai.internal.aiFoldAccents(prompt), '[a-z]+', 'match');
  tokens = {};
  for k = 1:numel(raw)
    word = raw{k};
    if length(word) >= 4 && ~isStopWord(word) && ~any(strcmp(tokens, word))
      tokens{end + 1} = word; %#ok<AGROW>
    end
  end
end
%=============================================================================
function tf = isStopWord(word)
  stop = {'avec', 'pour', 'dans', 'une', 'des', 'les', 'mon', 'mes', 'comment', ...
    'faire', 'veux', 'peux', 'this', 'that', 'with', 'from', 'into', 'using', ...
    'make', 'want', 'nelson', 'please', 'show', 'give'};
  tf = any(strcmp(stop, word));
end
%=============================================================================
% Cached catalog built from every shipped examples/index.json. Each entry holds
% the localized title and description, the example file path, and the lower-case
% "haystack" tokens matched against (title + tags + description).
function entries = demoIndex()
  persistent cached
  if ~isempty(cached)
    entries = cached;
    return
  end
  entries = struct('title', {}, 'description', {}, 'file', {}, 'tokens', {});
  lang = getlanguage();
  modules = getmodules();
  for m = 1:numel(modules)
    indexPath = moduleIndexPath(modules{m});
    if isempty(indexPath)
      continue
    end
    entries = appendModuleEntries(entries, indexPath, lang);
  end
  cached = entries;
end
%=============================================================================
function indexPath = moduleIndexPath(moduleName)
  indexPath = '';
  try
    candidate = [modulepath(moduleName, 'root'), '/examples/index.json'];
  catch
    return
  end
  if isfile(candidate)
    indexPath = candidate;
  end
end
%=============================================================================
function entries = appendModuleEntries(entries, indexPath, lang)
  try
    doc = jsondecode(fileread(indexPath));
  catch
    return
  end
  if ~isstruct(doc) || ~isfield(doc, 'examples')
    return
  end
  folder = fileparts(indexPath);
  list = doc.examples;
  for k = 1:numel(list)
    item = elementAt(list, k);
    entry = makeEntry(item, folder, lang);
    if ~isempty(entry.title)
      entries(end + 1) = entry; %#ok<AGROW>
    end
  end
end
%=============================================================================
function entry = makeEntry(item, folder, lang)
  entry = struct('title', '', 'description', '', 'file', '', 'tokens', {{}});
  if ~isstruct(item)
    return
  end
  titleField = getField(item, 'title', '');
  descField = getField(item, 'description', '');
  entry.title = localized(titleField, lang);
  entry.description = localized(descField, lang);
  file = getField(item, 'file', '');
  if ~isempty(file)
    entry.file = [folder, '/', char(file)];
  end
  tags = getField(item, 'tags', {});
  % Match against ALL languages (not just the displayed one) so a French request
  % still finds a demo whose UI language is English and vice versa.
  hay = [allLanguages(titleField), ' ', allLanguages(descField), ' ', ...
    strjoin(toCellstr(tags), ' ')];
  entry.tokens = uniqueTokens(hay);
end
%=============================================================================
% A localized string from a {en_US, fr_FR} struct (falls back to en_US, then any
% field), or the value itself when it is already a plain string.
function value = localized(field, lang)
  if ischar(field) || isstring(field)
    value = char(field);
    return
  end
  value = '';
  if ~isstruct(field)
    return
  end
  if isfield(field, lang)
    value = char(field.(lang));
  elseif isfield(field, 'en_US')
    value = char(field.en_US);
  else
    names = fieldnames(field);
    if ~isempty(names)
      value = char(field.(names{1}));
    end
  end
end
%=============================================================================
% Every language value of a {en_US, fr_FR, ...} field joined, or the plain string.
function value = allLanguages(field)
  if ischar(field) || isstring(field)
    value = char(field);
    return
  end
  value = '';
  if ~isstruct(field)
    return
  end
  names = fieldnames(field);
  parts = cell(1, numel(names));
  for k = 1:numel(names)
    parts{k} = char(field.(names{k}));
  end
  value = strjoin(parts, ' ');
end
%=============================================================================
function tokens = uniqueTokens(text)
  raw = regexp(nelson.ai.internal.aiFoldAccents(text), '[a-z]+', 'match');
  tokens = {};
  for k = 1:numel(raw)
    if length(raw{k}) >= 4 && ~any(strcmp(tokens, raw{k}))
      tokens{end + 1} = raw{k}; %#ok<AGROW>
    end
  end
end
%=============================================================================
% The best demo for the query, or [] when nothing is clearly relevant. Needs at
% least two matched tokens; ties are broken toward the most focused (shortest)
% title so a generic entry does not win over a specific one.
function position = bestDemo(query, entries)
  position = [];
  scores = zeros(1, numel(entries));
  for k = 1:numel(entries)
    scores(k) = countMatches(query, entries(k).tokens);
  end
  top = max(scores);
  if top < 2
    return
  end
  winners = find(scores == top);
  if numel(winners) > 1
    lengths = zeros(1, numel(winners));
    for k = 1:numel(winners)
      lengths(k) = length(entries(winners(k)).title);
    end
    [~, pick] = min(lengths);
    winners = winners(pick);
  end
  position = winners(1);
end
%=============================================================================
function count = countMatches(query, tokens)
  count = 0;
  for k = 1:numel(query)
    if any(strcmp(tokens, query{k}))
      count = count + 1;
    end
  end
end
%=============================================================================
function text = formatDemo(entry)
  header = ['Nelson ships a runnable example for this: "', entry.title, '"'];
  if ~isempty(entry.description)
    header = [header, ' - ', entry.description];
  end
  header = [header, ' (', relativeExamplePath(entry.file), ').'];
  lines = {header};
  excerpt = codeExcerpt(entry.file);
  if ~isempty(excerpt)
    lines{end + 1} = 'Example code (excerpt, follow this style):';
    lines{end + 1} = excerpt;
  end
  text = strjoin(lines, char(10));
end
%=============================================================================
% "<module>/examples/<file>" for a readable, machine-independent citation.
function rel = relativeExamplePath(fullPath)
  rel = strrep(char(fullPath), '\', '/');
  marker = '/modules/';
  pos = strfind(rel, marker);
  if ~isempty(pos)
    rel = rel(pos(end) + length(marker):end);
  end
end
%=============================================================================
% The first few real code lines of the example (license header and comment-only
% lines skipped), bounded, so the agent sees idiom without a wall of text.
function excerpt = codeExcerpt(path)
  excerpt = '';
  if isempty(path) || ~isfile(path)
    return
  end
  try
    content = fileread(path);
  catch
    return
  end
  allLines = strsplit(content, char(10));
  kept = {};
  for k = 1:numel(allLines)
    line = strtrim(allLines{k});
    if isempty(line) || startsWith(line, '%') || startsWith(line, '//')
      continue
    end
    kept{end + 1} = allLines{k}; %#ok<AGROW>
    if numel(kept) >= 10
      break
    end
  end
  excerpt = strjoin(kept, char(10));
end
%=============================================================================
function value = getField(st, field, default)
  if isstruct(st) && isfield(st, field)
    value = st.(field);
  else
    value = default;
  end
end
%=============================================================================
function items = toCellstr(value)
  if iscell(value)
    items = value;
  elseif isstring(value)
    items = cellstr(value);
  elseif ischar(value)
    items = {value};
  else
    items = {};
  end
end
%=============================================================================
function element = elementAt(array, k)
  if iscell(array)
    element = array{k};
  else
    element = array(k);
  end
end
%=============================================================================
