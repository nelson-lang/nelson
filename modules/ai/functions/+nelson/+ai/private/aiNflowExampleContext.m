%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% For the NFlow assistant, find the shipped .nflow example model closest to the
% request and inject a compact summary of it (blocks, signal wires, physical nets,
% solver) as a correct, runnable PATTERN to mirror. The live block catalog already
% says which blocks exist; this adds "here is a correct model like what you want",
% which is what makes first-shot wiring reliable. Example files ship in the binary
% (modules/nflow_blocks/examples), so this works wherever NFlow runs.
%
% Only the NFlow surface gets it. Returns '' unless there is a clear single match,
% so a loosely related example never misleads the agent.
function text = aiNflowExampleContext(prompt, surfaceId)
  narginchk(1, 2);
  if nargin < 2
    surfaceId = '';
  end
  text = '';
  if ~strcmp(char(surfaceId), 'nflow')
    return
  end
  query = requestTokens(prompt);
  if isempty(query)
    return
  end
  [files, tokensList] = exampleIndex();
  if isempty(files)
    return
  end
  position = bestMatch(query, tokensList);
  if isempty(position)
    return
  end
  text = summarizeExample(files{position});
end
%=============================================================================
% Meaningful tokens of the user's request, with French domain words mapped to the
% English the example names use (ressort->spring ...). The NFlow prompt carries the
% live catalog before "--- Request:"; match only the request part so the catalog's
% block names do not swamp the score.
function tokens = requestTokens(prompt)
  text = char(prompt);
  marker = 'Request:';
  pos = strfind(text, marker);
  if ~isempty(pos)
    text = text(pos(end) + length(marker):end);
  end
  raw = regexp(aiFoldAccents(text), '[a-z]+', 'match');
  tokens = {};
  for k = 1:numel(raw)
    word = raw{k};
    if length(word) < 3 || isGeneric(word)
      continue
    end
    tokens = addToken(tokens, word);
    english = frenchToEnglish(word);
    if ~isempty(english)
      tokens = addToken(tokens, english);
    end
  end
end
%=============================================================================
function tokens = addToken(tokens, word)
  if ~any(strcmp(tokens, word))
    tokens{end + 1} = word; %#ok<AGROW>
  end
end
%=============================================================================
% French domain vocabulary -> the English stem used in example file names. Mirrors
% the vocabulary of the nflow-assistant skill triggers.
function english = frenchToEnglish(word)
  map = { ...
    'ressort', 'spring'; 'masse', 'mass'; 'amortisseur', 'damper'; ...
    'bobine', 'inductor'; 'condensateur', 'capacitor'; 'resistance', 'resistor'; ...
    'pendule', 'pendulum'; 'moteur', 'motor'; 'thermique', 'thermal'; ...
    'bouilloire', 'kettle'; 'frein', 'brake'; 'redresseur', 'rectifier'; ...
    'electrique', 'electrical'; 'mecanique', 'mechanical'; 'chaleur', 'heat' };
  english = '';
  index = find(strcmp(map(:, 1), word), 1);
  if ~isempty(index)
    english = map{index, 2};
  end
end
%=============================================================================
% Words that carry no selective meaning for an example name.
function tf = isGeneric(word)
  generic = {'demo', 'acausal', 'causal', 'model', 'modele', 'test', 'example', ...
    'exemple', 'nflow', 'sim', 'simulation', 'construire', 'build', 'avec', 'with', ...
    'une', 'and', 'the', 'pour', 'for', 'dans'};
  tf = any(strcmp(generic, word));
end
%=============================================================================
% Cached list of example files and their name tokens (files are static in a build).
function [files, tokensList] = exampleIndex()
  persistent cachedFiles cachedTokens loadedFrom
  files = {};
  tokensList = {};
  root = exampleRoot();
  if isempty(root)
    return
  end
  if ~isempty(loadedFrom) && strcmp(loadedFrom, root)
    files = cachedFiles;
    tokensList = cachedTokens;
    return
  end
  files = findNflowFiles(root);
  tokensList = cell(1, numel(files));
  for k = 1:numel(files)
    [~, base] = fileparts(files{k});
    raw = regexp(lower(base), '[a-z]+', 'match');
    toks = {};
    for j = 1:numel(raw)
      if length(raw{j}) >= 3 && ~isGeneric(raw{j})
        toks = addToken(toks, raw{j});
      end
    end
    tokensList{k} = toks;
  end
  cachedFiles = files;
  cachedTokens = tokensList;
  loadedFrom = root;
end
%=============================================================================
function root = exampleRoot()
  root = '';
  try
    candidate = [modulepath('nflow_blocks', 'root'), '/examples'];
  catch
    return
  end
  if isfolder(candidate)
    root = candidate;
  end
end
%=============================================================================
function files = findNflowFiles(folder)
  files = {};
  entries = dir(folder);
  for k = 1:numel(entries)
    name = entries(k).name;
    if strcmp(name, '.') || strcmp(name, '..')
      continue
    end
    full = [folder, '/', name];
    if isfolder(full)
      files = [files, findNflowFiles(full)]; %#ok<AGROW>
    elseif endsWith(lower(name), '.nflow')
      files{end + 1} = full; %#ok<AGROW>
    end
  end
end
%=============================================================================
% Index of the best-matching example, or [] when nothing is close enough. A match
% needs either two matched tokens or one distinctive (>= 4 chars) one. When several
% examples tie on score (a family such as buck / buck-PID / buck-PWM) the base
% variant -- the one with the fewest name tokens -- is chosen, so a common request
% still gets a pattern instead of nothing.
function position = bestMatch(query, tokensList)
  position = [];
  scores = zeros(1, numel(tokensList));
  bestLen = zeros(1, numel(tokensList));
  for k = 1:numel(tokensList)
    [scores(k), bestLen(k)] = scoreTokens(query, tokensList{k});
  end
  top = max(scores);
  if top < 1
    return
  end
  winners = find(scores == top);
  if numel(winners) > 1
    counts = cellfun(@numel, tokensList(winners));
    [~, pick] = min(counts);
    winners = winners(pick);
  end
  only = winners(1);
  if top >= 2 || bestLen(only) >= 4
    position = only;
  end
end
%=============================================================================
function [score, longest] = scoreTokens(query, tokens)
  score = 0;
  longest = 0;
  for k = 1:numel(query)
    if any(strcmp(tokens, query{k}))
      score = score + 1;
      longest = max(longest, length(query{k}));
    end
  end
end
%=============================================================================
% A compact, bounded summary of one example model.
function text = summarizeExample(path)
  text = '';
  try
    doc = jsondecode(fileread(path));
  catch
    return
  end
  if ~isstruct(doc)
    return
  end
  name = getField(doc, 'name', '');
  if isempty(name)
    [~, name] = fileparts(path);
  end
  lines = {};
  lines{end + 1} = ['Closest shipped NFlow example: "', char(name), '" - a correct, ', ...
    'runnable model. Mirror its structure (adapt values); wire every input.'];
  lines = appendLine(lines, '  blocks: ', blockList(doc));
  lines = appendLine(lines, '  signal wires: ', signalWires(doc));
  lines = appendLine(lines, '  physical nets: ', physicalNets(doc));
  lines = appendLine(lines, '  ', solverLine(doc));
  text = strjoin(lines, char(10));
end
%=============================================================================
function lines = appendLine(lines, prefix, body)
  if ~isempty(body)
    lines{end + 1} = [prefix, body];
  end
end
%=============================================================================
function body = blockList(doc)
  blocks = fieldOrEmpty(doc, 'blocks');
  items = {};
  for k = 1:min(numel(blocks), 16)
    b = elementAt(blocks, k);
    if isstruct(b) && isfield(b, 'id') && isfield(b, 'type')
      items{end + 1} = [char(string(b.id)), ':', char(string(b.type))]; %#ok<AGROW>
    end
  end
  body = joinItems(items, numel(blocks) > 16);
end
%=============================================================================
% jsondecode yields a struct array for homogeneous items and a cell array for
% heterogeneous ones (blocks with different params); read either the same way.
function element = elementAt(array, k)
  if iscell(array)
    element = array{k};
  else
    element = array(k);
  end
end
%=============================================================================
% Signal wires in the 1-based scripted-API form the agent writes (stored indices
% are 0-based).
function body = signalWires(doc)
  conns = fieldOrEmpty(doc, 'connections');
  items = {};
  for k = 1:min(numel(conns), 16)
    c = elementAt(conns, k);
    if isstruct(c) && isfield(c, 'from') && isfield(c, 'to')
      fi = fieldOrDefault(c, 'fromIndex', 0) + 1;
      ti = fieldOrDefault(c, 'toIndex', 0) + 1;
      items{end + 1} = [char(string(c.from)), '/', num2str(fi), '->', ...
        char(string(c.to)), '/', num2str(ti)]; %#ok<AGROW>
    end
  end
  body = joinItems(items, numel(conns) > 16);
end
%=============================================================================
function body = physicalNets(doc)
  wires = fieldOrEmpty(doc, 'physicalWires');
  items = {};
  for k = 1:min(numel(wires), 10)
    w = elementAt(wires, k);
    if isstruct(w) && isfield(w, 'pins')
      pins = w.pins;
      if ischar(pins)
        pins = {pins};
      end
      items{end + 1} = ['{', strjoin(cellstr(string(pins)), ', '), '}']; %#ok<AGROW>
    end
  end
  body = joinItems(items, numel(wires) > 10);
end
%=============================================================================
function body = solverLine(doc)
  solver = getField(doc, 'solver', 'discrete');
  sampleTime = getField(doc, 'sampleTime', []);
  body = ['solver: ', char(string(solver))];
  if ~isempty(sampleTime)
    body = [body, ', sampleTime: ', num2str(sampleTime)];
  end
end
%=============================================================================
function value = fieldOrEmpty(st, field)
  if isfield(st, field)
    value = st.(field);
  else
    value = [];
  end
end
%=============================================================================
function value = fieldOrDefault(st, field, default)
  if isfield(st, field) && ~isempty(st.(field))
    value = st.(field);
  else
    value = default;
  end
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
function body = joinItems(items, truncated)
  if isempty(items)
    body = '';
    return
  end
  if truncated
    items{end + 1} = '...';
  end
  body = strjoin(items, ', ');
end
%=============================================================================
