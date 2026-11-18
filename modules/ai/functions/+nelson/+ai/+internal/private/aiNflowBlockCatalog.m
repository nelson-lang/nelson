%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% The NFlow block catalog of this build, for an NFlow request handled OFF the NFlow
% pane (web / Qt assistant). The embedded pane injects its own live block catalog,
% so this never fires there; everywhere else the NFlow skills ask the agent to use
% blocks "from the live block catalog", which off the pane would otherwise be absent.
%
% Two sections, each built only when it has content:
%   - Physical (acausal) components with their connection pins, from
%     NFlow.internal.acausalCatalog(), restricted to the physical domains the request
%     is about (electrical / thermal / mechanical) when it names one.
%   - Signal (causal) blocks whose name/description match the request, from the
%     shipped block help pages.
% Names, pins and descriptions are real (read live from the build); the exact
% add_block PATH is never fabricated - the closest shipped .nflow example
% (aiNflowExampleContext) shows the real path pattern. Returns '' off an NFlow
% request, on the pane, or when nothing matches.
function text = aiNflowBlockCatalog(prompt, surfaceId)
  narginchk(1, 2);
  if nargin < 2
    surfaceId = '';
  end
  text = '';
  if strcmp(char(surfaceId), 'nflow')
    return
  end
  prompt = char(prompt);
  selected = aiSelectedUserSkills(prompt, char(surfaceId));
  if ~hasNflowSkill(selected)
    return
  end
  parts = {};
  % Physical components are only relevant when the request is about physical
  % (acausal) modeling; a pure signal-diagram request gets the causal section only.
  if hasSkill(selected, 'nflow-physical-acausal')
    parts = appendPart(parts, acausalSection(prompt));
  end
  parts = appendPart(parts, causalSection(prompt));
  if isempty(parts)
    return
  end
  header = ['NFlow block catalog in this build (for the exact add_block path and ', ...
    'parameters, follow the shipped example above or the block''s help page):'];
  text = [header, char(10), strjoin(parts, char(10))];
end
%=============================================================================
function parts = appendPart(parts, text)
  if ~isempty(text)
    parts{end + 1} = text;
  end
end
%=============================================================================
% True when any selected skill is an NFlow one (the request is about NFlow).
function tf = hasNflowSkill(selected)
  tf = false;
  for k = 1:numel(selected)
    if strncmp(selected(k).name, 'nflow-', 6)
      tf = true;
      return
    end
  end
end
%=============================================================================
function tf = hasSkill(selected, name)
  tf = false;
  for k = 1:numel(selected)
    if strcmp(selected(k).name, name)
      tf = true;
      return
    end
  end
end
%=============================================================================
% Physical (acausal) components with pins, restricted to the request's domains.
function text = acausalSection(prompt)
  text = '';
  catalog = loadAcausalCatalog();
  if isempty(catalog)
    return
  end
  lines = groupLines(catalog, relevantDomains(prompt));
  if isempty(lines)
    return
  end
  intro = ['Physical (acausal) components - join pins with add_line using NAMED ', ...
    'endpoints (model, ''A/p'', ''B/n''):'];
  text = [intro, char(10), strjoin(lines, char(10))];
end
%=============================================================================
% The acausal catalog as a struct array, or [] when the accessor is absent.
function catalog = loadAcausalCatalog()
  catalog = [];
  try
    catalog = NFlow.internal.acausalCatalog();
  catch
  end
  if ~isstruct(catalog) || isempty(catalog)
    catalog = [];
  end
end
%=============================================================================
% The physical domains the request is about (electrical / thermal / translational /
% rotational), by cue words; empty means "no domain named", so all are kept.
function domains = relevantDomains(prompt)
  folded = nelson.ai.internal.aiFoldAccents(prompt);
  cues = { ...
    'electrical', {'electr', 'circuit', 'rlc', 'resist', 'condensa', 'capacit', ...
      'bobine', 'induct', 'diode', 'voltage', 'tension', 'courant', 'current'}; ...
    'thermal', {'therm', 'chaleur', 'heat', 'temperature'}; ...
    'translational', {'translat', 'masse', 'mass', 'ressort', 'spring', 'amorti', ...
      'damper', 'lineaire'}; ...
    'rotational', {'rotat', 'couple', 'torque', 'inertie', 'inertia', 'gear', ...
      'engren', 'arbre', 'shaft'} };
  domains = {};
  for i = 1:size(cues, 1)
    if anyCue(folded, cues{i, 2})
      domains{end + 1} = cues{i, 1}; %#ok<AGROW>
    end
  end
end
%=============================================================================
function tf = anyCue(folded, keywords)
  tf = false;
  for j = 1:numel(keywords)
    if contains(folded, keywords{j})
      tf = true;
      return
    end
  end
end
%=============================================================================
% One line per kept physical domain: "- <domain>: Name [pins], ...", capped. An
% empty keepDomains keeps every domain.
function lines = groupLines(catalog, keepDomains)
  lines = {};
  domains = distinctDomains(catalog);
  if ~isempty(keepDomains)
    domains = domains(ismember(domains, keepDomains));
  end
  maxPerDomain = 18;
  maxTotal = 70;
  total = 0;
  for i = 1:numel(domains)
    if total >= maxTotal
      break
    end
    room = min(maxPerDomain, maxTotal - total);
    [entries, extra] = domainEntries(catalog, domains{i}, room);
    if isempty(entries)
      continue
    end
    total = total + numel(entries);
    line = ['- ', domains{i}, ': ', strjoin(entries, ', ')];
    if extra > 0
      line = [line, ' (+', int2str(extra), ' more)'];
    end
    lines{end + 1} = line; %#ok<AGROW>
  end
end
%=============================================================================
% The physical domains present in the catalog, in first-seen order.
function domains = distinctDomains(catalog)
  domains = {};
  for k = 1:numel(catalog)
    d = domainOf(catalog(k));
    if ~any(strcmp(domains, d))
      domains{end + 1} = d; %#ok<AGROW>
    end
  end
end
%=============================================================================
% Up to room "Name [pins]" entries for one domain, plus how many were left out.
function [entries, extra] = domainEntries(catalog, domain, room)
  entries = {};
  extra = 0;
  for k = 1:numel(catalog)
    if ~strcmp(domainOf(catalog(k)), domain)
      continue
    end
    if numel(entries) >= room
      extra = extra + 1;
    else
      entries{end + 1} = blockEntry(catalog(k)); %#ok<AGROW>
    end
  end
end
%=============================================================================
function d = domainOf(entry)
  d = 'other';
  if isfield(entry, 'domain') && ~isempty(entry.domain)
    d = char(entry.domain);
  end
end
%=============================================================================
% "Name [p, n]" for a catalog entry, with its pin names in brackets.
function entry = blockEntry(block)
  name = '';
  if isfield(block, 'name')
    name = char(block.name);
  end
  pins = pinNames(block);
  if isempty(pins)
    entry = name;
  else
    entry = [name, ' [', strjoin(pins, ', '), ']'];
  end
end
%=============================================================================
% The pin NAMES of a block (the endpoint used in add_line), from its pins cell of
% {name, label} pairs.
function names = pinNames(block)
  names = {};
  if ~isfield(block, 'pins') || ~iscell(block.pins)
    return
  end
  for k = 1:numel(block.pins)
    pin = block.pins{k};
    if iscell(pin) && ~isempty(pin)
      names{end + 1} = char(pin{1}); %#ok<AGROW>
    elseif ischar(pin)
      names{end + 1} = pin; %#ok<AGROW>
    end
  end
end
%=============================================================================
% Signal (causal) blocks whose name or description matches the request, best first.
function text = causalSection(prompt)
  text = '';
  pages = loadCausalBlockPages();
  if isempty(pages)
    return
  end
  [~, words] = nelson.ai.internal.aiPromptWords(prompt);
  words = signalWords(words);
  if isempty(words)
    return
  end
  entries = rankCausal(pages, words);
  if isempty(entries)
    return
  end
  maxCausal = 12;
  if numel(entries) > maxCausal
    entries = entries(1:maxCausal);
  end
  intro = 'Signal (causal) blocks matching your request:';
  text = [intro, char(10), strjoin(entries, char(10))];
end
%=============================================================================
% The request words usable as block-match signal: at least 3 characters, not a
% common or generic-description stop word (so "avec"/"the"/"signal" never match
% every block), and expanded with the English block term for common French words.
function kept = signalWords(words)
  stop = {'the', 'and', 'for', 'with', 'une', 'les', 'des', 'avec', 'pour', ...
    'dans', 'que', 'qui', 'mon', 'ton', 'son', 'block', 'bloc', 'nflow', 'add', ...
    'signal', 'signaux', 'input', 'output', 'value', 'values', 'valeur', 'model', ...
    'modele', 'bound', 'scalar', 'number', 'element', 'elements', 'its', 'one', ...
    'two', 'construis', 'construit', 'cree', 'faire', 'fais', 'avec'};
  kept = {};
  for k = 1:numel(words)
    w = words{k};
    if length(w) < 3 || any(strcmp(stop, w))
      continue
    end
    kept{end + 1} = w; %#ok<AGROW>
    englishTerm = frenchBlockTerm(w);
    if ~isempty(englishTerm) && ~any(strcmp(kept, englishTerm))
      kept{end + 1} = englishTerm; %#ok<AGROW>
    end
  end
end
%=============================================================================
% The English block term a French request word points to, or '' when none. Lets a
% French request reach an English-named block (integrateur -> integrator).
function term = frenchBlockTerm(word)
  map = { ...
    'integrateur', 'integrator'; 'integration', 'integrator'; ...
    'derivee', 'derivative'; 'derivateur', 'derivative'; ...
    'somme', 'sum'; 'addition', 'sum'; 'produit', 'product'; ...
    'multiplication', 'product'; 'constante', 'constant'; 'retard', 'delay'; ...
    'interrupteur', 'switch'; 'commutateur', 'switch'; 'bascule', 'switch'; ...
    'comparateur', 'compare'; 'comparaison', 'compare'; 'oscilloscope', 'scope'; ...
    'bruit', 'noise'; 'rampe', 'ramp'; 'echelon', 'step'; 'sinus', 'sine'; ...
    'sinusoide', 'sine'; 'multiplexeur', 'mux'; 'demultiplexeur', 'demux'; ...
    'saturation', 'saturation'; 'gain', 'gain'};
  term = '';
  index = find(strcmp(map(:, 1), word), 1);
  if ~isempty(index)
    term = map{index, 2};
  end
end
%=============================================================================
% "- name - short" entries for causal pages with a non-zero match score, best
% first, de-duplicated by block name.
function entries = rankCausal(pages, words)
  scores = zeros(1, numel(pages));
  for k = 1:numel(pages)
    scores(k) = pageScore(pages(k), words);
  end
  hit = find(scores > 0);
  [~, order] = sort(scores(hit), 'descend');
  hit = hit(order);
  entries = {};
  seen = {};
  for i = 1:numel(hit)
    page = pages(hit(i));
    if any(strcmp(seen, lower(page.name)))
      continue
    end
    seen{end + 1} = lower(page.name); %#ok<AGROW>
    if isempty(page.short)
      entries{end + 1} = ['- ', page.name]; %#ok<AGROW>
    else
      entries{end + 1} = ['- ', page.name, ' - ', page.short]; %#ok<AGROW>
    end
  end
end
%=============================================================================
% Match score of a page: a request word on the block NAME weighs more than one only
% in the description, so a named block (gain, saturation) outranks a block that just
% mentions the word in prose.
function s = pageScore(page, words)
  nameWords = regexp(nelson.ai.internal.aiFoldAccents(page.name), '[a-z0-9]+', 'match');
  descWords = regexp(nelson.ai.internal.aiFoldAccents(page.short), '[a-z0-9]+', 'match');
  s = 0;
  for w = 1:numel(words)
    if any(strcmp(nameWords, words{w}))
      s = s + 3;
    elseif any(strcmp(descWords, words{w}))
      s = s + 1;
    end
  end
end
%=============================================================================
% Causal block help pages as a struct array {name, short}: the help index pages
% tagged "block:" whose name is NOT an acausal component (those are covered, with
% pins, by the acausal section). Cached: the help index is static in a build.
function pages = loadCausalBlockPages()
  persistent cached
  if ~isempty(cached)
    pages = cached;
    return
  end
  pages = struct('name', {}, 'short', {});
  index = loadHelpIndex();
  if ~isstruct(index)
    return
  end
  acausalNames = acausalNameSet();
  names = fieldnames(index);
  for k = 1:numel(names)
    page = index.(names{k});
    if ~isBlockPage(page)
      continue
    end
    keyword = pageField(page, 'keyword');
    if isempty(keyword) || any(strcmp(acausalNames, lower(keyword)))
      continue
    end
    pages(end + 1) = struct('name', keyword, ...
      'short', pageField(page, 'short_description')); %#ok<AGROW>
  end
  cached = pages;
end
%=============================================================================
% Lower-cased names of the acausal components, for excluding them from the causal
% list; {} when the accessor is absent.
function names = acausalNameSet()
  names = {};
  catalog = loadAcausalCatalog();
  for k = 1:numel(catalog)
    if isfield(catalog(k), 'name')
      names{end + 1} = lower(char(catalog(k).name)); %#ok<AGROW>
    end
  end
end
%=============================================================================
% True for an NFlow block help page (its keyword_alias carries "block:").
function tf = isBlockPage(page)
  tf = false;
  if ~isstruct(page) || ~isfield(page, 'keyword_alias')
    return
  end
  alias = page.keyword_alias;
  if iscell(alias) && ~isempty(alias)
    alias = alias{1};
  end
  tf = ischar(alias) && ~isempty(strfind(lower(alias), 'block:'));
end
%=============================================================================
function value = pageField(page, field)
  value = '';
  if isfield(page, field)
    v = page.(field);
    if iscell(v) && ~isempty(v)
      v = v{1};
    end
    if ischar(v)
      value = v;
    end
  end
end
%=============================================================================
% The decoded shipped help index for the current language, English fallback, or []
% when absent. Uses modulepath so it resolves inside the binary.
function index = loadHelpIndex()
  index = [];
  try
    base = [modulepath('help_tools'), '/help/nelson_help_'];
    jsonPath = [base, getlanguage(), '.json'];
    if ~isfile(jsonPath)
      jsonPath = [base, 'en_US.json'];
    end
    if isfile(jsonPath)
      index = jsondecode(fileread(jsonPath));
    end
  catch
    index = [];
  end
end
%=============================================================================
