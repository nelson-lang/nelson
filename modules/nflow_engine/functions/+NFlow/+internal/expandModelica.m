%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% documentJson = NFlow.internal.expandModelica(documentJson)
%
% Expand every top-level `modelica` block in a model's JSON into an `fmu` block:
% the Modelica source is compiled to an FMU with OpenModelica (modelicaToFmu) and
% the block is rewritten to type "fmu" pointing at the produced .fmu, so the
% engine (which knows nothing about Modelica) simulates a plain FMU diagram.
%
% The rewrite is SURGICAL: only the modelica block objects are replaced; the rest
% of the document string is left byte-for-byte intact. sim() passes raw model
% JSON to the engine on purpose -- a full jsondecode/jsonencode round-trip
% homogenizes the heterogeneous, possibly subsystem-nested block array and would
% collapse subsystem inner diagrams. A modelica block has no inner diagram, so
% decoding a single modelica element on its own is safe.
%
% If a modelica block is present but OpenModelica cannot build it, modelicaToFmu
% raises (Nelson:nflow_engine:modelicaUnavailable / :modelicaCompileFailed) and the
% simulation is blocked, as specified.
%=============================================================================
function documentJson = expandModelica(documentJson)
  % Cheap pre-check: nothing that looks like a modelica block.
  if isempty(strfind(documentJson, 'modelica'))
    return
  end
  span = nflow_json_toplevel_array_span(documentJson, 'blocks');
  if isempty(span)
    return
  end
  openIdx = span(1);
  closeIdx = span(2);
  inner = documentJson((openIdx + 1):(closeIdx - 1));
  elems = nflow_json_split_objects(inner);
  if isempty(elems)
    return
  end
  changed = false;
  for k = 1:numel(elems)
    e = elems{k};
    if isempty(strfind(e, 'modelica'))
      continue % not a modelica block -> keep verbatim
    end
    b = jsondecode(e); % safe: a single block object, no array
    if ~isfield(b, 'type') || ~strcmp(b.type, 'modelica')
      continue % 'modelica' appeared elsewhere (e.g. a name)
    end
    elems{k} = nflow_modelica_block_to_fmu(b);
    changed = true;
  end
  if ~changed
    return
  end
  newInner = strjoin(elems, ',');
  documentJson = [documentJson(1:openIdx), newInner, documentJson(closeIdx:end)];
end
%=============================================================================
function s = nflow_modelica_block_to_fmu(b)
  % Compile the Modelica source and produce the replacement fmu-block JSON.
  if ~nflow_have_function('modelicaToFmu')
    error('Nelson:nflow_engine:modelicaUnavailable', ...
      ['The model contains a Modelica block but the nflow_fmi module (modelicaToFmu) ', ...
      'is not loaded. Load nflow_fmi to use Modelica blocks.']);
  end
  params = struct();
  if isfield(b, 'params')
    params = b.params;
  end
  % A modelica block carries both a `source` (inline text) and a `file` (path)
  % parameter, either of which may be present-but-empty (the manifest defaults
  % both to ""). Pick the first NON-EMPTY one in priority order, so an empty
  % inline source falls through to the file path (and vice versa) instead of
  % shadowing it. modelicaToFmu accepts a path or inline text interchangeably.
  source = nflow_modelica_first_nonempty(params, ...
    {'source', 'ModelicaSource', 'file', 'ModelicaFile'});
  if isempty(source)
    error('Nelson:nflow_engine:modelicaBlock', ...
      'Modelica block ''%s'' has no source (set its ''source'' or ''file'' parameter).', ...
      nflow_block_id(b));
  end
  modelName = nflow_modelica_first_nonempty(params, {'modelName', 'ModelName'});

  fmu = modelicaToFmu(source, modelName);

  % Build the replacement block: same id/inputs/outputs, type fmu, params.path.
  nb = struct();
  nb.id = nflow_block_id(b);
  nb.type = 'fmu';
  if isfield(b, 'inputs'); nb.inputs = b.inputs; end
  if isfield(b, 'outputs'); nb.outputs = b.outputs; end
  nb.params = struct('path', strrep(fmu, '\', '/'));
  s = jsonencode(nb);
end
%=============================================================================
function id = nflow_block_id(b)
  id = '';
  if isfield(b, 'id')
    id = char(b.id);
  end
end
%=============================================================================
function value = nflow_modelica_first_nonempty(params, names)
  % First non-empty character value among params.(names) in priority order.
  value = '';
  for i = 1:numel(names)
    if isfield(params, names{i})
      candidate = char(params.(names{i}));
      if ~isempty(candidate)
        value = candidate;
        return
      end
    end
  end
end
%=============================================================================
function tf = nflow_have_function(name)
  tf = (exist(name) == 2) || (exist(name) == 5) || (exist(name) == 6);
end
%=============================================================================
function span = nflow_json_toplevel_array_span(s, key)
  % Return [openBracketIndex, closeBracketIndex] of the value array of the
  % top-level (root-object depth 1) key `key`, or [] if not found.
  span = [];
  n = numel(s);
  depth = 0; % nesting of { and [
  inStr = false;
  esc = false;
  i = 1;
  quotedKey = ['"', key, '"'];
  klen = numel(quotedKey);
  while i <= n
    c = s(i);
    if inStr
      if esc
        esc = false;
      elseif c == '\'
        esc = true;
      elseif c == '"'
        inStr = false;
      end
      i = i + 1;
      continue
    end
    if c == '"'
      % At root-object depth 1 a key can start here. Test for "blocks".
      if depth == 1 && i + klen - 1 <= n && strcmp(s(i:(i + klen - 1)), quotedKey)
        j = i + klen;
        while j <= n && nflow_is_ws(s(j)); j = j + 1; end
        if j <= n && s(j) == ':'
          j = j + 1;
          while j <= n && nflow_is_ws(s(j)); j = j + 1; end
          if j <= n && s(j) == '['
            span = [j, nflow_json_match_bracket(s, j)];
            return
          end
        end
      end
      inStr = true;
      i = i + 1;
      continue
    end
    if c == '{' || c == '['
      depth = depth + 1;
    elseif c == '}' || c == ']'
      depth = depth - 1;
    end
    i = i + 1;
  end
end
%=============================================================================
function close = nflow_json_match_bracket(s, openIdx)
  % Index of the ']' matching the '[' at openIdx (string/escape aware).
  n = numel(s);
  depth = 0;
  inStr = false;
  esc = false;
  close = -1;
  for i = openIdx:n
    c = s(i);
    if inStr
      if esc
        esc = false;
      elseif c == '\'
        esc = true;
      elseif c == '"'
        inStr = false;
      end
      continue
    end
    if c == '"'
      inStr = true;
    elseif c == '[' || c == '{'
      depth = depth + 1;
    elseif c == ']' || c == '}'
      depth = depth - 1;
      if depth == 0
        close = i;
        return
      end
    end
  end
end
%=============================================================================
function objs = nflow_json_split_objects(inner)
  % Split the inside of an array into its top-level { ... } object substrings
  % (string/escape/brace aware). Non-object content between objects is dropped.
  objs = {};
  n = numel(inner);
  depth = 0;
  inStr = false;
  esc = false;
  startIdx = -1;
  for i = 1:n
    c = inner(i);
    if inStr
      if esc
        esc = false;
      elseif c == '\'
        esc = true;
      elseif c == '"'
        inStr = false;
      end
      continue
    end
    if c == '"'
      inStr = true;
      continue
    end
    if c == '{'
      if depth == 0
        startIdx = i;
      end
      depth = depth + 1;
    elseif c == '}'
      depth = depth - 1;
      if depth == 0 && startIdx > 0
        objs{end + 1} = inner(startIdx:i); %#ok<AGROW>
        startIdx = -1;
      end
    end
  end
end
%=============================================================================
function tf = nflow_is_ws(c)
  tf = (c == ' ') || (c == sprintf('\t')) || (c == sprintf('\n')) || (c == sprintf('\r'));
end
%=============================================================================
