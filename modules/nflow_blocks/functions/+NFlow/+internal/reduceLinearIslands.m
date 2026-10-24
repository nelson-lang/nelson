%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% documentJson = NFlow.internal.reduceLinearIslands(documentJson)
%
% Code-generation pre-pass (acausal): replace every LINEAR, single-
% input / single-output electrical or thermal `physicalIsland` block by the
% equivalent explicit state-space block, so the (multi-state, scalar-I/O)
% stateSpace code generator can lower it. Each such island is linearised in
% isolation (linmod), its algebraic node/branch states Schur-eliminated
% (NFlow.internal.reduceDescriptor) and the block swapped for a `stateSpace` carrying
% the reduced (A, B, C, D). A non-linear island (diode, radiation, switch,
% signal-modulated component), a multi-port island, or one whose algebraic
% block is singular is left untouched (code generation then rejects it).
%
% The diagram is edited as RAW JSON (locate the blocks array, brace-match each
% object, splice the replacement) so it never round-trips the heterogeneous
% block array through a struct array, which would drop or homogenise fields.
% Zero-cost when the model has no physicalIsland block.
%=============================================================================
function documentJson = reduceLinearIslands(documentJson)
  if isempty(strfind(documentJson, 'physicalIsland'))
    return
  end
  [pre, arr, post] = ph_extract_array(documentJson, 'blocks');
  if isempty(arr)
    return
  end
  objs = ph_split_objects(arr);
  changed = false;
  for i = 1:numel(objs)
    if isempty(strfind(objs{i}, 'physicalIsland'))
      continue
    end
    nb = ph_try_reduce(objs{i});
    if ~isempty(nb)
      objs{i} = nb;
      changed = true;
    end
  end
  if changed
    documentJson = [pre, '[', strjoin(objs, ','), ']', post];
  end
end
%=============================================================================
function nb = ph_try_reduce(objStr)
  nb = '';
  b = jsondecode(objStr); % a single homogeneous object: safe to decode
  if ~isfield(b, 'type') || ~strcmp(b.type, 'physicalIsland')
    return
  end
  ni = ph_field(b, 'inputs', 0);
  no = ph_field(b, 'outputs', 0);
  if ni ~= 1 || no ~= 1
    return % SISO scope only (matches the scalar-I/O stateSpace codegen)
  end
  if ~isfield(b, 'params') || ~isfield(b.params, 'components')
    return
  end
  if ~ph_all_linear(b.params.components)
    return
  end
  % Linearise the island in isolation: one external input, one sensor output.
  bIso = b;
  bIso.id = 'isl';
  lin = ['{"version":"1.0.0","format":"nflow-2","name":"L","blocks":[', ...
    '{"id":"u","type":"labelSource","inputs":0,"outputs":1,', ...
    '"params":{"GotoTag":"u","isExternalPort":true}},', ...
    jsonencode(bIso), ...
    ',{"id":"y","type":"labelSink","inputs":1,"outputs":1,', ...
    '"params":{"GotoTag":"y","isExternalPort":true}}],', ...
    '"connections":[{"from":"u","to":"isl","fromIndex":0,"toIndex":0},', ...
    '{"from":"isl","to":"y","fromIndex":0,"toIndex":0}],', ...
    '"solver":"dae","runtime":1,"sampleTime":0.1}'];
  r = jsondecode(__nflow_linearize__(lin));
  if ~isfield(r, 'status') || ~strcmp(r.status, 'done')
    return
  end
  if ~isfield(r, 'mask') || r.nu ~= 1 || r.ny ~= 1 || r.nx < 1
    return
  end
  A = reshape(r.A, r.nx, r.nx).';
  B = reshape(r.B, r.nu, r.nx).';
  C = reshape(r.C, r.nx, r.ny).';
  D = r.D;
  if (iscell(D) || isempty(D))
    D = zeros(r.ny, r.nu);
  end
  try
    [Ar, Br, Cr, Dr] = NFlow.internal.reduceDescriptor(A, B, C, D, r.mask(:).');
  catch
    return % singular algebraic block (higher-index DAE): leave the island
  end
  nb = ph_build_ss(b, Ar, Br, Cr, Dr);
end
%=============================================================================
function tf = ph_all_linear(components)
  LINEAR = {'resistor', 'conductor', 'capacitor', 'inductor', 'vsource', ...
    'isource', 'vcvs', 'vccs', 'opAmp', 'short', 'idle', 'transformer', ...
    'gyrator', 'currentSensor', 'voltageSensor', 'potentialSensor', ...
    'heatCapacitor', 'thermalConductor', 'thermalResistor', ...
    'temperatureSensor', 'heatFlowSensor', 'heatFlowSource', ...
    'fixedTemperature', 'fixedHeatFlow'};
  tf = true;
  n = numel(components);
  for j = 1:n
    if iscell(components)
      c = components{j};
    else
      c = components(j);
    end
    if ~isfield(c, 'kind') || ~any(strcmp(c.kind, LINEAR))
      tf = false;
      return
    end
  end
end
%=============================================================================
function s = ph_build_ss(b, Ar, Br, Cr, Dr)
  Astr = jsonencode(reshape(Ar.', 1, [])); % row-major, as stateSpace expects
  Bstr = jsonencode(Br(:).');
  Cstr = jsonencode(Cr(:).');
  Dstr = jsonencode(Dr);
  x = ph_field(b, 'x', 0);
  y = ph_field(b, 'y', 0);
  rot = ph_field(b, 'rotation', 0);
  s = sprintf(['{"id":"%s","type":"stateSpace","inputs":1,"outputs":1,', ...
    '"x":%.10g,"y":%.10g,"rotation":%.10g,', ...
    '"params":{"A":%s,"B":%s,"C":%s,"D":%s}}'], ...
    b.id, x, y, rot, Astr, Bstr, Cstr, Dstr);
end
%=============================================================================
function v = ph_field(s, name, dflt)
  if isstruct(s) && isfield(s, name)
    v = s.(name);
  else
    v = dflt;
  end
end
%=============================================================================
% Locate "<key>":[ ... ] and return the text before '[', the array content
% between the brackets, and the text after ']'. Empty arr if not found.
function [pre, arr, post] = ph_extract_array(doc, key)
  pre = ''; arr = ''; post = '';
  pat = ['"', key, '"'];
  k = strfind(doc, pat);
  if isempty(k)
    return
  end
  p = k(1) + numel(pat);
  while p <= numel(doc) && doc(p) ~= '['
    p = p + 1;
  end
  if p > numel(doc)
    return
  end
  closeP = ph_match_bracket(doc, p);
  if closeP < 0
    return
  end
  pre = doc(1:p - 1);
  arr = doc(p + 1:closeP - 1);
  post = doc(closeP + 1:end);
end
%=============================================================================
% Position of the bracket/brace matching the one at openPos, skipping string
% contents (and their escapes).
function closePos = ph_match_bracket(s, openPos)
  closePos = -1;
  depth = 0; inStr = false; esc = false;
  for k = openPos:numel(s)
    c = s(k);
    if inStr
      if esc
        esc = false;
      elseif c == '\'
        esc = true;
      elseif c == '"'
        inStr = false;
      end
    else
      if c == '"'
        inStr = true;
      elseif c == '[' || c == '{'
        depth = depth + 1;
      elseif c == ']' || c == '}'
        depth = depth - 1;
        if depth == 0
          closePos = k;
          return
        end
      end
    end
  end
end
%=============================================================================
% Split a JSON array's inner text into its top-level object/element substrings
% (commas at bracket depth 0), skipping string contents.
function objs = ph_split_objects(arr)
  objs = {};
  depth = 0; inStr = false; esc = false; start = 1;
  for k = 1:numel(arr)
    c = arr(k);
    if inStr
      if esc
        esc = false;
      elseif c == '\'
        esc = true;
      elseif c == '"'
        inStr = false;
      end
    else
      if c == '"'
        inStr = true;
      elseif c == '[' || c == '{'
        depth = depth + 1;
      elseif c == ']' || c == '}'
        depth = depth - 1;
      elseif c == ',' && depth == 0
        objs{end + 1} = strtrim(arr(start:k - 1)); %#ok<AGROW>
        start = k + 1;
      end
    end
  end
  last = strtrim(arr(start:end));
  if ~isempty(last)
    objs{end + 1} = last;
  end
end
%=============================================================================
