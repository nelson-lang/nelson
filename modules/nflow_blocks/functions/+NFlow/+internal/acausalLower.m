%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% [json, info] = NFlow.internal.acausalLower(components, connections, opts)
%
% Acausal palette N1 (POC / executable specification for the C++ island
% block): lower an electrical netlist to an nflow DAE diagram built from the
% existing blocks (constraint / integrator / gain / sum / constant /
% toWorkspace), simulated with solver "dae" (IDAS).
%
% components: cell array of structs {id, type, params}:
%   'ground'          pins {p}          node reference (0 V)
%   'resistor'        pins {p,n}        params.R (ohm)
%   'capacitor'       pins {p,n}        params.C (farad), params.ic (V, opt)
%   'inductor'        pins {p,n}        params.L (henry), params.ic (A, opt)
%   'vsource'         pins {p,n}        params.V (volt, constant)
%   'isource'         pins {p,n}        params.I (ampere, constant, p->n)
%   'currentSensor'   pins {p,n}        series ideal ammeter; logs i(p->n)
%   'voltageSensor'   pins {p,n}        open-circuit voltmeter; logs e_p-e_n
%   'potentialSensor' pins {p}          logs e_p
%   sensors: params.name (log name, default = component id)
%
% connections: cell array; each entry is a cell array of pin names
%   'compId.p' / 'compId.n' wired to the same node (transitive union across
%   entries: a pin may appear in several entries).
%
% opts (optional struct): runtime (default 1), sampleTime (default 0.1).
%
% Formulation (MNA over the existing DAE blocks, index-1 by curation):
%   - per non-ground node: an algebraic `constraint` state e_n with residual
%     g = sum of branch currents into the node (KCL);
%   - capacitor: differential v_C (integrator, x' = i_C/C) + algebraic i_C
%     (constraint, g = v_C - (e_p - e_n));
%   - inductor: differential i_L (integrator, x' = (e_p - e_n)/L);
%   - voltage source / ideal ammeter: algebraic i_V (constraint,
%     g = (e_p - e_n) - V), ammeter V = 0;
%   - resistor current is the expression (e_p - e_n)/R (gain/sum blocks).
%
% Rejected topologies (actionable errors, graph analysis before emission):
%   - island without a ground             Nelson:nflow:acausalNoGround:island
%   - loop of capacitors/voltage sources  Nelson:nflow:acausalCapVLoop
%   - node fed only by inductors/current  Nelson:nflow:acausalInductorNode
%     sources
%   - non-ground node with no current-    Nelson:nflow:acausalDanglingNode
%     carrying branch
%=============================================================================
function [json, info] = acausalLower(components, connections, opts)
  if nargin < 3
    opts = struct();
  end
  runtime = 1.0;
  if isfield(opts, 'runtime'); runtime = opts.runtime; end
  sampleTime = 0.1;
  if isfield(opts, 'sampleTime'); sampleTime = opts.sampleTime; end
  vehicle = 'blocks'; % 'blocks' (DAE-block soup) | 'island' (native C++)
  if isfield(opts, 'vehicle'); vehicle = opts.vehicle; end

  % ---- catalogue ------------------------------------------------------------
  twoPin = {'resistor', 'capacitor', 'inductor', 'vsource', 'isource', ...
            'currentSensor', 'voltageSensor'};
  onePin = {'ground', 'potentialSensor'};

  % ---- collect pins ---------------------------------------------------------
  pinNames = {};
  for k = 1:numel(components)
    c = components{k};
    if ~isfield(c, 'id') || ~isfield(c, 'type')
      error('Nelson:nflow:acausalBadComponent', ...
        'Component #%d must have ''id'' and ''type'' fields.', k);
    end
    if any(strcmp(c.type, twoPin))
      pinNames{end + 1} = [c.id, '.p']; %#ok<AGROW>
      pinNames{end + 1} = [c.id, '.n']; %#ok<AGROW>
    elseif any(strcmp(c.type, onePin))
      pinNames{end + 1} = [c.id, '.p']; %#ok<AGROW>
    else
      error('Nelson:nflow:acausalUnknownType', ...
        'Component ''%s'': unknown type ''%s''.', c.id, c.type);
    end
  end

  % ---- union-find over pins -------------------------------------------------
  parent = 1:numel(pinNames);
  for k = 1:numel(connections)
    entry = connections{k};
    if ~iscell(entry) || numel(entry) < 2
      error('Nelson:nflow:acausalBadConnection', ...
        'Connection #%d must be a cell array of at least two pin names.', k);
    end
    i0 = uf_pin_index(pinNames, entry{1}, k);
    for j = 2:numel(entry)
      ij = uf_pin_index(pinNames, entry{j}, k);
      parent = uf_union(parent, i0, ij);
    end
  end
  % node index per pin (root order)
  roots = zeros(1, numel(pinNames));
  for i = 1:numel(pinNames)
    roots(i) = uf_find(parent, i);
  end
  uroots = unique(roots);
  nodeOf = zeros(1, numel(pinNames));
  for i = 1:numel(pinNames)
    nodeOf(i) = find(uroots == roots(i), 1);
  end
  numNodes = numel(uroots);

  % ---- ground marking -------------------------------------------------------
  isGround = false(1, numNodes);
  for k = 1:numel(components)
    c = components{k};
    if strcmp(c.type, 'ground')
      isGround(node_of_pin(pinNames, nodeOf, [c.id, '.p'])) = true;
    end
  end

  % ---- branch table (current-carrying two-pin elements) ---------------------
  % branch: {compIdx, type, na, nb}
  branches = {};
  for k = 1:numel(components)
    c = components{k};
    if any(strcmp(c.type, {'resistor', 'capacitor', 'inductor', 'vsource', ...
                           'isource', 'currentSensor'}))
      na = node_of_pin(pinNames, nodeOf, [c.id, '.p']);
      nb = node_of_pin(pinNames, nodeOf, [c.id, '.n']);
      branches{end + 1} = {k, c.type, na, nb}; %#ok<AGROW>
    end
  end

  % ---- validation -----------------------------------------------------------
  % Islands over every component pin adjacency (branches + voltmeters), so a
  % voltmeter-only attachment still joins its island for the ground check.
  islandParent = 1:numNodes;
  for k = 1:numel(components)
    c = components{k};
    if any(strcmp(c.type, twoPin))
      na = node_of_pin(pinNames, nodeOf, [c.id, '.p']);
      nb = node_of_pin(pinNames, nodeOf, [c.id, '.n']);
      islandParent = uf_union(islandParent, na, nb);
    end
  end
  islandRootOfNode = zeros(1, numNodes);
  for n = 1:numNodes
    islandRootOfNode(n) = uf_find(islandParent, n);
  end
  usedNodes = false(1, numNodes);
  for b = 1:numel(branches)
    usedNodes(branches{b}{3}) = true;
    usedNodes(branches{b}{4}) = true;
  end
  for k = 1:numel(components)
    c = components{k};
    if any(strcmp(c.type, twoPin)) || any(strcmp(c.type, onePin))
      % sensors and grounds keep their nodes alive for the island scan
    end
  end
  islandsSeen = [];
  for n = 1:numNodes
    if ~usedNodes(n) && ~isGround(n)
      continue
    end
    ir = islandRootOfNode(n);
    if any(islandsSeen == ir)
      continue
    end
    islandsSeen(end + 1) = ir; %#ok<AGROW>
    members = find(islandRootOfNode == ir);
    if ~any(isGround(members))
      error('Nelson:nflow:acausalNoGround:island', ...
        'Acausal island has no ground: add a ''ground'' component (island of %d node(s)).', ...
        numel(members));
    end
  end

  % Loop of capacitors / voltage sources / ammeters (index > 1): union-find
  % over nodes with only those edges; an edge closing a cycle is rejected.
  % Every ground node is merged FIRST (they share e = 0), so a vsource and a
  % capacitor both tied to ground close a loop through ground and are caught.
  cvParent = 1:numNodes;
  gnodes = find(isGround);
  for gi = 2:numel(gnodes)
    cvParent = uf_union(cvParent, gnodes(1), gnodes(gi));
  end
  for b = 1:numel(branches)
    ty = branches{b}{2};
    if any(strcmp(ty, {'capacitor', 'vsource', 'currentSensor'}))
      na = branches{b}{3};
      nb = branches{b}{4};
      if uf_find(cvParent, na) == uf_find(cvParent, nb)
        error('Nelson:nflow:acausalCapVLoop', ...
          'Loop of capacitors/voltage sources around ''%s'': index > 1, insert a resistor.', ...
          components{branches{b}{1}}.id);
      end
      cvParent = uf_union(cvParent, na, nb);
    end
  end

  % Node fed only by inductors / current sources (index-2) and dangling nodes.
  for n = 1:numNodes
    if isGround(n)
      continue
    end
    kinds = {};
    for b = 1:numel(branches)
      if branches{b}{3} == n || branches{b}{4} == n
        kinds{end + 1} = branches{b}{2}; %#ok<AGROW>
      end
    end
    if isempty(kinds)
      if any(nodeOf == n)
        % a node existing only through sensor pins (voltmeter side) or unwired
        error('Nelson:nflow:acausalDanglingNode', ...
          'Node %d carries no current-carrying branch: connect it or remove it.', n);
      end
      continue
    end
    onlyLI = true;
    for j = 1:numel(kinds)
      if ~any(strcmp(kinds{j}, {'inductor', 'isource'}))
        onlyLI = false;
      end
    end
    if onlyLI
      error('Nelson:nflow:acausalInductorNode', ...
        'Node %d is fed only by inductors/current sources: index > 1, add a resistor or ground path.', n);
    end
  end

  % ---- emission: native physicalIsland vehicle ------------------------------
  % opts.vehicle == 'island' emits ONE physicalIsland block per connected
  % island (the fast native C++ path); the default 'blocks' vehicle below emits
  % the DAE-block soup (the portable spec). Both share the node building and
  % validation above; the oracles cross-check them.
  if strcmp(vehicle, 'island')
    [json, info] = emit_island_vehicle(components, pinNames, nodeOf, numNodes, ...
      isGround, islandRootOfNode, runtime, sampleTime);
    return
  end

  % ---- emission: DAE blocks vehicle (default) -------------------------------
  E = emit_new();
  % node potential constraints (created first so e-signals exist by id)
  nodeBlockOf = cell(1, numNodes);
  for n = 1:numNodes
    if ~isGround(n) && usedNodes(n)
      nodeBlockOf{n} = sprintf('e_%d', n);
      E = emit_block(E, nodeBlockOf{n}, 'constraint', 1, 1, ...
        '{"InitialCondition":0}');
    end
  end
  esig = @(n) node_signal(nodeBlockOf, isGround, n);

  % per-node KCL accumulators: cell of {srcBlock, sign}
  kcl = cell(1, numNodes);

  for b = 1:numel(branches)
    ci = branches{b}{1};
    ty = branches{b}{2};
    na = branches{b}{3};
    nb = branches{b}{4};
    c = components{ci};
    bid = sanitize_id(c.id);
    if isGround(na) && isGround(nb)
      continue % both ends at reference: carries no information
    end
    switch ty
      case 'resistor'
        R = required_param(c, 'R');
        [E, cur] = emit_lincomb(E, ['ir_', bid], ...
          {{esig(na), 1.0}, {esig(nb), -1.0}}, 0.0, 1.0 / R);
      case 'capacitor'
        Cv = required_param(c, 'C');
        ic0 = 0.0;
        if isfield(c, 'params') && isfield(c.params, 'ic'); ic0 = c.params.ic; end
        vcId = ['vc_', bid];
        icId = ['ic_', bid];
        E = emit_block(E, icId, 'constraint', 1, 1, '{"InitialCondition":0}');
        E = emit_block(E, vcId, 'integrator', 1, 1, ...
          sprintf('{"InitialCondition":%.15g}', ic0));
        % x' = i_C / C
        gId = ['gc_', bid];
        E = emit_block(E, gId, 'gain', 1, 1, sprintf('{"Gain":%.15g}', 1.0 / Cv));
        E = emit_conn(E, icId, gId, 0, 0);
        E = emit_conn(E, gId, vcId, 0, 0);
        % g(i_C) = v_C - (e_a - e_b)
        [E, gsig] = emit_lincomb(E, ['gci_', bid], ...
          {{vcId, 1.0}, {esig(na), -1.0}, {esig(nb), 1.0}}, 0.0, 1.0);
        E = emit_conn(E, gsig, icId, 0, 0);
        cur = icId;
      case 'inductor'
        L = required_param(c, 'L');
        ic0 = 0.0;
        if isfield(c, 'params') && isfield(c.params, 'ic'); ic0 = c.params.ic; end
        ilId = ['il_', bid];
        E = emit_block(E, ilId, 'integrator', 1, 1, ...
          sprintf('{"InitialCondition":%.15g}', ic0));
        [E, dsig] = emit_lincomb(E, ['gl_', bid], ...
          {{esig(na), 1.0}, {esig(nb), -1.0}}, 0.0, 1.0 / L);
        E = emit_conn(E, dsig, ilId, 0, 0);
        cur = ilId;
      case {'vsource', 'currentSensor'}
        V = 0.0;
        if strcmp(ty, 'vsource'); V = required_param(c, 'V'); end
        ivId = ['iv_', bid];
        E = emit_block(E, ivId, 'constraint', 1, 1, '{"InitialCondition":0}');
        [E, gsig] = emit_lincomb(E, ['gv_', bid], ...
          {{esig(na), 1.0}, {esig(nb), -1.0}}, -V, 1.0);
        E = emit_conn(E, gsig, ivId, 0, 0);
        cur = ivId;
      case 'isource'
        I = required_param(c, 'I');
        iiId = ['ii_', bid];
        E = emit_block(E, iiId, 'constant', 0, 1, sprintf('{"Value":%.15g}', I));
        cur = iiId;
      otherwise
        error('Nelson:nflow:acausalUnreachable', 'unreachable');
    end
    % KCL: branch current flows p -> n: out of na (-), into nb (+).
    if ~isGround(na)
      kcl{na}{end + 1} = {cur, -1.0};
    end
    if ~isGround(nb)
      kcl{nb}{end + 1} = {cur, 1.0};
    end
  end

  % close each node's KCL into its constraint input
  for n = 1:numNodes
    if isGround(n) || ~usedNodes(n)
      continue
    end
    terms = {};
    for j = 1:numel(kcl{n})
      terms{end + 1} = {kcl{n}{j}{1}, kcl{n}{j}{2}}; %#ok<AGROW>
    end
    [E, gsig] = emit_lincomb(E, sprintf('kcl_%d', n), terms, 0.0, 1.0);
    E = emit_conn(E, gsig, nodeBlockOf{n}, 0, 0);
  end

  % ---- sensors --------------------------------------------------------------
  for k = 1:numel(components)
    c = components{k};
    bid = sanitize_id(c.id);
    name = c.id;
    if isfield(c, 'params') && isfield(c.params, 'name'); name = c.params.name; end
    switch c.type
      case 'voltageSensor'
        na = node_of_pin(pinNames, nodeOf, [c.id, '.p']);
        nb = node_of_pin(pinNames, nodeOf, [c.id, '.n']);
        [E, s] = emit_lincomb(E, ['vs_', bid], ...
          {{esig(na), 1.0}, {esig(nb), -1.0}}, 0.0, 1.0);
        E = emit_log(E, ['tw_', bid], s, name);
      case 'potentialSensor'
        na = node_of_pin(pinNames, nodeOf, [c.id, '.p']);
        [E, s] = emit_lincomb(E, ['ps_', bid], {{esig(na), 1.0}}, 0.0, 1.0);
        E = emit_log(E, ['tw_', bid], s, name);
      case 'currentSensor'
        E = emit_log(E, ['tw_', bid], ['iv_', bid], name);
    end
  end

  % ---- final JSON -----------------------------------------------------------
  json = ['{"blocks":[', strjoin(E.blocks, ','), '],', ...
    '"connections":[', strjoin(E.conns, ','), '],', ...
    sprintf('"solver":"dae","runtime":%.15g,"sampleTime":%.15g}', runtime, sampleTime)];

  info = struct();
  info.numNodes = numNodes;
  info.groundNodes = find(isGround);
  info.numBlocks = numel(E.blocks);
end
%=============================================================================
% Native physicalIsland emission: one physicalIsland block per connected island
% (islandRootOfNode groups nodes), each with its sensors wired to toWorkspace.
% Node numbering is per-island: ground -> 0, other nodes -> 1..K in first-seen
% order. Component node indices (a/b) are remapped accordingly.
function [json, info] = emit_island_vehicle(components, pinNames, nodeOf, ...
    numNodes, isGround, islandRootOfNode, runtime, sampleTime)
  blocks = {};
  conns = {};
  % Distinct island roots that actually carry components.
  roots = unique(islandRootOfNode(islandRootOfNode > 0));
  islandCount = 0;
  for r = 1:numel(roots)
    root = roots(r);
    members = find(islandRootOfNode == root);
    % Per-island node remap: ground -> 0, others -> 1..K (first-seen order).
    localOf = containers.Map('KeyType', 'double', 'ValueType', 'double');
    nextLocal = 1;
    for mi = 1:numel(members)
      gn = members(mi);
      if isGround(gn)
        localOf(gn) = 0;
      else
        localOf(gn) = nextLocal;
        nextLocal = nextLocal + 1;
      end
    end
    numLocalNodes = nextLocal; % 0..nextLocal-1 => count = nextLocal
    % Gather this island's components + sensor port assignment.
    compJson = {};
    outPort = 0;
    islandLogs = {};
    for k = 1:numel(components)
      c = components{k};
      if strcmp(c.type, 'ground')
        continue
      end
      na = node_of_pin(pinNames, nodeOf, [c.id, '.p']);
      if ~any(members == na)
        continue % not in this island
      end
      la = localOf(na);
      lb = 0;
      if ~strcmp(c.type, 'potentialSensor')
        nb = node_of_pin(pinNames, nodeOf, [c.id, '.n']);
        lb = localOf(nb);
      end
      name = c.id;
      if isfield(c, 'params') && isfield(c.params, 'name'); name = c.params.name; end
      switch c.type
        case 'resistor'
          compJson{end + 1} = sprintf('{"kind":"resistor","a":%d,"b":%d,"R":%.15g}', la, lb, required_param(c, 'R')); %#ok<AGROW>
        case 'capacitor'
          ic0 = 0.0; if isfield(c, 'params') && isfield(c.params, 'ic'); ic0 = c.params.ic; end
          compJson{end + 1} = sprintf('{"kind":"capacitor","a":%d,"b":%d,"C":%.15g,"ic":%.15g}', la, lb, required_param(c, 'C'), ic0); %#ok<AGROW>
        case 'inductor'
          ic0 = 0.0; if isfield(c, 'params') && isfield(c.params, 'ic'); ic0 = c.params.ic; end
          compJson{end + 1} = sprintf('{"kind":"inductor","a":%d,"b":%d,"L":%.15g,"ic":%.15g}', la, lb, required_param(c, 'L'), ic0); %#ok<AGROW>
        case 'vsource'
          compJson{end + 1} = sprintf('{"kind":"vsource","a":%d,"b":%d,"V":%.15g}', la, lb, required_param(c, 'V')); %#ok<AGROW>
        case 'isource'
          compJson{end + 1} = sprintf('{"kind":"isource","a":%d,"b":%d,"I":%.15g}', la, lb, required_param(c, 'I')); %#ok<AGROW>
        case 'currentSensor'
          compJson{end + 1} = sprintf('{"kind":"currentSensor","a":%d,"b":%d,"outPort":%d}', la, lb, outPort); %#ok<AGROW>
          islandLogs{end + 1} = {outPort, name}; %#ok<AGROW>
          outPort = outPort + 1;
        case 'voltageSensor'
          compJson{end + 1} = sprintf('{"kind":"voltageSensor","a":%d,"b":%d,"outPort":%d}', la, lb, outPort); %#ok<AGROW>
          islandLogs{end + 1} = {outPort, name}; %#ok<AGROW>
          outPort = outPort + 1;
        case 'potentialSensor'
          compJson{end + 1} = sprintf('{"kind":"potentialSensor","a":%d,"outPort":%d}', la, outPort); %#ok<AGROW>
          islandLogs{end + 1} = {outPort, name}; %#ok<AGROW>
          outPort = outPort + 1;
        otherwise
          error('Nelson:nflow:acausalIslandUnsupportedType', ...
            'Component ''%s'': type ''%s'' not supported by the island vehicle.', c.id, c.type);
      end
    end
    islandCount = islandCount + 1;
    islId = sprintf('isl_%d', islandCount);
    blocks{end + 1} = sprintf(['{"id":"%s","type":"physicalIsland","inputs":0,"outputs":%d,', ...
      '"params":{"domain":"electrical","nodes":%d,"components":[%s]}}'], ...
      islId, outPort, numLocalNodes, strjoin(compJson, ',')); %#ok<AGROW>
    for li = 1:numel(islandLogs)
      port = islandLogs{li}{1};
      nm = islandLogs{li}{2};
      twId = sprintf('tw_%d_%d', islandCount, port);
      blocks{end + 1} = sprintf('{"id":"%s","type":"toWorkspace","inputs":1,"outputs":0,"params":{"VariableName":"%s"}}', twId, nm); %#ok<AGROW>
      conns{end + 1} = sprintf('{"from":"%s","to":"%s","fromIndex":%d,"toIndex":0}', islId, twId, port); %#ok<AGROW>
    end
  end
  json = ['{"blocks":[', strjoin(blocks, ','), '],', ...
    '"connections":[', strjoin(conns, ','), '],', ...
    sprintf('"solver":"dae","runtime":%.15g,"sampleTime":%.15g}', runtime, sampleTime)];
  info = struct();
  info.numNodes = numNodes;
  info.groundNodes = find(isGround);
  info.numBlocks = numel(blocks);
  info.numIslands = islandCount;
end
%=============================================================================
% ---- helpers (static, bottom of file per repo style) -----------------------
%=============================================================================
function i = uf_pin_index(pinNames, name, entryIdx)
  i = find(strcmp(pinNames, name), 1);
  if isempty(i)
    error('Nelson:nflow:acausalUnknownConnectionPin', ...
      'Connection #%d references unknown pin ''%s''.', entryIdx, name);
  end
end
%=============================================================================
function r = uf_find(parent, i)
  r = i;
  while parent(r) ~= r
    r = parent(r);
  end
end
%=============================================================================
function parent = uf_union(parent, a, b)
  ra = uf_find(parent, a);
  rb = uf_find(parent, b);
  if ra ~= rb
    parent(rb) = ra;
  end
end
%=============================================================================
function n = node_of_pin(pinNames, nodeOf, name)
  i = find(strcmp(pinNames, name), 1);
  n = nodeOf(i);
end
%=============================================================================
function s = node_signal(nodeBlockOf, isGround, n)
  if isGround(n)
    s = ''; % ground potential: 0, contributes nothing
  else
    s = nodeBlockOf{n};
  end
end
%=============================================================================
function v = required_param(c, pname)
  if ~isfield(c, 'params') || ~isfield(c.params, pname)
    error('Nelson:nflow:acausalMissingParam', ...
      'Component ''%s'' (%s) requires parameter ''%s''.', c.id, c.type, pname);
  end
  v = c.params.(pname);
end
%=============================================================================
function s = sanitize_id(id)
  s = regexprep(id, '[^A-Za-z0-9_]', '_');
end
%=============================================================================
function E = emit_new()
  E = struct();
  E.blocks = {};
  E.conns = {};
end
%=============================================================================
function E = emit_block(E, id, type, nin, nout, paramsJson)
  E.blocks{end + 1} = sprintf(...
    '{"id":"%s","type":"%s","inputs":%d,"outputs":%d,"params":%s}', ...
    id, type, nin, nout, paramsJson);
end
%=============================================================================
function E = emit_conn(E, from, to, fi, ti)
  E.conns{end + 1} = sprintf(...
    '{"from":"%s","to":"%s","fromIndex":%d,"toIndex":%d}', from, to, fi, ti);
end
%=============================================================================
function E = emit_log(E, id, src, name)
  E = emit_block(E, id, 'toWorkspace', 1, 0, ...
    sprintf('{"VariableName":"%s"}', name));
  E = emit_conn(E, src, id, 0, 0);
end
%=============================================================================
% Weighted sum: out = scale * (sum_j coeff_j * sig_j + constTerm).
% Entries with an empty signal (ground potential) are dropped. Returns the id
% of the block carrying the result. Uses gain blocks for coefficients and an
% n-ary sum; degenerate cases collapse (single unit term -> the source
% itself; no terms -> a constant block).
function [E, outId] = emit_lincomb(E, prefix, entries, constTerm, scale)
  live = {};
  for j = 1:numel(entries)
    if ~isempty(entries{j}{1})
      live{end + 1} = entries{j}; %#ok<AGROW>
    end
  end
  inputs = {};
  for j = 1:numel(live)
    src = live{j}{1};
    coeff = live{j}{2};
    if abs(coeff - 1.0) < eps
      inputs{end + 1} = src; %#ok<AGROW>
    else
      gid = sprintf('%s_g%d', prefix, j);
      E = emit_block(E, gid, 'gain', 1, 1, sprintf('{"Gain":%.15g}', coeff));
      E = emit_conn(E, src, gid, 0, 0);
      inputs{end + 1} = gid; %#ok<AGROW>
    end
  end
  if constTerm ~= 0.0
    cid = [prefix, '_c'];
    E = emit_block(E, cid, 'constant', 0, 1, sprintf('{"Value":%.15g}', constTerm));
    inputs{end + 1} = cid;
  end
  if isempty(inputs)
    outId = [prefix, '_z'];
    E = emit_block(E, outId, 'constant', 0, 1, '{"Value":0}');
    return
  end
  if numel(inputs) == 1
    outId = inputs{1};
  else
    outId = [prefix, '_s'];
    E = emit_block(E, outId, 'sum', numel(inputs), 1, '{}');
    for j = 1:numel(inputs)
      E = emit_conn(E, inputs{j}, outId, 0, j - 1);
    end
  end
  if abs(scale - 1.0) >= eps
    sid = [prefix, '_k'];
    E = emit_block(E, sid, 'gain', 1, 1, sprintf('{"Gain":%.15g}', scale));
    E = emit_conn(E, outId, sid, 0, 0);
    outId = sid;
  end
end
%=============================================================================
