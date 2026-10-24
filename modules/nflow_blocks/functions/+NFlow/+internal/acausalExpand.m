%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% documentJson = NFlow.internal.acausalExpand(documentJson)
%
% Acausal palette N0.2: the catalog-driven diagram lowering. Finds every
% acausal component block (its `type` is a name in NFlow.internal.acausalCatalog) plus
% the diagram's `physicalWires`, groups the components into islands (connected
% components over the wired pins), and rewrites each island into ONE native
% island block (physicalIsland for electrical/thermal), with its sensors wired
% to the causal signal graph where each sensor's output previously connected.
%
% Driven entirely by the single-source catalog: a new component is a table row
% in NFlow.internal.acausalCatalog, not code here. The sim()/Run-side counterpart of
% NFlow.internal.expandModelica.
%
% Diagram contract (the GUI physical-wire format, N5):
%   - acausal component block: {id, type=<catalog name>, params:{...}}
%   - "physicalWires": [ {pins:["blkA.p","blkB.n", ...]}, ... ]: undirected
%     N-ary nets over physical pins (pin = "<blockId>.<pinName>").
%   - a sensor's signal-output connection stays in "connections" referencing the
%     sensor block id; it is rewritten to the island block's output port.
%
% v1 scope: electrical / thermal islands, constant sources + sensors, reference
% via a `Ground` component (electrical) or a `FixedTemperature`/reference pin.
% A signal-driven source or a mechanical component in a physicalWires diagram
% raises a clear "not yet supported by the diagram expansion" error (use the
% programmatic NFlow.internal.acausalLower meanwhile). Nested subsystems are untouched.
%=============================================================================
function documentJson = acausalExpand(documentJson)
  if isempty(strfind(documentJson, 'physicalWires'))
    return
  end
  doc = jsondecode(documentJson);
  catalog = NFlow.internal.acausalCatalog();
  catNames = {catalog.name};
  % Recurse into plain (virtual) subsystems and lower each inner acausal
  % diagram in place, so a self-contained physical island inside a subsystem is
  % emitted as a nested physicalIsland (the engine already simulates those - see
  % test_nflow_island_solver_modes, nested case). A controlled subsystem (enable /
  % trigger / reset / action / iterator / for-each) cannot host a DAE island - it
  % would be conditionally gated - and is rejected with a clear message. Physical
  % wires that CROSS a subsystem boundary remain a post-2.0 feature.
  [doc, subChanged] = ax_expand_subsystems(doc, catalog, catNames);
  hasTop = isfield(doc, 'physicalWires') && ~isempty(doc.physicalWires);
  if hasTop
    doc = ax_lower_diagram(doc, catalog, catNames);
  end
  if hasTop || subChanged
    % jsondecode turned any 1-element list into a scalar struct; keep the doc's
    % list fields as JSON arrays so a single top-level connection is not dropped.
    doc = ax_force_list_arrays(doc);
    documentJson = jsonencode(doc);
  end
end
%=============================================================================
function doc = ax_lower_diagram(doc, catalog, catNames)
  % Lower ONE diagram's physical wires into native island blocks. Operates on a
  % struct with .blocks (+ optional .connections, .physicalWires) and returns it
  % with each physical island rewritten to a single island block and the
  % physicalWires field removed. The top level and each plain subsystem run this.
  % Waveform-source pre-pass: split each SineVoltage / PulseVoltage / ... into a
  % causal generator feeding a SignalVoltage / SignalCurrent, then let the island
  % grouping below wire the generator into the source's input port.
  doc = ax_expand_waveforms(doc);
  % Composite pre-pass: split each SpringDamper into a parallel Spring + Damper
  % (both on the same physical net), rewriting the physicalWires accordingly.
  doc = ax_expand_composites(doc);
  doc = ax_expand_heating(doc);
  doc = ax_expand_emf(doc);
  blocks = ax_cellrow(doc.blocks);

  isAc = false(1, numel(blocks));
  catOf = zeros(1, numel(blocks));
  for i = 1:numel(blocks)
    idx = find(strcmp(catNames, ax_field(blocks{i}, 'type', '')), 1);
    if ~isempty(idx); isAc(i) = true; catOf(i) = idx; end
  end
  if ~any(isAc); return; end
  ax_validate_acausal_wire_pins(doc, catalog, blocks, isAc, catOf);

  % All four single-domain families are supported (electrical, thermal,
  % translational, rotational); signal-driven sources too. A single island must
  % be one domain; a physicalWires net joining two domains is rejected below.
  % (Cross-domain coupling, e.g. EMF or gears crossing domains, needs a unified
  % multi-domain island, not yet built.)

  % ---- pins + union-find over physicalWires ---------------------------------
  wires = ax_cellrow(doc.physicalWires);
  pinKeys = {};
  for i = find(isAc)
    ci = catalog(catOf(i));
    for pp = 1:numel(ci.pins)
      pinKeys{end + 1} = [blocks{i}.id, '.', ci.pins{pp}{1}]; %#ok<AGROW>
    end
  end
  parent = 1:numel(pinKeys);
  for w = 1:numel(wires)
    pins = ax_cellrow(wires{w}.pins);
    if numel(pins) < 2; continue; end
    i0 = find(strcmp(pinKeys, char(pins{1})), 1);
    for j = 2:numel(pins)
      ij = find(strcmp(pinKeys, char(pins{j})), 1);
      if ~isempty(i0) && ~isempty(ij); parent = ax_union(parent, i0, ij); end
    end
  end
  roots = arrayfun(@(i) ax_find(parent, i), 1:numel(pinKeys));
  uroots = unique(roots);
  nodeOfPin = containers.Map('KeyType', 'char', 'ValueType', 'double');
  for i = 1:numel(pinKeys); nodeOfPin(pinKeys{i}) = find(uroots == roots(i), 1); end

  % ---- island grouping over acausal blocks ----------------------------------
  acIdx = find(isAc);
  blkParent = 1:numel(acIdx);
  firstAtNode = containers.Map('KeyType', 'double', 'ValueType', 'double');
  for bi = 1:numel(acIdx)
    ci = catalog(catOf(acIdx(bi)));
    for pp = 1:numel(ci.pins)
      nd = nodeOfPin([blocks{acIdx(bi)}.id, '.', ci.pins{pp}{1}]);
      if isKey(firstAtNode, nd)
        blkParent = ax_union(blkParent, firstAtNode(nd), bi);
      else
        firstAtNode(nd) = bi;
      end
    end
  end
  islandRoot = arrayfun(@(i) ax_find(blkParent, i), 1:numel(acIdx));
  islandIds = unique(islandRoot);

  % ---- emit island blocks ---------------------------------------------------
  newBlocks = {};
  for i = 1:numel(blocks)
    if ~isAc(i); newBlocks{end + 1} = blocks{i}; end %#ok<AGROW>
  end
  sigOutMap = containers.Map('KeyType', 'char', 'ValueType', 'any');
  sigInMap = containers.Map('KeyType', 'char', 'ValueType', 'any');
  islandCount = 0;

  for g = 1:numel(islandIds)
    members = acIdx(islandRoot == islandIds(g));
    ci0 = catalog(catOf(members(1)));
    % One island = one domain: a physical net joining two domains is invalid.
    for mm = members
      if ~strcmp(catalog(catOf(mm)).domain, ci0.domain)
        error('Nelson:nflow:acausalExpandMixedDomain', ...
          ['Physical wires join two domains (%s and %s) in one island; ', ...
           'connect only same-domain pins.'], ci0.domain, catalog(catOf(mm)).domain);
      end
    end
    % An electrical island needs a 0-reference: without a Ground component no
    % node is fixed and the DAE is structurally singular, which otherwise
    % surfaces only as a cryptic solver initialisation failure. Fail fast.
    if strcmp(ci0.domain, 'electrical')
      hasGround = false;
      for mm = members
        if strcmp(blocks{mm}.type, 'Ground')
          hasGround = true;
          break;
        end
      end
      if ~hasGround
        error('Nelson:nflow:acausalNoGround:electricalIsland', ...
          'Electrical acausal island has no ground: add a Ground component (island of %d block(s)).', ...
          numel(members));
      end
    end
    islandCount = islandCount + 1;
    islId = sprintf('__island_%d', islandCount);
    inPort = 0;

    localNodes = containers.Map('KeyType', 'double', 'ValueType', 'double');
    nextLocal = 1;
    % Reference pins -> node 0 (a Ground component's pin).
    for m = members
      if strcmp(blocks{m}.type, 'Ground')
        ci = catalog(catOf(m));
        localNodes(nodeOfPin([blocks{m}.id, '.', ci.pins{1}{1}])) = 0;
      end
    end

    compJson = {};
    outPort = 0;
    for m = members
      if strcmp(blocks{m}.type, 'Ground'); continue; end
      ci = catalog(catOf(m));
      nodej = '';
      for pp = 1:numel(ci.pins)
        nd = nodeOfPin([blocks{m}.id, '.', ci.pins{pp}{1}]);
        [localNodes, nextLocal, lv] = ax_ensure(localNodes, nextLocal, nd);
        nodej = [nodej, sprintf(',"%s":%d', ci.pins{pp}{2}, lv)]; %#ok<AGROW>
      end
      pj = {};
      for qq = 1:numel(ci.params)
        pmap = ci.params{qq};
        pval = ax_param(blocks{m}, pmap{1}, pmap{3});
        % A capacitance / inductance drives a differential state (v_C' = i/C,
        % i_L' = v/L); a non-positive value divides by zero and fails the solve.
        % Reject it up front with a clear message (fail fast).
        if (strcmp(pmap{2}, 'C') || strcmp(pmap{2}, 'L')) && pval <= 0
          error('Nelson:nflow:acausalNonPositiveParam', ...
            'Acausal component ''%s'': %s must be greater than zero (got %.15g).', ...
            blocks{m}.id, pmap{1}, pval);
        end
        pj{end + 1} = sprintf('"%s":%.15g', pmap{2}, pval); %#ok<AGROW>
      end
      extra = '';
      if ~isempty(ci.signalIn)
        % Signal-driven source: bind the component to an island input port; the
        % connection feeding this component is rewritten to that port below.
        extra = sprintf(',"port":%d', inPort);
        sigInMap(blocks{m}.id) = {islId, inPort};
        inPort = inPort + 1;
      end
      if ~isempty(ci.signalOut)
        extra = [extra, sprintf(',"outPort":%d', outPort)]; %#ok<AGROW>
        sigOutMap(blocks{m}.id) = {islId, outPort};
        outPort = outPort + 1;
      end
      comp = ['{"kind":"', ci.kind, '"', nodej];
      if ~isempty(pj); comp = [comp, ',', strjoin(pj, ',')]; end
      newBlocks_comp = [comp, extra, '}'];
      compJson{end + 1} = newBlocks_comp; %#ok<AGROW>
    end

    isl = struct();
    isl.id = islId;
    isl.type = ci0.island;
    isl.inputs = inPort;
    isl.outputs = outPort;
    isl.params = jsondecode(sprintf('{"domain":"%s","nodes":%d,"components":[%s]}', ...
      ci0.domain, nextLocal, strjoin(compJson, ',')));
    newBlocks{end + 1} = isl; %#ok<AGROW>
  end

  % ---- rewrite sensor signal connections to island output ports -------------
  newConns = {};
  conns = {};
  if isfield(doc, 'connections') && ~isempty(doc.connections)
    conns = ax_cellrow(doc.connections);
  end
  for cc = 1:numel(conns)
    conn = conns{cc};
    fromId = ax_field(conn, 'from', '');
    toId = ax_field(conn, 'to', '');
    if isKey(sigOutMap, fromId)
      v = sigOutMap(fromId);
      conn.from = v{1};
      conn.fromIndex = v{2};
    end
    if isKey(sigInMap, toId)
      v = sigInMap(toId);
      conn.to = v{1};
      conn.toIndex = v{2};
    end
    newConns{end + 1} = conn; %#ok<AGROW>
  end

  doc.blocks = newBlocks;
  if ~isempty(newConns); doc.connections = newConns; end
  doc = rmfield(doc, 'physicalWires');
end
%=============================================================================
function r = ax_find(parent, i)
  r = i; while parent(r) ~= r; r = parent(r); end
end
function parent = ax_union(parent, a, b)
  ra = ax_find(parent, a); rb = ax_find(parent, b);
  if ra ~= rb; parent(rb) = ra; end
end
function [localNodes, nextLocal, lv] = ax_ensure(localNodes, nextLocal, nd)
  if isKey(localNodes, nd); lv = localNodes(nd);
  else; lv = nextLocal; localNodes(nd) = lv; nextLocal = nextLocal + 1; end
end
function v = ax_field(s, f, dflt)
  if isstruct(s) && isfield(s, f); v = s.(f); else; v = dflt; end
end
function v = ax_param(b, name, dflt)
  v = dflt;
  if isfield(b, 'params') && isfield(b.params, name); v = b.params.(name); end
end
function c = ax_cellrow(x)
  if iscell(x); c = x(:)'; else; c = num2cell(x(:)'); end
end
%=============================================================================
function doc = ax_expand_waveforms(doc)
  % Split each waveform-source block into a causal generator + a signal-driven
  % SignalVoltage / SignalCurrent, adding the feeding connection. The type map is
  % { catalog block type, causal source type, acausal signal-source type }.
  % { catalog block type, causal source type, acausal signal-source type }. Only
  % smooth (continuous) generators are listed: a discontinuous source (pulse /
  % sawtooth / step) makes the variable-step DAE solver fail at its edges, so
  % those await the solver's source-edge event handling.
  map = {'SineVoltage', 'sine', 'SignalVoltage'; ...
    'SineCurrent', 'sine', 'SignalCurrent'; ...
    'ExpSineVoltage', 'expSine', 'SignalVoltage'; ...
    'ExpSineCurrent', 'expSine', 'SignalCurrent'; ...
    'TrapezoidVoltage', 'trapezoid', 'SignalVoltage'; ...
    'TrapezoidCurrent', 'trapezoid', 'SignalCurrent'; ...
    'RampVoltage', 'ramp', 'SignalVoltage'; ...
    'RampCurrent', 'ramp', 'SignalCurrent'; ...
    'SineForce', 'sine', 'Force'; ...
    'RampForce', 'ramp', 'Force'; ...
    'ExpSineForce', 'expSine', 'Force'; ...
    'TrapezoidForce', 'trapezoid', 'Force'; ...
    'SineTorque', 'sine', 'Torque'; ...
    'RampTorque', 'ramp', 'Torque'; ...
    'ExpSineTorque', 'expSine', 'Torque'; ...
    'TrapezoidTorque', 'trapezoid', 'Torque'};
  blocks = ax_cellrow(doc.blocks);
  conns = {};
  if isfield(doc, 'connections') && ~isempty(doc.connections)
    conns = ax_cellrow(doc.connections);
  end
  out = {};
  changed = false;
  for i = 1:numel(blocks)
    b = blocks{i};
    row = find(strcmp(map(:, 1), ax_field(b, 'type', '')), 1);
    if isempty(row)
      out{end + 1} = b; %#ok<AGROW>
      continue
    end
    changed = true;
    srcId = [b.id, '__wav'];
    src = struct('id', srcId, 'type', map{row, 2}, 'inputs', 0, 'outputs', 1);
    if isfield(b, 'params')
      src.params = ax_runtime_source_params(b.params);
    end
    src.x = ax_field(b, 'x', 0) - 150; src.y = ax_field(b, 'y', 0); src.rotation = 0;
    out{end + 1} = src; %#ok<AGROW>
    b.type = map{row, 3};
    out{end + 1} = b; %#ok<AGROW>
    conns{end + 1} = struct('from', srcId, 'to', b.id, 'fromIndex', 0, 'toIndex', 0); %#ok<AGROW>
  end
  if changed
    doc.blocks = out;
    doc.connections = conns;
  end
end
%=============================================================================
function ax_validate_acausal_wire_pins(doc, catalog, blocks, isAc, catOf)
  acById = containers.Map('KeyType', 'char', 'ValueType', 'double');
  validPins = containers.Map('KeyType', 'char', 'ValueType', 'logical');
  for i = find(isAc)
    acById(blocks{i}.id) = i;
    ci = catalog(catOf(i));
    for pp = 1:numel(ci.pins)
      validPins([blocks{i}.id, '.', ci.pins{pp}{1}]) = true;
    end
  end
  wires = ax_cellrow(doc.physicalWires);
  for w = 1:numel(wires)
    pins = ax_cellrow(wires{w}.pins);
    touchesAcausal = false;
    for p = 1:numel(pins)
      key = char(pins{p});
      dot = find(key == '.', 1, 'last');
      if ~isempty(dot) && isKey(acById, key(1:dot - 1))
        touchesAcausal = true;
        break
      end
    end
    if ~touchesAcausal
      continue
    end
    for p = 1:numel(pins)
      key = char(pins{p});
      dot = find(key == '.', 1, 'last');
      if isempty(dot) || dot == 1 || dot == numel(key) || ~isKey(validPins, key)
        error('Nelson:nflow:acausalUnknownPin', ...
          'Physical wire #%d references unknown acausal pin ''%s''.', w, key);
      end
    end
  end
end
%=============================================================================
function params = ax_runtime_source_params(params)
  if isfield(params, 'Slope') && ~isfield(params, 'slope')
    params.slope = params.Slope;
  end
  if isfield(params, 'StartTime') && ~isfield(params, 'start')
    params.start = params.StartTime;
  end
end
%=============================================================================
function doc = ax_expand_composites(doc)
  % Split a SpringDamper / RotSpringDamper into a parallel spring + damper on the
  % same physical net. map = { block type, spring type, spring param, damper type }.
  map = {'SpringDamper', 'Spring', 'k', 'Damper'; ...
    'RotSpringDamper', 'RotSpring', 'c', 'RotDamper'};
  blocks = ax_cellrow(doc.blocks);
  splitIds = containers.Map('KeyType', 'char', 'ValueType', 'any');
  out = {};
  for i = 1:numel(blocks)
    b = blocks{i};
    t = ax_field(b, 'type', '');
    row = find(strcmp(map(:, 1), t), 1);
    if ~isempty(row)
      kId = [b.id, '_k']; dId = [b.id, '_d'];
      sp = struct('id', kId, 'type', map{row, 2}, 'params', struct());
      dm = struct('id', dId, 'type', map{row, 4}, 'params', struct());
      sk = map{row, 3};
      if isfield(b, 'params')
        if isfield(b.params, sk); sp.params.(sk) = b.params.(sk); end
        if isfield(b.params, 'd'); dm.params.d = b.params.d; end
      end
      out{end + 1} = sp; out{end + 1} = dm; %#ok<AGROW>
      splitIds(b.id) = {kId, dId};
    elseif strcmp(t, 'MassWithWeight')
      % Mass + a constant gravity force (F = -m g) on the same flange net.
      mId = [b.id, '_m']; wId = [b.id, '_w'];
      m = ax_param(b, 'm', 1); g = ax_param(b, 'g', 9.81);
      mass = struct('id', mId, 'type', 'Mass', 'params', ...
        struct('m', m, 's0', ax_param(b, 's0', 0), 'v0', ax_param(b, 'v0', 0)));
      wt = struct('id', wId, 'type', 'ConstantForce', 'params', struct('F', -m * g));
      out{end + 1} = mass; out{end + 1} = wt; %#ok<AGROW>
      splitIds(b.id) = {mId, wId};
    else
      out{end + 1} = b; %#ok<AGROW>
    end
  end
  if splitIds.Count == 0
    return
  end
  doc.blocks = out;
  % Rewrite the physicalWires: a pin "<SD>.a" becomes both "<SD>_k.a" and
  % "<SD>_d.a" so the spring and damper share the source's net.
  pw = ax_cellrow(doc.physicalWires);
  for w = 1:numel(pw)
    net = pw{w};
    pins = ax_cellrow(net.pins);
    newPins = {};
    for p = 1:numel(pins)
      key = pins{p};
      dot = find(key == '.', 1, 'last');
      bid = key(1:dot - 1); pin = key(dot:end);
      if ~isempty(dot) && isKey(splitIds, bid)
        v = splitIds(bid);
        newPins{end + 1} = [v{1}, pin]; %#ok<AGROW>
        newPins{end + 1} = [v{2}, pin]; %#ok<AGROW>
      else
        newPins{end + 1} = key; %#ok<AGROW>
      end
    end
    net.pins = newPins;
    pw{w} = net;
  end
  doc.physicalWires = pw;
end
%=============================================================================
function doc = ax_expand_heating(doc)
  % Cross-domain bridge for the HeatingResistor: an electrical resistor whose
  % dissipated power P = v^2 / R is injected as heat into a thermal port. There
  % is no unified multi-domain island; instead the block is split into an
  % electrical Resistor + VoltageSensor, a causal chain (square then gain 1/R)
  % that forms P from the sensed voltage, and a thermal PrescribedHeatFlow driven
  % by P. The coupling is one-way (electrical -> thermal) and flows entirely
  % through the causal signal layer, so no engine change is needed. R is constant
  % here (temperature feedback would make it a two-way algebraic loop).
  blocks = ax_cellrow(doc.blocks);
  pinRewrite = containers.Map('KeyType', 'char', 'ValueType', 'any');
  addBlocks = {};
  addConns = {};
  changed = false;
  for i = 1:numel(blocks)
    b = blocks{i};
    if ~strcmp(ax_field(b, 'type', ''), 'HeatingResistor')
      addBlocks{end + 1} = b; %#ok<AGROW>
      continue
    end
    changed = true;
    R = ax_param(b, 'R', 1);
    rId = [b.id, '_R']; vsId = [b.id, '_vs'];
    sqId = [b.id, '_sq']; gId = [b.id, '_g']; hfId = [b.id, '_hf'];
    addBlocks{end + 1} = struct('id', rId, 'type', 'Resistor', 'params', struct('R', R)); %#ok<AGROW>
    addBlocks{end + 1} = struct('id', vsId, 'type', 'VoltageSensor', 'params', struct()); %#ok<AGROW>
    addBlocks{end + 1} = struct('id', sqId, 'type', 'mathFunction', ...
      'inputs', 1, 'outputs', 1, 'params', struct('Function', 'square')); %#ok<AGROW>
    addBlocks{end + 1} = struct('id', gId, 'type', 'gain', ...
      'inputs', 1, 'outputs', 1, 'params', struct('Gain', 1 / R)); %#ok<AGROW>
    addBlocks{end + 1} = struct('id', hfId, 'type', 'PrescribedHeatFlow', 'params', struct()); %#ok<AGROW>
    addConns{end + 1} = struct('from', vsId, 'to', sqId, 'fromIndex', 0, 'toIndex', 0); %#ok<AGROW>
    addConns{end + 1} = struct('from', sqId, 'to', gId, 'fromIndex', 0, 'toIndex', 0); %#ok<AGROW>
    addConns{end + 1} = struct('from', gId, 'to', hfId, 'fromIndex', 0, 'toIndex', 0); %#ok<AGROW>
    % p / n feed both the resistor and the sensor (same electrical node);
    % heatPort feeds the thermal heat-flow source.
    pinRewrite([b.id, '.p']) = {[rId, '.p'], [vsId, '.p']};
    pinRewrite([b.id, '.n']) = {[rId, '.n'], [vsId, '.n']};
    pinRewrite([b.id, '.heatPort']) = {[hfId, '.port']};
  end
  if ~changed
    return
  end
  doc.blocks = addBlocks;
  if isfield(doc, 'connections') && ~isempty(doc.connections)
    doc.connections = [ax_cellrow(doc.connections), addConns];
  else
    doc.connections = addConns;
  end
  pw = ax_cellrow(doc.physicalWires);
  for w = 1:numel(pw)
    net = pw{w};
    pins = ax_cellrow(net.pins);
    newPins = {};
    for p = 1:numel(pins)
      key = char(pins{p});
      if isKey(pinRewrite, key)
        newPins = [newPins, pinRewrite(key)]; %#ok<AGROW>
      else
        newPins{end + 1} = key; %#ok<AGROW>
      end
    end
    net.pins = newPins;
    pw{w} = net;
  end
  doc.physicalWires = pw;
end
%=============================================================================
function doc = ax_expand_emf(doc)
  % Cross-domain bridge for the electro-mechanical EMF (Coselica EMF): couples an
  % electrical port (p, n) to a rotational flange with a gyrator-like relation,
  % back-emf v = k w and motor torque tau = k i. Two-way coupling, but the loop
  % is broken by the rotational inertia's speed state w, so it still reduces to
  % single-domain islands bridged by the causal signal layer:
  %   electrical: a SignalVoltage(p -> mid, = k w) in series with a CurrentSensor
  %               (mid -> n) measuring the branch current i;
  %   rotational: a RotSpeedSensor at the flange feeding v, and a Torque at the
  %               flange driven by k i.
  % No engine change: w is a state, so at each step v = k w is known, the
  % electrical island yields i, and tau = k i drives the inertia. Requires the DAE
  % solver (a mechanical ODE island next to an electrical DAE island; ode4 does not
  % solve the electrical algebraic part). map is { block type, torque block, speed
  % sensor } (TranslationalEMF swaps in Force / SpeedSensor).
  map = {'EMF', 'Torque', 'RotSpeedSensor'; ...
    'TranslationalEMF', 'Force', 'SpeedSensor'};
  blocks = ax_cellrow(doc.blocks);
  pinRewrite = containers.Map('KeyType', 'char', 'ValueType', 'any');
  addBlocks = {};
  addConns = {};
  addWires = {};
  changed = false;
  for i = 1:numel(blocks)
    b = blocks{i};
    row = find(strcmp(map(:, 1), ax_field(b, 'type', '')), 1);
    if isempty(row)
      addBlocks{end + 1} = b; %#ok<AGROW>
      continue
    end
    changed = true;
    k = ax_param(b, 'k', 1);
    vId = [b.id, '_v']; csId = [b.id, '_cs'];
    wsId = [b.id, '_ws']; tqId = [b.id, '_tq'];
    gvId = [b.id, '_gv']; gtId = [b.id, '_gt'];
    midPin = [vId, '.n']; % internal electrical node between source and ammeter
    addBlocks{end + 1} = struct('id', vId, 'type', 'SignalVoltage', 'params', struct()); %#ok<AGROW>
    addBlocks{end + 1} = struct('id', csId, 'type', 'CurrentSensor', 'params', struct()); %#ok<AGROW>
    addBlocks{end + 1} = struct('id', wsId, 'type', map{row, 3}, 'params', struct()); %#ok<AGROW>
    addBlocks{end + 1} = struct('id', tqId, 'type', map{row, 2}, 'params', struct()); %#ok<AGROW>
    addBlocks{end + 1} = struct('id', gvId, 'type', 'gain', ...
      'inputs', 1, 'outputs', 1, 'params', struct('Gain', k)); %#ok<AGROW>
    addBlocks{end + 1} = struct('id', gtId, 'type', 'gain', ...
      'inputs', 1, 'outputs', 1, 'params', struct('Gain', k)); %#ok<AGROW>
    % w -> gain k -> back-emf source ; i -> gain k -> torque
    addConns{end + 1} = struct('from', wsId, 'to', gvId, 'fromIndex', 0, 'toIndex', 0); %#ok<AGROW>
    addConns{end + 1} = struct('from', gvId, 'to', vId, 'fromIndex', 0, 'toIndex', 0); %#ok<AGROW>
    addConns{end + 1} = struct('from', csId, 'to', gtId, 'fromIndex', 0, 'toIndex', 0); %#ok<AGROW>
    addConns{end + 1} = struct('from', gtId, 'to', tqId, 'fromIndex', 0, 'toIndex', 0); %#ok<AGROW>
    % internal series node: SignalVoltage.n joins CurrentSensor.p
    addWires{end + 1} = struct('pins', {{midPin, [csId, '.p']}}); %#ok<AGROW>
    % external pins: p -> source.p ; n -> ammeter.n ; flange -> sensor + torque
    pinRewrite([b.id, '.p']) = {[vId, '.p']};
    pinRewrite([b.id, '.n']) = {[csId, '.n']};
    pinRewrite([b.id, '.flange']) = {[wsId, '.flange'], [tqId, '.flange']};
  end
  if ~changed
    return
  end
  doc.blocks = addBlocks;
  if isfield(doc, 'connections') && ~isempty(doc.connections)
    doc.connections = [ax_cellrow(doc.connections), addConns];
  else
    doc.connections = addConns;
  end
  pw = ax_cellrow(doc.physicalWires);
  for w = 1:numel(pw)
    net = pw{w};
    pins = ax_cellrow(net.pins);
    newPins = {};
    for p = 1:numel(pins)
      key = char(pins{p});
      if isKey(pinRewrite, key)
        newPins = [newPins, pinRewrite(key)]; %#ok<AGROW>
      else
        newPins{end + 1} = key; %#ok<AGROW>
      end
    end
    net.pins = newPins;
    pw{w} = net;
  end
  doc.physicalWires = [pw, addWires];
end
%=============================================================================
function [doc, changed] = ax_expand_subsystems(doc, catalog, catNames)
  % Depth-first Tier-1 recursion: lower the inner acausal diagram of every PLAIN
  % subsystem in place. A controlled subsystem that (recursively) contains an
  % acausal component is rejected - a physical (DAE) island cannot be gated.
  changed = false;
  if ~isfield(doc, 'blocks') || isempty(doc.blocks)
    return
  end
  blocks = ax_cellrow(doc.blocks);
  anyChange = false;
  for i = 1:numel(blocks)
    b = blocks{i};
    if ~strcmp(ax_field(b, 'type', ''), 'subsystem') || ~isfield(b, 'params')
      continue
    end
    if ~isfield(b.params, 'subsystem') || ~isstruct(b.params.subsystem) ...
        || ~isfield(b.params.subsystem, 'blocks')
      continue
    end
    inner = b.params.subsystem;
    if ~ax_inner_has_acausal(inner, catNames)
      continue
    end
    if ax_is_controlled_subsystem(b)
      error('Nelson:nflow:acausalInSubsystemUnsupported', ...
        ['Subsystem ''%s'' is a controlled subsystem (enable / trigger / reset / ', ...
         'action / iterator / for-each) and cannot contain acausal components: a ', ...
         'physical (DAE) island cannot be conditionally gated. Keep the acausal ', ...
         'components in a plain subsystem or at the top level of the diagram.'], ...
        ax_field(b, 'id', '?'));
    end
    % Lower nested plain subsystems first, then this level's own physical wires.
    [inner, innerChanged] = ax_expand_subsystems(inner, catalog, catNames);
    if isfield(inner, 'physicalWires') && ~isempty(inner.physicalWires)
      inner = ax_lower_diagram(inner, catalog, catNames);
      innerChanged = true;
    end
    if innerChanged
      b.params.subsystem = ax_force_list_arrays(inner);
      blocks{i} = b;
      anyChange = true;
    end
  end
  if anyChange
    doc.blocks = blocks;
    changed = true;
  end
end
%=============================================================================
function tf = ax_inner_has_acausal(inner, catNames)
  % True if the subsystem's inner diagram contains an acausal catalog component
  % at any depth (including inside nested subsystems).
  tf = false;
  if ~isstruct(inner) || ~isfield(inner, 'blocks') || isempty(inner.blocks)
    return
  end
  innerBlocks = ax_cellrow(inner.blocks);
  for j = 1:numel(innerBlocks)
    ib = innerBlocks{j};
    if any(strcmp(catNames, ax_field(ib, 'type', '')))
      tf = true;
      return
    end
    if strcmp(ax_field(ib, 'type', ''), 'subsystem') && isfield(ib, 'params') ...
        && isfield(ib.params, 'subsystem') ...
        && ax_inner_has_acausal(ib.params.subsystem, catNames)
      tf = true;
      return
    end
  end
end
%=============================================================================
function tf = ax_is_controlled_subsystem(b)
  % A subsystem is controlled (cannot host a DAE island) when it carries a
  % controlPort (enable / trigger / enableTrigger / reset / action / functionCall
  % / iterator) or a for-each specification, at any place the engine reads them
  % (SubsystemHandlers.cpp reads params.controlPort, subsystem.controlPort and
  % subsystem.params.controlPort; forEachInputDivision reads params.forEach).
  tf = false;
  if ~isfield(b, 'params') || ~isstruct(b.params)
    return
  end
  if ax_has_object(b.params, 'controlPort') || ax_has_object(b.params, 'forEach')
    tf = true;
    return
  end
  if isfield(b.params, 'subsystem') && isstruct(b.params.subsystem)
    sub = b.params.subsystem;
    if ax_has_object(sub, 'controlPort') || ax_has_object(sub, 'forEach')
      tf = true;
      return
    end
    if isfield(sub, 'params') && isstruct(sub.params) ...
        && (ax_has_object(sub.params, 'controlPort') || ax_has_object(sub.params, 'forEach'))
      tf = true;
    end
  end
end
%=============================================================================
function tf = ax_has_object(s, f)
  % Mirror the engine's `is_object() && !empty()` test: a present, non-empty
  % struct field (an empty {} controlPort means "always active", i.e. plain).
  tf = isstruct(s) && isfield(s, f) && isstruct(s.(f)) && ~isempty(fieldnames(s.(f)));
end
%=============================================================================
function s = ax_force_list_arrays(s)
  % jsondecode turns a 1-element JSON array into a scalar struct, and jsonencode
  % then re-emits it as a JSON object; but the parsers read blocks / connections /
  % externalInputs / externalOutputs only when they are JSON arrays. Force those
  % fields to cell arrays so the round-trip keeps them as arrays (a scalar struct
  % would otherwise silently drop a single connection, giving an all-zero trace).
  % Applied both to a subsystem's inner spec and to the top-level document.
  arrFields = {'blocks', 'connections', 'externalInputs', 'externalOutputs'};
  for k = 1:numel(arrFields)
    f = arrFields{k};
    if isfield(s, f) && ~isempty(s.(f)) && ~iscell(s.(f))
      s.(f) = ax_cellrow(s.(f));
    end
  end
end
%=============================================================================
