%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% documentJson = NFlow.internal.planarExpand(documentJson)
%
% Acausal palette N7: the planar 2-D multibody diagram lowering. Turns a
% frame-connected planar diagram (bodies, joints, sensors, forces wired by
% "physicalWires" over FRAME pins) into ONE native planarMechanicalIsland block
% whose params carry the resolved body list and component list. The sim()/Run
% counterpart of NFlow.internal.acausalExpand for the planar domain.
%
% Frame-diagram contract (the authoring model; the GUI N5 renders it):
%   - PlanarWorld  {id, params:{gravity}}            frame pin "frame" at origin
%   - PlanarFixed  {id, params:{x, y}}               frame pin "frame" at (x, y)
%   - PlanarBody   {id, params:{m, I, x0,y0,phi0, vx0,vy0,w0, frames:{n:[ax,ay]}}}
%                    frame pins = the declared frame names + "com" ([0,0])
%   - PlanarRevolute / PlanarPrismatic {params:{dx,dy}} / PlanarDistance
%                    {params:{length}}               two frame pins "a" and "b"
%   - PlanarPositionSensor{axis:'x'|'y'|'phi'} / PlanarVelocitySensor{axis} /
%     PlanarDistanceSensor / PlanarForceSensor       frame pin "a" (+ "b" for
%                    relative/distance sensors); a signal output to "connections"
%   - PlanarForce {params:{fx,fy}} / PlanarTorque {params:{tau}}   frame pin "a"
%   - "physicalWires": undirected nets over frame pins "<blockId>.<frameName>".
%
% Each wired net resolves to ONE physical point whose anchor is the single body
% frame or world/fixed frame in the net; joint/sensor/force frames in the net
% reference that anchor (body index + body-frame offset, or a fixed world
% point). A net with two body frames (a rigid weld) or none is rejected.
%=============================================================================
function documentJson = planarExpand(documentJson)
  if isempty(strfind(documentJson, 'physicalWires'))
    return
  end
  doc = jsondecode(documentJson);
  if ~isfield(doc, 'physicalWires') || isempty(doc.physicalWires)
    return
  end
  blocks = pl_cellrow(doc.blocks);
  ptypes = {'PlanarWorld', 'PlanarFixed', 'PlanarBody', 'PlanarRevolute', ...
    'PlanarPrismatic', 'PlanarDistance', 'PlanarPositionSensor', ...
    'PlanarVelocitySensor', 'PlanarAccelerationSensor', 'PlanarDistanceSensor', ...
    'PlanarForceSensor', 'PlanarForce', 'PlanarTorque', 'PlanarRelativeTorque', ...
    'PlanarPointMass', 'PlanarRollingWheel', 'PlanarSpringDamper', ...
    'PlanarSpring', 'PlanarDamper', 'PlanarRelPositionSensor'};
  isPl = false(1, numel(blocks));
  for i = 1:numel(blocks)
    isPl(i) = any(strcmp(ptypes, pl_field(blocks{i}, 'type', '')));
  end
  if ~any(isPl)
    return
  end

  % ---- body indices + gravity ----------------------------------------------
  bodyIndex = containers.Map('KeyType', 'char', 'ValueType', 'double');
  bodyList = {};
  gravity = 9.81;
  for i = find(isPl)
    t = blocks{i}.type;
    if strcmp(t, 'PlanarWorld')
      gravity = pl_param(blocks{i}, 'gravity', 9.81);
    elseif strcmp(t, 'PlanarBody') || strcmp(t, 'PlanarPointMass')
      bi = numel(bodyList);
      bodyIndex(blocks{i}.id) = bi;
      bodyList{end + 1} = blocks{i}; %#ok<AGROW>
    end
  end

  % ---- frame pins + union-find over physicalWires ---------------------------
  % A frame pin key is "<blockId>.<frameName>". Collect every block's frames.
  pinKeys = {};
  pinAnchor = {}; % parallel: anchor struct for body/fixed pins, else []
  for i = find(isPl)
    b = blocks{i};
    fr = pl_frames(b);
    for k = 1:size(fr, 1)
      pinKeys{end + 1} = [b.id, '.', fr{k, 1}]; %#ok<AGROW>
      a = fr{k, 2};
      if ~isempty(a) && isfield(a, 'ownerBody')
        a.body = bodyIndex(a.ownerBody); % resolve to the real body index
      end
      pinAnchor{end + 1} = a; %#ok<AGROW>
    end
  end
  parent = 1:numel(pinKeys);
  wires = pl_cellrow(doc.physicalWires);
  for w = 1:numel(wires)
    pins = pl_cellrow(wires{w}.pins);
    if numel(pins) < 2
      continue
    end
    i0 = find(strcmp(pinKeys, char(pins{1})), 1);
    for j = 2:numel(pins)
      ij = find(strcmp(pinKeys, char(pins{j})), 1);
      if ~isempty(i0) && ~isempty(ij)
        parent = pl_union(parent, i0, ij);
      end
    end
  end

  % ---- resolve each net's anchor (the body/fixed frame in the net) ----------
  % anchorOf(root) = struct(kind, body, ax, ay, px, py).
  anchorOf = containers.Map('KeyType', 'double', 'ValueType', 'any');
  for i = 1:numel(pinKeys)
    a = pinAnchor{i};
    if isempty(a)
      continue
    end
    r = pl_find(parent, i);
    if isKey(anchorOf, r)
      error('Nelson:nflow:planarWeld', ...
        ['Planar wire net has two anchor frames (a rigid weld is not ', ...
         'supported); connect through a joint instead.']);
    end
    anchorOf(r) = a;
  end

  % ---- emit components ------------------------------------------------------
  comps = {};
  outPort = 0;
  inPort = 0;
  sigOutMap = containers.Map('KeyType', 'char', 'ValueType', 'any');
  islId = '__planar_island';

  % bodies
  for bi = 1:numel(bodyList)
    b = bodyList{bi};
    cbody = struct('kind', 'body', 'body', bi - 1, ...
      'm', pl_param(b, 'm', 1), 'I', pl_param(b, 'I', 1), ...
      'x0', pl_param(b, 'x0', 0), 'y0', pl_param(b, 'y0', 0), ...
      'phi0', pl_param(b, 'phi0', 0), 'vx0', pl_param(b, 'vx0', 0), ...
      'vy0', pl_param(b, 'vy0', 0), 'w0', pl_param(b, 'w0', 0));
    if strcmp(b.type, 'PlanarPointMass')
      cbody.pointMass = 1;
    end
    comps{end + 1} = cbody; %#ok<AGROW>
  end

  % joints, forces, sensors
  for i = find(isPl)
    b = blocks{i};
    t = b.type;
    switch t
      case {'PlanarRevolute', 'PlanarPrismatic', 'PlanarDistance'}
        A = pl_anchor([b.id, '.a'], pinKeys, parent, anchorOf);
        B = pl_anchor([b.id, '.b'], pinKeys, parent, anchorOf);
        % Put a body on side A for the world-pin forms (joints are symmetric).
        if strcmp(A.kind, 'fixed') && strcmp(B.kind, 'body')
          tmp = A; A = B; B = tmp;
        end
        if ~strcmp(A.kind, 'body')
          error('Nelson:nflow:planarJointGround', ...
            'Planar joint "%s" must attach at least one body.', b.id);
        end
        c = struct('body', A.body, 'ax', A.ax, 'ay', A.ay);
        if strcmp(B.kind, 'body')
          c.body2 = B.body; c.ax2 = B.ax; c.ay2 = B.ay;
        else
          c.body2 = -1; c.px = B.px; c.py = B.py;
        end
        if strcmp(t, 'PlanarRevolute')
          c.kind = 'revolute';
        elseif strcmp(t, 'PlanarPrismatic')
          c.kind = 'prismatic';
          c.dx = pl_param(b, 'dx', 1); c.dy = pl_param(b, 'dy', 0);
          c.phi0 = pl_param(b, 'phi0', 0);
        else
          c.kind = 'distanceJoint';
          c.length = pl_param(b, 'length', 1);
        end
        comps{end + 1} = c; %#ok<AGROW>
      case {'PlanarSpringDamper', 'PlanarSpring', 'PlanarDamper'}
        A = pl_anchor([b.id, '.a'], pinKeys, parent, anchorOf);
        B = pl_anchor([b.id, '.b'], pinKeys, parent, anchorOf);
        if strcmp(A.kind, 'fixed') && strcmp(B.kind, 'body')
          tmp = A; A = B; B = tmp;
        end
        if ~strcmp(A.kind, 'body')
          error('Nelson:nflow:planarSpringBody', ...
            'PlanarSpringDamper "%s" must attach at least one body.', b.id);
        end
        if strcmp(t, 'PlanarSpring')
          cc = pl_param(b, 'c', 1); dd = 0;
        elseif strcmp(t, 'PlanarDamper')
          cc = 0; dd = pl_param(b, 'd', 1);
        else
          cc = pl_param(b, 'c', 1); dd = pl_param(b, 'd', 0);
        end
        c = struct('kind', 'springDamper', 'body', A.body, 'ax', A.ax, 'ay', A.ay, ...
          'c', cc, 'd', dd);
        if strcmp(B.kind, 'body')
          c.body2 = B.body; c.ax2 = B.ax; c.ay2 = B.ay;
        else
          c.body2 = -1; c.px = B.px; c.py = B.py;
        end
        comps{end + 1} = c; %#ok<AGROW>
      case 'PlanarRollingWheel'
        A = pl_anchor([b.id, '.a'], pinKeys, parent, anchorOf);
        if ~strcmp(A.kind, 'body')
          error('Nelson:nflow:planarRollBody', ...
            'PlanarRollingWheel "%s" must attach a body at frame a.', b.id);
        end
        comps{end + 1} = struct('kind', 'rollingWheel', 'body', A.body, ...
          'dx', pl_param(b, 'dx', 1), 'dy', pl_param(b, 'dy', 0), ...
          'px', pl_param(b, 'px', 0), 'py', pl_param(b, 'py', 0), ...
          'radius', pl_param(b, 'radius', 1)); %#ok<AGROW>
      case 'PlanarForce'
        A = pl_anchor([b.id, '.a'], pinKeys, parent, anchorOf);
        fx = pl_param(b, 'fx', 0); fy = pl_param(b, 'fy', 0);
        if fx ~= 0
          comps{end + 1} = struct('kind', 'forceX', 'body', A.body, ...
            'ax', A.ax, 'ay', A.ay, 'F', fx); %#ok<AGROW>
        end
        if fy ~= 0
          comps{end + 1} = struct('kind', 'forceY', 'body', A.body, ...
            'ax', A.ax, 'ay', A.ay, 'F', fy); %#ok<AGROW>
        end
      case 'PlanarTorque'
        A = pl_anchor([b.id, '.a'], pinKeys, parent, anchorOf);
        comps{end + 1} = struct('kind', 'torque', 'body', A.body, ...
          'F', pl_param(b, 'tau', 0)); %#ok<AGROW>
      case 'PlanarRelativeTorque'
        A = pl_anchor([b.id, '.a'], pinKeys, parent, anchorOf);
        B = pl_anchor([b.id, '.b'], pinKeys, parent, anchorOf);
        c = struct('kind', 'relativeTorque', 'body', A.body, ...
          'F', pl_param(b, 'tau', 0));
        if strcmp(B.kind, 'body'); c.body2 = B.body; else; c.body2 = -1; end
        comps{end + 1} = c; %#ok<AGROW>
      case {'PlanarPositionSensor', 'PlanarVelocitySensor', 'PlanarAccelerationSensor'}
        A = pl_anchor([b.id, '.a'], pinKeys, parent, anchorOf);
        axis = pl_param_str(b, 'axis', 'x');
        c = struct('body', A.body, 'ax', A.ax, 'ay', A.ay, 'outPort', outPort);
        if strcmp(t, 'PlanarAccelerationSensor')
          if strcmp(axis, 'alpha')
            c.kind = 'accelSensorAlpha';
          else
            c.kind = ['accelSensor', upper(axis)];
          end
        elseif strcmp(axis, 'phi')
          c.kind = 'positionSensorPhi';
        elseif strcmp(axis, 'omega')
          c.kind = 'speedSensorOmega';
        else
          up = upper(axis);
          if strcmp(t, 'PlanarPositionSensor')
            c.kind = ['framePosition', up];
          else
            c.kind = ['frameVelocity', up];
          end
        end
        comps{end + 1} = c; %#ok<AGROW>
        sigOutMap(b.id) = {islId, outPort};
        outPort = outPort + 1;
      case 'PlanarDistanceSensor'
        A = pl_anchor([b.id, '.a'], pinKeys, parent, anchorOf);
        B = pl_anchor([b.id, '.b'], pinKeys, parent, anchorOf);
        c = struct('kind', 'distanceSensor', 'body', A.body, 'ax', A.ax, ...
          'ay', A.ay, 'outPort', outPort);
        if strcmp(B.kind, 'body')
          c.body2 = B.body; c.ax2 = B.ax; c.ay2 = B.ay;
        else
          c.body2 = -1; c.px = B.px; c.py = B.py;
        end
        comps{end + 1} = c; %#ok<AGROW>
        sigOutMap(b.id) = {islId, outPort};
        outPort = outPort + 1;
      case 'PlanarRelPositionSensor'
        A = pl_anchor([b.id, '.a'], pinKeys, parent, anchorOf);
        B = pl_anchor([b.id, '.b'], pinKeys, parent, anchorOf);
        c = struct('kind', ['relPosition', upper(pl_param_str(b, 'axis', 'x'))], ...
          'body', A.body, 'ax', A.ax, 'ay', A.ay, 'outPort', outPort);
        if strcmp(B.kind, 'body')
          c.body2 = B.body; c.ax2 = B.ax; c.ay2 = B.ay;
        else
          c.body2 = -1; c.px = B.px; c.py = B.py;
        end
        comps{end + 1} = c; %#ok<AGROW>
        sigOutMap(b.id) = {islId, outPort};
        outPort = outPort + 1;
    end
  end

  % ---- assemble the island block --------------------------------------------
  islParams = struct('domain', 'planar', 'bodies', numel(bodyList), ...
    'gravity', gravity, 'baumgarte', 30, 'components', {comps});
  isl = struct('id', islId, 'type', 'planarMechanicalIsland', ...
    'inputs', inPort, 'outputs', outPort, 'params', islParams);

  newBlocks = {isl};
  for i = 1:numel(blocks)
    if ~isPl(i)
      newBlocks{end + 1} = blocks{i}; %#ok<AGROW>
    end
  end

  % ---- rewrite sensor signal connections to island output ports -------------
  newConns = {};
  if isfield(doc, 'connections') && ~isempty(doc.connections)
    conns = pl_cellrow(doc.connections);
    for cc = 1:numel(conns)
      conn = conns{cc};
      fromId = pl_field(conn, 'from', '');
      if isKey(sigOutMap, fromId)
        v = sigOutMap(fromId);
        conn.from = v{1};
        conn.fromIndex = v{2};
      end
      newConns{end + 1} = conn; %#ok<AGROW>
    end
  end

  doc.blocks = newBlocks;
  if ~isempty(newConns); doc.connections = newConns; end
  doc = rmfield(doc, 'physicalWires');
  documentJson = jsonencode(doc);
end
%=============================================================================
function fr = pl_frames(b)
  % Return an N-by-2 cell: {frameName, anchorStruct-or-[]}. Anchor is set only
  % for body / world / fixed frames (the points that define a net).
  t = b.type;
  switch t
    case 'PlanarWorld'
      fr = {'frame', struct('kind', 'fixed', 'body', -1, 'ax', 0, 'ay', 0, 'px', 0, 'py', 0)};
    case 'PlanarFixed'
      fr = {'frame', struct('kind', 'fixed', 'body', -1, 'ax', 0, 'ay', 0, ...
        'px', pl_param(b, 'x', 0), 'py', pl_param(b, 'y', 0))};
    case {'PlanarBody', 'PlanarPointMass'}
      fr = {'com', struct('kind', 'body', 'body', 0, 'ax', 0, 'ay', 0, 'px', 0, 'py', 0)};
      names = {}; offs = {};
      if isfield(b, 'params') && isfield(b.params, 'frames')
        fs = b.params.frames;
        names = fieldnames(fs);
        for k = 1:numel(names)
          offs{k} = fs.(names{k}); %#ok<AGROW>
        end
      end
      for k = 1:numel(names)
        o = offs{k};
        fr{end + 1, 1} = names{k}; %#ok<AGROW>
        fr{end, 2} = struct('kind', 'body', 'body', 0, 'ax', o(1), 'ay', o(2), 'px', 0, 'py', 0);
      end
    case {'PlanarRevolute', 'PlanarPrismatic', 'PlanarDistance', 'PlanarRelativeTorque', 'PlanarDistanceSensor', 'PlanarSpringDamper', 'PlanarSpring', 'PlanarDamper', 'PlanarRelPositionSensor'}
      fr = {'a', []; 'b', []};
    otherwise % single-frame sensors / forces
      fr = {'a', []};
  end
  % The body anchors carry body index 0 as a placeholder; patch to the real
  % index by looking it up from the block id is done by the caller via the map.
  if strcmp(t, 'PlanarBody') || strcmp(t, 'PlanarPointMass')
    % mark that these anchors need the body's own index (resolved below)
    for k = 1:size(fr, 1)
      fr{k, 2}.ownerBody = b.id;
    end
  end
end
%=============================================================================
function an = pl_anchor(pinKey, pinKeys, parent, anchorOf)
  idx = find(strcmp(pinKeys, pinKey), 1);
  if isempty(idx)
    error('Nelson:nflow:planarPin', 'Unknown planar frame pin "%s".', pinKey);
  end
  r = pl_find(parent, idx);
  if ~isKey(anchorOf, r)
    error('Nelson:nflow:planarDangling', ...
      'Planar frame pin "%s" is not wired to a body or world frame.', pinKey);
  end
  an = anchorOf(r);
end
function r = pl_find(parent, i)
  r = i; while parent(r) ~= r; r = parent(r); end
end
function parent = pl_union(parent, a, b)
  ra = pl_find(parent, a); rb = pl_find(parent, b);
  if ra ~= rb; parent(rb) = ra; end
end
function v = pl_field(s, f, dflt)
  if isstruct(s) && isfield(s, f); v = s.(f); else; v = dflt; end
end
function v = pl_param(b, name, dflt)
  v = dflt;
  if isfield(b, 'params') && isfield(b.params, name); v = b.params.(name); end
end
function v = pl_param_str(b, name, dflt)
  v = dflt;
  if isfield(b, 'params') && isfield(b.params, name); v = b.params.(name); end
  if ~ischar(v); v = dflt; end
end
function c = pl_cellrow(x)
  if iscell(x); c = x(:)'; else; c = num2cell(x(:)'); end
end
%=============================================================================
