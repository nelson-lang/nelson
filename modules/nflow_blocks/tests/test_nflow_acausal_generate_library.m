%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% Acausal palette N0.2: NFlow.internal.acausalGenerateLibrary emits a GUI palette
% manifest per domain from the single-source catalog, defining the physical-pin
% format. The manifest must jsondecode to a well-formed library with acausal
% component blocks carrying physicalPins.
%=============================================================================
catalog = NFlow.internal.acausalCatalog();
for dom = {'electrical', 'thermal', 'translational', 'rotational'}
  d = dom{1};
  json = NFlow.internal.acausalGenerateLibrary(d);
  lib = jsondecode(json);
  assert_istrue(strcmp(lib.id, ['builtin.acausal_', d]));
  assert_istrue(strcmp(lib.format, 'nflow-2'));
  assert_istrue(lib.builtin == true);
  % Same number of blocks as catalog entries in this domain.
  n = 0;
  for k = 1:numel(catalog)
    if strcmp(catalog(k).domain, d); n = n + 1; end
  end
  assert_istrue(numel(lib.blocks) == n);
  % Every block is acausal, in this domain, with at least one physical pin and a
  % default-params object.
  for b = 1:numel(lib.blocks)
    blk = lib.blocks(b);
    assert_istrue(blk.acausal == true);
    assert_istrue(strcmp(blk.domain, d));
    assert_istrue(~isempty(blk.physicalPins));
    assert_istrue(~isempty(blk.type));
  end
end
%=============================================================================
% Electrical Resistor: two physical pins p/n, default R, no causal ports.
elib = jsondecode(NFlow.internal.acausalGenerateLibrary('electrical'));
types = arrayfun(@(x) string(x.type), elib.blocks);
ri = find(types == "Resistor", 1);
assert_istrue(~isempty(ri));
r = elib.blocks(ri);
assert_istrue(numel(r.physicalPins) == 2);
pinNames = arrayfun(@(x) string(x.name), r.physicalPins);
assert_istrue(any(pinNames == "p") && any(pinNames == "n"));
assert_istrue(abs(r.defaultParams.R - 1000) < 1e-9);
%=============================================================================
hi = find(types == "HeatingResistor", 1);
hr = elib.blocks(hi);
heatDomains = {};
for k = 1:numel(hr.physicalPins)
  if strcmp(hr.physicalPins(k).name, 'heatPort')
    heatDomains{end + 1} = hr.physicalPins(k).domain; %#ok<AGROW>
  end
end
assert_isequal(heatDomains, {'thermal'});
%=============================================================================
% A signal-driven source (SignalVoltage) carries a causal input port.
si = find(types == "SignalVoltage", 1);
sv = elib.blocks(si);
assert_istrue(~isempty(sv.inputs));
% A sensor (VoltageSensor) carries a causal output port.
vi = find(types == "VoltageSensor", 1);
vs = elib.blocks(vi);
assert_istrue(~isempty(vs.outputs));
%=============================================================================
% Unknown domain errors.
msg = '';
try
  NFlow.internal.acausalGenerateLibrary('planar');
catch e
  msg = e.identifier;
end
assert_istrue(strcmp(msg, 'Nelson:nflow:acausalGenDomain'));
%=============================================================================
