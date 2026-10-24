%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
% Every acausal / planar example diagram (an examples/*.nflow with a
% physicalWires section) must be BOTH graphically coherent -- every physical-wire
% pin refers to a real block and every net joins at least two pins, so the GUI
% renders wires between the blocks -- AND functional: it lowers through
% NFlow.internal.acausalExpand + NFlow.internal.planarExpand and simulates to completion.
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
exroot = [modulepath('nflow_blocks'), '/examples'];
% Examples are organised in one level of by-domain sub-directories
% (acausal/, dashboard/, causal/, ...); collect every .nflow across them.
subs = dir(exroot);
paths = {};
for s = 1:numel(subs)
  if ~subs(s).isdir || strcmp(subs(s).name, '.') || strcmp(subs(s).name, '..')
    continue
  end
  ff = dir([exroot, '/', subs(s).name, '/*.nflow']);
  for k = 1:numel(ff)
    paths{end + 1} = [exroot, '/', subs(s).name, '/', ff(k).name]; %#ok<SAGROW>
  end
end
assert_istrue(~isempty(paths));
tested = 0;
for f = 1:numel(paths)
  nm = paths{f};
  txt = fileread(nm);
  d = jsondecode(txt);
  if ~isfield(d, 'physicalWires') || isempty(d.physicalWires)
    continue % causal-only demo: covered elsewhere
  end
  tested = tested + 1;
  % --- graphical coherence: wires connect real blocks ---
  B = d.blocks; if ~iscell(B); B = num2cell(B); end
  ids = containers.Map('KeyType', 'char', 'ValueType', 'double');
  for i = 1:numel(B); ids(B{i}.id) = 1; end
  W = d.physicalWires; if ~iscell(W); W = num2cell(W); end
  for w = 1:numel(W)
    pins = W{w}.pins; if ~iscell(pins); pins = num2cell(pins); end
    assert_istrue(numel(pins) >= 2); % a net wires at least two pins
    for pp = 1:numel(pins)
      key = char(pins{pp});
      dot = find(key == '.', 1, 'last');
      assert_istrue(~isempty(dot)); % pin key is "<blockId>.<pinName>"
      bid = key(1:dot - 1);
      assert_istrue(isKey(ids, bid)); % the block exists
    end
  end
  % --- functional: it lowers and simulates to completion ---
  r = jsondecode(__nflow_simulate__(NFlow.internal.planarExpand(NFlow.internal.acausalExpand(txt))));
  if ~strcmp(r.status, 'done')
    error('Nelson:nflow:exampleFailed', ...
      'Example "%s" did not simulate (status=%s).', nm, r.status);
  end
end
assert_istrue(tested >= 10); % the acausal/planar demo set
%=============================================================================
