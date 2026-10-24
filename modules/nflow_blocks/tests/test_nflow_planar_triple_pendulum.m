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
% The gallery model is a native three-link rigid mechanism and its scopes
% receive finite, moving trajectories from a complete DAE simulation.
modelPath = [modulepath('nflow_blocks', 'root'), ...
  '/examples/acausal/Planar_Rigid_Triple_Pendulum_Demo.nflow'];
document = jsondecode(fileread(modelPath));
blocks = document.blocks;
if ~iscell(blocks)
  blocks = num2cell(blocks);
end
blockTypes = cellfun(@(block) block.type, blocks, ...
  'UniformOutput', false);
assert_isequal(sum(strcmp(blockTypes, 'subsystem')), 1);
assert_isequal(sum(strcmp(blockTypes, 'scope')), 1);
assert_isequal(sum(strcmp(blockTypes, 'xyScope')), 1);
assert_isequal(sum(strcmp(blockTypes, 'toWorkspace')), 0);
assert_isequal(document.solver, 'dae');
assert_istrue(document.realTime);
assert_isfalse(isfield(document, 'physicalWires'));

mechanismIndex = find(strcmp(blockTypes, 'subsystem'), 1);
mechanism = blocks{mechanismIndex};
assert_isequal(mechanism.outputs, 5);
inner = mechanism.params.subsystem;
innerBlocks = inner.blocks;
if ~iscell(innerBlocks)
  innerBlocks = num2cell(innerBlocks);
end
innerTypes = cellfun(@(block) block.type, innerBlocks, ...
  'UniformOutput', false);
assert_isequal(sum(strcmp(innerTypes, 'planarMechanicalIsland')), 1);
island = innerBlocks{find(strcmp(innerTypes, 'planarMechanicalIsland'), 1)};
components = island.params.components;
if ~iscell(components)
  components = num2cell(components);
end
componentKinds = cellfun(@(component) component.kind, components, ...
  'UniformOutput', false);
assert_isequal(sum(strcmp(componentKinds, 'body')), 3);
assert_isequal(sum(strcmp(componentKinds, 'revolute')), 3);
revoluteComponents = components(strcmp(componentKinds, 'revolute'));
fixedPivot = revoluteComponents{1};
assert_isequal(fixedPivot.body, 0);
assert_isequal(fixedPivot.body2, -1);
assert_isequal([fixedPivot.px fixedPivot.py], [0 0]);

result = jsondecode(__nflow_simulate__(fileread(modelPath)));
assert_isequal(result.status, 'done');
scopes = result.scopes;
if ~iscell(scopes)
  scopes = num2cell(scopes);
end
scopeTypes = cellfun(@(scope) scope.type, scopes, ...
  'UniformOutput', false);

angleScope = scopes{find(strcmp(scopeTypes, 'scope'), 1)};
assert_istrue(numel(angleScope.times) > 100);
assert_isequal(size(angleScope.series, 1), 3);
assert_istrue(all(isfinite(angleScope.series(:))));
assert_istrue(max(angleScope.series(1, :)) - ...
  min(angleScope.series(1, :)) > 1);

trajectory = scopes{find(strcmp(scopeTypes, 'xyScope'), 1)};
assert_istrue(numel(trajectory.x) > 100);
assert_isequal(numel(trajectory.x), numel(trajectory.y));
assert_istrue(all(isfinite(trajectory.x)));
assert_istrue(all(isfinite(trajectory.y)));
assert_istrue(max(trajectory.x) - min(trajectory.x) > 1);
assert_istrue(max(trajectory.y) - min(trajectory.y) > 1);
assert_isapprox(trajectory.x(1), 3, 1e-10);
assert_isapprox(trajectory.y(1), 0, 1e-10);
%=============================================================================
