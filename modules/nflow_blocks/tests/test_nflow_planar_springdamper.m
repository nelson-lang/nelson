%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Planar 2-D spring-damper (PlanarSpringDamper): a linear elastic force
% F = -(c dr + d dv) between a body point and a second body point (or a fixed
% world point), applied with the torque r x F about each COM. Native
% planarMechanicalIsland kind springDamper.
%=============================================================================
function p = pl_write(json, stem)
  p = [tempname(), '.nflow'];
  fid = fopen(p, 'w'); fwrite(fid, json); fclose(fid);
end
%=============================================================================
% A body (m = 1) hangs from a fixed world point at the origin by a vertical
% spring-damper (c = 100, d = 20). Gravity (g = 9.81, down = -y) stretches it
% until the spring balances the weight: the COM settles at y = -m g / c ~ -0.0981.
g = 9.81;
d = ['{"blocks":[', ...
  '{"id":"isl","type":"planarMechanicalIsland","inputs":0,"outputs":1,"params":{', ...
    '"domain":"planar","bodies":1,"gravity":9.81,"components":[', ...
      '{"kind":"body","body":0,"m":1,"I":0.1},', ...
      '{"kind":"springDamper","body":0,"ax":0,"ay":0,"body2":-1,"px":0,"py":0,"c":100,"d":20},', ...
      '{"kind":"positionSensorY","body":0,"outPort":0}]}},', ...
  '{"id":"twY","type":"toWorkspace","inputs":1,"outputs":0,"params":{"VariableName":"Y"}}],', ...
  '"connections":[{"from":"isl","to":"twY","fromIndex":0,"toIndex":0}],', ...
  '"solver":"dae","runtime":3.0,"sampleTime":0.02}'];
out = sim(pl_write(d, 'test_nflow_planar_sd'));
lg = out.logsout; nms = {lg.name};
Y = lg(find(strcmp(nms, 'Y'), 1)).values;
Y = Y(:);
assert_istrue(abs(Y(end) - (-g / 100)) < 5e-3); % settles at -m g / c
assert_istrue(min(Y) > -0.25); % held by the spring (no free fall)
%=============================================================================
% Two bodies joined by a horizontal spring-damper along x, one pinned to the
% world by a revolute at the origin: the free body is pulled toward its neighbour
% and oscillates about the rest configuration rather than flying off.
d2 = ['{"blocks":[', ...
  '{"id":"isl","type":"planarMechanicalIsland","inputs":0,"outputs":1,"params":{', ...
    '"domain":"planar","bodies":2,"gravity":0,"components":[', ...
      '{"kind":"body","body":0,"m":1,"I":0.1,"x0":0,"y0":0},', ...
      '{"kind":"body","body":1,"m":1,"I":0.1,"x0":2,"y0":0},', ...
      '{"kind":"revolute","body":0,"ax":0,"ay":0,"body2":-1,"px":0,"py":0},', ...
      '{"kind":"springDamper","body":0,"ax":0,"ay":0,"body2":1,"ax2":0,"ay2":0,"c":50,"d":5},', ...
      '{"kind":"positionSensorX","body":1,"outPort":0}]}},', ...
  '{"id":"twX","type":"toWorkspace","inputs":1,"outputs":0,"params":{"VariableName":"X"}}],', ...
  '"connections":[{"from":"isl","to":"twX","fromIndex":0,"toIndex":0}],', ...
  '"solver":"dae","runtime":3.0,"sampleTime":0.02}'];
out = sim(pl_write(d2, 'test_nflow_planar_sd2'));
lg = out.logsout; nms = {lg.name};
X = lg(find(strcmp(nms, 'X'), 1)).values;
X = X(:);
assert_istrue(X(1) > 1.8); % body 1 starts near x = 2
assert_istrue(X(end) < 0.5); % pulled toward body 0 at the origin
%=============================================================================
