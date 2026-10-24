%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% roundingFunction: elementwise floor/ceil/round/fix selected by an Operator
% parameter. Feedthrough; logged at t0 (an integrator supplies the state the
% fixed-step solver requires).
%=============================================================================
function p = rf_write(json, stem)
  p = [tempname(), '.nflow'];
  fid = fopen(p, 'w'); fwrite(fid, json); fclose(fid);
end
%=============================================================================
function y = rf_eval(op, value)
  m = ['{"blocks":[', ...
    '{"id":"c","type":"constant","inputs":0,"outputs":1,"params":{"Value":', ...
      sprintf('%.15g', value), '}},', ...
    '{"id":"f","type":"roundingFunction","inputs":1,"outputs":1,"params":{"Operator":"', op, '"}},', ...
    '{"id":"ig","type":"integrator","inputs":1,"outputs":1,"params":{"InitialCondition":0}},', ...
    '{"id":"td","type":"toWorkspace","inputs":1,"outputs":0,"params":{"VariableName":"d"}},', ...
    '{"id":"tw","type":"toWorkspace","inputs":1,"outputs":0,"params":{"VariableName":"y"}}],', ...
    '"connections":[', ...
      '{"from":"c","to":"f","fromIndex":0,"toIndex":0},', ...
      '{"from":"f","to":"tw","fromIndex":0,"toIndex":0},', ...
      '{"from":"c","to":"ig","fromIndex":0,"toIndex":0},', ...
      '{"from":"ig","to":"td","fromIndex":0,"toIndex":0}],', ...
    '"solver":"ode1","runtime":0.1,"sampleTime":0.1}'];
  out = sim(rf_write(m, ['test_nflow_rf_', op]));
  lg = out.logsout; nms = {lg.name};
  vals = lg(find(strcmp(nms, 'y'), 1)).values;
  y = vals(1);
end
%=============================================================================
assert_istrue(abs(rf_eval('floor', 2.7) - 2.0) < 1e-12);
assert_istrue(abs(rf_eval('floor', -2.1) - (-3.0)) < 1e-12);
assert_istrue(abs(rf_eval('ceil', 2.1) - 3.0) < 1e-12);
assert_istrue(abs(rf_eval('ceil', -2.7) - (-2.0)) < 1e-12);
assert_istrue(abs(rf_eval('round', 2.5) - 3.0) < 1e-12);
assert_istrue(abs(rf_eval('round', -2.5) - (-3.0)) < 1e-12);
assert_istrue(abs(rf_eval('fix', 2.7) - 2.0) < 1e-12);
assert_istrue(abs(rf_eval('fix', -2.7) - (-2.0)) < 1e-12);
%=============================================================================
