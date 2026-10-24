%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% sqrt: elementwise sqrt/signedSqrt/rSqrt selected by a Function parameter.
% Feedthrough; logged at t0 (an integrator supplies the state the fixed-step
% solver requires).
%=============================================================================
function p = sq_write(json, stem)
  p = [tempname(), '.nflow'];
  fid = fopen(p, 'w'); fwrite(fid, json); fclose(fid);
end
%=============================================================================
function y = sq_eval(fn, value)
  m = ['{"blocks":[', ...
    '{"id":"c","type":"constant","inputs":0,"outputs":1,"params":{"Value":', ...
      sprintf('%.15g', value), '}},', ...
    '{"id":"f","type":"sqrt","inputs":1,"outputs":1,"params":{"Function":"', fn, '"}},', ...
    '{"id":"ig","type":"integrator","inputs":1,"outputs":1,"params":{"InitialCondition":0}},', ...
    '{"id":"td","type":"toWorkspace","inputs":1,"outputs":0,"params":{"VariableName":"d"}},', ...
    '{"id":"tw","type":"toWorkspace","inputs":1,"outputs":0,"params":{"VariableName":"y"}}],', ...
    '"connections":[', ...
      '{"from":"c","to":"f","fromIndex":0,"toIndex":0},', ...
      '{"from":"f","to":"tw","fromIndex":0,"toIndex":0},', ...
      '{"from":"c","to":"ig","fromIndex":0,"toIndex":0},', ...
      '{"from":"ig","to":"td","fromIndex":0,"toIndex":0}],', ...
    '"solver":"ode1","runtime":0.1,"sampleTime":0.1}'];
  out = sim(sq_write(m, ['test_nflow_sq_', fn]));
  lg = out.logsout; nms = {lg.name};
  vals = lg(find(strcmp(nms, 'y'), 1)).values;
  y = vals(1);
end
%=============================================================================
assert_istrue(abs(sq_eval('sqrt', 9) - 3.0) < 1e-12);
assert_istrue(abs(sq_eval('sqrt', 2) - sqrt(2)) < 1e-12);
assert_istrue(abs(sq_eval('signedSqrt', -16) - (-4.0)) < 1e-12);
assert_istrue(abs(sq_eval('signedSqrt', 16) - 4.0) < 1e-12);
assert_istrue(abs(sq_eval('signedSqrt', 0) - 0.0) < 1e-12);
assert_istrue(abs(sq_eval('rSqrt', 4) - 0.5) < 1e-12);
%=============================================================================
