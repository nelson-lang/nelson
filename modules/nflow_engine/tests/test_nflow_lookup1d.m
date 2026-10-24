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
% lookup1D: 1-D interpolated lookup table. Breakpoints [0 1 2 3 4], table
% [0 1 4 9 16]. Feedthrough; logged at t0 (an integrator supplies the state
% the fixed-step solver requires).
%=============================================================================
function p = lu_write(json, stem)
  p = [tempname(), '.nflow'];
  fid = fopen(p, 'w'); fwrite(fid, json); fclose(fid);
end
%=============================================================================
function y = lu_eval(interp, extrap, u)
  m = ['{"blocks":[', ...
    '{"id":"c","type":"constant","inputs":0,"outputs":1,"params":{"Value":', sprintf('%.15g', u), '}},', ...
    '{"id":"f","type":"lookup1D","inputs":1,"outputs":1,"params":{', ...
      '"BreakpointsForDimension1":[0,1,2,3,4],"Table":[0,1,4,9,16],', ...
      '"InterpMethod":"', interp, '","ExtrapMethod":"', extrap, '"}},', ...
    '{"id":"ig","type":"integrator","inputs":1,"outputs":1,"params":{"InitialCondition":0}},', ...
    '{"id":"td","type":"toWorkspace","inputs":1,"outputs":0,"params":{"VariableName":"d"}},', ...
    '{"id":"tw","type":"toWorkspace","inputs":1,"outputs":0,"params":{"VariableName":"y"}}],', ...
    '"connections":[', ...
      '{"from":"c","to":"f","fromIndex":0,"toIndex":0},', ...
      '{"from":"f","to":"tw","fromIndex":0,"toIndex":0},', ...
      '{"from":"c","to":"ig","fromIndex":0,"toIndex":0},', ...
      '{"from":"ig","to":"td","fromIndex":0,"toIndex":0}],', ...
    '"solver":"ode1","runtime":0.1,"sampleTime":0.1}'];
  out = sim(lu_write(m, 'test_nflow_lookup1d_eval'));
  lg = out.logsout; nms = {lg.name};
  y = lg(find(strcmp(nms, 'y'), 1)).values(1);
end
%=============================================================================
% Exact breakpoints.
assert_istrue(abs(lu_eval('Linear point-slope', 'Linear', 2) - 4.0) < 1e-12);
assert_istrue(abs(lu_eval('Linear point-slope', 'Linear', 0) - 0.0) < 1e-12);
% Linear interpolation at interior midpoints.
assert_istrue(abs(lu_eval('Linear point-slope', 'Linear', 0.5) - 0.5) < 1e-12);
assert_istrue(abs(lu_eval('Linear point-slope', 'Linear', 1.5) - 2.5) < 1e-12);
% Linear Lagrange matches point-slope in double precision.
assert_istrue(abs(lu_eval('Linear Lagrange', 'Linear', 1.5) - 2.5) < 1e-12);
% Flat: holds the lower breakpoint value inside the interval.
assert_istrue(abs(lu_eval('Flat', 'Clip', 1.5) - 1.0) < 1e-12);
% Nearest: rounds the fraction.
assert_istrue(abs(lu_eval('Nearest', 'Clip', 1.4) - 1.0) < 1e-12);
assert_istrue(abs(lu_eval('Nearest', 'Clip', 1.6) - 4.0) < 1e-12);
% Extrapolation below the range.
assert_istrue(abs(lu_eval('Linear point-slope', 'Clip', -1) - 0.0) < 1e-12);
assert_istrue(abs(lu_eval('Linear point-slope', 'Linear', -1) - (-1.0)) < 1e-12);
% Extrapolation above the range.
assert_istrue(abs(lu_eval('Linear point-slope', 'Clip', 5) - 16.0) < 1e-12);
assert_istrue(abs(lu_eval('Linear point-slope', 'Linear', 5) - 23.0) < 1e-12);
%=============================================================================
