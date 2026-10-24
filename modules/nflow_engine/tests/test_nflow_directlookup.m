%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% directLookup: Direct Lookup Table (Element mode). 2x3 column-major table
% T(i,j)=i+10j selected by two zero-based integer index inputs. Feedthrough;
% logged at t0 (an integrator supplies the state the fixed-step solver needs).
%=============================================================================
function p = dl_write(json, stem)
  p = [tempname(), '.nflow'];
  fid = fopen(p, 'w'); fwrite(fid, json); fclose(fid);
end
%=============================================================================
function y = dl_eval(i1, i2)
  m = ['{"blocks":[', ...
    '{"id":"a","type":"constant","inputs":0,"outputs":1,"params":{"Value":', sprintf('%.15g', i1), '}},', ...
    '{"id":"b","type":"constant","inputs":0,"outputs":1,"params":{"Value":', sprintf('%.15g', i2), '}},', ...
    '{"id":"f","type":"directLookup","inputs":2,"outputs":1,"params":{', ...
      '"NumberOfTableDimensions":2,"TableDimensions":[2,3],"Table":[0,1,10,11,20,21]}},', ...
    '{"id":"ig","type":"integrator","inputs":1,"outputs":1,"params":{"InitialCondition":0}},', ...
    '{"id":"td","type":"toWorkspace","inputs":1,"outputs":0,"params":{"VariableName":"d"}},', ...
    '{"id":"tw","type":"toWorkspace","inputs":1,"outputs":0,"params":{"VariableName":"y"}}],', ...
    '"connections":[', ...
      '{"from":"a","to":"f","fromIndex":0,"toIndex":0},', ...
      '{"from":"b","to":"f","fromIndex":0,"toIndex":1},', ...
      '{"from":"f","to":"tw","fromIndex":0,"toIndex":0},', ...
      '{"from":"a","to":"ig","fromIndex":0,"toIndex":0},', ...
      '{"from":"ig","to":"td","fromIndex":0,"toIndex":0}],', ...
    '"solver":"ode1","runtime":0.1,"sampleTime":0.1}'];
  out = sim(dl_write(m, 'test_nflow_directlookup_eval'));
  lg = out.logsout; nms = {lg.name};
  y = lg(find(strcmp(nms, 'y'), 1)).values(1);
end
%=============================================================================
assert_istrue(abs(dl_eval(0, 0) - 0.0) < 1e-12);
assert_istrue(abs(dl_eval(1, 0) - 1.0) < 1e-12);
assert_istrue(abs(dl_eval(0, 2) - 20.0) < 1e-12);
assert_istrue(abs(dl_eval(1, 2) - 21.0) < 1e-12);
% Rounding and clamping.
assert_istrue(abs(dl_eval(0.6, 1.4) - 11.0) < 1e-12);
assert_istrue(abs(dl_eval(9, 9) - 21.0) < 1e-12);
%=============================================================================
