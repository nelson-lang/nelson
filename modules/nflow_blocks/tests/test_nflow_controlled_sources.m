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
% Current-controlled sources CCV / CCC (Coselica MEAB_CCV / MEAB_CCC): the
% controlling current i_cp flows through the shorted sense branch (cp-cn), and
% the output is v_pn = gain i_cp (CCV) or i_pn = gain i_cp (CCC). Native
% physicalIsland kinds ccvs / cccs (two / one branch-current state).
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
function d = ccv_circuit(srcType, gain, rload)
  d = ['{"version":"1.0.0","format":"nflow-2","name":"CC","blocks":[', ...
    '{"id":"V1","type":"ConstantVoltage","params":{"V":10}},', ...
    '{"id":"R1","type":"Resistor","params":{"R":2}},', ...
    '{"id":"CS","type":"', srcType, '","params":{"gain":', num2str(gain), '}},', ...
    '{"id":"R3","type":"Resistor","params":{"R":', num2str(rload), '}},', ...
    '{"id":"G","type":"Ground","params":{}},', ...
    '{"id":"VS","type":"VoltageSensor","params":{}},', ...
    '{"id":"sc","type":"scope","inputs":1,"outputs":0,"params":{}}],', ...
    '"connections":[{"from":"VS","to":"sc","fromIndex":0,"toIndex":0}],', ...
    '"physicalWires":[{"pins":["V1.p","R1.p"]},{"pins":["R1.n","CS.cp"]},', ...
    '{"pins":["CS.p","R3.p","VS.p"]},', ...
    '{"pins":["V1.n","CS.cn","CS.n","R3.n","VS.n","G.p"]}],', ...
    '"solver":"dae","runtime":0.1,"sampleTime":0.02}'];
end
%=============================================================================
% CCV: the sense current is i = 10 / 2 = 5 A; v_out = gain i = 1 * 5 = 5 V.
r = jsondecode(__nflow_simulate__(NFlow.internal.acausalExpand(ccv_circuit('CCV', 1, 100))));
assert_isequal(r.status, 'done');
assert_isapprox(r.scopes(1).series(end), 5.0, 1e-3);
%=============================================================================
% CCC: output current gain i into R3; |v_out| = gain i R3 = 2 * 5 * 10 = 100
% (sign follows the internal current-source convention, like ISource).
r = jsondecode(__nflow_simulate__(NFlow.internal.acausalExpand(ccv_circuit('CCC', 2, 10))));
assert_isequal(r.status, 'done');
assert_isapprox(abs(r.scopes(1).series(end)), 100.0, 1e-2);
%=============================================================================
