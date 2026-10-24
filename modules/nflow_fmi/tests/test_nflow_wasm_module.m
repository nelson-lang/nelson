%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
% The browser harness registers a real component before running this test.
if ~iswasm()
  return;
end
fmuPath = getenv('NELSON_WASM_FMU_PATH');
if isempty(fmuPath)
  return;
end
assert_istrue(ismodule('nflow_fmi'));
assert_istrue(__nflow_fmi_available__());
assert_istrue(isdir(modulepath('nflow_fmi', 'functions')));
info = fmiInfo(fmuPath);
assert_isequal(info.fmiVersion, '3.0');
assert_istrue(info.coSimulation);
assert_isequal(info.names, {'input_a'; 'input_b'; 'sum'});
result = fmiCoSimulate(fmuPath, 0.5, 0.1, struct('input_a', 2, 'input_b', 3));
assert_isequal(result.outputNames, {'sum'});
assert_istrue(all(abs(result.outputs - 5) < 1e-12));
assert_isapprox(result.time(end), 0.5, 1e-12);

mePath = getenv('NELSON_WASM_FMU_ME_PATH');
if ~isempty(mePath)
  info = fmiInfo(mePath);
  assert_istrue(info.modelExchange);
  result = fmiModelExchange(mePath, 1, 0.01);
  assert_isapprox(result.outputs(end), exp(-1), 1e-8);
  result = fmiCoSimulate(mePath, 1, 0.1);
  assert_isapprox(result.outputs(end), 0.9 ^ 10, 1e-12);
end
