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
d = tempname();
mkdir(d);
c = onCleanup(@() rmdir(d, 's')); %#ok<NLS0004>
dlgenerategateway(d, 'test_gw', {{'cpp_sum', 1, 2, 'CPP_BUILTIN'}, {'cpp_sub', 3, 4}, {'cpp_sub2', 3, 4, ...
  'CPP_BUILTIN_WITH_EVALUATOR'}, {'cpp_sub3', 1, 1, 'CPP_BUILTIN', 'NLS_OVERLOAD_AUTO_OFF'}});
asserts.istrue(isfile(fullfile(d, 'Gateway.cpp')));
txt = fileread(fullfile(d, 'Gateway.cpp'));
asserts.istrue(contains(txt, '{ "cpp_sum", (ptrBuiltin)cpp_sumBuiltin, 1, 2, CPP_BUILTIN },'));
asserts.istrue(contains(txt, '{ "cpp_sub", (ptrBuiltin)cpp_subBuiltin, 3, 4, CPP_BUILTIN },'));
asserts.istrue(contains(txt, '{ "cpp_sub2", (ptrBuiltin)cpp_sub2Builtin, 3, 4, CPP_BUILTIN_WITH_EVALUATOR },'));
asserts.istrue(contains(txt, '{ "cpp_sub3", (ptrBuiltin)cpp_sub3Builtin, 1, 1, CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },'));
asserts.istrue(contains(txt, 'NLS_GATEWAY_REMOVE(gateway)'));
asserts.isfalse(contains(txt, 'NLSGATEWAYINFO'));
asserts.isfalse(contains(txt, 'NLSGATEWAYNAME'));
%=============================================================================
% builtins declaring how they use the caller workspace
dlgenerategateway(d, 'test_gw', {{'cpp_pure', 1, 2, 'CPP_BUILTIN_WITH_EVALUATOR_NO_CALLER_WORKSPACE'}, ...
  {'cpp_eval', 0, 1, 'CPP_BUILTIN_WITH_EVALUATOR_CALLER_WORKSPACE_ACCESS', 'NLS_OVERLOAD_AUTO_OFF'}});
txt = fileread(fullfile(d, 'Gateway.cpp'));
asserts.istrue(contains(txt, ['{ "cpp_pure", (ptrBuiltin)cpp_pureBuiltin, 1, 2, ', ...
  'CPP_BUILTIN_WITH_EVALUATOR_NO_CALLER_WORKSPACE },']));
asserts.istrue(contains(txt, ['{ "cpp_eval", (ptrBuiltin)cpp_evalBuiltin, 0, 1, ', ...
  'CPP_BUILTIN_WITH_EVALUATOR_CALLER_WORKSPACE_ACCESS, NLS_OVERLOAD_AUTO_OFF },']));
asserts.istrue(contains(txt, ['extern ArrayOfVector cpp_pureBuiltin(Evaluator* eval, int nLhs, ', ...
  'const ArrayOfVector& argIn);']));
asserts.istrue(contains(txt, ['extern ArrayOfVector cpp_evalBuiltin(Evaluator* eval, int nLhs, ', ...
  'const ArrayOfVector& argIn);']));
msg = '';
try
  dlgenerategateway(d, 'test_gw', {{'cpp_bad', 1, 1, 'CPP_BUILTIN_UNKNOWN'}});
catch e
  msg = e.message;
end
asserts.isequal(msg, 'Invalid builtin list.');
%=============================================================================
%=============================================================================
