%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
tests_path = modulepath('interpreter', 'tests');
addpath(tests_path, '-begin');
%=============================================================================
direct_report = '';
try
  direct_script_call_stack_case;
catch direct_exception
  direct_report = getLastReport();
end
asserts.istrue(contains(direct_report, 'Error in sprintf'));
asserts.istrue(contains(direct_report, 'at line    12 of'));
asserts.istrue(contains(direct_report, 'direct_script_call_stack_case.m'));
asserts.isequal(fieldnames(direct_exception.stack), {'file'; 'name'; 'line'});
asserts.isequal(direct_exception.stack(1).name, 'sprintf');
asserts.isequal(direct_exception.stack(2).name, 'direct_script_call_stack_case');
asserts.isequal(direct_exception.stack(2).line, 12);
%=============================================================================
run_report = '';
try
  run([tests_path, '/direct_script_call_stack_case.m']);
catch
  run_report = getLastReport();
end
asserts.istrue(contains(run_report, 'Error in sprintf'));
asserts.istrue(contains(run_report, 'at line    12 of'));
asserts.istrue(contains(run_report, 'direct_script_call_stack_case.m'));
%=============================================================================
nested_report = '';
try
  direct_script_nested_call_stack_case;
catch
  nested_report = getLastReport();
end
asserts.istrue(contains(nested_report, 'Error in direct_script_call_stack_helper (line 11)'));
asserts.istrue(contains(nested_report, 'at line    10 of'));
asserts.istrue(contains(nested_report, 'direct_script_nested_call_stack_case.m'));
%=============================================================================
rmpath(tests_path);
