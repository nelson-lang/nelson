%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
addpath([fileparts(nfilename('fullpathext'), 'path'), '/private']);
%=============================================================================
strictName = 'NELSON_IMAGE_PROCESSING_BENCH_STRICT';
previousStrict = getenv(strictName);
cleanup_env = onCleanup(@() setenv(strictName, previousStrict));
%=============================================================================
setenv(strictName);
ip_bench_report('bench_report_no_threshold_unit', 2, []);
ip_bench_report('bench_report_invalid_threshold_unit', 2, 0);
ip_bench_report('bench_report_under_threshold_unit', 0.25, 1);
%=============================================================================
setenv(strictName, 'FALSE');
ip_bench_report('bench_report_non_strict_unit', 2, 1);
%=============================================================================
setenv(strictName, 'TRUE');
didError = false;
message = '';
try
  ip_bench_report('bench_report_strict_unit', 2, 1);
catch ex
  didError = true;
  message = ex.message;
end
asserts.istrue(didError);
asserts.isequal(message, 'bench_report_strict_unit exceeded 1 s');
%=============================================================================
