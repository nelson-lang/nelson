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
diagram_names = {'test_abs', 'test_gain', 'test_max', 'test_min', 'test_mult', ...
  'test_sum', 'test_mathFunction', 'test_roundingFunction', 'test_sign', ...
  'test_sqrt', 'test_trigFunction', ...
  ... % Dynamic-port variants (two-input / two-output) and vector reductions
  ... % (decomposed into scalar sum / mult blocks by the vector-expansion pass).
  'test_trigFunction_atan2', 'test_trigFunction_sincos', ...
  'test_mathFunction_pow', 'test_mathFunction_mod', ...
  'test_sumElements', 'test_productOfElements', 'test_dotProduct', 'test_crossProduct', ...
  'test_lookup1D', 'test_lookup2D', 'test_lookupND', 'test_directLookup'};
nflow_codegen_check_golden('math', diagram_names);
%=============================================================================
