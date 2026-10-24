%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% Characterization net for the timing of arguments-block structural errors.
% Today the arguments block is desugared by a source-to-source pass that runs
% before the lexer and injects validation code into the function body, so every
% structural violation is reported at CALL time (runtime), never at load/parse
% time. This test locks that invariant: loading a function that carries a
% malformed arguments block must not throw; only calling it throws, with a
% stable message. A future native-grammar front-end must preserve this timing.
%=============================================================================
addpath([nelsonroot(), '/modules/interpreter/tests/arguments']);
%=============================================================================
% Each row: {function name, call expression, expected message fragment}.
cases = {...
  'arguments_edge_misc_nargin_default', ...
    'arguments_edge_misc_nargin_default()', ...
    _('Function nargin is not allowed in arguments block default values.'); ...
  'arguments_edge_misc_output_before_input', ...
    'arguments_edge_misc_output_before_input(1)', ...
    _('arguments (Input) block must appear before arguments (Output) block.'); ...
  'arguments_edge_misc_nv_in_repeating', ...
    'arguments_edge_misc_nv_in_repeating(1, 2)', ...
    _('Name-value arguments are not allowed in arguments (Repeating) blocks.'); ...
  'arguments_edge_misc_repeating_default', ...
    'arguments_edge_misc_repeating_default(1)', ...
    _('Repeating arguments cannot specify default values.') ...
};
%=============================================================================
for k = 1:size(cases, 1)
  name = cases{k, 1};
  callExpr = cases{k, 2};
  expectedMessage = cases{k, 3};
  % Loading must not throw: the function is visible and a handle can be built.
  asserts.isequal(exist(name), 2);
  handle = str2func(name);
  asserts.isequal(class(handle), 'function_handle');
  % Calling must throw the pinned message.
  failed = false;
  message = '';
  try
    eval(callExpr);
  catch me
    failed = true;
    message = me.message;
  end
  asserts.istrue(failed);
  asserts.istrue(contains(message, expectedMessage));
end
%=============================================================================
rmpath([nelsonroot(), '/modules/interpreter/tests/arguments']);
%=============================================================================
