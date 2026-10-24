%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('aioptions'), -1);
asserts.isequal(nargout('aioptions'), 1);
%=============================================================================
opts = aioptions();
asserts.istrue(isstruct(opts));
asserts.isequal(opts.Provider, 'ollama');
asserts.isequal(opts.Model, 'llama3');
asserts.isequal(opts.Timeout, 120);
asserts.isequal(opts.SystemPrompt, '');
asserts.istrue(isempty(opts.Think));
asserts.istrue(isempty(opts.NumPredict));
asserts.istrue(isempty(opts.Temperature));
%=============================================================================
opts = aioptions('Provider', 'openai-compatible', 'Model', 'test-model', 'Timeout', 10);
asserts.isequal(opts.Provider, 'openai-compatible');
asserts.isequal(opts.Model, 'test-model');
asserts.isequal(opts.Timeout, 10);
%=============================================================================
opts = aioptions('Provider', 'ollama', 'Think', false, 'NumPredict', 160, 'Temperature', 0);
asserts.isequal(opts.Provider, 'ollama');
asserts.isequal(opts.Think, false);
asserts.isequal(opts.NumPredict, 160);
asserts.isequal(opts.Temperature, 0);
%=============================================================================
asserts.checkerror('aioptions(''Timeout'', -1)', _('Timeout must be a positive numeric scalar.'));
asserts.checkerror('aioptions(''Think'', ''false'')', _('Think must be a logical scalar.'));
asserts.checkerror('aioptions(''NumPredict'', 0)', _('NumPredict must be a positive numeric scalar.'));
asserts.checkerror('aioptions(''Temperature'', -1)', _('Temperature must be a nonnegative numeric scalar.'));
asserts.checkerror('aioptions(''Unknown'', 1)', _('Unsupported option name.'));
%=============================================================================
% odd number of arguments is rejected before any option is parsed.
asserts.checkerror('aioptions(''Provider'')', _('Name-value arguments expected.'));
%=============================================================================
% option name must be text.
asserts.checkerror('aioptions(1, ''x'')', _('Option name must be a character vector or string scalar.'));
%=============================================================================
% option values that must be text reject non-text input.
asserts.checkerror('aioptions(''Provider'', 5)', _('Provider must be a character vector or string scalar.'));
%=============================================================================
% option names are case-insensitive.
opts = aioptions('PROVIDER', 'ollama', 'MODEL', 'x');
asserts.isequal(opts.Provider, 'ollama');
asserts.isequal(opts.Model, 'x');
%=============================================================================
% string inputs are accepted and stored as character vectors.
opts = aioptions("Endpoint", "http://host", "SystemPrompt", "sp");
asserts.isequal(opts.Endpoint, 'http://host');
asserts.isequal(class(opts.Endpoint), 'char');
asserts.isequal(opts.SystemPrompt, 'sp');
%=============================================================================
% Think accepts a numeric scalar and stores it as logical.
opts = aioptions('Think', 1);
asserts.isequal(opts.Think, true);
asserts.isequal(class(opts.Think), 'logical');
%=============================================================================
% TokenEnvVar can be set and is returned unchanged.
opts = aioptions('TokenEnvVar', 'MY_TOKEN');
asserts.isequal(opts.TokenEnvVar, 'MY_TOKEN');
%=============================================================================
