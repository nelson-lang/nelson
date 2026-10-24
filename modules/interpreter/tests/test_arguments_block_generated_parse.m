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
% A malformed arguments entry (default value, validator, output validator)
% lowers to validation code that does not parse. The function must then raise
% an error naming it at call time, never run without its validation.
%=============================================================================
addpath([modulepath('interpreter', 'tests'), '/arguments']);
%=============================================================================
id = 'Nelson:interpreter:invalidArgumentsBlock';
msg = sprintf(_('Invalid arguments block in function ''%s''.'), 'arguments_invalid_default_syntax');
asserts.checkerror('r = arguments_invalid_default_syntax([1 2 3]);', msg, id);
asserts.checkerror('r = arguments_invalid_default_syntax(1);', msg, id);
asserts.checkerror('r = arguments_invalid_default_syntax();', msg, id);
%=============================================================================
msg = sprintf(_('Invalid arguments block in function ''%s''.'), 'arguments_invalid_validator_syntax');
asserts.checkerror('r = arguments_invalid_validator_syntax(-1);', msg, id);
asserts.checkerror('r = arguments_invalid_validator_syntax(2);', msg, id);
%=============================================================================
% the output validation failure is reported before the body runs
msg = sprintf(_('Invalid arguments block in function ''%s''.'), 'arguments_invalid_output_syntax');
asserts.checkerror('r = arguments_invalid_output_syntax(3);', msg, id);
%=============================================================================
