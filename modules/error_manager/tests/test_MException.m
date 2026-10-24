%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addpath([nelsonroot(), '/modules/error_manager/tests']);
ME = fun_MException_1();
asserts.isequal(class(ME), 'MException')
asserts.istrue(isobject(ME))
asserts.isequal(fieldnames(ME), {'identifier'; 'message'; 'cause'; 'stack'; 'Correction'})
asserts.isequal(properties(ME), {'identifier'; 'message'; 'cause'; 'stack'; 'Correction'})
method_names = methods(ME);
asserts.istrue(any(strcmp(method_names, 'addCause')))
asserts.istrue(any(strcmp(method_names, 'addCorrection')))
asserts.istrue(any(strcmp(method_names, 'getReport')))
asserts.istrue(any(strcmp(method_names, 'last')))
asserts.istrue(any(strcmp(method_names, 'throw')))
asserts.isequal(ME.message, 'Input must be char.');
asserts.isequal(ME.identifier, 'sayHello:inputError');
asserts.isequal(ME.cause, {})
asserts.isequal(size(ME.stack), [0 1])
asserts.isequal(class(ME.stack), 'struct')
asserts.isequal(fieldnames(ME.stack), {'file'; 'name'; 'line'})
asserts.isequal(ME.Correction, [])
%=============================================================================
ME = MException('Nelson:test:format', 'Hello %s %d', 'world', 7);
asserts.isequal(ME.message, 'Hello world 7')
asserts.isequal(ME.identifier, 'Nelson:test:format')
asserts.checkerror('MException(''Nelson:test:bad-id'', ''msg'')', _('First input argument must be a valid message identifier.'))
%=============================================================================
cause = MException('Nelson:test:cause', 'Cause message.');
ME2 = addCause(ME, cause);
asserts.isequal(size(ME2.cause), [1 1])
asserts.isequal(class(ME2.cause{1}), 'MException')
asserts.isequal(ME2.cause{1}.message, 'Cause message.')
ME3 = ME.addCause(cause);
asserts.isequal(size(ME3.cause), [1 1])
asserts.isequal(class(ME3.cause{1}), 'MException')
%=============================================================================
correction = fun_MException_correction();
ME4 = addCorrection(ME, correction);
asserts.isequal(class(ME4.Correction), 'nelson.lang.correction.AppendArgumentsCorrection')
asserts.isequal(ME4.Correction.Arguments, 'arg')
caught = false;
try
  throw(ME4)
catch caughtME
  caught = true;
end
asserts.istrue(caught)
asserts.isequal(class(caughtME.Correction), 'nelson.lang.correction.AppendArgumentsCorrection')
asserts.isequal(caughtME.Correction.Arguments, 'arg')
%=============================================================================
caught = false;
try
  error(correction, 'Nelson:test:correction', 'Correction message.')
catch corrected
  caught = true;
end
asserts.istrue(caught)
asserts.isequal(corrected.identifier, 'Nelson:test:correction')
asserts.isequal(corrected.message, 'Correction message.')
asserts.isequal(class(corrected.Correction), 'nelson.lang.correction.AppendArgumentsCorrection')
asserts.isequal(corrected.Correction.Arguments, 'arg')
%=============================================================================
asserts.istrue(ME == ME)
asserts.isfalse(ME == ME2)
asserts.isfalse(ME ~= ME)
asserts.istrue(ME ~= ME2)
asserts.istrue(isequal(ME, ME))
asserts.isfalse(isequal(ME, ME2))
asserts.isequal(getReport(ME, 'basic'), 'Hello world 7')
asserts.isequal(ME.getReport('basic'), 'Hello world 7')
%=============================================================================
try
  error('Nelson:test:last', 'Last message.')
catch caught
  asserts.isequal(class(caught), 'MException')
  asserts.isequal(caught.identifier, 'Nelson:test:last')
  asserts.isequal(caught.message, 'Last message.')
end
MException.last('reset');
lastException = MException.last;
asserts.isequal(class(lastException), 'MException')
asserts.isequal(lastException.identifier, '')
asserts.isequal(lastException.message, '')
%=============================================================================
asserts.checkerror('ME.message = ''new message'';', _('Cannot set read-only property of MException.'))
%=============================================================================
