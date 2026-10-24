%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
caught = false;
try
  error('Nelson:test:errorDirect', 'Direct error message.');
catch exception
  caught = true;
end
%=============================================================================
asserts.istrue(caught);
asserts.isequal(class(exception), 'MException');
asserts.isequal(exception.identifier, 'Nelson:test:errorDirect');
asserts.isequal(exception.message, 'Direct error message.');
%=============================================================================
caught = false;
try
  error('Line1\nLine2');
catch exception
  caught = true;
end
asserts.istrue(caught);
asserts.isequal(exception.identifier, '');
asserts.isequal(exception.message, 'Line1\nLine2');
%=============================================================================
caught = false;
try
  error('Value %d', 7);
catch exception
  caught = true;
end
asserts.istrue(caught);
asserts.isequal(exception.identifier, '');
asserts.isequal(exception.message, 'Value 7');
%=============================================================================
caught = false;
try
  error('Nelson:test:fmt', 'Value %d', 7);
catch exception
  caught = true;
end
asserts.istrue(caught);
asserts.isequal(exception.identifier, 'Nelson:test:fmt');
asserts.isequal(exception.message, 'Value 7');
%=============================================================================
S = struct('message', 'Only message.');
caught = false;
try
  error(S);
catch exception
  caught = true;
end
asserts.istrue(caught);
asserts.isequal(exception.identifier, '');
asserts.isequal(exception.message, 'Only message.');
asserts.istrue(length(exception.stack) >= 1);
%=============================================================================
S = struct('identifier', 'Nelson:test:idOnly');
caught = false;
try
  error(S);
catch exception
  caught = true;
end
asserts.istrue(caught);
asserts.isequal(exception.identifier, 'Nelson:test:idOnly');
asserts.isequal(exception.message, '');
%=============================================================================
S = struct('message', 'Message and id.', 'identifier', 'Nelson:test:structId');
caught = false;
try
  error(S);
catch exception
  caught = true;
end
asserts.istrue(caught);
asserts.isequal(exception.identifier, 'Nelson:test:structId');
asserts.isequal(exception.message, 'Message and id.');
%=============================================================================
stackFrame = struct('file', 'file.nls', 'name', 'functionName', 'line', 12);
S = struct('message', 'Stack message.', 'stack', stackFrame);
caught = false;
try
  error(S);
catch exception
  caught = true;
end
asserts.istrue(caught);
asserts.isequal(exception.message, 'Stack message.');
asserts.isequal(length(exception.stack), 1);
asserts.isequal(exception.stack(1).file, 'file.nls');
asserts.isequal(exception.stack(1).name, 'functionName');
asserts.isequal(exception.stack(1).line, 12);
%=============================================================================
stackFrame = struct('file', 'file.nls', 'name', 'functionName', 'line', 12, 'extra', 123);
S = struct('message', 'Stack extra field.', 'stack', stackFrame);
caught = false;
try
  error(S);
catch exception
  caught = true;
end
asserts.istrue(caught);
asserts.isequal(exception.message, 'Stack extra field.');
asserts.isequal(fieldnames(exception.stack), {'file'; 'name'; 'line'});
asserts.isequal(exception.stack(1).line, 12);
%=============================================================================
stackFrame = struct('file', 'file.nls', 'name', 'functionName', 'line', 12.75);
S = struct('message', 'Stack noninteger line.', 'stack', stackFrame);
caught = false;
try
  error(S);
catch exception
  caught = true;
end
asserts.istrue(caught);
asserts.isequal(exception.stack(1).line, 12);
%=============================================================================
stackFrame = struct('file', 'file.nls', 'name', 'functionName', 'line', NaN);
S = struct('message', 'Stack invalid line.', 'stack', stackFrame);
caught = false;
try
  error(S);
catch exception
  caught = true;
end
asserts.istrue(caught);
asserts.isequal(exception.stack(1).line, 0);
%=============================================================================
stackFrame = struct('file', 'file.nls', 'name', 'functionName', 'line', Inf);
S = struct('message', 'Stack infinite line.', 'stack', stackFrame);
caught = false;
try
  error(S);
catch exception
  caught = true;
end
asserts.istrue(caught);
asserts.isequal(exception.stack(1).line, 0);
%=============================================================================
S = struct('message', 'Extra field.', 'extra', 123);
caught = false;
try
  error(S);
catch exception
  caught = true;
end
asserts.istrue(caught);
asserts.isequal(exception.message, 'Extra field.');
%=============================================================================
S(1).message = 'one';
S(2).message = 'two';
asserts.checkerror('error(S);', _('Input structure must be scalar.'));
%=============================================================================
msg = _('The message must be specified as either a character vector, string scalar, or a message structure.');
asserts.checkerror('error(1);', msg);
%=============================================================================
error('');
%=============================================================================
