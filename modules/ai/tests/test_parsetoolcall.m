%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargout('nelson.ai.parseToolCall'), 3);
%=============================================================================
% A well-formed tool call is recognized, with name and arguments.
[isTool, name, args] = nelson.ai.parseToolCall('{"tool": "evaluate", "arguments": {"code": "2+2"}}');
asserts.istrue(isTool);
asserts.isequal(name, 'evaluate');
asserts.isequal(args.code, '2+2');
%=============================================================================
% A tool call inside a ```json fenced block is extracted.
reply = sprintf('Sure, here it is:\n```json\n{"tool": "plot", "arguments": {"command": "plot(1:10)"}}\n```');
[isTool, name, args] = nelson.ai.parseToolCall(reply);
asserts.istrue(isTool);
asserts.isequal(name, 'plot');
asserts.isequal(args.command, 'plot(1:10)');
%=============================================================================
% Plain prose with no JSON object is not a tool call.
[isTool, name, args] = nelson.ai.parseToolCall('This is just a sine wave explanation.');
asserts.istrue(~isTool);
asserts.isequal(name, '');
%=============================================================================
% A tool call without an arguments object is still recognized.
[isTool, name, args] = nelson.ai.parseToolCall('{"tool": "get_nelson_workspace"}');
asserts.istrue(isTool);
asserts.isequal(name, 'get_nelson_workspace');
asserts.istrue(isstruct(args));
%=============================================================================
% Invalid JSON (a raw newline inside the code string) is salvaged: the name and
% the code argument are still recovered.
reply = sprintf('{"tool": "evaluate", "arguments": {"code": "x = 1\n y = 2"}}');
[isTool, name, args] = nelson.ai.parseToolCall(reply);
asserts.istrue(isTool);
asserts.isequal(name, 'evaluate');
asserts.istrue(contains(args.code, 'x = 1'));
asserts.istrue(contains(args.code, 'y = 2'));
%=============================================================================
% Empty and non-text inputs are handled without error.
[isTool, name, args] = nelson.ai.parseToolCall('');
asserts.istrue(~isTool);
[isTool, name, args] = nelson.ai.parseToolCall(42);
asserts.istrue(~isTool);
%=============================================================================
