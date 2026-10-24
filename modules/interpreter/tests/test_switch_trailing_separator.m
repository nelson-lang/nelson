%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
% Regression: a ',' after the switch expression followed by a newline must be
% accepted. Unlike ';', a ',' is not collapsed with its
% newline in the lexer, so the parser must absorb the trailing newline itself.
%=============================================================================
NL = char(10);
%=============================================================================
% The forms that used to fail must now parse as a script.
asserts.isequal(parsestring(['switch 2,' NL 'case 2, disp(2); end']), 'script');
asserts.isequal(parsestring(['switch 2 ,' NL 'case 2, disp(2); end']), 'script');
asserts.isequal(parsestring(['switch 2,;' NL 'case 2, disp(2); end']), 'script');
asserts.isequal(parsestring(['switch 2,' NL NL NL 'case 2, disp(2); end']), 'script');
asserts.isequal(parsestring(['switch 2,' NL 'end']), 'script');
%=============================================================================
% The already-working separator forms must keep parsing.
asserts.isequal(parsestring(['switch 2;' NL 'case 2, disp(2); end']), 'script');
asserts.isequal(parsestring(['switch 2' NL 'case 2, disp(2); end']), 'script');
asserts.isequal(parsestring('switch 2, case 1, disp(1); case 2, disp(2); end'), 'script');
%=============================================================================
% Executed forms must select the correct branch.
x = 2;
switch x,
  case 1
    r = 'one';
  case 2
    r = 'two';
  otherwise
    r = 'other';
end
asserts.isequal(r, 'two');
%=============================================================================
x = 9;
switch x,

  case 1
    r = 'one';
  otherwise
    r = 'other';
end
asserts.isequal(r, 'other');
%=============================================================================
% Empty switch body reached through a trailing comma + newline.
value = 0;
switch 42,
end
value = value + 1;
asserts.isequal(value, 1);
%=============================================================================
