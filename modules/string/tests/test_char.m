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
asserts.isequal(nargin('char'), -1);
asserts.isequal(nargout('char'), 1);
%=============================================================================
R = char({'', 'w'});
REF = [' '; 'w'];
asserts.isequal(R, REF);
%=============================================================================
R = char({'', 'with', '', 'gaps'});
REF = ['    '; 'with'; '    '; 'gaps'];
asserts.isequal(R, REF);
%=============================================================================
REF = [65, -1, 65];
C = char(REF);
asserts.isequal(double(C), [65, 0, 65]);
%=============================================================================
A = ['r  f'; 'd  l'];
asserts.isequal(char(A), A);
%=============================================================================
B = ['r  f', 'd  l'];
asserts.isequal(char(B), 'r  fd  l');
%=============================================================================
asserts.isequal(char({'r', 'a'}), ['r'; 'a'])
asserts.isequal(char({'r'; 'a'}), ['r'; 'a'])
%=============================================================================
asserts.isequal(char({'r a', 'b c'}), ['r a'; 'b c']);
asserts.isequal(char({'r a'; 'b c'}), ['r a'; 'b c']);
%=============================================================================
asserts.isequal(double(char(-1)), double(intmin('uint16')));
asserts.isequal(double(char(NaN)), double(intmin('uint16')));
asserts.isequal(double(char(Inf)), double(intmax('uint16')));
asserts.isequal(double(char(-Inf)), double(intmin('uint16')));
asserts.isequal(double(char(single(-1))), double(intmin('uint16')));
asserts.isequal(double(char(single(NaN))), double(intmin('uint16')));
asserts.isequal(double(char(single(Inf))), double(intmax('uint16')));
asserts.isequal(double(char(single(-Inf))), double(intmin('uint16')));
asserts.isequal(char(65536), char(65535));
asserts.isequal(char('one string'), 'one string');
asserts.isequal(double(char([1, 0, 3])), [1, 0, 3]);
% better choice compared to others
R = char([], '');
asserts.isequal(size(R), [0 0])
asserts.isequal(class(R), 'char')
%=============================================================================
% better choice compared to others
R = char(zeros(0, 4), '');
asserts.isequal(size(R), [0 0])
asserts.isequal(class(R), 'char')
%=============================================================================
% same choice than octave 4.0
R = char('test', '', 'strings', {'', 'with', '', 'gaps'}, {''}, '.');
REF = ['test   ';
  '       ';
  'strings';
  '       ';
  'with   ';
  '       ';
  'gaps   ';
  '       ';
  '.      '];
asserts.isequal(R, REF);
%=============================================================================
R = char(zeros(0, 4));
asserts.isequal(size(R), [0 4])
asserts.isequal(class(R), 'char')
%=============================================================================
R = char(zeros(3, 0, 4));
asserts.isequal(size(R), [3, 0, 4])
asserts.isequal(class(R), 'char')
%=============================================================================
A = ['A'; 'B'; 'C'];
B = ['A', 'B', 'C'];
R = char(A, B);
REF = ['A  '; 'B  '; 'C  '; 'ABC'];
asserts.isequal(R, REF);
%=============================================================================
A = ['A'; 'B'; 'C'];
B = ['A', 'B', 'C'];
R = char({A, B});
REF = ['A  '; 'B  '; 'C  '; 'ABC'];
asserts.isequal(R, REF);
%=============================================================================
R1 = char([65; 66; 67]);
R2 = char([65, 66, 67]);
R = char(R1, R2);
REF = ['A  '; 'B  '; 'C  '; 'ABC'];
asserts.isequal(R, REF);
%=============================================================================
A1 = [65 66; 67 68];
A2 = 'abcd';
R = char(A1, A2);
REF = ['AB  '; 'CD  '; 'abcd'];
asserts.isequal(R, REF);
%=============================================================================
A = char([65; 66; 67]);
B = char([65, 66, 67]);
R1 = char({A, B});
R2 = char(A, B);
asserts.isequal(R1, R2);
%=============================================================================
R = char('these', 'are', 'test', 'strings');
REF = ['these  ';
  'are    ';
  'test   ';
  'strings'];
asserts.isequal(R, REF);
%=============================================================================
R = char([65; 66; 67], [65, 66, 67], 'test', 65, 66, 67);
REF = ['A   ';
  'B   ';
  'C   ';
  'ABC ';
  'test';
  'A   ';
  'B   ';
  'C   '];
asserts.isequal(R, REF);
%=============================================================================
R = char([65, -1, 65]);
REF = [65, 0, 65];
asserts.isequal(double(R), REF);
%=============================================================================
R = char('Hello', 'Goodbye', 'Yes', 'No');
REF = ['Hello  ';
  'Goodbye';
  'Yes    ';
  'No     '];
asserts.isequal(R, REF);
%=============================================================================
R = char(["A"; "V  E"]);
REF = ['A   '; 'V  E'];
asserts.isequal(R, REF);
%=============================================================================
cmd = 'R = char(["A", string(NaN)]);';
asserts.checkerror(cmd, _('Conversion <missing> to character vector is not supported.'));
%=============================================================================
asserts.checkerror('char()', _('Not enough input arguments.'));
%=============================================================================
msg = sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'char');
asserts.checkerror('char(true)', msg);
%=============================================================================
asserts.checkerror('char([65i, 65])', _('Conversion to char from complex is not possible.'));
%=============================================================================
msg = sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'char');
asserts.checkerror('R = char([65, 66, 67], i)', msg);
%=============================================================================
msg = sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'char');
asserts.checkerror('char({''test'', [115, 101, 116]})', msg);
%=============================================================================
msg = sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'char');
asserts.checkerror('char({''of'', ''strings''; ''in'', 97})', msg);
asserts.checkerror('char({{''test''}})', msg);
asserts.checkerror('char({{''test'', {}, ''string''}})', msg);
asserts.checkerror('char({''test'', {}, ''string''})', msg);
%=============================================================================
asserts.checkerror('char({''test'', 256, ''test2''})', msg);
asserts.checkerror('char({''test'', [115, 101.1, 115.5], {''of'', ''in''; ''strings'', 96.9}, [99, 101; 108, 108], 97}, 114, [114; 97], ''y'')', msg);
%=============================================================================
% char of an empty cell array is a 0x0 character array and must not crash
R = char({});
assert_isequal(class(R), 'char');
assert_isequal(size(R), [0 0]);
%=============================================================================
% char of an empty string array does not crash
R = char(strings(0, 0));
assert_isequal(class(R), 'char');
assert_istrue(isempty(R));
%=============================================================================
