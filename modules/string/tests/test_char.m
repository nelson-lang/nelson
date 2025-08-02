%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('char'), -1);
assert_isequal(nargout('char'), 1);
%=============================================================================
R = char({'','w'});
REF = [' ';'w'];
assert_isequal(R, REF);
%=============================================================================
R = char({'','with','','gaps'});
REF =['    ';'with';'    ';'gaps'];
assert_isequal(R, REF);
%=============================================================================
REF = [65, -1, 65];
C = char(REF);
assert_isequal(double(C), [65, 0, 65]);
%=============================================================================
A = ['r  f';'d  l'];
assert_isequal(char(A), A);
%=============================================================================
B = ['r  f', 'd  l'];
assert_isequal(char(B), 'r  fd  l');
%=============================================================================
assert_isequal(char({'r', 'a'}), ['r';'a'])
assert_isequal(char({'r';'a'}), ['r';'a'])
%=============================================================================
assert_isequal(char({'r a', 'b c'}), ['r a'; 'b c']);
assert_isequal(char({'r a'; 'b c'}), ['r a'; 'b c']);
%=============================================================================
assert_isequal(double(char(-1)), double(intmin('uint16')));
assert_isequal(double(char(NaN)), double(intmin('uint16')));
assert_isequal(double(char(Inf)), double(intmax('uint16')));
assert_isequal(double(char(-Inf)), double(intmin('uint16')));
assert_isequal(double(char(single(-1))), double(intmin('uint16')));
assert_isequal(double(char(single(NaN))), double(intmin('uint16')));
assert_isequal(double(char(single(Inf))), double(intmax('uint16')));
assert_isequal(double(char(single(-Inf))), double(intmin('uint16')));
assert_isequal(char(65536), char(65535));
assert_isequal(char('one string'), 'one string');
assert_isequal(double(char([1,0,3])), [1,0,3]);
% better choice compared to others
R = char([], '');
assert_isequal(size(R), [0 0])
assert_isequal(class(R), 'char')
%=============================================================================
% better choice compared to others
R = char(zeros(0, 4), '');
assert_isequal(size(R), [0 0])
assert_isequal(class(R), 'char')
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
assert_isequal(R, REF);
%=============================================================================
R = char(zeros(0, 4));
assert_isequal(size(R), [0 4])
assert_isequal(class(R), 'char')
%=============================================================================
R = char(zeros(3, 0, 4));
assert_isequal(size(R), [3, 0, 4])
assert_isequal(class(R), 'char')
%=============================================================================
A = ['A';'B';'C'];
B = ['A','B','C'];
R = char(A, B);
REF = ['A  '; 'B  '; 'C  '; 'ABC'];
assert_isequal(R, REF);
%=============================================================================
A = ['A';'B';'C'];
B = ['A','B','C'];
R = char({A, B});
REF = ['A  '; 'B  '; 'C  '; 'ABC'];
assert_isequal(R, REF);
%=============================================================================
R1 = char([65; 66; 67]);
R2 = char([65, 66, 67]);
R = char(R1, R2);
REF = ['A  '; 'B  '; 'C  '; 'ABC'];
assert_isequal(R, REF);
%=============================================================================
A1 = [65 66; 67 68];
A2 = 'abcd';
R = char(A1, A2);
REF = ['AB  '; 'CD  '; 'abcd'];
assert_isequal(R, REF);
%=============================================================================
A = char([65; 66; 67]);
B = char([65, 66, 67]);
R1 = char({A, B});
R2 = char(A, B);
assert_isequal(R1, R2);
%=============================================================================
R = char('these','are','test','strings');
REF = ['these  ';
'are    ';
'test   ';
'strings'];
assert_isequal(R, REF);
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
assert_isequal(R, REF);
%=============================================================================
R = char([65, -1, 65]);
REF = [65, 0, 65];
assert_isequal(double(R), REF);
%=============================================================================
R = char('Hello', 'Goodbye', 'Yes', 'No');
REF = ['Hello  ';
'Goodbye';
'Yes    ';
'No     '];
assert_isequal(R, REF);
%=============================================================================
R = char(["A";"V  E"]);
REF = ['A   '; 'V  E'];
assert_isequal(R, REF);
%=============================================================================
cmd = 'R = char(["A", string(NaN)]);';
assert_checkerror(cmd, _('Conversion <missing> to character vector is not supported.'));
%=============================================================================
assert_checkerror('char()', _('Wrong number of input arguments.'));
%=============================================================================
msg = sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'char');
assert_checkerror('char(true)', msg);
%=============================================================================
assert_checkerror('char([65i, 65])', _('Conversion to char from complex is not possible.'));
%=============================================================================
msg = sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'char');
assert_checkerror('R = char([65, 66, 67], i)', msg);
%=============================================================================
msg = sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'char');
assert_checkerror('char({''test'', [115, 101, 116]})', msg);
%=============================================================================
msg = sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'char');
assert_checkerror('char({''of'', ''strings''; ''in'', 97})',  msg);
assert_checkerror('char({{''test''}})', msg);
assert_checkerror('char({{''test'', {}, ''string''}})', msg);
assert_checkerror('char({''test'', {}, ''string''})', msg);
%=============================================================================
assert_checkerror('char({''test'', 256, ''test2''})', msg);
assert_checkerror('char({''test'', [115, 101.1, 115.5], {''of'', ''in''; ''strings'', 96.9}, [99, 101; 108, 108], 97}, 114, [114; 97], ''y'')', msg);
%=============================================================================
