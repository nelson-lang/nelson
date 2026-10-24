%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mat2str'), -2);
asserts.isequal(nargout('mat2str'), 1);
%=============================================================================
R = mat2str(int16([3, 4]), 4, 'class');
REF = 'int16([3 4])';
asserts.isequal(R, REF);
%=============================================================================
R = mat2str(int16(eye(3, 4)), 4, 'class');
REF = 'int16([1 0 0 0;0 1 0 0;0 0 1 0])';
asserts.isequal(R, REF);
%=============================================================================
R = mat2str(int16(eye(3, 4)));
REF = '[1 0 0 0;0 1 0 0;0 0 1 0]';
asserts.isequal(R, REF);
%=============================================================================
R = mat2str(int16(eye(0, 0)));
REF = '[]';
asserts.isequal(R, REF);
%=============================================================================
R = mat2str(int16(eye(0, 4)));
REF = 'zeros(0,4)';
asserts.isequal(R, REF);
%=============================================================================
R = mat2str(int16(eye(0, 4)), 'class');
REF = 'int16(zeros(0,4))';
asserts.isequal(R, REF);
%=============================================================================
R = mat2str(eye(3, 4) * pi, 'class');
REF = 'double([3.14159265358979 0 0 0;0 3.14159265358979 0 0;0 0 3.14159265358979 0])';
asserts.isequal(R, REF);
%=============================================================================
R = mat2str(eps);
REF = '2.22044604925031e-16';
asserts.isequal(R, REF);
%=============================================================================
R = mat2str([NaN Inf; 0 -Inf]);
REF = '[NaN Inf;0 -Inf]';
asserts.isequal(R, REF);
%=============================================================================
R = mat2str(eps, 4);
REF = '2.22e-16';
asserts.isequal(R, REF);
%=============================================================================
R = mat2str(complex(NaN, NaN));
REF = 'NaN+1i*NaN';
asserts.isequal(R, REF);
%=============================================================================
R = mat2str(complex(NaN, Inf));
REF = 'NaN+1i*Inf';
asserts.isequal(R, REF);
%=============================================================================
R = mat2str(complex(NaN, -Inf));
REF = 'NaN-1i*Inf';
asserts.isequal(R, REF);
%=============================================================================
R = mat2str(complex(-Inf, -Inf));
REF = '-Inf-1i*Inf';
asserts.isequal(R, REF);
%=============================================================================
R = mat2str(3i);
REF = '0+3i';
asserts.isequal(R, REF);
%=============================================================================
R = mat2str([3.85 2.91; 7.74 8.99]);
REF = '[3.85 2.91;7.74 8.99]';
asserts.isequal(R, REF);
%=============================================================================
R = mat2str(sparse([3.85 2.91; 7.74 8.99]));
REF = 'sparse([1;2;1;2], [1;1;2;2], [3.85;7.74;2.91;8.99], 2, 2)';
asserts.isequal(R, REF);
%=============================================================================
R = mat2str(sparse([true() false(); false() true()]));
REF = 'sparse([1;2], [1;2], [true;true], 2, 2)';
asserts.isequal(R, REF);
%=============================================================================
R = mat2str(sparse([true() false(); false() true()]), 'class');
REF = 'sparse([1;2], [1;2], logical([true;true]), 2, 2)';
asserts.isequal(R, REF);
%=============================================================================
R = mat2str(eye(3, 3) + i);
REF = '[1+1i 0+1i 0+1i;0+1i 1+1i 0+1i;0+1i 0+1i 1+1i]';
asserts.isequal(R, REF);
%=============================================================================
asserts.checkerror('mat2str()', _('Not enough input arguments.'));
asserts.isequal(mat2str('nelson'), '''nelson''');
asserts.checkerror('mat2str(ones(8,9), [1 1])', _('A scalar expected.'));
asserts.checkerror('mat2str(ones(8,9), [1 1])', _('A scalar expected.'), 'Nelson:types:scalarExpected');
asserts.checkerror('mat2str(rand(2, 2, 3))', sprintf(_('%s must be a 2-D matrix.'), 'Value'));
asserts.checkerror('mat2str(rand(2, 2, 3))', sprintf(_('%s must be a 2-D matrix.'), 'Value'), 'Nelson:validators:mustBeMatrix');
asserts.checkerror('R = mat2str([3.85 2.91; 7.74 8.99], 12,''class'', 3)', _('Too many input arguments.'));
%=============================================================================


%=============================================================================
% negative zero prints as "0" (matching expected behavior)
assert_isequal(mat2str(-0), '0');
assert_isequal(mat2str([1 -0 2]), '[1 0 2]');
assert_isequal(mat2str(0 * -1), '0');
assert_isequal(num2str(-0), '0');
assert_isequal(num2str(0 * -1), '0');
%=============================================================================
% line-break and null control characters are emitted as char(N)
assert_isequal(mat2str(sprintf('a\nb')), '[''a'' char(10) ''b'']');
assert_isequal(mat2str(char(10)), '['''' char(10) '''']');
assert_isequal(mat2str([char(10) 'ab']), '['''' char(10) ''ab'']');
assert_isequal(mat2str(['ab' char(10)]), '[''ab'' char(10) '''']');
assert_isequal(mat2str(['a' char(10) char(13) 'b']), '[''a'' char(10) '''' char(13) ''b'']');
assert_isequal(mat2str(char([72 0 73])), '[''H'' char(0) ''I'']');
%=============================================================================
% a tab stays a literal character
assert_isequal(mat2str(sprintf('x\ty')), sprintf('''x\ty'''));
%=============================================================================
% a 0x0 empty character array prints as an empty string literal
assert_isequal(mat2str(''), '''''');
assert_isequal(mat2str(repmat('x', 1, 0)), 'zeros(1,0)');
assert_isequal(mat2str(repmat('x', 0, 3)), 'zeros(0,3)');
%=============================================================================
