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
% (7) a second leading sign is invalid.
asserts.isequal(str2double('--5'), NaN);
asserts.isequal(str2double('+-5'), NaN);
%=============================================================================
% (7) overflow saturates to +/- Inf.
asserts.isequal(str2double('1e400'), Inf);
asserts.isequal(str2double('-1e400'), -Inf);
%=============================================================================
% (7) hexadecimal literals.
asserts.isequal(str2double('0x1A'), 26);
%=============================================================================
% (7) lower-case signed infinities stay on the real path.
asserts.isequal(str2double('+inf'), Inf);
asserts.isequal(str2double('-inf'), -Inf);
asserts.isequal(str2double('inf'), Inf);
%=============================================================================
% Existing complex parsing is preserved.
asserts.isequal(str2double('3+4i'), complex(3, 4));
asserts.isequal(str2double('2i'), complex(0, 2));
%=============================================================================
% uppercase I and J are not imaginary units, no blank after a sign,
% Unicode spaces only after the number, non-text input.
asserts.isequal(str2double('I'), NaN);
asserts.isequal(str2double('2J'), NaN);
asserts.isequal(str2double('1+2I'), NaN);
asserts.isequal(str2double('- 1'), NaN);
asserts.isequal(str2double('+ 1'), NaN);
asserts.isequal(str2double('- 1 + 2i'), NaN);
asserts.isequal(str2double('1 - - 2i'), NaN);
asserts.isequal(str2double('InfI'), NaN);
asserts.isequal(str2double('2*I'), NaN);
asserts.isequal(str2double([char(8192) '5']), NaN);
asserts.isequal(str2double(['5' char(8192)]), 5);
asserts.isequal(str2double(['5' char(8199)]), NaN);
asserts.isequal(str2double(['5' char(160)]), NaN);
asserts.isequal(str2double([char(12288) '1+2i']), NaN);
asserts.isequal(str2double(['1' char(8192) '+' char(8192) '2i']), 1+2i);
asserts.isequal(str2double(['1' char(8199) '+' char(8199) '2i']), NaN);
asserts.isequal(str2double(['1' char(9) '+' char(10) '2i']), 1+2i);
asserts.isequal(str2double(@sin), NaN);
asserts.isequal(str2double(struct()), NaN);
asserts.isequal(str2double({'1', 2; 'x', '3'}), [1 NaN;NaN 3]);
%=============================================================================
