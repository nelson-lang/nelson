%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/979
% <-- Short Description -->
% wrong result from arithmetic operation with integers and floats
%=============================================================================
assert_isequal(int16(325) * 4.39, int16(1427))
assert_isequal(int16(325) .* 4.39, int16(1427))
assert_isequal(4.39 * int16(325) , int16(1427))
assert_isequal(4.39 .* int16(325), int16(1427))
%=============================================================================
assert_isequal(int16(325) / 4.39, int16(74))
assert_isequal(int16(325) ./ 4.39, int16(74))
assert_isequal(4.39 / int16(325), int16(0))
assert_isequal(4.39 ./ int16(325), int16(0))
%=============================================================================
assert_isequal(int16(325) \ 4.39, int16(0))
assert_isequal(int16(325) .\ 4.39, int16(0))
assert_isequal(4.39 \ int16(325), int16(74))
assert_isequal(4.39 .\ int16(325), int16(74))
%=============================================================================
assert_isequal(4.39 + int16(325), int16(329))
assert_isequal(int16(325) + 4.39, int16(329))
assert_isequal(4.39 - int16(325), int16(-321))
assert_isequal(int16(325) - 4.39, int16(321))
%=============================================================================
assert_isequal(4.39 ^ int16(325), intmax('int16'))
assert_isequal(4.39 ^ int64(325), intmax('int64'))
assert_checkerror('int64(325) .^ 4.39', _('Positive integral powers expected.'))
% assert_checkerror('int16(325) ^ 4.39', _('Positive integral powers expected.'))
%=============================================================================
R = [int16(325),4.39];
REF = int16([325, 4]);
assert_isequal(R, REF);
%=============================================================================
