%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
% <--CLI MODE--> 
%=============================================================================
skip_testsuite(~ismodule('trigonometric_functions'), _('trigonometric_functions module not installed'));
clear('functions');
% use always same macro list to have reproducible test
macro_list = {
'acosd';
'acosh';
'acot';
'acotd';
'acoth';
'acsc';
'acscd';
'acsch';
'asec';
'asecd';
'asech';
'asind';
'asinh';
'atan2d';
'atand';
'cart2pol';
'cart2sph';
'cosd';
'cospi';
'cot';
'cotd';
'coth';
'csc';
'cscd';
'csch';
'deg2rad';
'pol2cart';
'rad2deg';
'sec';
'secd';
'sech';
'sind';
'sinpi';
'sph2cart';
'tand'};
[u1, s1] = memory();
for j = 1:10
    for k = 1:length(macro_list)
        execstr([macro_list{k}, ';'], 'errcatch');
    end
end
[u2, s2] = memory();
clear('functions');
[u3, s3] = memory();
%=============================================================================
assert_istrue(u2.MemUsedNelson - u1.MemUsedNelson <= (110/100) * u1.MemUsedNelson)
assert_istrue(u3.MemUsedNelson - u1.MemUsedNelson <= (110/100) * u1.MemUsedNelson)
%=============================================================================
