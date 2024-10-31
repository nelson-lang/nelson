%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
A = {'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaagggggggggggggggrrgrgrgrgggggggggggggggggggggggggggrgrgr';'vvvvvvvvvvvvvvvvvvvvvvvvvv';'ddddddddddddddddddd'}
R = evalc('A');
REF = '
A =

  3×1 cell array

    {''aaaaaaaaaaaaaaaaaaaaaaaaaaaaaagggggggggggggggrrgrgrgrgggggggggggggggggg…''}
    {''vvvvvvvvvvvvvvvvvvvvvvvvvv''}                                              
    {''ddddddddddddddddddd''}                                                     

';
assert_isequal(R, REF)
%=============================================================================
A = {1,2,3;
     'text',rand(5,10,2),{11; 22; 33}};
R = evalc('A')
REF =  '
A =

  2×3 cell array

    {[1]}       {[2]}              {[3]}     
    {''text''}    {5×10×2 double}    {3×1 cell}

';
assert_isequal(R, REF)
%=============================================================================
A = {"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaagggggggggggggggrrgrgrgrgggggggggggggggggggggggggggrgrgr";"vvvvvvvvvvvvvvvvvvvvvvvvvv";"ddddddddddddddddddd"};
R = evalc('A');
REF =  '
A =

  3×1 cell array

    {1×1 string}                    
    {["vvvvvvvvvvvvvvvvvvvvvvvvvv"]}
    {["ddddddddddddddddddd"]}       

';
assert_isequal(R, REF)
%=============================================================================
A = cell(3);
R = evalc('A');
REF =  '
A =

  3×3 cell array

    {0×0 double}    {0×0 double}    {0×0 double}
    {0×0 double}    {0×0 double}    {0×0 double}
    {0×0 double}    {0×0 double}    {0×0 double}

';
assert_isequal(R, REF)
%=============================================================================

