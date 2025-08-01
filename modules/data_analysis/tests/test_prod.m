%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(class(prod(single(3i),'default')), 'single');
assert_isequal(prod(single(3i)), single(3i));
%=============================================================================
assert_isequal(class(prod(single(3i),'double')), 'double');
%=============================================================================
assert_isequal(class(prod(true,'native')), 'logical');
assert_isequal(prod(true,'native'), true);
%=============================================================================
assert_isequal(class(prod(true,'default')), 'double');
assert_isequal(prod(true,'default'), 1);
%=============================================================================
assert_isequal(class(prod(true,'double')), 'double');
assert_isequal(prod(true,'double'), 1);
%=============================================================================
assert_checkerror('prod(''A'')', [_('function') , ' ', 'char', '_prod', ' ',  _('undefined.')])
%=============================================================================
M = [ 8    1    6;
3    5    7;
4    9    2];
R = prod(M);
REF = [  96    45    84];
assert_isequal(R, REF);
%=============================================================================
R = prod(M, 2);
REF = [48; 105; 72];
assert_isequal(R, REF);
%=============================================================================
M = [ 8    1    6;
3    5    7;
4    9    2];
R = prod(M, 4);
REF = M;
assert_isequal(R, REF);
%=============================================================================
M = [10:30:70; 20:30:80; 30:30:90];
R = prod(M);
REF = [6000      120000      504000];
assert_isequal(R, REF);
%=============================================================================
M = [true,false;true,true];
R = prod(M);
REF = [1 0];
assert_isequal(R, REF);
%=============================================================================
M = [1:3:7;2:3:8;3:3:9];
M(:,:,2) = [10:3:16;11:3:17;12:3:18];
R = prod(M, 3);
REF = [ 10    52   112; 22    70   136; 36    90   162];
assert_isequal(R, REF);
%=============================================================================
M = single([12 15 18; 13 16 19; 14 17 20]);
R = prod(M, 2, 'double');
REF = [3240; 3952; 4760];
assert_isequal(R, REF);
assert_isequal(class(R), 'double');
%=============================================================================
M = uint8([10:30:70;20:30:80;30:30:90]);
R = prod(M, 'native');
REF = uint8([255  255  255]);
assert_isequal(R, REF);
%=============================================================================
M = [10 3 2 4 NaN 30 NaN 20];
R = prod(M, 'omitnan');
REF = 144000;
assert_isequal(R, REF);
%=============================================================================
R = prod(zeros(0, 0));
REF = 1;
assert_isequal(R, REF);
%=============================================================================
R = prod(zeros(0, 1));
REF = 1;
assert_isequal(R, REF);
%=============================================================================
R = prod([intmax('uint64'), intmin('uint64'), intmax('uint64')]);
REF = 0;
assert_isequal(R, REF);
%=============================================================================
A = int32(1:10);
R = prod(A, 'native');
REF = int32(3628800);
assert_isequal(R, REF);
%=============================================================================
