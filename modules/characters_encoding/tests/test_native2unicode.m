%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('native2unicode'), 2);
assert_isequal(nargout('native2unicode'), 1);
%=============================================================================
C = uint8([26   108    26   118   101]);
R = double(native2unicode(C, 'ascii'));
REF = double(C);
assert_isequal(R, REF);
%=============================================================================
R = native2unicode('Nelson', 'ascii');
REF = 'Nelson';
assert_isequal(R, REF);
%=============================================================================
C = uint8([149   208   137   188   150   188]);
R = native2unicode(C, 'SHIFT_JIS');
REF = '片仮名';
assert_isequal(R, REF);
%=============================================================================
C = [101   108   101   118   101 ] + i;
R = native2unicode(C, 'ascii');
REF = 'eleve';
assert_isequal(R, REF);
%=============================================================================
REF = 'Виртуальная';
C = uint8([194   232   240   242   243   224   235   252   237   224   255]);
R = native2unicode(C, 'Windows-1251');
assert_isequal(R, REF);
%=============================================================================
