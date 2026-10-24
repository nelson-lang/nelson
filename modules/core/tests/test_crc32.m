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
asserts.isequal(nargin('crc32'), 2);
asserts.isequal(nargout('crc32'), 1);
%=============================================================================
R = crc32('Nelson');
REF = 'BA6A9707';
asserts.isequal(R, REF)
%=============================================================================
R = crc32({'Hello', 'World'});
REF = {'F7D18982', ...
  'FBB63E47'};
asserts.isequal(R, REF)
%=============================================================================
R = crc32(["Hello"; "World"]);
REF = ["F7D18982";
  "FBB63E47"];
asserts.isequal(R, REF)
%=============================================================================
R = crc32(["Hello"; "World"], '-file');
REF = [""; ""];
asserts.isequal(R, REF)
%=============================================================================
R = crc32({'Hello'; 'World'}, '-file');
REF = {''; ''};
asserts.isequal(R, REF)
%=============================================================================
R = crc32([modulepath('matio', 'tests'), '/mat/test_char_array_unicode_7.4_GLNX86.mat']);
REF = '1293B98F';
asserts.isequal(R, REF)
%=============================================================================
R = crc32([modulepath('matio', 'tests'), '/mat/test_char_array_unicode_7.4_GLNX86.mat'], '-file');
REF = '1293B98F';
asserts.isequal(R, REF)
%=============================================================================
% crypto.crc32 is an alias
asserts.isequal(crypto.crc32('Nelson'), crc32('Nelson'));
asserts.isequal(feval('crypto.crc32', 'Nelson'), crc32('Nelson'));
%=============================================================================
