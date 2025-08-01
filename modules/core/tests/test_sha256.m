%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('sha256'), 2);
assert_isequal(nargout('sha256'), 1);
%=============================================================================
R = sha256('Nelson');
REF = '818d0e5ffae3c30dfbe68dbb57896728bc1598d0094467bb378db4cb21a1774c';
assert_isequal(R, REF)
%=============================================================================
R = sha256({'Hello', 'World'});
REF = {'185f8db32271fe25f561a6fc938b2e264306ec304eda518007d1764826381969', ...
'78ae647dc5544d227130a0682a51e30bc7777fbb6d8a8f17007463a3ecd1d524'};
assert_isequal(R, REF)
%=============================================================================
R = sha256(["Hello"; "World"]);
REF = ["185f8db32271fe25f561a6fc938b2e264306ec304eda518007d1764826381969";
"78ae647dc5544d227130a0682a51e30bc7777fbb6d8a8f17007463a3ecd1d524"];
assert_isequal(R, REF)
%=============================================================================
R = sha256(["Hello"; "World"], '-file');
REF = [""; ""];
assert_isequal(R, REF)
%=============================================================================
R = sha256({'Hello'; 'World'}, '-file');
REF = {''; ''};
assert_isequal(R, REF)
%=============================================================================
R = sha256([modulepath('matio', 'tests'), '/mat/test_char_array_unicode_7.4_GLNX86.mat']);
REF = 'f7ef31cc009995e0643236676ebf2de0d72cf41ea66f3ace3436e53e7b5e04f5';
assert_isequal(R, REF)
%=============================================================================
R = sha256([modulepath('matio', 'tests'), '/mat/test_char_array_unicode_7.4_GLNX86.mat'], '-file');
REF = 'f7ef31cc009995e0643236676ebf2de0d72cf41ea66f3ace3436e53e7b5e04f5';
assert_isequal(R, REF)
%=============================================================================
