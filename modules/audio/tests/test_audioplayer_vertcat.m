%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--AUDIO OUTPUT REQUIRED-->
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
json_audio = [modulepath('audio', 'tests'), '/test_audioplayer.json'];
st = jsondecode(fileread(json_audio));
%=============================================================================
r = audioplayer(st.y, st.fs);
R = [r, r];
assert_isequal(size(R),  [1 2]);
assert_isequal(class(R), 'audioplayer');
%=============================================================================
