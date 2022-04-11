%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WINDOWS ONLY-->
%=============================================================================
assert_isequal(nargin('COM_methods'), 1);
assert_isequal(nargout('COM_methods'), 1);
%=============================================================================
pTextToSpeech = actxserver('Sapi.SpVoice');
m = methods(pTextToSpeech);
assert_istrue(iscell(m));
assert_istrue(iscellstr(m));
assert_isfalse(isempty(m));
delete(pTextToSpeech);
clear pTextToSpeech
%=============================================================================
