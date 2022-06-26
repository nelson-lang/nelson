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
assert_isequal(nargin('COM_class'), 1);
assert_isequal(nargout('COM_class'), 1);
%=============================================================================
pTextToSpeech = actxserver('Sapi.SpVoice');
c1 = COM_class(pTextToSpeech);
c2 = class(pTextToSpeech);
assert_isequal(c1, c2);
assert_isequal(c1, 'COM.SpeechLibSpVoice');
%=============================================================================
x = actxserver('Microsoft.XMLDOM');
%=============================================================================
R = [x, pTextToSpeech];
assert_isequal(class(R), 'handle');
%=============================================================================
R = [x, x];
assert_isequal(class(R), 'COM.MSXML2DOMDocument');
%=============================================================================
delete(x);
delete(pTextToSpeech);
%=============================================================================
