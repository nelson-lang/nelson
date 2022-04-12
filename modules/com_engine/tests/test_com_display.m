%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
pTextToSpeech = actxserver('Sapi.SpVoice');
%=============================================================================
R = evalc('disp(pTextToSpeech)');
REF = '  1×1 handle [COM] 

	COM.SpeechLibSpVoice
';
assert_isequal(R, REF)
%=============================================================================
R = evalc('display(pTextToSpeech)');
REF = '
pTextToSpeech =

  1×1 handle [COM] 

	COM.SpeechLibSpVoice

';
assert_isequal(R, REF)
%=============================================================================
