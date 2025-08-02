%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
pTextToSpeech = actxserver('Sapi.SpVoice')
for i = 0:5
  invoke(pTextToSpeech, 'Speak', int2str(5 - i));
end
invoke(pTextToSpeech, 'Speak', _('Welcome to COM Interface for Nelson !'));
delete(pTextToSpeech)
clear pTextToSpeech
%=============================================================================
