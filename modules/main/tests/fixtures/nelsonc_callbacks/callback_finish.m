%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function callback_finish()
  f = findall(groot(), '-depth', 1, 'Tag', 'nelsonc-text-callback');
  clock = get(f, 'UserData');
  stop(clock);
  delete(f);
  callback_emit('TIMER_TEXT_OK');
end
