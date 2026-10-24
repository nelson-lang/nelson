%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function callbacks_entry(mode)
  asserts.isequal(fft([1, 0, 0, 0]), ones(1, 4));
  asserts.isequal(fft(single([1, 0, 0, 0])), single(ones(1, 4)));
  f = figure('Visible', 'off', 'Tag', 'nelsonc-text-callback', ...
    'Name', 'nelsonc callback validation', 'NumberTitle', 'off');
  if strcmp(mode, 'create')
    uicontrol(f, 'Style', 'pushbutton', 'CreateFcn', 'callback_emit(''CREATE_TEXT_OK'');');
    delete(f);
  elseif strcmp(mode, 'close')
    f.CloseRequestFcn = 'callback_close;';
    close(f);
  elseif strcmp(mode, 'timer')
    clock = timer('StartDelay', 0.2, 'TimerFcn', 'callback_finish;');
    set(f, 'Visible', 'on', 'HandleVisibility', 'off', 'UserData', clock);
    start(clock);
  else
    error('Unknown callback test mode.');
  end
  fprintf('CALLBACK_ENTRY_RETURN\n');
end
