%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% A key press updates the figure CurrentCharacter with the typed character,
% whether or not a WindowKeyPressFcn is set. Non-character keys (arrows) carry
% no text and leave it unchanged, and a key release does not change it.
% __web_figure_key_event__(f, pressed, character, key, shift, ctrl, alt, meta)
% replays the event through the web client entry point.
%=============================================================================
f = figure('Position', [100 100 400 300]);
assert_isequal(f.CurrentCharacter, '');
%=============================================================================
__web_figure_key_event__(f, true, 'x', 'x', false, false, false, false);
assert_isequal(f.CurrentCharacter, 'x');
%=============================================================================
% a non-character key (arrow) leaves CurrentCharacter unchanged
__web_figure_key_event__(f, true, '', 'ArrowUp', false, false, false, false);
assert_isequal(f.CurrentCharacter, 'x');
%=============================================================================
% a later character replaces it
__web_figure_key_event__(f, true, 'Z', 'Z', true, false, false, false);
assert_isequal(f.CurrentCharacter, 'Z');
%=============================================================================
% a key release does not change CurrentCharacter
__web_figure_key_event__(f, false, 'q', 'q', false, false, false, false);
assert_isequal(f.CurrentCharacter, 'Z');
close(f);
%=============================================================================
% the WindowKeyPressFcn callback sees the updated CurrentCharacter
f = figure('Position', [100 100 400 300]);
global KEY_SEEN
KEY_SEEN = {};
f.WindowKeyPressFcn = @(src, evt) record_current_character(src);
__web_figure_key_event__(f, true, 'm', 'm', false, false, false, false);
drawnow();
pause(0.2);
assert_isequal(numel(KEY_SEEN), 1);
assert_isequal(KEY_SEEN{1}, 'm');
close(f);
clear global KEY_SEEN
%=============================================================================
function record_current_character(fig)
  global KEY_SEEN
  KEY_SEEN{end + 1} = fig.CurrentCharacter;
end
%=============================================================================
