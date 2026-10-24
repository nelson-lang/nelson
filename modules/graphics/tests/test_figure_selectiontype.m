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
% A mouse press sets the figure SelectionType before the WindowButtonDownFcn
% callback runs: a plain left click is 'normal', shift-left or the middle
% button is 'extend', ctrl-left or the right button is 'alt', and a double
% click is 'open'. __figure_mouse_event__ replays the gesture through the
% figure window input path (native) or the web client entry point ('web').
%=============================================================================
skip_if_display_cannot_place_figures('figure selection type');
f = figure('Position', [100 100 400 300]);
assert_isequal(f.SelectionType, 'normal');
%=============================================================================
% web client entry point: works on every backend
assert_istrue(__figure_mouse_event__(f, 'press', [200 150], 'web', 'normal'));
assert_isequal(f.SelectionType, 'normal');
assert_istrue(__figure_mouse_event__(f, 'press', [200 150], 'web', 'extend'));
assert_isequal(f.SelectionType, 'extend');
assert_istrue(__figure_mouse_event__(f, 'press', [200 150], 'web', 'alt'));
assert_isequal(f.SelectionType, 'alt');
assert_istrue(__figure_mouse_event__(f, 'press', [200 150], 'web', 'open'));
assert_isequal(f.SelectionType, 'open');
%=============================================================================
% motion and release do not change SelectionType (reflects the last press)
__figure_mouse_event__(f, 'press', [200 150], 'web', 'alt');
f.WindowButtonMotionFcn = @(src, evt) disp('');
__figure_mouse_event__(f, 'motion', [210 160], 'web', 'normal');
assert_isequal(f.SelectionType, 'alt');
__figure_mouse_event__(f, 'release', [210 160], 'web', 'normal');
assert_isequal(f.SelectionType, 'alt');
f.WindowButtonMotionFcn = [];
close(f);
%=============================================================================
% the WindowButtonDownFcn callback sees the updated SelectionType
f = figure('Position', [100 100 400 300]);
global SEL_SEEN
SEL_SEEN = {};
f.WindowButtonDownFcn = @(src, evt) record_selection_type(src);
__figure_mouse_event__(f, 'press', [120 90], 'web', 'alt');
drawnow();
pause(0.2);
assert_isequal(numel(SEL_SEEN), 1);
assert_isequal(SEL_SEEN{1}, 'alt');
close(f);
clear global SEL_SEEN
%=============================================================================
% native figure window input path (needs native mouse synthesis)
is_portable = strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm();
if ~is_portable
  f = figure('Position', [100 100 400 300]);
  assert_istrue(__figure_mouse_event__(f, 'press', [200 150], 'native', 'normal'));
  assert_isequal(f.SelectionType, 'normal');
  assert_istrue(__figure_mouse_event__(f, 'press', [200 150], 'native', 'extend'));
  assert_isequal(f.SelectionType, 'extend');
  assert_istrue(__figure_mouse_event__(f, 'press', [200 150], 'native', 'alt'));
  assert_isequal(f.SelectionType, 'alt');
  assert_istrue(__figure_mouse_event__(f, 'press', [200 150], 'native', 'open'));
  assert_isequal(f.SelectionType, 'open');
  close(f);
end
%=============================================================================
function record_selection_type(fig)
  global SEL_SEEN
  SEL_SEEN{end + 1} = fig.SelectionType;
end
%=============================================================================
