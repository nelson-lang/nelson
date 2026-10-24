%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
% <--ADV-CLI MODE-->
%=============================================================================
% the keyboard selects a uicontrol like a click does, Callback included:
% Space on a button, a key that changes the selection of a listbox
%=============================================================================
function test_uicontrol_keyboard()
  skip_if_portable_graphics_backend('native keyboard focus events are not available.');
  %=============================================================================
  f = figure('Visible', 'off');
  b = uicontrol(f, 'Style', 'pushbutton', 'String', 'go', 'Position', [10 10 60 20], ...
    'Callback', @(src, evt) set(f, 'UserData', 'push'));
  % uicontrol(h) gives the keyboard focus to h, without creating a control
  nChildren = numel(f.Children);
  h = uicontrol(b);
  assert_isequal(h, b);
  assert_isequal(numel(f.Children), nChildren);
  assert_istrue(__uicontrol_key_event__(b, 'space'));
  drainCallbacks();
  assert_isequal(f.UserData, 'push');
  %---------------------------------------------------------------------------
  styles = {'checkbox', 'radiobutton', 'togglebutton'};
  for k = 1:numel(styles)
    f.UserData = [];
    c = uicontrol(f, 'Style', styles{k}, 'String', styles{k}, 'Position', [10 40 100 20], ...
      'Callback', @(src, evt) set(f, 'UserData', src.Value));
    assert_istrue(__uicontrol_key_event__(c, 'space'));
    drainCallbacks();
    assert_isequal(c.Value, 1);
    assert_isequal(f.UserData, 1);
    assert_istrue(__uicontrol_key_event__(c, 'space'));
    drainCallbacks();
    assert_isequal(c.Value, 0);
    assert_isequal(f.UserData, 0);
    delete(c);
  end
  %---------------------------------------------------------------------------
  f.UserData = [];
  lb = uicontrol(f, 'Style', 'listbox', 'String', {'a', 'b', 'c'}, 'Value', 1, ...
    'Position', [10 70 100 60], 'Callback', @(src, evt) set(f, 'UserData', src.Value));
  assert_istrue(__uicontrol_key_event__(lb, 'down'));
  drainCallbacks();
  assert_isequal(lb.Value, 2);
  assert_isequal(f.UserData, 2);
  assert_istrue(__uicontrol_key_event__(lb, 'end'));
  drainCallbacks();
  assert_isequal(lb.Value, 3);
  assert_isequal(f.UserData, 3);
  %---------------------------------------------------------------------------
  % 'inactive' looks normal but ignores the keyboard
  f.UserData = [];
  ci = uicontrol(f, 'Style', 'checkbox', 'Enable', 'inactive', ...
    'Callback', @(src, evt) set(f, 'UserData', 'inactive'));
  assert_istrue(__uicontrol_key_event__(ci, 'space'));
  drainCallbacks();
  assert_isequal(ci.Value, 0);
  assert_istrue(isempty(f.UserData));
  %---------------------------------------------------------------------------
  % in a button group, Space selects the button and the group reports it
  bg = uibuttongroup(f, 'Position', [0.5 0 0.5 1]);
  r1 = uicontrol(bg, 'Style', 'radiobutton', 'String', 'one', 'Position', [10 40 80 20]);
  r2 = uicontrol(bg, 'Style', 'radiobutton', 'String', 'two', 'Position', [10 10 80 20]);
  bg.SelectionChangedFcn = @(src, evt) set(src, 'UserData', evt.NewValue);
  assert_istrue(__uicontrol_key_event__(r2, 'space'));
  drainCallbacks();
  assert_isequal([r1.Value, r2.Value], [0 1]);
  assert_isequal(bg.SelectedObject, r2);
  assert_isequal(bg.UserData, r2);
  % Space on the selected button keeps it selected
  bg.UserData = [];
  assert_istrue(__uicontrol_key_event__(r2, 'space'));
  drainCallbacks();
  assert_isequal([r1.Value, r2.Value], [0 1]);
  assert_istrue(isempty(bg.UserData));
  %---------------------------------------------------------------------------
  assert_checkerror('__uicontrol_key_event__(f, ''space'')', getString(message('Nelson:uicontrol:expectedUicontrol')), 'Nelson:uicontrol:expectedUicontrol');
  close(f);
end
%=============================================================================
function drainCallbacks()
  % each drawnow runs one queued callback
  for k = 1:4
    drawnow();
  end
end
%=============================================================================
