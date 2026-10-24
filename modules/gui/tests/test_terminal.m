%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--GUI MODE-->
%=============================================================================
terminal.closeAll();
t = terminal('Name', 'Nelson terminal test');
asserts.istrue(isa(t, 'terminal'));
asserts.isequal(t.Name, 'Nelson terminal test');
asserts.isequal(t.Place, 'nelson');
asserts.isequal(t.WindowStyle, 'docked');
asserts.istrue(ischar(t.Shell));
asserts.istrue(t.Running);
asserts.istrue(t.ProcessId >= 0);
asserts.istrue(t.isrunning());
%=============================================================================
t.Name = 'Renamed terminal test';
asserts.isequal(t.Name, 'Renamed terminal test');
themes = terminal.themes();
asserts.istrue(any(strcmp(themes, 'auto')));
t.Theme = 'dark';
asserts.isequal(t.Theme, 'dark');
%=============================================================================
t.run('echo NELSON_TERMINAL_TEST');
found = false;
for k = 1:50
  pause(0.1);
  if contains(t.read(), 'NELSON_TERMINAL_TEST')
    found = true;
    break
  end
end
asserts.istrue(found);
%=============================================================================
if ispc()
  t.clear();
  accentedText = ['NELSON_TERMINAL_ACCENT_', char(233), char(232), char(224), char(231)];
  t.run(['echo ', accentedText]);
  found = false;
  for k = 1:50
    pause(0.1);
    if contains(t.read(), accentedText)
      found = true;
      break
    end
  end
  asserts.istrue(found);

  t.clear();
  t.run('NELSON_TERMINAL_UNKNOWN_COMMAND');
  found = false;
  for k = 1:50
    pause(0.1);
    transcript = strtrim(t.read());
    if contains(transcript, 'NELSON_TERMINAL_UNKNOWN_COMMAND') && endsWith(transcript, '>')
      found = true;
      break
    end
  end
  asserts.istrue(found);
end
%=============================================================================
active = terminal.list();
asserts.istrue(numel(active) >= 1);
asserts.checkerror('terminal(''Model'', ''x'')', _('"Model" is not supported by terminal.'));
delete(t);
terminal.closeAll();
asserts.istrue(isempty(terminal.list()));
%=============================================================================
