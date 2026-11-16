%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--NOT FIX FOR WASM-->
%=============================================================================
% stdout capture and process lifecycle
if ispc()
  p = nelson.os.Process('cmd', '/c', 'echo nelson_process_ok');
else
  p = nelson.os.Process('sh', '-c', 'echo nelson_process_ok');
end
assert_isequal(class(p), 'nelson.os.Process');
assert_istrue(isvalid(p));
p.wait(10);
out = '';
for k = 1:200
  out = [out, p.read()];
  if contains(out, 'nelson_process_ok')
    break;
  end
  pause(0.05);
end
assert_istrue(contains(out, 'nelson_process_ok'));
assert_isfalse(p.Running);
assert_isequal(p.ExitCode, 0);
assert_istrue(p.ProcessId > 0);
delete(p);
assert_isfalse(isvalid(p));
%=============================================================================
% stdin streaming: feed unsorted lines to 'sort', close input, read sorted output
if ispc()
  p = nelson.os.Process('cmd', '/c', 'sort');
else
  p = nelson.os.Process('sort');
end
p.write(sprintf('banana\napple\ncherry\n'));
p.close();
p.wait(10);
out = '';
for k = 1:200
  out = [out, p.read()];
  if contains(out, 'cherry')
    break;
  end
  pause(0.05);
end
ia = strfind(out, 'apple');
ib = strfind(out, 'banana');
ic = strfind(out, 'cherry');
assert_istrue(~isempty(ia) && ~isempty(ib) && ~isempty(ic));
assert_istrue(ia(1) < ib(1) && ib(1) < ic(1));
delete(p);
%=============================================================================
% properties reflect the live process
if ispc()
  p = nelson.os.Process('cmd', '/c', 'echo x');
else
  p = nelson.os.Process('sh', '-c', 'echo x');
end
assert_isequal(class(p.Command), 'char');
assert_istrue(islogical(p.Running));
p.wait(10);
delete(p);
%=============================================================================
% a missing program is rejected at launch on Windows
if ispc()
  assert_checkerror('p = nelson.os.Process(''a_program_that_surely_does_not_exist_zzz'');', ...
    'Unable to start the requested program.', 'Nelson:os_functions:processStartFailed');
end
%=============================================================================
