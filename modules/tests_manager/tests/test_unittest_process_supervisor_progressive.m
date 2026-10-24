%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--SEQUENTIAL TEST REQUIRED-->
skip_if_no_native_unittest_runner();
%=============================================================================
binpath = modulepath('nelson', 'bin');
nelson_exe = [binpath, '/nelson-cli'];
if ispc()
  nelson_exe = [nelson_exe, '.exe'];
end

jobs = run_process_jobs({nelson_exe; nelson_exe; nelson_exe}, ...
  {'--quiet --no-ipc --no-user-modules -e "pause(1);disp(''long_done'')"'; ...
  '--quiet --no-ipc --no-user-modules -e "disp(''short_a'')"'; ...
  '--quiet --no-ipc --no-user-modules -e "disp(''short_b'')"';}, ...
  [20, 20, 20], {'direct'; 'direct'; 'direct'}, [1, 2, 3], ...
  {'long'; 'short-a'; 'short-b'}, {'long'; 'short-a'; 'short-b'});
events = run_processes_progressive(jobs, 3);
processOrder = {};
for k = 1:length(events)
  if strcmp(events{k}.event_type, 'process')
    processOrder{end + 1} = events{k}.job_id; %#ok<AGROW>
  end
end
asserts.isequal(length(processOrder), 3);
asserts.istrue(~strcmp(processOrder{1}, 'long'));
asserts.istrue(~strcmp(processOrder{2}, 'long'));
asserts.isequal(processOrder{3}, 'long');

payloadCode = ['__nelson_unittest_worker_emit__(jsonencode(struct(''r'',0,', ...
  '''msg'','''',''stdout'',''first'',''timing'',0)));', ...
  'pause(0.25);', ...
  '__nelson_unittest_worker_emit__(jsonencode(struct(''r'',0,', ...
  '''msg'','''',''stdout'',''second'',''timing'',0)));'];
payloadJobs = run_process_jobs({nelson_exe}, ...
  {['--quiet --no-ipc --no-user-modules -e "', payloadCode, '"']}, ...
  20, {'pool'}, 1, {'payload-worker'}, {'payload-worker'});
payloadEvents = run_processes_progressive(payloadJobs, 1);
payloadOrder = {};
eventOrder = {};
for k = 1:length(payloadEvents)
  eventOrder{end + 1} = payloadEvents{k}.event_type; %#ok<AGROW>
  if strcmp(payloadEvents{k}.event_type, 'payload')
    payload = decode_runfile_payload(payloadEvents{k}.payload);
    payloadOrder{end + 1} = payload.stdout; %#ok<AGROW>
  end
end
asserts.isequal(payloadOrder, {'first', 'second'});
asserts.isequal(eventOrder{1}, 'payload');
asserts.isequal(eventOrder{end}, 'process');

resultFile = [tempname(), '.json'];
cleanupResultFile = onCleanup(@() evalc(['if isfile(''', resultFile, ...
  '''), rmfile(''', resultFile, '''), end'])); %#ok<NLS0004>
fileCode = ['payload=jsonencode(struct(''r'',0,''msg'','''',', ...
  '''stdout'',''file-ready'',''timing'',0));', ...
  'filewrite(', nelsonStringLiteral(resultFile), ',[payload,char(10)]);pause(1);'];
fileJobs = run_process_jobs({nelson_exe}, ...
  {['--quiet --no-ipc --no-user-modules -e "', fileCode, '"']}, ...
  20, {'direct'}, 1, {'result-file'}, {'result-file'});
fileJobs.result_file = {resultFile};
fileEvents = run_processes_progressive(fileJobs, 1);
filePayload = {};
fileReason = '';
for k = 1:length(fileEvents)
  if strcmp(fileEvents{k}.event_type, 'payload')
    payload = decode_runfile_payload(fileEvents{k}.payload);
    filePayload{end + 1} = payload.stdout; %#ok<AGROW>
  elseif strcmp(fileEvents{k}.event_type, 'process')
    fileReason = fileEvents{k}.reason;
  end
end
asserts.isequal(filePayload, {'file-ready'});
asserts.isequal(fileReason, 'result_received');
asserts.istrue(~isempty(cleanupResultFile));
clear cleanupResultFile
%=============================================================================
